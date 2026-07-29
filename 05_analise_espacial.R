# 05_analise_espacial.R
# ---------------------------------------------------------------------------
# Analise espacial municipal: suavizacao Bayesiana empirica (EBest), matriz de pesos de contiguidade Queen (padronizada por linha, zero.policy), Indice de Moran global (999 permutacoes MC), Moran local/LISA e Getis-Ord Gi* com correcao FDR (Benjamini-Hochberg), densidade de Kernel (banda de Sheather-Jones) e estatistica de varredura espacial (scan de Kulldorff).
#
# Funcoes do EpiSUS efetivamente utilizadas na analise do artigo
# "Escorpionismo no Brasil, 2016-2025". Codigo extraido VERBATIM do
# modulo de logica pura core_datasus.R do aplicativo EpiSUS (Shiny),
# sem a camada de interface. R 4.6.0.
# ---------------------------------------------------------------------------

# ─────────────────────────────────────────────────────────────────────────────
# ANÁLISE ESPACIAL (Moran global, Moran local/LISA, Getis-Ord Gi*) — opcional
# ─────────────────────────────────────────────────────────────────────────────
# Autocorrelação espacial no nível MUNICIPAL de um estado — mesma fonte de
# dados do mapa municipal (RDS local + contorno geobr + população SIDRA), mas
# em vez de só colorir o mapa pela taxa, quantifica se municípios vizinhos têm
# taxas parecidas (Moran global), onde estão os aglomerados/outliers (LISA) e
# onde estão as zonas quentes/frias (Getis-Ord Gi*).
#
# Depende de 'spdep' além dos pacotes do mapa (leaflet/geobr/sf). Como o resto
# do bloco de mapa, é OPCIONAL: pacotes_espacial_disponiveis() checa antes e a
# UI mostra instrução de instalação em vez de travar quando faltar.
#
# CUIDADOS METODOLÓGICOS embutidos (ver notas na aba):
#   • Taxa instável em município pequeno → opção de suavização Bayesiana
#     empírica (spdep::EBest) + Moran EB de Assunção-Reis (EBImoran.mc), que
#     corrige a variância da taxa antes de medir autocorrelação.
#   • Escolha da matriz de vizinhança (W) muda o resultado → contiguidade
#     (Queen/Rook) ou k-vizinhos mais próximos, à escolha do usuário.
#   • Múltiplos testes (1 por município em LISA/Gi*) → correção de p-valor
#     (FDR de Benjamini-Hochberg por padrão, ou Bonferroni).

pacotes_espacial_disponiveis <- function() {
  pacotes_mapa_disponiveis() && requireNamespace("spdep", quietly = TRUE)
}

# Cores dos mapas de cluster (categóricas, não gradiente): a convenção de
# spatial epi/GeoDa é vermelho=Alto-Alto (zona quente), azul=Baixo-Baixo (zona
# fria), tons claros=outliers espaciais, cinza=não significativo.
CORES_LISA <- c(
  "Alto-Alto"         = "#c0392b",   # cluster de taxa alta (hot)
  "Baixo-Baixo"       = "#2980b9",   # cluster de taxa baixa (cold)
  "Alto-Baixo"        = "#e59866",   # outlier: alto cercado de baixos
  "Baixo-Alto"        = "#aed6f1",   # outlier: baixo cercado de altos
  "Não significativo" = COR_SEM_DADO
)
CORES_GI <- c(
  "Hot spot"          = "#c0392b",
  "Cold spot"         = "#2980b9",
  "Não significativo" = COR_SEM_DADO
)

# Escolhe a coluna de p-valor de uma matriz de resultado do spdep. Prioriza a
# versão ANALÍTICA (aproximação normal), que é contínua — essencial para que a
# correção de múltiplos testes (FDR/Bonferroni) funcione: o pseudo p-valor de
# permutação tem piso de 1/(n_sim+1) (ex.: 0,001 com 999 sorteios), grosso
# demais para o limiar do FDR quando há centenas de municípios (nada
# sobreviveria à correção só por falta de resolução da permutação).
.escolher_col_p <- function(m, preferencia = c("Pr(z != E(Ii))",
                                               "Pr(z != E(Gi))",
                                               "Pr(z != E(Ibvi))",
                                               "Pr(folded) Sim",
                                               "Pr(z != E(Ii)) Sim",
                                               "Pr(z != E(Gi)) Sim")) {
  cols <- colnames(m)
  achou <- preferencia[preferencia %in% cols]
  if (length(achou) > 0) return(as.numeric(m[, achou[1]]))
  # Último recurso: qualquer coluna que comece com "Pr("
  pcols <- grep("^Pr\\(", cols, value = TRUE)
  if (length(pcols) > 0) return(as.numeric(m[, pcols[1]]))
  rep(NA_real_, nrow(m))
}

# Constrói a lista de vizinhança (nb) + os pesos espaciais row-standardized
# (listw) a partir de um objeto sf de polígonos. zero.policy=TRUE tolera
# municípios sem vizinho (ilhas, ou o único município de um recorte pequeno)
# em vez de dar erro.
construir_pesos_espaciais <- function(geo, tipo = c("queen", "rook", "knn"), k = 5) {
  tipo <- match.arg(tipo)
  if (!requireNamespace("spdep", quietly = TRUE))
    stop("Pacote 'spdep' não instalado. Rode: install.packages('spdep')")
  if (is.null(geo) || nrow(geo) < 3)
    stop("Poucos municípios para análise espacial (mínimo 3).")

  geo <- suppressWarnings(sf::st_make_valid(geo))  # evita erro de topologia inválida

  if (tipo == "knn") {
    k_use  <- max(1L, min(as.integer(k), nrow(geo) - 1L))
    coords <- suppressWarnings(sf::st_coordinates(sf::st_centroid(sf::st_geometry(geo))))
    nb     <- spdep::knn2nb(spdep::knearneigh(coords, k = k_use))
  } else {
    nb <- spdep::poly2nb(geo, queen = (tipo == "queen"))
  }

  listw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  list(nb = nb, listw = listw, tipo = tipo, k = k,
       n_sem_vizinho = sum(spdep::card(nb) == 0L))
}

# Suavização Bayesiana empírica global (spdep::EBest) da taxa por 100k —
# encolhe a taxa de municípios com população pequena em direção à média
# global, proporcional à instabilidade. Devolve a taxa suavizada por 100k na
# mesma ordem dos vetores de entrada.
suavizar_taxa_eb <- function(casos, populacao) {
  eb <- spdep::EBest(n = casos, x = populacao)
  eb$estmm * 1e5
}

# Moran's I GLOBAL por permutação de Monte Carlo (moran.mc) + Moran EB de
# Assunção-Reis (EBImoran.mc) quando casos/população são fornecidos. O Moran
# EB é o valor "robusto" recomendado, porque desconta a autocorrelação
# espúria que a mera instabilidade da taxa em área pequena pode gerar.
moran_global <- function(valores, listw, n_sim = 999, casos = NULL, populacao = NULL) {
  if (sd(valores, na.rm = TRUE) == 0)
    stop("Todas as taxas são iguais — Moran's I é indefinido.")
  mc <- spdep::moran.mc(valores, listw, nsim = n_sim, zero.policy = TRUE)
  out <- list(
    I        = unname(mc$statistic),
    esperado = -1 / (length(valores) - 1),
    p_valor  = mc$p.value,
    n_sim    = n_sim,
    metodo   = sprintf("Monte Carlo (%d permutações)", n_sim)
  )
  if (!is.null(casos) && !is.null(populacao)) {
    eb <- tryCatch(
      spdep::EBImoran.mc(n = casos, x = populacao, listw = listw,
                         nsim = n_sim, zero.policy = TRUE),
      error = function(e) NULL)
    if (!is.null(eb)) { out$I_eb <- unname(eb$statistic); out$p_eb <- eb$p.value }
  }
  out
}

# Moran LOCAL (LISA) com classificação em quadrantes e correção de múltiplos
# testes. Retorna 1 linha por área (mesma ordem de `valores`/`listw`):
#   Ii        — indicador local de Moran
#   p_valor   — p-valor local pela aproximação normal (contínuo; ver
#               .escolher_col_p sobre por que não a permutação aqui)
#   p_ajustado— após correção (FDR/Bonferroni/nenhuma)
#   quadrante — Alto-Alto / Baixo-Baixo / Alto-Baixo / Baixo-Alto
#   cluster   — quadrante se p_ajustado < alpha, senão "Não significativo"
moran_local <- function(valores, listw, ajuste_p = "fdr", alpha = 0.05) {
  lm    <- spdep::localmoran(valores, listw, zero.policy = TRUE)
  Ii    <- as.numeric(lm[, "Ii"])
  p_raw <- .escolher_col_p(lm)
  p_adj <- stats::p.adjust(p_raw, method = ajuste_p)

  # Quadrantes: taxa padronizada (z) vs. média espacial dos vizinhos (Wz).
  z     <- as.numeric(scale(valores))
  lag_z <- spdep::lag.listw(listw, z, zero.policy = TRUE)
  quad  <- ifelse(z > 0 & lag_z > 0, "Alto-Alto",
           ifelse(z < 0 & lag_z < 0, "Baixo-Baixo",
           ifelse(z > 0 & lag_z < 0, "Alto-Baixo",
           ifelse(z < 0 & lag_z > 0, "Baixo-Alto", "Não significativo"))))
  cluster <- ifelse(!is.na(p_adj) & p_adj < alpha, quad, "Não significativo")

  data.frame(Ii = Ii, z = z, lag_z = lag_z,
             p_valor = p_raw, p_ajustado = p_adj,
             quadrante = quad, cluster = cluster,
             stringsAsFactors = FALSE)
}

# Getis-Ord Gi* — zonas quentes/frias. Usa include.self() (o "*" de Gi*, que
# inclui a própria área no cálculo). O vetor devolvido pelo localG é o desvio
# padronizado (z-score): z alto e significativo = hot spot; z baixo negativo e
# significativo = cold spot. p bilateral pela normal (contínuo → compatível
# com a correção de múltiplos testes; ver .escolher_col_p).
getis_gi <- function(valores, nb, ajuste_p = "fdr", alpha = 0.05) {
  self_nb <- spdep::include.self(nb)
  lw_star <- spdep::nb2listw(self_nb, style = "W", zero.policy = TRUE)
  g     <- spdep::localG(valores, lw_star, zero.policy = TRUE)
  zi    <- as.numeric(g)
  p_raw <- 2 * stats::pnorm(-abs(zi))
  p_adj <- stats::p.adjust(p_raw, method = ajuste_p)
  tipo  <- ifelse(!is.na(p_adj) & p_adj < alpha & zi > 0, "Hot spot",
           ifelse(!is.na(p_adj) & p_adj < alpha & zi < 0, "Cold spot",
                  "Não significativo"))

  data.frame(Gi = zi, p_valor = p_raw, p_ajustado = p_adj, tipo = tipo,
             stringsAsFactors = FALSE)
}

# Monta a base de taxa por município ALINHADA à ordem das linhas do `geo`,
# preenchendo com 0 casos os municípios sem internação no recorte (para análise
# espacial de área, TODO município precisa entrar, não só os que tiveram caso).
# Recebe o numerador já agregado (`agg`: cod6 + internacoes do período) e a
# população municipal (`pop`: cod6 + ano + populacao). As duas variantes —
# estado único e região/Brasil — só diferem em como montam `agg`/`pop`.
.montar_base_espacial <- function(geo, agg, pop, ano_ini, ano_fim) {
  n_anos <- ano_fim - ano_ini + 1

  # População MÉDIA do período por município (pessoa-tempo por ano). Se nenhum
  # ano do recorte tiver estimativa publicada, cai na média de todos os anos.
  pop_per <- pop %>% filter(ano >= ano_ini, ano <= ano_fim) %>%
    group_by(cod6) %>% summarise(populacao = mean(populacao, na.rm = TRUE), .groups = "drop")
  if (nrow(pop_per) == 0)
    pop_per <- pop %>% group_by(cod6) %>%
      summarise(populacao = mean(populacao, na.rm = TRUE), .groups = "drop")

  cod6_geo <- substr(as.character(geo$code_muni), 1, 6)
  base <- data.frame(cod6 = cod6_geo,
                     name_muni = as.character(geo$name_muni),
                     stringsAsFactors = FALSE)

  adm_col <- if (!is.null(agg) && nrow(agg) > 0) agg[, c("cod6", "internacoes")]
             else data.frame(cod6 = character(0), internacoes = numeric(0))

  base %>%
    left_join(adm_col, by = "cod6") %>%
    left_join(pop_per, by = "cod6") %>%
    mutate(
      internacoes = coalesce(internacoes, 0),
      # Taxa média anual por 100k = casos do período / (anos × população média).
      taxa = ifelse(!is.na(populacao) & populacao > 0,
                    internacoes / n_anos / populacao * 1e5, NA_real_),
      pessoas_ano = ifelse(!is.na(populacao), populacao * n_anos, NA_real_)
    )  # já na mesma ordem das linhas de `geo`
}

# Base de taxa para UM estado. Reusa o pipeline municipal já existente para o
# numerador e a população SIDRA para o denominador de todos os municípios.
preparar_taxa_espacial <- function(geo, pasta, sistema, uf, code_state,
                                   ano_ini, ano_fim, cid_pref = character(0),
                                   faixa_eca = "geral", sinan_tipo = "3") {
  tx  <- tabela_taxa_municipio(pasta, sistema, uf, code_state,
                               ano_ini, ano_fim, cid_pref, faixa_eca, sinan_tipo = sinan_tipo)
  agg <- if (!is.null(tx) && nrow(tx) > 0)
    agregar_taxa_municipio_periodo(tx, ano_ini, ano_fim) else NULL
  pop <- carregar_populacao_municipios(uf, code_state)
  .montar_base_espacial(geo, agg, pop, ano_ini, ano_fim)
}

# ── Escala regional / nacional ───────────────────────────────────────────────
# Une vários estados numa análise espacial só (região inteira, ou Brasil).
# Diferença metodológica vs. estado único: as internações são somadas por
# município de RESIDÊNCIA varrendo os .rds de TODOS os estados do escopo, de
# modo que quem mora num estado e se interna em outro do mesmo escopo (fluxo
# entre vizinhos) seja contabilizado — senão municípios de divisa ficam
# subcontados. (Fluxo que SAI do escopo ainda não é capturado; para o Brasil
# inteiro isso é nulo, e para uma região é uma fração pequena.)

# Contorno dos municípios de vários estados, empilhados num sf só. Reaproveita
# o cache por-UF de carregar_geo_municipios() (só baixa da internet os estados
# que ainda não estiverem em data/).
carregar_geo_municipios_scope <- function(ufs, prog = NULL) {
  geos <- vector("list", length(ufs))
  for (i in seq_along(ufs)) {
    if (!is.null(prog)) prog(i / length(ufs), paste("Contorno de", ufs[i], "..."))
    geos[[i]] <- carregar_geo_municipios(ufs[i])
  }
  do.call(rbind, geos)
}

# População municipal de vários estados, empilhada (cod6 + ano + populacao).
carregar_populacao_municipios_scope <- function(ufs, prog = NULL) {
  pops <- list()
  for (i in seq_along(ufs)) {
    if (!is.null(prog)) prog(i / length(ufs), paste("População de", ufs[i], "..."))
    cs <- UF_COD_IBGE[[ufs[i]]]
    p  <- tryCatch(carregar_populacao_municipios(ufs[i], cs), error = function(e) NULL)
    if (!is.null(p)) pops[[length(pops) + 1]] <- p
  }
  if (length(pops) == 0) return(NULL)
  do.call(rbind, pops)
}

# Internações por município de residência somadas sobre os estados do escopo.
tabela_admissoes_scope <- function(pasta, sistema, ufs, ano_ini, ano_fim,
                                   cid_pref = character(0), faixa_eca = "geral", prog = NULL,
                                   sinan_tipo = "3") {
  # SINAN: o arquivo é nacional, então lê 1x por ANO (não por UF) e agrega o
  # escopo inteiro de uma vez — evita reler o arquivo nacional 27 vezes.
  if (sistema_tipo(sistema) == "SINAN") {
    anos <- ano_ini:ano_fim; frames <- list()
    for (j in seq_along(anos)) {
      if (!is.null(prog)) prog(j / length(anos), paste("Lendo SINAN", anos[j], "(nacional)..."))
      sub <- tryCatch(processar_sinan_ano(anos[j], sinan_tipo, ufs), error = function(e) NULL)
      if (is.null(sub) || nrow(sub) == 0) next
      if (!identical(faixa_eca, "geral"))
        sub <- sub[idade_eca_ok(sub$idade_anos, faixa_eca), , drop = FALSE]
      if (nrow(sub) > 0) frames[[length(frames) + 1]] <- sub
    }
    if (length(frames) == 0) return(NULL)
    return(dplyr::bind_rows(frames) %>%
      group_by(cod6) %>% summarise(internacoes = dplyr::n(), .groups = "drop"))
  }

  frames <- list()
  for (i in seq_along(ufs)) {
    if (!is.null(prog)) prog(i / length(ufs), paste("Lendo internações de", ufs[i], "..."))
    a <- tabela_admissoes_municipio(pasta, sistema, ufs[i], ano_ini, ano_fim,
                                    cid_pref, faixa_eca, ufs_keep = ufs)
    if (!is.null(a) && nrow(a) > 0) frames[[length(frames) + 1]] <- a
  }
  if (length(frames) == 0) return(NULL)
  # Cada internação aparece em exatamente um arquivo (o do estado onde ocorreu),
  # então somar por cod6 entre estados não duplica — só junta o fluxo interno.
  dplyr::bind_rows(frames) %>%
    group_by(cod6) %>% summarise(internacoes = sum(internacoes), .groups = "drop")
}

# Base de taxa para um escopo multi-estados (região / Brasil), alinhada ao geo.
preparar_taxa_espacial_scope <- function(geo, pasta, sistema, ufs,
                                         ano_ini, ano_fim, cid_pref = character(0),
                                         faixa_eca = "geral", prog = NULL, sinan_tipo = "3") {
  agg <- tabela_admissoes_scope(pasta, sistema, ufs, ano_ini, ano_fim, cid_pref, faixa_eca, prog,
                                sinan_tipo = sinan_tipo)
  pop <- carregar_populacao_municipios_scope(ufs, prog)
  if (is.null(pop)) stop("Não consegui obter população municipal para o escopo selecionado.")
  .montar_base_espacial(geo, agg, pop, ano_ini, ano_fim)
}

# Orquestra a análise espacial a partir de um sf (`geo`) e da base de taxa
# alinhada (`base`, saída de preparar_taxa_espacial). Faz o trabalho pesado de
# forma testável (sem depender de disco): monta os pesos, opcionalmente suaviza
# a taxa, e roda Moran global + LISA + Gi*. Devolve tudo já casado de volta ao
# município (por cod6) para os mapas e tabelas.
analise_espacial <- function(geo, base, tipo_w = "queen", k = 5,
                             suavizar = FALSE, ajuste_p = "fdr",
                             alpha = 0.05, n_sim = 999) {
  if (!requireNamespace("spdep", quietly = TRUE))
    stop("Pacote 'spdep' não instalado. Rode: install.packages('spdep')")

  # Só entram na análise municípios com taxa definida (população conhecida).
  # Os demais ficam como "sem dado" nos mapas, mas não podem entrar nos pesos.
  keep <- !is.na(base$taxa)
  if (sum(keep) < 3)
    stop("Menos de 3 municípios com taxa válida — insuficiente para análise espacial.")
  geo_ok  <- geo[keep, , drop = FALSE]
  base_ok <- base[keep, , drop = FALSE]

  pesos <- construir_pesos_espaciais(geo_ok, tipo = tipo_w, k = k)

  # Valor analisado: taxa bruta ou suavizada por Bayes empírico.
  if (isTRUE(suavizar)) {
    valores <- suavizar_taxa_eb(base_ok$internacoes, base_ok$pessoas_ano)
    rotulo_valor <- "Taxa suavizada (Bayes empírico)"
  } else {
    valores <- base_ok$taxa
    rotulo_valor <- "Taxa bruta por 100k"
  }

  glob <- moran_global(valores, pesos$listw, n_sim = n_sim,
                       casos = base_ok$internacoes, populacao = base_ok$pessoas_ano)
  lisa <- moran_local(valores, pesos$listw, ajuste_p = ajuste_p, alpha = alpha)
  gi   <- getis_gi(valores, pesos$nb, ajuste_p = ajuste_p, alpha = alpha)

  # Uma tabela por município com tudo casado (cod6/nome/taxa + cluster/gi).
  det <- data.frame(
    cod6        = base_ok$cod6,
    name_muni   = base_ok$name_muni,
    internacoes = base_ok$internacoes,
    taxa        = base_ok$taxa,
    valor       = valores,
    LISA_Ii     = lisa$Ii,
    LISA_p      = lisa$p_ajustado,
    LISA        = lisa$cluster,
    Gi_z        = gi$Gi,
    Gi_p        = gi$p_ajustado,
    Gi          = gi$tipo,
    stringsAsFactors = FALSE
  )

  list(
    global      = glob,
    detalhe      = det,
    valores      = valores,
    listw        = pesos$listw,
    pesos        = pesos,
    rotulo_valor = rotulo_valor,
    suavizado    = isTRUE(suavizar),
    ajuste_p     = ajuste_p,
    alpha        = alpha,
    n_areas      = nrow(base_ok)
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# DENSIDADE DE KERNEL (KDE espacial)
# ─────────────────────────────────────────────────────────────────────────────
# Estima uma superfície contínua a partir dos CENTROIDES municipais (o app não
# tem coordenada de caso individual — só agregado municipal), ponderando por
# casos e por população. Duas leituras:
#   • "casos" — intensidade de casos suavizada (onde há mais casos, cru).
#   • "risco" — log da razão entre a densidade de casos e a densidade
#     populacional (log-risco relativo): onde há mais casos DO QUE o esperado
#     pela população local. É o análogo agregado do relative-risk surface
#     (sparr): desconta simplesmente "onde as pessoas moram".
# KernSmooth/MASS não fazem KDE ponderado, então a soma gaussiana ponderada é
# feita à mão em forma matricial (rápida mesmo com os 5570 municípios).
.kde2d_pond <- function(x, y, w, hx, hy, gx, gy) {
  w  <- w / sum(w)
  Zx <- outer(gx, x, function(a, b) dnorm((a - b) / hx))   # length(gx) x n
  Zy <- outer(gy, y, function(a, b) dnorm((a - b) / hy))   # length(gy) x n
  Zxw <- sweep(Zx, 2, w, `*`)
  (Zxw %*% t(Zy)) / (hx * hy)                              # length(gx) x length(gy)
}

# Larguras de banda ALINHADAS À LITERATURA (não um multiplicador arbitrário):
#   • "sj"        — plug-in de Sheather-Jones (KernSmooth::dpik), data-driven.
#   • "silverman" — regra do polegar de Silverman (bw.nrd0), normal-reference.
#   • "scott"     — regra de Scott (bw.nrd), normal-reference.
#   • número (km) — RAIO FIXO em km (25/50/100/150/200...), como nos estudos que
#     reportam banda fixa; convertido de km para graus por eixo usando a latitude
#     média (1° lat ≈ 110,6 km; 1° lon ≈ 111,3·cos(lat) km), ficando isotrópico
#     em km.
SINAN_KDE_BANDAS <- c(
  "Sheather-Jones (plug-in, automático)" = "sj",
  "Silverman (regra do polegar)"          = "silverman",
  "Scott (normal-reference)"              = "scott",
  "Raio fixo 25 km"  = "25",  "Raio fixo 50 km"  = "50",
  "Raio fixo 100 km" = "100", "Raio fixo 150 km" = "150",
  "Raio fixo 200 km" = "200")

.kde_banda <- function(x, y, banda) {
  km <- suppressWarnings(as.numeric(banda))
  if (!is.na(km) && km > 0) {                        # raio fixo em km -> graus
    lat0 <- mean(y)
    hy <- km / 110.574
    hx <- km / (111.320 * cos(lat0 * pi / 180))
    return(list(hx = hx, hy = hy, rotulo = sprintf("raio fixo %g km", km)))
  }
  metodo <- match.arg(as.character(banda), c("sj", "silverman", "scott"))
  bw1 <- switch(metodo,
    sj        = function(v) tryCatch(KernSmooth::dpik(v), error = function(e) stats::bw.nrd0(v)),
    silverman = stats::bw.nrd0,
    scott     = stats::bw.nrd)
  hx <- bw1(x); hy <- bw1(y)
  rot <- c(sj = "Sheather-Jones (plug-in)", silverman = "Silverman", scott = "Scott")[metodo]
  list(hx = hx, hy = hy, rotulo = unname(rot))
}

densidade_kernel <- function(geo, base, tipo = c("risco", "casos"),
                             banda = "sj", n_grid = 220, mascara = TRUE) {
  tipo <- match.arg(tipo)
  for (p in c("KernSmooth", "sf"))
    if (!requireNamespace(p, quietly = TRUE))
      stop(sprintf("Pacote '%s' não instalado.", p))

  keep <- !is.na(base$populacao) & base$populacao > 0
  g <- geo[keep, , drop = FALSE]; b <- base[keep, , drop = FALSE]
  if (nrow(b) < 5) stop("Poucos municípios com população para estimar a densidade.")

  ct <- suppressWarnings(sf::st_coordinates(sf::st_centroid(sf::st_geometry(g))))
  x  <- ct[, 1]; y <- ct[, 2]

  bw <- .kde_banda(x, y, banda)
  hx <- bw$hx; hy <- bw$hy
  if (!is.finite(hx) || hx <= 0) hx <- diff(range(x)) / 20
  if (!is.finite(hy) || hy <= 0) hy <- diff(range(y)) / 20

  gx <- seq(min(x) - 2 * hx, max(x) + 2 * hx, length.out = n_grid)
  gy <- seq(min(y) - 2 * hy, max(y) + 2 * hy, length.out = n_grid)

  dc <- .kde2d_pond(x, y, pmax(b$internacoes, 0) + 1e-9, hx, hy, gx, gy)  # densidade de casos
  if (tipo == "casos") {
    z <- dc
    rotulo <- "Densidade de casos (KDE)"
  } else {
    dp <- .kde2d_pond(x, y, b$populacao, hx, hy, gx, gy)                  # densidade populacional
    eps <- stats::quantile(dp[dp > 0], 0.05, na.rm = TRUE)
    z <- log((dc + eps) / (dp + eps))                                    # log-risco relativo
    rotulo <- "Log-risco relativo (KDE casos ÷ população)"
  }

  grid <- data.frame(x = rep(gx, times = length(gy)),
                     y = rep(gy, each  = length(gx)),
                     z = as.numeric(z))

  # Contorno dissolvido do escopo. Feito em modo PLANAR (s2 desligado): o motor
  # esférico s2 rejeita os vértices degenerados que os polígonos simplificados do
  # geobr têm, e o buffer planar fecha as micro-frestas de divisa que, senão,
  # sobram como fiapos internos "furando" o mapa. Restaura o s2 ao sair.
  old_s2 <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)

  geom_ok  <- suppressWarnings(sf::st_make_valid(sf::st_geometry(g)))
  contorno <- suppressWarnings(sf::st_buffer(sf::st_union(geom_ok), 0.005))

  # Máscara: mantém a grade RETANGULAR completa (z = NA fora do contorno), para
  # o raster ficar liso; o de fora vira transparente no gráfico.
  if (isTRUE(mascara)) {
    pts <- sf::st_as_sf(grid, coords = c("x", "y"), crs = sf::st_crs(g))
    dentro <- lengths(suppressMessages(sf::st_intersects(pts, contorno))) > 0
    grid$z[!dentro] <- NA_real_
  }

  list(grid = grid, contorno = contorno, tipo = tipo, rotulo = rotulo,
       banda_rotulo = bw$rotulo, bandwidth = c(hx = hx, hy = hy), n_areas = nrow(b),
       pontos = data.frame(x = x, y = y, casos = b$internacoes,
                           name_muni = b$name_muni, stringsAsFactors = FALSE))
}

# ─────────────────────────────────────────────────────────────────────────────
# SCAN STATISTICS ESPACIAL (estatística de varredura de Kulldorff)
# ─────────────────────────────────────────────────────────────────────────────
# Detecta aglomerados (janelas circulares crescentes sobre os centroides) cuja
# taxa interna difere do esperado sob homogeneidade — modelo de Poisson, razão
# de verossimilhança, p-valor por Monte Carlo. Implementado direto em R (não usa
# mais smerc): a janela cresce pelos vizinhos mais próximos e é limitada por um
# NÚMERO DE MUNICÍPIOS `k` (a pedido — 15/20/25/30), em vez de uma fração da
# população. Sem binário externo (SaTScan) nem dependência extra.
#
# Distância grande-círculo (haversine) para ordenar os vizinhos de cada centro.
.dist_haversine <- function(lon, lat, i) {
  R <- 6371
  dlon <- (lon - lon[i]) * pi / 180; dlat <- (lat - lat[i]) * pi / 180
  a <- sin(dlat / 2)^2 + cos(lat[i] * pi / 180) * cos(lat * pi / 180) * sin(dlon / 2)^2
  2 * R * asin(pmin(1, sqrt(a)))
}

scan_espacial <- function(geo, base, k = 20L, n_sim = 999, alpha = 0.05, seed = 1) {
  if (!requireNamespace("sf", quietly = TRUE))
    stop("Pacote 'sf' não instalado.")

  keep <- !is.na(base$populacao) & base$populacao > 0
  g <- geo[keep, , drop = FALSE]; b <- base[keep, , drop = FALSE]
  n <- nrow(b)
  if (n < 5) stop("Poucos municípios com população para a varredura.")
  k <- max(2L, min(as.integer(k), n - 1L))

  ct  <- suppressWarnings(sf::st_coordinates(sf::st_centroid(sf::st_geometry(g))))
  lon <- ct[, 1]; lat <- ct[, 2]
  casos <- as.numeric(round(pmax(b$internacoes, 0)))
  pop   <- as.numeric(b$pessoas_ano)
  C <- sum(casos); P <- sum(pop)
  if (C < 2) stop("Casos insuficientes para a varredura espacial.")

  # Zonas aninhadas: para cada centro i, os k municípios mais próximos (o 1º é o
  # próprio i). idxZ[i, 1:j] = zona do centro i com j municípios.
  idxZ <- matrix(NA_integer_, n, k)
  for (i in seq_len(n)) idxZ[i, ] <- order(.dist_haversine(lon, lat, i))[seq_len(k)]

  # Esperado acumulado por zona (fixo entre simulações).
  cumcols <- function(M) { for (j in 2:ncol(M)) M[, j] <- M[, j - 1] + M[, j]; M }
  popZ <- cumcols(matrix(pop[idxZ], n, k))
  EZ   <- C * popZ / P

  # Razão de verossimilhança de Poisson para aglomerados de ALTA taxa.
  llr <- function(OZ) {
    t1 <- ifelse(OZ > 0, OZ * log(OZ / EZ), 0)
    t2 <- ifelse((C - OZ) > 0 & (C - EZ) > 0, (C - OZ) * log((C - OZ) / (C - EZ)), 0)
    val <- t1 + t2
    val[!(OZ > EZ)] <- 0                      # só excesso (taxa alta)
    val[!is.finite(val)] <- 0
    val
  }
  stat_zonas <- function(cvec) llr(cumcols(matrix(cvec[idxZ], n, k)))

  Lobs <- stat_zonas(casos)                   # n x k

  # Distribuição nula do MÁXIMO por Monte Carlo: realoca os C casos entre os
  # municípios ~ multinomial(pop/P) e recalcula o maior LLR.
  set.seed(seed)
  prob    <- pop / P
  maxdist <- vapply(seq_len(n_sim),
                    function(s) max(stat_zonas(as.numeric(rmultinom(1, C, prob)[, 1]))),
                    numeric(1))
  p_de <- function(L) (1 + sum(maxdist >= L)) / (1 + n_sim)

  # Aglomerado mais provável + secundários NÃO sobrepostos, em ordem de LLR.
  ord      <- order(as.numeric(Lobs), decreasing = TRUE)
  reivind  <- logical(n)                       # municípios já alocados a um cluster
  muni_cl  <- rep(0L, n)
  linhas   <- list(); sig <- 0L
  for (idx in ord) {
    L <- Lobs[idx]
    if (!is.finite(L) || L <= 0) break
    ci  <- ((idx - 1L) %% n) + 1L               # centro
    jz  <- ((idx - 1L) %/% n) + 1L              # nº de municípios na zona
    areas <- idxZ[ci, seq_len(jz)]
    if (any(reivind[areas])) next               # sobrepõe cluster já aceito
    p <- p_de(L)
    if (p > alpha) break                        # LLR decrescente → resto também
    sig <- sig + 1L
    reivind[areas] <- TRUE
    muni_cl[areas] <- sig
    O <- sum(casos[areas]); E <- C * sum(pop[areas]) / P
    linhas[[sig]] <- data.frame(
      cluster = sig, n_munic = length(areas),
      casos_obs = O, casos_esp = E,
      RR = (O / E) / ((C - O) / (C - E)), LLR = L, p_valor = p,
      stringsAsFactors = FALSE)
  }
  clusters <- if (length(linhas)) do.call(rbind, linhas) else
    data.frame(cluster = integer(0), n_munic = integer(0), casos_obs = numeric(0),
               casos_esp = numeric(0), RR = numeric(0), LLR = numeric(0),
               p_valor = numeric(0))

  det <- data.frame(cod6 = b$cod6, name_muni = b$name_muni,
                    internacoes = b$internacoes, taxa = b$taxa,
                    cluster = muni_cl, stringsAsFactors = FALSE)

  list(clusters = clusters, detalhe = det, n_sig = sig,
       n_areas = n, alpha = alpha, n_sim = n_sim, k = k, geo_ok = g)
}
