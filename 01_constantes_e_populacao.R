# 01_constantes_e_populacao.R
# ---------------------------------------------------------------------------
# Constantes (UFs, regioes, faixas etarias, mapeamentos IBGE) e leitura das estimativas populacionais do IBGE usadas como denominador das taxas.
#
# Funcoes do EpiSUS efetivamente utilizadas na analise do artigo
# "Escorpionismo no Brasil, 2016-2025". Codigo extraido VERBATIM do
# modulo de logica pura core_datasus.R do aplicativo EpiSUS (Shiny),
# sem a camada de interface. R 4.6.0.
# ---------------------------------------------------------------------------

# core_datasus.R — Lógica pura do app_datasus.R (constantes, coleta,
# classificação, séries temporais, estatística, gráficos).
#
# Extraído de app_datasus.R para ser testável sem depender do Shiny.
# Sourced automaticamente pelo Shiny (arquivos em R/ são carregados antes
# do app.R) e explicitamente pelos testes em tests/testthat/.

# ─────────────────────────────────────────────────────────────────────────────
# CONSTANTES
# ─────────────────────────────────────────────────────────────────────────────

ANOS <- 2016:2026
MESES_PT <- c("Janeiro","Fevereiro","Março","Abril","Maio","Junho",
              "Julho","Agosto","Setembro","Outubro","Novembro","Dezembro")

PERIOD_BINS   <- c(2015, 2019, 2021, 2025)
PERIOD_LABELS <- c("Pre","Pandemia","Pos")
PERIOD_DISPLAY <- c(Pre      = "Pré-pandemia (2016–2019)",
                    Pandemia = "Pandemia (2020–2021)",
                    Pos      = "Pós-pandemia (2022–2025)")

UF_NOME <- c(
  # Norte
  AC="Acre", AP="Amapá", AM="Amazonas", PA="Pará", RO="Rondônia",
  RR="Roraima", TO="Tocantins",
  # Nordeste
  AL="Alagoas", BA="Bahia", CE="Ceará", MA="Maranhão",
  PB="Paraíba", PE="Pernambuco", PI="Piauí",
  RN="Rio Grande do Norte", SE="Sergipe",
  # Centro-Oeste
  DF="Distrito Federal", GO="Goiás", MS="Mato Grosso do Sul", MT="Mato Grosso",
  # Sudeste
  ES="Espírito Santo", MG="Minas Gerais", RJ="Rio de Janeiro", SP="São Paulo",
  # Sul
  PR="Paraná", RS="Rio Grande do Sul", SC="Santa Catarina"
)

# Pseudo-códigos de região (não são UFs reais) usados por expand_uf() para
# selecionar todos os estados de uma região de uma vez, ou o Brasil inteiro.
# "SE" já é o código real de Sergipe, então a região Sudeste usa "SUD" para
# não colidir.
REGIAO_UF <- list(
  N   = c("AC","AP","AM","PA","RO","RR","TO"),
  NE  = c("AL","BA","CE","MA","PB","PE","PI","RN","SE"),
  CO  = c("DF","GO","MS","MT"),
  SUD = c("ES","MG","RJ","SP"),
  S   = c("PR","RS","SC")
)
REGIAO_NOME <- c(N="Norte", NE="Nordeste", CO="Centro-Oeste",
                  SUD="Sudeste", S="Sul", BR="Brasil (todos os estados)")

# ── Índices EPIDEMIOLOGIA.csv (R 1-indexed = Python 0-indexed + 1) ──────────

# Raça/cor: NE=10-12, depois cada estado +4 (3 raças + 1 blank)
# Ordem: NE, MA, PI, CE, RN, PB, PE, AL, BA, SE
RACE_ROWS <- list(
  Nordeste              = list(Branca=11L, Preta=12L, Parda=13L),
  Maranhão              = list(Branca=15L, Preta=16L, Parda=17L),
  Piauí                 = list(Branca=19L, Preta=20L, Parda=21L),
  Ceará                 = list(Branca=23L, Preta=24L, Parda=25L),
  `Rio Grande do Norte` = list(Branca=27L, Preta=28L, Parda=29L),
  Paraíba               = list(Branca=31L, Preta=32L, Parda=33L),
  Pernambuco            = list(Branca=35L, Preta=36L, Parda=37L),
  Alagoas               = list(Branca=39L, Preta=40L, Parda=41L),
  Bahia                 = list(Branca=43L, Preta=44L, Parda=45L),
  Sergipe               = list(Branca=47L, Preta=48L, Parda=49L)
)

# Sexo: NE=57, MA=58, PI=59, CE=60, RN=61, PB=62, PE=63, AL=64, BA=65, SE=66
SEX_ROWS <- c(
  Nordeste=57L, Maranhão=58L, Piauí=59L, Ceará=60L,
  `Rio Grande do Norte`=61L, Paraíba=62L, Pernambuco=63L,
  Alagoas=64L, Bahia=65L, Sergipe=66L
)

# Idade: 10 linhas por UF (2016-2025). Col V4=total, V13=<15, V22=15-59, V28=60+
AGE_START <- c(
  Nordeste=71L, Maranhão=81L, Piauí=91L, Ceará=101L,
  `Rio Grande do Norte`=111L, Paraíba=121L, Pernambuco=131L,
  Alagoas=141L, Bahia=151L, Sergipe=161L
)

# ─────────────────────────────────────────────────────────────────────────────
# POPULAÇÕES (EPIDEMIOLOGIA.csv)
# ─────────────────────────────────────────────────────────────────────────────

parse_n <- function(v) suppressWarnings(as.numeric(gsub("[^0-9]", "", as.character(v))))

nome_estados <- function(ufs) unname(UF_NOME[ufs])

carregar_populacao <- function(path) {
  epi <- read.csv(path, header=FALSE, stringsAsFactors=FALSE, colClasses="character")

  # ── Raça/cor (mil pessoas × 1000, anos 2016-2025 = cols V3:V12) ──
  pop_raca <- lapply(names(RACE_ROWS), function(est) {
    do.call(rbind, lapply(c("Branca","Preta","Parda"), function(cor) {
      ridx <- RACE_ROWS[[est]][[cor]]
      ncols <- min(12, ncol(epi))
      vals  <- sapply(epi[ridx, 3:ncols], parse_n) * 1000
      data.frame(estado=est, cor=cor, year=ANOS[seq_along(vals)],
                 pop=as.numeric(vals), stringsAsFactors=FALSE)
    }))
  })
  pop_raca <- do.call(rbind, pop_raca)

  # ── Sexo (mil pessoas × 1000) ──
  # Para ano i (1..10): Masculino = col (1+3i), Feminino = col (2+3i)
  pop_sexo <- lapply(names(SEX_ROWS), function(est) {
    ridx <- SEX_ROWS[[est]]
    do.call(rbind, lapply(1:10, function(i) {
      yr       <- 2015L + i
      col_m    <- 1L + 3L * i
      col_f    <- 2L + 3L * i
      data.frame(
        estado = est,
        year   = yr,
        sexo   = c("Masculino", "Feminino"),
        pop    = c(parse_n(epi[ridx, col_m]) * 1000,
                   parse_n(epi[ridx, col_f]) * 1000),
        stringsAsFactors = FALSE
      )
    }))
  })
  pop_sexo <- do.call(rbind, pop_sexo)

  # ── Faixa etária e total (valores absolutos) ──
  # LIMITAÇÃO: este parser legado (EPIDEMIOLOGIA.csv, só Nordeste) só traz 3
  # colunas pré-agregadas de idade (<15/15-59/60+) — não dá pra gerar as 7
  # faixas mais finas (FAIXA_ETARIA_NIVEIS) a partir dele, só o carregar_
  # populacao_br() (Tabela 6407 do IBGE) tem granularidade suficiente. Se
  # esse formato antigo for usado, a aba Faixa Etária não vai casar as
  # categorias com clf_faixa() (que agora produz as 8 faixas finas).
  pop_faixa <- lapply(names(AGE_START), function(est) {
    r0 <- AGE_START[[est]]
    do.call(rbind, lapply(0:9, function(i) {
      yr  <- 2016L + i
      row <- epi[r0 + i, ]
      data.frame(
        estado = est,
        year   = yr,
        grupo  = c("<15","15-59","60+","_total"),
        pop    = c(parse_n(row[13]), parse_n(row[22]),
                   parse_n(row[28]), parse_n(row[4])),
        stringsAsFactors = FALSE
      )
    }))
  })
  pop_faixa <- do.call(rbind, pop_faixa)

  pop_total <- pop_faixa %>% filter(grupo=="_total") %>%
    select(estado, year, pop)
  pop_faixa <- pop_faixa %>% filter(grupo!="_total")

  list(total=pop_total, sexo=pop_sexo, faixa=pop_faixa, raca=pop_raca)
}

# ─────────────────────────────────────────────────────────────────────────────
# POPULAÇÃO — BRASIL INTEIRO (EPIDEMIOLOGIABR.csv, Tabelas 6407+6408 IBGE)
# ─────────────────────────────────────────────────────────────────────────────
# Formato bem diferente do EPIDEMIOLOGIA.csv antigo: 1 linha por
# (estado, categoria), colunas repetem (Total,Homens,Mulheres) para cada ano
# (2012 em diante). Cobre os 27 estados — este parser casa por NOME (estado +
# rótulo da categoria), não por número de linha fixo, então funciona pra
# qualquer estado sem precisar mapear índices na mão. Cobre idade, sexo E
# raça/cor (Tabela 6408) — ao contrário de uma suposição inicial, o export
# do IBGE tem sim população por raça/cor pro Brasil inteiro.
# NOTA IMPORTANTE: o CSV exportado pelo IBGE (SIDRA) empilha DUAS tabelas
# num arquivo só — Tabela 6407 (idade × sexo) e, mais abaixo, Tabela 6408
# (cor/raça × sexo) — cada uma com seu próprio bloco de cabeçalho (título,
# linha de variável, linha de anos, linha de sexo) e um rodapé "Fonte: IBGE."
# antes da próxima. Um parser que lê "tudo depois da linha 6 até o fim do
# arquivo" (jeito ingênuo, usado numa versão inicial deste parser) trata as
# duas tabelas como uma só e DUPLICA a população total de cada estado (uma
# vez por tabela) — bug real, pego rodando os testes contra o CSV de
# verdade. .parse_bloco_ibge() isola cada tabela pelos próprios marcadores
# de título/rodapé, então isso não depende de números de linha fixos.
#
# 8 faixas (FAIXA_ETARIA_NIVEIS), construídas somando as categorias-folha
# mais finas que a Tabela 6407 oferece (nunca as agregadas "5 a 13 anos",
# "14 a 17 anos" ou "60 anos ou mais" quando uma quebra mais fina existe pro
# mesmo intervalo, pra não somar categoria com sua própria subcategoria).
# A primeira década é quebrada em dois quinquênios: a mortalidade/morbidade
# hospitalar da primeira infância (0-4, onde se concentram as causas
# perinatais e as infecciosas) não se parece com a da idade escolar (5-9), e
# somar as duas numa faixa só achata justamente o contraste de maior
# interesse pediátrico. Os dois quinquênios têm balde PRÓPRIO na Tabela 6407,
# então o denominador continua exato — nenhuma repartição por suposição de
# uniformidade, ao contrário do que get_pop_eca() precisa fazer no corte de
# 12 anos do ECA.
AGE_GROUPS_0_4   <- c("0 a 4 anos")
AGE_GROUPS_5_9   <- c("5 a 9 anos")
AGE_GROUPS_10_19 <- c("10 a 13 anos", "14 a 15 anos", "16 a 17 anos", "18 a 19 anos")
AGE_GROUPS_20_29 <- c("20 a 24 anos", "25 a 29 anos")
AGE_GROUPS_30_39 <- c("30 a 39 anos")
AGE_GROUPS_40_49 <- c("40 a 49 anos")
AGE_GROUPS_50_59 <- c("50 a 59 anos")
AGE_GROUPS_60P   <- c("60 anos ou mais")

# Isola um bloco de tabela do SIDRA/IBGE dentro do CSV empilhado, a partir do
# título (ex.: "^Tabela 6407") até o próximo rodapé "Fonte: ...". Formato
# fixo de 5 linhas de cabeçalho (título / variável / rótulo / anos / sexo),
# comum às tabelas de população do IBGE usadas aqui.
.parse_bloco_ibge <- function(raw, titulo_regex) {
  col1_txt <- trimws(as.character(raw[[1]]))
  idx_titulo <- which(grepl(titulo_regex, col1_txt, ignore.case = TRUE))
  if (length(idx_titulo) == 0) return(NULL)
  i0 <- idx_titulo[1]

  linha_ano <- as.character(unlist(raw[i0 + 3, ], use.names = FALSE))
  linha_sex <- as.character(unlist(raw[i0 + 4, ], use.names = FALSE))
  for (j in seq_along(linha_ano)) {
    if (is.na(linha_ano[j]) || trimws(linha_ano[j]) == "")
      linha_ano[j] <- if (j > 1) linha_ano[j - 1] else NA_character_
  }
  anos_col <- suppressWarnings(as.integer(linha_ano))

  i_ini <- i0 + 5
  resto <- col1_txt[i_ini:length(col1_txt)]
  idx_fonte <- which(grepl("^Fonte:", resto, ignore.case = TRUE))
  i_fim <- if (length(idx_fonte) > 0) i_ini + idx_fonte[1] - 2 else nrow(raw)
  if (i_fim < i_ini) return(NULL)

  dados <- raw[i_ini:i_fim, , drop = FALSE]
  estado_col <- trimws(dados[[1]])
  manter <- !is.na(estado_col) & estado_col != "" & estado_col != "Brasil"
  dados      <- dados[manter, , drop = FALSE]
  estado_col <- trimws(dados[[1]])
  grupo_col  <- trimws(dados[[2]])

  col_idx_total  <- which(linha_sex == "Total"    & !is.na(anos_col))
  col_idx_homens <- which(linha_sex == "Homens"   & !is.na(anos_col))
  col_idx_mulher <- which(linha_sex == "Mulheres" & !is.na(anos_col))

  extrai <- function(col_idxs) {
    do.call(rbind, lapply(col_idxs, function(cidx) {
      data.frame(estado = estado_col, grupo = grupo_col, year = anos_col[cidx],
                 pop = parse_n(dados[[cidx]]) * 1000, stringsAsFactors = FALSE)
    }))
  }

  list(total = extrai(col_idx_total), homens = extrai(col_idx_homens),
       mulheres = extrai(col_idx_mulher))
}

carregar_populacao_br <- function(path) {
  raw <- read.csv(path, header = FALSE, stringsAsFactors = FALSE,
                   colClasses = "character")

  # ── Tabela 6407: idade × sexo ──────────────────────────────────────────
  b1 <- .parse_bloco_ibge(raw, "^Tabela 6407")
  if (is.null(b1)) stop("Não achei 'Tabela 6407' no CSV — formato inesperado.")
  tot <- b1$total

  pop_total <- tot[tot$grupo == "Total", c("estado", "year", "pop")]

  # Colunas de idade mais finas que a Tabela 6407 oferece (sem somar em
  # década) — usadas por get_pop_eca() pros presets "criança"/"criança e
  # adolescente" do filtro etário, que cortam em 12 e 18 anos (não em
  # décadas). Mantém os rótulos originais do IBGE (ex.: "10 a 13 anos").
  pop_faixa_fina <- tot[tot$grupo != "Total", c("estado", "grupo", "year", "pop")]

  soma_grupo <- function(rotulos, nome_bucket) {
    sub <- tot[tot$grupo %in% rotulos, ]
    agg <- sub %>% group_by(estado, year) %>%
      summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")
    agg$grupo <- nome_bucket
    agg
  }
  pop_faixa <- bind_rows(
    soma_grupo(AGE_GROUPS_0_4,   "0-4"),
    soma_grupo(AGE_GROUPS_5_9,   "5-9"),
    soma_grupo(AGE_GROUPS_10_19, "10-19"),
    soma_grupo(AGE_GROUPS_20_29, "20-29"),
    soma_grupo(AGE_GROUPS_30_39, "30-39"),
    soma_grupo(AGE_GROUPS_40_49, "40-49"),
    soma_grupo(AGE_GROUPS_50_59, "50-59"),
    soma_grupo(AGE_GROUPS_60P,   "60+")
  )

  hom <- b1$homens[b1$homens$grupo == "Total", ]
  mul <- b1$mulheres[b1$mulheres$grupo == "Total", ]
  pop_sexo <- rbind(
    data.frame(estado = hom$estado, year = hom$year, sexo = "Masculino",
               pop = hom$pop, stringsAsFactors = FALSE),
    data.frame(estado = mul$estado, year = mul$year, sexo = "Feminino",
               pop = mul$pop, stringsAsFactors = FALSE)
  )

  # ── Tabela 6408: cor/raça × sexo (opcional — nem todo export do SIDRA
  # inclui essa tabela; se não achar, pop_raca fica NULL e o app avisa o
  # usuário em vez de travar) ─────────────────────────────────────────────
  b2 <- .parse_bloco_ibge(raw, "^Tabela 6408")
  pop_raca <- NULL
  if (!is.null(b2)) {
    tot2 <- b2$total
    # Só Branca/Preta/Parda têm equivalente direto em clf_raca() (códigos
    # 1/2/3 do RACA_COR do DATASUS); Amarela/Indígena não são quebradas
    # nesta tabela do IBGE (ficam soma-only dentro de "Total").
    pop_raca <- tot2[tot2$grupo %in% c("Branca", "Preta", "Parda"), ]
    names(pop_raca)[names(pop_raca) == "grupo"] <- "cor"
    pop_raca <- pop_raca[, c("estado", "cor", "year", "pop")]
  }

  list(total = pop_total, sexo = pop_sexo, faixa = pop_faixa, faixa_fina = pop_faixa_fina, raca = pop_raca)
}

# Detecta automaticamente qual dos dois formatos de CSV foi enviado (Tabela
# 6407 do IBGE, Brasil inteiro x EPIDEMIOLOGIA.csv antigo, só Nordeste com
# raça/cor) pela primeira linha do arquivo, e chama o parser certo — assim a
# UI só precisa de um único ponto de entrada, upload manual ou automático.
carregar_populacao_auto <- function(path) {
  primeira_linha <- tryCatch(readLines(path, n = 1, warn = FALSE),
                              error = function(e) "")
  if (grepl("Tabela 6407|Grupo de idade", primeira_linha, ignore.case = TRUE)) {
    carregar_populacao_br(path)
  } else {
    carregar_populacao(path)
  }
}

get_pop_total <- function(pop, nomes, yr) {
  v <- pop$total %>% filter(estado %in% nomes, year==yr)
  if (nrow(v)==0) NA_real_ else sum(v$pop, na.rm=TRUE)
}

get_pop_grupo <- function(pop_df, nomes, col_nm, grupo_val, yr) {
  # pop_df pode ser NULL (ex.: população por raça indisponível fora do
  # Nordeste, que vem só do EPIDEMIOLOGIA.csv antigo) — não trava, só
  # devolve NA pra sinalizar "sem denominador" em vez de erro.
  if (is.null(pop_df)) return(NA_real_)
  v <- pop_df %>% filter(estado %in% nomes, year==yr, .data[[col_nm]]==grupo_val)
  if (nrow(v)==0) NA_real_ else sum(v$pop, na.rm=TRUE)
}
