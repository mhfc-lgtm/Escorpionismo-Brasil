# 06_graficos_e_mapas.R
# ---------------------------------------------------------------------------
# Geracao das figuras e mapas do artigo: tema/paletas, graficos de tendencia e sazonalidade, forest plots, proporcoes anuais, mapas coropleticos (leaflet + geobr/sf), mapas de aglomerados LISA/Gi*, superficies de Kernel e diagrama de espalhamento de Moran.
#
# Funcoes do EpiSUS efetivamente utilizadas na analise do artigo
# "Escorpionismo no Brasil, 2016-2025". Codigo extraido VERBATIM do
# modulo de logica pura core_datasus.R do aplicativo EpiSUS (Shiny),
# sem a camada de interface. R 4.6.0.
# ---------------------------------------------------------------------------

# ─────────────────────────────────────────────────────────────────────────────
# GRÁFICOS
# ─────────────────────────────────────────────────────────────────────────────

# ── Paleta do tema (cinza-escuro + vermelho + dourado) ────────────────────────
TEMA_BG      <- "#17181b"
TEMA_BG_CARD <- "#212226"
TEMA_FG      <- "#e7e4de"
TEMA_MUTED   <- "#9a9a9e"
TEMA_BORDA   <- "#34353a"
TEMA_RED     <- "#c0392b"
TEMA_GOLD    <- "#d4af37"

C_A <- TEMA_RED; C_R <- TEMA_GOLD

# ── Cores "de tinta" dos gráficos, dirigidas por options() ─────────────────────
# Por padrão devolvem o tema escuro do app (tela não muda em nada). Na hora de
# EXPORTAR, salvar_grafico_hd() troca essas opções para um tema claro e
# RECONSTRÓI o gráfico — necessário porque o ggplot "assa" as cores no momento
# da construção, então mudar só o fundo do ggsave() deixaria texto claro
# (TEMA_FG) invisível sobre o branco. Todo gg_* usa estes acessores no lugar das
# constantes para poder alternar escuro/claro.
.plot_bg    <- function() getOption("episus.plot_bg",    TEMA_BG_CARD)
.plot_ink   <- function() getOption("episus.plot_ink",   TEMA_FG)
.plot_muted <- function() getOption("episus.plot_muted", TEMA_MUTED)
.plot_grid  <- function() getOption("episus.plot_grid",  TEMA_BORDA)

# ── Esquemas de cores selecionáveis pros gráficos ──────────────────────────
# Cada paleta tem: a/r (cor do grupo de interesse / referência, usada em
# tendência, sazonalidade), sig/ns (significativo / não-significativo, usada
# em forest plots e ranking), e cat (paleta categórica com ~10 cores, usada
# em pizza/histograma/heatmap quando há muitas categorias, ex.: 27 estados).
PALETAS <- list(
  "Vermelho & Dourado (padrão)" = list(
    a = "#c0392b", r = "#d4af37", sig = "#c0392b", ns = "#d4af37",
    baixo = "#f1c68a", alto = "#c0392b",
    cat = c("#c0392b","#d4af37","#8e44ad","#2980b9","#27ae60","#e67e22",
            "#16a085","#7f8c8d","#e74c3c","#f1c40f")
  ),
  "Azul & Laranja" = list(
    a = "#2980b9", r = "#e67e22", sig = "#2980b9", ns = "#e67e22",
    baixo = "#a9d3ea", alto = "#2980b9",
    cat = c("#2980b9","#e67e22","#27ae60","#c0392b","#8e44ad","#16a085",
            "#f1c40f","#7f8c8d","#d35400","#2c3e50")
  ),
  "Verde & Roxo" = list(
    a = "#27ae60", r = "#8e44ad", sig = "#27ae60", ns = "#8e44ad",
    baixo = "#a8e0c0", alto = "#27ae60",
    cat = c("#27ae60","#8e44ad","#e67e22","#2980b9","#c0392b","#f1c40f",
            "#16a085","#d35400","#7f8c8d","#e74c3c")
  ),
  "Viridis (alto contraste)" = list(
    a = "#3b528b", r = "#fde725", sig = "#21908d", ns = "#fde725",
    baixo = "#fde725", alto = "#440154",
    cat = c("#440154","#472d7b","#3b528b","#2c728e","#21908d","#27ad81",
            "#5dc863","#aadc32","#fde725","#f0f921")
  )
)
# Cor fixa pra "sem dado" nos mapas/heatmaps — sempre um cinza neutro, longe
# de qualquer paleta de gradiente (vermelho, dourado, verde, roxo, viridis),
# pra nunca ser confundida com um valor real baixo. Ver bug real encontrado:
# cor_baixo e na.color usando a mesma cor deixavam "Ceará com taxa baixa" e
# "estado sem dado nenhum" visualmente idênticos no mapa.
COR_SEM_DADO <- "#4a4b52"
PALETA_PADRAO <- PALETAS[[1]]

theme_app <- function(base_size = 13) {
  bg <- .plot_bg(); ink <- .plot_ink(); muted <- .plot_muted(); grid <- .plot_grid()
  theme_minimal(base_size = base_size) %+replace%
    theme(
      plot.background   = element_rect(fill = bg, color = NA),
      panel.background  = element_rect(fill = bg, color = NA),
      legend.background = element_rect(fill = bg, color = NA),
      legend.key        = element_rect(fill = bg, color = NA),
      panel.grid.major  = element_line(color = grid, linewidth = 0.35),
      panel.grid.minor  = element_line(color = grid, linewidth = 0.2),
      text              = element_text(color = ink),
      axis.text         = element_text(color = muted),
      axis.title        = element_text(color = ink),
      plot.title        = element_text(color = ink, face = "bold"),
      legend.text       = element_text(color = ink),
      legend.title      = element_text(color = ink),
      legend.position   = "bottom"
    )
}

gg_tend <- function(sa, sr, la, lr, cor_a = C_A, cor_r = C_R) {
  fn <- function(s,l) s %>% group_by(ano) %>%
    summarise(n=sum(count),pop=first(population),.groups="drop") %>%
    mutate(taxa=n/pop*1e5, grupo=l)
  ggplot(bind_rows(fn(sa,la),fn(sr,lr)), aes(ano,taxa,color=grupo,shape=grupo)) +
    geom_line(linewidth=1.1) + geom_point(size=3) +
    scale_color_manual(values=c(setNames(cor_a,la),setNames(cor_r,lr))) +
    labs(x="Ano",y="Taxa por 100.000 hab.",color=NULL,shape=NULL,
         title="Tendência anual — taxa por 100.000 habitantes") +
    theme_app()
}

gg_saz <- function(sa, sr, la, lr, cor_a = C_A, cor_r = C_R) {
  df <- bind_rows(
    sa %>% mutate(IRR=coalesce(IRR,1),grupo=la),
    sr %>% mutate(IRR=coalesce(IRR,1),grupo=lr)
  ) %>% mutate(Mes=factor(Mes,levels=MESES_PT))
  ggplot(df,aes(Mes,IRR,fill=grupo)) +
    geom_col(position=position_dodge(0.85),width=0.75,alpha=0.85) +
    geom_hline(yintercept=1,linetype="dashed",color=TEMA_MUTED) +
    scale_fill_manual(values=c(setNames(cor_a,la),setNames(cor_r,lr))) +
    scale_x_discrete(labels=substr(MESES_PT,1,3)) +
    labs(x=NULL,y="IRR vs. Janeiro",fill=NULL,
         title="Sazonalidade — IRR por mês (ref.: Janeiro)") +
    theme_app()
}

gg_forest <- function(df, titulo, cor_sig = TEMA_RED, cor_ns = TEMA_GOLD,
                       xlab = "IRR (IC 95%)") {
  if (is.null(df)||nrow(df)==0)
    return(ggplot()+theme_void()+
             theme(plot.background=element_rect(fill=.plot_bg(),color=NA))+
             labs(title="Sem dados"))
  df <- df %>%
    mutate(sig_p = !ref & !is.na(p) & p<0.05,
           lbl   = ifelse(ref,"ref (1,00)",
                    sprintf("%.2f (%.2f–%.2f) %s",IRR,IC_inf,IC_sup,sig(p))),
           y     = reorder(categoria, coalesce(IRR,1)))
  ggplot(df,aes(x=IRR,y=y)) +
    geom_vline(xintercept=1,linetype="dashed",color=TEMA_MUTED) +
    geom_errorbarh(data=filter(df,!ref,!is.na(IC_inf)),
                   aes(xmin=IC_inf,xmax=IC_sup,color=sig_p),height=0.3,linewidth=0.9) +
    geom_point(data=filter(df,!ref),aes(color=sig_p),size=3.5) +
    geom_point(data=filter(df,ref),shape=18,size=5,color=TEMA_MUTED) +
    geom_text(aes(x=coalesce(IC_sup,IRR),label=lbl),hjust=-0.1,size=3.1,color=.plot_ink()) +
    scale_color_manual(values=c(`FALSE`=cor_ns,`TRUE`=cor_sig),
                       labels=c(`FALSE`="p≥0,05",`TRUE`="p<0,05"),name=NULL) +
    scale_x_continuous(expand=expansion(mult=c(0.05,0.45))) +
    # limits explícito: sem ele, a categoria de REFERÊNCIA ia parar sempre no
    # topo, fora da ordem. A escala discreta é treinada camada a camada e
    # descarta nível não observado (drop=TRUE); como as duas primeiras camadas
    # são filter(!ref), a referência só aparecia na 3ª e era anexada DEPOIS de
    # todas — num gráfico ordenado por efeito, ela aparentava o maior valor.
    scale_y_discrete(limits = levels(df$y)) +
    labs(x=xlab,y=NULL,title=titulo) +
    theme_app(base_size = 12)
}

# Figura COMPOSTA (mosaico) de vários forest plots num só gráfico, no padrão de
# revista (painéis rotulados A, B, C... e uma legenda única embaixo). Usada para
# juntar o perfil demográfico — sexo, faixa etária e raça/cor — numa figura só.
# `paineis`: lista NOMEADA de data.frames no formato de gg_forest; cada nome vira
# o título do painel (ex.: "A) Sexo"). As alturas relativas seguem o número de
# categorias, para nenhum painel ficar espremido. `ncol=1` empilha (eixo IRR
# alinhado, leitura de cima p/ baixo); `ncol>1` põe lado a lado.
gg_forest_mosaico <- function(paineis, cor_sig = TEMA_RED, cor_ns = TEMA_GOLD,
                              xlab = "IRR (IC 95%)", ncol = 1L) {
  if (!requireNamespace("cowplot", quietly = TRUE))
    stop("Pacote 'cowplot' necessário para a figura composta.")
  paineis <- Filter(function(d) !is.null(d) && nrow(d) > 0, paineis)
  if (length(paineis) == 0)
    return(gg_forest(NULL, "Sem dados", cor_sig = cor_sig, cor_ns = cor_ns))
  plots <- Map(function(d, tit)
      gg_forest(d, tit, cor_sig = cor_sig, cor_ns = cor_ns, xlab = xlab) +
        theme(legend.position = "none",
              plot.title = element_text(size = 12, face = "bold")),
    paineis, names(paineis))
  # Uma legenda só (p<0,05 / p>=0,05). Extrai do painel que contém OS DOIS
  # níveis de significância — senão a legenda mostraria só a chave presente
  # naquele painel (ex.: um painel só com resultados significativos).
  tem_ambos <- function(d) {
    pv <- d$p[!d$ref]
    any(!is.na(pv) & pv < 0.05) && any(is.na(pv) | pv >= 0.05)
  }
  idx_leg <- which(vapply(paineis, tem_ambos, logical(1)))
  idx_leg <- if (length(idx_leg)) idx_leg[1] else 1L
  leg <- cowplot::get_legend(
    plots[[idx_leg]] + theme(legend.position = "bottom",
                             legend.justification = "center"))
  pesos <- unname(vapply(paineis, function(d) nrow(d) + 2, numeric(1)))
  # Passar rel_widths=NULL explicitamente faz o cowplot gerar larguras nulas
  # (viewport não-finito); então só incluímos a dimensão que se aplica ao layout.
  args <- list(plotlist = plots, ncol = as.integer(ncol))
  if (ncol == 1L) args$rel_heights <- pesos else args$rel_widths <- pesos
  grade <- do.call(cowplot::plot_grid, args)
  fig <- cowplot::plot_grid(grade, leg, ncol = 1,
                            rel_heights = c(sum(pesos), 0.9))
  fig + theme(plot.background = element_rect(fill = .plot_bg(), color = NA))
}

# ── Gráficos dos desfechos do SINAN (gravidade e óbito) ──────────────────────
# Proporção anual com faixa de IC 95% (Wilson). df: Ano, prop, IC_inf, IC_sup.
# A faixa é desenhada, e não barras de erro, porque a leitura aqui é a
# TRAJETÓRIA — e com IC assimétrico perto de zero (caso da letalidade) a fita
# mostra de imediato que o limite inferior encosta no chão.
gg_prop_ano <- function(df, titulo, ylab = "% (IC 95%)", cor = TEMA_RED,
                         subtitulo = NULL) {
  if (is.null(df) || nrow(df) == 0)
    return(ggplot() + theme_void() +
             theme(plot.background = element_rect(fill = .plot_bg(), color = NA)) +
             labs(title = "Sem dados"))
  anos <- sort(unique(df$Ano))
  ggplot(df, aes(x = Ano, y = prop)) +
    geom_ribbon(aes(ymin = IC_inf, ymax = IC_sup), fill = cor, alpha = 0.18) +
    geom_line(color = cor, linewidth = 1.1) +
    geom_point(color = cor, size = 2.6) +
    scale_x_continuous(breaks = if (length(anos) <= 15) anos else waiver()) +
    labs(x = "Ano", y = ylab, title = titulo, subtitle = subtitulo) +
    theme_app(base_size = 12)
}

# Composição percentual por ano (100% empilhado). df: ano, categoria, casos.
# Empilhado a 100% de propósito: a pergunta é se o PERFIL de gravidade mudou,
# não se o número de casos cresceu — em valor absoluto a curva de notificação
# domina tudo e esconde a mudança de composição.
gg_composicao_ano <- function(df, titulo, niveis, cores = PALETA_PADRAO$cat,
                               subtitulo = NULL) {
  if (is.null(df) || nrow(df) == 0)
    return(ggplot() + theme_void() +
             theme(plot.background = element_rect(fill = .plot_bg(), color = NA)) +
             labs(title = "Sem dados"))
  d <- df %>%
    mutate(categoria = factor(categoria, levels = niveis)) %>%
    group_by(ano) %>% mutate(prop = casos / sum(casos) * 100) %>% ungroup()
  anos <- sort(unique(d$ano))
  ggplot(d, aes(x = ano, y = prop, fill = categoria)) +
    geom_col(width = 0.78, alpha = 0.9) +
    scale_fill_manual(values = setNames(rep(cores, length.out = length(niveis)), niveis),
                      name = NULL) +
    scale_x_continuous(breaks = if (length(anos) <= 15) anos else waiver()) +
    labs(x = "Ano", y = "% dos casos com informação",
         title = titulo, subtitle = subtitulo) +
    theme_app(base_size = 12)
}


# ─────────────────────────────────────────────────────────────────────────────
# GRÁFICOS EXTRA — PENSADOS PARA APRESENTAÇÃO (pôster/slide de congresso)
# ─────────────────────────────────────────────────────────────────────────────
# Complementam gg_tend/gg_saz/gg_forest: ranking entre muitos estados de uma
# vez (a aba "Por Estado" fica ilegível como tabela quando o grupo tem 20+
# estados) e mapa de calor estado x ano (mostra evolução temporal de todo um
# grupo numa imagem só, sem precisar de várias linhas sobrepostas).

# Ranking horizontal de estados por uma métrica (IRR, RPI, Taxa...). Colore
# por significância quando a coluna Sig existe (usa a mesma paleta do
# forest plot: dourado = não sig., vermelho = sig.).
gg_ranking_estados <- function(df, valor_col, titulo, subtitulo = NULL,
                                ref_line = NULL, fmt = "%.2f",
                                cor_sig = TEMA_RED, cor_ns = TEMA_GOLD) {
  if (is.null(df) || nrow(df) == 0)
    return(ggplot() + theme_void() +
             theme(plot.background = element_rect(fill = .plot_bg(), color = NA)) +
             labs(title = "Sem dados"))

  df <- df %>%
    mutate(valor = .data[[valor_col]],
           sig_p = if ("Sig" %in% names(df)) !Sig %in% c("ns","ref") else FALSE,
           y     = reorder(Estado, valor),
           lbl   = sprintf(fmt, valor))

  p <- ggplot(df, aes(x = valor, y = y)) +
    geom_col(aes(fill = sig_p), width = 0.72, alpha = 0.9) +
    geom_text(aes(label = lbl), hjust = -0.15, size = 3.1, color = .plot_ink()) +
    # Os rótulos precisam ser NOMEADOS: quando todas as barras caem do mesmo
    # lado (ex.: todas significativas), o ggplot descarta o nível ausente dos
    # limites e um vetor posicional passaria a rotular a categoria restante
    # com o texto da outra ("p≥0,05" numa barra p<0,05).
    scale_fill_manual(values = c(`FALSE` = cor_ns, `TRUE` = cor_sig),
                       labels = c(`FALSE` = "p≥0,05 / n.d.", `TRUE` = "p<0,05"),
                       name = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.18))) +
    labs(x = NULL, y = NULL, title = titulo, subtitle = subtitulo) +
    theme_app(base_size = 12) +
    theme(panel.grid.major.y = element_blank())

  if (!is.null(ref_line))
    p <- p + geom_vline(xintercept = ref_line, linetype = "dashed", color = TEMA_MUTED)

  p
}

# Tendência anual da taxa/100k com uma linha por REGIÃO. Complementa gg_tend
# (que só compara grupo de interesse vs. referência) quando o recorte é o
# Brasil ou várias regiões de uma vez: 27 linhas de estado ficam ilegíveis
# sobrepostas, 5 de região não. df precisa das colunas: Regiao, ano, taxa.
gg_tend_regioes <- function(df, titulo = "Tendência anual por região — taxa por 100.000 hab.",
                             subtitulo = NULL, cores = PALETA_PADRAO$cat) {
  if (is.null(df) || nrow(df) == 0)
    return(ggplot() + theme_void() +
             theme(plot.background = element_rect(fill = .plot_bg(), color = NA)) +
             labs(title = "Sem dados"))

  # Ordena a legenda pela taxa média (maior em cima) — assim a ordem das
  # entradas bate com a ordem visual das linhas no fim da série, em vez de
  # sair alfabética e obrigar a caçar a cor de cada região.
  ordem <- df %>% group_by(Regiao) %>%
    summarise(m = mean(taxa, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(m)) %>% pull(Regiao)
  df <- df %>% mutate(Regiao = factor(Regiao, levels = ordem))
  anos <- sort(unique(df$ano))

  ggplot(df, aes(x = ano, y = taxa, color = Regiao, shape = Regiao)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.6) +
    scale_color_manual(values = setNames(rep(cores, length.out = length(ordem)), ordem)) +
    scale_x_continuous(breaks = if (length(anos) <= 15) anos else waiver()) +
    labs(x = "Ano", y = "Taxa por 100.000 hab.", color = NULL, shape = NULL,
         title = titulo, subtitle = subtitulo) +
    theme_app(base_size = 12)
}

# Mapa de calor estado (linha) x ano (coluna), colorido pela taxa/100k.
# df precisa das colunas: Estado, ano, taxa.
gg_heatmap_estado_ano <- function(df, cor_baixo = TEMA_GOLD, cor_alto = TEMA_RED,
                                   cor_sem_dado = COR_SEM_DADO) {
  if (is.null(df) || nrow(df) == 0)
    return(ggplot() + theme_void() +
             theme(plot.background = element_rect(fill = .plot_bg(), color = NA)) +
             labs(title = "Sem dados"))

  ordem <- df %>% group_by(Estado) %>% summarise(m = mean(taxa, na.rm = TRUE)) %>%
    arrange(m) %>% pull(Estado)
  df <- df %>% mutate(Estado = factor(Estado, levels = ordem),
                       ano = factor(ano))

  # cor_baixo/cor_alto formam o degradê dos valores reais; cor_sem_dado (um
  # cinza neutro fixo, fora do degradê) marca célula sem dado nenhum — sem
  # isso, um valor genuinamente baixo fica visualmente indistinguível de
  # "não tem dado" (mesmo bug encontrado no mapa; ver COR_SEM_DADO).
  ggplot(df, aes(x = ano, y = Estado, fill = taxa)) +
    geom_tile(color = TEMA_BG, linewidth = 0.6) +
    scale_fill_gradient(low = cor_baixo, high = cor_alto, na.value = cor_sem_dado,
                        name = "Taxa/100k") +
    labs(x = NULL, y = NULL, title = "Taxa por 100.000 hab. — estado × ano") +
    theme_app(base_size = 11) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

# Boxplot comparando a distribuição das taxas mensais entre grupos (ex.:
# vários estados de uma vez, ou regiões) — mostra dispersão, não só a média.
gg_boxplot_grupos <- function(df, titulo = "Distribuição da taxa mensal por grupo",
                               cor = TEMA_GOLD) {
  if (is.null(df) || nrow(df) == 0)
    return(ggplot() + theme_void() +
             theme(plot.background = element_rect(fill = .plot_bg(), color = NA)) +
             labs(title = "Sem dados"))
  ggplot(df, aes(x = reorder(grupo, taxa, FUN = median), y = taxa)) +
    geom_boxplot(fill = cor, color = .plot_ink(), alpha = 0.75, outlier.color = TEMA_RED) +
    labs(x = NULL, y = "Taxa por 100.000 hab./mês", title = titulo) +
    theme_app(base_size = 12) +
    coord_flip()
}

# Gráfico de pizza — internações por estado dentro do grupo de interesse.
# Agrupa estados além dos top N numa fatia "Outros" pra não virar 27 fatias
# ilegíveis quando o grupo é o Brasil inteiro.
gg_pizza_estados <- function(df, titulo = "Internações por estado", top_n = 8,
                              cores = PALETA_PADRAO$cat) {
  if (is.null(df) || nrow(df) == 0)
    return(ggplot() + theme_void() +
             theme(plot.background = element_rect(fill = .plot_bg(), color = NA)) +
             labs(title = "Sem dados"))

  d <- df %>% arrange(desc(Internacoes))
  tem_outros <- nrow(d) > top_n
  if (tem_outros) {
    resto <- d[(top_n + 1):nrow(d), ]
    d <- bind_rows(
      d[1:top_n, ],
      data.frame(Estado = "Outros", UF = NA_character_,
                 Internacoes = sum(resto$Internacoes),
                 Percentual  = sum(resto$Percentual))
    )
  }
  # Rótulo (estado + %) vai pra legenda embaixo, não escrito em cima da
  # fatia — com muitas fatias pequenas (Brasil inteiro, por exemplo) o texto
  # nas fatias se sobrepõe e fica ilegível. A legenda mantém a leitura
  # independente do tamanho da fatia.
  rotulos <- sprintf("%s (%.1f%%)", d$Estado, d$Percentual)
  d <- d %>% mutate(legenda = factor(rotulos, levels = rotulos))

  # "Outros" sempre usa uma cor neutra fixa (cinza), nunca uma cor reciclada
  # da paleta categórica — do contrário pode calhar de recair numa cor
  # parecida (ou igual) à de um estado real (ex.: dois vermelhos diferentes),
  # o que confundia a leitura do gráfico.
  n_estados <- nrow(d) - as.integer(tem_outros)
  cores_fatias <- rep(cores, length.out = n_estados)
  if (tem_outros) cores_fatias <- c(cores_fatias, COR_SEM_DADO)

  ggplot(d, aes(x = "", y = Internacoes, fill = legenda)) +
    geom_col(width = 1, color = .plot_bg(), linewidth = 0.8) +
    coord_polar(theta = "y") +
    scale_fill_manual(values = cores_fatias, name = NULL) +
    guides(fill = guide_legend(ncol = 3, byrow = TRUE,
                                override.aes = list(color = NA))) +
    labs(x = NULL, y = NULL, title = titulo) +
    theme_void(base_size = 12) +
    theme(plot.background  = element_rect(fill = .plot_bg(), color = NA),
          panel.background = element_rect(fill = .plot_bg(), color = NA),
          legend.background = element_rect(fill = .plot_bg(), color = NA),
          legend.key        = element_rect(fill = .plot_bg(), color = NA),
          legend.position = "bottom",
          legend.text = element_text(color = .plot_ink(), size = 9),
          plot.title = element_text(color = .plot_ink(), face = "bold", hjust = 0.5),
          plot.margin = margin(10, 10, 10, 10))
}

# Histograma da distribuição de taxas de incidência (uma observação por
# estado, ou por estado×mês — depende do que for passado em `vetor`). Ajuda
# a ver se a distribuição é concentrada ou tem cauda longa (poucos estados
# puxando a média), o que a tabela sozinha não mostra bem.
gg_hist_incidencia <- function(vetor, titulo = "Distribuição da taxa de incidência",
                                bins = 20, cor = TEMA_RED) {
  vetor <- vetor[!is.na(vetor)]
  if (length(vetor) == 0)
    return(ggplot() + theme_void() +
             theme(plot.background = element_rect(fill = .plot_bg(), color = NA)) +
             labs(title = "Sem dados"))
  df <- data.frame(taxa = vetor)
  ggplot(df, aes(x = taxa)) +
    geom_histogram(bins = bins, fill = cor, color = TEMA_BG, alpha = 0.9) +
    geom_vline(xintercept = mean(vetor), linetype = "dashed", color = .plot_ink()) +
    labs(x = "Taxa por 100.000 hab.", y = "Nº de observações", title = titulo,
         subtitle = sprintf("Média = %.2f | Mediana = %.2f | n = %d",
                            mean(vetor), median(vetor), length(vetor))) +
    theme_app(base_size = 12)
}

# Opções que pintam os gráficos em tema CLARO (fundo branco), usadas só na
# exportação. Ver .plot_bg()/.plot_ink()/etc. — o gráfico é reconstruído com
# estas opções ativas para que texto e linhas fiquem escuros sobre o branco.
OPCOES_EXPORT_CLARO <- list(
  episus.plot_bg    = "#ffffff",
  episus.plot_ink   = "#1a1a1a",
  episus.plot_muted = "#606060",
  episus.plot_grid  = "#dcdcdc"
)

# Salva um ggplot em alta resolução (padrão pôster/slide: 300 DPI). Usada
# pelos downloadHandler do Shiny na aba "Gráficos (Congresso)".
#
# `plot` pode ser um ggplot JÁ construído OU uma função/reactive que CONSTRÓI o
# gráfico. Para `fundo = "claro"` (padrão), passe a função: o gráfico é montado
# com as opções de tema claro ativas, garantindo tinta escura sobre o branco —
# passar um ggplot pronto exportaria com o texto claro do tema de tela, ilegível
# no papel. `legenda_num` (ex.: "Figura 3") vira uma legenda discreta no canto.
# `formato`: "png" (raster) ou vetor "pdf"/"eps"/"svg" (o que a RESP pede para
# gráficos e mapas). PDF usa cairo_pdf (embute fontes → acentos e transparência
# das faixas de IC corretos); SVG exige o pacote svglite.
salvar_grafico_hd <- function(plot, file, largura = 10, altura = 6, dpi = 300,
                              fundo = c("claro", "escuro"), legenda_num = NULL,
                              formato = "png") {
  fundo <- match.arg(fundo)
  if (identical(fundo, "claro")) {
    old <- options(OPCOES_EXPORT_CLARO)
    on.exit(options(old), add = TRUE)
  }
  p <- if (is.function(plot)) plot() else plot
  if (!is.null(legenda_num) && nzchar(legenda_num) && inherits(p, "ggplot"))
    p <- p + ggplot2::labs(caption = legenda_num) +
      ggplot2::theme(plot.caption = ggplot2::element_text(
        color = .plot_muted(), hjust = 1, size = 9,
        margin = ggplot2::margin(t = 6)))
  dev <- switch(tolower(formato %||% "png"),
                pdf = grDevices::cairo_pdf, eps = "eps", svg = "svg", "png")
  ggsave(file, plot = p, width = largura, height = altura, dpi = dpi,
         bg = getOption("episus.plot_bg", TEMA_BG_CARD), device = dev)
}

# Monta o rótulo "Figura N"/"Tabela N" a partir do valor do numericInput do
# export (NA/vazio → NULL, sem legenda). Reutilizado nos downloadHandler.
rotulo_export <- function(n, tipo = "Figura") {
  n <- suppressWarnings(as.integer(n))
  if (length(n) != 1 || is.na(n)) return(NULL)
  sprintf("%s %d", tipo, n)
}

# Sufixo de nome de arquivo com o número (ex.: "_fig3"); vazio se sem número.
sufixo_num <- function(n, prefixo = "fig") {
  n <- suppressWarnings(as.integer(n))
  if (length(n) != 1 || is.na(n)) return("")
  sprintf("_%s%d", prefixo, n)
}

# ─────────────────────────────────────────────────────────────────────────────
# MAPA DINÂMICO DO BRASIL (leaflet + geobr) — opcional
# ─────────────────────────────────────────────────────────────────────────────
# leaflet/geobr/sf NÃO entram no bloco de library() obrigatório do topo do
# app — são pacotes pesados (sf principalmente, depende de GDAL/PROJ no
# sistema) e a ideia é que o resto do app continue funcionando normalmente
# mesmo se o usuário não os tiver instalado. pacotes_mapa_disponiveis()
# checa antes de qualquer coisa; a UI mostra instrução de instalação em vez
# de travar quando faltar.

pacotes_mapa_disponiveis <- function() {
  requireNamespace("leaflet", quietly = TRUE) &&
    requireNamespace("geobr", quietly = TRUE) &&
    requireNamespace("sf", quietly = TRUE)
}

# Baixa (1ª vez) e cacheia em disco o contorno dos 27 estados via geobr —
# depende de internet só na primeira chamada; depois lê do cache local, sem
# depender do servidor do IPEA/IBGE de novo. Retorna um objeto sf com pelo
# menos as colunas abbrev_state (UF) e geometry.
carregar_geo_estados <- function(cache_path = file.path("data", "br_estados_geo.rds")) {
  if (!pacotes_mapa_disponiveis())
    stop("Pacotes do mapa não instalados. Rode: install.packages(c('leaflet','geobr','sf'))")

  if (file.exists(cache_path)) {
    geo <- tryCatch(readRDS(cache_path), error = function(e) NULL)
    if (!is.null(geo)) return(geo)
  }

  geo <- tryCatch(
    geobr::read_state(year = 2020, showProgress = FALSE),
    error = function(e) stop(paste(
      "Não consegui baixar o contorno dos estados (geobr precisa de internet",
      "na primeira vez). Erro original:", e$message
    ))
  )
  dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)
  tryCatch(saveRDS(geo, cache_path), error = function(e) NULL)  # cache é bônus, não crítico
  geo
}

# Constrói a paleta coroplética do leaflet conforme o método de classificação:
#   "continuo" — degradê liso (colorNumeric)
#   "jenks"    — quebras naturais de Jenks (classInt style "fisher", o Jenks
#                exato — mais rápido e determinístico que o "jenks" clássico)
#   "quantis"  — mesmo nº de áreas por classe (style "quantile")
#   "iguais"   — faixas de largura igual (style "equal")
# Jenks/quantis/iguais viram faixas discretas (colorBin). Cai para o degradê
# contínuo se o classInt faltar, se o método for desconhecido, ou se houver
# poucos valores distintos para formar classes.
.paleta_coropletica <- function(valores, cor_baixo, cor_alto, cor_sem_dado,
                                classificacao = "continuo", n_classes = 5) {
  degrade <- function(n) grDevices::colorRampPalette(c(cor_baixo, cor_alto))(n)
  continuo <- function()
    leaflet::colorNumeric(palette = degrade(20), domain = valores, na.color = cor_sem_dado)

  if (identical(classificacao, "continuo") || !requireNamespace("classInt", quietly = TRUE))
    return(continuo())

  estilo <- switch(classificacao, jenks = "fisher", quantis = "quantile",
                   iguais = "equal", NULL)
  if (is.null(estilo)) return(continuo())

  v <- valores[is.finite(valores)]
  if (length(unique(v)) < 3) return(continuo())
  n <- max(2L, min(as.integer(n_classes), length(unique(v))))
  brks <- tryCatch(unique(classInt::classIntervals(v, n = n, style = estilo)$brks),
                   error = function(e) NULL)
  if (is.null(brks) || length(brks) < 3) return(continuo())

  leaflet::colorBin(palette = degrade(length(brks) - 1), bins = brks,
                    domain = valores, na.color = cor_sem_dado)
}

# Monta o mapa leaflet coroplético — uma linha por estado (df com colunas
# UF e valor_col), casado com o contorno geográfico por abbrev_state.
mapa_leaflet_estados <- function(geo, df, valor_col, titulo = NULL,
                                  cor_baixo = TEMA_GOLD, cor_alto = TEMA_RED,
                                  cor_sem_dado = COR_SEM_DADO,
                                  classificacao = "continuo", n_classes = 5,
                                  fmt = "%.2f") {
  if (!pacotes_mapa_disponiveis())
    stop("Pacotes do mapa não instalados. Rode: install.packages(c('leaflet','geobr','sf'))")
  if (is.null(df) || nrow(df) == 0 || is.null(geo)) return(NULL)

  d <- df %>% mutate(valor = .data[[valor_col]])
  geo_j <- merge(geo, d, by.x = "abbrev_state", by.y = "UF", all.x = TRUE)

  # cor_baixo/cor_alto formam o degradê dos valores REAIS; cor_sem_dado é
  # sempre um cinza neutro fixo, deliberadamente fora desse degradê — não
  # pode nunca coincidir com a ponta "baixo" da escala (ver comentário em
  # COR_SEM_DADO), senão um estado com valor baixo fica visualmente idêntico
  # a um estado sem nenhum dado.
  pal <- .paleta_coropletica(geo_j$valor, cor_baixo, cor_alto, cor_sem_dado,
                             classificacao, n_classes)

  rotulo <- sprintf(
    "<b>%s (%s)</b><br/>%s",
    geo_j$name_state, geo_j$abbrev_state,
    ifelse(is.na(geo_j$valor), "sem dado", sprintf(fmt, geo_j$valor))
  ) %>% lapply(htmltools::HTML)

  leaflet::leaflet(geo_j) %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter) %>%
    leaflet::addPolygons(
      fillColor = ~pal(valor), fillOpacity = 0.82,
      color = TEMA_BORDA, weight = 1,
      highlightOptions = leaflet::highlightOptions(weight = 2, color = TEMA_FG, bringToFront = TRUE),
      label = rotulo,
      labelOptions = leaflet::labelOptions(style = list("font-weight" = "normal"))
    ) %>%
    leaflet::addLegend(pal = pal, values = ~valor, title = titulo, position = "bottomright",
                       na.label = "Sem dado")
}

# ─────────────────────────────────────────────────────────────────────────────
# MAPA MUNICIPAL (drill-down de 1 estado — mesma lógica do mapa estadual
# acima, em granularidade de município)
# ─────────────────────────────────────────────────────────────────────────────

# Contorno dos municípios de 1 estado via geobr — mesma ideia de
# carregar_geo_estados(), mas 1 cache por UF (baixar os ~5570 municípios do
# Brasil inteiro de uma vez não compensa se o usuário só olha 1-2 estados
# por sessão).
carregar_geo_municipios <- function(uf, cache_path = file.path("data", paste0("geo_municipios_", uf, ".rds"))) {
  if (!pacotes_mapa_disponiveis())
    stop("Pacotes do mapa não instalados. Rode: install.packages(c('leaflet','geobr','sf'))")

  if (file.exists(cache_path)) {
    geo <- tryCatch(readRDS(cache_path), error = function(e) NULL)
    if (!is.null(geo)) return(geo)
  }

  geo <- tryCatch(
    geobr::read_municipality(code_muni = uf, year = 2020, showProgress = FALSE),
    error = function(e) stop(paste(
      "Não consegui baixar o contorno dos municípios (geobr precisa de internet",
      "na primeira vez). Erro original:", e$message
    ))
  )
  dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)
  tryCatch(saveRDS(geo, cache_path), error = function(e) NULL)  # cache é bônus, não crítico
  geo
}

# População por município/ano via API pública do IBGE (SIDRA, tabela 6579 —
# "População residente estimada"). 1 chamada traz todos os municípios do
# estado e todos os anos disponíveis de uma vez; cacheada em disco depois
# disso, igual ao contorno geográfico. code_state é o código IBGE de 2
# dígitos do estado (ex. 25=PB) — vem de carregar_geo_municipios()$code_state,
# sem precisar de tabela de conversão própria.
carregar_populacao_municipios <- function(uf, code_state, cache_path = file.path("data", paste0("pop_municipios_", uf, ".rds"))) {
  if (file.exists(cache_path)) {
    pop <- tryCatch(readRDS(cache_path), error = function(e) NULL)
    if (!is.null(pop)) return(pop)
  }

  url <- sprintf("https://apisidra.ibge.gov.br/values/t/6579/n6/in%%20n3%%20%s/v/9324/p/all", code_state)
  pop <- tryCatch({
    raw <- jsonlite::fromJSON(url)
    raw <- raw[-1, ]
    data.frame(
      cod6      = substr(as.character(raw$D1C), 1, 6),
      ano       = as.integer(raw$D3C),
      populacao = as.numeric(raw$V),
      stringsAsFactors = FALSE
    )
  }, error = function(e) stop(paste(
    "Não consegui buscar população municipal no IBGE (API SIDRA precisa de",
    "internet na primeira vez). Erro original:", e$message
  )))

  dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)
  tryCatch(saveRDS(pop, cache_path), error = function(e) NULL)  # cache é bônus, não crítico
  pop
}

# Internações por município/ano de residência — lê o .rds bruto local (que
# ainda tem MUNIC_RES; o pipeline geral de baixar_dados()/ler_rds_local()
# descarta essa coluna mais adiante, mantendo só a UF). Mesmo escopo da
# taxa estadual já existente: só conta quem foi internado dentro do próprio
# estado (não varre os outros 26 .rds atrás de quem buscou atendimento fora).
# cid_pref aplica o MESMO filtro de CID-10 usado no resto do app (ex.: só
# DM2) — sem isso, contaria internação por qualquer causa, não só a
# selecionada pelo usuário.
# `ufs_keep` define quais UFs de RESIDÊNCIA manter (padrão: só a própria UF do
# arquivo, comportamento original do mapa municipal). Para análises de região /
# Brasil, passa-se o conjunto todo (ex.: todas as UFs do Nordeste), de modo que
# um morador de PB internado em PE (que aparece no .rds de PE com uf_res=PB)
# seja contabilizado — sem isso, o fluxo entre estados vizinhos some do mapa.
tabela_admissoes_municipio <- function(pasta, sistema, uf, ano_ini, ano_fim, cid_pref = character(0),
                                        faixa_eca = "geral", ufs_keep = uf, sinan_tipo = "3") {
  # SINAN: sem RDS por UF — monta a partir do arquivo nacional (cacheado),
  # filtrando o tipo de animal e as UFs de residência do escopo.
  if (sistema_tipo(sistema) == "SINAN") {
    frames <- list()
    for (ano in ano_ini:ano_fim) {
      sub <- tryCatch(processar_sinan_ano(ano, sinan_tipo, ufs_keep), error = function(e) NULL)
      if (!is.null(sub) && nrow(sub) > 0) frames[[length(frames) + 1]] <- sub
    }
    if (length(frames) == 0) return(NULL)
    dat <- dplyr::bind_rows(frames)
    if (!identical(faixa_eca, "geral"))
      dat <- dat[idade_eca_ok(dat$idade_anos, faixa_eca), , drop = FALSE]
    if (nrow(dat) == 0) return(NULL)
    return(dat %>%
      filter(!is.na(ano), ano >= ano_ini, ano <= ano_fim, uf_res %in% ufs_keep) %>%
      count(cod6, ano, name = "internacoes"))
  }

  dat <- ler_rds_local(pasta, sistema, uf)
  if (is.null(dat) || nrow(dat) == 0) return(NULL)

  is_sih <- grepl("SIH", sistema, ignore.case = TRUE)
  c_mun  <- if (is_sih) col1(dat,"MUNIC_RES","munic_res","CODMUNRES","codmunres")
            else        col1(dat,"CODMUNRES","codmunres","MUNIC_RES")
  if (is.na(c_mun)) return(NULL)

  c_cid <- if (is_sih) col1(dat,"DIAG_PRINC","diag_princ")
           else        col1(dat,"CAUSABAS","causabas")
  if (!is.na(c_cid) && length(cid_pref) > 0)
    dat <- dat[cid_ok(dat[[c_cid]], cid_pref), , drop = FALSE]
  if (is.null(dat) || nrow(dat) == 0) return(NULL)

  # Filtro de faixa etária "tipo ECA" — mesmo princípio do filtro de CID
  # acima. Usa idade_em_anos() direto (exata por registro), não o bucket de
  # década de clf_faixa(), porque os cortes de 12/18 anos não caem em
  # fronteira de década. Só restringe de fato quando preset != "geral" (o
  # default), então nenhuma chamada existente antes desta mudança é afetada.
  # Nota: a população municipal do IBGE (carregar_populacao_municipios) não
  # é quebrada por faixa etária — só o numerador (internações) respeita este
  # filtro; o denominador do mapa continua sendo a população de todas as
  # idades do município.
  if (!identical(faixa_eca, "geral")) {
    c_id <- col1(dat,"IDADE","idade")
    c_ci <- col1(dat,"COD_IDADE","cod_idade")
    if (!is.na(c_id)) {
      anos <- idade_em_anos(dat[[c_id]], if (!is.na(c_ci)) dat[[c_ci]] else NULL)
      dat <- dat[idade_eca_ok(anos, faixa_eca), , drop = FALSE]
    }
  }
  if (is.null(dat) || nrow(dat) == 0) return(NULL)

  c_dti <- col1(dat,"DT_INTER","dt_inter")
  c_ano <- col1(dat,"ANO_CMPT","ano_cmpt")
  c_mes <- col1(dat,"MES_CMPT","mes_cmpt")

  dat$uf_res <- extrair_uf_ibge(dat[[c_mun]])
  dat <- extrair_datas(dat, is_sih, c_dti, c_ano, c_mes, ano_fallback = ano_ini)
  dat$cod6  <- substr(trimws(as.character(dat[[c_mun]])), 1, 6)

  dat %>%
    filter(!is.na(ano), ano >= ano_ini, ano <= ano_fim, uf_res %in% ufs_keep) %>%
    count(cod6, ano, name = "internacoes")
}

# Taxa por 100k por município/ano = internações (RDS local) / população
# (IBGE SIDRA), casadas por (cod6, ano). Quando o ano exato não tem
# população publicada (2022/2023 ficam sem estimativa nessa tabela, entre
# outros), usa o ano disponível mais próximo (até 1 ano de distância — a
# população municipal não muda o bastante nesse intervalo pra distorcer a
# taxa) e sinaliza isso em pop_aprox, pra não virar "sem dado" à toa nos
# anos mais recentes.
tabela_taxa_municipio <- function(pasta, sistema, uf, code_state, ano_ini, ano_fim, cid_pref = character(0),
                                   faixa_eca = "geral", sinan_tipo = "3") {
  adm <- tabela_admissoes_municipio(pasta, sistema, uf, ano_ini, ano_fim, cid_pref, faixa_eca,
                                    ufs_keep = uf, sinan_tipo = sinan_tipo)
  if (is.null(adm) || nrow(adm) == 0) return(NULL)

  pop <- carregar_populacao_municipios(uf, code_state)
  anos_pop <- sort(unique(pop$ano))
  adm$ano <- as.integer(adm$ano)

  ano_mais_proximo <- function(ano) {
    if (ano %in% anos_pop) return(as.integer(ano))
    dist <- abs(anos_pop - ano)
    cand <- anos_pop[dist == min(dist)][1]
    if (min(dist) <= 1) as.integer(cand) else NA_integer_
  }

  adm$ano_pop   <- vapply(adm$ano, ano_mais_proximo, integer(1))
  adm$pop_aprox <- !is.na(adm$ano_pop) & adm$ano_pop != adm$ano

  adm %>%
    left_join(pop, by = c("cod6" = "cod6", "ano_pop" = "ano")) %>%
    mutate(taxa = internacoes / populacao * 1e5)
}

# Resume a saída de tabela_taxa_municipio() (1 linha por município x ano)
# numa média anual do período inteiro: soma as internações do período e
# divide pela população MÉDIA do período (equivalente a usar pessoa-ano
# como denominador) e pelo número de anos do período — não o número de
# anos com linha na tabela, pra não inflar a média caso algum ano tenha
# zero internações (e portanto nenhuma linha, já que a contagem vem de
# count()).
agregar_taxa_municipio_periodo <- function(tx, ano_ini, ano_fim) {
  if (is.null(tx) || nrow(tx) == 0) return(NULL)
  n_anos <- ano_fim - ano_ini + 1
  tx %>%
    group_by(cod6) %>%
    summarise(internacoes = sum(internacoes),
              populacao   = mean(populacao, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(internacoes_media_anual = internacoes / n_anos,
           taxa = internacoes / n_anos / populacao * 1e5)
}

# Mapa coroplético dos municípios de 1 estado — cópia de
# mapa_leaflet_estados() trocando o join por código de município (6
# dígitos, truncando o código IBGE de 7 dígitos que o geobr devolve) e o
# rótulo por nome do município.
mapa_leaflet_municipios <- function(geo, df, valor_col, titulo = NULL,
                                     cor_baixo = TEMA_GOLD, cor_alto = TEMA_RED,
                                     cor_sem_dado = COR_SEM_DADO,
                                     classificacao = "continuo", n_classes = 5,
                                     fmt = "%.1f") {
  if (!pacotes_mapa_disponiveis())
    stop("Pacotes do mapa não instalados. Rode: install.packages(c('leaflet','geobr','sf'))")
  if (is.null(df) || nrow(df) == 0 || is.null(geo)) return(NULL)

  geo$cod6 <- substr(as.character(geo$code_muni), 1, 6)
  d <- df %>% mutate(valor = .data[[valor_col]])
  geo_j <- merge(geo, d, by = "cod6", all.x = TRUE)

  pal <- .paleta_coropletica(geo_j$valor, cor_baixo, cor_alto, cor_sem_dado,
                             classificacao, n_classes)

  aprox  <- if (!is.null(geo_j$pop_aprox)) ifelse(isTRUE(geo_j$pop_aprox), " (pop. aprox.)", "") else ""
  rotulo <- sprintf(
    "<b>%s</b><br/>%s%s",
    geo_j$name_muni,
    ifelse(is.na(geo_j$valor), "sem dado", sprintf(fmt, geo_j$valor)),
    aprox
  ) %>% lapply(htmltools::HTML)

  leaflet::leaflet(geo_j) %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter) %>%
    leaflet::addPolygons(
      fillColor = ~pal(valor), fillOpacity = 0.82,
      color = TEMA_BORDA, weight = 1,
      highlightOptions = leaflet::highlightOptions(weight = 2, color = TEMA_FG, bringToFront = TRUE),
      label = rotulo,
      labelOptions = leaflet::labelOptions(style = list("font-weight" = "normal"))
    ) %>%
    leaflet::addLegend(pal = pal, values = ~valor, title = titulo, position = "bottomright",
                       na.label = "Sem dado")
}

# Mapa da densidade de Kernel: superfície (geom_raster) recortada ao contorno.
# "risco" usa escala divergente centrada em 0 (log-RR); "casos" usa sequencial.
gg_kernel <- function(kd, titulo = NULL) {
  if (!requireNamespace("sf", quietly = TRUE))
    stop("Pacote 'sf' não instalado.")
  sub <- if (!is.null(kd$banda_rotulo)) paste("Largura de banda:", kd$banda_rotulo) else NULL
  g <- ggplot() +
    geom_raster(data = kd$grid, aes(x = x, y = y, fill = z), interpolate = TRUE) +
    geom_sf(data = sf::st_sf(geometry = kd$contorno), fill = NA,
            color = "grey35", linewidth = 0.35)
  g <- if (identical(kd$tipo, "risco")) {
    lim <- max(abs(kd$grid$z), na.rm = TRUE)
    g + scale_fill_gradient2(low = "#2c7bb6", mid = "#ffffbf", high = "#d7191c",
                             midpoint = 0, limits = c(-lim, lim), name = "log-RR",
                             na.value = NA)
  } else {
    g + scale_fill_viridis_c(option = "inferno", name = "Densidade", na.value = NA)
  }
  g + labs(title = titulo %||% kd$rotulo, subtitle = sub, x = NULL, y = NULL) +
    coord_sf(expand = FALSE) +
    theme_minimal(base_size = 12) +
    theme(plot.background = element_rect(fill = .plot_bg(), color = NA),
          plot.title = element_text(color = .plot_ink(), face = "bold"),
          plot.subtitle = element_text(color = .plot_muted()),
          legend.text = element_text(color = .plot_ink()),
          legend.title = element_text(color = .plot_ink()),
          panel.grid = element_blank(),
          axis.text = element_blank(), axis.ticks = element_blank())
}

# Figura COMPOSTA (mosaico) com as DUAS superfícies de Kernel lado a lado:
# A) densidade de casos e B) risco relativo (log-RR). Cada painel mantém a sua
# própria escala/legenda — são grandezas diferentes, então uma legenda única
# induziria a erro. `sufixo` (ex.: o rótulo do escopo) entra no título dos dois.
gg_kernel_duplo <- function(geo, base, banda = "sj", sufixo = NULL,
                            titulo_casos = "A) Densidade de casos",
                            titulo_risco = "B) Risco relativo (log-RR)") {
  if (!requireNamespace("cowplot", quietly = TRUE))
    stop("Pacote 'cowplot' necessário para a figura composta.")
  suf  <- function(t) if (!is.null(sufixo) && nzchar(sufixo))
                        paste0(t, " — ", sufixo) else t
  kd_c <- densidade_kernel(geo, base, tipo = "casos", banda = banda)
  kd_r <- densidade_kernel(geo, base, tipo = "risco", banda = banda)
  leg  <- theme(legend.position = "bottom",
                legend.key.width = grid::unit(1.1, "cm"))
  ga <- gg_kernel(kd_c, suf(titulo_casos)) + leg
  gb <- gg_kernel(kd_r, suf(titulo_risco)) + leg
  fig <- cowplot::plot_grid(ga, gb, ncol = 2)
  fig + theme(plot.background = element_rect(fill = .plot_bg(), color = NA))
}

# Mosaico GENÉRICO de ggplots já prontos (cada painel traz o rótulo A/B/... no
# próprio título). Fundo segue o tema (branco na exportação). `rel` = alturas
# relativas (ncol=1) ou larguras (ncol>1). NULLs na lista são descartados.
gg_mosaico <- function(plots, ncol = 1L, rel = NULL) {
  if (!requireNamespace("cowplot", quietly = TRUE))
    stop("Pacote 'cowplot' necessário para a figura composta.")
  plots <- Filter(Negate(is.null), plots)
  if (!length(plots)) return(NULL)
  args <- list(plotlist = plots, ncol = as.integer(ncol))
  if (!is.null(rel)) {
    if (ncol == 1L) args$rel_heights <- rel else args$rel_widths <- rel
  }
  do.call(cowplot::plot_grid, args) +
    theme(plot.background = element_rect(fill = .plot_bg(), color = NA))
}

# Diagrama de espalhamento de Moran: taxa padronizada (z) no eixo x vs. média
# espacial dos vizinhos (Wz) no y. A inclinação da reta = Moran's I global. Os
# quadrantes correspondem à classificação do LISA.
gg_moran_scatter <- function(valores, listw, titulo = NULL, cor = TEMA_RED) {
  if (sd(valores, na.rm = TRUE) == 0)
    return(ggplot() + theme_void() +
             theme(plot.background = element_rect(fill = .plot_bg(), color = NA)) +
             labs(title = "Taxas constantes — sem espalhamento"))
  z    <- as.numeric(scale(valores))
  wz   <- spdep::lag.listw(listw, z, zero.policy = TRUE)
  I    <- stats::coef(stats::lm(wz ~ z))[2]
  df   <- data.frame(z = z, wz = wz)
  ttl  <- if (is.null(titulo)) sprintf("Diagrama de Moran (I = %.3f)", I)
          else sprintf("%s (I = %.3f)", titulo, I)
  ggplot(df, aes(z, wz)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = TEMA_MUTED) +
    geom_vline(xintercept = 0, linetype = "dashed", color = TEMA_MUTED) +
    geom_point(color = cor, alpha = 0.7, size = 2.2) +
    geom_smooth(method = "lm", se = FALSE, color = .plot_ink(), linewidth = 0.9, formula = y ~ x) +
    labs(x = "Taxa padronizada (z)", y = "Média dos vizinhos (Wz)", title = ttl) +
    theme_app(base_size = 12)
}

# Mapa leaflet coroplético CATEGÓRICO (cluster LISA ou hot/cold Gi*) — mesma
# base do mapa municipal por taxa, mas colorido por categoria discreta em vez
# de gradiente. `df` precisa de cod6 + a coluna de categoria (e, opcional, uma
# coluna numérica extra pro rótulo, ex.: a taxa).
mapa_leaflet_clusters <- function(geo, df, col_categoria, cores, titulo = NULL,
                                  col_extra = NULL, fmt_extra = "%.1f", rotulo_extra = "Taxa/100k") {
  if (!pacotes_mapa_disponiveis())
    stop("Pacotes do mapa não instalados. Rode: install.packages(c('leaflet','geobr','sf'))")
  if (is.null(df) || nrow(df) == 0 || is.null(geo)) return(NULL)

  geo$cod6 <- substr(as.character(geo$code_muni), 1, 6)
  # Só as colunas necessárias entram no merge — o `df` (detalhe) tem um
  # name_muni próprio que colidiria com o do geo (viraria name_muni.x/.y e
  # apagaria o rótulo). O nome do município sai sempre do geo.
  cols_keep <- c("cod6", col_categoria, if (!is.null(col_extra)) col_extra)
  df2   <- df[, intersect(cols_keep, names(df)), drop = FALSE]
  geo_j <- merge(geo, df2, by = "cod6", all.x = TRUE)

  cats <- names(cores)
  catv <- as.character(geo_j[[col_categoria]])
  catv[is.na(catv)] <- "Não significativo"
  catv <- factor(catv, levels = cats)

  cores_poly <- unname(cores[as.character(catv)])
  cores_poly[is.na(cores_poly)] <- COR_SEM_DADO

  extra_txt <- if (!is.null(col_extra) && col_extra %in% names(geo_j))
    ifelse(is.na(geo_j[[col_extra]]), "",
           sprintf("<br/>%s: %s", rotulo_extra, sprintf(fmt_extra, geo_j[[col_extra]])))
  else ""

  rotulo <- sprintf("<b>%s</b><br/>%s%s",
                    geo_j$name_muni, as.character(catv), extra_txt) %>%
    lapply(htmltools::HTML)

  leaflet::leaflet(geo_j) %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter) %>%
    leaflet::addPolygons(
      fillColor = cores_poly, fillOpacity = 0.82,
      color = TEMA_BORDA, weight = 1,
      highlightOptions = leaflet::highlightOptions(weight = 2, color = TEMA_FG, bringToFront = TRUE),
      label = rotulo,
      labelOptions = leaflet::labelOptions(style = list("font-weight" = "normal"))
    ) %>%
    leaflet::addLegend(colors = unname(cores), labels = names(cores),
                       title = titulo, position = "bottomright", opacity = 0.9)
}

# Seta de Norte + barra de escala (km, aproximada) num mapa geom_sf em graus
# (SIRGAS 2000). Sem depender de ggspatial: a seta vai no canto superior direito
# e a barra no inferior esquerdo; a escala usa 111,32 km/grau corrigido pela
# latitude média. Se o cálculo falhar, só a seta é adicionada.
.add_norte_escala <- function(g, geo_j) {
  bb  <- sf::st_bbox(geo_j)
  dx  <- as.numeric(bb["xmax"] - bb["xmin"]); dy <- as.numeric(bb["ymax"] - bb["ymin"])
  ink <- .plot_ink()
  nx  <- as.numeric(bb["xmax"]) - dx * 0.05
  g <- g +
    annotate("segment", x = nx, xend = nx,
             y = as.numeric(bb["ymax"]) - dy * 0.16,
             yend = as.numeric(bb["ymax"]) - dy * 0.07,
             arrow = grid::arrow(length = grid::unit(0.18, "cm"), type = "closed"),
             color = ink, linewidth = 0.6) +
    annotate("text", x = nx, y = as.numeric(bb["ymax"]) - dy * 0.035,
             label = "N", color = ink, size = 3.4, fontface = "bold")
  esc <- tryCatch({
    lat_mid <- as.numeric(bb["ymin"]) + dy * 0.5
    km_tot  <- dx * 111.32 * cos(lat_mid * pi / 180)
    passo   <- c(50, 100, 200, 250, 500, 1000, 2000)
    km      <- passo[which.min(abs(passo - km_tot / 4))]
    deg     <- km / (111.32 * cos(lat_mid * pi / 180))
    x0 <- as.numeric(bb["xmin"]) + dx * 0.05
    list(x0 = x0, xe = x0 + deg, y0 = as.numeric(bb["ymin"]) + dy * 0.05, km = km)
  }, error = function(e) NULL)
  if (!is.null(esc)) g <- g +
    annotate("segment", x = esc$x0, xend = esc$xe, y = esc$y0, yend = esc$y0,
             color = ink, linewidth = 0.8) +
    annotate("text", x = (esc$x0 + esc$xe) / 2, y = esc$y0 + dy * 0.028,
             label = sprintf("%d km", esc$km), color = ink, size = 3)
  g
}

# Versão ESTÁTICA (geom_sf) do mapa de aglomerados LISA/Gi*/scan — para exportar
# como imagem no manuscrito, sem depender de print da tela do leaflet. Mesmo
# join e mesmas cores de `mapa_leaflet_clusters`, mas "Não significativo" vira um
# cinza CLARO (como no padrão da revista) para ler bem sobre o branco. Norte e
# barra de escala inclusos (exigência da RESP para mapas).
mapa_estatico_clusters <- function(geo, df, col_categoria, cores, titulo = NULL,
                                   subtitulo = NULL, cor_sem_dado = "#e6e6e6") {
  if (!requireNamespace("sf", quietly = TRUE)) stop("Pacote 'sf' não instalado.")
  if (is.null(df) || nrow(df) == 0 || is.null(geo)) return(NULL)
  geo$cod6 <- substr(as.character(geo$code_muni), 1, 6)
  df2   <- df[, intersect(c("cod6", col_categoria), names(df)), drop = FALSE]
  geo_j <- merge(geo, df2, by = "cod6", all.x = TRUE)
  catv  <- as.character(geo_j[[col_categoria]])
  catv[is.na(catv)] <- "Não significativo"
  geo_j$.cat <- factor(catv, levels = names(cores))
  cores2 <- cores
  if ("Não significativo" %in% names(cores2)) cores2["Não significativo"] <- cor_sem_dado

  g <- ggplot(geo_j) +
    geom_sf(aes(fill = .cat), color = "grey60", linewidth = 0.06) +
    scale_fill_manual(values = cores2, breaks = names(cores2), drop = FALSE,
                      name = NULL, na.value = cor_sem_dado) +
    labs(title = titulo, subtitle = subtitulo, x = NULL, y = NULL) +
    coord_sf(expand = FALSE) +
    theme_minimal(base_size = 12) +
    theme(plot.background  = element_rect(fill = .plot_bg(), color = NA),
          panel.background = element_rect(fill = .plot_bg(), color = NA),
          panel.grid       = element_blank(),
          axis.text        = element_blank(), axis.ticks = element_blank(),
          plot.title       = element_text(color = .plot_ink(), face = "bold"),
          plot.subtitle    = element_text(color = .plot_muted()),
          legend.text      = element_text(color = .plot_ink()),
          legend.position  = "left",
          legend.key       = element_rect(color = "grey60", linewidth = 0.2))
  .add_norte_escala(g, geo_j)
}
