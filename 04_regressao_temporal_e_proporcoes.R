# 04_regressao_temporal_e_proporcoes.R
# ---------------------------------------------------------------------------
# Nucleo estatistico. Regressao de Poisson com erros-padrao robustos HAC (Newey-West, kernel de Bartlett, lag=12) e offset=log(populacao) -> IRR/IC95%; sazonalidade (IRR por mes); regressao logistica de gravidade/evolucao ajustada por mes e tendencia anual -> OR; IC de proporcao de Wilson; e o gerador da tabela Brasil->regioes->estados (crescimento anual medio).
#
# Funcoes do EpiSUS efetivamente utilizadas na analise do artigo
# "Escorpionismo no Brasil, 2016-2025". Codigo extraido VERBATIM do
# modulo de logica pura core_datasus.R do aplicativo EpiSUS (Shiny),
# sem a camada de interface. R 4.6.0.
# ---------------------------------------------------------------------------

# ─────────────────────────────────────────────────────────────────────────────
# ANÁLISE ESTATÍSTICA
# ─────────────────────────────────────────────────────────────────────────────

hac_vcov <- function(mod) {
  tryCatch(sandwich::NeweyWest(mod, lag=12, prewhite=FALSE, adjust=TRUE),
           error=function(e) sandwich::vcovHAC(mod))
}

fit_poisson <- function(serie) {
  d <- serie %>%
    mutate(year_c  = ano - mean(ano),
           mes_f   = relevel(factor(mes), ref="1"),
           log_pop = log(pmax(population, 1)))
  mod <- glm(count ~ mes_f + year_c + offset(log_pop),
             family=poisson(link="log"), data=d)
  list(mod=mod, vcov=hac_vcov(mod), data=d)
}

irr_ci <- function(est, se) {
  z <- qnorm(0.975)
  list(IRR=exp(est), IC_inf=exp(est-z*se), IC_sup=exp(est+z*se))
}

extrair_tend <- function(res) {
  ct  <- coeftest(res$mod, vcov=res$vcov)
  est <- ct["year_c","Estimate"]; se <- ct["year_c","Std. Error"]
  c(irr_ci(est, se), list(p=ct["year_c","Pr(>|z|)"]))
}

# ─────────────────────────────────────────────────────────────────────────────
# JOINPOINT (pontos de inflexão da tendência) — estilo NCI Joinpoint
# ─────────────────────────────────────────────────────────────────────────────
# Reimplementado em R base (não usa mais 'segmented'), alinhado ao Joinpoint
# Regression Program do NCI:
#  • Os pontos de inflexão caem em ANOS OBSERVADOS (grade inteira) — não em
#    posições contínuas — então o AAPC é reprodutível a partir dos APCs/anos
#    exibidos. Modelo linear por partes (funções-dobradiça) no log da taxa.
#  • SELEÇÃO do nº de joinpoints por QUASI-BIC: deviance/φ_ref + log(n)·npar, com
#    a deviance ESCALONADA pela superdispersão φ_ref (dispersão de Pearson do
#    modelo mais flexível, com piso 1). Leva a superdispersão em conta. NÃO se usa
#    BIC "puro": quase-verossimilhança não é verossimilhança (BIC() num glm
#    quasipoisson é NA), e um BIC de Poisson ignoraria φ e SOBRE-selecionaria
#    inflexões em contagens muito dispersas. Sem superdispersão (φ≤1) recai no
#    BIC de Poisson usual.
#  • INFERÊNCIA por QUASI-POISSON: o erro-padrão é inflado pela superdispersão
#    (φ = Pearson/df), corrigindo o IC irrealisticamente estreito que o Poisson
#    puro dá sobre contagens grandes; o IC usa a distribuição t com os graus de
#    liberdade residuais (amostras curtas), como no NCI.
#  • APC (Annual Percent Change) por segmento = (e^slope − 1)·100; AAPC = média
#    dos slopes ponderada pela duração dos segmentos, com IC próprio (combinação
#    linear dos coeficientes). `serie_anual`: colunas ano, count (casos), pop.
#  • LIMITAÇÃO declarada: os IC de APC/AAPC são CONDICIONAIS aos joinpoints
#    selecionados — não incorporam a incerteza da LOCALIZAÇÃO das inflexões, logo
#    são anticonservadores (os p são otimistas). O tratamento pleno seria um
#    bootstrap paramétrico da série inteira refazendo a seleção em cada réplica
#    (como discute Kim et al. 2000, que motiva o teste de permutação do NCI).

# Ajuste linear por partes (dobradiças em `knots`, anos inteiros interiores).
.jp_design <- function(ano, knots) {
  M <- matrix(ano - min(ano), ncol = 1, dimnames = list(NULL, "x0"))
  for (j in seq_along(knots)) M <- cbind(M, pmax(ano - knots[j], 0))
  if (length(knots)) colnames(M)[-1] <- paste0("h", seq_along(knots))
  as.data.frame(M)
}
.jp_fit <- function(d, knots, family) {
  X <- .jp_design(d$ano, knots); X$count <- d$count; X$lpop <- d$lpop
  preds <- setdiff(names(X), c("count", "lpop"))
  glm(stats::reformulate(c(preds, "offset(lpop)"), response = "count"),
      family = family, data = X)
}

extrair_saz <- function(res) {
  ct  <- coeftest(res$mod, vcov=res$vcov)
  z   <- qnorm(0.975)
  do.call(rbind, lapply(1:12, function(m) {
    if (m==1) return(data.frame(Mes=MESES_PT[1],IRR=1,IC_inf=NA,IC_sup=NA,p=NA))
    lab <- paste0("mes_f",m)
    if (!lab %in% rownames(ct))
      return(data.frame(Mes=MESES_PT[m],IRR=NA,IC_inf=NA,IC_sup=NA,p=NA))
    est <- ct[lab,"Estimate"]; se <- ct[lab,"Std. Error"]
    data.frame(Mes=MESES_PT[m], IRR=exp(est),
               IC_inf=exp(est-z*se), IC_sup=exp(est+z*se),
               p=ct[lab,"Pr(>|z|)"])
  }))
}

fit_periodo <- function(serie, ref="Pre") {
  d <- serie %>%
    mutate(year_c  = ano - mean(ano),
           mes_f   = relevel(factor(mes), ref="1"),
           log_pop = log(pmax(population,1)),
           period  = relevel(
             factor(cut(ano, breaks=PERIOD_BINS, labels=PERIOD_LABELS)),
             ref=ref))
  mod <- glm(count ~ period + mes_f + offset(log_pop),
             family=poisson(link="log"), data=d)
  list(mod=mod, vcov=hac_vcov(mod))
}

extrair_periodo <- function(res, ref="Pre") {
  ct  <- coeftest(res$mod, vcov=res$vcov)
  z   <- qnorm(0.975)
  do.call(rbind, lapply(PERIOD_LABELS, function(lab) {
    if (lab==ref)
      return(data.frame(periodo=lab,IRR=1,IC_inf=NA,IC_sup=NA,p=NA,
                        stringsAsFactors=FALSE))
    key <- paste0("period",lab)
    if (!key %in% rownames(ct)) return(NULL)
    est <- ct[key,"Estimate"]; se <- ct[key,"Std. Error"]
    data.frame(periodo=lab, IRR=exp(est), IC_inf=exp(est-z*se),
               IC_sup=exp(est+z*se), p=ct[key,"Pr(>|z|)"],
               stringsAsFactors=FALSE)
  }))
}

fit_tend_adj <- function(serie) {
  d <- serie %>%
    mutate(year_c   = ano - mean(ano),
           mes_f    = relevel(factor(mes), ref="1"),
           log_pop  = log(pmax(population,1)),
           pandemia = as.integer(ano>=2020 & ano<=2021))
  mod <- glm(count ~ mes_f + year_c + pandemia + offset(log_pop),
             family=poisson(link="log"), data=d)
  list(mod=mod, vcov=hac_vcov(mod))
}

demog_hac <- function(sg, grupo_col, ref_cat) {
  d <- sg %>%
    mutate(year_c  = ano - mean(ano),
           mes_f   = relevel(factor(mes), ref="1"),
           log_pop = log(pmax(population,1)),
           grp     = relevel(factor(.data[[grupo_col]]), ref=ref_cat))
  mod <- glm(count ~ grp + mes_f + year_c + offset(log_pop),
             family=poisson(link="log"), data=d)
  vc  <- hac_vcov(mod)
  ct  <- coeftest(mod, vcov=vc)
  z   <- qnorm(0.975)
  do.call(rbind, lapply(levels(d$grp), function(cat) {
    if (cat==ref_cat)
      return(data.frame(categoria=cat,IRR=1,IC_inf=NA,IC_sup=NA,p=NA,
                        ref=TRUE,stringsAsFactors=FALSE))
    key <- paste0("grp",cat)
    if (!key %in% rownames(ct)) return(NULL)
    est <- ct[key,"Estimate"]; se <- ct[key,"Std. Error"]
    data.frame(categoria=cat, IRR=exp(est), IC_inf=exp(est-z*se),
               IC_sup=exp(est+z*se), p=ct[key,"Pr(>|z|)"],
               ref=FALSE, stringsAsFactors=FALSE)
  }))
}

# ─────────────────────────────────────────────────────────────────────────────
# DESFECHOS DO SINAN — GRAVIDADE E ÓBITO (proporções, não taxas)
# ─────────────────────────────────────────────────────────────────────────────
# Diferença essencial em relação a todo o resto do app: aqui o denominador é o
# CASO NOTIFICADO, não a população. Não faz sentido "óbitos por 100.000
# habitantes" quando a pergunta é letalidade — e usar o Poisson com offset
# populacional responderia outra coisa (risco de morrer na população, que
# confunde incidência com gravidade). Por isso: família binomial sobre
# k sucessos em n casos, com a MESMA correção HAC do resto do app (a série
# mensal de proporções é autocorrelacionada e superdispersa igual à de
# contagens), e IC de proporção por Wilson.

# IC de proporção pelo método de WILSON (score), não pelo de Wald: Wald
# quebra justamente onde estes dados vivem — proporções perto de 0 (letalidade
# de escorpionismo é da ordem de 0,05%), onde ele produz limite inferior
# negativo e cobertura muito abaixo do nominal. Wilson é vetorizado aqui de
# propósito, para entrar direto num mutate por ano/categoria.
prop_wilson <- function(x, n, conf = 0.95) {
  z    <- qnorm(1 - (1 - conf) / 2)
  x    <- as.numeric(x); n <- as.numeric(n)
  phat <- ifelse(n > 0, x / n, NA_real_)
  den  <- 1 + z^2 / n
  cen  <- (phat + z^2 / (2 * n)) / den
  mar  <- z * sqrt(phat * (1 - phat) / n + z^2 / (4 * n^2)) / den
  data.frame(p  = phat,
             lo = pmax(0, cen - mar),
             hi = pmin(1, cen + mar))
}

# Série mensal de um indicador BINÁRIO (k de n) — a matéria-prima de todos os
# modelos desta seção. Registros com o campo ignorado/em branco ficam FORA de
# k e de n: a proporção é calculada sobre quem tem informação, e a completude
# é reportada à parte (tab_completude). Meses sem nenhum registro informado
# saem da série — ao contrário da série de contagens, um mês com n = 0 não é
# "zero casos graves", é ausência de denominador, e não pode virar 0/0.
serie_binaria <- function(df_raw, ufs, ind_col, ano_ini = NULL, ano_fim = NULL,
                           grupo_col = NULL) {
  d <- df_raw %>%
    filter(uf_res %in% ufs, !is.na(ano), !is.na(mes), !is.na(.data[[ind_col]]))
  if (!is.null(ano_ini)) d <- d %>% filter(ano >= ano_ini)
  if (!is.null(ano_fim)) d <- d %>% filter(ano <= ano_fim)
  if (!is.null(grupo_col)) d <- d %>% filter(!is.na(.data[[grupo_col]]))
  chaves <- if (is.null(grupo_col)) c("ano","mes") else c("ano","mes",grupo_col)
  d %>%
    group_by(across(all_of(chaves))) %>%
    summarise(k = sum(.data[[ind_col]], na.rm = TRUE), n = dplyr::n(), .groups = "drop") %>%
    filter(n > 0)
}

# Proporção agregada por ano, com IC 95% de Wilson. `escala` multiplica o
# resultado (1 = proporção, 100 = %, 1000 = por mil) — letalidade de
# escorpionismo se reporta por 1.000 casos, % daria "0,0" em toda linha.
tab_prop_ano <- function(sb, escala = 100) {
  d <- sb %>% group_by(Ano = ano) %>%
    summarise(k = sum(k), n = sum(n), .groups = "drop")
  ic <- prop_wilson(d$k, d$n)
  d %>% mutate(prop = ic$p * escala, IC_inf = ic$lo * escala, IC_sup = ic$hi * escala)
}

# Tendência da PROPORÇÃO no tempo: logística sobre a série mensal, com dummies
# de mês (mesma lógica sazonal do resto do app) e ano centrado. O efeito sai
# como razão de CHANCES por ano (OR), não razão de taxas — a interpretação
# muda e está rotulada assim na tela. A vcov é a mesma NeweyWest lag=12.
fit_prop <- function(serie) {
  if (length(unique(serie$ano)) < 2)
    stop("Tendência da proporção exige pelo menos 2 anos com informação.")
  d <- serie %>%
    mutate(year_c = ano - mean(ano),
           mes_f  = relevel(factor(mes), ref = "1"))
  mod <- glm(cbind(k, n - k) ~ mes_f + year_c, family = binomial(link = "logit"), data = d)
  list(mod = mod, vcov = hac_vcov(mod), data = d)
}

extrair_tend_prop <- function(res) {
  ct  <- coeftest(res$mod, vcov = res$vcov)
  est <- ct["year_c","Estimate"]; se <- ct["year_c","Std. Error"]
  c(irr_ci(est, se), list(p = ct["year_c","Pr(>|z|)"]))   # exp(beta) = OR/ano
}

# Extrai o efeito de cada nível de um fator a partir de um coeftest, no shape
# que gg_forest/tbl_irr já consomem (categoria, IRR, IC_inf, IC_sup, p, ref).
# A coluna se chama IRR por compatibilidade com esses dois — aqui o número é
# uma razão de CHANCES; quem chama rotula o eixo.
.efeito_por_nivel <- function(ct, niveis, ref_cat, prefixo = "grp") {
  z <- qnorm(0.975)
  do.call(rbind, lapply(niveis, function(cat) {
    if (cat == ref_cat)
      return(data.frame(categoria = cat, IRR = 1, IC_inf = NA, IC_sup = NA,
                        p = NA, ref = TRUE, stringsAsFactors = FALSE))
    key <- paste0(prefixo, cat)
    if (!key %in% rownames(ct)) return(NULL)
    est <- ct[key,"Estimate"]; se <- ct[key,"Std. Error"]
    data.frame(categoria = cat, IRR = exp(est), IC_inf = exp(est - z * se),
               IC_sup = exp(est + z * se), p = ct[key,"Pr(>|z|)"],
               ref = FALSE, stringsAsFactors = FALSE)
  }))
}

# OR de um desfecho binário por categoria (faixa etária, sexo, gravidade...),
# ajustado por mês e por tendência anual — o análogo de demog_hac para
# proporções. `sb` vem de serie_binaria() com grupo_col preenchido.
or_hac <- function(sb, grupo_col, ref_cat) {
  d <- sb %>%
    mutate(mes_f = relevel(factor(mes), ref = "1"),
           grp   = relevel(factor(.data[[grupo_col]]), ref = ref_cat))
  # Com um único ano no recorte, year_c seria constante (zero) e entraria como
  # coluna colinear com o intercepto — o termo de tendência só entra quando há
  # 2+ anos para estimá-lo.
  if (length(unique(d$ano)) >= 2) {
    d$year_c <- d$ano - mean(d$ano)
    fml <- cbind(k, n - k) ~ grp + mes_f + year_c
  } else {
    fml <- cbind(k, n - k) ~ grp + mes_f
  }
  mod <- glm(fml, family = binomial(link = "logit"), data = d)
  ct  <- coeftest(mod, vcov = hac_vcov(mod))
  .efeito_por_nivel(ct, levels(d$grp), ref_cat)
}

# Completude dos campos clínicos: sem isto, toda proporção desta seção é
# ininterpretável. Um campo com 30% de ignorado admite viés de seleção maior
# que o efeito que se está medindo — e no SINAN a completude varia MUITO por
# ano e por UF, então a tabela sai por campo e o texto avisa.
tab_completude <- function(df_raw, ufs, campos, ano_ini = NULL, ano_fim = NULL) {
  d <- df_raw %>% filter(uf_res %in% ufs)
  if (!is.null(ano_ini)) d <- d %>% filter(ano >= ano_ini)
  if (!is.null(ano_fim)) d <- d %>% filter(ano <= ano_fim)
  campos <- campos[campos %in% names(d)]
  if (nrow(d) == 0 || length(campos) == 0) return(NULL)
  do.call(rbind, lapply(campos, function(cp) {
    v  <- d[[cp]]
    ok <- sum(!is.na(v))
    data.frame(Campo = cp, Registros = nrow(d), `Com informação` = ok,
               `% ignorado/em branco` = round((nrow(d) - ok) / nrow(d) * 100, 1),
               check.names = FALSE, stringsAsFactors = FALSE)
  }))
}

# Distribuição de uma categórica (ex.: gravidade) com IC 95% de Wilson por
# nível, calculada sobre os registros COM informação.
tab_distribuicao <- function(df_raw, ufs, col, niveis, ano_ini = NULL, ano_fim = NULL) {
  d <- df_raw %>% filter(uf_res %in% ufs, !is.na(.data[[col]]))
  if (!is.null(ano_ini)) d <- d %>% filter(ano >= ano_ini)
  if (!is.null(ano_fim)) d <- d %>% filter(ano <= ano_fim)
  if (nrow(d) == 0) return(NULL)
  n_tot <- nrow(d)
  cont  <- vapply(niveis, function(x) sum(d[[col]] == x, na.rm = TRUE), numeric(1))
  ic    <- prop_wilson(cont, rep(n_tot, length(niveis)))
  data.frame(Categoria = niveis, Casos = as.integer(cont),
             `%` = round(ic$p * 100, 2),
             `IC 95%` = sprintf("%.2f – %.2f", ic$lo * 100, ic$hi * 100),
             check.names = FALSE, stringsAsFactors = FALSE)
}

taxa_aj <- function(res, serie) {
  tx_aj  <- mean(fitted(res$mod)/serie$population*1e5, na.rm=TRUE)
  O      <- sum(serie$count)
  P      <- sum(serie$population)
  tx_br  <- O/P*1e5
  fat    <- if (tx_br>0) tx_aj/tx_br else 1
  lo <- qchisq(0.025,2*O)/2; hi <- qchisq(0.975,2*(O+1))/2
  list(taxa=tx_aj, ic_inf=lo/P*1e5*fat, ic_sup=hi/P*1e5*fat)
}

rpi_calc <- function(taxa_ref, serie_alvo) {
  O  <- sum(serie_alvo$count)
  E  <- taxa_ref/1e5 * sum(serie_alvo$population)
  rpi <- O/E
  lo  <- qchisq(0.025,2*O)/2/E
  hi  <- qchisq(0.975,2*(O+1))/2/E
  p   <- min(1, 2*min(ppois(O,E), ppois(O-1,E,lower.tail=FALSE)))
  list(rpi=rpi, ic_inf=lo, ic_sup=hi, p=p)
}

disp_stat <- function(serie) {
  d <- serie %>% mutate(year_c=ano-mean(ano),
                        mes_f=relevel(factor(mes),ref="1"),
                        log_pop=log(pmax(population,1)))
  mod <- glm(count ~ mes_f + year_c + offset(log_pop), family=poisson(), data=d)
  sum(residuals(mod,"pearson")^2)/mod$df.residual
}

# ─────────────────────────────────────────────────────────────────────────────
# ─────────────────────────────────────────────────────────────────────────────
# FORMATAÇÃO
# ─────────────────────────────────────────────────────────────────────────────

sig <- function(p) {
  ifelse(is.na(p),"ref",
  ifelse(p<0.001,"***",
  ifelse(p<0.01,"**",
  ifelse(p<0.05,"*","ns"))))
}

fmt_p <- function(p) {
  ifelse(is.na(p),"—",
  ifelse(p<0.001,"p<0,001",
  paste0("p=",format(round(p,3),nsmall=3,decimal.mark=","))))
}

tbl_irr <- function(df) {
  df %>% mutate(
    IRR     = ifelse(ref,"ref (1,00)",sprintf("%.4f",IRR)),
    `IC 95%`= ifelse(ref,"—",sprintf("%.4f – %.4f",IC_inf,IC_sup)),
    `p-valor`=fmt_p(p),
    Sig     = sig(p)
  ) %>% select(Categoria=categoria,IRR,`IC 95%`,`p-valor`,Sig)
}

# ─────────────────────────────────────────────────────────────────────────────
# TABELA BRASIL (regiões + estados) — .docx no padrão RESP
# ─────────────────────────────────────────────────────────────────────────────
# Ajusta um Poisson HAC (lag=12) sobre a série mensal de um grupo de UFs e
# devolve: nº de casos, TAXA MÉDIA ANUAL por 100 mil hab. (taxa mensal ajustada
# × 12) e o CRESCIMENTO anual = (IRR − 1) × 100, com IC95% e p. Usa exatamente
# as mesmas funções das abas por-estado / por-região (serie_total, fit_poisson,
# extrair_tend, taxa_aj), então os números batem com a tela.
.fit_grupo_taxa <- function(df, pop, ufs, ano_ini, ano_fim, faixa_eca = "geral") {
  tryCatch({
    s <- serie_total(df, ufs, pop, ano_ini, ano_fim, faixa_eca = faixa_eca)
    if (nrow(s) < 6) return(NULL)
    res  <- fit_poisson(s)
    tend <- extrair_tend(res)
    txa  <- taxa_aj(res, s)
    data.frame(n = sum(s$count), taxa_ano = txa$taxa * 12,
               cresc = (tend$IRR - 1) * 100,
               ic_lo = (tend$IC_inf - 1) * 100,
               ic_hi = (tend$IC_sup - 1) * 100,
               p = tend$p, stringsAsFactors = FALSE)
  }, error = function(e) NULL)
}

# Monta o quadro hierárquico Brasil → regiões (por taxa desc) → estados (por
# taxa desc) para a tabela do manuscrito. Colunas: nivel/regiao/rotulo + n,
# taxa_ano, cresc, ic_lo, ic_hi, p. `ufs` = grupo analisado (para o "Brasil todo"
# são as 27 UFs). Devolve NULL se não houver UF válida.
resumo_brasil_regioes <- function(df, pop, ufs, ano_ini, ano_fim,
                                  faixa_eca = "geral") {
  ufs <- intersect(ufs, names(UF_NOME))
  if (!length(ufs)) return(NULL)
  ff <- function(u) .fit_grupo_taxa(df, pop, u, ano_ini, ano_fim, faixa_eca)
  mk <- function(nivel, regiao, rotulo, d) {
    if (is.null(d)) return(NULL)
    data.frame(nivel = nivel, regiao = regiao, rotulo = rotulo,
               n = d$n, taxa_ano = d$taxa_ano, cresc = d$cresc,
               ic_lo = d$ic_lo, ic_hi = d$ic_hi, p = d$p,
               stringsAsFactors = FALSE)
  }
  br <- mk("brasil", "", "Brasil", ff(ufs))

  grupos   <- agrupar_por_regiao(ufs)                 # região -> UFs presentes
  reg_rows <- Filter(Negate(is.null), lapply(names(grupos), function(r)
    mk("regiao", r, unname(REGIAO_NOME[[r]]), ff(grupos[[r]]))))
  if (length(reg_rows))
    reg_rows <- reg_rows[order(-vapply(reg_rows, function(x) x$taxa_ano, numeric(1)))]

  blocos <- lapply(reg_rows, function(rr) {
    est <- Filter(Negate(is.null), lapply(grupos[[rr$regiao]], function(u)
      mk("estado", rr$regiao, unname(UF_NOME[[u]]), ff(u))))
    if (length(est))
      est <- est[order(-vapply(est, function(x) x$taxa_ano, numeric(1)))]
    do.call(rbind, c(list(rr), est))
  })
  res <- do.call(rbind, c(list(br), blocos))
  rownames(res) <- NULL
  res
}

# Formata o quadro de resumo_brasil_regioes em texto (padrão RESP: vírgula
# decimal, IC95% na mesma célula do crescimento). Compartilhado pela flextable
# (.docx) e pela prévia em tela, para os números serem idênticos.
disp_brasil <- function(res, contagem_rotulo = "Casos") {
  f2 <- function(x) formatC(x, format = "f", digits = 2, big.mark = ".",
                            decimal.mark = ",")
  fp <- function(p) ifelse(is.na(p), "—",
          ifelse(p < 0.001, "<0,001",
                 formatC(round(p, 3), format = "f", digits = 3,
                         decimal.mark = ",")))
  disp <- data.frame(
    Local = res$rotulo,
    N     = formatC(res$n, format = "d", big.mark = ".", decimal.mark = ","),
    Taxa  = f2(res$taxa_ano),
    Cresc = sprintf("%s (%s; %s)", f2(res$cresc), f2(res$ic_lo), f2(res$ic_hi)),
    P     = fp(res$p),
    check.names = FALSE, stringsAsFactors = FALSE)
  names(disp) <- c("Região e unidade federativa",
                   sprintf("%s, n", contagem_rotulo),
                   "Taxa média anual por 100 mil hab.",
                   "Crescimento anual, % (IC95%)", "Valor p")
  disp
}

# Constrói a flextable (padrão RESP: Times New Roman 10 pt, cabeçalho em
# negrito, Brasil e regiões em negrito, estados recuados, numéricos à direita).
# `titulo` é a legenda autossuficiente (local, ano, n). `nota` (opcional) vira
# rodapé. `contagem_rotulo` nomeia a coluna de contagem (ex.: "Casos").
flextable_brasil <- function(res, titulo, nota = NULL, contagem_rotulo = "Casos") {
  if (!requireNamespace("flextable", quietly = TRUE) ||
      !requireNamespace("officer", quietly = TRUE))
    stop("Pacotes 'flextable' e 'officer' são necessários para exportar .docx.")
  disp <- disp_brasil(res, contagem_rotulo)
  ib <- which(res$nivel %in% c("brasil", "regiao"))
  ie <- which(res$nivel == "estado")
  ft <- flextable::flextable(disp)
  ft <- flextable::set_caption(ft, titulo)
  if (length(ib)) ft <- flextable::bold(ft, i = ib, part = "body")
  if (length(ie)) ft <- flextable::padding(ft, i = ie, j = 1,
                                            padding.left = 18, part = "body")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::align(ft, j = 2:5, align = "right", part = "all")
  ft <- flextable::align(ft, j = 1, align = "left",  part = "all")
  ft <- flextable::font(ft, fontname = "Times New Roman", part = "all")
  ft <- flextable::fontsize(ft, size = 10, part = "all")
  ft <- flextable::padding(ft, padding.top = 1, padding.bottom = 1, part = "body")
  if (!is.null(nota) && nzchar(nota)) {
    ft <- flextable::add_footer_lines(ft, nota)
    ft <- flextable::font(ft, fontname = "Times New Roman", part = "footer")
    ft <- flextable::fontsize(ft, size = 9, part = "footer")
  }
  flextable::autofit(ft)
}

# Salva a flextable em .docx com margens de 1,5 cm (padrão RESP), retrato.
salvar_tabela_docx <- function(ft, file) {
  sect <- officer::prop_section(
    page_size    = officer::page_size(orient = "portrait"),
    page_margins = officer::page_mar(top = 0.59, bottom = 0.59,
                                     left = 0.59, right = 0.59))
  flextable::save_as_docx(ft, path = file, pr_section = sect)
}
