# 07_selecao_estados.R
# ---------------------------------------------------------------------------
# Utilitarios de escopo geografico: expansao de pseudo-codigos de regiao (N, NE, CO, SUD, S, BR) para as UFs reais e agrupamento inverso por regiao.
#
# Funcoes do EpiSUS efetivamente utilizadas na analise do artigo
# "Escorpionismo no Brasil, 2016-2025". Codigo extraido VERBATIM do
# modulo de logica pura core_datasus.R do aplicativo EpiSUS (Shiny),
# sem a camada de interface. R 4.6.0.
# ---------------------------------------------------------------------------

# ─────────────────────────────────────────────────────────────────────────────
# SELEÇÃO DE ESTADOS (pseudo-UF de região: N, NE, CO, SUD, S, BR)
# ─────────────────────────────────────────────────────────────────────────────

# Expande pseudo-códigos de região (ex.: "NE", "BR") para os códigos reais de
# UF que eles representam. Permite marcar "Nordeste (região inteira)" ou
# "Brasil (todos os estados)" numa lista de checkbox em vez de selecionar
# manualmente cada estado. Suporta qualquer combinação de regiões + estados
# soltos, sem duplicar.
expand_uf <- function(codes) {
  regioes <- intersect(codes, c(names(REGIAO_UF), "BR"))
  if (length(regioes) == 0) return(codes)

  extra <- unlist(lapply(regioes, function(r) {
    if (r == "BR") names(UF_NOME) else REGIAO_UF[[r]]
  }))
  unique(c(setdiff(codes, regioes), extra))
}

# Caminho inverso do expand_uf: agrupa um vetor de UFs reais pelas regiões a
# que pertencem. Devolve uma lista nomeada pelo CÓDIGO da região (N, NE, CO,
# SUD, S), na ordem canônica do IBGE, contendo só as regiões que têm ao menos
# uma UF no vetor. Códigos desconhecidos (ou pseudo-códigos que ninguém
# expandiu antes) são simplesmente ignorados — nunca viram uma região vazia.
agrupar_por_regiao <- function(ufs) {
  ufs <- unique(ufs)
  res <- lapply(REGIAO_UF, function(estados) intersect(estados, ufs))
  res[vapply(res, length, integer(1)) > 0]
}
