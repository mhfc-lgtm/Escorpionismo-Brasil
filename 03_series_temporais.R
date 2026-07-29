# 03_series_temporais.R
# ---------------------------------------------------------------------------
# Construcao das series mensais de contagens/taxas por grupo (total, faixa etaria, sexo, raca) com preenchimento da grade tempo x categoria.
#
# Funcoes do EpiSUS efetivamente utilizadas na analise do artigo
# "Escorpionismo no Brasil, 2016-2025". Codigo extraido VERBATIM do
# modulo de logica pura core_datasus.R do aplicativo EpiSUS (Shiny),
# sem a camada de interface. R 4.6.0.
# ---------------------------------------------------------------------------

# ─────────────────────────────────────────────────────────────────────────────
# SÉRIES TEMPORAIS
# ─────────────────────────────────────────────────────────────────────────────

# NOTA — bug do "mês zero" (investigado, confirmado e corrigido nesta versão):
# `group_by(ano, mes) %>% summarise(count = n())` só gera uma LINHA para
# combinações de (ano, mes) que aparecem nos dados filtrados. Um mês sem
# NENHUM caso (comum em recortes raros: uma faixa etária específica de uma
# doença pouco frequente, num estado pequeno) não vira "count = 0" — a linha
# simplesmente não existe. O modelo Poisson então nunca "vê" esse mês, em vez
# de aprender que a taxa ali foi zero. Isso é invisível quando o total é alto
# (sempre tem algum caso em algum mês, ex.: internações gerais), mas gera
# viés real em recortes finos com poucos casos — exatamente o padrão do gap
# residual observado na aba Faixa Etária durante a validação contra o
# TabNet. `.completar_grade()` preenche esses buracos com count=0 antes de
# seguir pro modelo.
.completar_grade <- function(d, ano_ini, ano_fim, categorias = NULL) {
  if (nrow(d) == 0 && (is.null(ano_ini) || is.null(ano_fim))) return(d)
  a_ini <- if (!is.null(ano_ini)) ano_ini else suppressWarnings(min(d$ano))
  a_fim <- if (!is.null(ano_fim)) ano_fim else suppressWarnings(max(d$ano))
  if (!is.finite(a_ini) || !is.finite(a_fim)) return(d)

  grade <- if (is.null(categorias)) {
    expand.grid(ano = a_ini:a_fim, mes = 1:12)
  } else {
    expand.grid(ano = a_ini:a_fim, mes = 1:12, gval = categorias,
                stringsAsFactors = FALSE)
  }
  chaves <- if (is.null(categorias)) c("ano","mes") else c("ano","mes","gval")
  d2 <- merge(grade, d, by = chaves, all.x = TRUE)
  d2$count[is.na(d2$count)] <- 0L
  d2
}

serie_total <- function(df_raw, ufs, pop, ano_ini = NULL, ano_fim = NULL, faixa_eca = "geral") {
  nomes <- nome_estados(ufs)
  d <- df_raw %>%
    filter(uf_res %in% ufs) %>%
    group_by(ano, mes) %>%
    summarise(count=n(), .groups="drop")
  d <- .completar_grade(d, ano_ini, ano_fim)

  # Denominador populacional: "geral" (padrão — e o que toda chamada
  # existente antes desta mudança continua recebendo) usa a população total
  # do(s) estado(s); um preset restrito (get_pop_eca()) usa só a população
  # da faixa etária correspondente — sem isso, a taxa ficaria artificialmente
  # baixa (numerador só de crianças/idosos dividido pela população de TODAS
  # as idades).
  d %>%
    mutate(population=sapply(ano, function(y) get_pop_eca(pop, nomes, faixa_eca, y))) %>%
    filter(!is.na(population), population>0)
}

# manter_sem_pop: por padrão (FALSE) uma categoria sem denominador é DESCARTADA
# — o modelo precisa do offset populacional, não há taxa sem população. Isso,
# porém, apaga a categoria da tela sem dizer nada: se o rótulo do numerador
# (clf_faixa, clf_raca...) deixar de casar com o do denominador (pop$faixa,
# pop$raca...), os casos simplesmente somem, e o usuário não tem como saber se
# a categoria não existe no recorte ou se o pareamento quebrou. Com TRUE as
# linhas ficam com population = NA, para as tabelas de diagnóstico poderem
# mostrar os casos brutos e denunciar o descasamento.
serie_grupo <- function(df_raw, ufs, pop, col_raw, tipo_pop, col_pop,
                         ano_ini = NULL, ano_fim = NULL, manter_sem_pop = FALSE) {
  nomes <- nome_estados(ufs)
  d <- df_raw %>%
    filter(uf_res %in% ufs, !is.na(.data[[col_raw]])) %>%
    rename(gval=!!col_raw) %>%
    group_by(ano, mes, gval) %>%
    summarise(count=n(), .groups="drop")
  # Só preenche zeros pras categorias que de fato existem no recorte (ex.:
  # "Branca"/"Preta"/"Parda" que aparecem em algum momento) — uma categoria
  # que nunca aparece em NENHUM mês continua invisível, pois não há como
  # saber que ela existiria sem conhecimento externo do domínio da coluna.
  categorias <- unique(d$gval)
  d <- .completar_grade(d, ano_ini, ano_fim, categorias = categorias)
  out <- d %>%
    mutate(
      population=mapply(function(g,y) get_pop_grupo(pop[[tipo_pop]], nomes, col_pop, g, y),
                        gval, ano)
    ) %>%
    rename(!!col_raw:=gval)
  if (manter_sem_pop) out else out %>% filter(!is.na(population), population>0)
}

# Categorias que TÊM casos no recorte mas ficaram sem denominador (rótulo do
# numerador não casou com nenhuma linha da tabela de população). Vazio = tudo
# pareado. Serve para avisar na tela em vez de sumir com a categoria.
cats_sem_denominador <- function(df_raw, ufs, pop, col_raw, tipo_pop, col_pop,
                                  ano_ini = NULL, ano_fim = NULL) {
  sg <- serie_grupo(df_raw, ufs, pop, col_raw, tipo_pop, col_pop,
                    ano_ini, ano_fim, manter_sem_pop = TRUE)
  if (nrow(sg) == 0) return(character(0))
  ruins <- sg[(is.na(sg$population) | sg$population <= 0) & sg$count > 0, col_raw, drop = TRUE]
  sort(unique(as.character(ruins)))
}
