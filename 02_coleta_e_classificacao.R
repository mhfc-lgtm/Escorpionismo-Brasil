# 02_coleta_e_classificacao.R
# ---------------------------------------------------------------------------
# Coleta dos microdados do SINAN-Animais Peconhentos via DATASUS (arquivos .dbc anuais nacionais, cache local) e classificacao das variaveis: idade, sexo, raca/cor, gravidade (TRA_CLASSI), evolucao (EVOLUCAO), tempo ate o atendimento e soroterapia. Filtro do agravo: TP_ACIDENT = 3 (escorpiao).
#
# Funcoes do EpiSUS efetivamente utilizadas na analise do artigo
# "Escorpionismo no Brasil, 2016-2025". Codigo extraido VERBATIM do
# modulo de logica pura core_datasus.R do aplicativo EpiSUS (Shiny),
# sem a camada de interface. R 4.6.0.
# ---------------------------------------------------------------------------

# ─────────────────────────────────────────────────────────────────────────────
# COLETA (microdatasus)
# ─────────────────────────────────────────────────────────────────────────────

extrair_uf_ibge <- function(codmun) {
  # Os 2 primeiros dígitos do código de município IBGE identificam a UF —
  # tabela completa das 27 UFs (antes só cobria os 9 estados do Nordeste).
  ibge <- c(
    "11"="RO","12"="AC","13"="AM","14"="RR","15"="PA","16"="AP","17"="TO",
    "21"="MA","22"="PI","23"="CE","24"="RN","25"="PB","26"="PE","27"="AL",
    "28"="SE","29"="BA",
    "31"="MG","32"="ES","33"="RJ","35"="SP",
    "41"="PR","42"="SC","43"="RS",
    "50"="MS","51"="MT","52"="GO","53"="DF"
  )
  unname(ibge[substr(trimws(as.character(codmun)), 1, 2)])
}

# Mapa inverso: sigla da UF -> código IBGE de 2 dígitos (o `code_state` que o
# SIDRA/geobr usam). Usado pela análise espacial de região/Brasil, que precisa
# do code_state de cada estado para buscar a população municipal sem depender
# de já ter o contorno geográfico carregado.
UF_COD_IBGE <- c(
  RO="11", AC="12", AM="13", RR="14", PA="15", AP="16", TO="17",
  MA="21", PI="22", CE="23", RN="24", PB="25", PE="26", AL="27", SE="28", BA="29",
  MG="31", ES="32", RJ="33", SP="35",
  PR="41", SC="42", RS="43",
  MS="50", MT="51", GO="52", DF="53"
)

cid_ok <- function(vec, prefixos) {
  if (length(prefixos)==0 || all(nchar(trimws(prefixos))==0)) return(rep(TRUE, length(vec)))
  pat <- paste0("^(", paste(toupper(trimws(prefixos)), collapse="|"), ")")
  grepl(pat, toupper(trimws(as.character(vec))), perl=TRUE)
}

clf_sexo <- function(x) {
  dplyr::case_when(
    as.character(x) %in% c("1","M","m") ~ "Masculino",
    as.character(x) %in% c("2","3","F","f") ~ "Feminino",
    TRUE ~ NA_character_
  )
}

clf_raca <- function(x) {
  # Código oficial DATASUS/IBGE (RACA_COR): 1=Branca, 2=Preta, 3=Parda,
  # 4=Amarela, 5=Indígena, 9/99=Sem informação.
  # BUG anterior: código 4 (Amarela) era rotulado como "Parda" e o código 3
  # (Parda de verdade) era descartado (NA) — invertia completamente o
  # resultado da análise por raça/cor (ver IRR divergente vs. TabNet).
  dplyr::case_when(
    trimws(as.character(x)) %in% c("1","01") ~ "Branca",
    trimws(as.character(x)) %in% c("2","02") ~ "Preta",
    trimws(as.character(x)) %in% c("3","03") ~ "Parda",
    TRUE ~ NA_character_
  )
}

# Converte IDADE(+COD_IDADE) pra idade em anos completos. Extraído de
# clf_faixa() (que só bucketiza o resultado em década) pra ser reaproveitado
# também pelos presets etários "tipo ECA" do filtro da tela de configuração
# (idade_eca_ok(), mais abaixo), que precisam de cortes exatos (12 e 18 anos)
# que não caem em nenhuma fronteira de década.
idade_em_anos <- function(idade, cod_idade = NULL) {
  # DATASUS (Manual Técnico SIH-RD): IDADE é um valor cru na unidade indicada
  # por COD_IDADE — NÃO é um código único mesclando unidade+valor.
  #   COD_IDADE: 2=dias, 3=meses, 4=anos, 5=anos acima de 100 (IDADE=anos-100)
  # Sem COD_IDADE não há como saber a unidade: values típicos de adultos
  # (IDADE em anos, ex.: 45) ficam < 100 e são indistinguíveis de bebês em dias.
  n <- suppressWarnings(as.numeric(as.character(idade)))

  if (is.null(cod_idade)) {
    # Fallback (sem COD_IDADE no RDS): assume IDADE já em anos.
    # AVISO: isso é uma aproximação — reconstrua o RDS com COD_IDADE para precisão.
    return(ifelse(is.na(n) | n > 130, NA_real_, n))
  }

  cod <- trimws(as.character(cod_idade))
  dplyr::case_when(
    is.na(n)                 ~ NA_real_,
    cod %in% c("2","02")     ~ 0,          # dias        -> <1 ano
    cod %in% c("3","03")     ~ 0,          # meses       -> <1 ano
    cod %in% c("4","04")     ~ n,          # anos
    cod %in% c("5","05")     ~ n + 100,    # 100+ anos
    is.na(cod) | cod %in% c("","0","00") ~ ifelse(n <= 130, n, NA_real_), # fallback linha a linha
    TRUE                      ~ NA_real_
  )
}

clf_faixa <- function(idade, cod_idade = NULL) {
  anos <- idade_em_anos(idade, cod_idade)
  dplyr::case_when(
    is.na(anos) ~ NA_character_,
    anos < 5    ~ "0-4",
    anos < 10   ~ "5-9",
    anos < 20   ~ "10-19",
    anos < 30   ~ "20-29",
    anos < 40   ~ "30-39",
    anos < 50   ~ "40-49",
    anos < 60   ~ "50-59",
    TRUE        ~ "60+"
  )
}

# Ordem de exibição das 8 faixas etárias (do app_datasus.R e dos testes) e
# categoria de referência do modelo (IRR relativo a esta faixa).
FAIXA_ETARIA_NIVEIS <- c("0-4","5-9","10-19","20-29","30-39","40-49","50-59","60+")
FAIXA_ETARIA_REF     <- "30-39"

# ─────────────────────────────────────────────────────────────────────────────
# FILTRO DE FAIXA ETÁRIA "TIPO ECA" (tela de configuração)
# ─────────────────────────────────────────────────────────────────────────────
# Corte por lei em vez de década arbitrária: ECA (Lei 8.069/1990, art. 2º)
# define criança como até 12 anos INCOMPLETOS (idade < 12) e adolescente
# entre 12 e 18 anos incompletos (junto: idade < 18); Estatuto do Idoso
# (Lei 10.741/2003, art. 1º) define idoso como 60 anos ou mais. Nenhum dos
# cortes de criança/adolescente cai em fronteira de década (10/20), por isso
# usa idade_em_anos() direto (exata, registro a registro) em vez de reciclar
# FAIXA_ETARIA_NIVEIS — só "idosos" (60+) coincide com um bucket já existente.
FAIXA_ECA_CHOICES <- c(
  "Geral (todas as idades)"                            = "geral",
  "Crianças (0 a 11 anos — ECA)"                        = "criancas",
  "Crianças e adolescentes (0 a 17 anos — ECA)"         = "criancas_adolescentes",
  "Idosos (60+ anos — Estatuto do Idoso)"                = "idosos"
)

# TRUE/FALSE por registro: pertence ao preset etário escolhido? NA de idade
# sempre vira FALSE (registro descartado, não "indefinido incluído").
idade_eca_ok <- function(anos, preset) {
  switch(preset,
    geral                 = !is.na(anos),
    criancas               = !is.na(anos) & anos < 12,
    criancas_adolescentes = !is.na(anos) & anos < 18,
    idosos                 = !is.na(anos) & anos >= 60,
    rep(FALSE, length(anos))
  )
}

# Denominador populacional pros 4 presets. "geral" e "idosos" são exatos
# (população total, ou o bucket "60+" que já existe em pop$faixa). Os dois
# de criança precisam de pop$faixa_fina (colunas mais finas da Tabela 6407,
# ver carregar_populacao_br()):
#  - "criancas_adolescentes" (0-17) é EXATO: "16 a 17 anos" é o último balde
#    do IBGE antes de 18, então soma-se 0-4+5-9+10-13+14-15+16-17 sem sobra.
#  - "criancas" (0-11) é uma APROXIMAÇÃO: o corte do ECA (12 anos) cai no
#    meio do balde "10 a 13 anos" (10,11,12,13), que o IBGE não quebra mais
#    fino — assume distribuição uniforme dentro do balde e usa metade dele
#    (10 e 11 são 2 dos 4 anos). Avisado na UI e no texto de Método.
get_pop_eca <- function(pop, nomes, preset, yr) {
  if (identical(preset, "geral")) return(get_pop_total(pop, nomes, yr))
  if (identical(preset, "idosos")) return(get_pop_grupo(pop$faixa, nomes, "grupo", "60+", yr))

  fina <- pop$faixa_fina
  if (is.null(fina)) return(NA_real_)
  pega <- function(rotulos) {
    v <- fina$pop[fina$estado %in% nomes & fina$year == yr & fina$grupo %in% rotulos]
    if (length(v) == 0) NA_real_ else sum(v, na.rm = FALSE)
  }

  if (identical(preset, "criancas_adolescentes"))
    return(pega(c("0 a 4 anos","5 a 9 anos","10 a 13 anos","14 a 15 anos","16 a 17 anos")))
  if (identical(preset, "criancas"))
    return(pega(c("0 a 4 anos","5 a 9 anos")) + 0.5 * pega("10 a 13 anos"))

  NA_real_
}

col1 <- function(dat, ...) {
  nms <- c(...)
  found <- intersect(nms, names(dat))
  if (length(found)==0) NA_character_ else found[1]
}

# ── SINAN (Agravos de Notificação) — Animais Peçonhentos ─────────────────────
# O app nasceu binário (SIH x SIM); o SINAN entra como 3º sistema. Diferenças
# estruturais: arquivo é NACIONAL por ano (ANIMBR<AA>.dbc, não por UF); o
# "agravo" (tipo de animal) é um campo (TP_ACIDENT), não um CID-10; idade,
# sexo e raça têm codificação própria.
sistema_tipo <- function(sistema) {
  if (grepl("SINAN|ESCORP|PE[CÇ]ONH|ANIM", sistema, ignore.case = TRUE)) "SINAN"
  else if (grepl("SIH", sistema, ignore.case = TRUE)) "SIH"
  else "SIM"
}

# Tipo de acidente por animal peçonhento (campo TP_ACIDENT do SINAN).
SINAN_ANIM_TIPOS <- c(
  "Serpente"  = "1", "Aranha"    = "2", "Escorpião" = "3",
  "Lagarta"   = "4", "Abelha"    = "5", "Outros"    = "6", "Ignorado" = "9"
)

# NU_IDADE_N do SINAN: 4 dígitos, o 1º é a unidade (1=hora, 2=dia, 3=mês,
# 4=ano) e os 3 últimos o valor. Ex.: 4008 = 8 anos; 3011 = 11 meses (<1 ano).
# Retorna idade em anos completos (unidades < ano viram 0 = "menor de 1 ano").
idade_sinan_anos <- function(x) {
  s <- suppressWarnings(as.integer(as.character(x)))
  u <- s %/% 1000L
  v <- s %%  1000L
  dplyr::case_when(
    is.na(s)                 ~ NA_real_,
    u == 4L & v <= 130L      ~ as.numeric(v),   # anos
    u %in% c(1L, 2L, 3L)     ~ 0,               # hora/dia/mês -> <1 ano
    TRUE                     ~ NA_real_
  )
}

# Raça/cor do SINAN (CS_RACA) segue o padrão IBGE/Censo: 1=Branca, 2=Preta,
# 3=Amarela, 4=PARDA, 5=Indígena, 9=Ignorado. ATENÇÃO: difere do RACA_COR do
# SIH (onde 3=Parda), por isso não dá pra reusar clf_raca() aqui — usaria o
# código errado e trocaria Amarela<->Parda. Mantém as mesmas 3 categorias
# reportadas no resto do app (Branca/Preta/Parda).
clf_raca_sinan <- function(x) {
  dplyr::case_when(
    trimws(as.character(x)) %in% c("1","01") ~ "Branca",
    trimws(as.character(x)) %in% c("2","02") ~ "Preta",
    trimws(as.character(x)) %in% c("4","04") ~ "Parda",
    TRUE ~ NA_character_
  )
}

# ── Campos clínicos do SINAN-ANIM (gravidade, desfecho, tempo, soroterapia) ──
# Estes quatro campos vêm no MESMO arquivo nacional que o app já baixa para
# contar os casos — não custam nenhuma coleta adicional, só deixavam de ser
# selecionados. Eles mudam a natureza da análise: enquanto sexo/idade/raça são
# analisados como TAXA (casos por população), gravidade e óbito são
# PROPORÇÕES DENTRO DOS CASOS notificados — o denominador é o caso, não a
# população, e por isso têm funções próprias (prop_wilson, fit_prop, or_hac)
# em vez de reciclar o Poisson com offset populacional.
#
# TRA_CLASSI — classificação do caso quanto à gravidade. 9 e branco são
# IGNORADO, e viram NA de propósito: entrariam como "leve" numa análise
# ingênua e diluiriam a proporção de graves. A completude é reportada à parte.
clf_gravidade_sinan <- function(x) {
  v <- trimws(as.character(x))
  dplyr::case_when(
    v %in% c("1","01") ~ "Leve",
    v %in% c("2","02") ~ "Moderado",
    v %in% c("3","03") ~ "Grave",
    TRUE               ~ NA_character_
  )
}
GRAVIDADE_NIVEIS <- c("Leve","Moderado","Grave")
GRAVIDADE_REF    <- "Leve"

# EVOLUCAO — 1=Cura, 2=Óbito PELO AGRAVO, 3=Óbito por OUTRA causa, 9=Ignorado.
# A distinção entre 2 e 3 é o que separa letalidade do agravo de mortalidade
# por qualquer causa entre notificados: obito_agravo_sinan() marca só o código
# 2, que é o numerador da letalidade específica reportada na literatura.
clf_evolucao_sinan <- function(x) {
  v <- trimws(as.character(x))
  dplyr::case_when(
    v %in% c("1","01") ~ "Cura",
    v %in% c("2","02") ~ "Óbito pelo agravo",
    v %in% c("3","03") ~ "Óbito por outra causa",
    TRUE               ~ NA_character_
  )
}
obito_agravo_sinan <- function(evolucao) {
  ifelse(is.na(evolucao), NA, evolucao == "Óbito pelo agravo")
}

# ANT_TEMPO_ — tempo decorrido entre a picada e o atendimento, em faixas
# fechadas pelo próprio SINAN (não é contínuo; não dá para tratar como horas).
TEMPO_ATEND_NIVEIS <- c("0-1h","1-3h","3-6h","6-12h","12-24h","24h+")
clf_tempo_atend_sinan <- function(x) {
  v <- trimws(as.character(x))
  dplyr::case_when(
    v %in% c("1","01") ~ "0-1h",
    v %in% c("2","02") ~ "1-3h",
    v %in% c("3","03") ~ "3-6h",
    v %in% c("4","04") ~ "6-12h",
    v %in% c("5","05") ~ "12-24h",
    v %in% c("6","06") ~ "24h+",
    TRUE               ~ NA_character_
  )
}

# CON_SOROTE — indicação de soroterapia (1=Sim, 2=Não, 9=Ignorado).
clf_soroterapia_sinan <- function(x) {
  v <- trimws(as.character(x))
  dplyr::case_when(
    v %in% c("1","01") ~ "Sim",
    v %in% c("2","02") ~ "Não",
    TRUE               ~ NA_character_
  )
}

# Colunas que o processado nacional do SINAN precisa ter. Serve de "versão de
# schema": um cache em disco que não tenha todas é reprocessado, em vez de
# devolver um data.frame sem os campos clínicos e derrubar a aba nova.
COLS_SINAN_PROC <- c("uf_res","cod6","ano","mes","data","sexo","faixa","idade_anos",
                     "cor","gravidade","evolucao","obito_agravo","tempo_atend",
                     "soroterapia")

# ── Download direto do FTP DATASUS com cache persistente ──────────────────────
# Os arquivos DBC são históricos e não mudam — ficam salvos em disco.
# Na segunda rodada (mesmo CID, mesmo estado, mesmo ano) a leitura é instantânea.
CACHE_DIR <- file.path(path.expand("~"), "datasus_cache")
dir.create(file.path(CACHE_DIR, "SIH"), showWarnings=FALSE, recursive=TRUE)
dir.create(file.path(CACHE_DIR, "SIM"), showWarnings=FALSE, recursive=TRUE)
dir.create(file.path(CACHE_DIR, "SINAN"), showWarnings=FALSE, recursive=TRUE)
dir.create(file.path(CACHE_DIR, "INMET"), showWarnings=FALSE, recursive=TRUE)

ler_dbc <- function(url, cache_file) {
  # Se arquivo já está em cache, ler do disco (sem download)
  if (file.exists(cache_file) && file.size(cache_file) > 50) {
    df <- tryCatch(suppressWarnings(read.dbc::read.dbc(cache_file)),
                   error=function(e) NULL)
    if (!is.null(df) && is.data.frame(df) && nrow(df)>0) return(df)
  }
  # Não está em cache: baixar e guardar para próxima vez
  tryCatch({
    options(timeout=180)
    download.file(url, cache_file, quiet=TRUE, mode="wb", method="libcurl")
    if (!file.exists(cache_file) || file.size(cache_file) < 50) {
      unlink(cache_file)
      return(NULL)
    }
    df <- read.dbc::read.dbc(cache_file)
    # Manter cache_file no disco — não apagar
    if (is.data.frame(df) && nrow(df)>0) df else NULL
  }, error=function(e) { unlink(cache_file); NULL },
     warning=function(w) NULL)
}

baixar_sih_rd <- function(uf, ano) {
  yy   <- substr(as.character(ano), 3, 4)
  base <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados"
  partes <- list()
  for (mm in sprintf("%02d", 1:12)) {
    fname  <- paste0("RD", uf, yy, mm, ".dbc")
    cached <- file.path(CACHE_DIR, "SIH", fname)
    df <- ler_dbc(paste0(base, "/", fname), cached)
    if (!is.null(df)) partes[[length(partes)+1]] <- df
  }
  if (length(partes)==0) NULL else suppressWarnings(dplyr::bind_rows(partes))
}

baixar_sim_do <- function(uf, ano) {
  base   <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES"
  fname  <- paste0("DO", uf, ano, ".dbc")
  cached <- file.path(CACHE_DIR, "SIM", fname)
  ler_dbc(paste0(base, "/", fname), cached)
}

# SINAN Animais Peçonhentos: 1 arquivo NACIONAL por ano (ANIMBR<AA>.dbc).
# Tenta FINAIS (anos fechados) e cai pra PRELIM (anos recentes ainda
# preliminares). O cache em disco (ler_dbc) evita rebaixar o arquivo nacional
# — importante porque ele é lido uma vez por ano, não por UF.
baixar_sinan_anim <- function(ano) {
  yy    <- substr(as.character(ano), 3, 4)
  fname <- paste0("ANIMBR", yy, ".dbc")
  bases <- c(
    "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS",
    "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM"
  )
  for (i in seq_along(bases)) {
    # 1 cache por origem (FINAIS/PRELIM) pra não confundir versões do mesmo ano.
    cached <- file.path(CACHE_DIR, "SINAN", paste0(basename(bases[i]), "_", fname))
    df <- ler_dbc(paste0(bases[i], "/", fname), cached)
    if (!is.null(df) && nrow(df) > 0) return(df)
  }
  NULL
}

# Processado NACIONAL por (ano, tipo), COM CACHE PERSISTENTE EM RDS. O .dbc já
# fica em disco (ler_dbc não rebaixa), mas descompactá-lo e reprocessar ~300 mil
# linhas a cada análise é caro — parece que "baixa de novo". Então o resultado
# processado (record-level nacional: filtrado pelo tipo de animal, com UF/mun.
# de residência, ano/mês, sexo, faixa, idade e cor já derivados) é gravado como
# RDS e relido instantaneamente nas rodadas seguintes, inclusive por escopos/UFs
# diferentes (que só filtram uf_res depois). Invalida junto com o cache do .dbc
# (apagar ~/datasus_cache força reprocessar).
.sinan_nacional_processado <- function(ano, tipo = "3") {
  tipo     <- sort(as.character(tipo)); tipo <- tipo[!is.na(tipo) & nzchar(tipo)]
  tipo_key <- if (length(tipo)) paste(tipo, collapse = "-") else "todos"
  cache_rds <- file.path(CACHE_DIR, "SINAN", sprintf("proc_ANIM_%s_tp%s.rds", ano, tipo_key))
  if (file.exists(cache_rds)) {
    d <- tryCatch(readRDS(cache_rds), error = function(e) NULL)
    # COLS_SINAN_PROC é a "versão de schema" do cache: faltando qualquer
    # coluna (a "data" da DLNM diária, os campos clínicos de gravidade/óbito),
    # reprocessa em vez de devolver um shape incompleto.
    if (is.data.frame(d) && all(COLS_SINAN_PROC %in% names(d))) {
      # A faixa etária é RECLASSIFICADA na leitura, e não devolvida como estava
      # gravada: este cache guarda rótulo derivado (clf_faixa), então um cache
      # antigo carregaria a definição de faixa VELHA para dentro de um app com
      # a definição nova. Foi o que aconteceu quando a primeira década virou
      # 0-4/5-9: os registros vinham rotulados "0-9", que não tem mais linha na
      # tabela de população, e a década inteira sumia da aba Faixa Etária sem
      # aviso. idade_anos é o dado bruto e fica no cache — reclassificar a
      # partir dele custa nada e mantém o cache válido para sempre.
      if ("idade_anos" %in% names(d))
        d$faixa <- clf_faixa(d$idade_anos, rep("4", nrow(d)))
      return(d)
    }
  }

  dat <- baixar_sinan_anim(ano)
  if (is.null(dat) || nrow(dat) == 0) return(NULL)

  c_tp <- col1(dat, "TP_ACIDENT", "tp_acident")
  if (!is.na(c_tp) && length(tipo) > 0)
    dat <- dat[as.character(dat[[c_tp]]) %in% tipo, , drop = FALSE]
  if (nrow(dat) == 0) return(NULL)

  c_mun <- col1(dat, "ID_MN_RESI", "id_mn_resi")
  c_dt  <- col1(dat, "DT_NOTIFIC", "dt_notific")
  c_ano <- col1(dat, "NU_ANO", "nu_ano")
  c_sx  <- col1(dat, "CS_SEXO", "cs_sexo")
  c_id  <- col1(dat, "NU_IDADE_N", "nu_idade_n")
  c_rc  <- col1(dat, "CS_RACA", "cs_raca")
  # Campos clínicos — ausentes em anos antigos do SINAN, então cada um é
  # opcional: sem a coluna, o campo vira NA e a aba de gravidade avisa em vez
  # de quebrar.
  # (nomes distintos de c_tp, que acima guarda o TP_ACIDENT usado no filtro)
  c_grav  <- col1(dat, "TRA_CLASSI", "tra_classi")
  c_evo   <- col1(dat, "EVOLUCAO",   "evolucao")
  c_tempo <- col1(dat, "ANT_TEMPO_", "ant_tempo_")
  c_soro  <- col1(dat, "CON_SOROTE", "con_sorote")
  if (is.na(c_mun)) return(NULL)

  dat$uf_res <- extrair_uf_ibge(dat[[c_mun]])
  dat$cod6   <- substr(trimws(as.character(dat[[c_mun]])), 1, 6)

  # Data do caso: DT_NOTIFIC (ano de notificação, padrão TabNet/SINAN); onde
  # for inválida, cai pro NU_ANO (sem mês — o registro sai do comparativo, que
  # exige mês, mas fica nos mapas, que só usam o ano).
  dt     <- if (!is.na(c_dt)) parse_datasus_dt(dat[[c_dt]]) else as.Date(rep(NA, nrow(dat)))
  dt_ano <- suppressWarnings(lubridate::year(dt))
  dt_mes <- suppressWarnings(lubridate::month(dt))
  nu_ano <- if (!is.na(c_ano)) suppressWarnings(as.integer(as.character(dat[[c_ano]]))) else rep(NA_integer_, nrow(dat))
  ok_dt  <- !is.na(dt_ano) & dt_ano >= 1990L & dt_ano <= 2030L
  dat$ano <- as.integer(ifelse(ok_dt, dt_ano, nu_ano))
  dat$mes <- as.integer(ifelse(ok_dt, dt_mes, NA_integer_))
  # Data completa de notificação (para a DLNM DIÁRIA); NA quando a DT_NOTIFIC
  # é inválida (esses registros ficam de fora da série diária, que exige o dia).
  dat$data <- as.Date(rep(NA_real_, nrow(dat)), origin = "1970-01-01")
  dat$data[ok_dt] <- dt[ok_dt]

  anos_i <- if (!is.na(c_id)) idade_sinan_anos(dat[[c_id]]) else rep(NA_real_, nrow(dat))
  dat$idade_anos <- anos_i
  dat$faixa <- clf_faixa(anos_i, rep("4", length(anos_i)))  # cod_idade "4" = anos
  dat$sexo  <- if (!is.na(c_sx)) clf_sexo(dat[[c_sx]])       else NA_character_
  dat$cor   <- if (!is.na(c_rc)) clf_raca_sinan(dat[[c_rc]]) else NA_character_

  dat$gravidade   <- if (!is.na(c_grav))  clf_gravidade_sinan(dat[[c_grav]])   else NA_character_
  dat$evolucao    <- if (!is.na(c_evo))   clf_evolucao_sinan(dat[[c_evo]])     else NA_character_
  dat$obito_agravo<- obito_agravo_sinan(dat$evolucao)
  dat$tempo_atend <- if (!is.na(c_tempo)) clf_tempo_atend_sinan(dat[[c_tempo]])else NA_character_
  dat$soroterapia <- if (!is.na(c_soro))  clf_soroterapia_sinan(dat[[c_soro]]) else NA_character_

  out <- dat %>% filter(!is.na(uf_res)) %>%
    select(all_of(COLS_SINAN_PROC))
  # Cache é bônus — se não conseguir gravar (disco cheio/permissão), segue.
  tryCatch(saveRDS(out, cache_rds), error = function(e) NULL)
  out
}

# Processa 1 ano do SINAN-ANIM para um escopo: pega o processado nacional
# (cacheado) e recorta pelas UFs de residência pedidas. Mesmo shape usado no
# resto do pipeline (record-level), tanto no comparativo quanto nos mapas.
processar_sinan_ano <- function(ano, tipo = "3", ufs_keep = NULL) {
  out <- .sinan_nacional_processado(ano, tipo)
  if (is.null(out) || nrow(out) == 0) return(NULL)
  if (!is.null(ufs_keep)) out <- out[out$uf_res %in% ufs_keep, , drop = FALSE]
  out
}

# Série DIÁRIA de casos por UF (para a DLNM diária de 2 estágios): conta por
# (uf, data de notificação) e preenche com ZERO os dias sem caso (grade completa
# uf × dia no intervalo observado — mesmo cuidado do "mês zero" da série mensal,
# crucial aqui porque a maioria dos dias-UF tem zero e um GLM que nunca "vê" o
# zero enviesa a taxa). Registros sem data de notificação válida ficam de fora.
serie_diaria_sinan <- function(ufs, anos, tipo = "3", prog = NULL) {
  frames <- list(); k <- 0L
  for (a in anos) {
    k <- k + 1L
    if (!is.null(prog)) prog(k / length(anos), sprintf("SINAN %d…", a))
    d <- tryCatch(processar_sinan_ano(a, tipo, ufs_keep = ufs), error = function(e) NULL)
    if (is.data.frame(d) && nrow(d) && "data" %in% names(d))
      frames[[length(frames) + 1L]] <- d[!is.na(d$data), c("uf_res", "data")]
  }
  if (!length(frames)) return(NULL)
  cont <- bind_rows(frames) %>%
    group_by(uf = uf_res, data) %>%
    summarise(count = dplyr::n(), .groups = "drop")
  if (nrow(cont) == 0) return(NULL)
  rng   <- range(cont$data)
  grade <- expand.grid(uf = sort(unique(cont$uf)),
                       data = seq(rng[1], rng[2], by = "day"),
                       stringsAsFactors = FALSE)
  out <- merge(grade, cont, by = c("uf", "data"), all.x = TRUE)
  out$count[is.na(out$count)] <- 0L
  out[order(out$uf, out$data), c("uf", "data", "count")]
}

# ── Extração de data: usa DT_INTER (atendimento) para SIH, DTOBITO para SIM ──
# Isso garante compatibilidade com TabNet "por ano de atendimento".
# Robusto a diferentes formatos:
#   - Date (process_sih já converteu)
#   - "YYYYMMDD" character (raw DBC)
#   - "YYYY-MM-DD" character (ISO)
parse_datasus_dt <- function(col_vals) {
  if (inherits(col_vals, c("Date","POSIXct","POSIXlt")))
    return(as.Date(col_vals))

  # ── Numérico: pode ser days-since-epoch R (1970) ou SAS (1960) ──────────────
  if (is.numeric(col_vals) || is.integer(col_vals)) {
    n <- as.numeric(col_vals)
    n[is.na(n) | n <= 0 | n > 99999999] <- NA_real_
    # Tenta epoch R (1970-01-01) — valores ~15000-22000 para anos 2010-2030
    r  <- suppressWarnings(as.Date(n, origin="1970-01-01"))
    yr <- suppressWarnings(lubridate::year(r))
    if (mean(yr >= 2000 & yr <= 2030, na.rm=TRUE) > 0.5) return(r)
    # Tenta epoch SAS (1960-01-01) — valores ~18000-25500 para anos 2009-2029
    s   <- suppressWarnings(as.Date(n, origin="1960-01-01"))
    yr2 <- suppressWarnings(lubridate::year(s))
    if (mean(yr2 >= 2000 & yr2 <= 2030, na.rm=TRUE) > 0.5) return(s)
    # Último recurso: inteiro YYYYMMDD (ex: 20190523L)
    return(suppressWarnings(as.Date(sprintf("%08.0f", n), "%Y%m%d")))
  }

  x <- trimws(as.character(col_vals))
  # Invalida padrões conhecidos (inclui "0000-00-00" que quebra R 4.x)
  invalido <- is.na(x) | x %in% c("NA","","0","00000000","99999999",
                                   "0000-00-00","9999-99-99") |
              grepl("^0+$", x)
  x[invalido] <- NA_character_

  dt <- rep(as.Date(NA_character_), length(x))

  # ── ISO "YYYY-MM-DD" (10 chars) ─────────────────────────────────────────────
  # Usa lubridate::ymd() que retorna NA por elemento sem lançar erro no vetor
  iso <- !is.na(x) & nchar(x) == 10L & grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
  if (any(iso))
    dt[iso] <- suppressWarnings(as.Date(lubridate::ymd(x[iso])))

  # ── "YYYYMMDD" (8 dígitos) ──────────────────────────────────────────────────
  ymd8 <- !is.na(x) & is.na(dt) & nchar(x) == 8L & grepl("^\\d{8}$", x)
  if (any(ymd8))
    dt[ymd8] <- suppressWarnings(tryCatch(
      as.Date(x[ymd8], format="%Y%m%d"),
      error = function(e) rep(as.Date(NA_character_), sum(ymd8))
    ))

  dt
}

extrair_datas <- function(dat, is_sih, c_dti, c_ano, c_mes, ano_fallback=NA_integer_) {
  if (is_sih && !is.na(c_dti)) {
    dt <- parse_datasus_dt(dat[[c_dti]])
    # Só usa DT_INTER se pelo menos 50% dos registros têm data válida
    if (mean(!is.na(dt)) >= 0.5) {
      dat$ano <- lubridate::year(dt)
      dat$mes <- lubridate::month(dt)
      return(dat)
    }
    # Caso contrário, avisa e cai no fallback competência
    message("[info] DT_INTER com muitos NAs — usando ANO_CMPT/MES_CMPT como fallback")
  }

  if (!is_sih) {
    dt_col <- col1(dat,"DTOBITO","dtobito")
    if (!is.na(dt_col)) {
      dt <- parse_datasus_dt(dat[[dt_col]])
      dat$ano <- lubridate::year(dt)
      dat$mes <- lubridate::month(dt)
      return(dat)
    }
  }

  # Fallback: competência (MES_CMPT / ANO_CMPT)
  dat$ano <- if (!is.na(c_ano)) suppressWarnings(as.integer(as.character(dat[[c_ano]]))) else ano_fallback
  dat$mes <- if (!is.na(c_mes)) suppressWarnings(as.integer(as.character(dat[[c_mes]]))) else NA_integer_
  dat
}

# ── Leitura de RDS local (pasta fornecida pelo usuário) ───────────────────────
# Convenção de nome: sih_rd_{UF}.rds  /  sim_do_{UF}.rds  (case-insensitive)
ler_rds_local <- function(pasta, sistema, uf) {
  prefixo <- if (grepl("SIH", sistema, ignore.case=TRUE)) "sih_rd" else "sim_do"
  # Tenta nomes com maiúsculo e minúsculo
  candidatos <- c(
    file.path(pasta, paste0(prefixo, "_", uf,        ".rds")),
    file.path(pasta, paste0(prefixo, "_", tolower(uf),".rds")),
    file.path(pasta, paste0(toupper(prefixo), "_", uf,".rds"))
  )
  arq <- candidatos[file.exists(candidatos)][1]
  if (is.na(arq)) return(NULL)
  tryCatch(readRDS(arq), error=function(e) NULL)
}

# Baixa e processa 1 (uf, ano) via FTP DATASUS — usado tanto no modo download
# puro quanto como fallback automático quando o modo local (RDS) não tem o
# arquivo daquela UF (ex.: raw/ só tem os 9 estados do Nordeste, mas o
# usuário pediu Brasil inteiro — os 18 estados que faltam caem aqui em vez
# de serem descartados silenciosamente).
processar_uf_ftp <- function(uf, ano, is_sih, cid_pref, ufs) {
  tryCatch({
    if (is_sih) {
      dat <- baixar_sih_rd(uf, ano)
      if (is.null(dat) || nrow(dat)==0) return(NULL)
      c_mun <- col1(dat,"MUNIC_RES","munic_res","CODMUNRES","codmunres")
      c_cid <- col1(dat,"DIAG_PRINC","diag_princ")
      c_dti <- col1(dat,"DT_INTER","dt_inter")
      c_mes <- col1(dat,"MES_CMPT","mes_cmpt")
      c_ano <- col1(dat,"ANO_CMPT","ano_cmpt")
      c_sx  <- col1(dat,"SEXO","sexo")
      c_id  <- col1(dat,"IDADE","idade")
      c_ci  <- col1(dat,"COD_IDADE","cod_idade")
      c_rc  <- col1(dat,"RACA_COR","raca_cor")
    } else {
      dat <- baixar_sim_do(uf, ano)
      if (is.null(dat) || nrow(dat)==0) return(NULL)
      c_mun <- col1(dat,"CODMUNRES","codmunres","MUNIC_RES")
      c_cid <- col1(dat,"CAUSABAS","causabas")
      c_dti <- NA_character_; c_mes <- NA_character_; c_ano <- NA_character_
      c_sx  <- col1(dat,"SEXO","sexo")
      c_id  <- col1(dat,"IDADE","idade")
      c_ci  <- col1(dat,"COD_IDADE","cod_idade")
      c_rc  <- col1(dat,"RACACOR","racacor","RACA_COR","raca_cor")
    }

    if (!is.na(c_cid) && length(cid_pref)>0)
      dat <- dat[cid_ok(dat[[c_cid]], cid_pref), , drop=FALSE]
    if (is.null(dat) || !is.data.frame(dat) || nrow(dat)==0) return(NULL)

    dat$uf_res <- if (!is.na(c_mun)) extrair_uf_ibge(dat[[c_mun]]) else uf
    dat <- extrair_datas(dat, is_sih, c_dti, c_ano, c_mes, ano_fallback=ano)

    dat$sexo  <- if (!is.na(c_sx)) clf_sexo(dat[[c_sx]])  else NA_character_
    dat$faixa <- if (!is.na(c_id)) {
      clf_faixa(dat[[c_id]], if (!is.na(c_ci)) dat[[c_ci]] else NULL)
    } else NA_character_
    # idade_anos: idade exata (não bucketizada em década) — precisa pros
    # cortes etários "tipo ECA" (12/18 anos) do filtro na tela de
    # configuração, que não caem em fronteira de década (ver idade_eca_ok()).
    dat$idade_anos <- if (!is.na(c_id)) {
      idade_em_anos(dat[[c_id]], if (!is.na(c_ci)) dat[[c_ci]] else NULL)
    } else NA_real_
    dat$cor   <- if (!is.na(c_rc)) clf_raca(dat[[c_rc]])  else NA_character_

    dat %>%
      filter(!is.na(mes), !is.na(ano), !is.na(uf_res), uf_res %in% ufs) %>%
      select(uf_res, ano, mes, sexo, faixa, idade_anos, cor)
  }, error=function(e) { message(sprintf("[aviso] %s %d: %s", uf, ano, e$message)); NULL })
}

baixar_dados <- function(sistema, ufs, ano_ini, ano_fim, cid_pref, prog=NULL, pasta=NULL, sinan_tipo="3") {
  # ── SINAN: arquivo nacional por ano (não há .rds/DBC por UF) ─────────────────
  # Baixa uma vez por ano, filtra o tipo de animal e recorta pelas UFs de
  # residência pedidas. Independe de RDS local (usa o cache de DBC).
  if (sistema_tipo(sistema) == "SINAN") {
    anos <- ano_ini:ano_fim
    frames <- list(); n_tot <- length(anos); done <- 0L
    for (ano in anos) {
      done <- done + 1L
      if (!is.null(prog)) prog(done/n_tot, paste("Carregando SINAN", ano, "(nacional; cache em disco)..."))
      sub <- tryCatch(processar_sinan_ano(ano, sinan_tipo, ufs),
                      error=function(e){ message(sprintf("[sinan] %d: %s", ano, e$message)); NULL })
      if (!is.null(sub) && nrow(sub)>0) {
        # any_of (não select fixo): os campos clínicos — gravidade, evolução,
        # tempo até o atendimento, soroterapia — vêm do mesmo arquivo nacional
        # e alimentam a aba de desfechos; anos antigos do SINAN que não tenham
        # alguma dessas colunas simplesmente não a trazem, sem quebrar.
        sub <- sub %>% filter(ano %in% anos, !is.na(mes), !is.na(ano)) %>%
          select(any_of(c("uf_res","ano","mes","sexo","faixa","idade_anos","cor",
                          "gravidade","evolucao","obito_agravo","tempo_atend",
                          "soroterapia")))
        if (nrow(sub)>0) frames[[length(frames)+1]] <- sub
      }
    }
    resultado <- if (length(frames)==0) NULL else bind_rows(frames)
    if (!is.null(resultado)) attr(resultado, "ufs_ftp_fallback") <- character(0)
    return(resultado)
  }

  is_sih  <- grepl("SIH", sistema, ignore.case=TRUE)
  usa_rds <- !is.null(pasta) && nchar(trimws(pasta))>0 && dir.exists(trimws(pasta))
  frames  <- list()
  anos    <- ano_ini:ano_fim

  ufs_faltantes <- character(0)  # UFs sem RDS local -> caem no fallback FTP abaixo

  if (usa_rds) {
    # ── Modo local: ler RDS por UF, filtrar por ano ──────────────────────────
    n_tot <- length(ufs); done <- 0L
    for (uf in ufs) {
      done <- done + 1L
      if (!is.null(prog)) prog(done/n_tot, paste("Lendo", uf, "do disco..."))
      tryCatch({
        dat <- ler_rds_local(trimws(pasta), sistema, uf)
        if (is.null(dat) || nrow(dat)==0) {
          message(sprintf("[local] %s: arquivo não encontrado ou vazio — vai baixar do FTP", uf))
          ufs_faltantes <- c(ufs_faltantes, uf)
          next
        }

        # Diagnóstico: mostra colunas disponíveis na primeira UF
        if (done==1L)
          message(sprintf("[local] colunas RDS (%s): %s", uf,
                          paste(names(dat)[seq_len(min(20,ncol(dat)))], collapse=", ")))

        if (is_sih) {
          c_mun <- col1(dat,"MUNIC_RES","munic_res","CODMUNRES","codmunres")
          c_cid <- col1(dat,"DIAG_PRINC","diag_princ")
          c_dti <- col1(dat,"DT_INTER","dt_inter")
          c_mes <- col1(dat,"MES_CMPT","mes_cmpt")
          c_ano <- col1(dat,"ANO_CMPT","ano_cmpt")
          c_sx  <- col1(dat,"SEXO","sexo")
          c_id  <- col1(dat,"IDADE","idade")
          c_ci  <- col1(dat,"COD_IDADE","cod_idade")
          c_rc  <- col1(dat,"RACA_COR","raca_cor")
        } else {
          c_mun <- col1(dat,"CODMUNRES","codmunres","MUNIC_RES")
          c_cid <- col1(dat,"CAUSABAS","causabas")
          c_dti <- NA_character_
          c_mes <- NA_character_
          c_ano <- NA_character_
          c_sx  <- col1(dat,"SEXO","sexo")
          c_id  <- col1(dat,"IDADE","idade")
          c_ci  <- col1(dat,"COD_IDADE","cod_idade")
          c_rc  <- col1(dat,"RACACOR","racacor","RACA_COR","raca_cor")
        }

        if (is_sih && is.na(c_ci))
          message(sprintf(
            "[aviso] %s: RDS sem coluna COD_IDADE — faixa etaria sera aproximada (reconstrua o RDS com download_sih.R atualizado para precisao)",
            uf))

        message(sprintf("[local] %s: %d linhas | c_cid=%s c_dti=%s c_ano=%s c_mun=%s",
                        uf, nrow(dat), c_cid, c_dti, c_ano, c_mun))

        # Filtro CID
        n_antes <- nrow(dat)
        if (!is.na(c_cid) && length(cid_pref)>0)
          dat <- dat[cid_ok(dat[[c_cid]], cid_pref), , drop=FALSE]
        message(sprintf("[local] %s: após filtro CID: %d/%d linhas", uf, nrow(dat), n_antes))
        if (is.null(dat) || nrow(dat)==0) next

        # UF de residência
        dat$uf_res <- if (!is.na(c_mun)) extrair_uf_ibge(dat[[c_mun]]) else uf

        # ── Ano/mês por linha: DT_INTER quando cai no range, ANO_CMPT nos demais ──
        # Estratégia: não descarta toda a UF — usa DT_INTER registro a registro.
        # Se DT_INTER estiver corrompido (ex: todos = 2026 por bug do RDS),
        # nenhum registro passará na checagem de range e todos usarão ANO_CMPT.
        ano_cmpt_vec <- if (!is.na(c_ano)) suppressWarnings(as.integer(as.character(dat[[c_ano]]))) else rep(NA_integer_, nrow(dat))
        mes_cmpt_vec <- if (!is.na(c_mes)) suppressWarnings(as.integer(as.character(dat[[c_mes]]))) else rep(NA_integer_, nrow(dat))

        if (is_sih && !is.na(c_dti)) {
          # Diagnóstico do formato de DT_INTER (ajuda a identificar corrupção)
          sample_dti <- tryCatch(head(dat[[c_dti]], 3), error=function(e) NA)
          message(sprintf("[local] %s: DT_INTER class=<%s> | amostra: %s",
                          uf, paste(class(dat[[c_dti]]), collapse=","),
                          paste(as.character(sample_dti), collapse=", ")))

          dt_vals <- tryCatch(parse_datasus_dt(dat[[c_dti]]),
                              error=function(e) rep(as.Date(NA_character_), nrow(dat)))
          dt_ano  <- suppressWarnings(lubridate::year(dt_vals))
          dt_mes  <- suppressWarnings(lubridate::month(dt_vals))

          # Três categorias:
          # • DT_INTER válida E no range  → usa DT_INTER (como TabNet)
          # • DT_INTER válida E fora do range → exclui (pertence a outro período)
          # • DT_INTER inválida/NA         → usa ANO_CMPT como melhor estimativa
          dti_plaus    <- !is.na(dt_ano) & dt_ano >= 1990L & dt_ano <= 2030L
          dti_in_range <- dti_plaus & dt_ano %in% anos
          dti_out_range<- dti_plaus & !(dt_ano %in% anos)
          n_in  <- sum(dti_in_range,  na.rm=TRUE)
          n_out <- sum(dti_out_range, na.rm=TRUE)
          n_cpt <- nrow(dat) - n_in - n_out
          message(sprintf(
            "[local] %s: DT_INTER no range=%d | fora do range(excl.)=%d | ANO_CMPT=%d",
            uf, n_in, n_out, n_cpt))

          dat$ano <- as.integer(ifelse(dti_in_range,  dt_ano,
                               ifelse(dti_out_range, NA_integer_, ano_cmpt_vec)))
          dat$mes <- as.integer(ifelse(dti_in_range,  dt_mes,
                               ifelse(dti_out_range, NA_integer_, mes_cmpt_vec)))

        } else if (!is_sih) {
          # SIM: usa DTOBITO
          dat <- tryCatch(
            extrair_datas(dat, is_sih, c_dti, c_ano, c_mes),
            error = function(e) {
              message(sprintf("[local] %s: erro DTOBITO (%s) — fallback ANO_CMPT", uf, e$message))
              dat$ano <- ano_cmpt_vec; dat$mes <- mes_cmpt_vec; dat
            }
          )
        } else {
          dat$ano <- ano_cmpt_vec
          dat$mes <- mes_cmpt_vec
        }

        message(sprintf("[local] %s: anos disponíveis: %s",
                        uf, paste(sort(unique(dat$ano[!is.na(dat$ano)])), collapse=",")))

        dat$sexo  <- if (!is.na(c_sx)) clf_sexo(dat[[c_sx]])  else NA_character_
        dat$faixa <- if (!is.na(c_id)) {
          clf_faixa(dat[[c_id]], if (!is.na(c_ci)) dat[[c_ci]] else NULL)
        } else NA_character_
        # idade_anos: idade exata (não bucketizada em década) — precisa pros
        # cortes etários "tipo ECA" (12/18 anos) do filtro na tela de
        # configuração, que não caem em fronteira de década (ver idade_eca_ok()).
        dat$idade_anos <- if (!is.na(c_id)) {
          idade_em_anos(dat[[c_id]], if (!is.na(c_ci)) dat[[c_ci]] else NULL)
        } else NA_real_
        dat$cor   <- if (!is.na(c_rc)) clf_raca(dat[[c_rc]])  else NA_character_

        sub <- dat %>%
          filter(ano %in% anos, !is.na(mes), !is.na(ano),
                 !is.na(uf_res), uf_res %in% ufs) %>%
          select(uf_res, ano, mes, sexo, faixa, idade_anos, cor)

        if (nrow(sub)>0) frames[[length(frames)+1]] <- sub
      }, error=function(e) message(sprintf("[aviso local] %s: %s", uf, e$message)))
    }

    # ── Fallback FTP para UFs que não tinham RDS na pasta local ──────────────
    if (length(ufs_faltantes) > 0) {
      message(sprintf("[fallback] sem RDS local, baixando do FTP: %s",
                      paste(ufs_faltantes, collapse=", ")))
      n_tot <- length(ufs_faltantes) * length(anos); done <- 0L
      for (uf in ufs_faltantes) {
        for (ano in anos) {
          done <- done + 1L
          if (!is.null(prog))
            prog(done/n_tot, paste("Baixando", uf, ano, "(sem RDS local)..."))
          sub <- processar_uf_ftp(uf, ano, is_sih, cid_pref, ufs)
          if (!is.null(sub) && nrow(sub)>0) frames[[length(frames)+1]] <- sub
        }
      }
    }

  } else {
    # ── Modo download: FTP DATASUS ────────────────────────────────────────────
    n_tot <- length(ufs) * length(anos); done <- 0L

    for (uf in ufs) {
      for (ano in anos) {
        done <- done + 1L
        if (!is.null(prog)) prog(done/n_tot, paste("Baixando", uf, ano, "..."))
        sub <- processar_uf_ftp(uf, ano, is_sih, cid_pref, ufs)
        if (!is.null(sub) && nrow(sub)>0) frames[[length(frames)+1]] <- sub
      }
    }  # fim bloco download

  }  # fim if/else usa_rds

  resultado <- if (length(frames)==0) NULL else bind_rows(frames)
  if (!is.null(resultado)) attr(resultado, "ufs_ftp_fallback") <- ufs_faltantes
  resultado
}
