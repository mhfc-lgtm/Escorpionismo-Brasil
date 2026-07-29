# Código de análise — Escorpionismo no Brasil, 2016–2025

Repositório de **transparência e reprodutibilidade** do artigo *"Escorpionismo no
Brasil, 2016–2025: análise temporal, espacial, sociodemográfica e clínica"*
(estudo ecológico de série temporal e espacial, dados do SINAN/DATASUS e IBGE).

Contém as funções em R efetivamente utilizadas na análise. O código foi
**extraído verbatim** do módulo de lógica pura (`core_datasus.R`) do aplicativo
**EpiSUS** (Shiny) desenvolvido pelos autores, sem a camada de interface — de modo
que cada resultado do artigo possa ser rastreado até a função que o produziu.

- **Software:** R 4.6.0
- **Fontes de dados:** SINAN — Sistema de Informação de Agravos de Notificação
  (Animais Peçonhentos, arquivos nacionais anuais `ANIMBR<AA>.dbc` do DATASUS,
  filtro `TP_ACIDENT = 3` = escorpião); estimativas populacionais do IBGE;
  malhas territoriais via `geobr`.
- **Período:** notificações de 2016 a 2025. Dados obtidos em julho de 2026.

> **Nota:** este código depende dos microdados públicos do DATASUS e das malhas
> do `geobr`, baixados em tempo de execução (com cache local) — eles não são
> redistribuídos aqui. As funções documentam integralmente **como** os dados
> foram coletados, tratados e analisados.

---

## Estrutura

| Arquivo | Conteúdo |
|---|---|
| [`01_constantes_e_populacao.R`](01_constantes_e_populacao.R) | Constantes (UFs, regiões, faixas etárias) e leitura das estimativas populacionais do IBGE (denominador das taxas). |
| [`02_coleta_e_classificacao.R`](02_coleta_e_classificacao.R) | Coleta dos microdados do SINAN e classificação das variáveis (idade, sexo, raça/cor, gravidade, evolução). |
| [`03_series_temporais.R`](03_series_temporais.R) | Montagem das séries mensais de contagens/taxas por grupo. |
| [`04_regressao_temporal_e_proporcoes.R`](04_regressao_temporal_e_proporcoes.R) | Núcleo estatístico: Poisson HAC (IRR), sazonalidade, logística (OR), IC de Wilson e tabela Brasil→regiões→estados. |
| [`05_analise_espacial.R`](05_analise_espacial.R) | Bayes empírico, pesos Queen, Moran global/local (LISA), Getis-Ord Gi*, densidade de Kernel e scan espacial. |
| [`06_graficos_e_mapas.R`](06_graficos_e_mapas.R) | Geração das figuras e mapas (ggplot2, leaflet, sf). |
| [`07_selecao_estados.R`](07_selecao_estados.R) | Utilitários de escopo geográfico (região ↔ UFs). |

---

## Mapa: método do artigo → função

### Coleta e definição de caso (`02`)
- **Filtro do agravo (escorpião, `TP_ACIDENT = 3`)** e leitura dos `.dbc` nacionais:
  `baixar_sinan_anim()`, `.sinan_nacional_processado()`, `processar_sinan_ano()`,
  `baixar_dados()`.
- **Gravidade (`TRA_CLASSI`)**: `clf_gravidade_sinan()` — códigos 9/branco → `NA`
  (excluídos do numerador **e** do denominador).
- **Evolução / óbito pelo agravo (`EVOLUCAO`)**: `clf_evolucao_sinan()`,
  `obito_agravo_sinan()`.
- **Variáveis sociodemográficas**: `idade_sinan_anos()`, `clf_faixa()`,
  `clf_sexo()`, `clf_raca_sinan()`.

### Taxas de incidência e tendência temporal (`03`, `04`)
- **Séries mensais** (contagem + população): `serie_total()`, `serie_grupo()`.
- **Regressão de Poisson com EP robusto HAC** (Newey-West, kernel de Bartlett,
  `lag = 12`) e `offset = log(população)`: `hac_vcov()`, `fit_poisson()`.
- **IRR e IC 95%** (crescimento anual médio — 7,79%/ano no Brasil):
  `irr_ci()`, `extrair_tend()`.
- **Tabela 1 (Brasil → regiões → estados)**, taxa média e crescimento anual:
  `resumo_brasil_regioes()`, `.fit_grupo_taxa()`, `flextable_brasil()`,
  `salvar_tabela_docx()`.

### Perfil sociodemográfico — Figura 1 (`04`)
- **IRR por sexo / faixa etária / raça-cor** (referência 30–39 anos):
  `demog_hac()`.

### Sazonalidade (`04`)
- **IRR por mês** (referência janeiro): `extrair_saz()` (a partir do mesmo
  ajuste de Poisson HAC).

### Gravidade e evolução — proporções e chance (`04`) — Figura 4
- **IC de proporção de Wilson**: `prop_wilson()`.
- **Regressão logística** da série mensal (k eventos em n casos), ajustada por
  mês e tendência anual, mesma matriz HAC → **OR** por faixa etária
  (0–4 anos: OR 12,31): `serie_binaria()`, `or_hac()`.
- **Tendência da proporção de casos graves** (queda 3,04%/ano):
  `fit_prop()`, `extrair_tend_prop()`, `tab_prop_ano()`.
- **Completude dos campos clínicos**: `tab_completude()`.

### Análise espacial (`05`) — Figuras 2 e 3
- **Suavização Bayesiana empírica global** (`spdep::EBest`): `suavizar_taxa_eb()`.
- **Matriz de pesos — contiguidade Queen** padronizada por linha, `zero.policy`
  (`poly2nb` + `nb2listw`): `construir_pesos_espaciais()`.
- **Índice de Moran global** (999 permutações de Monte Carlo; `moran.mc` +
  `EBImoran.mc` = 0,658; p < 0,001): `moran_global()`.
- **Moran local / LISA** e **Getis-Ord Gi\*** com **correção FDR
  (Benjamini-Hochberg)**, α = 0,05: `moran_local()`, `getis_gi()`.
- **Densidade de Kernel** (largura de banda de **Sheather-Jones**), superfícies
  de casos e de risco relativo: `densidade_kernel()`.
- **Estatística de varredura espacial** (scan de Kulldorff): `scan_espacial()`.
- **Orquestrador**: `analise_espacial()`.

### Figuras e mapas (`06`)
`gg_tend()`, `gg_saz()` (tendência/sazonalidade); `gg_forest()`,
`gg_prop_ano()` (demografia/proporções); `mapa_leaflet_estados()`,
`mapa_estatico_clusters()` (aglomerados LISA/Gi\* — Figura 2);
`gg_kernel()`, `gg_kernel_duplo()` (superfícies de Kernel — Figura 3);
`gg_moran_scatter()` (diagrama de espalhamento de Moran).

---

## Pacotes de R

Análise temporal e coleta: **microdatasus**, **read.dbc**, **sandwich**,
**lmtest**, **lubridate**, **dplyr**, **stats**.
Análise espacial: **spdep**, **sf**, **geobr**, **classInt**, **KernSmooth**.
Figuras, mapas e tabelas: **ggplot2**, **cowplot**, **grid**, **leaflet**,
**htmltools**, **grDevices**, **flextable**, **officer**, **jsonlite**.

```r
install.packages(c(
  "microdatasus", "read.dbc", "sandwich", "lmtest", "lubridate", "dplyr",
  "spdep", "sf", "geobr", "classInt", "KernSmooth",
  "ggplot2", "cowplot", "leaflet", "htmltools", "flextable", "officer", "jsonlite"
))
```

---

## Escopo do código

As funções aqui reunidas são exatamente as que sustentam os resultados relatados
no artigo. O aplicativo EpiSUS de origem inclui ainda módulos **não utilizados
neste estudo** (modelagem climática DLNM da sazonalidade e análise espacial
bivariada com covariáveis socioeconômicas), que foram deliberadamente omitidos
deste repositório para manter a correspondência direta com os métodos descritos.

## Licença

Distribuído sob a **Licença MIT** — ver [`LICENSE`](LICENSE). Uso, adaptação e
redistribuição são livres, mediante citação dos autores. *(Edite a linha
`Copyright (c) 2026 <NOME DOS AUTORES DO ARTIGO>` do arquivo `LICENSE` com o nome
completo dos autores antes de publicar a versão final.)*
