# Análise de diversidade beta e complementaridade entre paisagens
# A base inferencial considera apenas as armadilhas de suspensão

# Pacotes utilizados
options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  timeout = 600
)

pacotes <- c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "janitor",
  "lubridate",
  "tibble",
  "purrr",
  "betapart"
)

pacotes_ausentes <- pacotes[
  !sapply(pacotes, requireNamespace, quietly = TRUE)
]

if (length(pacotes_ausentes) > 0) {
  install.packages(
    pacotes_ausentes,
    dependencies = TRUE
  )
}

invisible(
  lapply(
    pacotes,
    library,
    character.only = TRUE
  )
)


# Localização da planilha
script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub(
  "^--file=",
  "",
  script_args[grep("^--file=", script_args)]
)

script_directory <- if (length(script_file) > 0) {
  dirname(
    normalizePath(
      script_file[1],
      winslash = "/",
      mustWork = TRUE
    )
  )
} else {
  normalizePath(
    getwd(),
    winslash = "/",
    mustWork = TRUE
  )
}

analysis_directory <- normalizePath(
  file.path(
    script_directory,
    ".."
  ),
  winslash = "/",
  mustWork = TRUE
)

output_directory <- file.path(
  script_directory,
  "output"
)

dir.create(
  output_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

arquivo_csv <- file.path(
  analysis_directory,
  "data",
  "dados.csv"
)

if (!file.exists(arquivo_csv)) {
  stop(
    "\nO arquivo não foi encontrado neste caminho:\n",
    arquivo_csv,
    "\n\nConfira o nome e a localização da planilha."
  )
}

message("Arquivo encontrado: ", arquivo_csv)


# Padroniza os nomes das paisagens nos resultados e gráficos
salvar_grafico <- function(grafico, nome_arquivo_base, largura = 9, altura = 6) {
  arquivo_png <- file.path(
    output_directory,
    paste0(nome_arquivo_base, ".png")
  )

  arquivo_pdf <- file.path(
    output_directory,
    paste0(nome_arquivo_base, ".pdf")
  )

  ggplot2::ggsave(
    filename = arquivo_png,
    plot = grafico,
    width = largura,
    height = altura,
    units = "in",
    dpi = 600,
    bg = "white"
  )

  ggplot2::ggsave(
    filename = arquivo_pdf,
    plot = grafico,
    width = largura,
    height = altura,
    units = "in",
    bg = "white"
  )
}

salvar_csv <- function(dados, nome_arquivo) {
  readr::write_csv(
    dados,
    file.path(
      output_directory,
      nome_arquivo
    ),
    na = ""
  )
}

nome_paisagem <- function(x) {
  dplyr::recode(
    x,
    "cerrado" = "Cerrado",
    "floodplain" = "Floodplain",
    "pantanal" = "Pantanal",
    .default = x
  )
}


# Converte os objetos de distância do betapart em tabelas
distancia_para_tabela <- function(d, nome_valor) {
  
  matriz <- as.matrix(d)
  posicoes <- upper.tri(matriz)
  
  resultado <- tibble(
    paisagem_1 = rownames(matriz)[row(matriz)[posicoes]],
    paisagem_2 = colnames(matriz)[col(matriz)[posicoes]],
    valor = matriz[posicoes]
  )
  
  names(resultado)[3] <- nome_valor
  
  resultado
}


# Recupera um componente específico do resultado da diversidade beta
extrair_beta <- function(resultado, padrao) {
  
  posicao <- grep(
    padrao,
    names(resultado),
    ignore.case = TRUE
  )
  
  if (length(posicao) == 0) {
    return(NA_real_)
  }
  
  as.numeric(resultado[posicao[1]])
}


# Renomeia uma coluna quando ela aparece com outro nome
renomear_coluna <- function(
    dados,
    nome_correto,
    nomes_alternativos
) {
  
  if (nome_correto %in% names(dados)) {
    return(dados)
  }
  
  encontrado <- nomes_alternativos[
    nomes_alternativos %in% names(dados)
  ]
  
  if (length(encontrado) > 0) {
    names(dados)[
      names(dados) == encontrado[1]
    ] <- nome_correto
  }
  
  dados
}


# Identifica o separador utilizado na planilha
primeira_linha <- readLines(
  arquivo_csv,
  n = 1,
  encoding = "UTF-8",
  warn = FALSE
)

separadores <- c(
  ";",
  ",",
  "\t"
)

quantidade_separadores <- vapply(
  separadores,
  function(separador) {
    
    ocorrencias <- gregexpr(
      separador,
      primeira_linha,
      fixed = TRUE
    )[[1]]
    
    if (
      length(ocorrencias) == 1 &&
      ocorrencias[1] == -1
    ) {
      return(0L)
    }
    
    length(ocorrencias)
  },
  integer(1)
)

if (max(quantidade_separadores) == 0) {
  stop(
    "Não foi possível identificar o separador da planilha."
  )
}

separador_usado <- separadores[
  which.max(quantidade_separadores)
]

nome_separador <- if (separador_usado == "\t") {
  "tabulação"
} else {
  separador_usado
}

message(
  "Separador identificado: ",
  nome_separador
)


# Leitura da planilha
dados_brutos <- read.csv(
  file = arquivo_csv,
  sep = separador_usado,
  fileEncoding = "UTF-8-BOM",
  stringsAsFactors = FALSE,
  check.names = FALSE
) |>
  janitor::clean_names()

message(
  "Número de registros: ",
  nrow(dados_brutos)
)

message(
  "Número de colunas: ",
  ncol(dados_brutos)
)

message(
  "Colunas encontradas: ",
  paste(
    names(dados_brutos),
    collapse = ", "
  )
)


# Aceita alguns nomes alternativos para as colunas principais
dados_brutos <- dados_brutos |>
  
  renomear_coluna(
    nome_correto = "species",
    nomes_alternativos = c(
      "specie",
      "especie",
      "especies",
      "nome_da_especie",
      "nome_especie"
    )
  ) |>
  
  renomear_coluna(
    nome_correto = "date",
    nomes_alternativos = c(
      "data",
      "data_de_coleta",
      "data_coleta",
      "collection_date"
    )
  ) |>
  
  renomear_coluna(
    nome_correto = "trap",
    nomes_alternativos = c(
      "armadilha",
      "tipo_de_armadilha",
      "tipo_armadilha",
      "trap_type"
    )
  ) |>
  
  renomear_coluna(
    nome_correto = "lati_s",
    nomes_alternativos = c(
      "latitude",
      "latitude_s",
      "lat",
      "lati",
      "lat_s"
    )
  ) |>
  
  renomear_coluna(
    nome_correto = "long_w",
    nomes_alternativos = c(
      "longitude",
      "longitude_w",
      "long",
      "lon",
      "lng",
      "long_o",
      "longitude_o"
    )
  ) |>
  
  renomear_coluna(
    nome_correto = "landscape",
    nomes_alternativos = c(
      "paisagem",
      "ambiente",
      "habitat",
      "landscape_type"
    )
  )


colunas_necessarias <- c(
  "species",
  "date",
  "trap",
  "lati_s",
  "long_w",
  "landscape"
)

colunas_ausentes <- setdiff(
  colunas_necessarias,
  names(dados_brutos)
)

if (length(colunas_ausentes) > 0) {
  stop(
    "\nEstas colunas não foram encontradas:\n",
    paste(
      colunas_ausentes,
      collapse = ", "
    ),
    "\n\nColunas existentes na planilha:\n",
    paste(
      names(dados_brutos),
      collapse = ", "
    )
  )
}

message(
  "Todas as colunas necessárias foram encontradas."
)


# Limpeza dos valores e criação de um identificador para cada ponto
dados <- dados_brutos |>
  mutate(
    across(
      where(is.character),
      ~ trimws(.x)
    ),
    trap = tolower(trap),
    landscape = tolower(landscape),
    date = suppressWarnings(
      lubridate::dmy(date)
    ),
    point_id = paste(
      lati_s,
      long_w,
      sep = " | "
    )
  )


datas_invalidas <- sum(
  is.na(dados$date)
)

if (datas_invalidas > 0) {
  warning(
    datas_invalidas,
    " registros apresentam datas inválidas ou não reconhecidas."
  )
}


# Seleção dos registros das armadilhas de suspensão
suspensao <- dados |>
  filter(
    trap == "suspension",
    !is.na(species),
    species != "",
    !is.na(date),
    !is.na(lati_s),
    lati_s != "",
    !is.na(long_w),
    long_w != "",
    !is.na(landscape),
    landscape != ""
  )

if (nrow(suspensao) == 0) {
  stop(
    "Nenhum registro válido de armadilha suspension foi encontrado."
  )
}

message(
  "Registros de suspension antes da deduplicação: ",
  nrow(suspensao)
)


# Mantém uma ocorrência por espécie, data e ponto
suspensao_sem_repeticao <- suspensao |>
  distinct(
    species,
    date,
    point_id,
    .keep_all = TRUE
  )

message(
  "Registros de suspension após a deduplicação: ",
  nrow(suspensao_sem_repeticao)
)


ordem_paisagens <- c(
  "cerrado",
  "floodplain",
  "pantanal"
)

paisagens_inesperadas <- setdiff(
  unique(suspensao_sem_repeticao$landscape),
  ordem_paisagens
)

if (length(paisagens_inesperadas) > 0) {
  warning(
    "Foram encontradas paisagens diferentes das esperadas: ",
    paste(
      paisagens_inesperadas,
      collapse = ", "
    )
  )
}


# Informações dos pontos amostrados
informacoes_pontos <- suspensao_sem_repeticao |>
  distinct(
    point_id,
    landscape
  ) |>
  mutate(
    landscape = factor(
      landscape,
      levels = ordem_paisagens
    )
  ) |>
  arrange(
    landscape,
    point_id
  )


# Matriz de presença e ausência por ponto
tabela_ponto_especie <- suspensao_sem_repeticao |>
  distinct(
    point_id,
    landscape,
    species
  ) |>
  mutate(
    presence = 1L
  ) |>
  select(
    point_id,
    species,
    presence
  ) |>
  pivot_wider(
    names_from = species,
    values_from = presence,
    values_fill = 0
  )

tabela_ponto_especie <- informacoes_pontos |>
  left_join(
    tabela_ponto_especie,
    by = "point_id"
  )

matriz_ponto_especie <- tabela_ponto_especie |>
  select(
    -landscape
  ) |>
  as.data.frame()

rownames(matriz_ponto_especie) <-
  matriz_ponto_especie$point_id

matriz_ponto_especie$point_id <- NULL

matriz_ponto_especie <- as.matrix(
  matriz_ponto_especie
)

matriz_ponto_especie <- matriz_ponto_especie[
  rowSums(matriz_ponto_especie) > 0,
  ,
  drop = FALSE
]

matriz_ponto_especie <- matriz_ponto_especie[
  ,
  colSums(matriz_ponto_especie) > 0,
  drop = FALSE
]


# Matriz de presença e ausência por paisagem
tabela_paisagem_especie <- suspensao_sem_repeticao |>
  distinct(
    landscape,
    species
  ) |>
  mutate(
    presence = 1L
  ) |>
  pivot_wider(
    names_from = species,
    values_from = presence,
    values_fill = 0
  ) |>
  mutate(
    landscape = factor(
      landscape,
      levels = ordem_paisagens
    )
  ) |>
  arrange(
    landscape
  )

matriz_paisagem_especie <- tabela_paisagem_especie |>
  as.data.frame()

rownames(matriz_paisagem_especie) <-
  as.character(
    matriz_paisagem_especie$landscape
  )

matriz_paisagem_especie$landscape <- NULL

matriz_paisagem_especie <- as.matrix(
  matriz_paisagem_especie
)

matriz_paisagem_especie <- matriz_paisagem_especie[
  rowSums(matriz_paisagem_especie) > 0,
  ,
  drop = FALSE
]

matriz_paisagem_especie <- matriz_paisagem_especie[
  ,
  colSums(matriz_paisagem_especie) > 0,
  drop = FALSE
]

if (nrow(matriz_paisagem_especie) < 2) {
  stop(
    "São necessárias pelo menos duas paisagens com espécies."
  )
}


# Valores usados no resumo final
numero_registros_brutos <- nrow(
  dados_brutos
)

numero_registros_suspensao <- nrow(
  suspensao
)

numero_registros_deduplicados <- nrow(
  suspensao_sem_repeticao
)

numero_pontos <- nrow(
  matriz_ponto_especie
)

numero_especies <- ncol(
  matriz_ponto_especie
)


pontos_por_paisagem <- informacoes_pontos |>
  filter(
    !is.na(landscape)
  ) |>
  count(
    landscape,
    name = "n_points"
  ) |>
  mutate(
    landscape = nome_paisagem(
      as.character(landscape)
    )
  )


riqueza_por_paisagem <- suspensao_sem_repeticao |>
  distinct(
    landscape,
    species
  ) |>
  count(
    landscape,
    name = "richness"
  ) |>
  mutate(
    landscape = nome_paisagem(
      landscape
    )
  )


# Diversidade beta considerando as três paisagens
beta_multisite <- betapart::beta.multi(
  matriz_paisagem_especie,
  index.family = "jaccard"
)

beta_total <- extrair_beta(
  beta_multisite,
  "JAC"
)

beta_turnover <- extrair_beta(
  beta_multisite,
  "JTU"
)

beta_nestedness <- extrair_beta(
  beta_multisite,
  "JNE"
)

tabela_beta_multisite <- tibble(
  component = c(
    "Total beta",
    "Turnover",
    "Nestedness"
  ),
  value = c(
    beta_total,
    beta_turnover,
    beta_nestedness
  )
)


# Comparações de diversidade beta entre pares de paisagens
beta_pares <- betapart::beta.pair(
  matriz_paisagem_especie,
  index.family = "jaccard"
)

tabela_beta_total <- distancia_para_tabela(
  beta_pares$beta.jac,
  "total_beta"
)

tabela_turnover <- distancia_para_tabela(
  beta_pares$beta.jtu,
  "turnover"
)

tabela_nestedness <- distancia_para_tabela(
  beta_pares$beta.jne,
  "nestedness"
)

tabela_beta_pares <- tabela_beta_total |>
  left_join(
    tabela_turnover,
    by = c(
      "paisagem_1",
      "paisagem_2"
    )
  ) |>
  left_join(
    tabela_nestedness,
    by = c(
      "paisagem_1",
      "paisagem_2"
    )
  ) |>
  mutate(
    paisagem_1 = nome_paisagem(
      paisagem_1
    ),
    paisagem_2 = nome_paisagem(
      paisagem_2
    ),
    comparison = paste(
      paisagem_1,
      "vs",
      paisagem_2
    )
  ) |>
  select(
    comparison,
    paisagem_1,
    paisagem_2,
    total_beta,
    turnover,
    nestedness
  )


# Conjuntos de espécies encontrados em cada paisagem
especies_por_paisagem <- suspensao_sem_repeticao |>
  distinct(
    landscape,
    species
  )

conjuntos_especies <- split(
  especies_por_paisagem$species,
  especies_por_paisagem$landscape
)

paisagens <- names(
  conjuntos_especies
)

riqueza_total <- length(
  unique(
    unlist(conjuntos_especies)
  )
)


frequencia_especies <- especies_por_paisagem |>
  count(
    species,
    name = "n_landscapes"
  ) |>
  arrange(
    desc(n_landscapes),
    species
  )


tabela_especies_exclusivas <- especies_por_paisagem |>
  left_join(
    frequencia_especies,
    by = "species"
  ) |>
  group_by(
    landscape
  ) |>
  summarise(
    richness = n(),
    unique_species = sum(
      n_landscapes == 1
    ),
    species_list = paste(
      sort(
        unique(species)
      ),
      collapse = "; "
    ),
    unique_species_list = paste(
      sort(
        unique(
          species[
            n_landscapes == 1
          ]
        )
      ),
      collapse = "; "
    ),
    .groups = "drop"
  ) |>
  mutate(
    gamma_total = riqueza_total,
    retention_pct_of_gamma = round(
      100 * richness / gamma_total,
      2
    ),
    unique_pct_of_gamma = round(
      100 * unique_species / gamma_total,
      2
    ),
    landscape = nome_paisagem(
      landscape
    )
  ) |>
  arrange(
    match(
      landscape,
      c(
        "Cerrado",
        "Floodplain",
        "Pantanal"
      )
    )
  )


# Combinações possíveis entre uma, duas ou três paisagens
lista_combinacoes <- unlist(
  lapply(
    1:length(paisagens),
    function(k) {
      combn(
        paisagens,
        k,
        simplify = FALSE
      )
    }
  ),
  recursive = FALSE
)

tabela_combinacoes <- purrr::map_dfr(
  lista_combinacoes,
  function(combinacao) {
    
    especies <- sort(
      unique(
        unlist(
          conjuntos_especies[combinacao]
        )
      )
    )
    
    tibble(
      n_landscapes = length(
        combinacao
      ),
      combination = paste(
        nome_paisagem(combinacao),
        collapse = " + "
      ),
      richness = length(
        especies
      ),
      retention_pct_of_gamma = round(
        100 * length(especies) / riqueza_total,
        2
      ),
      species_list = paste(
        especies,
        collapse = "; "
      )
    )
  }
)

maior_riqueza_individual <- tabela_combinacoes |>
  filter(
    n_landscapes == 1
  ) |>
  summarise(
    maior_riqueza = max(
      richness
    )
  ) |>
  pull(
    maior_riqueza
  )

tabela_combinacoes <- tabela_combinacoes |>
  mutate(
    additional_vs_best_single =
      richness - maior_riqueza_individual
  ) |>
  arrange(
    n_landscapes,
    richness
  )


# Ganho de espécies ao acrescentar uma paisagem
lista_subconjuntos <- c(
  list(character(0)),
  unlist(
    lapply(
      1:(length(paisagens) - 1),
      function(k) {
        combn(
          paisagens,
          k,
          simplify = FALSE
        )
      }
    ),
    recursive = FALSE
  )
)

tabela_ganho_marginal <- purrr::map_dfr(
  lista_subconjuntos,
  function(subconjunto_atual) {
    
    paisagens_restantes <- setdiff(
      paisagens,
      subconjunto_atual
    )
    
    especies_antes <- if (
      length(subconjunto_atual) == 0
    ) {
      character(0)
    } else {
      unique(
        unlist(
          conjuntos_especies[subconjunto_atual]
        )
      )
    }
    
    purrr::map_dfr(
      paisagens_restantes,
      function(nova_paisagem) {
        
        especies_depois <- unique(
          c(
            especies_antes,
            conjuntos_especies[[nova_paisagem]]
          )
        )
        
        tibble(
          current_network = if (
            length(subconjunto_atual) == 0
          ) {
            "Nenhuma paisagem"
          } else {
            paste(
              nome_paisagem(subconjunto_atual),
              collapse = " + "
            )
          },
          added_landscape = nome_paisagem(
            nova_paisagem
          ),
          richness_before = length(
            especies_antes
          ),
          richness_after = length(
            especies_depois
          ),
          gain = length(
            especies_depois
          ) - length(
            especies_antes
          )
        )
      }
    )
  }
) |>
  arrange(
    richness_before,
    desc(gain),
    added_landscape
  )


# Sobreposição de espécies entre cada par de paisagens
pares_paisagens <- combn(
  paisagens,
  2,
  simplify = FALSE
)

tabela_sobreposicao <- purrr::map_dfr(
  pares_paisagens,
  function(par) {
    
    especies_1 <- sort(
      conjuntos_especies[[par[1]]]
    )
    
    especies_2 <- sort(
      conjuntos_especies[[par[2]]]
    )
    
    compartilhadas <- intersect(
      especies_1,
      especies_2
    )
    
    exclusivas_1 <- setdiff(
      especies_1,
      especies_2
    )
    
    exclusivas_2 <- setdiff(
      especies_2,
      especies_1
    )
    
    total_uniao <- length(
      union(
        especies_1,
        especies_2
      )
    )
    
    tibble(
      landscape_1 = nome_paisagem(
        par[1]
      ),
      landscape_2 = nome_paisagem(
        par[2]
      ),
      shared_species = length(
        compartilhadas
      ),
      only_landscape_1 = length(
        exclusivas_1
      ),
      only_landscape_2 = length(
        exclusivas_2
      ),
      jaccard_similarity = round(
        length(compartilhadas) / total_uniao,
        4
      ),
      shared_list = paste(
        compartilhadas,
        collapse = "; "
      ),
      only_landscape_1_list = paste(
        exclusivas_1,
        collapse = "; "
      ),
      only_landscape_2_list = paste(
        exclusivas_2,
        collapse = "; "
      )
    )
  }
)


# Cores usadas nos gráficos
cores_paisagens <- c(
  "Cerrado" = "#C98B1D",
  "Floodplain" = "#4A86E8",
  "Pantanal" = "#3D9961"
)

cores_beta <- c(
  "Total beta" = "#5E5E5E",
  "Turnover" = "#4A86E8",
  "Nestedness" = "#D9796C"
)

cores_combinacoes <- c(
  "1 landscape" = "#9A9A9A",
  "2 landscapes" = "#6F96DA",
  "3 landscapes" = "#47A56A"
)


# Formatação comum aos quatro gráficos
tema_artigo <- function() {
  
  theme_minimal(
    base_size = 14
  ) +
    theme(
      plot.background = element_rect(
        fill = "#EBEBEB",
        color = NA
      ),
      panel.background = element_rect(
        fill = "#EBEBEB",
        color = NA
      ),
      panel.grid.major = element_line(
        color = "#D0D0D0",
        linewidth = 0.75
      ),
      panel.grid.minor = element_line(
        color = "#DDDDDD",
        linewidth = 0.45
      ),
      axis.line = element_line(
        color = "black",
        linewidth = 0.8
      ),
      axis.ticks = element_line(
        color = "black",
        linewidth = 0.55
      ),
      axis.text = element_text(
        color = "black"
      ),
      axis.title = element_text(
        face = "bold",
        color = "black",
        size = 15
      ),
      plot.title = element_text(
        face = "bold",
        size = 22,
        color = "black"
      ),
      plot.subtitle = element_text(
        size = 15,
        color = "black"
      ),
      strip.text = element_text(
        face = "bold",
        size = 13,
        color = "black"
      ),
      legend.title = element_text(
        face = "bold",
        color = "black"
      ),
      legend.text = element_text(
        color = "black"
      ),
      legend.background = element_rect(
        fill = "#EBEBEB",
        color = NA
      ),
      legend.key = element_rect(
        fill = "#EBEBEB",
        color = NA
      ),
      plot.margin = margin(
        18,
        36,
        18,
        18
      )
    )
}


dados_grafico_multisite <- tabela_beta_multisite |>
  mutate(
    component = factor(
      component,
      levels = c(
        "Total beta",
        "Turnover",
        "Nestedness"
      )
    )
  )


grafico_beta_multisite <- ggplot(
  dados_grafico_multisite,
  aes(
    x = value,
    y = component,
    color = component,
    fill = component
  )
) +
  geom_segment(
    aes(
      x = 0,
      xend = value,
      yend = component
    ),
    linewidth = 2.3,
    alpha = 0.95
  ) +
  geom_point(
    shape = 21,
    size = 6.8,
    stroke = 0.45,
    color = "black"
  ) +
  geom_label(
    aes(
      label = sprintf(
        "%.2f",
        value
      )
    ),
    nudge_x = 0.03,
    hjust = 0,
    size = 4.7,
    color = "black",
    fill = "#EBEBEB",
    label.size = NA,
    label.padding = grid::unit(
      0.10,
      "lines"
    )
  ) +
  scale_color_manual(
    values = cores_beta
  ) +
  scale_fill_manual(
    values = cores_beta
  ) +
  scale_x_continuous(
    limits = c(
      0,
      1.08
    ),
    breaks = seq(
      0,
      1,
      0.2
    ),
    expand = expansion(
      mult = c(
        0,
        0.04
      )
    )
  ) +
  labs(
    title = "Multisite beta diversity across landscapes",
    subtitle = "Jaccard decomposition for Cerrado, Floodplain and Pantanal",
    x = "Beta diversity value",
    y = NULL
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  tema_artigo() +
  theme(
    legend.position = "none",
    panel.grid.minor.y = element_blank()
  )

salvar_grafico(
  grafico_beta_multisite,
  "beta_multisite_jaccard"
)


dados_grafico_pares <- tabela_beta_pares |>
  pivot_longer(
    cols = c(
      total_beta,
      turnover,
      nestedness
    ),
    names_to = "component",
    values_to = "value"
  ) |>
  mutate(
    component = recode(
      component,
      total_beta = "Total beta",
      turnover = "Turnover",
      nestedness = "Nestedness"
    ),
    component = factor(
      component,
      levels = c(
        "Total beta",
        "Turnover",
        "Nestedness"
      )
    ),
    comparison = factor(
      comparison,
      levels = c(
        "Cerrado vs Floodplain",
        "Cerrado vs Pantanal",
        "Floodplain vs Pantanal"
      )
    )
  )


grafico_beta_pares <- ggplot(
  dados_grafico_pares,
  aes(
    x = value,
    y = component,
    color = component,
    fill = component
  )
) +
  geom_segment(
    aes(
      x = 0,
      xend = value,
      yend = component
    ),
    linewidth = 2,
    alpha = 0.95
  ) +
  geom_point(
    shape = 21,
    size = 5.5,
    stroke = 0.4,
    color = "black"
  ) +
  geom_label(
    aes(
      label = sprintf(
        "%.3f",
        value
      )
    ),
    nudge_x = 0.03,
    hjust = 0,
    size = 4,
    color = "black",
    fill = "#EBEBEB",
    label.size = NA,
    label.padding = grid::unit(
      0.08,
      "lines"
    )
  ) +
  facet_wrap(
    ~ comparison,
    nrow = 1
  ) +
  scale_color_manual(
    values = cores_beta
  ) +
  scale_fill_manual(
    values = cores_beta
  ) +
  scale_x_continuous(
    limits = c(
      0,
      1.08
    ),
    breaks = seq(
      0,
      1,
      0.2
    ),
    expand = expansion(
      mult = c(
        0,
        0.04
      )
    )
  ) +
  labs(
    title = "Pairwise beta diversity decomposition among landscapes",
    subtitle = "Jaccard total beta, turnover and nestedness",
    x = "Beta diversity value",
    y = NULL
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  tema_artigo() +
  theme(
    legend.position = "none",
    panel.grid.minor.y = element_blank()
  )

salvar_grafico(
  grafico_beta_pares,
  "beta_pairwise_landscapes_jaccard"
)


dados_grafico_combinacoes <- tabela_combinacoes |>
  mutate(
    group = factor(
      n_landscapes,
      levels = c(
        1,
        2,
        3
      ),
      labels = c(
        "1 landscape",
        "2 landscapes",
        "3 landscapes"
      )
    )
  ) |>
  arrange(
    n_landscapes,
    richness
  ) |>
  mutate(
    combination = factor(
      combination,
      levels = combination
    )
  )


grafico_combinacoes <- ggplot(
  dados_grafico_combinacoes,
  aes(
    x = richness,
    y = combination,
    color = group,
    fill = group
  )
) +
  geom_segment(
    aes(
      x = 0,
      xend = richness,
      yend = combination
    ),
    linewidth = 2,
    alpha = 0.95
  ) +
  geom_point(
    shape = 21,
    size = 5.9,
    stroke = 0.4,
    color = "black"
  ) +
  geom_label(
    aes(
      label = paste0(
        richness,
        " spp. | ",
        sprintf(
          "%.2f",
          retention_pct_of_gamma
        ),
        "%"
      )
    ),
    nudge_x = 0.25,
    hjust = 0,
    size = 4.1,
    color = "black",
    fill = "#EBEBEB",
    label.size = NA,
    label.padding = grid::unit(
      0.10,
      "lines"
    )
  ) +
  scale_color_manual(
    values = cores_combinacoes
  ) +
  scale_fill_manual(
    values = cores_combinacoes
  ) +
  scale_x_continuous(
    limits = c(
      0,
      max(
        dados_grafico_combinacoes$richness
      ) + 3.2
    ),
    breaks = 0:(
      max(
        dados_grafico_combinacoes$richness
      ) + 3
    ),
    expand = expansion(
      mult = c(
        0,
        0.04
      )
    )
  ) +
  labs(
    title = "Complementarity among landscape combinations",
    subtitle = "Species retained by one, two and three landscapes",
    x = "Retained richness",
    y = NULL,
    color = NULL,
    fill = NULL
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  tema_artigo() +
  theme(
    legend.position = "top",
    panel.grid.minor.y = element_blank()
  )

salvar_grafico(
  grafico_combinacoes,
  "species_retention_by_landscape_combinations"
)


dados_grafico_exclusivas <- tabela_especies_exclusivas |>
  mutate(
    landscape = factor(
      landscape,
      levels = c(
        "Cerrado",
        "Floodplain",
        "Pantanal"
      )
    )
  )


grafico_especies_exclusivas <- ggplot(
  dados_grafico_exclusivas,
  aes(
    x = unique_species,
    y = landscape,
    color = landscape,
    fill = landscape
  )
) +
  geom_segment(
    aes(
      x = 0,
      xend = unique_species,
      yend = landscape
    ),
    linewidth = 2.2,
    alpha = 0.95
  ) +
  geom_point(
    shape = 21,
    size = 6.4,
    stroke = 0.4,
    color = "black"
  ) +
  geom_label(
    aes(
      label = unique_species
    ),
    nudge_x = 0.18,
    hjust = 0,
    size = 4.8,
    color = "black",
    fill = "#EBEBEB",
    label.size = NA,
    label.padding = grid::unit(
      0.08,
      "lines"
    )
  ) +
  scale_color_manual(
    values = cores_paisagens
  ) +
  scale_fill_manual(
    values = cores_paisagens
  ) +
  scale_x_continuous(
    limits = c(
      0,
      max(
        dados_grafico_exclusivas$unique_species
      ) + 1.8
    ),
    breaks = 0:(
      max(
        dados_grafico_exclusivas$unique_species
      ) + 1
    ),
    expand = expansion(
      mult = c(
        0,
        0.04
      )
    )
  ) +
  labs(
    title = "Unique species by landscape",
    subtitle = "Exclusive species in the inferential suspension-trap base",
    x = "Number of unique species",
    y = NULL
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  tema_artigo() +
  theme(
    legend.position = "none",
    panel.grid.minor.y = element_blank()
  )

salvar_grafico(
  grafico_especies_exclusivas,
  "unique_species_by_landscape"
)


# Principais resultados apresentados no console
melhor_paisagem <- tabela_combinacoes |>
  filter(
    n_landscapes == 1
  ) |>
  arrange(
    desc(richness)
  ) |>
  slice(1)

melhor_par <- tabela_combinacoes |>
  filter(
    n_landscapes == 2
  ) |>
  arrange(
    desc(richness)
  ) |>
  slice(1)

todas_paisagens <- tabela_combinacoes |>
  filter(
    n_landscapes == length(
      paisagens
    )
  )

salvar_csv(
  tabela_beta_multisite,
  "beta_multisite_jaccard.csv"
)

salvar_csv(
  tabela_beta_total,
  "beta_pairwise_landscapes_jaccard_total.csv"
)

salvar_csv(
  tabela_turnover,
  "beta_pairwise_landscapes_jaccard_turnover.csv"
)

salvar_csv(
  tabela_nestedness,
  "beta_pairwise_landscapes_jaccard_nestedness.csv"
)

salvar_csv(
  tabela_beta_pares,
  "beta_pairwise_landscapes_jaccard_summary.csv"
)

salvar_csv(
  tabela_combinacoes,
  "species_retention_by_landscape_combinations.csv"
)

salvar_csv(
  tabela_ganho_marginal,
  "marginal_gain_by_added_landscape.csv"
)

salvar_csv(
  tabela_sobreposicao,
  "pairwise_overlap_and_uniqueness.csv"
)

salvar_csv(
  tabela_especies_exclusivas,
  "species_uniqueness_by_landscape.csv"
)

salvar_csv(
  pontos_por_paisagem,
  "points_per_landscape.csv"
)

salvar_csv(
  riqueza_por_paisagem,
  "richness_per_landscape.csv"
)

saveRDS(
  list(
    arquivo_csv = arquivo_csv,
    numero_registros_brutos = numero_registros_brutos,
    numero_registros_suspensao = numero_registros_suspensao,
    numero_registros_deduplicados = numero_registros_deduplicados,
    numero_pontos = numero_pontos,
    numero_especies = numero_especies,
    pontos_por_paisagem = pontos_por_paisagem,
    riqueza_por_paisagem = riqueza_por_paisagem,
    tabela_beta_multisite = tabela_beta_multisite,
    tabela_beta_total = tabela_beta_total,
    tabela_turnover = tabela_turnover,
    tabela_nestedness = tabela_nestedness,
    tabela_beta_pares = tabela_beta_pares,
    tabela_combinacoes = tabela_combinacoes,
    tabela_ganho_marginal = tabela_ganho_marginal,
    tabela_sobreposicao = tabela_sobreposicao,
    tabela_especies_exclusivas = tabela_especies_exclusivas
  ),
  file = file.path(
    output_directory,
    "beta_results_bundle.rds"
  )
)


cat(
  "\nAnálise concluída\n\n"
)

cat(
  "Arquivo utilizado:\n",
  arquivo_csv,
  "\n\n"
)

cat(
  "Registros brutos:",
  numero_registros_brutos,
  "\n"
)

cat(
  "Registros de suspension:",
  numero_registros_suspensao,
  "\n"
)

cat(
  "Registros após a deduplicação:",
  numero_registros_deduplicados,
  "\n"
)

cat(
  "Pontos válidos:",
  numero_pontos,
  "\n"
)

cat(
  "Espécies na base inferencial:",
  numero_especies,
  "\n\n"
)

cat(
  "Pontos por paisagem:\n"
)

print(
  pontos_por_paisagem
)

cat(
  "\nRiqueza por paisagem:\n"
)

print(
  riqueza_por_paisagem
)

cat(
  "\nDiversidade beta multisite pelo índice de Jaccard:\n"
)

cat(
  "Beta total:",
  round(
    beta_total,
    4
  ),
  "\n"
)

cat(
  "Turnover:",
  round(
    beta_turnover,
    4
  ),
  "\n"
)

cat(
  "Nestedness:",
  round(
    beta_nestedness,
    4
  ),
  "\n\n"
)

cat(
  "Paisagem com maior riqueza isolada:",
  melhor_paisagem$combination,
  "-",
  melhor_paisagem$richness,
  "espécies, correspondendo a",
  melhor_paisagem$retention_pct_of_gamma,
  "% da riqueza total.\n"
)

cat(
  "Melhor combinação de duas paisagens:",
  melhor_par$combination,
  "-",
  melhor_par$richness,
  "espécies, correspondendo a",
  melhor_par$retention_pct_of_gamma,
  "% da riqueza total.\n"
)

cat(
  "Todas as paisagens reunidas:",
  todas_paisagens$richness,
  "espécies, correspondendo a",
  todas_paisagens$retention_pct_of_gamma,
  "% da riqueza total.\n\n"
)

cat(
  "Os arquivos foram salvos em:\n",
  output_directory,
  "\n"
)