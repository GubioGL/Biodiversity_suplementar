set.seed(123)

options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  timeout = 600
)

required_packages <- c(
  "dplyr",
  "tidyr",
  "readr",
  "stringr",
  "tibble",
  "janitor",
  "ggplot2",
  "patchwork",
  "scales",
  "zetadiv"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  message(
    "Installing missing packages: ",
    paste(missing_packages, collapse = ", ")
  )
  
  install.packages(
    missing_packages,
    dependencies = c(
      "Depends",
      "Imports",
      "LinkingTo"
    )
  )
}

packages_still_missing <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(packages_still_missing) > 0) {
  stop(
    "The following packages could not be installed: ",
    paste(packages_still_missing, collapse = ", "),
    ". Check the internet connection and the installed R version."
  )
}

script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub(
  "^--file=",
  "",
  script_args[grep("^--file=", script_args)]
)

candidate_directories <- unique(
  c(
    if (length(script_file) > 0) {
      dirname(script_file)
    },
    file.path(
      getwd(),
      "material_suplementar",
      "ZETA"
    ),
    getwd()
  )
)

analysis_directory <- NA_character_

for (candidate_directory in candidate_directories) {
  if (dir.exists(candidate_directory)) {
    analysis_directory <- normalizePath(
      candidate_directory,
      winslash = "/",
      mustWork = TRUE
    )
    break
  }
}

if (is.na(analysis_directory)) {
  stop(
    "The analysis directory was not found. Tried:\n- ",
    paste(candidate_directories, collapse = "\n- ")
  )
}

output_directory <- file.path(
  analysis_directory,
  "output"
)

dir.create(
  output_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

normalize_path_safe <- function(path) {
  normalizePath(
    path,
    winslash = "/",
    mustWork = FALSE
  )
}

is_inside_directory <- function(paths, directory) {
  normalized_paths <- normalize_path_safe(paths)
  
  normalized_directory <- paste0(
    normalize_path_safe(directory),
    "/"
  )
  
  startsWith(
    normalized_paths,
    normalized_directory
  )
}

search_directories <- unique(
  c(
    analysis_directory,
    file.path(
      analysis_directory,
      "data"
    ),
    file.path(
      dirname(analysis_directory),
      "data"
    )
  )
)

search_directories <- search_directories[
  dir.exists(search_directories)
]

csv_files <- unique(
  unlist(
    lapply(
      search_directories,
      function(directory) {
        list.files(
          path = directory,
          pattern = "\\.csv$",
          full.names = TRUE,
          recursive = FALSE,
          ignore.case = TRUE
        )
      }
    ),
    use.names = FALSE
  )
)

csv_files <- csv_files[
  file.exists(csv_files)
]

if (length(csv_files) > 0) {
  csv_files <- csv_files[
    !is_inside_directory(
      csv_files,
      output_directory
    )
  ]
}

csv_files <- csv_files[
  !grepl(
    "suspens|nmds|zeta|poncho|result|output",
    basename(csv_files),
    ignore.case = TRUE
  )
]

preferred_csv <- csv_files[
  grepl(
    "^dados\\.csv$",
    basename(csv_files),
    ignore.case = TRUE
  )
]

if (length(preferred_csv) > 0) {
  csv_files <- preferred_csv
}

if (length(csv_files) == 0) {
  if (interactive()) {
    message(
      "The main dataset was not found automatically. ",
      "Select the main dados.csv file."
    )
    
    selected_file <- tryCatch(
      file.choose(),
      error = function(e) ""
    )
    
    if (
      length(selected_file) == 1 &&
      nzchar(selected_file) &&
      file.exists(selected_file) &&
      tolower(tools::file_ext(selected_file)) == "csv"
    ) {
      csv_files <- selected_file
    }
  }
}

if (length(csv_files) == 0) {
  stop(
    "No valid CSV file was found inside:\n- ",
    paste(search_directories, collapse = "\n- ")
  )
}

if (length(csv_files) == 0) {
  stop(
    "No valid CSV file was found inside:\n",
    analysis_directory
  )
}

csv_information <- file.info(
  csv_files
)

csv_modification_times <- csv_information$mtime

csv_modification_times[
  is.na(csv_modification_times)
] <- as.POSIXct(
  "1970-01-01",
  tz = "UTC"
)

file_csv <- normalizePath(
  csv_files[
    which.max(csv_modification_times)
  ],
  winslash = "/",
  mustWork = TRUE
)

cat(
  "Analysis directory:\n",
  analysis_directory,
  "\n\n",
  sep = ""
)

cat(
  "Input file:\n",
  file_csv,
  "\n\n",
  sep = ""
)

cat(
  "Output directory:\n",
  output_directory,
  "\n\n",
  sep = ""
)

max_zeta_order_user <- 8
zeta_samples <- 5000

clean_text <- function(x) {
  x <- as.character(x)
  
  stringr::str_squish(
    x
  )
}

normalize_text <- function(x) {
  x <- as.character(x)
  
  x <- iconv(
    x,
    from = "",
    to = "ASCII//TRANSLIT"
  )
  
  x <- tolower(
    trimws(x)
  )
  
  x
}

pick_column <- function(
    data,
    candidates
) {
  matches <- candidates[
    candidates %in% names(data)
  ]
  
  if (length(matches) == 0) {
    return(
      NA_character_
    )
  }
  
  matches[1]
}

optional_character_column <- function(
    data,
    column_name
) {
  if (
    !is.na(column_name) &&
    column_name %in% names(data)
  ) {
    return(
      clean_text(
        data[[column_name]]
      )
    )
  }
  
  rep(
    NA_character_,
    nrow(data)
  )
}

extract_zeta_output <- function(
    zeta_object,
    group_name
) {
  zeta_order <- as.numeric(
    zeta_object$zeta.order
  )
  
  zeta_values <- as.numeric(
    zeta_object$zeta.val
  )
  
  if (
    length(zeta_order) == 0 ||
    length(zeta_values) == 0
  ) {
    stop(
      "The zetadiv result did not contain valid zeta values for: ",
      group_name
    )
  }
  
  if (
    length(zeta_order) !=
    length(zeta_values)
  ) {
    stop(
      "The zeta orders and values have different lengths for: ",
      group_name
    )
  }
  
  if (!is.null(zeta_object$zeta.val.sd)) {
    zeta_sd <- as.numeric(
      zeta_object$zeta.val.sd
    )
  } else {
    zeta_sd <- rep(
      NA_real_,
      length(zeta_values)
    )
  }
  
  if (
    length(zeta_sd) !=
    length(zeta_values)
  ) {
    zeta_sd <- rep(
      NA_real_,
      length(zeta_values)
    )
  }
  
  if (!is.null(zeta_object$combinations)) {
    combinations <- as.numeric(
      zeta_object$combinations
    )
  } else {
    combinations <- rep(
      NA_real_,
      length(zeta_values)
    )
  }
  
  if (
    length(combinations) !=
    length(zeta_values)
  ) {
    combinations <- rep(
      NA_real_,
      length(zeta_values)
    )
  }
  
  zeta_1 <- zeta_values[
    zeta_order == 1
  ][1]
  
  if (
    length(zeta_1) == 0 ||
    is.na(zeta_1) ||
    zeta_1 <= 0
  ) {
    stop(
      "A valid first-order zeta value was not obtained for: ",
      group_name
    )
  }
  
  tibble::tibble(
    landscape = group_name,
    order = zeta_order,
    combinations = combinations,
    zeta = zeta_values,
    zeta_sd = zeta_sd
  ) |>
    dplyr::arrange(
      order
    ) |>
    dplyr::mutate(
      zeta_norm = zeta / zeta_1,
      zeta_sd_norm = zeta_sd / zeta_1,
      zeta_lcl_norm = pmax(
        0,
        zeta_norm - zeta_sd_norm
      ),
      zeta_ucl_norm = pmin(
        1,
        zeta_norm + zeta_sd_norm
      ),
      zeta_ratio = zeta /
        dplyr::lag(zeta)
    )
}

run_zeta_decline <- function(
    community_matrix,
    group_name,
    orders,
    samples = 5000
) {
  community_matrix <- as.data.frame(
    community_matrix
  )
  
  community_matrix[] <- lapply(
    community_matrix,
    function(x) {
      as.integer(
        as.numeric(x) > 0
      )
    }
  )
  
  community_matrix <- community_matrix[
    rowSums(community_matrix) > 0,
    ,
    drop = FALSE
  ]
  
  community_matrix <- community_matrix[
    ,
    colSums(community_matrix) > 0,
    drop = FALSE
  ]
  
  if (nrow(community_matrix) < 2) {
    stop(
      "Too few sampling units to calculate zeta diversity for: ",
      group_name
    )
  }
  
  if (ncol(community_matrix) < 1) {
    stop(
      "No species remained in the matrix for: ",
      group_name
    )
  }
  
  group_orders <- orders[
    orders <= nrow(community_matrix)
  ]
  
  if (length(group_orders) < 2) {
    stop(
      "Too few zeta orders could be calculated for: ",
      group_name
    )
  }
  
  names(community_matrix) <- make.names(
    names(community_matrix),
    unique = TRUE
  )
  
  set.seed(123)
  
  zeta_object <- zetadiv::Zeta.decline.mc(
    data.spec = community_matrix,
    orders = group_orders,
    sam = samples,
    sd.correct = TRUE,
    sd.correct.adapt = TRUE,
    confint.level = 0.95,
    sd.plot = FALSE,
    rescale = FALSE,
    normalize = FALSE,
    empty.row = "empty",
    plot = FALSE,
    silent = TRUE
  )
  
  extract_zeta_output(
    zeta_object,
    group_name
  )
}

fit_zeta_models <- function(zeta_data) {
  landscape_names <- unique(
    as.character(
      zeta_data$landscape
    )
  )
  
  model_results <- lapply(
    landscape_names,
    function(landscape_name) {
      current_data <- zeta_data |>
        dplyr::filter(
          as.character(landscape) ==
            landscape_name,
          order >= 1,
          zeta_norm > 0,
          is.finite(zeta_norm)
        )
      
      if (nrow(current_data) < 3) {
        return(
          tibble::tibble(
            landscape = landscape_name,
            exponential_AIC = NA_real_,
            powerlaw_AIC = NA_real_,
            best_model = NA_character_,
            delta_AIC_power_minus_exp = NA_real_
          )
        )
      }
      
      exponential_model <- stats::lm(
        log(zeta_norm) ~ order,
        data = current_data
      )
      
      powerlaw_model <- stats::lm(
        log(zeta_norm) ~ log(order),
        data = current_data
      )
      
      exponential_aic <- stats::AIC(
        exponential_model
      )
      
      powerlaw_aic <- stats::AIC(
        powerlaw_model
      )
      
      tibble::tibble(
        landscape = landscape_name,
        exponential_AIC = exponential_aic,
        powerlaw_AIC = powerlaw_aic,
        best_model = ifelse(
          powerlaw_aic < exponential_aic,
          "Power-law",
          "Exponential"
        ),
        delta_AIC_power_minus_exp =
          powerlaw_aic -
          exponential_aic
      )
    }
  )
  
  dplyr::bind_rows(
    model_results
  )
}

raw_data <- readr::read_delim(
  file = file_csv,
  delim = ";",
  locale = readr::locale(
    encoding = "UTF-8"
  ),
  show_col_types = FALSE,
  trim_ws = TRUE
) |>
  janitor::clean_names()

cat(
  "Detected columns:\n"
)

print(
  names(raw_data)
)

cat(
  "\n"
)

species_column <- pick_column(
  raw_data,
  c(
    "species",
    "especie"
  )
)

point_column <- pick_column(
  raw_data,
  c(
    "point",
    "ponto",
    "site",
    "site_id",
    "local",
    "localidade"
  )
)

landscape_column <- pick_column(
  raw_data,
  c(
    "landscape",
    "paisagem",
    "landscape_class"
  )
)

trap_column <- pick_column(
  raw_data,
  c(
    "trap",
    "armadilha",
    "method",
    "metodo"
  )
)

collection_column <- pick_column(
  raw_data,
  c(
    "collection",
    "coleta",
    "sample_type",
    "tipo_coleta"
  )
)

date_column <- pick_column(
  raw_data,
  c(
    "date",
    "data"
  )
)

latitude_column <- pick_column(
  raw_data,
  c(
    "lati_s",
    "latitude",
    "lat"
  )
)

longitude_column <- pick_column(
  raw_data,
  c(
    "long_w",
    "longitude",
    "lon",
    "long"
  )
)

classification_column <- pick_column(
  raw_data,
  c(
    "classification",
    "classificacao"
  )
)

if (is.na(species_column)) {
  stop(
    "The species column was not found."
  )
}

if (is.na(point_column)) {
  stop(
    "The sampling point column was not found."
  )
}

if (is.na(trap_column)) {
  stop(
    "The trap or collection-method column was not found."
  )
}

if (is.na(latitude_column)) {
  stop(
    "The latitude column was not found."
  )
}

if (is.na(longitude_column)) {
  stop(
    "The longitude column was not found."
  )
}

species_values <- clean_text(
  raw_data[[species_column]]
)

point_values <- clean_text(
  raw_data[[point_column]]
)

trap_values <- clean_text(
  raw_data[[trap_column]]
)

latitude_values <- clean_text(
  raw_data[[latitude_column]]
)

longitude_values <- clean_text(
  raw_data[[longitude_column]]
)

landscape_values <- optional_character_column(
  raw_data,
  landscape_column
)

collection_values <- optional_character_column(
  raw_data,
  collection_column
)

date_values <- optional_character_column(
  raw_data,
  date_column
)

classification_values <- optional_character_column(
  raw_data,
  classification_column
)

data_clean <- raw_data |>
  dplyr::mutate(
    species = species_values,
    point = point_values,
    trap = trap_values,
    trap_normalized = normalize_text(
      trap_values
    ),
    collection = normalize_text(
      collection_values
    ),
    landscape = normalize_text(
      landscape_values
    ),
    classification = classification_values,
    date = date_values,
    date_standardized = dplyr::if_else(
      is.na(date_values) |
        date_values == "",
      "NO_DATE",
      date_values
    ),
    lati_s = latitude_values,
    long_w = longitude_values,
    trap_standardized = dplyr::case_when(
      stringr::str_detect(
        trap_normalized,
        "susp"
      ) ~ "suspension",
      stringr::str_detect(
        trap_normalized,
        "puc"
      ) ~ "puca",
      stringr::str_detect(
        trap_normalized,
        "mala"
      ) ~ "malaise",
      TRUE ~ trap_normalized
    ),
    landscape_class = dplyr::case_when(
      stringr::str_detect(
        landscape,
        "pant"
      ) ~ "Pantanal",
      stringr::str_detect(
        landscape,
        "cerr"
      ) ~ "Cerrado",
      stringr::str_detect(
        landscape,
        "flood|varzea"
      ) ~ "Floodplain",
      stringr::str_detect(
        normalize_text(point),
        "baia negra|baianegra"
      ) ~ "Pantanal",
      stringr::str_detect(
        normalize_text(point),
        "bodoquena"
      ) ~ "Cerrado",
      stringr::str_detect(
        normalize_text(point),
        "pevri|ivinhema|varzea"
      ) ~ "Floodplain",
      TRUE ~ NA_character_
    ),
    area_name = dplyr::case_when(
      landscape_class == "Pantanal" ~
        "Baia Negra",
      landscape_class == "Cerrado" ~
        "Serra da Bodoquena",
      landscape_class == "Floodplain" ~
        "PEVRI",
      TRUE ~ point
    ),
    point_id = dplyr::case_when(
      !is.na(lati_s) &
        lati_s != "" &
        !is.na(long_w) &
        long_w != "" ~
        paste(
          area_name,
          lati_s,
          long_w,
          sep = " | "
        ),
      TRUE ~
        paste(
          area_name,
          point,
          sep = " | "
        )
    ),
    event_id = paste(
      point_id,
      date_standardized,
      sep = " || "
    )
  ) |>
  dplyr::filter(
    !is.na(species),
    species != "",
    !is.na(trap_standardized),
    trap_standardized != "",
    !is.na(landscape_class),
    !is.na(point_id),
    point_id != ""
  )

suspension_rows <- data_clean |>
  dplyr::filter(
    trap_standardized == "suspension"
  )

if (nrow(suspension_rows) == 0) {
  stop(
    "No suspension-trap records were found."
  )
}

suspension_records <- suspension_rows |>
  dplyr::distinct(
    point_id,
    event_id,
    landscape_class,
    area_name,
    date_standardized,
    species,
    .keep_all = TRUE
  )

cat(
  "DATA DIAGNOSTICS\n"
)

cat(
  "Raw rows in the input file:               ",
  nrow(raw_data),
  "\n"
)

cat(
  "Rows after general cleaning:              ",
  nrow(data_clean),
  "\n"
)

cat(
  "Raw suspension-trap rows:                 ",
  nrow(suspension_rows),
  "\n"
)

cat(
  "Deduplicated suspension records:          ",
  nrow(suspension_records),
  "\n"
)

cat(
  "Species in the suspension dataset:        ",
  dplyr::n_distinct(
    suspension_records$species
  ),
  "\n"
)

cat(
  "Sampling units in the suspension dataset: ",
  dplyr::n_distinct(
    suspension_records$point_id
  ),
  "\n\n"
)

records_by_landscape <- suspension_records |>
  dplyr::count(
    landscape_class,
    name = "n_records"
  ) |>
  dplyr::arrange(
    match(
      landscape_class,
      c(
        "Cerrado",
        "Floodplain",
        "Pantanal"
      )
    )
  )

species_by_landscape <- suspension_records |>
  dplyr::distinct(
    landscape_class,
    species
  ) |>
  dplyr::count(
    landscape_class,
    name = "n_species"
  ) |>
  dplyr::arrange(
    match(
      landscape_class,
      c(
        "Cerrado",
        "Floodplain",
        "Pantanal"
      )
    )
  )

points_by_landscape <- suspension_records |>
  dplyr::distinct(
    landscape_class,
    point_id
  ) |>
  dplyr::count(
    landscape_class,
    name = "n_points"
  ) |>
  dplyr::arrange(
    match(
      landscape_class,
      c(
        "Cerrado",
        "Floodplain",
        "Pantanal"
      )
    )
  )

cat(
  "Deduplicated records by landscape:\n"
)

print(
  records_by_landscape
)

cat(
  "\nSpecies by landscape:\n"
)

print(
  species_by_landscape
)

cat(
  "\nSampling units by landscape:\n"
)

print(
  points_by_landscape
)

expected_records <- 99
expected_species <- 17
expected_points <- 46

obtained_records <- nrow(
  suspension_records
)

obtained_species <- dplyr::n_distinct(
  suspension_records$species
)

obtained_points <- dplyr::n_distinct(
  suspension_records$point_id
)

if (
  obtained_records != expected_records ||
  obtained_species != expected_species ||
  obtained_points != expected_points
) {
  warning(
    "\nThe generated values differ from the expected manuscript dataset.\n",
    "Expected: 99 records, 17 species and 46 sampling units.\n",
    "Obtained: ",
    obtained_records,
    " records, ",
    obtained_species,
    " species and ",
    obtained_points,
    " sampling units.\n",
    "Check whether the selected CSV is the correct main dataset."
  )
}

community_data <- suspension_records |>
  dplyr::mutate(
    presence = 1L
  ) |>
  dplyr::distinct(
    landscape_class,
    point_id,
    species,
    presence
  ) |>
  tidyr::pivot_wider(
    names_from = species,
    values_from = presence,
    values_fill = 0
  ) |>
  dplyr::arrange(
    match(
      landscape_class,
      c(
        "Cerrado",
        "Floodplain",
        "Pantanal"
      )
    ),
    point_id
  )

metadata <- community_data |>
  dplyr::select(
    landscape_class,
    point_id
  ) |>
  dplyr::mutate(
    landscape_class = as.character(
      landscape_class
    ),
    point_id = as.character(
      point_id
    )
  )

community_matrix <- community_data |>
  dplyr::select(
    -landscape_class,
    -point_id
  ) |>
  as.data.frame()

rownames(
  community_matrix
) <- metadata$point_id

keep_rows <- rowSums(
  community_matrix
) > 0

community_matrix <- community_matrix[
  keep_rows,
  ,
  drop = FALSE
]

metadata <- metadata[
  keep_rows,
  ,
  drop = FALSE
]

keep_columns <- colSums(
  community_matrix
) > 0

community_matrix <- community_matrix[
  ,
  keep_columns,
  drop = FALSE
]

cat(
  "\nFinal zeta matrix dimensions:\n"
)

cat(
  nrow(community_matrix),
  "sampling units x",
  ncol(community_matrix),
  "species\n\n"
)

if (nrow(community_matrix) < 4) {
  stop(
    "Too few sampling units remained for the zeta-diversity analysis."
  )
}

if (ncol(community_matrix) < 2) {
  stop(
    "Too few species remained for the zeta-diversity analysis."
  )
}

landscape_order <- c(
  "Cerrado",
  "Floodplain",
  "Pantanal"
)

units_by_landscape <- metadata |>
  dplyr::count(
    landscape_class,
    name = "n_units"
  ) |>
  dplyr::filter(
    landscape_class %in%
      landscape_order
  )

cat(
  "Sampling units used in the zeta analysis:\n"
)

print(
  units_by_landscape
)

missing_landscapes <- setdiff(
  landscape_order,
  units_by_landscape$landscape_class
)

if (length(missing_landscapes) > 0) {
  stop(
    "The following landscapes have no sampling units in the matrix: ",
    paste(
      missing_landscapes,
      collapse = ", "
    )
  )
}

common_maximum_order <- min(
  max_zeta_order_user,
  min(
    units_by_landscape$n_units
  )
)

if (common_maximum_order < 2) {
  stop(
    "The number of sampling units is insufficient to calculate ",
    "at least two zeta orders for every landscape."
  )
}

orders_used <- seq_len(
  common_maximum_order
)

cat(
  "\nMaximum zeta order used: ",
  common_maximum_order,
  "\n",
  sep = ""
)

zeta_landscape_results <- lapply(
  landscape_order,
  function(landscape_name) {
    selected_rows <- which(
      metadata$landscape_class ==
        landscape_name
    )
    
    if (length(selected_rows) == 0) {
      stop(
        "No sampling units were found for: ",
        landscape_name
      )
    }
    
    cat(
      "\nRunning zeta for ",
      landscape_name,
      " using ",
      length(selected_rows),
      " sampling units...\n",
      sep = ""
    )
    
    landscape_matrix <- community_matrix[
      selected_rows,
      ,
      drop = FALSE
    ]
    
    run_zeta_decline(
      community_matrix = landscape_matrix,
      group_name = landscape_name,
      orders = orders_used,
      samples = zeta_samples
    )
  }
)

zeta_by_landscape <- dplyr::bind_rows(
  zeta_landscape_results
)

cat(
  "\nRunning zeta for all landscapes...\n"
)

zeta_total <- run_zeta_decline(
  community_matrix = community_matrix,
  group_name = "All landscapes",
  orders = orders_used,
  samples = zeta_samples
)

zeta_results <- dplyr::bind_rows(
  zeta_by_landscape,
  zeta_total
)

zeta_models <- fit_zeta_models(
  zeta_results
)

cat(
  "\nZeta values:\n"
)

print(
  zeta_results
)

cat(
  "\nZeta-decline model comparison:\n"
)

print(
  zeta_models
)

landscape_colors <- c(
  "Cerrado" = "#B9770E",
  "Floodplain" = "#2F80ED",
  "Pantanal" = "#2E8B57",
  "All landscapes" = "grey30"
)

plot_order <- c(
  "Cerrado",
  "Floodplain",
  "Pantanal",
  "All landscapes"
)

zeta_results <- zeta_results |>
  dplyr::mutate(
    landscape = factor(
      landscape,
      levels = plot_order
    )
  )

decline_plot <- ggplot2::ggplot(
  zeta_results,
  ggplot2::aes(
    x = order,
    y = zeta_norm,
    color = landscape,
    fill = landscape
  )
) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = zeta_lcl_norm,
      ymax = zeta_ucl_norm
    ),
    alpha = 0.12,
    color = NA,
    show.legend = FALSE
  ) +
  ggplot2::geom_line(
    linewidth = 1.15
  ) +
  ggplot2::geom_point(
    size = 3.2,
    shape = 21,
    stroke = 0.75
  ) +
  ggplot2::scale_color_manual(
    values = landscape_colors,
    name = "Landscape"
  ) +
  ggplot2::scale_fill_manual(
    values = landscape_colors,
    name = "Landscape"
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format(
      accuracy = 1
    ),
    limits = c(
      0,
      1.02
    ),
    expand = ggplot2::expansion(
      mult = c(
        0.01,
        0.04
      )
    )
  ) +
  ggplot2::scale_x_continuous(
    breaks = orders_used,
    minor_breaks = NULL
  ) +
  ggplot2::labs(
    title = "A. Zeta diversity decline",
    x = "Zeta order: number of sampling units combined",
    y = expression(
      "Shared species retained relative to " *
        zeta[1]
    )
  ) +
  ggplot2::theme_classic(
    base_size = 14
  ) +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = ggplot2::element_rect(
      fill = "white",
      color = NA
    ),
    panel.border = ggplot2::element_rect(
      fill = NA,
      color = "black",
      linewidth = 0.75
    ),
    axis.line = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(
      color = "black"
    ),
    axis.title = ggplot2::element_text(
      color = "black"
    ),
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 16,
      hjust = 0
    ),
    legend.position = "right",
    legend.title = ggplot2::element_text(
      face = "bold"
    )
  )

ratio_plot <- ggplot2::ggplot(
  zeta_results |>
    dplyr::filter(
      order > 1
    ),
  ggplot2::aes(
    x = order,
    y = zeta_ratio,
    color = landscape,
    fill = landscape
  )
) +
  ggplot2::geom_line(
    linewidth = 1.15
  ) +
  ggplot2::geom_point(
    size = 3.2,
    shape = 21,
    stroke = 0.75
  ) +
  ggplot2::scale_color_manual(
    values = landscape_colors,
    name = "Landscape"
  ) +
  ggplot2::scale_fill_manual(
    values = landscape_colors,
    name = "Landscape"
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format(
      accuracy = 1
    ),
    limits = c(
      0,
      NA
    ),
    expand = ggplot2::expansion(
      mult = c(
        0.02,
        0.08
      )
    )
  ) +
  ggplot2::scale_x_continuous(
    breaks = orders_used[
      orders_used > 1
    ],
    minor_breaks = NULL
  ) +
  ggplot2::labs(
    title = "B. Zeta retention ratio",
    x = "Zeta order",
    y = expression(
      zeta[i] / zeta[i - 1]
    )
  ) +
  ggplot2::theme_classic(
    base_size = 14
  ) +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = ggplot2::element_rect(
      fill = "white",
      color = NA
    ),
    panel.border = ggplot2::element_rect(
      fill = NA,
      color = "black",
      linewidth = 0.75
    ),
    axis.line = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(
      color = "black"
    ),
    axis.title = ggplot2::element_text(
      color = "black"
    ),
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 16,
      hjust = 0
    ),
    legend.position = "none"
  )

zeta_plot <- patchwork::wrap_plots(
  decline_plot,
  ratio_plot,
  widths = c(
    1.15,
    1
  )
) +
  patchwork::plot_annotation(
    title = paste(
      "Zeta diversity of social wasp assemblages",
      "across landscapes"
    ),
    subtitle = paste0(
      "Computed with zetadiv::Zeta.decline.mc | ",
      "Presence/absence matrix from standardized ",
      "suspension-trap records | ",
      nrow(community_matrix),
      " sampling units x ",
      ncol(community_matrix),
      " species"
    ),
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 22,
        hjust = 0.5
      ),
      plot.subtitle = ggplot2::element_text(
        size = 12.5,
        hjust = 0.5
      ),
      plot.background = ggplot2::element_rect(
        fill = "white",
        color = NA
      )
    )
  )

print(
  zeta_plot
)

output_png <- file.path(
  output_directory,
  "Zeta_diversity_zetadiv_social_wasps_suspension_traps.png"
)

output_tiff <- file.path(
  output_directory,
  "Zeta_diversity_zetadiv_social_wasps_suspension_traps.tif"
)

output_pdf <- file.path(
  output_directory,
  "Zeta_diversity_zetadiv_social_wasps_suspension_traps.pdf"
)

output_zeta_csv <- file.path(
  output_directory,
  "Zeta_diversity_zetadiv_values_social_wasps.csv"
)

output_model_csv <- file.path(
  output_directory,
  "Zeta_diversity_zetadiv_model_comparison.csv"
)

output_summary <- file.path(
  output_directory,
  "Zeta_diversity_zetadiv_summary.txt"
)

if (capabilities("cairo")) {
  grDevices::png(
    filename = output_png,
    width = 13.5,
    height = 7.8,
    units = "in",
    res = 1200,
    bg = "white",
    type = "cairo-png"
  )
} else {
  grDevices::png(
    filename = output_png,
    width = 13.5,
    height = 7.8,
    units = "in",
    res = 1200,
    bg = "white"
  )
}

tryCatch(
  {
    print(
      zeta_plot
    )
  },
  finally = {
    grDevices::dev.off()
  }
)

grDevices::tiff(
  filename = output_tiff,
  width = 13.5,
  height = 7.8,
  units = "in",
  res = 1200,
  compression = "lzw",
  bg = "white"
)

tryCatch(
  {
    print(
      zeta_plot
    )
  },
  finally = {
    grDevices::dev.off()
  }
)

if (capabilities("cairo")) {
  grDevices::cairo_pdf(
    filename = output_pdf,
    width = 13.5,
    height = 7.8,
    bg = "white"
  )
} else {
  grDevices::pdf(
    file = output_pdf,
    width = 13.5,
    height = 7.8,
    bg = "white",
    useDingbats = FALSE
  )
}

tryCatch(
  {
    print(
      zeta_plot
    )
  },
  finally = {
    grDevices::dev.off()
  }
)

readr::write_csv(
  zeta_results,
  output_zeta_csv,
  na = ""
)

readr::write_csv(
  zeta_models,
  output_model_csv,
  na = ""
)

zetadiv_version <- as.character(
  utils::packageVersion(
    "zetadiv"
  )
)

summary_lines <- capture.output(
  {
    cat(
      "Zeta diversity - social wasps\n"
    )
    
    cat(
      "Computation: zetadiv::Zeta.decline.mc\n"
    )
    
    cat(
      "zetadiv version:",
      zetadiv_version,
      "\n"
    )
    
    cat(
      "Base: standardized suspension-trap records only\n"
    )
    
    cat(
      "Matrix: sampling point x species presence/absence\n"
    )
    
    cat(
      "Sampling unit: point_id from area, latitude and longitude\n"
    )
    
    cat(
      "Deduplication: species, date and sampling point/location\n"
    )
    
    cat(
      "Orders:",
      paste(
        orders_used,
        collapse = ", "
      ),
      "\n"
    )
    
    cat(
      "Monte Carlo samples per order:",
      zeta_samples,
      "\n\n"
    )
    
    cat(
      "Input file:\n",
      file_csv,
      "\n\n",
      sep = ""
    )
    
    cat(
      "Raw file rows:\n",
      nrow(raw_data),
      "\n\n",
      sep = ""
    )
    
    cat(
      "Raw suspension rows:\n",
      nrow(suspension_rows),
      "\n\n",
      sep = ""
    )
    
    cat(
      "Deduplicated suspension records:\n",
      nrow(suspension_records),
      "\n\n",
      sep = ""
    )
    
    cat(
      "Final matrix dimensions:\n"
    )
    
    cat(
      nrow(community_matrix),
      "sampling units x",
      ncol(community_matrix),
      "species\n\n"
    )
    
    cat(
      "Sampling units by landscape:\n"
    )
    
    print(
      units_by_landscape
    )
    
    cat(
      "\nDeduplicated suspension records by landscape:\n"
    )
    
    print(
      records_by_landscape
    )
    
    cat(
      "\nSpecies by landscape:\n"
    )
    
    print(
      species_by_landscape
    )
    
    cat(
      "\nZeta values:\n"
    )
    
    print(
      zeta_results
    )
    
    cat(
      "\nZeta-decline model comparison:\n"
    )
    
    print(
      zeta_models
    )
    
    cat(
      "\nPackage citation:\n"
    )
    
    print(
      utils::citation(
        "zetadiv"
      )
    )
  }
)

writeLines(
  summary_lines,
  output_summary,
  useBytes = TRUE
)

cat(
  "\nAnalysis completed.\n\n"
)

cat(
  "zetadiv version: ",
  zetadiv_version,
  "\n\n",
  sep = ""
)

cat(
  "PNG saved at:\n",
  normalizePath(
    output_png,
    winslash = "/",
    mustWork = TRUE
  ),
  "\n\n",
  sep = ""
)

cat(
  "TIFF saved at:\n",
  normalizePath(
    output_tiff,
    winslash = "/",
    mustWork = TRUE
  ),
  "\n\n",
  sep = ""
)

cat(
  "PDF saved at:\n",
  normalizePath(
    output_pdf,
    winslash = "/",
    mustWork = TRUE
  ),
  "\n\n",
  sep = ""
)

cat(
  "Zeta values saved at:\n",
  normalizePath(
    output_zeta_csv,
    winslash = "/",
    mustWork = TRUE
  ),
  "\n\n",
  sep = ""
)

cat(
  "Model comparison saved at:\n",
  normalizePath(
    output_model_csv,
    winslash = "/",
    mustWork = TRUE
  ),
  "\n\n",
  sep = ""
)

cat(
  "Summary saved at:\n",
  normalizePath(
    output_summary,
    winslash = "/",
    mustWork = TRUE
  ),
  "\n",
  sep = ""
)