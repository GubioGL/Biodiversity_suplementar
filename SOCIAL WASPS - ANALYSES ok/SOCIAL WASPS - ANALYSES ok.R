# Social wasps analyses at point level

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
  "vegan",
  "indicspecies",
  "permute",
  "writexl",
  "ggplot2",
  "ggdist",
  "patchwork",
  "forcats",
  "scales",
  "rstudioapi"
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
    ". Check the internet connection and run the script again."
  )
}

suppressPackageStartupMessages(
  invisible(
    lapply(
      required_packages,
      library,
      character.only = TRUE
    )
  )
)

normalize_directory_name <- function(x) {
  x <- as.character(x)
  x <- iconv(
    x,
    from = "",
    to = "ASCII//TRANSLIT"
  )
  x <- tolower(x)
  x <- gsub(
    "[^a-z0-9]+",
    " ",
    x
  )
  stringr::str_squish(x)
}

get_starting_directories <- function() {
  directories <- character(0)
  
  if (
    requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()
  ) {
    active_document <- tryCatch(
      rstudioapi::getActiveDocumentContext()$path,
      error = function(e) ""
    )
    
    source_document <- tryCatch(
      rstudioapi::getSourceEditorContext()$path,
      error = function(e) ""
    )
    
    project_directory <- tryCatch(
      rstudioapi::getActiveProject(),
      error = function(e) NULL
    )
    
    document_paths <- c(
      active_document,
      source_document
    )
    
    document_paths <- document_paths[
      !is.na(document_paths) &
        nzchar(document_paths) &
        file.exists(document_paths)
    ]
    
    if (length(document_paths) > 0) {
      directories <- c(
        directories,
        dirname(document_paths)
      )
    }
    
    if (
      length(project_directory) == 1 &&
      !is.null(project_directory) &&
      !is.na(project_directory) &&
      nzchar(project_directory) &&
      dir.exists(project_directory)
    ) {
      directories <- c(
        directories,
        project_directory
      )
    }
  }
  
  command_arguments <- commandArgs(
    trailingOnly = FALSE
  )
  
  script_argument <- grep(
    "^--file=",
    command_arguments,
    value = TRUE
  )
  
  if (length(script_argument) > 0) {
    script_path <- sub(
      "^--file=",
      "",
      script_argument[1]
    )
    
    if (file.exists(script_path)) {
      directories <- c(
        directories,
        dirname(
          normalizePath(
            script_path,
            winslash = "/",
            mustWork = TRUE
          )
        )
      )
    }
  }
  
  frame_paths <- vapply(
    sys.frames(),
    function(frame) {
      if (
        !is.null(frame$ofile) &&
        length(frame$ofile) == 1
      ) {
        as.character(frame$ofile)
      } else {
        NA_character_
      }
    },
    character(1)
  )
  
  frame_paths <- frame_paths[
    !is.na(frame_paths) &
      nzchar(frame_paths) &
      file.exists(frame_paths)
  ]
  
  if (length(frame_paths) > 0) {
    directories <- c(
      directories,
      dirname(frame_paths)
    )
  }
  
  directories <- c(
    directories,
    getwd()
  )
  
  unique(
    directories[
      dir.exists(directories)
    ]
  )
}

add_parent_directories <- function(
    directory,
    maximum_levels = 10
) {
  result <- directory
  current_directory <- directory
  
  for (i in seq_len(maximum_levels)) {
    parent_directory <- dirname(
      current_directory
    )
    
    if (
      identical(
        parent_directory,
        current_directory
      )
    ) {
      break
    }
    
    result <- c(
      result,
      parent_directory
    )
    
    current_directory <- parent_directory
  }
  
  unique(result)
}

locate_analysis_directory <- function() {
  target_directory_name <- "SOCIAL WASPS - ANALYSES ok"
  
  target_normalized <- normalize_directory_name(
    target_directory_name
  )
  
  starting_directories <- get_starting_directories()
  
  candidate_directories <- unique(
    unlist(
      lapply(
        starting_directories,
        add_parent_directories
      ),
      use.names = FALSE
    )
  )
  
  candidate_directories <- candidate_directories[
    dir.exists(candidate_directories)
  ]
  
  direct_matches <- candidate_directories[
    normalize_directory_name(
      basename(candidate_directories)
    ) == target_normalized
  ]
  
  if (length(direct_matches) > 0) {
    return(
      normalizePath(
        direct_matches[1],
        winslash = "/",
        mustWork = TRUE
      )
    )
  }
  
  possible_paths <- unique(
    unlist(
      lapply(
        candidate_directories,
        function(directory) {
          c(
            file.path(
              directory,
              target_directory_name
            ),
            file.path(
              directory,
              "material_suplementar",
              target_directory_name
            )
          )
        }
      ),
      use.names = FALSE
    )
  )
  
  possible_paths <- possible_paths[
    dir.exists(possible_paths)
  ]
  
  if (length(possible_paths) > 0) {
    return(
      normalizePath(
        possible_paths[1],
        winslash = "/",
        mustWork = TRUE
      )
    )
  }
  
  for (directory in starting_directories) {
    if (!dir.exists(directory)) {
      next
    }
    
    nearby_directories <- list.dirs(
      path = directory,
      full.names = TRUE,
      recursive = FALSE
    )
    
    nearby_directories <- nearby_directories[
      dir.exists(nearby_directories)
    ]
    
    nearby_matches <- nearby_directories[
      normalize_directory_name(
        basename(nearby_directories)
      ) == target_normalized
    ]
    
    if (length(nearby_matches) > 0) {
      return(
        normalizePath(
          nearby_matches[1],
          winslash = "/",
          mustWork = TRUE
        )
      )
    }
  }
  
  if (
    interactive() &&
    .Platform$OS.type == "windows"
  ) {
    selected_directory <- utils::choose.dir(
      caption = paste(
        "Select the folder:",
        target_directory_name
      )
    )
    
    if (
      length(selected_directory) == 1 &&
      !is.na(selected_directory) &&
      nzchar(selected_directory) &&
      dir.exists(selected_directory)
    ) {
      return(
        normalizePath(
          selected_directory,
          winslash = "/",
          mustWork = TRUE
        )
      )
    }
  }
  
  stop(
    "The analysis directory could not be located. ",
    "Save this script inside the folder '",
    target_directory_name,
    "' and run it again."
  )
}

locate_input_file <- function(
    analysis_directory
) {
  parent_directory <- dirname(
    analysis_directory
  )

  search_directories <- unique(
    c(
      analysis_directory,
      file.path(
        analysis_directory,
        "data"
      ),
      file.path(
        parent_directory,
        "data"
      )
    )
  )

  file_candidates <- list.files(
    path = search_directories,
    pattern = "^dados\\.csv$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  
  file_candidates <- unique(
    file_candidates[
      file.exists(file_candidates)
    ]
  )
  
  if (length(file_candidates) == 0) {
    if (interactive()) {
      message(
        "The dataset was not found automatically. ",
        "Select the dados.csv file."
      )
      
      selected_file <- tryCatch(
        file.choose(),
        error = function(e) ""
      )
      
      if (
        length(selected_file) == 1 &&
        nzchar(selected_file) &&
        file.exists(selected_file) &&
        tolower(
          tools::file_ext(selected_file)
        ) == "csv"
      ) {
        return(
          normalizePath(
            selected_file,
            winslash = "/",
            mustWork = TRUE
          )
        )
      }
    }
    
    stop(
      "No CSV file matching 'dados.csv' was found inside:\n",
      paste(search_directories, collapse = "\n")
    )
  }
  
  file_information <- file.info(
    file_candidates
  )
  
  modification_times <- file_information$mtime
  
  modification_times[
    is.na(modification_times)
  ] <- as.POSIXct(
    "1970-01-01",
    tz = "UTC"
  )
  
  newest_position <- which.max(
    modification_times
  )
  
  normalizePath(
    file_candidates[newest_position],
    winslash = "/",
    mustWork = TRUE
  )
}

analysis_directory <- locate_analysis_directory()

input_file <- locate_input_file(
  analysis_directory
)

results_directory <- file.path(
  analysis_directory,
  "output"
)

dir.create(
  results_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

cat(
  "ANALYSIS DIRECTORY:\n",
  analysis_directory,
  "\n\n",
  sep = ""
)

cat(
  "INPUT FILE USED:\n",
  input_file,
  "\n\n",
  sep = ""
)

cat(
  "RESULTS DIRECTORY:\n",
  results_directory,
  "\n\n",
  sep = ""
)

clean_text <- function(x) {
  x <- as.character(x)
  stringr::str_squish(x)
}

normalize_text <- function(x) {
  x <- as.character(x)
  
  x <- iconv(
    x,
    from = "",
    to = "ASCII//TRANSLIT"
  )
  
  tolower(
    stringr::str_squish(x)
  )
}

optional_character_column <- function(
    data,
    column_name
) {
  if (column_name %in% names(data)) {
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

optional_integer_column <- function(
    data,
    column_name
) {
  if (column_name %in% names(data)) {
    return(
      suppressWarnings(
        as.integer(
          data[[column_name]]
        )
      )
    )
  }
  
  rep(
    NA_integer_,
    nrow(data)
  )
}

pairwise_matrix_to_data_frame <- function(
    pairwise_object,
    metric_name
) {
  p_value_matrix <- pairwise_object$p.value
  
  if (
    is.null(p_value_matrix) ||
    length(p_value_matrix) == 0
  ) {
    return(
      tibble::tibble(
        metric = character(0),
        group_1 = character(0),
        group_2 = character(0),
        p_value = numeric(0)
      )
    )
  }
  
  result <- as.data.frame(
    as.table(
      p_value_matrix
    ),
    stringsAsFactors = FALSE
  )
  
  names(result) <- c(
    "group_1",
    "group_2",
    "p_value"
  )
  
  result |>
    dplyr::filter(
      !is.na(p_value)
    ) |>
    dplyr::mutate(
      metric = metric_name
    ) |>
    dplyr::relocate(
      metric
    )
}

pairwise_permanova <- function(
    distance_object,
    groups,
    permutations = 9999
) {
  groups <- factor(
    groups
  )
  
  group_levels <- levels(
    groups
  )
  
  if (length(group_levels) < 2) {
    stop(
      "At least two groups are required for pairwise PERMANOVA."
    )
  }
  
  group_combinations <- combn(
    group_levels,
    2,
    simplify = FALSE
  )
  
  results <- lapply(
    group_combinations,
    function(combination) {
      selected_rows <- groups %in%
        combination
      
      distance_matrix <- as.matrix(
        distance_object
      )
      
      subset_distance <- stats::as.dist(
        distance_matrix[
          selected_rows,
          selected_rows,
          drop = FALSE
        ]
      )
      
      subset_groups <- droplevels(
        groups[
          selected_rows
        ]
      )
      
      permanova_result <- vegan::adonis2(
        subset_distance ~ subset_groups,
        data = data.frame(
          subset_groups = subset_groups
        ),
        permutations = permutations
      )
      
      tibble::tibble(
        group_1 = combination[1],
        group_2 = combination[2],
        F_model = permanova_result$F[1],
        R2 = permanova_result$R2[1],
        p_value = permanova_result$`Pr(>F)`[1]
      )
    }
  )
  
  dplyr::bind_rows(
    results
  ) |>
    dplyr::mutate(
      p_adj = stats::p.adjust(
        p_value,
        method = "BH"
      )
    )
}

format_p_value <- function(
    x,
    digits = 4
) {
  if (
    length(x) == 0 ||
    is.na(x)
  ) {
    return(
      "NA"
    )
  }
  
  formatC(
    x,
    format = "f",
    digits = digits
  )
}

save_plot <- function(
    plot_object,
    filename_base,
    width,
    height
) {
  png_file <- file.path(
    results_directory,
    paste0(
      filename_base,
      ".png"
    )
  )
  
  pdf_file <- file.path(
    results_directory,
    paste0(
      filename_base,
      ".pdf"
    )
  )
  
  ggplot2::ggsave(
    filename = png_file,
    plot = plot_object,
    width = width,
    height = height,
    units = "in",
    dpi = 600,
    bg = "white"
  )
  
  pdf_device <- if (
    capabilities("cairo")
  ) {
    grDevices::cairo_pdf
  } else {
    grDevices::pdf
  }
  
  ggplot2::ggsave(
    filename = pdf_file,
    plot = plot_object,
    width = width,
    height = height,
    units = "in",
    device = pdf_device,
    bg = "white"
  )
}

raw_data <- readr::read_delim(
  file = input_file,
  delim = ";",
  locale = readr::locale(
    encoding = "UTF-8"
  ),
  show_col_types = FALSE,
  trim_ws = TRUE
) |>
  janitor::clean_names()

cat(
  "COLUMN NAMES:\n"
)

print(
  names(raw_data)
)

cat(
  "\n"
)

required_columns <- c(
  "species",
  "point",
  "trap"
)

missing_columns <- setdiff(
  required_columns,
  names(raw_data)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(
      missing_columns,
      collapse = ", "
    )
  )
}

collection_values <- optional_character_column(
  raw_data,
  "collection"
)

landscape_values <- optional_character_column(
  raw_data,
  "landscape"
)

classification_values <- optional_character_column(
  raw_data,
  "classification"
)

bait_values <- optional_character_column(
  raw_data,
  "baits"
)

date_values <- optional_character_column(
  raw_data,
  "date"
)

latitude_values <- optional_character_column(
  raw_data,
  "lati_s"
)

longitude_values <- optional_character_column(
  raw_data,
  "long_w"
)

year_values <- optional_integer_column(
  raw_data,
  "year"
)

month_values <- optional_integer_column(
  raw_data,
  "month"
)

data_clean <- raw_data |>
  dplyr::mutate(
    species = clean_text(
      species
    ),
    point = clean_text(
      point
    ),
    trap = clean_text(
      trap
    ),
    trap_lower = normalize_text(
      trap
    ),
    collection = normalize_text(
      collection_values
    ),
    landscape = normalize_text(
      landscape_values
    ),
    classification = clean_text(
      classification_values
    ),
    baits = clean_text(
      bait_values
    ),
    date = clean_text(
      date_values
    ),
    date_standardized = dplyr::if_else(
      is.na(date) |
        date == "",
      "NO_DATE",
      date
    ),
    year = year_values,
    month = month_values,
    lati_s = clean_text(
      latitude_values
    ),
    long_w = clean_text(
      longitude_values
    ),
    trap_standardized = dplyr::case_when(
      stringr::str_detect(
        trap_lower,
        "susp"
      ) ~ "suspension",
      stringr::str_detect(
        trap_lower,
        "puc"
      ) ~ "puca",
      stringr::str_detect(
        trap_lower,
        "mala"
      ) ~ "malaise",
      TRUE ~ trap_lower
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
        "baia negra"
      ) ~ "Pantanal",
      stringr::str_detect(
        normalize_text(point),
        "bodoquena"
      ) ~ "Cerrado",
      stringr::str_detect(
        normalize_text(point),
        "pevri|ivinhema"
      ) ~ "Floodplain",
      TRUE ~ NA_character_
    ),
    area_name = dplyr::case_when(
      landscape_class == "Pantanal" ~ "Baia Negra",
      landscape_class == "Cerrado" ~ "Serra da Bodoquena",
      landscape_class == "Floodplain" ~ "PEVRI",
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
      TRUE ~ paste(
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

occasional_rows <- data_clean |>
  dplyr::filter(
    trap_standardized %in% c(
      "puca",
      "malaise"
    )
  )

if (nrow(suspension_rows) == 0) {
  stop(
    "No suspension records were found."
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

occasional_records <- occasional_rows |>
  dplyr::distinct(
    point_id,
    landscape_class,
    area_name,
    trap_standardized,
    date_standardized,
    species,
    .keep_all = TRUE
  )

required_landscapes <- c(
  "Cerrado",
  "Floodplain",
  "Pantanal"
)

missing_landscapes <- setdiff(
  required_landscapes,
  unique(
    suspension_records$landscape_class
  )
)

if (length(missing_landscapes) > 0) {
  stop(
    "The following landscapes were not represented in the ",
    "suspension-trap dataset: ",
    paste(
      missing_landscapes,
      collapse = ", "
    ),
    "."
  )
}

cat(
  "DIAGNOSTICS\n"
)

cat(
  "Rows in raw file:                         ",
  nrow(raw_data),
  "\n"
)

cat(
  "Rows after cleaning:                      ",
  nrow(data_clean),
  "\n"
)

cat(
  "Raw suspension rows:                      ",
  nrow(suspension_rows),
  "\n"
)

cat(
  "Deduplicated suspension records:          ",
  nrow(suspension_records),
  "\n"
)

cat(
  "Raw puca + malaise rows:                  ",
  nrow(occasional_rows),
  "\n"
)

cat(
  "Deduplicated puca + malaise records:      ",
  nrow(occasional_records),
  "\n"
)

cat(
  "Landscapes in suspension:                 ",
  paste(
    sort(
      unique(
        suspension_records$landscape_class
      )
    ),
    collapse = "; "
  ),
  "\n"
)

cat(
  "Species in suspension:                    ",
  dplyr::n_distinct(
    suspension_records$species
  ),
  "\n"
)

cat(
  "Sampling units (point_id):                ",
  dplyr::n_distinct(
    suspension_records$point_id
  ),
  "\n"
)

cat(
  "Sampling events (point_id + date):        ",
  dplyr::n_distinct(
    suspension_records$event_id
  ),
  "\n\n"
)

summary_landscape_suspension <- suspension_records |>
  dplyr::group_by(
    landscape_class,
    area_name
  ) |>
  dplyr::summarise(
    n_records = dplyr::n(),
    n_species = dplyr::n_distinct(
      species
    ),
    n_points = dplyr::n_distinct(
      point_id
    ),
    n_events = dplyr::n_distinct(
      event_id
    ),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    match(
      landscape_class,
      required_landscapes
    )
  )

summary_landscape_occasional <- occasional_records |>
  dplyr::group_by(
    landscape_class,
    area_name,
    trap_standardized
  ) |>
  dplyr::summarise(
    n_records = dplyr::n(),
    n_species = dplyr::n_distinct(
      species
    ),
    n_points = dplyr::n_distinct(
      point_id
    ),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    match(
      landscape_class,
      required_landscapes
    ),
    trap_standardized
  )

point_species_frequency <- suspension_records |>
  dplyr::count(
    point_id,
    landscape_class,
    area_name,
    species,
    name = "record_frequency"
  ) |>
  tidyr::pivot_wider(
    names_from = species,
    values_from = record_frequency,
    values_fill = 0
  ) |>
  dplyr::arrange(
    match(
      landscape_class,
      required_landscapes
    ),
    point_id
  )

species_columns <- setdiff(
  names(point_species_frequency),
  c(
    "point_id",
    "landscape_class",
    "area_name"
  )
)

if (length(species_columns) < 2) {
  stop(
    "At least two species are required for the analyses."
  )
}

point_species_presence <- point_species_frequency

point_species_presence[
  species_columns
] <- lapply(
  point_species_presence[
    species_columns
  ],
  function(x) {
    ifelse(
      x > 0,
      1,
      0
    )
  }
)

point_frequency_matrix <- point_species_frequency |>
  dplyr::select(
    dplyr::all_of(
      species_columns
    )
  ) |>
  as.data.frame()

rownames(
  point_frequency_matrix
) <- point_species_frequency$point_id

point_presence_matrix <- point_species_presence |>
  dplyr::select(
    dplyr::all_of(
      species_columns
    )
  ) |>
  as.data.frame()

rownames(
  point_presence_matrix
) <- point_species_presence$point_id

keep_points <- rowSums(
  point_presence_matrix
) > 0

point_frequency_matrix <- point_frequency_matrix[
  keep_points,
  ,
  drop = FALSE
]

point_presence_matrix <- point_presence_matrix[
  keep_points,
  ,
  drop = FALSE
]

point_species_frequency <- point_species_frequency[
  keep_points,
  ,
  drop = FALSE
]

point_species_presence <- point_species_presence[
  keep_points,
  ,
  drop = FALSE
]

keep_species <- colSums(
  point_presence_matrix
) > 0

point_frequency_matrix <- point_frequency_matrix[
  ,
  keep_species,
  drop = FALSE
]

point_presence_matrix <- point_presence_matrix[
  ,
  keep_species,
  drop = FALSE
]

group_factor_point <- factor(
  point_species_frequency$landscape_class,
  levels = required_landscapes
)

points_by_landscape <- table(
  group_factor_point
)

if (
  any(
    points_by_landscape < 2
  )
) {
  warning(
    "At least one landscape has fewer than two sampling points. ",
    "Some inferential results may be unstable."
  )
}

alpha_by_point <- tibble::tibble(
  point_id = point_species_frequency$point_id,
  area_name = point_species_frequency$area_name,
  landscape_class = point_species_frequency$landscape_class,
  record_frequency = rowSums(
    point_frequency_matrix
  ),
  richness = rowSums(
    point_frequency_matrix > 0
  ),
  shannon = vegan::diversity(
    point_frequency_matrix,
    index = "shannon"
  )
) |>
  dplyr::arrange(
    match(
      landscape_class,
      required_landscapes
    ),
    point_id
  ) |>
  dplyr::mutate(
    landscape_class = factor(
      landscape_class,
      levels = required_landscapes
    )
  )

kruskal_record_frequency <- stats::kruskal.test(
  record_frequency ~ landscape_class,
  data = alpha_by_point
)

kruskal_richness <- stats::kruskal.test(
  richness ~ landscape_class,
  data = alpha_by_point
)

kruskal_shannon <- stats::kruskal.test(
  shannon ~ landscape_class,
  data = alpha_by_point
)

kruskal_results <- tibble::tibble(
  metric = c(
    "record_frequency",
    "richness",
    "shannon"
  ),
  statistic = c(
    unname(
      kruskal_record_frequency$statistic
    ),
    unname(
      kruskal_richness$statistic
    ),
    unname(
      kruskal_shannon$statistic
    )
  ),
  df = c(
    unname(
      kruskal_record_frequency$parameter
    ),
    unname(
      kruskal_richness$parameter
    ),
    unname(
      kruskal_shannon$parameter
    )
  ),
  p_value = c(
    kruskal_record_frequency$p.value,
    kruskal_richness$p.value,
    kruskal_shannon$p.value
  )
)

pairwise_record_frequency <- stats::pairwise.wilcox.test(
  x = alpha_by_point$record_frequency,
  g = alpha_by_point$landscape_class,
  p.adjust.method = "BH",
  exact = FALSE
)

pairwise_richness <- stats::pairwise.wilcox.test(
  x = alpha_by_point$richness,
  g = alpha_by_point$landscape_class,
  p.adjust.method = "BH",
  exact = FALSE
)

pairwise_shannon <- stats::pairwise.wilcox.test(
  x = alpha_by_point$shannon,
  g = alpha_by_point$landscape_class,
  p.adjust.method = "BH",
  exact = FALSE
)

pairwise_wilcoxon_results <- dplyr::bind_rows(
  pairwise_matrix_to_data_frame(
    pairwise_record_frequency,
    "record_frequency"
  ),
  pairwise_matrix_to_data_frame(
    pairwise_richness,
    "richness"
  ),
  pairwise_matrix_to_data_frame(
    pairwise_shannon,
    "shannon"
  )
)

set.seed(123)

jaccard_distance <- vegan::vegdist(
  point_presence_matrix,
  method = "jaccard",
  binary = TRUE
)

set.seed(123)

permanova_point <- vegan::adonis2(
  jaccard_distance ~ landscape_class,
  data = data.frame(
    landscape_class = group_factor_point
  ),
  permutations = 9999
)

permanova_point_results <- as.data.frame(
  permanova_point
) |>
  tibble::rownames_to_column(
    "term"
  )

set.seed(123)

pairwise_permanova_results <- pairwise_permanova(
  distance_object = jaccard_distance,
  groups = group_factor_point,
  permutations = 9999
)

beta_dispersion_point <- vegan::betadisper(
  jaccard_distance,
  group_factor_point
)

betadisper_anova_results <- as.data.frame(
  stats::anova(
    beta_dispersion_point
  )
) |>
  tibble::rownames_to_column(
    "term"
  )

set.seed(123)

betadisper_permutest <- vegan::permutest(
  beta_dispersion_point,
  permutations = 9999
)

betadisper_permutest_results <- as.data.frame(
  betadisper_permutest$tab
) |>
  tibble::rownames_to_column(
    "term"
  )

betadisper_scores <- tibble::tibble(
  point_id = names(
    beta_dispersion_point$distances
  ),
  distance_to_centroid = as.numeric(
    beta_dispersion_point$distances
  )
) |>
  dplyr::left_join(
    point_species_frequency |>
      dplyr::select(
        point_id,
        landscape_class
      ),
    by = "point_id"
  ) |>
  dplyr::select(
    point_id,
    landscape_class,
    distance_to_centroid
  )

set.seed(123)

indicator_result <- indicspecies::multipatt(
  x = point_frequency_matrix,
  cluster = group_factor_point,
  func = "IndVal.g",
  duleg = TRUE,
  control = permute::how(
    nperm = 9999
  )
)

indicator_results <- indicator_result$sign |>
  as.data.frame() |>
  tibble::rownames_to_column(
    "species"
  ) |>
  dplyr::rename(
    index_stat = stat,
    p_value = p.value
  ) |>
  dplyr::mutate(
    landscape_class = dplyr::case_when(
      s.Cerrado == 1 ~ "Cerrado",
      s.Floodplain == 1 ~ "Floodplain",
      s.Pantanal == 1 ~ "Pantanal",
      TRUE ~ "Combination"
    )
  ) |>
  dplyr::arrange(
    p_value,
    dplyr::desc(
      index_stat
    ),
    species
  )

indicator_significant <- indicator_results |>
  dplyr::filter(
    p_value <= 0.05
  )

species_landscape_frequency <- suspension_records |>
  dplyr::count(
    species,
    landscape_class,
    name = "record_frequency"
  ) |>
  tidyr::pivot_wider(
    names_from = landscape_class,
    values_from = record_frequency,
    values_fill = 0
  )

for (landscape_name in required_landscapes) {
  if (
    !landscape_name %in%
    names(species_landscape_frequency)
  ) {
    species_landscape_frequency[
      landscape_name
    ] <- 0
  }
}

species_landscape_frequency <- species_landscape_frequency |>
  dplyr::select(
    species,
    Cerrado,
    Floodplain,
    Pantanal
  ) |>
  dplyr::mutate(
    total = Cerrado +
      Floodplain +
      Pantanal
  ) |>
  dplyr::arrange(
    dplyr::desc(
      total
    ),
    species
  )

species_landscape_presence <- species_landscape_frequency |>
  dplyr::mutate(
    Cerrado = ifelse(
      Cerrado > 0,
      1,
      0
    ),
    Floodplain = ifelse(
      Floodplain > 0,
      1,
      0
    ),
    Pantanal = ifelse(
      Pantanal > 0,
      1,
      0
    ),
    total = Cerrado +
      Floodplain +
      Pantanal
  )

landscape_colors <- c(
  "Cerrado" = "#B9770E",
  "Floodplain" = "#2F80ED",
  "Pantanal" = "#2E8B57"
)

base_theme <- ggplot2::theme_classic(
  base_size = 13
) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold",
      hjust = 0.5,
      size = 15
    ),
    plot.subtitle = ggplot2::element_text(
      hjust = 0.5,
      size = 11
    ),
    axis.title = ggplot2::element_text(
      face = "bold"
    ),
    legend.title = ggplot2::element_text(
      face = "bold"
    ),
    legend.position = "right",
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(
      face = "bold"
    )
  )

get_kruskal_p <- function(
    metric_name
) {
  result <- kruskal_results |>
    dplyr::filter(
      metric == metric_name
    ) |>
    dplyr::pull(
      p_value
    )
  
  if (length(result) == 0) {
    return(
      NA_real_
    )
  }
  
  result[1]
}

make_halfeye_plot <- function(
    data,
    y_variable,
    y_label,
    title_text,
    metric_name
) {
  p_value <- get_kruskal_p(
    metric_name
  )
  
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = landscape_class,
      y = .data[[y_variable]],
      fill = landscape_class,
      color = landscape_class
    )
  ) +
    ggdist::stat_halfeye(
      adjust = 0.7,
      width = 0.55,
      .width = 0,
      justification = -0.25,
      point_colour = NA,
      alpha = 0.40
    ) +
    ggplot2::geom_boxplot(
      width = 0.18,
      outlier.shape = NA,
      alpha = 0.55,
      color = "grey20",
      linewidth = 0.7
    ) +
    ggplot2::geom_point(
      position = ggplot2::position_jitter(
        width = 0.05,
        height = 0
      ),
      size = 2.1,
      alpha = 0.70,
      color = "grey10"
    ) +
    ggplot2::scale_fill_manual(
      values = landscape_colors
    ) +
    ggplot2::scale_color_manual(
      values = landscape_colors
    ) +
    ggplot2::labs(
      title = title_text,
      subtitle = paste0(
        "Kruskal-Wallis p = ",
        format_p_value(
          p_value,
          4
        )
      ),
      x = NULL,
      y = y_label
    ) +
    base_theme +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(
        face = "bold"
      )
    )
}

richness_plot <- make_halfeye_plot(
  data = alpha_by_point,
  y_variable = "richness",
  y_label = "Richness",
  title_text = "Species richness",
  metric_name = "richness"
)

record_frequency_plot <- make_halfeye_plot(
  data = alpha_by_point,
  y_variable = "record_frequency",
  y_label = "Record frequency",
  title_text = "Record frequency",
  metric_name = "record_frequency"
)

shannon_plot <- make_halfeye_plot(
  data = alpha_by_point,
  y_variable = "shannon",
  y_label = "Shannon index",
  title_text = "Shannon diversity",
  metric_name = "shannon"
)

alpha_panel <- (
  richness_plot |
    record_frequency_plot |
    shannon_plot
) +
  patchwork::plot_annotation(
    title = "Alpha diversity metrics by landscape"
  )

composition_bubble <- suspension_records |>
  dplyr::count(
    landscape_class,
    species,
    name = "record_frequency"
  ) |>
  dplyr::group_by(
    landscape_class
  ) |>
  dplyr::mutate(
    relative_frequency =
      record_frequency /
      sum(record_frequency)
  ) |>
  dplyr::ungroup()

species_order_bubble <- composition_bubble |>
  dplyr::group_by(
    species
  ) |>
  dplyr::summarise(
    total_records = sum(
      record_frequency
    ),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    total_records,
    species
  ) |>
  dplyr::pull(
    species
  )

composition_bubble <- composition_bubble |>
  dplyr::mutate(
    landscape_class = factor(
      landscape_class,
      levels = required_landscapes
    ),
    species = factor(
      species,
      levels = species_order_bubble
    )
  )

bubble_plot <- ggplot2::ggplot(
  composition_bubble,
  ggplot2::aes(
    x = landscape_class,
    y = species,
    size = relative_frequency,
    fill = landscape_class
  )
) +
  ggplot2::geom_point(
    shape = 21,
    color = "grey20",
    alpha = 0.92,
    stroke = 0.35
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = ifelse(
        record_frequency >= 10,
        record_frequency,
        ""
      )
    ),
    color = "black",
    size = 3
  ) +
  ggplot2::scale_fill_manual(
    values = landscape_colors,
    name = "Landscape"
  ) +
  ggplot2::scale_size_area(
    max_size = 18,
    labels = scales::label_percent(
      accuracy = 1
    ),
    name = "Relative record frequency"
  ) +
  ggplot2::scale_y_discrete(
    expand = ggplot2::expansion(
      mult = c(
        0.02,
        0.20
      )
    )
  ) +
  ggplot2::scale_x_discrete(
    expand = ggplot2::expansion(
      mult = c(
        0.10,
        0.10
      )
    )
  ) +
  ggplot2::labs(
    title = "Species composition across landscapes",
    subtitle = paste0(
      "Bubble size = relative record frequency | ",
      "Labels shown for n ≥ 10"
    ),
    x = NULL,
    y = NULL
  ) +
  base_theme +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      face = "bold"
    ),
    axis.text.y = ggplot2::element_text(
      face = "italic"
    ),
    legend.position = "right",
    plot.title = ggplot2::element_text(
      face = "bold",
      hjust = 0.5,
      size = 15,
      margin = ggplot2::margin(
        t = 4,
        b = 14
      )
    ),
    plot.subtitle = ggplot2::element_text(
      hjust = 0.5,
      size = 11,
      margin = ggplot2::margin(
        b = 14
      )
    ),
    plot.margin = ggplot2::margin(
      t = 34,
      r = 10,
      b = 10,
      l = 10
    )
  ) +
  ggplot2::coord_cartesian(
    clip = "off"
  )

indicator_plot_data <- indicator_significant |>
  dplyr::filter(
    landscape_class %in%
      required_landscapes
  )

if (nrow(indicator_plot_data) > 0) {
  indicator_plot_data <- indicator_plot_data |>
    dplyr::mutate(
      species = forcats::fct_reorder(
        species,
        index_stat
      ),
      p_label = paste0(
        "p = ",
        formatC(
          p_value,
          format = "f",
          digits = 3
        )
      )
    )
  
  indicator_plot <- ggplot2::ggplot(
    indicator_plot_data,
    ggplot2::aes(
      x = index_stat,
      y = species,
      color = landscape_class
    )
  ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 0,
        xend = index_stat,
        y = species,
        yend = species
      ),
      linewidth = 0.8,
      alpha = 0.7
    ) +
    ggplot2::geom_point(
      size = 3
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = p_label
      ),
      hjust = -0.15,
      size = 3.1,
      color = "grey20"
    ) +
    ggplot2::scale_color_manual(
      values = landscape_colors,
      name = "Landscape"
    ) +
    ggplot2::labs(
      title = "Indicator species by landscape",
      subtitle = "IndVal.g | Significant species only",
      x = "Indicator value",
      y = NULL,
      color = "Landscape"
    ) +
    base_theme +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(
        face = "italic"
      ),
      legend.position = "top"
    ) +
    ggplot2::coord_cartesian(
      xlim = c(
        0,
        max(
          indicator_plot_data$index_stat,
          na.rm = TRUE
        ) * 1.25
      )
    )
  
} else {
  indicator_plot <- ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 1,
      y = 1,
      label = "No significant indicator species",
      size = 6
    ) +
    ggplot2::theme_void() +
    ggplot2::labs(
      title = "Indicator species by landscape",
      subtitle = "IndVal.g | Significant species only"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        hjust = 0.5
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5
      )
    )
}

print(
  alpha_panel
)

print(
  bubble_plot
)

print(
  indicator_plot
)

save_plot(
  plot_object = alpha_panel,
  filename_base =
    "01_alpha_diversity_panel_suspension_pointlevel",
  width = 14,
  height = 5.5
)

save_plot(
  plot_object = bubble_plot,
  filename_base =
    "02_species_composition_bubble_suspension_pointlevel",
  width = 10,
  height = 8.5
)

save_plot(
  plot_object = indicator_plot,
  filename_base =
    "03_indicator_species_lollipop_suspension_pointlevel",
  width = 9,
  height = 6.5
)

readr::write_excel_csv2(
  suspension_rows,
  file.path(
    results_directory,
    "01_suspension_rows_cleaned.csv"
  )
)

readr::write_excel_csv2(
  suspension_records,
  file.path(
    results_directory,
    "02_suspension_records_deduplicated.csv"
  )
)

readr::write_excel_csv2(
  occasional_rows,
  file.path(
    results_directory,
    "03_occasional_rows_cleaned.csv"
  )
)

readr::write_excel_csv2(
  occasional_records,
  file.path(
    results_directory,
    "04_occasional_records_deduplicated.csv"
  )
)

readr::write_excel_csv2(
  summary_landscape_suspension,
  file.path(
    results_directory,
    "05_summary_landscape_suspension.csv"
  )
)

readr::write_excel_csv2(
  summary_landscape_occasional,
  file.path(
    results_directory,
    "06_summary_landscape_occasional.csv"
  )
)

readr::write_excel_csv2(
  alpha_by_point,
  file.path(
    results_directory,
    "07_alpha_by_point.csv"
  )
)

readr::write_excel_csv2(
  point_species_frequency,
  file.path(
    results_directory,
    "08_point_species_record_frequency_suspension.csv"
  )
)

readr::write_excel_csv2(
  point_species_presence,
  file.path(
    results_directory,
    "09_point_species_presence_suspension.csv"
  )
)

readr::write_excel_csv2(
  kruskal_results,
  file.path(
    results_directory,
    "10_kruskal_alpha.csv"
  )
)

readr::write_excel_csv2(
  pairwise_wilcoxon_results,
  file.path(
    results_directory,
    "11_pairwise_wilcox_alpha.csv"
  )
)

readr::write_excel_csv2(
  permanova_point_results,
  file.path(
    results_directory,
    "12_permanova_jaccard_point.csv"
  )
)

readr::write_excel_csv2(
  pairwise_permanova_results,
  file.path(
    results_directory,
    "13_pairwise_permanova_point.csv"
  )
)

readr::write_excel_csv2(
  betadisper_anova_results,
  file.path(
    results_directory,
    "14_betadisper_point_anova.csv"
  )
)

readr::write_excel_csv2(
  betadisper_permutest_results,
  file.path(
    results_directory,
    "15_betadisper_point_permutest.csv"
  )
)

readr::write_excel_csv2(
  betadisper_scores,
  file.path(
    results_directory,
    "16_betadisper_point_scores.csv"
  )
)

readr::write_excel_csv2(
  indicator_results,
  file.path(
    results_directory,
    "17_indval_all.csv"
  )
)

readr::write_excel_csv2(
  indicator_significant,
  file.path(
    results_directory,
    "18_indval_significant.csv"
  )
)

readr::write_excel_csv2(
  species_landscape_frequency,
  file.path(
    results_directory,
    "19_species_landscape_record_frequency_suspension.csv"
  )
)

readr::write_excel_csv2(
  species_landscape_presence,
  file.path(
    results_directory,
    "20_species_landscape_presence_suspension.csv"
  )
)

excel_tables <- list(
  "01_suspension_rows" =
    suspension_rows,
  
  "02_suspension_dedup" =
    suspension_records,
  
  "03_occasional_rows" =
    occasional_rows,
  
  "04_occasional_dedup" =
    occasional_records,
  
  "05_summary_suspension" =
    summary_landscape_suspension,
  
  "06_summary_occasional" =
    summary_landscape_occasional,
  
  "07_alpha_by_point" =
    alpha_by_point,
  
  "08_point_species_frequency" =
    point_species_frequency,
  
  "09_point_species_presence" =
    point_species_presence,
  
  "10_kruskal_alpha" =
    kruskal_results,
  
  "11_pairwise_wilcox" =
    pairwise_wilcoxon_results,
  
  "12_permanova_point" =
    permanova_point_results,
  
  "13_pairwise_permanova" =
    pairwise_permanova_results,
  
  "14_betadisper_anova" =
    betadisper_anova_results,
  
  "15_betadisper_permutest" =
    betadisper_permutest_results,
  
  "16_betadisper_scores" =
    betadisper_scores,
  
  "17_indval_all" =
    indicator_results,
  
  "18_indval_significant" =
    indicator_significant,
  
  "19_species_landscape_freq" =
    species_landscape_frequency,
  
  "20_species_landscape_presence" =
    species_landscape_presence
)

excel_output <- file.path(
  results_directory,
  "social_wasps_all_results_pointlevel.xlsx"
)

writexl::write_xlsx(
  x = excel_tables,
  path = excel_output
)

summary_text <- c(
  "SOCIAL WASPS - ANALYSIS SUMMARY",
  "POINT-LEVEL VERSION",
  "",
  paste(
    "Input file:",
    input_file
  ),
  paste(
    "Rows in raw file:",
    nrow(raw_data)
  ),
  paste(
    "Rows after cleaning:",
    nrow(data_clean)
  ),
  paste(
    "Raw suspension rows:",
    nrow(suspension_rows)
  ),
  paste(
    "Deduplicated suspension records:",
    nrow(suspension_records)
  ),
  paste(
    "Sampling units (point_id):",
    dplyr::n_distinct(
      suspension_records$point_id
    )
  ),
  paste(
    "Sampling events (point_id + date):",
    dplyr::n_distinct(
      suspension_records$event_id
    )
  ),
  paste(
    "Kruskal record frequency p-value:",
    kruskal_results$p_value[
      kruskal_results$metric ==
        "record_frequency"
    ]
  ),
  paste(
    "Kruskal richness p-value:",
    kruskal_results$p_value[
      kruskal_results$metric ==
        "richness"
    ]
  ),
  paste(
    "Kruskal Shannon p-value:",
    kruskal_results$p_value[
      kruskal_results$metric ==
        "shannon"
    ]
  ),
  paste(
    "PERMANOVA point-level p-value:",
    permanova_point_results$`Pr(>F)`[1]
  ),
  paste(
    "PERMANOVA point-level R2:",
    permanova_point_results$R2[1]
  ),
  paste(
    "Betadisper point permutest p-value:",
    betadisper_permutest_results$`Pr(>F)`[1]
  ),
  paste(
    "Significant indicator species:",
    nrow(
      indicator_significant
    )
  ),
  "",
  "INTERPRETATION NOTE:",
  paste(
    "This version uses point_id as the inferential unit."
  ),
  paste(
    "Because each landscape is represented by one area,",
    "landscape and area remain confounded."
  )
)

summary_output <- file.path(
  results_directory,
  "00_analysis_summary_pointlevel.txt"
)

writeLines(
  summary_text,
  summary_output,
  useBytes = TRUE
)

session_information <- capture.output(
  sessionInfo()
)

session_output <- file.path(
  results_directory,
  "99_session_information.txt"
)

writeLines(
  session_information,
  session_output,
  useBytes = TRUE
)

alpha_summary <- alpha_by_point |>
  dplyr::group_by(
    landscape_class
  ) |>
  dplyr::summarise(
    n_points = dplyr::n(),
    mean_record_frequency = mean(
      record_frequency
    ),
    median_record_frequency = stats::median(
      record_frequency
    ),
    mean_richness = mean(
      richness
    ),
    median_richness = stats::median(
      richness
    ),
    mean_shannon = mean(
      shannon
    ),
    median_shannon = stats::median(
      shannon
    ),
    .groups = "drop"
  )

cat(
  "\nBASIC INFERENTIAL DIAGNOSTICS\n"
)

cat(
  "Number of sampling points:",
  dplyr::n_distinct(
    suspension_records$point_id
  ),
  "\n\n"
)

print(
  summary_landscape_suspension
)

cat(
  "\nALPHA METRICS\n"
)

print(
  kruskal_results
)

print(
  pairwise_wilcoxon_results
)

print(
  alpha_summary
)

cat(
  "\nSPECIES COMPOSITION\n"
)

print(
  permanova_point_results
)

print(
  pairwise_permanova_results
)

print(
  betadisper_permutest_results
)

cat(
  "\nINDICATOR SPECIES\n"
)

print(
  indicator_significant
)

cat(
  "\nAUTOMATIC SUMMARY\n\n"
)

cat(
  readLines(
    summary_output,
    warn = FALSE
  ),
  sep = "\n"
)

cat(
  "\n\nANALYSIS COMPLETED.\n\n"
)

cat(
  "Results saved in:\n",
  normalizePath(
    results_directory,
    winslash = "/",
    mustWork = TRUE
  ),
  "\n\n",
  sep = ""
)

cat(
  "Main outputs:\n"
)

cat(
  "- social_wasps_all_results_pointlevel.xlsx\n"
)

cat(
  "- 01 to 20 CSV files\n"
)

cat(
  "- 01_alpha_diversity_panel_suspension_pointlevel.png / pdf\n"
)

cat(
  "- 02_species_composition_bubble_suspension_pointlevel.png / pdf\n"
)

cat(
  "- 03_indicator_species_lollipop_suspension_pointlevel.png / pdf\n"
)

cat(
  "- 00_analysis_summary_pointlevel.txt\n"
)

cat(
  "- 99_session_information.txt\n"
)