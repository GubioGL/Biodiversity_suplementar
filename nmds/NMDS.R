# NMDS of social wasp assemblages

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
  "ggplot2",
  "ggrepel",
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

if (length(missing_packages) > 0) {
  
  message(
    "Installing missing packages: ",
    paste(
      missing_packages,
      collapse = ", "
    )
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
    paste(
      packages_still_missing,
      collapse = ", "
    ),
    ". Check the internet connection and run the script again."
  )
}

invisible(
  lapply(
    required_packages,
    library,
    character.only = TRUE
  )
)


get_candidate_directories <- function() {
  
  directories <- getwd()
  
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
        as.character(
          frame$ofile
        )
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
  
  if (
    requireNamespace(
      "rstudioapi",
      quietly = TRUE
    ) &&
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
  
  directories <- unique(
    directories[
      dir.exists(directories)
    ]
  )
  
  add_parent_directories <- function(
    directory,
    maximum_levels = 8
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
  
  unique(
    unlist(
      lapply(
        directories,
        add_parent_directories
      ),
      use.names = FALSE
    )
  )
}


locate_data_file <- function() {
  
  candidate_directories <- get_candidate_directories()
  
  csv_files <- character(0)
  
  for (directory in candidate_directories) {
    
    possible_directories <- c(
      file.path(
        directory,
        "data"
      ),
      directory
    )
    
    for (possible_directory in possible_directories) {
      
      if (dir.exists(possible_directory)) {
        
        csv_files <- c(
          csv_files,
          list.files(
            path = possible_directory,
            pattern = "\\.csv$",
            full.names = TRUE,
            recursive = FALSE,
            ignore.case = TRUE
          )
        )
      }
    }
  }
  
  csv_files <- unique(
    csv_files[
      file.exists(csv_files)
    ]
  )
  
  preferred_files <- csv_files[
    grepl(
      "^dados\\.csv$",
      basename(csv_files),
      ignore.case = TRUE
    )
  ]
  
  if (length(preferred_files) == 1) {
    
    return(
      normalizePath(
        preferred_files[1],
        winslash = "/",
        mustWork = TRUE
      )
    )
  }
  
  if (length(csv_files) == 1) {
    
    return(
      normalizePath(
        csv_files[1],
        winslash = "/",
        mustWork = TRUE
      )
    )
  }
  
  if (
    length(csv_files) > 1 &&
    interactive()
  ) {
    
    selected_file <- utils::select.list(
      choices = csv_files,
      preselect = csv_files[1],
      multiple = FALSE,
      title = "Select the occurrence dataset",
      graphics = .Platform$OS.type == "windows"
    )
    
    if (
      length(selected_file) == 1 &&
      nzchar(selected_file)
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
  
  if (!interactive()) {
    
    stop(
      "The occurrence dataset could not be located."
    )
  }
  
  message(
    "The dataset was not found automatically. ",
    "Select the occurrence-data CSV file."
  )
  
  selected_file <- tryCatch(
    file.choose(),
    error = function(e) ""
  )
  
  if (
    length(selected_file) != 1 ||
    !nzchar(selected_file) ||
    !file.exists(selected_file)
  ) {
    
    stop(
      "No valid data file was selected."
    )
  }
  
  if (
    tolower(
      tools::file_ext(selected_file)
    ) != "csv"
  ) {
    
    stop(
      "The selected data file must have the extension .csv."
    )
  }
  
  normalizePath(
    selected_file,
    winslash = "/",
    mustWork = TRUE
  )
}


file_csv <- locate_data_file()

data_directory <- dirname(
  file_csv
)

analysis_directory <- if (
  tolower(
    basename(data_directory)
  ) == "data"
) {
  dirname(
    data_directory
  )
} else {
  data_directory
}

output_directory <- file.path(
  script_directory,
  "output"
)

dir.create(
  output_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

message(
  "Dataset selected: ",
  file_csv
)

message(
  "Analysis directory: ",
  analysis_directory
)

message(
  "Output directory: ",
  output_directory
)


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
  
  tolower(
    trimws(x)
  )
}


recode_landscape <- function(x) {
  
  normalized_value <- normalize_text(
    x
  )
  
  dplyr::case_when(
    normalized_value == "pantanal" ~ "Pantanal",
    normalized_value == "cerrado" ~ "Cerrado",
    normalized_value == "floodplain" ~ "Floodplain",
    normalized_value == "varzea" ~ "Floodplain",
    normalized_value == "várzea" ~ "Floodplain",
    TRUE ~ as.character(x)
  )
}


format_p_value <- function(x) {
  
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
    format = "fg",
    digits = 3
  )
}


format_number <- function(
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
    format = "fg",
    digits = digits
  )
}


save_png_plot <- function(
    plot_object,
    file_path
) {
  
  if (capabilities("cairo")) {
    
    grDevices::png(
      filename = file_path,
      width = 11,
      height = 8.5,
      units = "in",
      res = 1200,
      bg = "white",
      type = "cairo-png"
    )
    
  } else {
    
    grDevices::png(
      filename = file_path,
      width = 11,
      height = 8.5,
      units = "in",
      res = 1200,
      bg = "white"
    )
  }
  
  on.exit(
    grDevices::dev.off(),
    add = TRUE
  )
  
  print(
    plot_object
  )
}


save_tiff_plot <- function(
    plot_object,
    file_path
) {
  
  grDevices::tiff(
    filename = file_path,
    width = 11,
    height = 8.5,
    units = "in",
    res = 1200,
    compression = "lzw",
    bg = "white"
  )
  
  on.exit(
    grDevices::dev.off(),
    add = TRUE
  )
  
  print(
    plot_object
  )
}


save_pdf_plot <- function(
    plot_object,
    file_path
) {
  
  if (capabilities("cairo")) {
    
    grDevices::cairo_pdf(
      filename = file_path,
      width = 11,
      height = 8.5,
      bg = "white"
    )
    
  } else {
    
    grDevices::pdf(
      file = file_path,
      width = 11,
      height = 8.5,
      bg = "white"
    )
  }
  
  on.exit(
    grDevices::dev.off(),
    add = TRUE
  )
  
  print(
    plot_object
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


required_columns <- c(
  "species",
  "landscape",
  "trap",
  "date",
  "lati_s",
  "long_w"
)

missing_columns <- setdiff(
  required_columns,
  names(raw_data)
)

if (length(missing_columns) > 0) {
  
  stop(
    "The following columns were not found in the dataset: ",
    paste(
      missing_columns,
      collapse = ", "
    ),
    "."
  )
}


collection_values <- if (
  "collection" %in%
  names(raw_data)
) {
  
  normalize_text(
    raw_data$collection
  )
  
} else {
  
  rep(
    NA_character_,
    nrow(raw_data)
  )
}


data_filtered <- raw_data |>
  dplyr::mutate(
    species = clean_text(
      species
    ),
    landscape = recode_landscape(
      landscape
    ),
    trap_normalized = normalize_text(
      trap
    ),
    collection_normalized = collection_values,
    date_raw = clean_text(
      date
    ),
    latitude_raw = clean_text(
      lati_s
    ),
    longitude_raw = clean_text(
      long_w
    )
  ) |>
  dplyr::filter(
    !is.na(species),
    species != ""
  ) |>
  dplyr::filter(
    landscape %in% c(
      "Pantanal",
      "Cerrado",
      "Floodplain"
    )
  ) |>
  dplyr::filter(
    stringr::str_detect(
      trap_normalized,
      "susp"
    )
  ) |>
  dplyr::filter(
    is.na(collection_normalized) |
      collection_normalized == "" |
      collection_normalized == "passive"
  ) |>
  dplyr::filter(
    !is.na(latitude_raw),
    latitude_raw != ""
  ) |>
  dplyr::filter(
    !is.na(longitude_raw),
    longitude_raw != ""
  )


if (nrow(data_filtered) == 0) {
  
  stop(
    "No records remained after applying the analytical filters."
  )
}


cat(
  "\nRecords after filtering:\n"
)

print(
  nrow(data_filtered)
)


cat(
  "\nRecords by landscape:\n"
)

print(
  table(
    data_filtered$landscape
  )
)


data_filtered <- data_filtered |>
  dplyr::mutate(
    sampling_unit = paste(
      landscape,
      latitude_raw,
      longitude_raw,
      sep = "_"
    )
  )


cat(
  "\nSampling units before deduplication:\n"
)

print(
  data_filtered |>
    dplyr::distinct(
      landscape,
      sampling_unit
    ) |>
    dplyr::count(
      landscape,
      name = "n_units"
    )
)


data_deduplicated <- data_filtered |>
  dplyr::distinct(
    species,
    date_raw,
    sampling_unit,
    landscape,
    .keep_all = TRUE
  )


cat(
  "\nRecords after deduplication:\n"
)

print(
  nrow(data_deduplicated)
)


cat(
  "\nSampling units after deduplication:\n"
)

print(
  data_deduplicated |>
    dplyr::distinct(
      landscape,
      sampling_unit
    ) |>
    dplyr::count(
      landscape,
      name = "n_units"
    )
)


cat(
  "\nSpecies by landscape:\n"
)

print(
  data_deduplicated |>
    dplyr::distinct(
      landscape,
      species
    ) |>
    dplyr::count(
      landscape,
      name = "n_species"
    )
)


community_data <- data_deduplicated |>
  dplyr::mutate(
    presence = 1
  ) |>
  dplyr::distinct(
    landscape,
    sampling_unit,
    species,
    presence
  ) |>
  tidyr::pivot_wider(
    names_from = species,
    values_from = presence,
    values_fill = 0
  )


metadata <- community_data |>
  dplyr::select(
    landscape,
    sampling_unit
  )


community_matrix <- community_data |>
  dplyr::select(
    -landscape,
    -sampling_unit
  ) |>
  as.data.frame()


rownames(community_matrix) <- metadata$sampling_unit


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
  "\nFinal community-matrix dimensions:\n"
)

cat(
  nrow(community_matrix),
  "sampling units x",
  ncol(community_matrix),
  "species\n\n"
)


if (nrow(community_matrix) < 4) {
  
  stop(
    "At least four sampling units are required for this NMDS."
  )
}


if (ncol(community_matrix) < 2) {
  
  stop(
    "At least two species are required for this NMDS."
  )
}


nmds_result <- vegan::metaMDS(
  community_matrix,
  distance = "jaccard",
  binary = TRUE,
  k = 2,
  trymax = 1000,
  autotransform = FALSE,
  trace = FALSE
)


stress_value <- nmds_result$stress


cat(
  "NMDS stress:\n"
)

print(
  stress_value
)


jaccard_distance <- vegan::vegdist(
  community_matrix,
  method = "jaccard",
  binary = TRUE
)


permanova_result <- vegan::adonis2(
  jaccard_distance ~ landscape,
  data = metadata,
  permutations = 9999
)


dispersion_result <- vegan::betadisper(
  jaccard_distance,
  group = metadata$landscape
)


permdisp_result <- vegan::permutest(
  dispersion_result,
  permutations = 9999
)


permanova_p <- permanova_result$`Pr(>F)`[1]

permdisp_p <- permdisp_result$tab$`Pr(>F)`[1]


cat(
  "\nPERMANOVA:\n"
)

print(
  permanova_result
)


cat(
  "\nPERMDISP:\n"
)

print(
  permdisp_result
)


site_scores <- vegan::scores(
  nmds_result,
  display = "sites"
) |>
  as.data.frame() |>
  tibble::rownames_to_column(
    "sampling_unit"
  ) |>
  dplyr::left_join(
    metadata,
    by = "sampling_unit"
  )


site_scores <- site_scores |>
  dplyr::group_by(
    landscape
  ) |>
  dplyr::mutate(
    unit_id = paste0(
      substr(
        landscape,
        1,
        1
      ),
      dplyr::row_number()
    )
  ) |>
  dplyr::ungroup()


range_x <- diff(
  range(
    site_scores$NMDS1,
    na.rm = TRUE
  )
)

range_y <- diff(
  range(
    site_scores$NMDS2,
    na.rm = TRUE
  )
)


amount_x <- ifelse(
  range_x == 0,
  0.02,
  range_x * 0.02
)

amount_y <- ifelse(
  range_y == 0,
  0.02,
  range_y * 0.02
)


site_scores <- site_scores |>
  dplyr::mutate(
    NMDS1_plot = jitter(
      NMDS1,
      amount = amount_x
    ),
    NMDS2_plot = jitter(
      NMDS2,
      amount = amount_y
    )
  )


landscape_order <- c(
  "Pantanal",
  "Cerrado",
  "Floodplain"
)


site_scores <- site_scores |>
  dplyr::mutate(
    landscape = factor(
      landscape,
      levels = landscape_order
    )
  )


landscape_palette <- c(
  "Pantanal" = "#4E9B73",
  "Cerrado" = "#C89A42",
  "Floodplain" = "#6CA0E8"
)


subtitle_text <- paste0(
  "Jaccard distance | Stress = ",
  format_number(
    stress_value,
    4
  ),
  " | n = ",
  nrow(site_scores),
  " sampling units",
  " | PERMANOVA p = ",
  format_p_value(
    permanova_p
  ),
  " | PERMDISP p = ",
  format_p_value(
    permdisp_p
  )
)


nmds_points_plot <- ggplot2::ggplot(
  site_scores,
  ggplot2::aes(
    x = NMDS1_plot,
    y = NMDS2_plot
  )
) +
  ggplot2::geom_point(
    ggplot2::aes(
      fill = landscape,
      color = landscape
    ),
    shape = 21,
    size = 3.8,
    stroke = 0.8,
    alpha = 0.85
  ) +
  ggplot2::scale_fill_manual(
    values = landscape_palette,
    name = "Landscape"
  ) +
  ggplot2::scale_color_manual(
    values = landscape_palette,
    name = "Landscape"
  ) +
  ggplot2::labs(
    title = "NMDS ordination of social wasp assemblages",
    subtitle = subtitle_text,
    x = "NMDS1",
    y = "NMDS2"
  ) +
  ggplot2::theme_classic(
    base_size = 16
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
      linewidth = 0.85
    ),
    axis.line = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_line(
      color = "black",
      linewidth = 0.5
    ),
    axis.text = ggplot2::element_text(
      color = "black",
      size = 13
    ),
    axis.title = ggplot2::element_text(
      color = "black",
      size = 16
    ),
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 22,
      hjust = 0.5,
      margin = ggplot2::margin(
        b = 6
      )
    ),
    plot.subtitle = ggplot2::element_text(
      size = 12.5,
      hjust = 0.5,
      margin = ggplot2::margin(
        b = 10
      )
    ),
    legend.position = "right",
    legend.title = ggplot2::element_text(
      face = "bold",
      size = 13
    ),
    legend.text = ggplot2::element_text(
      size = 12
    ),
    plot.margin = ggplot2::margin(
      18,
      18,
      18,
      18
    )
  )


print(
  nmds_points_plot
)


nmds_labels_plot <- nmds_points_plot +
  ggrepel::geom_text_repel(
    ggplot2::aes(
      label = unit_id,
      color = landscape
    ),
    size = 3,
    show.legend = FALSE,
    box.padding = 0.25,
    point.padding = 0.18,
    segment.color = "grey60",
    segment.linewidth = 0.25,
    max.overlaps = Inf
  ) +
  ggplot2::labs(
    subtitle = paste0(
      subtitle_text,
      " | unit labels added"
    )
  )


print(
  nmds_labels_plot
)


output_png_points <- file.path(
  output_directory,
  "NMDS_all_sampling_units_points.png"
)

output_tiff_points <- file.path(
  output_directory,
  "NMDS_all_sampling_units_points.tif"
)

output_pdf_points <- file.path(
  output_directory,
  "NMDS_all_sampling_units_points.pdf"
)


output_png_labels <- file.path(
  output_directory,
  "NMDS_all_sampling_units_labels.png"
)

output_tiff_labels <- file.path(
  output_directory,
  "NMDS_all_sampling_units_labels.tif"
)

output_pdf_labels <- file.path(
  output_directory,
  "NMDS_all_sampling_units_labels.pdf"
)


output_scores <- file.path(
  output_directory,
  "NMDS_all_sampling_units_scores.csv"
)

output_statistics <- file.path(
  output_directory,
  "NMDS_all_sampling_units_statistics.txt"
)


message(
  "Saving the NMDS point figure."
)

save_png_plot(
  nmds_points_plot,
  output_png_points
)

save_tiff_plot(
  nmds_points_plot,
  output_tiff_points
)

save_pdf_plot(
  nmds_points_plot,
  output_pdf_points
)


message(
  "Saving the NMDS labelled figure."
)

save_png_plot(
  nmds_labels_plot,
  output_png_labels
)

save_tiff_plot(
  nmds_labels_plot,
  output_tiff_labels
)

save_pdf_plot(
  nmds_labels_plot,
  output_pdf_labels
)


readr::write_csv(
  site_scores,
  output_scores,
  na = ""
)


statistics_text <- capture.output({
  
  cat(
    "NMDS - social wasps | suspension traps only\n"
  )
  
  cat(
    "Distance: Jaccard\n"
  )
  
  cat(
    "Matrix: presence/absence\n"
  )
  
  cat(
    "Sampling unit: landscape + latitude + longitude\n\n"
  )
  
  cat(
    "Stress:\n"
  )
  
  print(
    stress_value
  )
  
  cat(
    "\nMatrix dimensions:\n"
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
    metadata |>
      dplyr::count(
        landscape,
        name = "n_units"
      )
  )
  
  cat(
    "\nSpecies by landscape:\n"
  )
  
  print(
    data_deduplicated |>
      dplyr::distinct(
        landscape,
        species
      ) |>
      dplyr::count(
        landscape,
        name = "n_species"
      )
  )
  
  cat(
    "\nPERMANOVA:\n"
  )
  
  print(
    permanova_result
  )
  
  cat(
    "\nPERMDISP:\n"
  )
  
  print(
    permdisp_result
  )
  
  cat(
    "\nR session information:\n"
  )
  
  print(
    sessionInfo()
  )
})


writeLines(
  statistics_text,
  output_statistics,
  useBytes = TRUE
)


cat(
  "\nAnalysis completed.\n\n"
)

cat(
  "Dataset:",
  basename(file_csv),
  "\n"
)

cat(
  "Sampling units:",
  nrow(community_matrix),
  "\n"
)

cat(
  "Species:",
  ncol(community_matrix),
  "\n"
)

cat(
  "NMDS stress:",
  format_number(
    stress_value,
    4
  ),
  "\n"
)

cat(
  "PERMANOVA p:",
  format_p_value(
    permanova_p
  ),
  "\n"
)

cat(
  "PERMDISP p:",
  format_p_value(
    permdisp_p
  ),
  "\n\n"
)

cat(
  "Files saved in:\n",
  normalizePath(
    output_directory,
    winslash = "/",
    mustWork = TRUE
  ),
  "\n",
  sep = ""
)