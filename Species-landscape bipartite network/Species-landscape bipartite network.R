# Species-landscape bipartite network

set.seed(123)

options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  timeout = 600
)

required_packages <- c(
  "dplyr",
  "readr",
  "stringr",
  "ggplot2",
  "jpeg",
  "png",
  "tiff"
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
  install.packages(
    missing_packages,
    dependencies = c("Depends", "Imports", "LinkingTo")
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
    "."
  )
}

script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub(
  "^--file=",
  "",
  script_args[grep("^--file=", script_args)]
)

analysis_directory <- if (length(script_file) > 0) {
  normalizePath(
    dirname(
      script_file[1]
    ),
    winslash = "/",
    mustWork = TRUE
  )
} else {
  normalizePath(
    getwd(),
    winslash = "/",
    mustWork = TRUE
  )
}

image_directory <- {
  data_directory <- file.path(
    dirname(analysis_directory),
    "data"
  )

  if (dir.exists(data_directory)) {
    normalizePath(
      data_directory,
      winslash = "/",
      mustWork = TRUE
    )
  } else {
    analysis_directory
  }
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
  stop(
    "No CSV file named 'dados.csv' was found inside:\n- ",
    paste(search_directories, collapse = "\n- ")
  )
}

csv_information <- file.info(csv_files)
csv_times <- csv_information$mtime
csv_times[is.na(csv_times)] <- as.POSIXct("1970-01-01", tz = "UTC")

file_csv <- normalizePath(
  csv_files[which.max(csv_times)],
  winslash = "/",
  mustWork = TRUE
)

cat("Analysis directory:\n", analysis_directory, "\n\n", sep = "")
cat("Dataset selected:\n", file_csv, "\n\n", sep = "")
cat("Image directory:\n", image_directory, "\n\n", sep = "")
cat("Output directory:\n", output_directory, "\n\n", sep = "")

use_all_methods <- TRUE
deduplicate_records <- FALSE
top_n_images_per_landscape <- 2

title_main <- "Species-landscape links"
title_sub <- if (use_all_methods) {
  "All collection methods"
} else {
  "Suspension traps only"
}

landscape_colors <- c(
  "Pantanal" = "#4E9B73",
  "Cerrado" = "#C89A42",
  "Floodplain" = "#6CA0E8"
)

landscape_order <- c(
  "Pantanal",
  "Cerrado",
  "Floodplain"
)

background_color <- "white"
base_text_size <- 15
title_text_size <- 24
subtitle_text_size <- 15
legend_title_size <- 13
legend_text_size <- 12
species_text_size <- 4.9
line_legend_title <- "Number of records"
export_width_in <- 16
export_height_factor <- 0.36
export_dpi <- 1200

clean_names_safe <- function(x) {
  x <- gsub("^\\ufeff", "", x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

pick_column <- function(data, candidates) {
  matches <- names(data)[names(data) %in% candidates]
  
  if (length(matches) == 0) {
    return(NA_character_)
  }
  
  matches[1]
}

normalize_text <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(trimws(x))
  x
}

normalize_file_name <- function(x) {
  x <- tools::file_path_sans_ext(basename(x))
  x <- normalize_text(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  stringr::str_squish(x)
}

standardize_landscape <- function(x) {
  normalized_value <- normalize_text(x)
  
  dplyr::case_when(
    stringr::str_detect(normalized_value, "pant") ~ "Pantanal",
    stringr::str_detect(normalized_value, "cerr") ~ "Cerrado",
    stringr::str_detect(normalized_value, "flood|varzea|ivinhema|pevri") ~ "Floodplain",
    TRUE ~ NA_character_
  )
}

safe_read_image <- function(image_path) {
  extension <- tolower(
    tools::file_ext(image_path)
  )
  
  tryCatch(
    {
      if (extension == "png") {
        png::readPNG(image_path)
      } else if (extension %in% c("jpg", "jpeg")) {
        jpeg::readJPEG(image_path)
      } else if (extension %in% c("tif", "tiff")) {
        tiff::readTIFF(image_path)
      } else {
        NULL
      }
    },
    error = function(e) {
      warning(
        "The image could not be read: ",
        basename(image_path),
        ". ",
        conditionMessage(e)
      )
      NULL
    }
  )
}

find_landscape_images <- function(
    image_files,
    landscape_label,
    maximum_number = 2
) {
  if (length(image_files) == 0) {
    return(character(0))
  }
  
  aliases <- switch(
    landscape_label,
    "Pantanal" = c(
      "pantanal",
      "baia negra",
      "baianegra",
      "apa baia negra"
    ),
    "Cerrado" = c(
      "cerrado",
      "bodoquena",
      "serra da bodoquena"
    ),
    "Floodplain" = c(
      "floodplain",
      "varzea",
      "varzeas",
      "ivinhema",
      "rio ivinhema",
      "pevri"
    ),
    character(0)
  )
  
  aliases <- vapply(
    aliases,
    normalize_file_name,
    character(1)
  )
  
  normalized_names <- vapply(
    image_files,
    normalize_file_name,
    character(1)
  )
  
  root_bonus <- ifelse(
    normalize_path_safe(dirname(image_files)) ==
      normalize_path_safe(image_directory),
    5000,
    0
  )
  
  image_scores <- vapply(
    seq_along(image_files),
    function(i) {
      current_name <- normalized_names[i]
      
      alias_scores <- vapply(
        aliases,
        function(alias) {
          if (identical(current_name, alias)) {
            return(10000 + nchar(alias))
          }
          
          if (grepl(alias, current_name, fixed = TRUE)) {
            return(1000 + nchar(alias))
          }
          
          0
        },
        numeric(1)
      )
      
      max(alias_scores) + root_bonus[i]
    },
    numeric(1)
  )
  
  matched_positions <- which(
    image_scores > root_bonus
  )
  
  if (length(matched_positions) == 0) {
    return(character(0))
  }
  
  matched_positions <- matched_positions[
    order(
      -image_scores[matched_positions],
      basename(image_files[matched_positions])
    )
  ]
  
  selected_files <- unique(
    image_files[matched_positions]
  )
  
  head(
    normalizePath(
      selected_files,
      winslash = "/",
      mustWork = TRUE
    ),
    maximum_number
  )
}

add_image_stack_framed <- function(
    plot_object,
    image_files,
    x_min,
    x_max,
    y_center,
    border_color,
    fallback_label
) {
  frame_aspect <- 2.85 / 3.90
  frame_width <- x_max - x_min
  frame_height <- frame_width / frame_aspect
  frame_ymin <- y_center - frame_height / 2
  frame_ymax <- y_center + frame_height / 2
  inner_margin_x <- 0.0015
  inner_margin_y <- 0.0015
  gap_between <- 0.0008
  inner_xmin <- x_min + inner_margin_x
  inner_xmax <- x_max - inner_margin_x
  
  plot_object <- plot_object +
    ggplot2::annotate(
      "rect",
      xmin = x_min,
      xmax = x_max,
      ymin = frame_ymin,
      ymax = frame_ymax,
      fill = "white",
      color = NA
    )
  
  if (length(image_files) == 0) {
    return(
      plot_object +
        ggplot2::annotate(
          "text",
          x = (x_min + x_max) / 2,
          y = y_center,
          label = fallback_label,
          fontface = "bold",
          size = 4.2,
          color = "grey25"
        ) +
        ggplot2::annotate(
          "rect",
          xmin = x_min,
          xmax = x_max,
          ymin = frame_ymin,
          ymax = frame_ymax,
          fill = NA,
          color = border_color,
          linewidth = 1.15
        )
    )
  }
  
  image_files <- head(image_files, 2)
  
  if (length(image_files) == 1) {
    image_positions <- data.frame(
      ymin = frame_ymin + inner_margin_y,
      ymax = frame_ymax - inner_margin_y
    )
  } else {
    image_positions <- data.frame(
      ymin = c(
        y_center + gap_between / 2,
        frame_ymin + inner_margin_y
      ),
      ymax = c(
        frame_ymax - inner_margin_y,
        y_center - gap_between / 2
      )
    )
  }
  
  for (i in seq_along(image_files)) {
    image_object <- safe_read_image(
      image_files[i]
    )
    
    if (!is.null(image_object)) {
      image_grob <- grid::rasterGrob(
        image_object,
        width = grid::unit(1, "npc"),
        height = grid::unit(1, "npc"),
        interpolate = TRUE
      )
      
      plot_object <- plot_object +
        ggplot2::annotation_custom(
          grob = image_grob,
          xmin = inner_xmin,
          xmax = inner_xmax,
          ymin = image_positions$ymin[i],
          ymax = image_positions$ymax[i]
        )
    }
  }
  
  plot_object +
    ggplot2::annotate(
      "rect",
      xmin = x_min,
      xmax = x_max,
      ymin = frame_ymin,
      ymax = frame_ymax,
      fill = NA,
      color = border_color,
      linewidth = 1.15
    )
}

raw_data <- readr::read_delim(
  file = file_csv,
  delim = ";",
  locale = readr::locale(encoding = "UTF-8"),
  show_col_types = FALSE,
  trim_ws = TRUE
)

names(raw_data) <- clean_names_safe(
  names(raw_data)
)

cat("Detected columns:\n")
print(names(raw_data))
cat("\n")

species_column <- pick_column(
  raw_data,
  c("species", "especie")
)

landscape_column <- pick_column(
  raw_data,
  c("landscape", "paisagem", "landscape_class")
)

trap_column <- pick_column(
  raw_data,
  c("trap", "armadilha", "method", "metodo")
)

point_column <- pick_column(
  raw_data,
  c("point", "ponto", "local", "localidade", "area")
)

date_column <- pick_column(
  raw_data,
  c("date", "data")
)

latitude_column <- pick_column(
  raw_data,
  c("lati_s", "latitude", "lat")
)

longitude_column <- pick_column(
  raw_data,
  c("long_w", "longitude", "lon", "long")
)

if (is.na(species_column)) {
  stop("The species column was not found.")
}

if (is.na(trap_column)) {
  stop("The trap or collection-method column was not found.")
}

species_values <- as.character(
  raw_data[[species_column]]
)

trap_values <- as.character(
  raw_data[[trap_column]]
)

point_values <- if (!is.na(point_column)) {
  as.character(raw_data[[point_column]])
} else {
  rep(NA_character_, nrow(raw_data))
}

landscape_values <- if (!is.na(landscape_column)) {
  as.character(raw_data[[landscape_column]])
} else {
  rep(NA_character_, nrow(raw_data))
}

date_values <- if (!is.na(date_column)) {
  as.character(raw_data[[date_column]])
} else {
  rep(NA_character_, nrow(raw_data))
}

latitude_values <- if (!is.na(latitude_column)) {
  as.character(raw_data[[latitude_column]])
} else {
  rep(NA_character_, nrow(raw_data))
}

longitude_values <- if (!is.na(longitude_column)) {
  as.character(raw_data[[longitude_column]])
} else {
  rep(NA_character_, nrow(raw_data))
}

analysis_data <- raw_data |>
  dplyr::mutate(
    species = trimws(species_values),
    point = trimws(point_values),
    point_key = normalize_text(point_values),
    trap_normalized = normalize_text(trap_values),
    landscape = dplyr::case_when(
      !is.na(landscape_values) &
        trimws(landscape_values) != "" ~
        standardize_landscape(landscape_values),
      stringr::str_detect(
        point_key,
        "baia negra|baia_negra|baianegra"
      ) ~ "Pantanal",
      stringr::str_detect(
        point_key,
        "bodoquena"
      ) ~ "Cerrado",
      stringr::str_detect(
        point_key,
        "pevri|ivinhema|varzea"
      ) ~ "Floodplain",
      TRUE ~ NA_character_
    ),
    method_group = dplyr::case_when(
      stringr::str_detect(
        trap_normalized,
        "susp"
      ) ~ "Suspension",
      stringr::str_detect(
        trap_normalized,
        "mala"
      ) ~ "Malaise",
      stringr::str_detect(
        trap_normalized,
        "puc"
      ) ~ "Puçá",
      TRUE ~ NA_character_
    ),
    date_original = date_values,
    latitude_original = latitude_values,
    longitude_original = longitude_values
  ) |>
  dplyr::filter(
    !is.na(species),
    species != ""
  ) |>
  dplyr::filter(
    !is.na(landscape),
    landscape %in% landscape_order
  ) |>
  dplyr::filter(
    !is.na(method_group),
    method_group %in% c(
      "Suspension",
      "Malaise",
      "Puçá"
    )
  )

if (!use_all_methods) {
  analysis_data <- analysis_data |>
    dplyr::filter(
      method_group == "Suspension"
    )
}

if (deduplicate_records) {
  date_available <- !all(
    is.na(analysis_data$date_original)
  )
  
  latitude_available <- !all(
    is.na(analysis_data$latitude_original)
  )
  
  longitude_available <- !all(
    is.na(analysis_data$longitude_original)
  )
  
  if (
    date_available &&
    latitude_available &&
    longitude_available
  ) {
    analysis_data <- analysis_data |>
      dplyr::distinct(
        species,
        date_original,
        latitude_original,
        longitude_original,
        landscape,
        method_group,
        .keep_all = TRUE
      )
  } else {
    warning(
      "Deduplication was not applied because date, latitude or longitude was unavailable."
    )
  }
}

if (nrow(analysis_data) == 0) {
  stop("No valid records remained after filtering.")
}

cat("Number of records used: ", nrow(analysis_data), "\n", sep = "")
cat(
  "Number of species: ",
  dplyr::n_distinct(analysis_data$species),
  "\n\n",
  sep = ""
)

cat("Collection methods:\n")
print(table(analysis_data$method_group))
cat("\nRecords by landscape:\n")
print(table(analysis_data$landscape))
cat("\n")

links <- analysis_data |>
  dplyr::count(
    landscape,
    species,
    name = "n_records"
  ) |>
  dplyr::filter(
    n_records > 0
  )

if (nrow(links) == 0) {
  stop("No species-landscape links were generated.")
}

species_totals <- links |>
  dplyr::group_by(species) |>
  dplyr::summarise(
    total_records = sum(n_records),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    dplyr::desc(total_records),
    species
  )

species_order <- species_totals$species
number_of_species <- length(species_order)
vertical_limit <- number_of_species + 1

landscape_nodes <- data.frame(
  landscape = factor(
    landscape_order,
    levels = landscape_order
  ),
  x = 4.15,
  y = c(
    vertical_limit * 0.76,
    vertical_limit * 0.50,
    vertical_limit * 0.24
  )
)

species_nodes <- data.frame(
  species = species_order,
  x = 10.70,
  y = seq(
    from = vertical_limit,
    to = 0.8,
    length.out = number_of_species
  )
)

plot_data <- links |>
  dplyr::mutate(
    landscape = factor(
      landscape,
      levels = landscape_order
    )
  ) |>
  dplyr::left_join(
    landscape_nodes,
    by = "landscape"
  ) |>
  dplyr::rename(
    x_landscape = x,
    y_landscape = y
  ) |>
  dplyr::left_join(
    species_nodes,
    by = "species"
  ) |>
  dplyr::rename(
    x_species = x,
    y_species = y
  )

maximum_records <- max(
  plot_data$n_records,
  na.rm = TRUE
)

line_width_breaks <- pretty(
  c(1, maximum_records),
  n = 4
)

line_width_breaks <- line_width_breaks[
  line_width_breaks > 0 &
    line_width_breaks <= maximum_records
]

if (length(line_width_breaks) < 3) {
  line_width_breaks <- unique(
    c(
      1,
      round(maximum_records / 2),
      maximum_records
    )
  )
}

line_width_breaks <- sort(
  unique(
    line_width_breaks[
      line_width_breaks > 0
    ]
  )
)

species_label_offset <- 0.22

network_plot <- ggplot2::ggplot() +
  ggplot2::geom_curve(
    data = plot_data,
    ggplot2::aes(
      x = x_landscape,
      y = y_landscape,
      xend = x_species,
      yend = y_species,
      color = landscape,
      linewidth = n_records
    ),
    curvature = 0.18,
    alpha = 0.85,
    lineend = "round",
    ncp = 80
  ) +
  ggplot2::scale_color_manual(
    values = landscape_colors,
    breaks = landscape_order,
    name = "Landscape classification"
  ) +
  ggplot2::scale_linewidth_continuous(
    range = c(0.35, 9.5),
    breaks = line_width_breaks,
    labels = line_width_breaks,
    name = line_legend_title
  ) +
  ggplot2::geom_point(
    data = landscape_nodes,
    ggplot2::aes(
      x = x,
      y = y,
      fill = landscape,
      color = landscape
    ),
    shape = 21,
    size = 4.2,
    stroke = 1.15,
    show.legend = FALSE
  ) +
  ggplot2::scale_fill_manual(
    values = landscape_colors,
    guide = "none"
  ) +
  ggplot2::geom_point(
    data = species_nodes,
    ggplot2::aes(
      x = x,
      y = y
    ),
    shape = 21,
    size = 2,
    stroke = 0.25,
    color = "grey35",
    fill = "grey55"
  ) +
  ggplot2::geom_text(
    data = species_nodes |>
      dplyr::mutate(
        x_label = x + species_label_offset
      ),
    ggplot2::aes(
      x = x_label,
      y = y,
      label = species
    ),
    hjust = 0,
    size = species_text_size,
    fontface = "italic",
    color = "black"
  ) +
  ggplot2::coord_cartesian(
    xlim = c(0.2, 15.2),
    ylim = c(0.1, vertical_limit + 0.8),
    clip = "off"
  ) +
  ggplot2::labs(
    title = title_main,
    subtitle = title_sub,
    x = NULL,
    y = NULL
  ) +
  ggplot2::theme_void(
    base_size = base_text_size
  ) +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(
      fill = background_color,
      color = NA
    ),
    panel.background = ggplot2::element_rect(
      fill = background_color,
      color = NA
    ),
    legend.background = ggplot2::element_rect(
      fill = background_color,
      color = NA
    ),
    plot.title = ggplot2::element_text(
      face = "bold",
      size = title_text_size,
      hjust = 0.5,
      margin = ggplot2::margin(b = 5)
    ),
    plot.subtitle = ggplot2::element_text(
      size = subtitle_text_size,
      hjust = 0.5,
      margin = ggplot2::margin(b = 12)
    ),
    legend.box = "vertical",
    legend.spacing.y = grid::unit(0.26, "cm"),
    legend.title = ggplot2::element_text(
      face = "bold",
      size = legend_title_size
    ),
    legend.text = ggplot2::element_text(
      size = legend_text_size
    ),
    legend.box.margin = ggplot2::margin(
      0,
      0,
      0,
      8
    ),
    plot.margin = ggplot2::margin(
      28,
      180,
      28,
      24
    )
  ) +
  ggplot2::guides(
    color = ggplot2::guide_legend(
      order = 1,
      override.aes = list(
        linewidth = 1.5,
        alpha = 1
      )
    ),
    linewidth = ggplot2::guide_legend(
      order = 2,
      override.aes = list(
        color = "grey20",
        alpha = 1
      )
    )
  )

if (utils::packageVersion("ggplot2") >= "3.5.0") {
  network_plot <- network_plot +
    ggplot2::theme(
      legend.position = "inside",
      legend.position.inside = c(0.915, 0.47),
      legend.justification = c(0, 0.5)
    )
} else {
  network_plot <- network_plot +
    ggplot2::theme(
      legend.position = c(0.915, 0.47),
      legend.justification = c(0, 0.5)
    )
}

root_images <- list.files(
  path = image_directory,
  pattern = "\\.(png|jpg|jpeg|tif|tiff)$",
  full.names = TRUE,
  recursive = FALSE,
  ignore.case = TRUE
)

all_images <- list.files(
  path = image_directory,
  pattern = "\\.(png|jpg|jpeg|tif|tiff)$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

available_images <- unique(
  c(root_images, all_images)
)

available_images <- available_images[
  file.exists(available_images)
]

if (length(available_images) > 0) {
  available_images <- available_images[
    !is_inside_directory(
      available_images,
      output_directory
    )
  ]
}

available_images <- available_images[
  !grepl(
    "^Figure_bipartite_species_landscape_",
    basename(available_images),
    ignore.case = TRUE
  )
]

cat("Images found in the analysis folder:\n")

if (length(available_images) == 0) {
  cat("None\n\n")
} else {
  cat(
    paste0("- ", basename(available_images)),
    sep = "\n"
  )
  cat("\n\n")
}

pantanal_images <- find_landscape_images(
  image_files = available_images,
  landscape_label = "Pantanal",
  maximum_number = top_n_images_per_landscape
)

cerrado_images <- find_landscape_images(
  image_files = available_images,
  landscape_label = "Cerrado",
  maximum_number = top_n_images_per_landscape
)

floodplain_images <- find_landscape_images(
  image_files = available_images,
  landscape_label = "Floodplain",
  maximum_number = top_n_images_per_landscape
)

cat("Images selected for Pantanal:\n")
print(pantanal_images)
cat("\nImages selected for Cerrado:\n")
print(cerrado_images)
cat("\nImages selected for Floodplain:\n")
print(floodplain_images)
cat("\n")

missing_landscape_images <- character(0)

if (length(pantanal_images) == 0) {
  missing_landscape_images <- c(
    missing_landscape_images,
    "Pantanal"
  )
}

if (length(cerrado_images) == 0) {
  missing_landscape_images <- c(
    missing_landscape_images,
    "Cerrado"
  )
}

if (length(floodplain_images) == 0) {
  missing_landscape_images <- c(
    missing_landscape_images,
    "Floodplain"
  )
}

if (length(missing_landscape_images) > 0) {
  stop(
    "The following landscape images could not be identified: ",
    paste(missing_landscape_images, collapse = ", "),
    ". Files found: ",
    if (length(available_images) == 0) {
      "none"
    } else {
      paste(basename(available_images), collapse = ", ")
    },
    "."
  )
}

network_plot <- add_image_stack_framed(
  plot_object = network_plot,
  image_files = pantanal_images,
  x_min = 0.30,
  x_max = 3.95,
  y_center = landscape_nodes$y[
    landscape_nodes$landscape == "Pantanal"
  ],
  border_color = landscape_colors["Pantanal"],
  fallback_label = "Pantanal"
)

network_plot <- add_image_stack_framed(
  plot_object = network_plot,
  image_files = cerrado_images,
  x_min = 0.30,
  x_max = 3.95,
  y_center = landscape_nodes$y[
    landscape_nodes$landscape == "Cerrado"
  ],
  border_color = landscape_colors["Cerrado"],
  fallback_label = "Cerrado"
)

network_plot <- add_image_stack_framed(
  plot_object = network_plot,
  image_files = floodplain_images,
  x_min = 0.30,
  x_max = 3.95,
  y_center = landscape_nodes$y[
    landscape_nodes$landscape == "Floodplain"
  ],
  border_color = landscape_colors["Floodplain"],
  fallback_label = "Floodplain"
)

print(network_plot)

figure_width <- export_width_in

figure_height <- max(
  11.5,
  export_height_factor *
    number_of_species +
    4.8
)

suffix_name <- if (use_all_methods) {
  "all_methods"
} else {
  "suspension_only"
}

output_png <- file.path(
  output_directory,
  paste0(
    "Figure_bipartite_species_landscape_",
    suffix_name,
    ".png"
  )
)

output_tiff <- file.path(
  output_directory,
  paste0(
    "Figure_bipartite_species_landscape_",
    suffix_name,
    ".tif"
  )
)

output_pdf <- file.path(
  output_directory,
  paste0(
    "Figure_bipartite_species_landscape_",
    suffix_name,
    ".pdf"
  )
)

output_links <- file.path(
  output_directory,
  paste0(
    "Figure_bipartite_species_landscape_",
    suffix_name,
    "_links.csv"
  )
)

output_species <- file.path(
  output_directory,
  paste0(
    "Figure_bipartite_species_landscape_",
    suffix_name,
    "_species_totals.csv"
  )
)

if (capabilities("cairo")) {
  grDevices::png(
    filename = output_png,
    width = figure_width,
    height = figure_height,
    units = "in",
    res = export_dpi,
    bg = background_color,
    type = "cairo-png"
  )
} else {
  grDevices::png(
    filename = output_png,
    width = figure_width,
    height = figure_height,
    units = "in",
    res = export_dpi,
    bg = background_color
  )
}

tryCatch(
  print(network_plot),
  finally = grDevices::dev.off()
)

grDevices::tiff(
  filename = output_tiff,
  width = figure_width,
  height = figure_height,
  units = "in",
  res = export_dpi,
  compression = "lzw",
  bg = background_color
)

tryCatch(
  print(network_plot),
  finally = grDevices::dev.off()
)

if (capabilities("cairo")) {
  grDevices::cairo_pdf(
    filename = output_pdf,
    width = figure_width,
    height = figure_height,
    bg = background_color
  )
} else {
  grDevices::pdf(
    file = output_pdf,
    width = figure_width,
    height = figure_height,
    bg = background_color,
    useDingbats = FALSE
  )
}

tryCatch(
  print(network_plot),
  finally = grDevices::dev.off()
)

readr::write_csv(
  plot_data,
  output_links,
  na = ""
)

readr::write_csv(
  species_totals,
  output_species,
  na = ""
)

cat("Analysis completed.\n\n")
cat("Records used: ", nrow(analysis_data), "\n", sep = "")
cat("Species: ", number_of_species, "\n", sep = "")
cat("Species-landscape links: ", nrow(plot_data), "\n\n", sep = "")

cat("Pantanal images:\n")
cat(pantanal_images, sep = "\n")
cat("\n\nCerrado images:\n")
cat(cerrado_images, sep = "\n")
cat("\n\nFloodplain images:\n")
cat(floodplain_images, sep = "\n")
cat("\n\n")

cat("PNG saved at:\n", normalizePath(output_png, winslash = "/"), "\n\n", sep = "")
cat("TIFF saved at:\n", normalizePath(output_tiff, winslash = "/"), "\n\n", sep = "")
cat("PDF saved at:\n", normalizePath(output_pdf, winslash = "/"), "\n\n", sep = "")
cat("Links CSV saved at:\n", normalizePath(output_links, winslash = "/"), "\n\n", sep = "")
cat("Species totals CSV saved at:\n", normalizePath(output_species, winslash = "/"), "\n", sep = "")