# Heat map of social wasp occurrence density in Mato Grosso do Sul

map_author <- "Leite JMG"
map_year <- "2026"

main_title <- "Heat map of social wasp occurrence density in Mato Grosso do Sul"
main_subtitle <- "Kernel density estimate over shaded relief"


options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  timeout = 600
)


required_packages <- c(
  "sf",
  "dplyr",
  "readr",
  "stringr",
  "ggplot2",
  "patchwork",
  "ggspatial",
  "geobr",
  "MASS",
  "purrr",
  "scales",
  "terra",
  "elevatr",
  "ggnewscale",
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
    dependencies = TRUE
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


sf::sf_use_s2(FALSE)


get_script_directories <- function() {
  
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
  
  candidate_directories <- get_script_directories()
  
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
  
  
  if (length(csv_files) > 1) {
    
    selected_file <- utils::select.list(
      choices = csv_files,
      preselect = csv_files[1],
      multiple = FALSE,
      title = "Select the occurrence dataset",
      graphics = TRUE
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


if (
  tolower(
    basename(data_directory)
  ) == "data"
) {
  
  analysis_directory <- dirname(
    data_directory
  )
  
} else {
  
  analysis_directory <- data_directory
}


output_directory <- file.path(
  script_directory,
  "output"
)


if (!dir.exists(output_directory)) {
  
  dir.create(
    output_directory,
    recursive = TRUE
  )
}


message(
  "Dataset selected: ",
  file_csv
)

message(
  "Output directory: ",
  output_directory
)


use_standardized_only <- FALSE
use_deduplicated_records <- TRUE
outlier_threshold_km <- 100

kde_grid_n <- 320
heat_gamma <- 0.42
heat_cutoff <- 0.08
contour_bins <- 8

bandwidth_factor_x <- 0.11
bandwidth_factor_y <- 0.11

rep_point_size <- 2.8
rep_point_fill <- "white"
rep_point_color <- "grey10"
rep_point_stroke <- 0.8


clean_names_safe <- function(x) {
  
  x <- gsub(
    "^\ufeff",
    "",
    x
  )
  
  x <- tolower(x)
  
  x <- gsub(
    "[^a-z0-9]+",
    "_",
    x
  )
  
  x <- gsub(
    "^_+|_+$",
    "",
    x
  )
  
  x
}


pick_column <- function(
    data,
    candidates
) {
  
  column_names <- names(data)
  
  matches <- column_names[
    column_names %in% candidates
  ]
  
  if (length(matches) == 0) {
    
    return(
      NA_character_
    )
  }
  
  matches[1]
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


dms_to_decimal <- function(x) {
  
  if (
    is.na(x) ||
    trimws(x) == ""
  ) {
    
    return(
      NA_real_
    )
  }
  
  
  x <- tolower(
    trimws(
      as.character(x)
    )
  )
  
  x <- gsub(
    "º",
    "°",
    x
  )
  
  x <- gsub(
    "’|´|`|‘",
    "'",
    x
  )
  
  x <- gsub(
    '""',
    '"',
    x
  )
  
  x <- gsub(
    "''",
    '"',
    x
  )
  
  x <- gsub(
    "”|“",
    '"',
    x
  )
  
  x <- gsub(
    "\\s+",
    "",
    x
  )
  
  
  match_result <- stringr::str_match(
    x,
    "^(\\d+)°(\\d+)'(\\d+(?:\\.\\d+)?)\\\"?([nsew])$"
  )
  
  
  if (all(is.na(match_result))) {
    
    numeric_value <- suppressWarnings(
      as.numeric(x)
    )
    
    return(
      numeric_value
    )
  }
  
  
  degrees <- as.numeric(
    match_result[2]
  )
  
  minutes <- as.numeric(
    match_result[3]
  )
  
  seconds <- as.numeric(
    match_result[4]
  )
  
  hemisphere <- match_result[5]
  
  
  decimal_value <-
    degrees +
    minutes / 60 +
    seconds / 3600
  
  
  if (hemisphere %in% c("s", "w")) {
    
    decimal_value <- -decimal_value
  }
  
  
  decimal_value
}


build_relief <- function(
    projected_polygon,
    zoom,
    buffer_m = 30000,
    shadow_max = 0.04
) {
  
  buffered_polygon <- projected_polygon |>
    sf::st_union() |>
    sf::st_buffer(
      buffer_m
    ) |>
    sf::st_as_sf()
  
  
  buffered_geographic <- sf::st_transform(
    buffered_polygon,
    4674
  )
  
  
  elevation_source <- tryCatch(
    elevatr::get_elev_raster(
      locations = buffered_geographic,
      z = zoom,
      clip = "locations",
      override_size_check = TRUE
    ),
    error = function(e) {
      
      stop(
        "The elevation model could not be downloaded. ",
        "Check the internet connection and run the script again. ",
        "Original error: ",
        conditionMessage(e)
      )
    }
  )
  
  
  elevation_raster <- terra::rast(
    elevation_source
  )
  
  
  elevation_raster <- terra::project(
    elevation_raster,
    sf::st_crs(
      projected_polygon
    )$wkt
  )
  
  
  polygon_vector <- terra::vect(
    projected_polygon
  )
  
  
  elevation_raster <- terra::crop(
    elevation_raster,
    polygon_vector
  )
  
  
  elevation_raster <- terra::mask(
    elevation_raster,
    polygon_vector
  )
  
  
  slope <- terra::terrain(
    elevation_raster,
    v = "slope",
    unit = "radians",
    neighbors = 8
  )
  
  
  aspect <- terra::terrain(
    elevation_raster,
    v = "aspect",
    unit = "radians",
    neighbors = 8
  )
  
  
  hillshade <- terra::shade(
    slope,
    aspect,
    angle = 35,
    direction = 315,
    normalize = TRUE
  )
  
  
  elevation_data <- as.data.frame(
    elevation_raster,
    xy = TRUE,
    na.rm = TRUE
  )
  
  names(elevation_data)[3] <- "elevation"
  
  
  hillshade_data <- as.data.frame(
    hillshade,
    xy = TRUE,
    na.rm = TRUE
  )
  
  names(hillshade_data)[3] <- "hillshade"
  
  
  hillshade_data$shadow_alpha <- scales::rescale(
    1 - hillshade_data$hillshade,
    to = c(
      0,
      shadow_max
    )
  )
  
  
  list(
    elevation = elevation_data,
    hillshade = hillshade_data
  )
}


make_heat_legend <- function(
    display_threshold
) {
  
  heat_colors <- c(
    "#FFF7EC",
    "#FEE8C8",
    "#FDBB84",
    "#FC8D59",
    "#E34A33",
    "#B30000"
  )
  
  
  legend_data <- data.frame(
    xmin = c(
      0,
      1.2,
      2.4,
      3.6,
      4.8
    ),
    xmax = c(
      1,
      2.2,
      3.4,
      4.6,
      5.8
    ),
    xmid = c(
      0.5,
      1.7,
      2.9,
      4.1,
      5.3
    ),
    fill_color = c(
      heat_colors[2],
      heat_colors[3],
      heat_colors[4],
      heat_colors[5],
      heat_colors[6]
    ),
    label = c(
      "Very low",
      "Low",
      "Moderate",
      "High",
      "Very high"
    )
  )
  
  
  ggplot() +
    
    theme_void() +
    
    coord_cartesian(
      xlim = c(
        0,
        12.2
      ),
      ylim = c(
        0,
        3.2
      ),
      clip = "off"
    ) +
    
    annotate(
      "text",
      x = 0,
      y = 2.88,
      label = "Legend",
      hjust = 0,
      fontface = "bold",
      size = 5
    ) +
    
    annotate(
      "text",
      x = 0,
      y = 2.35,
      label = "Relative density (KDE)",
      hjust = 0,
      fontface = "bold",
      size = 4
    ) +
    
    geom_rect(
      data = legend_data,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = 1.72,
        ymax = 2.08,
        fill = fill_color
      ),
      color = "grey35",
      linewidth = 0.25,
      inherit.aes = FALSE
    ) +
    
    scale_fill_identity() +
    
    geom_text(
      data = legend_data,
      aes(
        x = xmid,
        y = 1.30,
        label = label
      ),
      size = 2.8,
      inherit.aes = FALSE
    ) +
    
    annotate(
      "segment",
      x = 7.3,
      xend = 8.2,
      y = 1.98,
      yend = 1.98,
      color = "white",
      linewidth = 0.72
    ) +
    
    annotate(
      "text",
      x = 8.45,
      y = 1.98,
      label = "KDE contour",
      hjust = 0,
      size = 3.5
    ) +
    
    annotate(
      "point",
      x = 7.75,
      y = 1.36,
      shape = 21,
      size = 3.2,
      fill = rep_point_fill,
      color = rep_point_color,
      stroke = 0.45
    ) +
    
    annotate(
      "text",
      x = 8.45,
      y = 1.36,
      label = "Representative sampling point",
      hjust = 0,
      size = 3.4
    ) +
    
    annotate(
      "text",
      x = 7.3,
      y = 0.54,
      label = paste0(
        "Display threshold: ",
        display_threshold
      ),
      hjust = 0,
      size = 3.3
    ) +
    
    theme(
      plot.margin = margin(
        t = 0,
        r = 12,
        b = 0,
        l = 12
      )
    )
}


occurrence_data <- readr::read_delim(
  file = file_csv,
  delim = ";",
  locale = readr::locale(
    encoding = "UTF-8"
  ),
  show_col_types = FALSE,
  trim_ws = TRUE
)


names(occurrence_data) <- clean_names_safe(
  names(occurrence_data)
)


point_column <- pick_column(
  occurrence_data,
  c(
    "point",
    "ponto",
    "local",
    "localidade",
    "area",
    "area_name"
  )
)


landscape_column <- pick_column(
  occurrence_data,
  c(
    "landscape",
    "paisagem",
    "landscape_class"
  )
)


latitude_column <- pick_column(
  occurrence_data,
  c(
    "lati_s",
    "latitude",
    "lat",
    "latitude_s"
  )
)


longitude_column <- pick_column(
  occurrence_data,
  c(
    "long_w",
    "longitude",
    "lon",
    "long",
    "longitude_w"
  )
)


trap_column <- pick_column(
  occurrence_data,
  c(
    "trap",
    "armadilha",
    "method",
    "metodo"
  )
)


species_column <- pick_column(
  occurrence_data,
  c(
    "species",
    "especie"
  )
)


date_column <- pick_column(
  occurrence_data,
  c(
    "date",
    "data"
  )
)


if (is.na(point_column)) {
  
  stop(
    "A sampling-point column was not found."
  )
}


if (is.na(latitude_column)) {
  
  stop(
    "A latitude column was not found."
  )
}


if (is.na(longitude_column)) {
  
  stop(
    "A longitude column was not found."
  )
}


point_values <- as.character(
  occurrence_data[[point_column]]
)


landscape_values <- if (
  !is.na(landscape_column)
) {
  
  as.character(
    occurrence_data[[landscape_column]]
  )
  
} else {
  
  rep(
    NA_character_,
    nrow(occurrence_data)
  )
}


latitude_values <- as.character(
  occurrence_data[[latitude_column]]
)


longitude_values <- as.character(
  occurrence_data[[longitude_column]]
)


trap_values <- if (
  !is.na(trap_column)
) {
  
  as.character(
    occurrence_data[[trap_column]]
  )
  
} else {
  
  rep(
    NA_character_,
    nrow(occurrence_data)
  )
}


species_values <- if (
  !is.na(species_column)
) {
  
  as.character(
    occurrence_data[[species_column]]
  )
  
} else {
  
  rep(
    NA_character_,
    nrow(occurrence_data)
  )
}


date_values <- if (
  !is.na(date_column)
) {
  
  as.character(
    occurrence_data[[date_column]]
  )
  
} else {
  
  rep(
    NA_character_,
    nrow(occurrence_data)
  )
}


geographic_data <- occurrence_data |>
  dplyr::mutate(
    point = trimws(
      point_values
    ),
    point_key = normalize_text(
      point_values
    ),
    landscape = dplyr::case_when(
      !is.na(landscape_values) &
        trimws(landscape_values) != "" ~
        normalize_text(landscape_values),
      
      stringr::str_detect(
        point_key,
        "baia negra|baia_negra"
      ) ~ "pantanal",
      
      stringr::str_detect(
        point_key,
        "pevri|ivinhema"
      ) ~ "floodplain",
      
      stringr::str_detect(
        point_key,
        "bodoquena"
      ) ~ "cerrado",
      
      TRUE ~ NA_character_
    ),
    trap = normalize_text(
      trap_values
    ),
    species = species_values,
    date = date_values,
    lat = purrr::map_dbl(
      latitude_values,
      dms_to_decimal
    ),
    lon = purrr::map_dbl(
      longitude_values,
      dms_to_decimal
    )
  ) |>
  dplyr::filter(
    !is.na(lat),
    !is.na(lon),
    !is.na(landscape),
    point != ""
  )


if (use_standardized_only) {
  
  geographic_data <- geographic_data |>
    dplyr::filter(
      trap %in% c(
        "suspension",
        "suspended",
        "suspension trap"
      )
    )
}


if (use_deduplicated_records) {
  
  geographic_data <- geographic_data |>
    dplyr::distinct(
      species,
      date,
      lat,
      lon,
      landscape,
      point,
      .keep_all = TRUE
    )
}


if (nrow(geographic_data) < 2) {
  
  stop(
    "At least two valid georeferenced records are required."
  )
}


points_projected <- geographic_data |>
  sf::st_as_sf(
    coords = c(
      "lon",
      "lat"
    ),
    crs = 4674,
    remove = FALSE
  ) |>
  sf::st_transform(
    5880
  )


projected_coordinates <- sf::st_coordinates(
  points_projected
)


points_projected$x <- projected_coordinates[, 1]
points_projected$y <- projected_coordinates[, 2]


points_projected <- points_projected |>
  dplyr::group_by(
    landscape
  ) |>
  dplyr::mutate(
    median_x = median(
      x,
      na.rm = TRUE
    ),
    median_y = median(
      y,
      na.rm = TRUE
    ),
    distance_km = sqrt(
      (x - median_x)^2 +
        (y - median_y)^2
    ) / 1000
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(
    distance_km <= outlier_threshold_km
  )


if (nrow(points_projected) < 2) {
  
  stop(
    "Fewer than two records remained after geographic outlier removal."
  )
}


points_geographic <- sf::st_transform(
  points_projected,
  4674
)


representative_points <- points_geographic |>
  sf::st_drop_geometry() |>
  dplyr::group_by(
    landscape
  ) |>
  dplyr::summarise(
    lon = median(
      lon,
      na.rm = TRUE
    ),
    lat = median(
      lat,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


representative_points <- sf::st_as_sf(
  representative_points,
  coords = c(
    "lon",
    "lat"
  ),
  crs = 4674
)


message(
  "Downloading the Mato Grosso do Sul boundary."
)


state_boundary <- tryCatch(
  geobr::read_state(
    year = 2020,
    showProgress = FALSE
  ),
  error = function(e) {
    
    stop(
      "The administrative boundary could not be downloaded through geobr. ",
      "Check the internet connection and run the script again. ",
      "Original error: ",
      conditionMessage(e)
    )
  }
)


state_boundary <- state_boundary |>
  dplyr::filter(
    abbrev_state == "MS"
  )


if (nrow(state_boundary) == 0) {
  
  stop(
    "Mato Grosso do Sul was not found in the geobr state dataset."
  )
}


state_projected <- sf::st_transform(
  state_boundary,
  5880
)


representative_projected <- sf::st_transform(
  representative_points,
  5880
)


message(
  "Downloading the elevation model."
)


relief <- build_relief(
  projected_polygon = state_projected,
  zoom = 7,
  buffer_m = 30000,
  shadow_max = 0.04
)


map_bounds <- sf::st_bbox(
  state_projected
)


kde_coordinates <- sf::st_coordinates(
  points_projected
)


padding_x <- (
  map_bounds$xmax -
    map_bounds$xmin
) * 0.03


padding_y <- (
  map_bounds$ymax -
    map_bounds$ymin
) * 0.03


bandwidth_x <- as.numeric(
  (
    map_bounds$xmax -
      map_bounds$xmin
  ) * bandwidth_factor_x
)


bandwidth_y <- as.numeric(
  (
    map_bounds$ymax -
      map_bounds$ymin
  ) * bandwidth_factor_y
)


kernel_density <- MASS::kde2d(
  x = kde_coordinates[, 1],
  y = kde_coordinates[, 2],
  n = kde_grid_n,
  h = c(
    bandwidth_x,
    bandwidth_y
  ),
  lims = c(
    map_bounds$xmin - padding_x,
    map_bounds$xmax + padding_x,
    map_bounds$ymin - padding_y,
    map_bounds$ymax + padding_y
  )
)


kernel_data <- expand.grid(
  x = kernel_density$x,
  y = kernel_density$y
) |>
  dplyr::mutate(
    z = as.vector(
      kernel_density$z
    )
  )


kernel_points <- sf::st_as_sf(
  kernel_data,
  coords = c(
    "x",
    "y"
  ),
  crs = 5880,
  remove = FALSE
)


inside_boundary <- as.vector(
  sf::st_within(
    kernel_points,
    state_projected,
    sparse = FALSE
  )
)


kernel_data <- kernel_data[
  inside_boundary,
  ,
  drop = FALSE
]


kernel_data$relative_density <-
  kernel_data$z /
  max(
    kernel_data$z,
    na.rm = TRUE
  )


kernel_data$adjusted_density <-
  kernel_data$relative_density ^
  heat_gamma


kernel_data$display_density <- ifelse(
  kernel_data$adjusted_density <
    heat_cutoff,
  NA_real_,
  kernel_data$adjusted_density
)


kernel_data$display_alpha <- ifelse(
  is.na(
    kernel_data$display_density
  ),
  NA_real_,
  scales::rescale(
    kernel_data$display_density,
    to = c(
      0.30,
      1
    ),
    from = c(
      heat_cutoff,
      1
    )
  )
)


legend_plot <- make_heat_legend(
  display_threshold = heat_cutoff
)


heatmap_plot <- ggplot() +
  
  geom_raster(
    data = relief$elevation,
    aes(
      x = x,
      y = y,
      fill = elevation
    ),
    interpolate = TRUE
  ) +
  
  scale_fill_gradient(
    low = "grey98",
    high = "grey91",
    guide = "none",
    na.value = "transparent"
  ) +
  
  geom_raster(
    data = relief$hillshade,
    aes(
      x = x,
      y = y,
      alpha = shadow_alpha
    ),
    fill = "black",
    interpolate = TRUE
  ) +
  
  scale_alpha_identity(
    guide = "none"
  ) +
  
  geom_sf(
    data = state_projected,
    fill = NA,
    color = "grey45",
    linewidth = 0.55
  ) +
  
  ggnewscale::new_scale_fill() +
  
  geom_raster(
    data = kernel_data,
    aes(
      x = x,
      y = y,
      fill = display_density,
      alpha = display_alpha
    ),
    interpolate = TRUE
  ) +
  
  geom_contour(
    data = kernel_data |>
      dplyr::filter(
        !is.na(display_density)
      ),
    aes(
      x = x,
      y = y,
      z = display_density
    ),
    bins = contour_bins,
    color = "white",
    alpha = 0.55,
    linewidth = 0.22
  ) +
  
  geom_sf(
    data = representative_projected,
    shape = 21,
    size = rep_point_size,
    fill = rep_point_fill,
    color = rep_point_color,
    stroke = rep_point_stroke
  ) +
  
  ggspatial::annotation_scale(
    location = "bl",
    width_hint = 0.16,
    line_width = 0.6
  ) +
  
  ggspatial::annotation_north_arrow(
    location = "br",
    which_north = "true",
    style = ggspatial::north_arrow_orienteering(
      fill = c(
        "white",
        "black"
      ),
      line_col = "black"
    ),
    height = grid::unit(
      0.9,
      "cm"
    ),
    width = grid::unit(
      0.9,
      "cm"
    )
  ) +
  
  scale_fill_gradientn(
    colours = c(
      "#FFF7EC",
      "#FEE8C8",
      "#FDBB84",
      "#FC8D59",
      "#E34A33",
      "#B30000"
    ),
    values = scales::rescale(
      c(
        heat_cutoff,
        0.18,
        0.35,
        0.55,
        0.78,
        1
      )
    ),
    limits = c(
      heat_cutoff,
      1
    ),
    guide = "none",
    na.value = NA
  ) +
  
  scale_alpha(
    range = c(
      0.30,
      1
    ),
    guide = "none"
  ) +
  
  coord_sf(
    crs = sf::st_crs(
      state_projected
    ),
    expand = FALSE
  ) +
  
  labs(
    title = main_title,
    subtitle = main_subtitle,
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal(
    base_size = 12
  ) +
  
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(
      face = "bold",
      size = 15,
      margin = margin(
        b = 6
      )
    ),
    plot.subtitle = element_text(
      size = 10.5,
      margin = margin(
        b = 14
      )
    ),
    plot.margin = margin(
      t = 20,
      r = 10,
      b = 10,
      l = 10
    ),
    legend.position = "none",
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    )
  )


caption_line_1 <- paste0(
  "Coordinate system / datum: Geographic coordinates ",
  "(latitude/longitude), SIRGAS 2000 (EPSG:4674)."
)


caption_line_2 <- paste0(
  "Kernel density estimation and relief processing were performed ",
  "in projected CRS EPSG:5880. Hypsometric tint and hillshade ",
  "derived from DEM obtained via the elevatr package and processed ",
  "in terra."
)


caption_line_3 <- paste0(
  "Administrative limits: geobr. ",
  if (use_standardized_only) {
    paste0(
      "Kernel density estimate based only on standardized trap ",
      "records inside Mato Grosso do Sul."
    )
  } else {
    paste0(
      "Kernel density estimate based on georeferenced occurrence ",
      "records inside Mato Grosso do Sul."
    )
  }
)


caption_line_4 <- paste0(
  if (use_deduplicated_records) {
    "Records were deduplicated by species, date and coordinates. "
  } else {
    ""
  },
  "Representative points correspond to median coordinates per landscape. ",
  "Cartographic elaboration: ",
  map_author,
  ", ",
  map_year,
  "."
)


caption_text <- paste(
  caption_line_1,
  caption_line_2,
  caption_line_3,
  caption_line_4,
  sep = "\n"
)


heatmap_figure <- (
  heatmap_plot /
    legend_plot
) +
  
  patchwork::plot_layout(
    heights = c(
      2.7,
      0.75
    )
  ) +
  
  patchwork::plot_annotation(
    caption = caption_text,
    theme = theme(
      plot.caption = element_text(
        size = 6.8,
        hjust = 0,
        lineheight = 1.12,
        margin = margin(
          t = 8
        )
      )
    )
  )


print(
  heatmap_figure
)


output_png <- file.path(
  output_directory,
  "Figure_HeatMap_CAP2_only_heatmap.png"
)


output_pdf <- file.path(
  output_directory,
  "Figure_HeatMap_CAP2_only_heatmap.pdf"
)


if (capabilities("cairo")) {
  
  grDevices::png(
    filename = output_png,
    width = 8.3,
    height = 9.8,
    units = "in",
    res = 600,
    bg = "white",
    type = "cairo-png"
  )
  
} else {
  
  grDevices::png(
    filename = output_png,
    width = 8.3,
    height = 9.8,
    units = "in",
    res = 600,
    bg = "white"
  )
}


print(
  heatmap_figure
)

grDevices::dev.off()


if (capabilities("cairo")) {
  
  grDevices::cairo_pdf(
    filename = output_pdf,
    width = 8.3,
    height = 9.8,
    bg = "white"
  )
  
} else {
  
  grDevices::pdf(
    file = output_pdf,
    width = 8.3,
    height = 9.8,
    bg = "white"
  )
}


print(
  heatmap_figure
)

grDevices::dev.off()


cat(
  "\nAnalysis completed.\n\n"
)


cat(
  "Dataset:",
  basename(file_csv),
  "\n"
)


cat(
  "Records after filtering and deduplication:",
  nrow(geographic_data),
  "\n"
)


cat(
  "Records after geographic outlier removal:",
  nrow(points_projected),
  "\n"
)


cat(
  "Sampling localities:",
  dplyr::n_distinct(
    geographic_data$point
  ),
  "\n"
)


cat(
  "Bandwidth X:",
  round(
    bandwidth_x,
    0
  ),
  "m\n"
)


cat(
  "Bandwidth Y:",
  round(
    bandwidth_y,
    0
  ),
  "m\n"
)


cat(
  "PNG saved at:\n",
  normalizePath(
    output_png,
    winslash = "/",
    mustWork = TRUE
  ),
  "\n",
  sep = ""
)


cat(
  "PDF saved at:\n",
  normalizePath(
    output_pdf,
    winslash = "/",
    mustWork = TRUE
  ),
  "\n",
  sep = ""
)