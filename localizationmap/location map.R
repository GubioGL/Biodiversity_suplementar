# Study-area location map

map_author <- "Leite JMG"
map_year <- "2026"
sampling_period <- "2024–2025"
contour_interval_ms <- 100

main_title <- "Hypsometric relief and study areas in Mato Grosso do Sul"
main_subtitle <- "Location panels and enlarged protected-area insets"


options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  timeout = 600
)


required_packages <- c(
  "sf",
  "dplyr",
  "readr",
  "stringr",
  "stringi",
  "ggplot2",
  "ggspatial",
  "geobr",
  "rnaturalearth",
  "rnaturalearthdata",
  "purrr",
  "scales",
  "grid",
  "terra",
  "elevatr",
  "ggnewscale",
  "cowplot",
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


sf::sf_use_s2(FALSE)


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
      "^dados.csv$",
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


locate_script_directory <- function() {

  candidate_directories <- get_candidate_directories()

  script_directories <- candidate_directories[
    vapply(
      candidate_directories,
      function(directory) {

        dir.exists(
          file.path(
            directory,
            "shapes_ucs_ms"
          )
        )
      },
      logical(1)
    )
  ]

  if (length(script_directories) > 0) {

    return(
      normalizePath(
        script_directories[1],
        winslash = "/",
        mustWork = TRUE
      )
    )
  }

  normalizePath(
    getwd(),
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
  dirname(data_directory)
} else {
  data_directory
}

script_directory <- locate_script_directory()

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
  "Script directory: ",
  script_directory
)

message(
  "Output directory: ",
  output_directory
)


clean_names_safe <- function(x) {
  
  x <- gsub(
    "^\ufeff",
    "",
    x
  )
  
  x <- iconv(
    x,
    from = "",
    to = "ASCII//TRANSLIT"
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
  
  matches <- names(data)[
    names(data) %in% candidates
  ]
  
  if (length(matches) == 0) {
    
    return(
      NA_character_
    )
  }
  
  matches[1]
}


normalize_text_upper <- function(x) {
  
  x <- as.character(x)
  
  x <- stringi::stri_trans_general(
    x,
    "Latin-ASCII"
  )
  
  toupper(
    trimws(x)
  )
}


normalize_text_lower <- function(x) {
  
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
  
  if (all(
    is.na(match_result)
  )) {
    
    return(
      suppressWarnings(
        as.numeric(x)
      )
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
  
  if (
    hemisphere %in% c(
      "s",
      "w"
    )
  ) {
    
    decimal_value <- -decimal_value
  }
  
  decimal_value
}


identify_area_id <- function(x) {
  
  normalized_text <- normalize_text_upper(
    x
  )
  
  dplyr::case_when(
    grepl(
      "BAIA NEGRA",
      normalized_text
    ) ~ "pantanal",
    
    grepl(
      "SERRA DA BODOQUENA|BODOQUENA",
      normalized_text
    ) ~ "cerrado",
    
    grepl(
      "VARZEAS DO RIO IVINHEMA|IVINHEMA|PEVRI",
      normalized_text
    ) ~ "floodplain",
    
    TRUE ~ NA_character_
  )
}


build_relief_geographic <- function(
    spatial_object,
    zoom,
    buffer_m = 30000,
    shadow_max = 0.22
) {
  
  object_projected <- sf::st_transform(
    spatial_object,
    5880
  )
  
  object_buffer <- object_projected |>
    sf::st_union() |>
    sf::st_buffer(
      buffer_m
    ) |>
    sf::st_as_sf()
  
  object_buffer_geographic <- sf::st_transform(
    object_buffer,
    4674
  )
  
  elevation_source <- tryCatch(
    elevatr::get_elev_raster(
      locations = object_buffer_geographic,
      z = zoom,
      clip = "locations",
      override_size_check = TRUE
    ),
    error = function(e) {
      
      stop(
        "The elevation model could not be downloaded. ",
        "Check the internet connection. Original error: ",
        conditionMessage(e)
      )
    }
  )
  
  elevation_raster <- terra::rast(
    elevation_source
  )
  
  elevation_raster <- terra::project(
    elevation_raster,
    "EPSG:5880"
  )
  
  object_vector <- terra::vect(
    object_projected
  )
  
  elevation_raster <- terra::crop(
    elevation_raster,
    object_vector
  )
  
  elevation_raster <- terra::mask(
    elevation_raster,
    object_vector
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
  
  elevation_geographic <- terra::project(
    elevation_raster,
    "EPSG:4674",
    method = "bilinear"
  )
  
  hillshade_geographic <- terra::project(
    hillshade,
    "EPSG:4674",
    method = "bilinear"
  )
  
  elevation_data <- as.data.frame(
    elevation_geographic,
    xy = TRUE,
    na.rm = TRUE
  )
  
  names(elevation_data)[3] <- "elevation"
  
  hillshade_data <- as.data.frame(
    hillshade_geographic,
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
    dem_df = elevation_data,
    hill_df = hillshade_data
  )
}


build_relief_projected <- function(
    spatial_object_projected,
    zoom,
    buffer_m = 30000,
    shadow_max = 0.22
) {
  
  object_buffer <- spatial_object_projected |>
    sf::st_union() |>
    sf::st_buffer(
      buffer_m
    ) |>
    sf::st_as_sf()
  
  object_buffer_geographic <- sf::st_transform(
    object_buffer,
    4674
  )
  
  elevation_source <- tryCatch(
    elevatr::get_elev_raster(
      locations = object_buffer_geographic,
      z = zoom,
      clip = "locations",
      override_size_check = TRUE
    ),
    error = function(e) {
      
      stop(
        "The elevation model could not be downloaded. ",
        "Check the internet connection. Original error: ",
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
      spatial_object_projected
    )$wkt
  )
  
  object_vector <- terra::vect(
    spatial_object_projected
  )
  
  elevation_raster <- terra::crop(
    elevation_raster,
    object_vector
  )
  
  elevation_raster <- terra::mask(
    elevation_raster,
    object_vector
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
    dem_df = elevation_data,
    hill_df = hillshade_data
  )
}


make_legend_panel <- function(
    elevation_values,
    hypsometric_palette,
    contour_interval
) {
  
  elevation_range <- range(
    elevation_values,
    na.rm = TRUE
  )
  
  breaks <- pretty(
    elevation_range,
    n = 5
  )
  
  breaks <- unique(
    sort(breaks)
  )
  
  breaks <- breaks[
    breaks >= floor(
      elevation_range[1]
    ) &
      breaks <= ceiling(
        elevation_range[2]
      )
  ]
  
  if (length(breaks) < 2) {
    
    breaks <- c(
      floor(
        elevation_range[1]
      ),
      ceiling(
        elevation_range[2]
      )
    )
  }
  
  if (
    min(breaks) >
    elevation_range[1]
  ) {
    
    breaks <- c(
      floor(
        elevation_range[1]
      ),
      breaks
    )
  }
  
  if (
    max(breaks) <
    elevation_range[2]
  ) {
    
    breaks <- c(
      breaks,
      ceiling(
        elevation_range[2]
      )
    )
  }
  
  breaks <- unique(
    sort(breaks)
  )
  
  if (length(breaks) > 6) {
    
    positions <- round(
      seq(
        1,
        length(breaks),
        length.out = 6
      )
    )
    
    breaks <- unique(
      sort(
        breaks[positions]
      )
    )
  }
  
  legend_colors <- grDevices::colorRampPalette(
    hypsometric_palette
  )(
    length(breaks) - 1
  )
  
  step_x <- 1.35
  rectangle_width <- 1.05
  
  legend_data <- data.frame(
    xmin = seq(
      0,
      by = step_x,
      length.out = length(legend_colors)
    ),
    xmax = seq(
      rectangle_width,
      by = step_x,
      length.out = length(legend_colors)
    ),
    xmid = seq(
      rectangle_width / 2,
      by = step_x,
      length.out = length(legend_colors)
    ),
    fill_color = legend_colors,
    label = paste0(
      round(
        breaks[-length(breaks)]
      ),
      "–",
      round(
        breaks[-1]
      ),
      " m"
    )
  )
  
  maximum_x <- max(
    legend_data$xmax
  ) + 8
  
  ggplot() +
    theme_void() +
    coord_cartesian(
      xlim = c(
        0,
        maximum_x
      ),
      ylim = c(
        0,
        4.4
      ),
      clip = "off"
    ) +
    annotate(
      "text",
      x = 0,
      y = 4,
      label = "Legend",
      hjust = 0,
      fontface = "bold",
      size = 5.2
    ) +
    annotate(
      "text",
      x = 0,
      y = 3.15,
      label = "Elevation (m)",
      hjust = 0,
      fontface = "bold",
      size = 4.2
    ) +
    geom_rect(
      data = legend_data,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = 2.25,
        ymax = 2.68,
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
        y = 1.72,
        label = label
      ),
      size = 2.7,
      inherit.aes = FALSE
    ) +
    annotate(
      "segment",
      x = max(
        legend_data$xmax
      ) + 0.7,
      xend = max(
        legend_data$xmax
      ) + 1.55,
      y = 2.46,
      yend = 2.46,
      color = scales::alpha(
        "#7A2D1B",
        0.55
      ),
      linewidth = 0.70
    ) +
    annotate(
      "text",
      x = max(
        legend_data$xmax
      ) + 1.80,
      y = 2.46,
      label = paste0(
        "Contour interval in panel C: ",
        contour_interval,
        " m"
      ),
      hjust = 0,
      size = 3.5
    ) +
    annotate(
      "point",
      x = max(
        legend_data$xmax
      ) + 1.10,
      y = 1.50,
      shape = 21,
      size = 3.3,
      fill = "grey20",
      color = "white",
      stroke = 0.6
    ) +
    annotate(
      "text",
      x = max(
        legend_data$xmax
      ) + 1.80,
      y = 1.50,
      label = "Representative point in panel C",
      hjust = 0,
      size = 3.5
    ) +
    theme(
      plot.margin = margin(
        0,
        0,
        0,
        0
      )
    )
}


make_caption_panel <- function(
    caption_text
) {
  
  ggplot() +
    theme_void() +
    annotate(
      "text",
      x = 0,
      y = 1,
      label = caption_text,
      hjust = 0,
      vjust = 1,
      size = 3.7,
      lineheight = 1.03
    ) +
    coord_cartesian(
      xlim = c(
        0,
        1
      ),
      ylim = c(
        0,
        1
      ),
      expand = FALSE,
      clip = "off"
    ) +
    theme(
      plot.margin = margin(
        0,
        0,
        0,
        0
      )
    )
}


read_protected_area_file <- function(path) {
  
  protected_area <- tryCatch(
    suppressWarnings(
      sf::st_read(
        path,
        quiet = TRUE,
        options = "ENCODING=UTF-8"
      )
    ),
    error = function(e) {
      
      message(
        "The spatial file could not be read: ",
        basename(path),
        ". ",
        conditionMessage(e)
      )
      
      NULL
    }
  )
  
  if (
    is.null(protected_area) ||
    nrow(protected_area) == 0
  ) {
    
    return(
      NULL
    )
  }
  
  if (
    is.na(
      sf::st_crs(
        protected_area
      )
    )
  ) {
    
    stop(
      "The spatial file has no coordinate reference system: ",
      basename(path)
    )
  }
  
  names(protected_area) <- clean_names_safe(
    names(protected_area)
  )
  
  protected_area <- protected_area |>
    sf::st_make_valid() |>
    sf::st_transform(
      4674
    )
  
  area_from_filename <- identify_area_id(
    basename(path)
  )
  
  area_from_filename <- area_from_filename[
    !is.na(area_from_filename)
  ]
  
  if (length(area_from_filename) == 1) {
    
    combined_geometry <- sf::st_union(
      sf::st_geometry(
        protected_area
      )
    )
    
    return(
      sf::st_sf(
        area_id = area_from_filename,
        geometry = combined_geometry
      )
    )
  }
  
  attribute_data <- sf::st_drop_geometry(
    protected_area
  )
  
  character_columns <- names(attribute_data)[
    vapply(
      attribute_data,
      function(column) {
        
        is.character(column) ||
          is.factor(column)
      },
      logical(1)
    )
  ]
  
  if (length(character_columns) == 0) {
    
    return(
      NULL
    )
  }
  
  attribute_text <- apply(
    as.data.frame(
      attribute_data[
        ,
        character_columns,
        drop = FALSE
      ],
      stringsAsFactors = FALSE
    ),
    1,
    function(values) {
      
      paste(
        values,
        collapse = " | "
      )
    }
  )
  
  protected_area$area_id <- identify_area_id(
    attribute_text
  )
  
  protected_area <- protected_area |>
    dplyr::filter(
      !is.na(area_id)
    )
  
  if (nrow(protected_area) == 0) {
    
    return(
      NULL
    )
  }
  
  protected_area |>
    dplyr::select(
      area_id
    ) |>
    dplyr::group_by(
      area_id
    ) |>
    dplyr::summarise(
      do_union = TRUE,
      .groups = "drop"
    ) |>
    sf::st_make_valid()
}


load_local_protected_areas <- function(
    analysis_directory
) {
  
  possible_directories <- unique(
    c(
      file.path(
        analysis_directory,
        "shapes_ucs_ms"
      ),
      file.path(
        analysis_directory,
        "sha"
      ),
      file.path(
        analysis_directory,
        "data",
        "shapes_ucs_ms"
      ),
      analysis_directory
    )
  )
  
  possible_directories <- possible_directories[
    dir.exists(
      possible_directories
    )
  ]
  
  spatial_files <- unique(
    unlist(
      lapply(
        possible_directories,
        function(directory) {
          
          list.files(
            path = directory,
            pattern = "\\.(geojson|gpkg|shp)$",
            full.names = TRUE,
            recursive = TRUE,
            ignore.case = TRUE
          )
        }
      ),
      use.names = FALSE
    )
  )
  
  spatial_files <- spatial_files[
    file.exists(spatial_files)
  ]
  
  if (length(spatial_files) == 0) {
    
    return(
      NULL
    )
  }
  
  relevant_files <- spatial_files[
    grepl(
      paste0(
        "baia.?negra|",
        "bodoquena|",
        "ivinhema|",
        "pevri|",
        "ucs_ms_3_areas"
      ),
      basename(spatial_files),
      ignore.case = TRUE
    )
  ]
  
  if (length(relevant_files) > 0) {
    
    spatial_files <- relevant_files
  }
  
  protected_area_objects <- lapply(
    spatial_files,
    read_protected_area_file
  )
  
  protected_area_objects <- protected_area_objects[
    !vapply(
      protected_area_objects,
      is.null,
      logical(1)
    )
  ]
  
  if (length(protected_area_objects) == 0) {
    
    return(
      NULL
    )
  }
  
  area_ids <- unlist(
    lapply(
      protected_area_objects,
      function(object) {
        
        as.character(
          object$area_id
        )
      }
    ),
    use.names = FALSE
  )
  
  geometry_collection <- do.call(
    c,
    lapply(
      protected_area_objects,
      sf::st_geometry
    )
  )
  
  sf::st_crs(
    geometry_collection
  ) <- 4674
  
  protected_areas <- sf::st_sf(
    area_id = area_ids,
    geometry = geometry_collection
  )
  
  protected_areas <- protected_areas |>
    dplyr::filter(
      area_id %in% c(
        "pantanal",
        "cerrado",
        "floodplain"
      )
    ) |>
    dplyr::group_by(
      area_id
    ) |>
    dplyr::summarise(
      do_union = TRUE,
      .groups = "drop"
    ) |>
    sf::st_make_valid()
  
  required_areas <- c(
    "pantanal",
    "cerrado",
    "floodplain"
  )
  
  missing_areas <- setdiff(
    required_areas,
    as.character(
      protected_areas$area_id
    )
  )
  
  if (length(missing_areas) > 0) {
    
    message(
      "The local spatial files did not contain: ",
      paste(
        missing_areas,
        collapse = ", "
      )
    )
    
    return(
      NULL
    )
  }
  
  message(
    "Protected-area polygons loaded from the supplementary files."
  )
  
  protected_areas
}


load_geobr_protected_areas <- function() {
  
  message(
    "Local protected-area files were not complete. ",
    "Attempting to use the geobr conservation-unit dataset."
  )
  
  conservation_units <- tryCatch(
    geobr::read_conservation_units(
      date = 201909,
      simplified = FALSE,
      showProgress = FALSE,
      cache = TRUE
    ),
    error = function(e) {
      
      stop(
        "The protected-area polygons could not be loaded. ",
        "Include the three spatial files in the 'shapes_ucs_ms' ",
        "directory. Original error: ",
        conditionMessage(e)
      )
    }
  )
  
  names(conservation_units) <- clean_names_safe(
    names(conservation_units)
  )
  
  conservation_units <- conservation_units |>
    sf::st_make_valid() |>
    sf::st_transform(
      4674
    )
  
  attribute_data <- sf::st_drop_geometry(
    conservation_units
  )
  
  character_columns <- names(attribute_data)[
    vapply(
      attribute_data,
      function(column) {
        
        is.character(column) ||
          is.factor(column)
      },
      logical(1)
    )
  ]
  
  if (length(character_columns) == 0) {
    
    stop(
      "The protected areas could not be identified in the ",
      "geobr conservation-unit dataset."
    )
  }
  
  search_text <- apply(
    as.data.frame(
      attribute_data[
        ,
        character_columns,
        drop = FALSE
      ],
      stringsAsFactors = FALSE
    ),
    1,
    function(values) {
      
      paste(
        values,
        collapse = " | "
      )
    }
  )
  
  conservation_units$area_id <- identify_area_id(
    search_text
  )
  
  protected_areas <- conservation_units |>
    dplyr::filter(
      !is.na(area_id)
    )
  
  protected_areas_projected <- sf::st_transform(
    protected_areas,
    5880
  )
  
  protected_areas_projected$area_m2 <- as.numeric(
    sf::st_area(
      protected_areas_projected
    )
  )
  
  protected_areas <- protected_areas_projected |>
    dplyr::group_by(
      area_id
    ) |>
    dplyr::slice_max(
      order_by = area_m2,
      n = 1,
      with_ties = FALSE
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      area_id
    ) |>
    sf::st_transform(
      4674
    )
  
  required_areas <- c(
    "pantanal",
    "cerrado",
    "floodplain"
  )
  
  missing_areas <- setdiff(
    required_areas,
    as.character(
      protected_areas$area_id
    )
  )
  
  if (length(missing_areas) > 0) {
    
    stop(
      "The three protected areas could not be identified. ",
      "Missing areas: ",
      paste(
        missing_areas,
        collapse = ", "
      ),
      "."
    )
  }
  
  protected_areas
}


load_protected_areas <- function(
    analysis_directory
) {
  
  local_areas <- load_local_protected_areas(
    analysis_directory
  )
  
  if (!is.null(local_areas)) {
    
    return(
      local_areas
    )
  }
  
  load_geobr_protected_areas()
}


make_protected_area_inset <- function(
    polygon,
    fill_color
) {
  
  polygon_projected <- sf::st_transform(
    polygon,
    5880
  )
  
  polygon_bbox <- sf::st_bbox(
    polygon_projected
  )
  
  polygon_width <- as.numeric(
    polygon_bbox["xmax"] -
      polygon_bbox["xmin"]
  )
  
  polygon_height <- as.numeric(
    polygon_bbox["ymax"] -
      polygon_bbox["ymin"]
  )
  
  padding_x <- max(
    polygon_width * 0.10,
    3000
  )
  
  padding_y <- max(
    polygon_height * 0.10,
    3000
  )
  
  x_limits <- c(
    polygon_bbox["xmin"] - padding_x,
    polygon_bbox["xmax"] + padding_x
  )
  
  y_limits <- c(
    polygon_bbox["ymin"] - padding_y,
    polygon_bbox["ymax"] + padding_y
  )
  
  ggplot() +
    geom_sf(
      data = polygon_projected,
      fill = scales::alpha(
        fill_color,
        0.18
      ),
      color = fill_color,
      linewidth = 1.05
    ) +
    geom_sf(
      data = sf::st_boundary(
        polygon_projected
      ),
      color = fill_color,
      linewidth = 0.55
    ) +
    coord_sf(
      xlim = x_limits,
      ylim = y_limits,
      expand = FALSE,
      crs = sf::st_crs(
        polygon_projected
      )
    ) +
    theme_minimal(
      base_size = 10
    ) +
    theme(
      panel.grid.major = element_line(
        color = "grey90",
        linewidth = 0.22
      ),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.margin = margin(
        0,
        0,
        0,
        0
      ),
      plot.background = element_rect(
        fill = "white",
        color = "grey70",
        linewidth = 0.45
      ),
      panel.background = element_rect(
        fill = "white",
        color = NA
      )
    )
}


hypsometric_colors <- c(
  "#1C8D8A",
  "#1FA45B",
  "#6EAE43",
  "#C6B46C",
  "#A36835",
  "#7A2D1B",
  "#EFE7DA"
)


landscape_colors <- c(
  "pantanal" = "#2BAE66",
  "cerrado" = "#B86B1B",
  "floodplain" = "#3C89FF"
)


area_titles <- c(
  "pantanal" = paste0(
    "APA da Baía Negra\n",
    "(Pantanal 5.420,58 hectares)"
  ),
  "cerrado" = paste0(
    "Parque Nacional da Serra da Bodoquena\n",
    "(Cerrado 77.021,58 hectares)"
  ),
  "floodplain" = paste0(
    "Parque Estadual das Várzeas do Rio Ivinhema\n",
    "(Floodplain 73.345,15 hectares)"
  )
)


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


geographic_data <- occurrence_data |>
  dplyr::mutate(
    point = trimws(
      point_values
    ),
    point_key = normalize_text_lower(
      point_values
    ),
    landscape = dplyr::case_when(
      !is.na(landscape_values) &
        trimws(landscape_values) != "" ~
        normalize_text_lower(
          landscape_values
        ),
      
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


if (nrow(geographic_data) == 0) {
  
  stop(
    "No valid georeferenced records were found."
  )
}


cat(
  "Valid georeferenced rows:",
  nrow(geographic_data),
  "\n\n"
)


outlier_threshold_km <- 100


sampling_points <- geographic_data |>
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
  sampling_points
)


sampling_points$x <- projected_coordinates[, 1]
sampling_points$y <- projected_coordinates[, 2]


sampling_points <- sampling_points |>
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


sampling_points_geographic <- sf::st_transform(
  sampling_points,
  4674
)


representative_points <- sampling_points_geographic |>
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


protected_areas <- load_protected_areas(
  script_directory
)


protected_areas <- protected_areas |>
  sf::st_make_valid() |>
  dplyr::mutate(
    area_id = factor(
      area_id,
      levels = c(
        "pantanal",
        "cerrado",
        "floodplain"
      )
    ),
    area_label = dplyr::recode(
      as.character(area_id),
      !!!as.list(area_titles)
    )
  ) |>
  dplyr::arrange(
    area_id
  )


polygon_points <- protected_areas |>
  sf::st_transform(
    5880
  ) |>
  sf::st_point_on_surface() |>
  sf::st_transform(
    4674
  )


polygon_coordinates <- cbind(
  sf::st_drop_geometry(
    polygon_points
  ),
  sf::st_coordinates(
    polygon_points
  )
) |>
  dplyr::mutate(
    area_id = as.character(
      area_id
    )
  ) |>
  dplyr::select(
    area_id,
    X,
    Y
  )


representative_coordinates <- cbind(
  sf::st_drop_geometry(
    representative_points
  ),
  sf::st_coordinates(
    representative_points
  )
) |>
  dplyr::mutate(
    area_id = as.character(
      landscape
    )
  ) |>
  dplyr::select(
    area_id,
    X_rep = X,
    Y_rep = Y
  )


anchor_coordinates <- polygon_coordinates |>
  dplyr::left_join(
    representative_coordinates,
    by = "area_id"
  ) |>
  dplyr::mutate(
    X = dplyr::coalesce(
      X,
      X_rep
    ),
    Y = dplyr::coalesce(
      Y,
      Y_rep
    )
  ) |>
  dplyr::select(
    area_id,
    X,
    Y
  )


anchor_points <- sf::st_as_sf(
  anchor_coordinates,
  coords = c(
    "X",
    "Y"
  ),
  crs = 4674
)


south_america <- rnaturalearth::ne_countries(
  continent = "South America",
  scale = "medium",
  returnclass = "sf"
) |>
  sf::st_transform(
    4674
  )


south_america_union <- sf::st_sf(
  geometry = sf::st_combine(
    sf::st_geometry(
      south_america
    )
  )
)


brazil <- south_america |>
  dplyr::filter(
    admin == "Brazil"
  )


brazil_union <- sf::st_sf(
  geometry = sf::st_combine(
    sf::st_geometry(
      brazil
    )
  )
)


brazilian_states <- tryCatch(
  geobr::read_state(
    year = 2020,
    showProgress = FALSE
  ),
  error = function(e) {
    
    stop(
      "The Brazilian state boundaries could not be downloaded. ",
      "Check the internet connection. Original error: ",
      conditionMessage(e)
    )
  }
)


mato_grosso_do_sul <- brazilian_states |>
  dplyr::filter(
    abbrev_state == "MS"
  )


if (nrow(mato_grosso_do_sul) == 0) {
  
  stop(
    "Mato Grosso do Sul was not found in the state dataset."
  )
}


mato_grosso_do_sul_projected <- sf::st_transform(
  mato_grosso_do_sul,
  5880
)


protected_areas_projected <- sf::st_transform(
  protected_areas,
  5880
)


anchor_points_projected <- sf::st_transform(
  anchor_points,
  5880
)


anchor_projected_coordinates <- cbind(
  sf::st_drop_geometry(
    anchor_points_projected
  ),
  sf::st_coordinates(
    anchor_points_projected
  )
)


names(anchor_projected_coordinates)[
  names(anchor_projected_coordinates) == "X"
] <- "Xp"


names(anchor_projected_coordinates)[
  names(anchor_projected_coordinates) == "Y"
] <- "Yp"


cat(
  "Downloading the South America elevation model...\n"
)


relief_south_america <- build_relief_geographic(
  south_america_union,
  zoom = 2,
  buffer_m = 120000,
  shadow_max = 0.18
)


cat(
  "Downloading the Brazil elevation model...\n"
)


relief_brazil <- build_relief_geographic(
  brazil_union,
  zoom = 3,
  buffer_m = 80000,
  shadow_max = 0.20
)


cat(
  "Downloading the Mato Grosso do Sul elevation model...\n"
)


relief_ms <- build_relief_projected(
  mato_grosso_do_sul_projected,
  zoom = 7,
  buffer_m = 30000,
  shadow_max = 0.24
)


cat(
  "Elevation models loaded successfully.\n\n"
)


legend_plot <- make_legend_panel(
  relief_ms$dem_df$elevation,
  hypsometric_colors,
  contour_interval_ms
)


caption_line_1 <- paste0(
  "Coordinate system / datum: Geographic coordinates ",
  "(latitude/longitude), SIRGAS 2000 (EPSG:4674)."
)


caption_line_2 <- paste0(
  "Hypsometric tint and hillshade derived from DEM obtained ",
  "via the elevatr package and processed in terra."
)


caption_line_3 <- paste0(
  "Administrative limits: Natural Earth and geobr. ",
  "Protected areas: official MMA/CNUC polygons when available ",
  "(fallback: geobr)."
)


caption_line_4 <- paste0(
  "Contour interval in panel C: ",
  contour_interval_ms,
  " m. Sampling data: ",
  sampling_period,
  ". Cartographic elaboration: ",
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


caption_plot <- make_caption_panel(
  caption_text
)


south_america_plot <- ggplot() +
  geom_raster(
    data = relief_south_america$dem_df,
    aes(
      x = x,
      y = y,
      fill = elevation
    ),
    interpolate = TRUE
  ) +
  scale_fill_gradientn(
    colours = hypsometric_colors,
    guide = "none",
    na.value = "transparent"
  ) +
  geom_raster(
    data = relief_south_america$hill_df,
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
    data = south_america,
    fill = NA,
    color = "grey70",
    linewidth = 0.22
  ) +
  geom_sf(
    data = brazil,
    fill = NA,
    color = "black",
    linewidth = 0.55
  ) +
  coord_sf(
    expand = FALSE
  ) +
  theme_void(
    base_size = 11
  ) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = "grey80",
      linewidth = 0.35
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.margin = margin(
      0,
      0,
      0,
      0
    )
  )


brazil_plot <- ggplot() +
  geom_raster(
    data = relief_brazil$dem_df,
    aes(
      x = x,
      y = y,
      fill = elevation
    ),
    interpolate = TRUE
  ) +
  scale_fill_gradientn(
    colours = hypsometric_colors,
    guide = "none",
    na.value = "transparent"
  ) +
  geom_raster(
    data = relief_brazil$hill_df,
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
    data = brazil_union,
    fill = NA,
    color = "grey55",
    linewidth = 0.35
  ) +
  geom_sf(
    data = mato_grosso_do_sul,
    fill = NA,
    color = "black",
    linewidth = 0.65
  ) +
  coord_sf(
    expand = FALSE
  ) +
  theme_void(
    base_size = 11
  ) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = "grey80",
      linewidth = 0.35
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.margin = margin(
      0,
      0,
      0,
      0
    )
  )


ms_bbox <- sf::st_bbox(
  mato_grosso_do_sul_projected
)


ms_plot <- ggplot() +
  geom_raster(
    data = relief_ms$dem_df,
    aes(
      x = x,
      y = y,
      fill = elevation
    ),
    interpolate = TRUE
  ) +
  scale_fill_gradientn(
    colours = hypsometric_colors,
    guide = "none",
    na.value = "transparent"
  ) +
  geom_raster(
    data = relief_ms$hill_df,
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
  geom_contour(
    data = relief_ms$dem_df,
    aes(
      x = x,
      y = y,
      z = elevation
    ),
    binwidth = contour_interval_ms,
    color = scales::alpha(
      "#7A2D1B",
      0.22
    ),
    linewidth = 0.16
  ) +
  geom_sf(
    data = mato_grosso_do_sul_projected,
    fill = NA,
    color = "grey30",
    linewidth = 0.60
  ) +
  ggnewscale::new_scale_fill() +
  geom_sf(
    data = protected_areas_projected,
    aes(
      fill = area_id
    ),
    color = NA,
    alpha = 0.10
  ) +
  geom_sf(
    data = protected_areas_projected,
    aes(
      color = area_id
    ),
    fill = NA,
    linewidth = 1.05,
    show.legend = FALSE
  ) +
  geom_point(
    data = anchor_projected_coordinates,
    aes(
      x = Xp,
      y = Yp,
      fill = area_id
    ),
    shape = 21,
    size = 3.8,
    color = "white",
    stroke = 0.90
  ) +
  ggspatial::annotation_scale(
    location = "bl",
    width_hint = 0.22,
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
      1,
      "cm"
    ),
    width = grid::unit(
      1,
      "cm"
    )
  ) +
  scale_fill_manual(
    values = landscape_colors,
    guide = "none"
  ) +
  scale_color_manual(
    values = landscape_colors,
    guide = "none"
  ) +
  coord_sf(
    crs = sf::st_crs(
      mato_grosso_do_sul_projected
    ),
    xlim = c(
      ms_bbox["xmin"],
      ms_bbox["xmax"]
    ),
    ylim = c(
      ms_bbox["ymin"],
      ms_bbox["ymax"]
    ),
    expand = FALSE,
    clip = "on"
  ) +
  theme_void(
    base_size = 11
  ) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = "grey80",
      linewidth = 0.35
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.margin = margin(
      0,
      0,
      0,
      0
    )
  )


protected_area_split <- split(
  protected_areas,
  as.character(
    protected_areas$area_id
  )
)


pantanal_inset <- make_protected_area_inset(
  protected_area_split[["pantanal"]],
  landscape_colors[["pantanal"]]
)


cerrado_inset <- make_protected_area_inset(
  protected_area_split[["cerrado"]],
  landscape_colors[["cerrado"]]
)


floodplain_inset <- make_protected_area_inset(
  protected_area_split[["floodplain"]],
  landscape_colors[["floodplain"]]
)


box_A <- c(
  x = 0.05,
  y = 0.73,
  w = 0.17,
  h = 0.16
)


box_B <- c(
  x = 0.25,
  y = 0.73,
  w = 0.17,
  h = 0.16
)


ms_aspect <- as.numeric(
  (
    ms_bbox["ymax"] -
      ms_bbox["ymin"]
  ) /
    (
      ms_bbox["xmax"] -
        ms_bbox["xmin"]
    )
)


box_C_width <- 0.36
box_C_height <- box_C_width * ms_aspect

maximum_box_C_height <- 0.33


if (box_C_height > maximum_box_C_height) {
  
  box_C_height <- maximum_box_C_height
  box_C_width <- box_C_height / ms_aspect
}


box_C <- c(
  x = 0.06,
  y = 0.20,
  w = box_C_width,
  h = box_C_height
)


box_pantanal <- c(
  x = 0.73,
  y = 0.67,
  w = 0.21,
  h = 0.15
)


box_cerrado <- c(
  x = 0.73,
  y = 0.44,
  w = 0.21,
  h = 0.15
)


box_floodplain <- c(
  x = 0.73,
  y = 0.21,
  w = 0.21,
  h = 0.15
)


box_legend <- c(
  x = 0.08,
  y = 0.102,
  w = 0.84,
  h = 0.086
)


box_caption <- c(
  x = 0.028,
  y = 0.012,
  w = 0.945,
  h = 0.062
)


title_A_y <- box_A["y"] +
  box_A["h"] +
  0.018


title_B_y <- box_B["y"] +
  box_B["h"] +
  0.018


title_C_y <- box_C["y"] +
  box_C["h"] +
  0.058


subtitle_C_y <- title_C_y -
  0.024


title_pantanal_y <- box_pantanal["y"] +
  box_pantanal["h"] +
  0.014


title_cerrado_y <- box_cerrado["y"] +
  box_cerrado["h"] +
  0.014


title_floodplain_y <- box_floodplain["y"] +
  box_floodplain["h"] +
  0.014


title_A_x <- box_A["x"] +
  box_A["w"] / 2 +
  0.02


title_B_x <- box_B["x"] +
  box_B["w"] / 2 +
  0.02


title_C_x <- box_C["x"] +
  0.018


subtitle_C_x <- box_C["x"] +
  0.018


title_pantanal_x <- box_pantanal["x"] +
  box_pantanal["w"] / 2


title_cerrado_x <- box_cerrado["x"] +
  box_cerrado["w"] / 2


title_floodplain_x <- box_floodplain["x"] +
  box_floodplain["w"] / 2


tag_A_x <- box_A["x"] -
  0.002


tag_B_x <- box_B["x"] -
  0.002


tag_C_x <- box_C["x"] -
  0.020


tag_A_y <- title_A_y
tag_B_y <- title_B_y
tag_C_y <- title_C_y + 0.004


target_centers <- data.frame(
  area_id = c(
    "pantanal",
    "cerrado",
    "floodplain"
  ),
  xend = c(
    box_pantanal["x"],
    box_cerrado["x"],
    box_floodplain["x"]
  ),
  yend = c(
    box_pantanal["y"] +
      box_pantanal["h"] / 2,
    
    box_cerrado["y"] +
      box_cerrado["h"] / 2,
    
    box_floodplain["y"] +
      box_floodplain["h"] / 2
  )
)


connector_data <- anchor_projected_coordinates |>
  dplyr::mutate(
    area_id = as.character(
      area_id
    )
  ) |>
  dplyr::left_join(
    target_centers,
    by = "area_id"
  ) |>
  dplyr::mutate(
    x_normalized = (
      Xp -
        ms_bbox["xmin"]
    ) /
      (
        ms_bbox["xmax"] -
          ms_bbox["xmin"]
      ),
    
    y_normalized = (
      Yp -
        ms_bbox["ymin"]
    ) /
      (
        ms_bbox["ymax"] -
          ms_bbox["ymin"]
      ),
    
    x = box_C["x"] +
      box_C["w"] *
      x_normalized,
    
    y = box_C["y"] +
      box_C["h"] *
      y_normalized,
    
    xend = xend + 0.001
  )


location_figure <- cowplot::ggdraw() +
  cowplot::draw_plot(
    south_america_plot,
    x = box_A["x"],
    y = box_A["y"],
    width = box_A["w"],
    height = box_A["h"]
  ) +
  cowplot::draw_plot(
    brazil_plot,
    x = box_B["x"],
    y = box_B["y"],
    width = box_B["w"],
    height = box_B["h"]
  ) +
  cowplot::draw_plot(
    ms_plot,
    x = box_C["x"],
    y = box_C["y"],
    width = box_C["w"],
    height = box_C["h"]
  ) +
  cowplot::draw_plot(
    pantanal_inset,
    x = box_pantanal["x"],
    y = box_pantanal["y"],
    width = box_pantanal["w"],
    height = box_pantanal["h"]
  ) +
  cowplot::draw_plot(
    cerrado_inset,
    x = box_cerrado["x"],
    y = box_cerrado["y"],
    width = box_cerrado["w"],
    height = box_cerrado["h"]
  ) +
  cowplot::draw_plot(
    floodplain_inset,
    x = box_floodplain["x"],
    y = box_floodplain["y"],
    width = box_floodplain["w"],
    height = box_floodplain["h"]
  ) +
  cowplot::draw_plot(
    legend_plot,
    x = box_legend["x"],
    y = box_legend["y"],
    width = box_legend["w"],
    height = box_legend["h"]
  ) +
  cowplot::draw_plot(
    caption_plot,
    x = box_caption["x"],
    y = box_caption["y"],
    width = box_caption["w"],
    height = box_caption["h"]
  )


for (i in seq_len(
  nrow(connector_data)
)) {
  
  area_name <- as.character(
    connector_data$area_id[i]
  )
  
  line_color <- unname(
    landscape_colors[area_name]
  )
  
  if (
    length(line_color) == 0 ||
    is.na(line_color) ||
    is.na(connector_data$x[i]) ||
    is.na(connector_data$y[i]) ||
    is.na(connector_data$xend[i]) ||
    is.na(connector_data$yend[i])
  ) {
    
    next
  }
  
  location_figure <- location_figure +
    cowplot::draw_line(
      x = c(
        connector_data$x[i],
        connector_data$xend[i]
      ),
      y = c(
        connector_data$y[i],
        connector_data$yend[i]
      ),
      color = scales::alpha(
        line_color,
        0.65
      ),
      linewidth = 1,
      lineend = "round"
    )
}


location_figure <- location_figure +
  cowplot::draw_label(
    "A",
    x = tag_A_x,
    y = tag_A_y,
    hjust = 0,
    fontface = "bold",
    size = 16
  ) +
  cowplot::draw_label(
    "South America",
    x = title_A_x,
    y = title_A_y,
    fontface = "bold",
    size = 13
  ) +
  cowplot::draw_label(
    "B",
    x = tag_B_x,
    y = tag_B_y,
    hjust = 0,
    fontface = "bold",
    size = 16
  ) +
  cowplot::draw_label(
    "Brazil",
    x = title_B_x,
    y = title_B_y,
    fontface = "bold",
    size = 13
  ) +
  cowplot::draw_label(
    "C",
    x = tag_C_x,
    y = tag_C_y,
    hjust = 0,
    fontface = "bold",
    size = 16
  ) +
  cowplot::draw_label(
    main_title,
    x = title_C_x,
    y = title_C_y,
    hjust = 0,
    fontface = "bold",
    size = 13.8
  ) +
  cowplot::draw_label(
    main_subtitle,
    x = subtitle_C_x,
    y = subtitle_C_y,
    hjust = 0,
    size = 10.2
  ) +
  cowplot::draw_label(
    area_titles[["pantanal"]],
    x = title_pantanal_x,
    y = title_pantanal_y,
    fontface = "bold",
    size = 10.3,
    lineheight = 0.93
  ) +
  cowplot::draw_label(
    area_titles[["cerrado"]],
    x = title_cerrado_x,
    y = title_cerrado_y,
    fontface = "bold",
    size = 9.7,
    lineheight = 0.93
  ) +
  cowplot::draw_label(
    area_titles[["floodplain"]],
    x = title_floodplain_x,
    y = title_floodplain_y,
    fontface = "bold",
    size = 9.4,
    lineheight = 0.93
  )


print(
  location_figure
)


output_png <- file.path(
  output_directory,
  "Figure_StudyArea_Location_100FINAL.png"
)


output_pdf <- file.path(
  output_directory,
  "Figure_StudyArea_Location_100FINAL.pdf"
)


cat(
  "Saving PNG...\n"
)


ggplot2::ggsave(
  filename = output_png,
  plot = location_figure,
  width = 11.2,
  height = 11.2,
  units = "in",
  dpi = 600,
  bg = "white"
)


cat(
  "Saving PDF...\n"
)


pdf_device <- if (
  capabilities("cairo")
) {
  grDevices::cairo_pdf
} else {
  grDevices::pdf
}


ggplot2::ggsave(
  filename = output_pdf,
  plot = location_figure,
  width = 11.2,
  height = 11.2,
  units = "in",
  device = pdf_device,
  bg = "white"
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
  "Valid georeferenced rows:",
  nrow(geographic_data),
  "\n"
)


cat(
  "Rows after geographic outlier removal:",
  nrow(sampling_points),
  "\n"
)


cat(
  "Protected areas:",
  paste(
    as.character(
      protected_areas$area_id
    ),
    collapse = ", "
  ),
  "\n"
)


cat(
  "PNG exists: ",
  file.exists(output_png),
  "\n",
  sep = ""
)


cat(
  "PDF exists: ",
  file.exists(output_pdf),
  "\n",
  sep = ""
)


if (file.exists(output_png)) {
  
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
}


if (file.exists(output_pdf)) {
  
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
}