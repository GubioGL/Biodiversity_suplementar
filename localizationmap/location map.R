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
  "ragg",
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


# Text is measured with the same engine used to save the PNG, so the
# layout matches the saved file.
cowplot::set_null_device("agg")


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
      "^dados_utf8(\\.csv)?\\.csv$",
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

output_directory <- file.path(
  analysis_directory,
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
    "(Pantanal · 5,420.58 ha)"
  ),
  "cerrado" = paste0(
    "Parque Nacional da Serra da Bodoquena\n",
    "(Cerrado · 77,021.58 ha)"
  ),
  "floodplain" = paste0(
    "Parque Estadual das Várzeas do Rio Ivinhema\n",
    "(Floodplain · 73,345.15 ha)"
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
  analysis_directory
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


figure_width_in <- 190 / 25.4

output_suffix <- "v2_redesign"


font_family <- "Arial"


font_size <- c(
  tag = 11,
  panel_title = 9.5,
  inset_name = 8,
  inset_info = 7.5,
  axis = 7,
  context = 7,
  legend_title = 8.5,
  legend_text = 7.5
)


text_pt <- function(points) {
  points / ggplot2::.pt
}


inches_to_x <- function(inches) {
  inches / figure_width_in
}


inches_to_y <- function(inches) {
  inches / figure_height_in
}


elevation_limits <- c(
  floor(
    stats::quantile(
      relief_ms$dem_df$elevation,
      0.0001,
      names = FALSE
    )
  ),
  ceiling(
    max(
      relief_ms$dem_df$elevation,
      na.rm = TRUE
    )
  )
)


ms_palette_colors <- hypsometric_colors[-1]

ms_palette_stops <- c(
  elevation_limits[1],
  125,
  325,
  525,
  725,
  elevation_limits[2]
)

ms_palette_values <- scales::rescale(
  ms_palette_stops,
  from = elevation_limits
)


elevation_fill_scale <- function() {
  
  scale_fill_gradientn(
    colours = ms_palette_colors,
    values = ms_palette_values,
    limits = elevation_limits,
    oob = scales::squish,
    na.value = "transparent",
    guide = "none"
  )
}


highlight_color <- "#C0392B"


area_letters <- c(
  pantanal = "d",
  cerrado = "e",
  floodplain = "f"
)


inset_titles <- list(
  pantanal = c(
    name = "APA da Baía Negra",
    info = "Pantanal · 5,420.58 ha"
  ),
  cerrado = c(
    name = "Parque Nacional da\nSerra da Bodoquena",
    info = "Cerrado · 77,021.58 ha"
  ),
  floodplain = c(
    name = "Parque Estadual das\nVárzeas do Rio\nIvinhema",
    info = "Floodplain · 73,345.15 ha"
  )
)


protected_area_split <- split(
  protected_areas,
  as.character(
    protected_areas$area_id
  )
)


blank_map_theme <- theme_void(
  base_family = font_family
) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
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


# ---- A and B: simple locator maps ----


south_america_plot <- ggplot() +
  geom_sf(
    data = south_america,
    fill = "grey88",
    color = "white",
    linewidth = 0.2
  ) +
  geom_sf(
    data = brazil_union,
    fill = "grey72",
    color = "grey40",
    linewidth = 0.3
  ) +
  geom_sf(
    data = mato_grosso_do_sul,
    fill = highlight_color,
    color = highlight_color,
    linewidth = 0.2
  ) +
  coord_sf(
    expand = FALSE
  ) +
  blank_map_theme


brazil_plot <- ggplot() +
  geom_sf(
    data = brazilian_states,
    fill = "grey88",
    color = "white",
    linewidth = 0.25
  ) +
  geom_sf(
    data = brazil_union,
    fill = NA,
    color = "grey40",
    linewidth = 0.3
  ) +
  geom_sf(
    data = mato_grosso_do_sul,
    fill = highlight_color,
    color = highlight_color,
    linewidth = 0.2
  ) +
  coord_sf(
    expand = FALSE
  ) +
  blank_map_theme


# ---- C: Mato Grosso do Sul with neighbouring units for context ----


ms_bbox <- sf::st_bbox(
  mato_grosso_do_sul_projected
)


ms_padding <- 0.04 * as.numeric(
  ms_bbox["xmax"] - ms_bbox["xmin"]
)


ms_extent <- c(
  xmin = as.numeric(ms_bbox["xmin"]) - ms_padding,
  xmax = as.numeric(ms_bbox["xmax"]) + ms_padding,
  ymin = as.numeric(ms_bbox["ymin"]) - ms_padding,
  ymax = as.numeric(ms_bbox["ymax"]) + ms_padding
)


ms_extent_polygon <- sf::st_as_sfc(
  sf::st_bbox(
    ms_extent,
    crs = sf::st_crs(5880)
  )
)


crop_to_extent <- function(layer) {
  
  layer |>
    sf::st_transform(
      5880
    ) |>
    sf::st_make_valid() |>
    sf::st_intersection(
      ms_extent_polygon
    ) |>
    (\(cropped) sf::st_sf(
      label = cropped$label,
      geometry = sf::st_geometry(cropped)
    ))()
}


neighbour_countries <- south_america |>
  dplyr::filter(
    admin != "Brazil"
  ) |>
  dplyr::select(
    label = admin
  ) |>
  crop_to_extent()


neighbour_states <- brazilian_states |>
  dplyr::filter(
    abbrev_state != "MS"
  ) |>
  dplyr::select(
    label = abbrev_state
  ) |>
  crop_to_extent()


context_layers <- rbind(
  neighbour_countries |>
    dplyr::mutate(
      label = toupper(label)
    ),
  neighbour_states
)


context_labels <- context_layers |>
  sf::st_difference(
    sf::st_buffer(
      sf::st_geometry(
        mato_grosso_do_sul_projected
      ),
      8000
    )
  ) |>
  dplyr::mutate(
    area_km2 = as.numeric(
      sf::st_area(geometry)
    ) / 1e6
  ) |>
  dplyr::filter(
    area_km2 > 2500
  ) |>
  sf::st_point_on_surface()


context_labels <- cbind(
  sf::st_drop_geometry(
    context_labels
  ),
  sf::st_coordinates(
    context_labels
  )
)


manual_context_labels <- data.frame(
  label = c(
    "BOLIVIA",
    "PARAGUAY"
  ),
  lon = c(
    -58.36,
    -57.15
  ),
  lat = c(
    -19.15,
    -23.45
  ),
  angle = c(
    90,
    0
  )
)


manual_context_points <- sf::st_as_sf(
  manual_context_labels,
  coords = c(
    "lon",
    "lat"
  ),
  crs = 4674
) |>
  sf::st_transform(
    5880
  )


context_labels <- rbind(
  context_labels |>
    dplyr::filter(
      !label %in% manual_context_labels$label
    ) |>
    dplyr::transmute(
      label,
      angle = 0,
      X,
      Y
    ),
  cbind(
    sf::st_drop_geometry(
      manual_context_points
    ),
    sf::st_coordinates(
      manual_context_points
    )
  )
)


marker_offsets_km <- data.frame(
  area_id = c(
    "pantanal",
    "cerrado",
    "floodplain"
  ),
  dx = c(
    55,
    -55,
    -40
  ),
  dy = c(
    30,
    -20,
    -45
  )
)


marker_data <- anchor_projected_coordinates |>
  dplyr::mutate(
    area_id = as.character(
      area_id
    )
  ) |>
  dplyr::left_join(
    marker_offsets_km,
    by = "area_id"
  ) |>
  dplyr::mutate(
    label_x = Xp + dx * 1000,
    label_y = Yp + dy * 1000,
    letter = unname(
      area_letters[area_id]
    )
  )


ms_plot <- ggplot() +
  geom_sf(
    data = context_layers,
    fill = "grey93",
    color = "white",
    linewidth = 0.35
  ) +
  geom_raster(
    data = relief_ms$dem_df,
    aes(
      x = x,
      y = y,
      fill = elevation
    ),
    interpolate = TRUE
  ) +
  elevation_fill_scale() +
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
    linewidth = 0.14
  ) +
  geom_sf(
    data = mato_grosso_do_sul_projected,
    fill = NA,
    color = "grey20",
    linewidth = 0.5
  ) +
  geom_text(
    data = context_labels,
    aes(
      x = X,
      y = Y,
      label = label,
      angle = angle
    ),
    size = text_pt(font_size[["context"]]),
    family = font_family,
    fontface = "italic",
    color = "grey45"
  ) +
  geom_sf(
    data = protected_areas_projected,
    fill = NA,
    color = "white",
    linewidth = 1.6
  ) +
  geom_sf(
    data = protected_areas_projected,
    aes(
      color = area_id
    ),
    fill = NA,
    linewidth = 0.8
  ) +
  geom_segment(
    data = marker_data,
    aes(
      x = Xp,
      y = Yp,
      xend = label_x,
      yend = label_y
    ),
    color = "grey15",
    linewidth = 0.35
  ) +
  ggnewscale::new_scale_fill() +
  geom_point(
    data = marker_data,
    aes(
      x = label_x,
      y = label_y,
      fill = area_id
    ),
    shape = 21,
    size = 5.2,
    color = "white",
    stroke = 0.8
  ) +
  geom_text(
    data = marker_data,
    aes(
      x = label_x,
      y = label_y,
      label = letter
    ),
    size = text_pt(8),
    family = font_family,
    fontface = "bold",
    color = "white"
  ) +
  scale_fill_manual(
    values = landscape_colors,
    guide = "none"
  ) +
  scale_color_manual(
    values = landscape_colors,
    guide = "none"
  ) +
  ggspatial::annotation_scale(
    location = "bl",
    width_hint = 0.20,
    style = "bar",
    bar_cols = c(
      "grey15",
      "white"
    ),
    line_width = 0.4,
    height = grid::unit(
      0.12,
      "cm"
    ),
    text_cex = 0.62,
    text_family = font_family,
    pad_x = grid::unit(
      0.25,
      "cm"
    ),
    pad_y = grid::unit(
      0.25,
      "cm"
    )
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = ggspatial::north_arrow_orienteering(
      fill = c(
        "white",
        "grey15"
      ),
      line_col = "grey15",
      text_size = 7,
      text_family = font_family
    ),
    height = grid::unit(
      0.8,
      "cm"
    ),
    width = grid::unit(
      0.8,
      "cm"
    ),
    pad_x = grid::unit(
      0.2,
      "cm"
    ),
    pad_y = grid::unit(
      0.2,
      "cm"
    )
  ) +
  coord_sf(
    crs = sf::st_crs(5880),
    datum = sf::st_crs(4674),
    xlim = ms_extent[c("xmin", "xmax")],
    ylim = ms_extent[c("ymin", "ymax")],
    expand = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(-58, -50, by = 2)
  ) +
  scale_y_continuous(
    breaks = seq(-24, -16, by = 2)
  ) +
  theme_bw(
    base_family = font_family
  ) +
  theme(
    panel.grid.major = element_line(
      color = scales::alpha(
        "grey30",
        0.25
      ),
      linewidth = 0.2,
      linetype = "dashed"
    ),
    panel.ontop = TRUE,
    panel.background = element_blank(),
    panel.border = element_rect(
      fill = NA,
      color = "grey30",
      linewidth = 0.4
    ),
    axis.title = element_blank(),
    axis.text = element_text(
      size = font_size[["axis"]],
      color = "grey25"
    ),
    axis.text.y = element_text(
      angle = 90,
      hjust = 0.5
    ),
    axis.ticks = element_line(
      color = "grey30",
      linewidth = 0.3
    ),
    axis.ticks.length = grid::unit(
      0.08,
      "cm"
    ),
    plot.background = element_rect(
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


# ---- D, E, F: enlarged study areas with local relief ----


inset_extent <- function(
    polygon,
    box_aspect,
    shift_x = 0
) {
  
  box_aspect <- unname(
    box_aspect
  )
  
  polygon_bbox <- sf::st_bbox(
    sf::st_transform(
      polygon,
      5880
    )
  )
  
  center_x <- mean(
    as.numeric(
      polygon_bbox[c("xmin", "xmax")]
    )
  )
  
  center_y <- mean(
    as.numeric(
      polygon_bbox[c("ymin", "ymax")]
    )
  )
  
  half_width <- as.numeric(
    polygon_bbox["xmax"] - polygon_bbox["xmin"]
  ) * 0.58 + 1500
  
  half_height <- as.numeric(
    polygon_bbox["ymax"] - polygon_bbox["ymin"]
  ) * 0.58 + 1500
  
  if (half_height / half_width < box_aspect) {
    half_height <- half_width * box_aspect
  } else {
    half_width <- half_height / box_aspect
  }
  
  center_x <- center_x + shift_x * half_width
  
  c(
    xmin = center_x - half_width,
    xmax = center_x + half_width,
    ymin = center_y - half_height,
    ymax = center_y + half_height
  )
}


make_protected_area_inset <- function(
    polygon,
    area_color,
    extent,
    relief,
    scale_location = "bl"
) {
  
  polygon_projected <- sf::st_transform(
    polygon,
    5880
  )
  
  ggplot() +
    geom_raster(
      data = relief$dem_df,
      aes(
        x = x,
        y = y,
        fill = elevation
      ),
      interpolate = TRUE
    ) +
    elevation_fill_scale() +
    geom_raster(
      data = relief$hill_df,
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
      data = polygon_projected,
      fill = scales::alpha(
        "white",
        0.22
      ),
      color = "white",
      linewidth = 1.7
    ) +
    geom_sf(
      data = polygon_projected,
      fill = NA,
      color = area_color,
      linewidth = 0.8
    ) +
    ggspatial::annotation_scale(
      location = scale_location,
      width_hint = 0.28,
      style = "bar",
      bar_cols = c(
        "grey15",
        "white"
      ),
      line_width = 0.35,
      height = grid::unit(
        0.10,
        "cm"
      ),
      text_cex = 0.58,
      text_family = font_family,
      text_col = "grey10",
      pad_x = grid::unit(
        0.12,
        "cm"
      ),
      pad_y = grid::unit(
        0.12,
        "cm"
      )
    ) +
    coord_sf(
      crs = sf::st_crs(5880),
      xlim = extent[c("xmin", "xmax")],
      ylim = extent[c("ymin", "ymax")],
      expand = FALSE
    ) +
    blank_map_theme +
    theme(
      panel.border = element_rect(
        fill = NA,
        color = area_color,
        linewidth = 1
      )
    )
}


# ---- Layout (inches from the bottom-left corner) ----


make_box <- function(
    x_in,
    y_in,
    w_in,
    h_in
) {
  c(
    x = inches_to_x(x_in),
    y = inches_to_y(y_in),
    w = inches_to_x(w_in),
    h = inches_to_y(h_in)
  )
}


ms_aspect <- (ms_extent[["ymax"]] - ms_extent[["ymin"]]) /
  (ms_extent[["xmax"]] - ms_extent[["xmin"]])


legend_height_in <- 0.62
top_title_in <- 0.28

c_axis_left_in <- 0.20
c_axis_bottom_in <- 0.18
c_panel_w_in <- 4.00
c_panel_h_in <- c_panel_w_in * ms_aspect

main_bottom_in <- legend_height_in + 0.16
main_top_in <- main_bottom_in + c_axis_bottom_in + c_panel_h_in

figure_height_in <- main_top_in + top_title_in + 0.04

box_C <- make_box(
  0.10,
  main_bottom_in,
  c_axis_left_in + c_panel_w_in,
  c_axis_bottom_in + c_panel_h_in
)


right_x_in <- 0.10 + c_axis_left_in + c_panel_w_in + 0.25
right_w_in <- figure_width_in - right_x_in - 0.08
column_gap_in <- 0.14
half_w_in <- (right_w_in - column_gap_in) / 2


# Row A/B
ab_box_h_in <- 0.95
ab_y_in <- main_top_in - ab_box_h_in

box_A <- make_box(
  right_x_in,
  ab_y_in,
  half_w_in,
  ab_box_h_in
)

box_B <- make_box(
  right_x_in + half_w_in + column_gap_in,
  ab_y_in,
  half_w_in,
  ab_box_h_in
)


# Row D
d_title_in <- 0.40
d_box_h_in <- 0.80
d_y_in <- ab_y_in - 0.10 - d_title_in - d_box_h_in

box_pantanal <- make_box(
  right_x_in,
  d_y_in,
  right_w_in,
  d_box_h_in
)


# Row E/F
ef_title_in <- 0.64
ef_y_in <- main_bottom_in + c_axis_bottom_in
ef_box_h_in <- d_y_in - 0.10 - ef_title_in - ef_y_in

box_cerrado <- make_box(
  right_x_in,
  ef_y_in,
  half_w_in,
  ef_box_h_in
)

box_floodplain <- make_box(
  right_x_in + half_w_in + column_gap_in,
  ef_y_in,
  half_w_in,
  ef_box_h_in
)


box_legend <- make_box(
  0.10,
  0.06,
  figure_width_in - 0.20,
  legend_height_in
)


inset_boxes <- list(
  pantanal = box_pantanal,
  cerrado = box_cerrado,
  floodplain = box_floodplain
)


inset_scale_location <- c(
  pantanal = "br",
  cerrado = "br",
  floodplain = "br"
)


inset_shift_x <- c(
  pantanal = 0,
  cerrado = 0.30,
  floodplain = 0
)


inset_zoom <- c(
  pantanal = 11,
  cerrado = 10,
  floodplain = 10
)


cat(
  "Downloading the elevation models for the enlarged insets...\n"
)


inset_plots <- list()

for (area_name in names(inset_boxes)) {
  
  box <- inset_boxes[[area_name]]
  
  extent <- inset_extent(
    protected_area_split[[area_name]],
    (box["h"] * figure_height_in) /
      (box["w"] * figure_width_in),
    shift_x = inset_shift_x[[area_name]]
  )
  
  extent_polygon <- sf::st_as_sf(
    sf::st_as_sfc(
      sf::st_bbox(
        extent,
        crs = sf::st_crs(5880)
      )
    )
  )
  
  inset_relief <- build_relief_projected(
    extent_polygon,
    zoom = inset_zoom[[area_name]],
    buffer_m = 3000,
    shadow_max = 0.30
  )
  
  inset_plots[[area_name]] <- make_protected_area_inset(
    protected_area_split[[area_name]],
    landscape_colors[[area_name]],
    extent,
    inset_relief,
    scale_location = inset_scale_location[[area_name]]
  )
}


# ---- Legend (single row, full width) ----


make_legend_panel <- function(
    panel_width_in,
    panel_height_in
) {
  
  bar_x0 <- 0.10
  bar_width <- 2.2
  bar_y0 <- 0.20
  bar_y1 <- 0.32
  
  bar_values <- seq(
    elevation_limits[1],
    elevation_limits[2],
    length.out = 200
  )
  
  bar_data <- data.frame(
    xmin = bar_x0 + bar_width * scales::rescale(
      utils::head(bar_values, -1),
      from = elevation_limits
    ),
    xmax = bar_x0 + bar_width * scales::rescale(
      utils::tail(bar_values, -1),
      from = elevation_limits
    ),
    value = utils::head(bar_values, -1)
  )
  
  tick_values <- seq(
    100,
    900,
    by = 200
  )
  
  tick_values <- tick_values[
    tick_values >= elevation_limits[1] &
      tick_values <= elevation_limits[2]
  ]
  
  tick_data <- data.frame(
    x = bar_x0 + bar_width * scales::rescale(
      tick_values,
      from = elevation_limits
    ),
    label = tick_values
  )
  
  symbols_x <- bar_x0 + bar_width + 0.45
  areas_x <- symbols_x + 1.95
  
  area_key <- data.frame(
    area_id = names(area_letters),
    letter = unname(area_letters),
    x = areas_x + c(0, 0.72, 1.48),
    label = c(
      "Pantanal",
      "Cerrado",
      "Floodplain"
    )
  )
  
  ggplot() +
    theme_void(
      base_family = font_family
    ) +
    coord_cartesian(
      xlim = c(
        0,
        panel_width_in
      ),
      ylim = c(
        0,
        panel_height_in
      ),
      expand = FALSE,
      clip = "off"
    ) +
    annotate(
      "segment",
      x = 0,
      xend = panel_width_in,
      y = panel_height_in,
      yend = panel_height_in,
      color = "grey80",
      linewidth = 0.3
    ) +
    annotate(
      "text",
      x = bar_x0,
      y = panel_height_in - 0.10,
      label = "Elevation (m)",
      hjust = 0,
      vjust = 1,
      fontface = "bold",
      family = font_family,
      size = text_pt(font_size[["legend_title"]])
    ) +
    geom_rect(
      data = bar_data,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = bar_y0,
        ymax = bar_y1,
        fill = value
      ),
      color = NA
    ) +
    elevation_fill_scale() +
    annotate(
      "rect",
      xmin = bar_x0,
      xmax = bar_x0 + bar_width,
      ymin = bar_y0,
      ymax = bar_y1,
      fill = NA,
      color = "grey35",
      linewidth = 0.25
    ) +
    geom_segment(
      data = tick_data,
      aes(
        x = x,
        xend = x,
        y = bar_y0 - 0.03,
        yend = bar_y0
      ),
      color = "grey35",
      linewidth = 0.25
    ) +
    geom_text(
      data = tick_data,
      aes(
        x = x,
        y = bar_y0 - 0.06,
        label = label
      ),
      vjust = 1,
      family = font_family,
      size = text_pt(font_size[["legend_text"]])
    ) +
    annotate(
      "text",
      x = symbols_x,
      y = panel_height_in - 0.10,
      label = "Symbols",
      hjust = 0,
      vjust = 1,
      fontface = "bold",
      family = font_family,
      size = text_pt(font_size[["legend_title"]])
    ) +
    annotate(
      "segment",
      x = symbols_x,
      xend = symbols_x + 0.25,
      y = 0.30,
      yend = 0.30,
      color = scales::alpha(
        "#7A2D1B",
        0.55
      ),
      linewidth = 0.5
    ) +
    annotate(
      "text",
      x = symbols_x + 0.33,
      y = 0.30,
      label = paste0(
        "Contour (",
        contour_interval_ms,
        " m)"
      ),
      hjust = 0,
      family = font_family,
      size = text_pt(font_size[["legend_text"]])
    ) +
    annotate(
      "rect",
      xmin = symbols_x,
      xmax = symbols_x + 0.25,
      ymin = 0.04,
      ymax = 0.16,
      fill = highlight_color,
      color = NA
    ) +
    annotate(
      "text",
      x = symbols_x + 0.33,
      y = 0.10,
      label = "Mato Grosso do Sul (a, b)",
      hjust = 0,
      family = font_family,
      size = text_pt(font_size[["legend_text"]])
    ) +
    annotate(
      "text",
      x = areas_x,
      y = panel_height_in - 0.10,
      label = "Study areas (enlarged in d–f)",
      hjust = 0,
      vjust = 1,
      fontface = "bold",
      family = font_family,
      size = text_pt(font_size[["legend_title"]])
    ) +
    ggnewscale::new_scale_fill() +
    geom_point(
      data = area_key,
      aes(
        x = x + 0.08,
        y = 0.22,
        fill = area_id
      ),
      shape = 21,
      size = 4.4,
      color = "white",
      stroke = 0.6
    ) +
    geom_text(
      data = area_key,
      aes(
        x = x + 0.08,
        y = 0.22,
        label = letter
      ),
      fontface = "bold",
      color = "white",
      family = font_family,
      size = text_pt(7)
    ) +
    geom_text(
      data = area_key,
      aes(
        x = x + 0.22,
        y = 0.22,
        label = label
      ),
      hjust = 0,
      family = font_family,
      size = text_pt(font_size[["legend_text"]])
    ) +
    scale_fill_manual(
      values = landscape_colors,
      guide = "none"
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


legend_plot <- make_legend_panel(
  box_legend["w"] * figure_width_in,
  box_legend["h"] * figure_height_in
)


# ---- Assemble ----


draw_box <- function(
    figure,
    plot,
    box
) {
  
  figure +
    cowplot::draw_plot(
      plot,
      x = box["x"],
      y = box["y"],
      width = box["w"],
      height = box["h"]
    )
}


draw_panel_title <- function(
    figure,
    tag,
    title,
    x,
    y
) {
  
  figure +
    cowplot::draw_label(
      tag,
      x = x,
      y = y,
      hjust = 0,
      vjust = 0,
      fontface = "bold",
      fontfamily = font_family,
      size = font_size[["tag"]]
    ) +
    cowplot::draw_label(
      title,
      x = x + inches_to_x(0.20),
      y = y,
      hjust = 0,
      vjust = 0,
      fontface = "bold",
      fontfamily = font_family,
      size = font_size[["panel_title"]]
    )
}


location_figure <- cowplot::ggdraw() |>
  draw_box(
    ms_plot,
    box_C
  ) |>
  draw_box(
    south_america_plot,
    box_A
  ) |>
  draw_box(
    brazil_plot,
    box_B
  ) |>
  draw_box(
    legend_plot,
    box_legend
  )


location_figure <- location_figure |>
  draw_panel_title(
    "a",
    "South America",
    box_A["x"],
    box_A["y"] + box_A["h"] + inches_to_y(0.06)
  ) |>
  draw_panel_title(
    "b",
    "Brazil",
    box_B["x"],
    box_B["y"] + box_B["h"] + inches_to_y(0.06)
  ) |>
  draw_panel_title(
    "c",
    "Mato Grosso do Sul: relief and study areas",
    box_C["x"],
    box_C["y"] + box_C["h"] + inches_to_y(0.06)
  )


for (area_name in names(inset_boxes)) {
  
  box <- inset_boxes[[area_name]]
  
  location_figure <- location_figure |>
    draw_box(
      inset_plots[[area_name]],
      box
    )
  
  location_figure <- location_figure +
    cowplot::draw_label(
      area_letters[[area_name]],
      x = box["x"],
      y = box["y"] + box["h"] + inches_to_y(0.20),
      hjust = 0,
      vjust = 0,
      fontface = "bold",
      fontfamily = font_family,
      size = font_size[["tag"]]
    ) +
    cowplot::draw_label(
      inset_titles[[area_name]][["name"]],
      x = box["x"] + inches_to_x(0.20),
      y = box["y"] + box["h"] + inches_to_y(0.20),
      hjust = 0,
      vjust = 0,
      fontface = "bold",
      fontfamily = font_family,
      size = font_size[["inset_name"]],
      lineheight = 0.95
    ) +
    cowplot::draw_label(
      inset_titles[[area_name]][["info"]],
      x = box["x"] + inches_to_x(0.20),
      y = box["y"] + box["h"] + inches_to_y(0.04),
      hjust = 0,
      vjust = 0,
      fontfamily = font_family,
      size = font_size[["inset_info"]],
      color = landscape_colors[[area_name]]
    )
}


output_png <- file.path(
  output_directory,
  paste0(
    "Figure_StudyArea_Location_",
    output_suffix,
    ".png"
  )
)


output_pdf <- file.path(
  output_directory,
  paste0(
    "Figure_StudyArea_Location_",
    output_suffix,
    ".pdf"
  )
)


writeLines(
  paste0(
    "Location of the study areas in Mato Grosso do Sul, Brazil. ",
    "(a) South America and (b) Brazil, with Mato Grosso do Sul in red. ",
    "(c) Hypsometric relief of Mato Grosso do Sul (",
    contour_interval_ms,
    " m contour lines) showing the three protected areas sampled; ",
    "letters refer to the enlarged panels. ",
    "(d) APA da Baía Negra (Pantanal; 5,420.58 ha); ",
    "(e) Parque Nacional da Serra da Bodoquena (Cerrado; 77,021.58 ha); ",
    "(f) Parque Estadual das Várzeas do Rio Ivinhema (Floodplain; 73,345.15 ha). ",
    "Elevation from a digital elevation model obtained with the elevatr ",
    "R package and processed with terra; administrative limits from ",
    "Natural Earth and geobr; protected-area boundaries from MMA/CNUC. ",
    "Datum SIRGAS 2000; panels a–b in geographic coordinates ",
    "(EPSG:4674), panels c–f in Brazil Polyconic projection (EPSG:5880). ",
    "Sampling period: ",
    sampling_period,
    "."
  ),
  file.path(
    output_directory,
    paste0(
      "Figure_StudyArea_Location_",
      output_suffix,
      "_caption.txt"
    )
  ),
  useBytes = TRUE
)


cat(
  "Saving PNG...\n"
)


ggplot2::ggsave(
  filename = output_png,
  plot = location_figure,
  width = figure_width_in,
  height = figure_height_in,
  units = "in",
  device = ragg::agg_png,
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
  width = figure_width_in,
  height = figure_height_in,
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