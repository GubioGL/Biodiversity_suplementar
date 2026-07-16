# Relative occurrence of social wasp species across landscapes

options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  timeout = 600
)

required_packages <- c(
  "dplyr",
  "tidyr",
  "readr",
  "stringr",
  "janitor",
  "vegan",
  "png",
  "jpeg",
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


# ----------------------------------------------------------
# Folder-location functions
# ----------------------------------------------------------

normalize_directory_name <- function(x) {
  
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
  
  directories <- unique(
    directories[
      dir.exists(directories)
    ]
  )
  
  directories
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
  
  target_directory_name <-
    "Relative occurrence of species across landscapes data"
  
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
    "Open or save this script inside the folder '",
    target_directory_name,
    "' and run it again."
  )
}


locate_data_file <- function(
    analysis_directory
) {
  parent_directory <- dirname(
    analysis_directory
  )

  possible_directories <- unique(
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

  possible_directories <- possible_directories[
    dir.exists(possible_directories)
  ]

  csv_files <- unique(
    unlist(
      lapply(
        possible_directories,
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

  if (length(preferred_files) > 1) {
    csv_files <- preferred_files
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
    "No CSV file named 'dados.csv' was found in: ",
    paste(
      possible_directories,
      collapse = "\n"
    )
  )
}


locate_image_directory <- function(
    analysis_directory
) {
  parent_directory <- dirname(
    analysis_directory
  )

  data_directory <- file.path(
    parent_directory,
    "data"
  )

  if (dir.exists(data_directory)) {
    return(
      normalizePath(
        data_directory,
        winslash = "/",
        mustWork = TRUE
      )
    )
  }
  
  target_name <-
    "Relative occurrence of species across landscapes"
  
  expected_directory <- file.path(
    analysis_directory,
    target_name
  )
  
  if (dir.exists(expected_directory)) {
    
    return(
      normalizePath(
        expected_directory,
        winslash = "/",
        mustWork = TRUE
      )
    )
  }
  
  subdirectories <- list.dirs(
    path = analysis_directory,
    full.names = TRUE,
    recursive = FALSE
  )
  
  subdirectories <- subdirectories[
    dir.exists(subdirectories)
  ]
  
  matched_directories <- subdirectories[
    normalize_directory_name(
      basename(subdirectories)
    ) ==
      normalize_directory_name(
        target_name
      )
  ]
  
  if (length(matched_directories) > 0) {
    
    return(
      normalizePath(
        matched_directories[1],
        winslash = "/",
        mustWork = TRUE
      )
    )
  }
  
  stop(
    "The image directory was not found. Expected directory:\n",
    expected_directory
  )
}


analysis_directory <- locate_analysis_directory()

file_csv <- locate_data_file(
  analysis_directory
)

image_directory <- locate_image_directory(
  analysis_directory
)

output_directory <- file.path(
  analysis_directory,
  "output"
)

dir.create(
  output_directory,
  recursive = TRUE,
  showWarnings = FALSE
)


cat(
  "Analysis directory:\n",
  analysis_directory,
  "\n\n",
  sep = ""
)

cat(
  "Dataset selected:\n",
  file_csv,
  "\n\n",
  sep = ""
)

cat(
  "Image directory:\n",
  image_directory,
  "\n\n",
  sep = ""
)

cat(
  "Output directory:\n",
  output_directory,
  "\n\n",
  sep = ""
)


# ----------------------------------------------------------
# Text functions
# ----------------------------------------------------------

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
  
  x <- tolower(
    stringr::str_squish(x)
  )
  
  x
}


normalize_file_name <- function(x) {
  
  x <- tools::file_path_sans_ext(
    basename(x)
  )
  
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


# ----------------------------------------------------------
# Poncho function
# ----------------------------------------------------------

poncho <- function(
    table,
    col = 1,
    col.places = 2,
    col.species = 3,
    col.gradient = "gray",
    highlight.species = NA,
    highlight.places = NA,
    xyratio = 1.3333333333,
    cex.species = NULL,
    lwd = 1,
    cex.lab = NULL,
    cex.title = 3,
    cex.gradient = 2,
    gradient = NULL,
    file = NULL,
    title = NULL,
    xlab = "Ordered places",
    ylab = "Abundance",
    show.grad = TRUE,
    show.places = NULL,
    xfactor = 0,
    gradlab = "Gradient",
    lty = 1,
    lty.lines = 1,
    border = 1,
    decrease = FALSE,
    add = FALSE,
    spgradient = NULL
) {
  
  if (is.null(file)) {
    
    if (is.null(cex.species)) {
      cex.species <- 0.74
    }
    
    if (is.null(cex.lab)) {
      cex.lab <- 2
    }
    
    if (is.null(cex.gradient)) {
      cex.gradient <- 1.5
    }
  }
  
  if (is.null(cex.gradient)) {
    cex.gradient <- 2
  }
  
  if (is.null(cex.lab)) {
    cex.lab <- 3
  }
  
  if (!is.null(file)) {
    
    grDevices::pdf(
      file = file,
      paper = "A4",
      width = 0,
      height = 0
    )
  }
  
  graphics::par(
    mar = c(
      2,
      2,
      0.7,
      0
    )
  )
  
  if (is.null(gradient)) {
    
    gradient <- stats::prcomp(
      table
    )$x[, 1]
  }
  
  multiplier <- ifelse(
    decrease == FALSE,
    1,
    -1
  )
  
  row_order <- order(
    multiplier * gradient
  )
  
  if (!is.null(spgradient)) {
    
    species_order <- order(
      spgradient
    )
    
  } else {
    
    species_weight <- colSums(
      table *
        multiplier *
        gradient
    ) /
      colSums(table)
    
    species_order <- order(
      species_weight
    )
  }
  
  table_ordered <- table[
    row_order,
    species_order,
    drop = FALSE
  ]
  
  x <- 84
  y <- x * xyratio
  
  dimx <- x /
    nrow(table_ordered)
  
  dimy <- y /
    ncol(table_ordered)
  
  gradient_space <- if (
    is.null(show.grad)
  ) {
    0
  } else {
    22
  }
  
  if (add == FALSE) {
    
    graphics::plot.new()
    
    graphics::plot.window(
      xlim = c(
        0,
        106 + xfactor
      ),
      ylim = c(
        0,
        y + gradient_space + 12
      )
    )
  }
  
  place_colors <- rep(
    col,
    nrow(table_ordered)
  )
  
  ordered_place_matches <- match(
    rownames(table_ordered),
    highlight.places
  )
  
  valid_place_matches <- !is.na(
    ordered_place_matches
  )
  
  if (
    any(valid_place_matches) &&
    length(col.places) >=
    max(
      ordered_place_matches[
        valid_place_matches
      ]
    )
  ) {
    
    place_colors[
      valid_place_matches
    ] <- col.places[
      ordered_place_matches[
        valid_place_matches
      ]
    ]
  }
  
  maximum_value <- max(
    table_ordered,
    na.rm = TRUE
  )
  
  if (
    !is.finite(maximum_value) ||
    maximum_value <= 0
  ) {
    
    maximum_value <- 1
  }
  
  for (i in seq_len(
    ncol(table_ordered)
  )) {
    
    species_match <- match(
      highlight.species,
      colnames(table_ordered)[i]
    )
    
    if (
      sum(
        species_match,
        na.rm = TRUE
      ) != 0
    ) {
      current_color <- col.species
    } else {
      current_color <- place_colors
    }
    
    graphics::rect(
      (0:(nrow(table_ordered) - 1)) *
        dimx,
      (i - 1) *
        dimy,
      (1:nrow(table_ordered)) *
        dimx -
        dimx *
        0.1,
      (i - 1) *
        dimy +
        (
          table_ordered[, i] /
            maximum_value
        ) *
        dimy *
        0.8,
      col = current_color,
      lty = lty,
      lwd = lwd,
      border = border
    )
    
    graphics::lines(
      c(
        -1,
        x
      ),
      c(
        (i - 1) * dimy,
        (i - 1) * dimy
      ),
      lty = lty.lines,
      lwd = lwd
    )
  }
  
  species_label_x <- x + 1.2
  
  species_label_y <- (
    seq_len(
      ncol(table_ordered)
    ) -
      0.5
  ) *
    dimy
  
  graphics::text(
    x = rep(
      species_label_x,
      ncol(table_ordered)
    ),
    y = species_label_y,
    labels = gsub(
      "_",
      " ",
      colnames(table_ordered)
    ),
    cex = cex.species,
    adj = 0,
    font = 3,
    xpd = NA
  )
  
  if (lty.lines != 0) {
    
    graphics::rect(
      -1,
      0,
      -1,
      (
        ncol(table_ordered) - 1
      ) *
        dimy +
        dimy *
        0.8,
      lwd = lwd
    )
  }
  
  if (!is.null(show.grad)) {
    
    graphics::text(
      x + 1,
      133.5,
      gradlab,
      cex = cex.gradient,
      adj = 0,
      xpd = NA
    )
  }
  
  graphics::par(
    mgp = c(
      1,
      4,
      5
    )
  )
  
  graphics::title(
    main = title,
    cex.main = cex.title
  )
  
  graphics::par(
    mgp = c(
      0,
      4,
      5
    )
  )
  
  graphics::title(
    ylab = ylab,
    cex.lab = cex.lab
  )
  
  graphics::par(
    mgp = c(
      1,
      6,
      8
    ),
    mar = c(
      3.1,
      3,
      0.7,
      1.2
    )
  )
  
  graphics::title(
    xlab = xlab,
    cex.lab = cex.lab
  )
  
  graphics::par(
    mgp = c(
      4,
      8,
      10
    ),
    mar = c(
      0,
      0,
      0,
      0
    )
  )
  
  if (!is.null(show.grad)) {
    
    gradient <- sort(
      gradient,
      decreasing = decrease
    )
    
    gradient_range <- abs(
      max(
        c(
          gradient,
          0
        )
      ) -
        min(
          c(
            gradient,
            0
          )
        )
    )
    
    if (gradient_range == 0) {
      gradient_range <- 1
    }
    
    gradient_scaled <- gradient /
      gradient_range
    
    gradient_base <- 126 -
      min(
        c(
          gradient_scaled * 15,
          0
        )
      )
    
    graphics::rect(
      (0:(nrow(table_ordered) - 1)) *
        dimx,
      gradient_base,
      (1:nrow(table_ordered)) *
        dimx -
        dimx *
        0.1,
      gradient_base +
        gradient_scaled *
        15,
      lwd = lwd,
      col = col.gradient
    )
    
    graphics::rect(
      x + 0.1,
      126,
      x + 0.1,
      141,
      lwd = lwd
    )
    
    graphics::rect(
      x + 0.1,
      126,
      x + 0.9,
      126,
      lwd = lwd
    )
    
    graphics::rect(
      x + 0.1,
      141,
      x + 0.9,
      141,
      lwd = lwd
    )
    
    graphics::text(
      x + 1.2,
      126,
      round(
        min(
          c(
            0,
            gradient
          )
        ),
        2
      ),
      adj = 0,
      cex = 0.7,
      xpd = NA
    )
    
    graphics::text(
      x + 1.2,
      141,
      round(
        max(
          c(
            0,
            gradient
          )
        ),
        2
      ),
      adj = 0,
      cex = 0.7,
      xpd = NA
    )
  }
  
  graphics::par(
    mar = c(
      5,
      4,
      4,
      2
    ) + 0.1,
    mgp = c(
      0,
      -0.5,
      0
    )
  )
  
  if (!is.null(show.places)) {
    
    graphics::axis(
      1,
      at = (
        (
          0:(nrow(table_ordered) - 1)
        ) *
          dimx +
          (
            1:nrow(table_ordered)
          ) *
          dimx -
          dimx *
          0.1
      ) /
        2,
      labels = rownames(
        table_ordered
      ),
      las = 2,
      cex.axis = cex.species,
      tick = FALSE
    )
  }
  
  graphics::par(
    mar = c(
      5,
      4,
      4,
      2
    ) + 0.1,
    mgp = c(
      3,
      1,
      0
    )
  )
  
  if (!is.null(file)) {
    
    grDevices::dev.off()
    
    return(
      paste(
        "Image saved as",
        file
      )
    )
  }
  
  invisible(
    table_ordered
  )
}


# ----------------------------------------------------------
# Image functions
# ----------------------------------------------------------

list_available_images <- function(
    image_directory
) {
  
  image_files <- list.files(
    path = image_directory,
    pattern = "\\.(png|jpg|jpeg)$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  
  image_files[
    file.exists(image_files)
  ]
}


find_landscape_image <- function(
    image_files,
    landscape_name
) {
  
  if (length(image_files) == 0) {
    
    return(
      NA_character_
    )
  }
  
  normalized_names <- vapply(
    image_files,
    normalize_file_name,
    character(1)
  )
  
  aliases <- switch(
    landscape_name,
    
    "pantanal" = c(
      "pantanal",
      "baia negra",
      "baianegra",
      "apa baia negra"
    ),
    
    "cerrado" = c(
      "cerrado",
      "bodoquena",
      "serra da bodoquena"
    ),
    
    "floodplain" = c(
      "floodplain",
      "varzea",
      "varzeas",
      "ivinhema",
      "pevri",
      "rio ivinhema"
    )
  )
  
  aliases <- normalize_directory_name(
    aliases
  )
  
  image_scores <- vapply(
    normalized_names,
    function(image_name) {
      
      alias_scores <- vapply(
        aliases,
        function(alias) {
          
          if (identical(
            image_name,
            alias
          )) {
            
            return(
              1000 + nchar(alias)
            )
          }
          
          if (
            grepl(
              alias,
              image_name,
              fixed = TRUE
            )
          ) {
            
            return(
              nchar(alias)
            )
          }
          
          0
        },
        numeric(1)
      )
      
      max(alias_scores)
    },
    numeric(1)
  )
  
  if (
    length(image_scores) == 0 ||
    max(image_scores) == 0
  ) {
    
    return(
      NA_character_
    )
  }
  
  selected_file <- image_files[
    which.max(image_scores)
  ]
  
  normalizePath(
    selected_file,
    winslash = "/",
    mustWork = TRUE
  )
}


read_landscape_image <- function(
    file_path
) {
  
  if (
    is.na(file_path) ||
    !file.exists(file_path)
  ) {
    
    return(
      NULL
    )
  }
  
  extension <- tolower(
    tools::file_ext(file_path)
  )
  
  tryCatch(
    {
      if (extension == "png") {
        
        png::readPNG(
          file_path
        )
        
      } else if (
        extension %in% c(
          "jpg",
          "jpeg"
        )
      ) {
        
        jpeg::readJPEG(
          file_path
        )
        
      } else {
        
        NULL
      }
    },
    error = function(e) {
      
      stop(
        "The image could not be read: ",
        basename(file_path),
        ". Original error: ",
        conditionMessage(e)
      )
    }
  )
}


plot_photo_panel <- function(
    image,
    border_color = "black"
) {
  
  graphics::par(
    mar = c(
      0.0001,
      0.0001,
      0.0001,
      0.0001
    )
  )
  
  graphics::plot.new()
  
  graphics::plot.window(
    xlim = c(
      0,
      1
    ),
    ylim = c(
      0,
      1
    ),
    asp = 1
  )
  
  graphics::rasterImage(
    image,
    0.001,
    0.001,
    0.999,
    0.999
  )
  
  graphics::rect(
    0.001,
    0.001,
    0.999,
    0.999,
    border = border_color,
    lwd = 2
  )
}


# ----------------------------------------------------------
# Read and prepare data
# ----------------------------------------------------------

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
  "trap",
  "collection",
  "landscape",
  "lati_s",
  "long_w",
  "baits",
  "date"
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


data_clean <- raw_data |>
  dplyr::mutate(
    dplyr::across(
      dplyr::where(is.character),
      clean_text
    ),
    species = clean_text(
      species
    ),
    trap = normalize_text(
      trap
    ),
    collection = normalize_text(
      collection
    ),
    landscape = normalize_text(
      landscape
    )
  )


suspension_data <- data_clean |>
  dplyr::filter(
    !is.na(species),
    species != "",
    trap == "suspension",
    collection == "passive",
    landscape %in% c(
      "pantanal",
      "cerrado",
      "floodplain"
    )
  )


if (nrow(suspension_data) == 0) {
  
  stop(
    "No records remained after filtering for suspension traps, ",
    "passive collection and the three landscapes."
  )
}


cat(
  "\nRecords after the suspension-trap filter:\n"
)

print(
  nrow(suspension_data)
)


cat(
  "\nRecords by landscape:\n"
)

print(
  table(
    suspension_data$landscape
  )
)


cat(
  "\nSpecies by landscape:\n"
)

print(
  suspension_data |>
    dplyr::distinct(
      landscape,
      species
    ) |>
    dplyr::count(
      landscape,
      name = "n_species"
    )
)


occurrence_data <- suspension_data |>
  dplyr::mutate(
    lati_s = as.character(
      lati_s
    ),
    long_w = as.character(
      long_w
    ),
    baits = as.character(
      baits
    ),
    date = as.character(
      date
    ),
    sampling_unit = paste(
      landscape,
      lati_s,
      long_w,
      sep = "_"
    )
  ) |>
  dplyr::distinct(
    landscape,
    sampling_unit,
    date,
    lati_s,
    long_w,
    baits,
    species
  )


species_wide_landscape <- occurrence_data |>
  dplyr::group_by(
    landscape,
    sampling_unit,
    species
  ) |>
  dplyr::summarise(
    count = dplyr::n(),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = species,
    values_from = count,
    values_fill = 0
  )


landscape_order <- c(
  "pantanal",
  "cerrado",
  "floodplain"
)


landscape_labels <- c(
  "pantanal" = "Pantanal",
  "cerrado" = "Cerrado",
  "floodplain" = "Floodplain"
)


landscape_colors <- c(
  "pantanal" = "#4E9B73",
  "cerrado" = "#C89A42",
  "floodplain" = "#6CA0E8"
)


species_wide_landscape <- species_wide_landscape |>
  dplyr::mutate(
    landscape = factor(
      landscape,
      levels = landscape_order
    )
  ) |>
  dplyr::arrange(
    landscape,
    sampling_unit
  )


variables_landscape <- species_wide_landscape |>
  dplyr::select(
    landscape,
    sampling_unit
  )


species_abundance <- species_wide_landscape |>
  dplyr::select(
    -landscape,
    -sampling_unit
  ) |>
  as.data.frame(
    check.names = FALSE
  )


species_occurrence <- ifelse(
  species_abundance > 0,
  1,
  0
)


species_occurrence <- as.data.frame(
  species_occurrence,
  check.names = FALSE
)


keep_rows <- rowSums(
  species_occurrence
) > 0


species_occurrence <- species_occurrence[
  keep_rows,
  ,
  drop = FALSE
]


variables_landscape <- variables_landscape[
  keep_rows,
  ,
  drop = FALSE
]


keep_columns <- colSums(
  species_occurrence
) > 0


species_occurrence <- species_occurrence[
  ,
  keep_columns,
  drop = FALSE
]


if (nrow(species_occurrence) == 0) {
  
  stop(
    "No sampling units remained in the occurrence matrix."
  )
}


if (ncol(species_occurrence) == 0) {
  
  stop(
    "No species remained in the occurrence matrix."
  )
}


rownames(species_occurrence) <- seq_len(
  nrow(species_occurrence)
)


gradient_landscape <- as.numeric(
  variables_landscape$landscape
)


colors_by_unit <- unname(
  landscape_colors[
    as.character(
      variables_landscape$landscape
    )
  ]
)


species_relative_occurrence <- vegan::decostand(
  as.matrix(
    species_occurrence
  ),
  method = "total"
)


cat(
  "\nFinal matrix dimensions:\n"
)

cat(
  nrow(species_relative_occurrence),
  "sampling units x",
  ncol(species_relative_occurrence),
  "species\n\n"
)


# ----------------------------------------------------------
# Locate and read landscape images
# ----------------------------------------------------------

available_images <- list_available_images(
  image_directory
)


cat(
  "Images found inside the correct image directory:\n"
)

if (length(available_images) > 0) {
  
  cat(
    paste0(
      "- ",
      basename(available_images),
      collapse = "\n"
    ),
    "\n\n"
  )
  
} else {
  
  cat(
    "No PNG, JPG or JPEG images were found.\n\n"
  )
}


pantanal_image_file <- find_landscape_image(
  available_images,
  "pantanal"
)


cerrado_image_file <- find_landscape_image(
  available_images,
  "cerrado"
)


floodplain_image_file <- find_landscape_image(
  available_images,
  "floodplain"
)


cat(
  "Images selected for the figure:\n"
)

cat(
  "Pantanal: ",
  ifelse(
    is.na(pantanal_image_file),
    "not found",
    pantanal_image_file
  ),
  "\n",
  sep = ""
)

cat(
  "Cerrado: ",
  ifelse(
    is.na(cerrado_image_file),
    "not found",
    cerrado_image_file
  ),
  "\n",
  sep = ""
)

cat(
  "Floodplain: ",
  ifelse(
    is.na(floodplain_image_file),
    "not found",
    floodplain_image_file
  ),
  "\n\n",
  sep = ""
)


missing_images <- c(
  Pantanal = pantanal_image_file,
  Cerrado = cerrado_image_file,
  Floodplain = floodplain_image_file
)

missing_images <- names(
  missing_images
)[
  is.na(missing_images)
]


if (length(missing_images) > 0) {
  
  stop(
    "The following landscape images could not be identified by filename: ",
    paste(
      missing_images,
      collapse = ", "
    ),
    ". Files present in the correct image directory: ",
    if (
      length(available_images) == 0
    ) {
      "none"
    } else {
      paste(
        basename(available_images),
        collapse = ", "
      )
    },
    ". Rename the images using Pantanal, Cerrado and Floodplain ",
    "in their filenames."
  )
}


pantanal_image <- read_landscape_image(
  pantanal_image_file
)


cerrado_image <- read_landscape_image(
  cerrado_image_file
)


floodplain_image <- read_landscape_image(
  floodplain_image_file
)


# ----------------------------------------------------------
# Draw complete figure
# ----------------------------------------------------------

draw_full_figure <- function() {
  
  graphics::layout(
    matrix(
      c(
        1, 2,
        1, 3,
        1, 4
      ),
      ncol = 2,
      byrow = TRUE
    ),
    widths = c(
      1.58,
      2.42
    ),
    heights = c(
      1,
      1,
      1
    )
  )
  
  poncho(
    table = species_relative_occurrence,
    gradient = gradient_landscape,
    show.grad = NULL,
    highlight.places = rownames(
      species_relative_occurrence
    ),
    col.places = colors_by_unit,
    cex.species = 0.74,
    xfactor = 0,
    xlab = "Sampling units",
    ylab = "Relative occurrence",
    cex.lab = 1.15,
    lwd = 0.8,
    border = "black"
  )
  
  units_by_landscape <- table(
    factor(
      variables_landscape$landscape,
      levels = landscape_order
    )
  )
  
  total_units <- sum(
    units_by_landscape
  )
  
  unit_width <- 84 /
    total_units
  
  start_x <- 0
  
  for (landscape_name in landscape_order) {
    
    number_of_units <- as.numeric(
      units_by_landscape[
        landscape_name
      ]
    )
    
    if (number_of_units > 0) {
      
      end_x <- start_x +
        number_of_units *
        unit_width -
        unit_width *
        0.1
      
      middle_x <- (
        start_x +
          end_x
      ) /
        2
      
      graphics::segments(
        start_x,
        120,
        end_x,
        120,
        xpd = TRUE,
        col = landscape_colors[
          landscape_name
        ],
        lwd = 3
      )
      
      graphics::text(
        middle_x,
        122.5,
        landscape_labels[
          landscape_name
        ],
        cex = 0.95,
        xpd = TRUE,
        font = 2,
        col = landscape_colors[
          landscape_name
        ]
      )
      
      start_x <- start_x +
        number_of_units *
        unit_width
    }
  }
  
  plot_photo_panel(
    pantanal_image,
    border_color = landscape_colors[
      "pantanal"
    ]
  )
  
  plot_photo_panel(
    cerrado_image,
    border_color = landscape_colors[
      "cerrado"
    ]
  )
  
  plot_photo_panel(
    floodplain_image,
    border_color = landscape_colors[
      "floodplain"
    ]
  )
}


# ----------------------------------------------------------
# Display and save figure
# ----------------------------------------------------------

output_jpg <- file.path(
  output_directory,
  "fig_relative_occurrence_species_landscapes.jpg"
)


output_tiff <- file.path(
  output_directory,
  "fig_relative_occurrence_species_landscapes.tiff"
)


output_pdf <- file.path(
  output_directory,
  "fig_relative_occurrence_species_landscapes.pdf"
)


if (interactive()) {
  
  draw_full_figure()
}


message(
  "Saving JPEG."
)


grDevices::jpeg(
  filename = output_jpg,
  width = 22,
  height = 15,
  units = "cm",
  res = 600,
  quality = 100,
  bg = "white"
)


tryCatch(
  {
    draw_full_figure()
  },
  finally = {
    grDevices::dev.off()
  }
)


message(
  "Saving TIFF."
)


grDevices::tiff(
  filename = output_tiff,
  width = 22,
  height = 15,
  units = "cm",
  res = 600,
  compression = "lzw",
  bg = "white"
)


tryCatch(
  {
    draw_full_figure()
  },
  finally = {
    grDevices::dev.off()
  }
)


message(
  "Saving PDF."
)


if (capabilities("cairo")) {
  
  grDevices::cairo_pdf(
    filename = output_pdf,
    width = 8.66,
    height = 5.90,
    bg = "white"
  )
  
} else {
  
  grDevices::pdf(
    file = output_pdf,
    width = 8.66,
    height = 5.90,
    bg = "white",
    useDingbats = FALSE
  )
}


tryCatch(
  {
    draw_full_figure()
  },
  finally = {
    grDevices::dev.off()
  }
)


# ----------------------------------------------------------
# Final summary
# ----------------------------------------------------------

cat(
  "\nAnalysis completed.\n\n"
)


cat(
  "Dataset:",
  basename(file_csv),
  "\n"
)


cat(
  "Records after filtering:",
  nrow(suspension_data),
  "\n"
)


cat(
  "Sampling units:",
  nrow(species_relative_occurrence),
  "\n"
)


cat(
  "Species:",
  ncol(species_relative_occurrence),
  "\n\n"
)


cat(
  "Pantanal image:\n",
  pantanal_image_file,
  "\n\n",
  sep = ""
)


cat(
  "Cerrado image:\n",
  cerrado_image_file,
  "\n\n",
  sep = ""
)


cat(
  "Floodplain image:\n",
  floodplain_image_file,
  "\n\n",
  sep = ""
)


cat(
  "JPEG saved at:\n",
  normalizePath(
    output_jpg,
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
  "\n",
  sep = ""
)