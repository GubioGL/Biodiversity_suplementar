set.seed(123)

sampling_period <- "2024-2025"

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
  "cowplot",
  "scales",
  "grid",
  "zetadiv",
  "ragg",
  "systemfonts"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(
    missing_packages,
    dependencies = c("Depends", "Imports", "LinkingTo")
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(janitor)
  library(ggplot2)
  library(cowplot)
  library(scales)
  library(grid)
  library(zetadiv)
  library(ragg)
  library(systemfonts)
})

cowplot::set_null_device("agg")

output_directory <- normalizePath(
  "C:/Users/João Marcos/Desktop/DOUTORADO/analises/Not all paper dissolves in water social wasps/material_suplementar/ZETA/output", 
  winslash = "/", 
  mustWork = FALSE
)
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)


normalize_path_safe <- function(path) {
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

if (interactive()) {
  message("Selecione o arquivo CSV da sua base de dados (ex: dados_utf8.csv)")
  file_csv <- normalizePath(file.choose(), winslash = "/", mustWork = TRUE)
} else {
  file_csv <- if (file.exists("data/dados_utf8.csv.csv")) "data/dados_utf8.csv.csv" else "dados_utf8.csv"
}


max_zeta_order_user <- 8
zeta_samples <- 5000

figure_width_mm <- 150
figure_height_mm <- 165
figure_width_in <- figure_width_mm / 25.4
figure_height_in <- figure_height_mm / 25.4
export_dpi <- 600
background_color <- "white"
preview_on_screen <- interactive()

font_candidates <- c("Arial", "Helvetica", "Liberation Sans", "Arimo")

pick_font_family <- function(candidates) {
  installed <- tryCatch(
    unique(systemfonts::system_fonts()$family),
    error = function(e) character(0)
  )
  found <- candidates[candidates %in% installed]
  if (length(found) > 0) found[1] else "sans"
}

font_family <- pick_font_family(font_candidates)

font_size <- c(
  tag = 12.0,
  axis_title = 8.5,
  axis_text = 7.8,
  legend_title = 7.8,
  legend_text = 6.6,
  footer = 6.2
)

pt_to_size <- function(pt) pt / ggplot2::.pt
mm_to_linewidth <- function(mm) mm / (ggplot2::.pt * 25.4 / 96)

landscape_colors <- c(
  "Cerrado" = "#C89A42",
  "Floodplain" = "#3C89FF",
  "Pantanal" = "#2BAE66",
  "All landscapes" = "#2C3E50"
)

landscape_shapes <- c(
  "Cerrado" = 21,
  "Floodplain" = 22,
  "Pantanal" = 24,
  "All landscapes" = 23
)

landscape_linetypes <- c(
  "Cerrado" = "solid",
  "Floodplain" = "solid",
  "Pantanal" = "solid",
  "All landscapes" = "22"
)

landscape_order <- c("Cerrado", "Floodplain", "Pantanal")
plot_order <- c("Cerrado", "Floodplain", "Pantanal", "All landscapes")
dodge_offsets <- c("Cerrado" = -0.09, "Floodplain" = -0.03, "Pantanal" = 0.03, "All landscapes" = 0.09)

clean_text <- function(x) stringr::str_squish(as.character(x))

normalize_text <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  tolower(trimws(x))
}

pick_column <- function(data, candidates) {
  matches <- candidates[candidates %in% names(data)]
  if (length(matches) == 0) return(NA_character_)
  matches[1]
}

optional_character_column <- function(data, column_name) {
  if (!is.na(column_name) && column_name %in% names(data)) {
    return(clean_text(data[[column_name]]))
  }
  rep(NA_character_, nrow(data))
}

extract_zeta_output <- function(zeta_object, group_name) {
  zeta_order <- as.numeric(zeta_object$zeta.order)
  zeta_values <- as.numeric(zeta_object$zeta.val)
  
  if (length(zeta_order) == 0 || length(zeta_values) == 0) {
    stop("The zetadiv result did not contain valid zeta values for: ", group_name)
  }
  
  zeta_sd <- if (
    !is.null(zeta_object$zeta.val.sd) &&
    length(zeta_object$zeta.val.sd) == length(zeta_values)
  ) {
    as.numeric(zeta_object$zeta.val.sd)
  } else {
    rep(NA_real_, length(zeta_values))
  }
  
  combinations <- if (
    !is.null(zeta_object$combinations) &&
    length(zeta_object$combinations) == length(zeta_values)
  ) {
    as.numeric(zeta_object$combinations)
  } else {
    rep(NA_real_, length(zeta_values))
  }
  
  zeta_1 <- zeta_values[zeta_order == 1][1]
  
  if (length(zeta_1) == 0 || is.na(zeta_1) || zeta_1 <= 0) {
    stop("A valid first-order zeta value was not obtained for: ", group_name)
  }
  
  tibble::tibble(
    landscape = group_name,
    order = zeta_order,
    combinations = combinations,
    zeta = zeta_values,
    zeta_sd = zeta_sd
  ) |>
    dplyr::arrange(order) |>
    dplyr::mutate(
      zeta_1 = zeta_1,
      zeta_norm = zeta / zeta_1,
      zeta_sd_norm = zeta_sd / zeta_1,
      zeta_lcl_norm = pmax(0, zeta_norm - zeta_sd_norm),
      zeta_ucl_norm = pmin(1, zeta_norm + zeta_sd_norm),
      zeta_ratio = zeta / dplyr::lag(zeta)
    )
}

run_zeta_decline <- function(community_matrix, group_name, orders, samples = 5000) {
  community_matrix <- as.data.frame(community_matrix)
  community_matrix[] <- lapply(community_matrix, function(x) as.integer(as.numeric(x) > 0))
  
  community_matrix <- community_matrix[rowSums(community_matrix) > 0, , drop = FALSE]
  community_matrix <- community_matrix[, colSums(community_matrix) > 0, drop = FALSE]
  
  group_orders <- orders[orders <= nrow(community_matrix)]
  if (length(group_orders) < 2) {
    stop("Too few zeta orders could be calculated for: ", group_name)
  }
  
  names(community_matrix) <- make.names(names(community_matrix), unique = TRUE)
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
  
  extract_zeta_output(zeta_object, group_name)
}

fit_zeta_models <- function(zeta_data) {
  landscape_names <- unique(as.character(zeta_data$landscape))
  
  model_results <- lapply(
    landscape_names,
    function(landscape_name) {
      current_data <- zeta_data |>
        dplyr::filter(
          as.character(landscape) == landscape_name,
          order >= 1,
          zeta_norm > 0,
          is.finite(zeta_norm)
        )
      
      if (nrow(current_data) < 3) {
        return(tibble::tibble(
          landscape = landscape_name,
          exponential_AIC = NA_real_,
          powerlaw_AIC = NA_real_,
          best_model = NA_character_,
          delta_AIC_power_minus_exp = NA_real_,
          akaike_weight_pl = NA_real_
        ))
      }
      
      exponential_model <- stats::lm(log10(zeta_norm) ~ order, data = current_data)
      powerlaw_model <- stats::lm(log10(zeta_norm) ~ log10(order), data = current_data)
      
      exponential_aic <- stats::AIC(exponential_model)
      powerlaw_aic <- stats::AIC(powerlaw_model)
      
      min_aic <- min(exponential_aic, powerlaw_aic)
      rel_lik_exp <- exp(-0.5 * (exponential_aic - min_aic))
      rel_lik_pl <- exp(-0.5 * (powerlaw_aic - min_aic))
      w_pl <- rel_lik_pl / (rel_lik_exp + rel_lik_pl)
      
      tibble::tibble(
        landscape = landscape_name,
        exponential_AIC = exponential_aic,
        powerlaw_AIC = powerlaw_aic,
        best_model = ifelse(powerlaw_aic < exponential_aic, "Power-law", "Exponential"),
        delta_AIC_power_minus_exp = powerlaw_aic - exponential_aic,
        akaike_weight_pl = w_pl
      )
    }
  )
  
  dplyr::bind_rows(model_results)
}

raw_data <- readr::read_delim(
  file = file_csv,
  delim = ";",
  locale = readr::locale(encoding = "UTF-8"),
  show_col_types = FALSE,
  trim_ws = TRUE
) |>
  janitor::clean_names()

species_column <- pick_column(raw_data, c("species", "especie"))
point_column <- pick_column(raw_data, c("point", "ponto", "site", "site_id", "local", "localidade"))
landscape_column <- pick_column(raw_data, c("landscape", "paisagem", "landscape_class"))
trap_column <- pick_column(raw_data, c("trap", "armadilha", "method", "metodo"))
collection_column <- pick_column(raw_data, c("collection", "coleta", "sample_type", "tipo_coleta"))
date_column <- pick_column(raw_data, c("date", "data"))
latitude_column <- pick_column(raw_data, c("lati_s", "latitude", "lat"))
longitude_column <- pick_column(raw_data, c("long_w", "longitude", "lon", "long"))
classification_column <- pick_column(raw_data, c("classification", "classificacao"))

if (is.na(species_column) || is.na(point_column) || is.na(trap_column) ||
    is.na(latitude_column) || is.na(longitude_column)) {
  stop("One or more required columns (species, point, trap, lat, lon) were not found.")
}

species_values <- clean_text(raw_data[[species_column]])
point_values <- clean_text(raw_data[[point_column]])
trap_values <- clean_text(raw_data[[trap_column]])
latitude_values <- clean_text(raw_data[[latitude_column]])
longitude_values <- clean_text(raw_data[[longitude_column]])
landscape_values <- optional_character_column(raw_data, landscape_column)
collection_values <- optional_character_column(raw_data, collection_column)
date_values <- optional_character_column(raw_data, date_column)
classification_values <- optional_character_column(raw_data, classification_column)

data_clean <- raw_data |>
  dplyr::mutate(
    species = species_values,
    point = point_values,
    trap = trap_values,
    trap_normalized = normalize_text(trap_values),
    collection = normalize_text(collection_values),
    landscape = normalize_text(landscape_values),
    classification = classification_values,
    date = date_values,
    date_standardized = dplyr::if_else(
      is.na(date_values) | date_values == "",
      "NO_DATE",
      date_values
    ),
    lati_s = latitude_values,
    long_w = longitude_values,
    trap_standardized = dplyr::case_when(
      stringr::str_detect(trap_normalized, "susp") ~ "suspension",
      stringr::str_detect(trap_normalized, "puc") ~ "puca",
      stringr::str_detect(trap_normalized, "mala") ~ "malaise",
      TRUE ~ trap_normalized
    ),
    landscape_class = dplyr::case_when(
      stringr::str_detect(landscape, "pant") ~ "Pantanal",
      stringr::str_detect(landscape, "cerr|bodoq|sdf") ~ "Cerrado",
      stringr::str_detect(landscape, "flood|varzea|ivinhema|pevri|rf") ~ "Floodplain",
      stringr::str_detect(normalize_text(point), "baia negra|baianegra") ~ "Pantanal",
      stringr::str_detect(normalize_text(point), "bodoquena") ~ "Cerrado",
      stringr::str_detect(normalize_text(point), "pevri|ivinhema|varzea") ~ "Floodplain",
      TRUE ~ NA_character_
    ),
    area_name = dplyr::case_when(
      landscape_class == "Pantanal" ~ "Baia Negra",
      landscape_class == "Cerrado" ~ "Serra da Bodoquena",
      landscape_class == "Floodplain" ~ "PEVRI",
      TRUE ~ point
    ),
    point_id = dplyr::case_when(
      !is.na(lati_s) & lati_s != "" & !is.na(long_w) & long_w != "" ~
        paste(area_name, lati_s, long_w, sep = " | "),
      TRUE ~
        paste(area_name, point, sep = " | ")
    ),
    event_id = paste(point_id, date_standardized, sep = " || ")
  ) |>
  dplyr::filter(
    !is.na(species), species != "",
    !is.na(trap_standardized), trap_standardized != "",
    !is.na(landscape_class),
    !is.na(point_id), point_id != ""
  )

suspension_rows <- data_clean |>
  dplyr::filter(trap_standardized == "suspension")

if (nrow(suspension_rows) == 0) {
  stop("No suspension-trap records were found.")
}

suspension_records <- suspension_rows |>
  dplyr::distinct(
    point_id, event_id, landscape_class, area_name, date_standardized, species,
    .keep_all = TRUE
  )

community_data <- suspension_records |>
  dplyr::mutate(presence = 1L) |>
  dplyr::distinct(landscape_class, point_id, species, presence) |>
  tidyr::pivot_wider(names_from = species, values_from = presence, values_fill = 0) |>
  dplyr::arrange(match(landscape_class, landscape_order), point_id)

metadata <- community_data |>
  dplyr::select(landscape_class, point_id) |>
  dplyr::mutate(
    landscape_class = as.character(landscape_class),
    point_id = as.character(point_id)
  )

community_matrix <- community_data |>
  dplyr::select(-landscape_class, -point_id) |>
  as.data.frame()
rownames(community_matrix) <- metadata$point_id

keep_rows <- rowSums(community_matrix) > 0
community_matrix <- community_matrix[keep_rows, , drop = FALSE]
metadata <- metadata[keep_rows, , drop = FALSE]
community_matrix <- community_matrix[, colSums(community_matrix) > 0, drop = FALSE]

units_by_landscape <- metadata |>
  dplyr::count(landscape_class, name = "n_units") |>
  dplyr::filter(landscape_class %in% landscape_order) |>
  dplyr::arrange(match(landscape_class, landscape_order))

common_maximum_order <- min(max_zeta_order_user, min(units_by_landscape$n_units))
orders_used <- seq_len(common_maximum_order)

zeta_landscape_results <- lapply(landscape_order, function(landscape_name) {
  selected_rows <- which(metadata$landscape_class == landscape_name)
  run_zeta_decline(community_matrix = community_matrix[selected_rows, , drop = FALSE], group_name = landscape_name, orders = orders_used, samples = zeta_samples)
})
zeta_total <- run_zeta_decline(community_matrix = community_matrix, group_name = "All landscapes", orders = orders_used, samples = zeta_samples)

zeta_results <- dplyr::bind_rows(zeta_landscape_results, zeta_total)
zeta_models <- fit_zeta_models(zeta_results)

zeta_results <- zeta_results |> dplyr::mutate(landscape = factor(landscape, levels = plot_order), x_plot = ifelse(order == 1, as.numeric(order), as.numeric(order) + unname(dodge_offsets[as.character(landscape)])))
zeta_1_lookup <- zeta_results |> dplyr::filter(order == 1) |> dplyr::select(landscape, zeta_1)

legend_table_df <- tibble::tibble(landscape = factor(plot_order, levels = plot_order)) |>
  dplyr::left_join(zeta_1_lookup, by = "landscape") |>
  dplyr::left_join(zeta_models |> dplyr::mutate(landscape = factor(landscape, levels = plot_order)), by = "landscape") |>
  dplyr::mutate(
    row_id = dplyr::row_number(), y_pos = 0.955 - (row_id - 1) * 0.088,
    color = unname(landscape_colors[as.character(landscape)]), shape = unname(landscape_shapes[as.character(landscape)]),
    linetype = unname(landscape_linetypes[as.character(landscape)]), short_model = ifelse(best_model == "Power-law", "Power-law", "Exponential"),
    stat_label = sprintf("\u03b61 = %.2f \u00b7 %s (\u0394AIC = %+.1f)", zeta_1, short_model, delta_AIC_power_minus_exp)
  )

base_panel_theme <- ggplot2::theme_bw(base_family = font_family) +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_line(color = "grey92", linewidth = mm_to_linewidth(0.18), linetype = "dashed"),
    panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.border = ggplot2::element_rect(fill = NA, color = "grey20", linewidth = mm_to_linewidth(0.40)),
    axis.title = ggplot2::element_text(size = font_size[["axis_title"]], color = "grey10"),
    axis.text = ggplot2::element_text(size = font_size[["axis_text"]], color = "grey15"),
    axis.ticks = ggplot2::element_line(color = "grey20", linewidth = mm_to_linewidth(0.30)),
    axis.ticks.length = grid::unit(1.1, "mm"), plot.background = ggplot2::element_rect(fill = "white", color = NA),
    plot.margin = ggplot2::margin(1, 2, 1, 2, unit = "mm"), legend.position = "none"
  )

box_xmin <- 4.45; box_xmax <- 8.16; box_ymin <- 0.64; box_ymax <- 1.01

panel_a_decline <- ggplot2::ggplot(zeta_results, ggplot2::aes(x = x_plot, y = zeta_norm, color = landscape, fill = landscape, linetype = landscape, shape = landscape)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = order, ymin = zeta_lcl_norm, ymax = zeta_ucl_norm), alpha = 0.12, color = NA, show.legend = FALSE) +
  ggplot2::geom_line(linewidth = mm_to_linewidth(0.65)) + ggplot2::geom_point(size = 2.5, color = "white", stroke = 0.50) +
  ggplot2::annotate("rect", xmin = box_xmin, xmax = box_xmax, ymin = box_ymin, ymax = box_ymax, fill = scales::alpha("white", 0.96), color = "grey75", linewidth = mm_to_linewidth(0.25)) +
  ggplot2::annotate("segment", x = box_xmin + 0.10, xend = box_xmin + 0.42, y = legend_table_df$y_pos, yend = legend_table_df$y_pos, color = legend_table_df$color, linetype = legend_table_df$linetype, linewidth = mm_to_linewidth(0.65)) +
  ggplot2::annotate("point", x = box_xmin + 0.26, y = legend_table_df$y_pos, shape = legend_table_df$shape, fill = legend_table_df$color, color = "white", size = 2.2, stroke = 0.45) +
  ggplot2::annotate("text", x = box_xmin + 0.50, y = legend_table_df$y_pos, label = as.character(legend_table_df$landscape), hjust = 0, vjust = 0.5, family = font_family, fontface = "bold", size = pt_to_size(font_size[["legend_text"]]), color = "grey10") +
  ggplot2::annotate("text", x = box_xmax - 0.10, y = legend_table_df$y_pos, label = legend_table_df$stat_label, hjust = 1, vjust = 0.5, family = font_family, size = pt_to_size(font_size[["legend_text"]] - 0.3), color = "grey25") +
  ggplot2::scale_color_manual(values = landscape_colors) + ggplot2::scale_fill_manual(values = landscape_colors) + ggplot2::scale_shape_manual(values = landscape_shapes) + ggplot2::scale_linetype_manual(values = landscape_linetypes) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1.04), breaks = seq(0, 1, by = 0.2), expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
  ggplot2::scale_x_continuous(limits = c(0.85, common_maximum_order + 0.25), breaks = orders_used, expand = c(0, 0)) +
  ggplot2::labs(x = NULL, y = expression("Normalized zeta diversity (" * zeta[i] / zeta[1] * ")")) + base_panel_theme

ratio_data <- zeta_results |> dplyr::filter(order > 1, !is.na(zeta_ratio), is.finite(zeta_ratio))
panel_b_ratio <- ggplot2::ggplot(ratio_data, ggplot2::aes(x = x_plot, y = zeta_ratio, color = landscape, fill = landscape, linetype = landscape, shape = landscape)) +
  ggplot2::geom_vline(xintercept = 2, linetype = "dotted", color = "grey70", linewidth = mm_to_linewidth(0.28)) +
  ggplot2::geom_line(linewidth = mm_to_linewidth(0.65)) + ggplot2::geom_point(size = 2.5, color = "white", stroke = 0.50) +
  ggplot2::scale_color_manual(values = landscape_colors) + ggplot2::scale_fill_manual(values = landscape_colors) + ggplot2::scale_shape_manual(values = landscape_shapes) + ggplot2::scale_linetype_manual(values = landscape_linetypes) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1.04), breaks = seq(0, 1, by = 0.2), expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
  ggplot2::scale_x_continuous(limits = c(0.85, common_maximum_order + 0.25), breaks = orders_used, expand = c(0, 0)) +
  ggplot2::labs(x = "Zeta order (i: number of combined sampling units)", y = expression("Species retention ratio (" * zeta[i] / zeta[i - 1] * ")")) + base_panel_theme

inches_to_x <- function(inches) inches / figure_width_in; inches_to_y <- function(inches) inches / figure_height_in
make_box <- function(x_in, y_in, w_in, h_in) c(x = inches_to_x(x_in), y = inches_to_y(y_in), w = inches_to_x(w_in), h = inches_to_y(h_in))
draw_box <- function(figure, plot, box) figure + cowplot::draw_plot(plot, x = box["x"], y = box["y"], width = box["w"], height = box["h"])

draw_panel_title <- function(figure, tag, x_in, y_in) {
  figure + cowplot::draw_label(tag, x = inches_to_x(x_in), y = inches_to_y(y_in), hjust = 0, vjust = 0, fontface = "bold", fontfamily = font_family, size = font_size[["tag"]])
}

margin_x_in <- 0.08; panel_w_in <- figure_width_in - 2 * margin_x_in
footer_h_in <- 0.22; panel_b_y_in <- footer_h_in + 0.04; panel_b_h_in <- 2.82
panel_a_y_in <- panel_b_y_in + panel_b_h_in + 0.34; panel_a_h_in <- 2.72
box_A <- make_box(margin_x_in, panel_a_y_in, panel_w_in, panel_a_h_in)
box_B <- make_box(margin_x_in, panel_b_y_in, panel_w_in, panel_b_h_in)

footer_text <- paste0("Matrix: ", nrow(community_matrix), " sampling units \U00D7 ", ncol(community_matrix), " species (", nrow(suspension_records), " deduplicated suspension-trap records) \U00B7 Monte Carlo: ", format(zeta_samples, big.mark = ","), " \U00B7 Shaded ribbons: \u00B11 SD")

zeta_figure <- cowplot::ggdraw() |>
  draw_box(panel_a_decline, box_A) |>
  draw_box(panel_b_ratio, box_B) |>
  draw_panel_title("a", margin_x_in, panel_a_y_in + panel_a_h_in + 0.05) |>
  draw_panel_title("b", margin_x_in, panel_b_y_in + panel_b_h_in + 0.05) +
  cowplot::draw_label(footer_text, x = 0.5, y = inches_to_y(0.08), hjust = 0.5, vjust = 0.5, fontfamily = font_family, size = font_size[["footer"]], color = "grey35")

output_png <- file.path(output_directory, "Figure_5_zeta_diversity_v2.png")
output_pdf <- file.path(output_directory, "Figure_5_zeta_diversity_v2.pdf")

ragg::agg_png(filename = output_png, width = figure_width_mm, height = figure_height_mm, units = "mm", res = export_dpi, background = background_color)
tryCatch(print(zeta_figure), finally = grDevices::dev.off())

if (capabilities("cairo")) {
  grDevices::cairo_pdf(filename = output_pdf, width = figure_width_mm / 25.4, height = figure_height_mm / 25.4, bg = background_color, family = font_family)
} else {
  grDevices::pdf(file = output_pdf, width = figure_width_mm / 25.4, height = figure_height_mm / 25.4, bg = background_color, useDingbats = FALSE)
}
tryCatch(print(zeta_figure), finally = grDevices::dev.off())


cat("\nAnalysis completed successfully!\n")
cat(sprintf("Pontos amostrais analisados (n): %d\n", nrow(community_matrix)))
cat("PNG saved at:\n", normalizePath(output_png, winslash = "/", mustWork = TRUE), "\n")