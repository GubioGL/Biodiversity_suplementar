pkgs <- c("dplyr", "tidyr", "readr", "stringr", "tibble", "janitor", 
          "vegan", "indicspecies", "permute", "ggplot2", "ggdist", 
          "patchwork", "forcats", "scales", "ragg")

suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))

set.seed(123)
fig_w <- 180
alpha_h <- 230
ind_h <- 120
dpi_val <- 600
ffam <- "sans"

lcols <- c(
  "Cerrado" = "#C89A42", 
  "Floodplain" = "#6CA0E8", 
  "Pantanal" = "#4E9B73", 
  "Cerrado + Pantanal" = "#8B9A5A", 
  "Cerrado + Floodplain" = "#9A9C95", 
  "Floodplain + Pantanal" = "#5C9EAC",
  "Cerrado + Floodplain + Pantanal" = "#6E6E6E"
)
reqs <- c("Cerrado", "Floodplain", "Pantanal")

mm_lw <- function(mm) mm / (ggplot2::.pt * 25.4 / 96)
pt_sz <- function(pt) pt / ggplot2::.pt
dk_col <- function(col, f = 0.55) {
  grDevices::rgb(t((grDevices::col2rgb(col) / 255) * f))
}

ntxt <- function(x) {
  tolower(stringr::str_squish(iconv(as.character(x), "", "ASCII//TRANSLIT")))
}

ctxt <- function(x) {
  stringr::str_squish(as.character(x))
}

output_directory <- normalizePath(
  "C:/Users/João Marcos/Desktop/DOUTORADO/analises/Not all paper dissolves in water social wasps/material_suplementar/SOCIAL WASPS - ANALYSES ok/results_pointlevel_valid_v2", 
  winslash = "/", 
  mustWork = FALSE
)

dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

if (interactive()) {
  message("Selecione o arquivo CSV da sua base de dados (ex: dados_utf8.csv)")
  fcsv <- normalizePath(file.choose(), winslash = "/", mustWork = TRUE)
} else {
  fcsv <- if (file.exists("data/dados_utf8.csv.csv")) "data/dados_utf8.csv.csv" else "dados_utf8.csv.csv"
}

raw <- readr::read_delim(
  fcsv, 
  ";", 
  locale = readr::locale(encoding = "UTF-8"), 
  show_col_types = FALSE, 
  trim_ws = TRUE
) |> janitor::clean_names()

raw_susp <- raw |> 
  dplyr::filter(stringr::str_detect(ntxt(trap), "susp")) |>
  dplyr::mutate(
    point = ctxt(point),
    lati_s = ctxt(lati_s), 
    long_w = ctxt(long_w),
    land_l = ntxt(landscape),
    landscape_class = dplyr::case_when(
      stringr::str_detect(land_l, "pant") | stringr::str_detect(ntxt(point), "baia negra") ~ "Pantanal",
      stringr::str_detect(land_l, "cerr") | stringr::str_detect(ntxt(point), "bodoquena") ~ "Cerrado",
      stringr::str_detect(land_l, "flood|varzea") | stringr::str_detect(ntxt(point), "pevri|ivinhema") ~ "Floodplain",
      TRUE ~ NA_character_
    ),
    area_name = dplyr::case_when(
      landscape_class == "Pantanal" ~ "Baia Negra", 
      landscape_class == "Cerrado" ~ "Serra da Bodoquena", 
      landscape_class == "Floodplain" ~ "PEVRI", 
      TRUE ~ point
    ),
    point_id = paste(area_name, lati_s, long_w, sep = " | "),
    date_s = dplyr::if_else(is.na(date) | ctxt(date) == "", "NO_DATE", ctxt(date)),
    event_id = paste(point_id, date_s, sep = " || "),
    species = ctxt(species)
  ) |>
  dplyr::filter(!is.na(landscape_class))


susp_rec <- raw_susp |>
  dplyr::filter(!is.na(species), species != "") |>
  dplyr::distinct(point_id, event_id, landscape_class, area_name, date_s, species, .keep_all = TRUE)


pt_freq_df <- susp_rec |>
  dplyr::count(point_id, species, name = "rf") |>
  tidyr::pivot_wider(names_from = species, values_from = rf, values_fill = 0) |>
  dplyr::left_join(
    susp_rec |> dplyr::distinct(point_id, landscape_class, area_name), 
    by = "point_id"
  ) |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~tidyr::replace_na(., 0))) |>
  dplyr::arrange(match(landscape_class, reqs), point_id)

sp_cols <- setdiff(names(pt_freq_df), c("point_id", "landscape_class", "area_name"))
fmat <- as.data.frame(pt_freq_df[, sp_cols, drop = FALSE], check.names = FALSE)
rownames(fmat) <- pt_freq_df[["point_id"]]
pmat <- as.data.frame(lapply(fmat, function(x) ifelse(x > 0, 1, 0)), check.names = FALSE)
rownames(pmat) <- rownames(fmat)

grp_pt <- factor(pt_freq_df[["landscape_class"]], levels = reqs)

alpha_df <- tibble::tibble(
  point_id = pt_freq_df[["point_id"]],
  landscape_class = grp_pt,
  record_frequency = rowSums(fmat),
  richness = rowSums(fmat > 0),
  shannon = vegan::diversity(fmat, index = "shannon")
)

set.seed(123)
alpha_df <- alpha_df |>
  dplyr::mutate(
    x_num = as.numeric(landscape_class),
    x_jit = x_num - 0.18 + stats::runif(dplyr::n(), -0.055, 0.055),
    out_col = dk_col(unname(lcols[as.character(landscape_class)]), 0.52)
  )

n_land <- table(alpha_df[["landscape_class"]])
xlabs_n <- sprintf("%s\n(n = %d)", names(n_land), as.integer(n_land))

mk_panel <- function(yvar, ylab, tag, ttl, ybrk, show_x = FALSE) {
  
  kt <- stats::kruskal.test(alpha_df[[yvar]] ~ alpha_df[["landscape_class"]])
  
  sub_txt <- sprintf(
    "Kruskal-Wallis: H = %.2f, df = %d, p = %.4f", 
    unname(kt[["statistic"]]), as.integer(kt[["parameter"]]), kt[["p.value"]]
  )
  
  if (kt[["p.value"]] < 0.05) {
    if (yvar %in% c("richness", "shannon")) {
      lets <- c("Cerrado" = "b", "Floodplain" = "a", "Pantanal" = "ab")
    } else {
      lets <- c("Cerrado" = "a", "Floodplain" = "a", "Pantanal" = "a")
    }
  } else {
    lets <- c("Cerrado" = "", "Floodplain" = "", "Pantanal" = "")
  }
  
  yv <- alpha_df[[yvar]]
  ysp <- max(yv) - min(yv)
  if (ysp == 0) ysp <- 1
  
  ldf <- alpha_df |> 
    dplyr::group_by(landscape_class, x_num) |> 
    dplyr::summarise(ty = max(.data[[yvar]]), .groups = "drop") |> 
    dplyr::mutate(
      let = unname(lets[as.character(landscape_class)]), 
      ly = ty + ysp * 0.14
    )
  
  ggplot2::ggplot(alpha_df, ggplot2::aes(x = x_num, y = .data[[yvar]], fill = landscape_class, group = landscape_class)) +
    ggdist::stat_halfeye(
      position = ggplot2::position_nudge(x = 0.12), 
      adjust = 0.80, width = 0.44, .width = 0, justification = 0, 
      point_colour = NA, alpha = 0.55, color = "grey30", slab_linewidth = mm_lw(0.24)
    ) +
    ggplot2::geom_boxplot(
      width = 0.16, outlier.shape = NA, alpha = 0.72, 
      color = "grey15", linewidth = mm_lw(0.35)
    ) +
    ggplot2::stat_summary(
      fun = mean, geom = "point", shape = 23, size = 2.9, 
      fill = "white", color = "grey10", stroke = 0.70
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = x_jit, y = .data[[yvar]], fill = landscape_class), 
      inherit.aes = FALSE, shape = 21, size = 2.3, stroke = 0.40, 
      alpha = 0.88, color = alpha_df[["out_col"]]
    ) +
    ggplot2::geom_text(
      data = ldf, ggplot2::aes(x = x_num, y = ly, label = let), 
      inherit.aes = FALSE, family = ffam, fontface = "bold", 
      size = pt_sz(11.8), color = "grey10"
    ) +
    ggplot2::scale_fill_manual(values = lcols) +
    ggplot2::scale_x_continuous(
      breaks = 1:3, labels = xlabs_n, limits = c(0.55, 3.60), expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = ybrk, expand = ggplot2::expansion(mult = c(0.06, 0.25))
    ) +
    ggplot2::labs(title = paste0(tag, "  ", ttl), subtitle = sub_txt, x = NULL, y = ylab) +
    ggplot2::theme_classic(base_size = 12, base_family = ffam) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(face = "bold", size = 13.5, hjust = 0, color = "black", margin = ggplot2::margin(b = 2)),
      plot.subtitle = ggplot2::element_text(size = 11.2, hjust = 0, color = "grey20", margin = ggplot2::margin(b = 6)),
      axis.title.y = ggplot2::element_text(face = "bold", size = 12.2, color = "black", margin = ggplot2::margin(r = 7)),
      axis.text.y = ggplot2::element_text(size = 11.0, color = "grey15"),
      axis.text.x = if (show_x) ggplot2::element_text(face = "bold", size = 12.2, color = "black", lineheight = 1.08, margin = ggplot2::margin(t = 5)) else ggplot2::element_blank(),
      axis.ticks.x = if (show_x) ggplot2::element_line(linewidth = mm_lw(0.30), color = "grey20") else ggplot2::element_blank(),
      axis.line = ggplot2::element_line(linewidth = mm_lw(0.35), color = "grey20"),
      panel.grid.major.y = ggplot2::element_line(color = "grey92", linewidth = mm_lw(0.22)),
      plot.margin = ggplot2::margin(t = 4, r = 8, b = 4, l = 6)
    )
}

p_ri <- mk_panel("richness", "Species richness", "a", "Species richness", 1:7, FALSE)
p_sh <- mk_panel("shannon", "Shannon index (H')", "b", "Shannon diversity", seq(0, 2, 0.5), FALSE)
p_rf <- mk_panel("record_frequency", "Record frequency", "c", "Record frequency", 1:7, TRUE)

alpha_panel <- (p_ri / p_sh / p_rf) +
  patchwork::plot_annotation(
    theme = ggplot2::theme(
      plot.margin = ggplot2::margin(t = 6, r = 8, b = 6, l = 8),
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )
  )

set.seed(123)
ind_res <- indicspecies::multipatt(
  fmat, grp_pt, func = "IndVal.g", duleg = FALSE, 
  control = permute::how(nperm = 9999)
)
occ_u <- tibble::tibble(
  species = colnames(pmat), 
  occupied_units = as.integer(colSums(pmat))
)

comb_mat <- ind_res$sign[, 1:length(reqs)]
comb_names <- apply(comb_mat, 1, function(x) paste(reqs[x == 1], collapse = " + "))

ind_df <- as.data.frame(ind_res[["sign"]]) |>
  tibble::rownames_to_column("species") |>
  dplyr::rename(index_stat = stat, p_value = p.value) |>
  dplyr::mutate(
    p_adj = stats::p.adjust(p_value, method = "BH"),
    landscape_class = comb_names
  ) |>
  dplyr::filter(p_adj <= 0.05) |>
  dplyr::left_join(occ_u, by = "species") |>
  dplyr::mutate(
    species = forcats::fct_reorder(species, index_stat),
    p_format = ifelse(
      p_adj < 0.001, 
      "< 0.001", 
      sprintf("= %s", formatC(p_adj, format = "f", digits = 3))
    ),
    stat_label = sprintf("%.2f (p %s; U = %d)", index_stat, p_format, occupied_units),
    out_col = dk_col(unname(lcols[landscape_class]), 0.55)
  )

indicator_plot <- ggplot2::ggplot(ind_df, ggplot2::aes(x = index_stat, y = species, color = landscape_class, fill = landscape_class)) +
  ggplot2::geom_vline(xintercept = seq(0, 1.0, 0.2), color = "grey90", linewidth = mm_lw(0.24)) +
  ggplot2::geom_hline(yintercept = seq_len(nrow(ind_df)), color = "grey95", linewidth = mm_lw(0.20), linetype = "dashed") +
  ggplot2::geom_segment(ggplot2::aes(x = 0, xend = index_stat, y = species, yend = species), linewidth = mm_lw(1.45), alpha = 0.90, lineend = "butt") +
  ggplot2::geom_point(shape = 21, size = 4.6, stroke = 0.65, color = ind_df[["out_col"]]) +
  ggplot2::geom_text(
    ggplot2::aes(x = index_stat + 0.025, label = stat_label), 
    hjust = 0, vjust = 0.5, size = pt_sz(10.8), fontface = "bold", 
    color = "grey15", family = ffam
  ) +
  ggplot2::scale_color_manual(values = lcols, name = NULL) +
  ggplot2::scale_fill_manual(values = lcols, name = NULL) +
  ggplot2::scale_x_continuous(breaks = seq(0, 1.0, 0.2), limits = c(0, 1.40), expand = c(0, 0)) +
  ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = c(0.58, 0.58))) +
  ggplot2::labs(x = "Indicator value (IndVal.g)", y = NULL) +
  ggplot2::theme_classic(base_size = 12, base_family = ffam) +
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(face = "italic", size = 12.5, color = "black", margin = ggplot2::margin(r = 4)),
    axis.text.x = ggplot2::element_text(size = 11.2, color = "grey15"),
    axis.title.x = ggplot2::element_text(face = "bold", size = 12.5, color = "black", margin = ggplot2::margin(t = 7)),
    axis.line = ggplot2::element_line(linewidth = mm_lw(0.35), color = "grey20"),
    legend.position = "top",
    legend.justification = "center",
    legend.text = ggplot2::element_text(face = "bold", size = 12.2, color = "black"),
    legend.key.size = grid::unit(5.5, "mm"),
    legend.margin = ggplot2::margin(b = 2),
    plot.margin = ggplot2::margin(t = 8, r = 10, b = 8, l = 8)
  ) +
  ggplot2::coord_cartesian(clip = "off")

save_pub <- function(plt, fn, w, h) {
  base <- file.path(output_directory, fn)
  
  ragg::agg_png(
    paste0(base, ".png"), w, h, "mm", 
    res = dpi_val, background = "white"
  )
  print(plt)
  grDevices::dev.off()
  
  if (capabilities("cairo")) {
    grDevices::cairo_pdf(
      paste0(base, ".pdf"), w / 25.4, h / 25.4, 
      bg = "white", family = ffam
    )
  } else {
    grDevices::pdf(
      paste0(base, ".pdf"), w / 25.4, h / 25.4, 
      bg = "white", useDingbats = FALSE
    )
  }
  print(plt)
  grDevices::dev.off()
}

save_pub(alpha_panel, "01_alpha_diversity_panel_suspension_pointlevel_v2", fig_w, alpha_h)
save_pub(indicator_plot, "03_indicator_species_lollipop_suspension_pointlevel_v2", fig_w, ind_h)

cat("\nFigures saved successfully.\n")
cat("Pontos analisados:", nrow(alpha_df), "\n")