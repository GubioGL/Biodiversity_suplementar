pkgs <- c("ggplot2", "png", "jpeg", "tiff", "grid", "ragg", "systemfonts", "tools")
miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss) > 0) install.packages(miss, dependencies = TRUE)
suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))

bdir <- "C:/Users/João Marcos/Desktop/DOUTORADO/analises/Not all paper dissolves in water social wasps/material_suplementar/material suplementar prancha/prancha 2"
if (dir.exists(bdir)) {
  wdir <- normalizePath(bdir, "/", TRUE)
} else if (interactive()) {
  message("Selecione qualquer uma das imagens dentro da pasta 'prancha 2':")
  wdir <- dirname(normalizePath(file.choose(), "/", TRUE))
} else {
  wdir <- normalizePath(getwd(), "/", TRUE)
}
setwd(wdir)

fig_w <- 180
dpi_val <- 600
inst_f <- tryCatch(unique(systemfonts::system_fonts()[["family"]]), error = function(e) character(0))
fcand <- c("Arial", "Helvetica", "Liberation Sans", "Arimo")
ffam <- if (any(fcand %in% inst_f)) fcand[fcand %in% inst_f][1] else "sans"

mm_lw <- function(mm) mm / (ggplot2::.pt * 25.4 / 96)
pt_sz <- function(pt) pt / ggplot2::.pt

ntxt <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  tolower(trimws(x))
}

find_panel_img <- function(type_keys, land_keys, label_prompt) {
  allf <- list.files(wdir, full.names = TRUE)
  imgs <- allf[
    file.exists(allf) &
      tolower(tools::file_ext(allf)) %in% c("png", "jpg", "jpeg", "tif", "tiff") &
      !grepl("^supplementary_figure_s2", tolower(basename(allf)))
  ]
  bnames <- ntxt(tools::file_path_sans_ext(basename(imgs)))
  type_hit <- Reduce(`|`, lapply(type_keys, function(k) grepl(k, bnames)))
  land_hit <- Reduce(`|`, lapply(land_keys, function(k) grepl(k, bnames)))
  hits <- imgs[type_hit & land_hit]
  if (length(hits) == 0 && interactive()) {
    message("Selecione a imagem para: ", label_prompt)
    return(normalizePath(file.choose(), "/", TRUE))
  }
  if (length(hits) == 0) stop("Imagem nao encontrada para: ", label_prompt)
  normalizePath(hits[1], "/", TRUE)
}

read_center_square <- function(path, target_ar = 1.0) {
  ext <- tolower(tools::file_ext(path))
  im <- if (ext == "png") {
    png::readPNG(path)
  } else if (ext %in% c("jpg", "jpeg")) {
    jpeg::readJPEG(path)
  } else {
    tiff::readTIFF(path)
  }
  d <- dim(im)
  h <- d[1]; w <- d[2]
  cur_ar <- h / w
  if (abs(cur_ar - target_ar) < 0.02) return(im)
  if (cur_ar > target_ar) {
    new_h <- floor(w * target_ar)
    r1 <- floor((h - new_h) / 2) + 1
    r2 <- r1 + new_h - 1
    if (length(d) == 3) im[r1:r2, , , drop = FALSE] else im[r1:r2, , drop = FALSE]
  } else {
    new_w <- floor(h / target_ar)
    c1 <- floor((w - new_w) / 2) + 1
    c2 <- c1 + new_w - 1
    if (length(d) == 3) im[, c1:c2, , drop = FALSE] else im[, c1:c2, drop = FALSE]
  }
}

cols_meta <- list(
  list(
    key = "pantanal",
    land_keys = c("pantanal", "baia", "baianegra"),
    title_top = "Pantanal",
    title_bot = "(APA Baía Negra)",
    site = "",
    col = "#4E9B73",
    tag1 = "a1",
    sub1 = "Epiponini",
    tag2 = "a2",
    sub2 = "Suspension trap"
  ),
  list(
    key = "cerrado",
    land_keys = c("deciduous", "sdf", "cerrado", "bodoquena"),
    title_top = "Cerrado",
    title_bot = "(Bodoquena)",
    site = "",
    col = "#C89A42",
    tag1 = "b1",
    sub1 = "Mischocyttarini",
    tag2 = "b2",
    sub2 = "Suspension trap"
  ),
  list(
    key = "floodplain",
    land_keys = c("riparian", "rf", "floodplain", "varzea", "pevri", "ivinhema"),
    title_top = "Floodplain",
    title_bot = "(PEVRI)",
    site = "",
    col = "#6CA0E8",
    tag1 = "c1",
    sub1 = "Polistini",
    tag2 = "c2",
    sub2 = "Suspension trap"
  )
)

for (i in seq_along(cols_meta)) {
  cm <- cols_meta[[i]]
  f_nest <- find_panel_img(
    c("ninho", "vespa", "nest", "wasp", "epiponini", "mischocyttarini", "polistini", "a1", "b1", "c1"),
    cm[["land_keys"]],
    paste(cm[["tag1"]], cm[["title_top"]], "- Ninho/Vespa")
  )
  f_trap <- find_panel_img(
    c("armadilha", "trap", "suspens", "garrafa", "isca", "a2", "b2", "c2"),
    cm[["land_keys"]],
    paste(cm[["tag2"]], cm[["title_top"]], "- Armadilha")
  )
  cols_meta[[i]][["img_top"]] <- read_center_square(f_nest, 1.0)
  cols_meta[[i]][["img_bot"]] <- read_center_square(f_trap, 1.0)
}

margin_x <- 4.0
gap_x <- 3.5
col_w <- (fig_w - 2 * margin_x - 2 * gap_x) / 3
photo_h <- col_w

col_header_h <- 12.5
label_h <- 8.2
row_gap <- 5.5
bot_margin <- 4.0

fig_h <- bot_margin + 2 * (label_h + photo_h) + row_gap + col_header_h 

plt <- ggplot2::ggplot()

y_col_head_top <- fig_h 
y_row1_label_top <- y_col_head_top - col_header_h
y_row1_photo_top <- y_row1_label_top - label_h
y_row1_photo_bot <- y_row1_photo_top - photo_h

y_row2_label_top <- y_row1_photo_bot - row_gap
y_row2_photo_top <- y_row2_label_top - label_h
y_row2_photo_bot <- y_row2_photo_top - photo_h

for (i in seq_along(cols_meta)) {
  cm <- cols_meta[[i]]
  xmin_i <- margin_x + (i - 1) * (col_w + gap_x)
  xmax_i <- xmin_i + col_w
  xmid_i <- (xmin_i + xmax_i) / 2
  
  plt <- plt +
    ggplot2::annotate(
      "text",
      x = xmid_i,
      y = y_col_head_top - 2.8,
      label = cm[["title_top"]],
      family = ffam,
      fontface = "bold",
      size = pt_sz(12.0),
      color = cm[["col"]],
      hjust = 0.5,
      vjust = 1
    ) +
    ggplot2::annotate(
      "text",
      x = xmid_i,
      y = y_col_head_top - 6.8,
      label = cm[["title_bot"]],
      family = ffam,
      size = pt_sz(10.0),
      color = "grey35",
      hjust = 0.5,
      vjust = 1
    ) +
    ggplot2::annotate(
      "segment",
      x = xmin_i, xend = xmax_i,
      y = y_col_head_top - 11.2, yend = y_col_head_top - 11.2,
      color = cm[["col"]],
      linewidth = mm_lw(1.35),
      lineend = "butt"
    ) +
    ggplot2::annotate(
      "rect",
      xmin = xmin_i, xmax = xmin_i + 8.6,
      ymin = y_row1_photo_top + 1.2, ymax = y_row1_photo_top + 7.2,
      fill = cm[["col"]], color = NA
    ) +
    ggplot2::annotate(
      "text",
      x = xmin_i + 4.3, y = y_row1_photo_top + 4.2,
      label = cm[["tag1"]],
      family = ffam, fontface = "bold", size = pt_sz(9.8),
      color = "white", hjust = 0.5, vjust = 0.5
    ) +
    ggplot2::annotate(
      "text",
      x = xmin_i + 10.4, y = y_row1_photo_top + 4.2,
      label = cm[["sub1"]],
      family = ffam, fontface = "bold", size = pt_sz(10.0),
      color = "black", hjust = 0, vjust = 0.5
    ) +
    ggplot2::annotation_custom(
      grid::rasterGrob(cm[["img_top"]], width = grid::unit(1, "npc"), height = grid::unit(1, "npc"), interpolate = TRUE),
      xmin = xmin_i, xmax = xmax_i, ymin = y_row1_photo_bot, ymax = y_row1_photo_top
    ) +
    ggplot2::annotate(
      "rect",
      xmin = xmin_i, xmax = xmax_i, ymin = y_row1_photo_bot, ymax = y_row1_photo_top,
      fill = NA, color = cm[["col"]], linewidth = mm_lw(0.65), linejoin = "mitre"
    ) +
    ggplot2::annotate(
      "rect",
      xmin = xmin_i, xmax = xmin_i + 8.6,
      ymin = y_row2_photo_top + 1.2, ymax = y_row2_photo_top + 7.2,
      fill = cm[["col"]], color = NA
    ) +
    ggplot2::annotate(
      "text",
      x = xmin_i + 4.3, y = y_row2_photo_top + 4.2,
      label = cm[["tag2"]],
      family = ffam, fontface = "bold", size = pt_sz(9.8),
      color = "white", hjust = 0.5, vjust = 0.5
    ) +
    ggplot2::annotate(
      "text",
      x = xmin_i + 10.4, y = y_row2_photo_top + 4.2,
      label = cm[["sub2"]],
      family = ffam, fontface = "bold", size = pt_sz(10.0),
      color = "black", hjust = 0, vjust = 0.5
    ) +
    ggplot2::annotation_custom(
      grid::rasterGrob(cm[["img_bot"]], width = grid::unit(1, "npc"), height = grid::unit(1, "npc"), interpolate = TRUE),
      xmin = xmin_i, xmax = xmax_i, ymin = y_row2_photo_bot, ymax = y_row2_photo_top
    ) +
    ggplot2::annotate(
      "rect",
      xmin = xmin_i, xmax = xmax_i, ymin = y_row2_photo_bot, ymax = y_row2_photo_top,
      fill = NA, color = cm[["col"]], linewidth = mm_lw(0.65), linejoin = "mitre"
    )
}

plt <- plt +
  ggplot2::coord_fixed(ratio = 1, xlim = c(0, fig_w), ylim = c(0, fig_h), expand = FALSE, clip = "off") +
  ggplot2::theme_void(base_family = ffam) +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    plot.margin = ggplot2::margin(0, 0, 0, 0)
  )

if (interactive()) {
  grDevices::dev.new(width = fig_w / 25.4, height = fig_h / 25.4, unit = "in", noRStudioGD = TRUE)
  print(plt)
  grDevices::dev.flush()
}

out_base <- file.path(wdir, "Supplementary_Figure_S2_Tribes_Traps_V2")
ragg::agg_png(paste0(out_base, ".png"), fig_w, fig_h, "mm", res = dpi_val, background = "white")
print(plt)
grDevices::dev.off()

if (capabilities("cairo")) {
  grDevices::cairo_pdf(paste0(out_base, ".pdf"), fig_w / 25.4, fig_h / 25.4, bg = "white", family = ffam)
} else {
  grDevices::pdf(paste0(out_base, ".pdf"), fig_w / 25.4, fig_h / 25.4, bg = "white", useDingbats = FALSE)
}
tryCatch(print(plt), finally = grDevices::dev.off())

cat(sprintf("\nSupplementary Figure S2 saved (%.0f x %.0f mm, %d dpi) in:\n%s\n", fig_w, fig_h, dpi_val, wdir))