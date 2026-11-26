pkgs <- c("ggplot2", "png", "jpeg", "tiff", "grid", "ragg", "systemfonts", "tools")
miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss) > 0) install.packages(miss, dependencies = TRUE)
suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))

bdir <- "C:/Users/João Marcos/Desktop/DOUTORADO/analises/Not all paper dissolves in water social wasps/material_suplementar/material suplementar prancha"
if (dir.exists(bdir)) {
  wdir <- normalizePath(bdir, "/", TRUE)
} else if (interactive()) {
  message("Selecione qualquer uma das imagens (Pantanal, Cerrado ou Floodplain) na sua pasta:")
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

find_img <- function(prefix) {
  allf <- list.files(wdir, full.names = TRUE)
  hits <- allf[
    file.exists(allf) &
      tolower(tools::file_ext(allf)) %in% c("png", "jpg", "jpeg", "tif", "tiff") &
      grepl(prefix, tolower(tools::file_path_sans_ext(basename(allf))))
  ]
  if (length(hits) == 0 && interactive()) {
    message("Selecione a imagem para: ", prefix)
    return(normalizePath(file.choose(), "/", TRUE))
  }
  if (length(hits) == 0) stop("Imagem nao encontrada para: ", prefix)
  normalizePath(hits[1], "/", TRUE)
}

read_and_split <- function(path) {
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
  if (h / w > 0.82) {
    mid <- floor(h / 2)
    r1_end <- max(1, mid - 1)
    r2_start <- min(h, mid + 2)
    p1 <- if (length(d) == 3) im[1:r1_end, , , drop = FALSE] else im[1:r1_end, , drop = FALSE]
    p2 <- if (length(d) == 3) im[r2_start:h, , , drop = FALSE] else im[r2_start:h, , drop = FALSE]
    list(left = p1, right = p2, ar = r1_end / w)
  } else {
    mid_w <- floor(w / 2)
    p1 <- if (length(d) == 3) im[, 1:mid_w, , drop = FALSE] else im[, 1:mid_w, drop = FALSE]
    p2 <- if (length(d) == 3) im[, (mid_w + 1):w, , drop = FALSE] else im[, (mid_w + 1):w, drop = FALSE]
    list(left = p1, right = p2, ar = h / mid_w)
  }
}

im_pant <- read_and_split(find_img("pantanal|baia negra|\\bp\\b"))
im_cerr <- read_and_split(find_img("cerrado|bodoquena"))
im_floo <- read_and_split(find_img("floodplain|varzea|ivinhema|pevri"))

margin_x <- 4.0
gap_x <- 3.2
photo_w <- (fig_w - 2 * margin_x - gap_x) / 2
photo_h <- photo_w * im_pant[["ar"]]

header_h <- 12.2
block_h <- header_h + photo_h
block_gap <- 6.0
bot_margin <- 4.0

fig_h <- bot_margin + 3 * block_h + 2 * block_gap

x_left_min <- margin_x
x_left_max <- margin_x + photo_w
x_right_min <- x_left_max + gap_x
x_right_max <- fig_w - margin_x

specs <- list(
  list(
    tag = "a",
    title = "Área de Proteção Ambiental Baía Negra — Pantanal",
    sub = "Coordinates: 19°01'13.82\"S, 57°34'20.06\"W   ·   5,420.58 ha",
    col = "#4E9B73",
    imgs = im_pant
  ),
  list(
    tag = "b",
    title = "Parque Nacional da Serra da Bodoquena — Cerrado",
    sub = "Coordinates: 20°50'02.41\"S, 56°46'07.03\"W   ·   77,021.58 ha",
    col = "#C89A42",
    imgs = im_cerr
  ),
  list(
    tag = "c",
    title = "Parque Estadual das Várzeas do Rio Ivinhema — Floodplain",
    sub = "Coordinates: 22°55'07\"S, 53°39'11\"W   ·   73,345.15 ha",
    col = "#6CA0E8",
    imgs = im_floo
  )
)

plt <- ggplot2::ggplot() 

cursor_y <- fig_h 

for (k in seq_along(specs)) {
  sp <- specs[[k]]
  head_top <- cursor_y
  photo_top <- head_top - header_h
  photo_bot <- photo_top - photo_h
  
  plt <- plt +
    ggplot2::annotate(
      "rect",
      xmin = x_left_min,
      xmax = x_left_min + 8.0,
      ymin = photo_top + 1.8,
      ymax = photo_top + 9.8,
      fill = sp[["col"]],
      color = NA
    ) +
    ggplot2::annotate(
      "text",
      x = x_left_min + 4.0,
      y = photo_top + 5.8,
      label = sp[["tag"]],
      family = ffam,
      fontface = "bold",
      size = pt_sz(12.0),
      color = "white",
      hjust = 0.5,
      vjust = 0.5
    ) +
    ggplot2::annotate(
      "text",
      x = x_left_min + 10.2,
      y = photo_top + 6.0,
      label = sp[["title"]],
      family = ffam,
      fontface = "bold",
      size = pt_sz(11.2),
      color = "black",
      hjust = 0,
      vjust = 0
    ) +
    ggplot2::annotate(
      "text",
      x = x_left_min + 10.2,
      y = photo_top + 1.8,
      label = sp[["sub"]],
      family = ffam,
      size = pt_sz(9.5),
      color = "grey25",
      hjust = 0,
      vjust = 0
    ) +
    ggplot2::annotation_custom(
      grid::rasterGrob(sp[["imgs"]][["left"]], width = grid::unit(1, "npc"), height = grid::unit(1, "npc"), interpolate = TRUE),
      xmin = x_left_min, xmax = x_left_max, ymin = photo_bot, ymax = photo_top
    ) +
    ggplot2::annotation_custom(
      grid::rasterGrob(sp[["imgs"]][["right"]], width = grid::unit(1, "npc"), height = grid::unit(1, "npc"), interpolate = TRUE),
      xmin = x_right_min, xmax = x_right_max, ymin = photo_bot, ymax = photo_top
    ) +
    ggplot2::annotate(
      "rect",
      xmin = x_left_min, xmax = x_left_max, ymin = photo_bot, ymax = photo_top,
      fill = NA, color = sp[["col"]], linewidth = mm_lw(0.65), linejoin = "mitre"
    ) +
    ggplot2::annotate(
      "rect",
      xmin = x_right_min, xmax = x_right_max, ymin = photo_bot, ymax = photo_top,
      fill = NA, color = sp[["col"]], linewidth = mm_lw(0.65), linejoin = "mitre"
    )
  
  if (k < length(specs)) {
    div_y <- photo_bot - block_gap / 2
    plt <- plt +
      ggplot2::annotate(
        "segment",
        x = x_left_min, xend = x_right_max, y = div_y, yend = div_y,
        color = "grey82", linewidth = mm_lw(0.25)
      )
  }
  
  cursor_y <- photo_bot - block_gap
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

out_base <- file.path(wdir, "Supplementary_Figure_S1_Landscapes")
ragg::agg_png(paste0(out_base, ".png"), fig_w, fig_h, "mm", res = dpi_val, background = "white")
print(plt)
grDevices::dev.off()

if (capabilities("cairo")) {
  grDevices::cairo_pdf(paste0(out_base, ".pdf"), fig_w / 25.4, fig_h / 25.4, bg = "white", family = ffam)
} else {
  grDevices::pdf(paste0(out_base, ".pdf"), fig_w / 25.4, fig_h / 25.4, bg = "white", useDingbats = FALSE)
}
tryCatch(print(plt), finally = grDevices::dev.off())

cat(sprintf("\nSupplementary Figure S1 saved (%.0f x %.0f mm, %d dpi) in:\n%s\n", fig_w, fig_h, dpi_val, wdir))