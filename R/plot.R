# ---------------------------------------------------------------------------
# Plot construction: the map, the loadings compass, and the two composed.
# ---------------------------------------------------------------------------

pm_theme <- function(dark = FALSE, base_size = 11) {
  bg   <- if (dark) "#16181D" else "#FFFFFF"
  fg   <- if (dark) "#E6E8EC" else "grey12"
  mid  <- if (dark) "#9AA0AA" else "grey35"
  grid <- if (dark) "#272A31" else "grey93"
  brd  <- if (dark) "#3A3E47" else "grey75"
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = grid, linewidth = .25),
      panel.border     = ggplot2::element_rect(colour = brd, fill = NA, linewidth = .4),
      plot.title       = ggplot2::element_text(face = "bold", size = base_size * 1.4, colour = fg),
      plot.subtitle    = ggplot2::element_text(colour = mid, size = base_size * 0.88),
      plot.caption     = ggplot2::element_text(colour = mid, size = base_size * 0.70),
      axis.title       = ggplot2::element_text(colour = mid, size = base_size * 0.88),
      axis.text        = ggplot2::element_text(colour = mid, size = base_size * 0.78),
      legend.title     = ggplot2::element_text(colour = fg, size = base_size * 0.82),
      legend.text      = ggplot2::element_text(colour = mid, size = base_size * 0.78),
      legend.key.width = grid::unit(22, "pt"),
      legend.position  = "bottom",
      plot.background  = ggplot2::element_rect(fill = bg, colour = NA),
      panel.background = ggplot2::element_rect(fill = bg, colour = NA)
    )
}

QUAD_FILL <- c(tr = "#C44E52", tl = "#4C72B0", bl = "#55A868", br = "#CCB974")

#' Default quadrant tags, derived from whichever attribute dominates each axis.
auto_quads <- function(loadings, xpc, ypc) {
  xt <- loadings$var[which.max(abs(loadings[[xpc]]))]
  yt <- loadings$var[which.max(abs(loadings[[ypc]]))]
  c(tr = sprintf("HIGH %s / HIGH %s", toupper(xt), toupper(yt)),
    tl = sprintf("LOW %s / HIGH %s",  toupper(xt), toupper(yt)),
    bl = sprintf("LOW %s / LOW %s",   toupper(xt), toupper(yt)),
    br = sprintf("HIGH %s / LOW %s",  toupper(xt), toupper(yt)))
}

#' The main perceptual map.
build_map <- function(m, xpc = "PC1", ypc = "PC2", car_pt = 26,
                      show_labels = TRUE, show_quadrants = TRUE, show_hulls = FALSE,
                      bodies = NULL, dev_in = c(11, 9), xlim = NULL, ylim = NULL,
                      dark = FALSE, quads = NULL, title = "Perceptual Map of the Car Market",
                      subtitle = NULL, drop_quad = NULL) {

  d <- m$cars
  if (!is.null(bodies)) d <- d[d$body %in% bodies, , drop = FALSE]
  if (!nrow(d)) return(ggplot2::ggplot() + pm_theme(dark) +
                       ggplot2::labs(title = "No cars match the current filter"))

  d$.x <- d[[xpc]]; d$.y <- d[[ypc]]
  if (is.null(xlim)) xlim <- map_limits(d$.x)
  if (is.null(ylim)) ylim <- map_limits(d$.y)
  fa <- fit_aspect(xlim, ylim, dev_in); xlim <- fa$x; ylim <- fa$y

  fg  <- if (dark) "#E6E8EC" else "grey12"
  mid <- if (dark) "#9AA0AA" else "grey55"
  qa  <- if (dark) 0.10 else 0.05
  if (is.null(quads)) quads <- auto_quads(m$loadings, xpc, ypc)

  p <- ggplot2::ggplot()

  if (show_quadrants) {
    p <- p +
      ggplot2::annotate("rect", xmin = 0, xmax = xlim[2], ymin = 0, ymax = ylim[2],
                        fill = QUAD_FILL[["tr"]], alpha = qa) +
      ggplot2::annotate("rect", xmin = xlim[1], xmax = 0, ymin = 0, ymax = ylim[2],
                        fill = QUAD_FILL[["tl"]], alpha = qa) +
      ggplot2::annotate("rect", xmin = xlim[1], xmax = 0, ymin = ylim[1], ymax = 0,
                        fill = QUAD_FILL[["bl"]], alpha = qa) +
      ggplot2::annotate("rect", xmin = 0, xmax = xlim[2], ymin = ylim[1], ymax = 0,
                        fill = QUAD_FILL[["br"]], alpha = qa + .01)
  }

  p <- p +
    ggplot2::geom_hline(yintercept = 0, colour = mid, linewidth = .35) +
    ggplot2::geom_vline(xintercept = 0, colour = mid, linewidth = .35)

  if (show_hulls) {
    h <- hull_rows(d, ".x", ".y", "body")
    if (!is.null(h) && nrow(h))
      p <- p + ggplot2::geom_polygon(data = h, ggplot2::aes(.x, .y, fill = body, group = body),
                                     alpha = .10, colour = NA, show.legend = FALSE)
  }

  if (show_labels) {
    L <- repel_labels(d$.x, d$.y, d$model, xlim, ylim, dev_in = dev_in, car_pt = car_pt)
    d$lx <- L$lx; d$ly <- L$ly
    p <- p + ggplot2::geom_segment(data = d, ggplot2::aes(.x, .y, xend = lx, yend = ly),
                                   colour = mid, linewidth = .2)
  }

  p <- p +
    geom_car(data = d, ggplot2::aes(.x, .y, body = body, fill = body), car_pt = car_pt) +
    ggplot2::geom_point(data = d, ggplot2::aes(.x, .y), size = .4, colour = mid)

  if (show_labels)
    p <- p + ggplot2::geom_label(data = d, ggplot2::aes(lx, ly, label = model),
                                 size = 2.6, colour = fg, linewidth = 0,
                                 label.padding = grid::unit(1, "pt"),
                                 fill = scales::alpha(if (dark) "#16181D" else "#FFFFFF", .80))

  if (show_quadrants) {
    qs <- list(c(xlim[2], ylim[2], 1, 1.6, "tr"), c(xlim[1], ylim[2], 0, 1.6, "tl"),
               c(xlim[1], ylim[1], 0, -1.0, "bl"), c(xlim[2], ylim[1], 1, -1.0, "br"))
    for (q in qs) {
      if (!is.null(drop_quad) && q[[5]] == drop_quad) next  # the compass inset lives here
      p <- p + ggplot2::annotate("text", x = as.numeric(q[[1]]), y = as.numeric(q[[2]]),
                                 label = quads[[q[[5]]]], hjust = as.numeric(q[[3]]),
                                 vjust = as.numeric(q[[4]]), size = 2.8, fontface = "bold",
                                 colour = QUAD_FILL[[q[[5]]]], alpha = .75)
    }
  }

  vx <- m$ve[as.integer(sub("PC", "", xpc))]
  vy <- m$ve[as.integer(sub("PC", "", ypc))]
  if (is.null(subtitle))
    subtitle <- sprintf("PCA on %d attributes — %d models, %.1f%% of variance on these two axes",
                        length(m$vars), nrow(d), vx + vy)

  p +
    ggplot2::scale_fill_manual(values = pal, name = "Body type",
                               limits = intersect(BODY_LEVELS, unique(d$body))) +
    ggplot2::coord_fixed(ratio = 1, xlim = xlim, ylim = ylim, expand = FALSE) +
    ggplot2::labs(
      title = title, subtitle = subtitle,
      x = sprintf("%s (%.1f%%)  —  %s →", xpc, vx, axis_name(m$loadings, xpc)),
      y = sprintf("%s (%.1f%%)  —  %s →", ypc, vy, axis_name(m$loadings, ypc)),
      caption = "Silhouettes are drawn in R. Specs are approximate, curated for illustration."
    ) +
    pm_theme(dark)
}

#' Loadings compass: a correlation circle, kept OFF the data area.
build_compass <- function(m, xpc = "PC1", ypc = "PC2", dark = FALSE) {
  fg  <- if (dark) "#E6E8EC" else "grey20"
  mid <- if (dark) "#6B7280" else "grey85"
  bg  <- if (dark) "#16181D" else "#FFFFFF"
  L <- m$loadings
  L$x <- L[[xpc]]; L$y <- L[[ypc]]
  th <- seq(0, 2 * pi, length.out = 200)

  # Arrows that point almost the same way would stack their labels, so fan the
  # LABELS apart in angle. The arrows themselves stay exactly where they belong.
  ang <- atan2(L$y, L$x); rad <- sqrt(L$x^2 + L$y^2)
  o <- order(ang); a <- ang[o]
  min_gap <- 0.30
  if (length(a) > 1) {
    for (pass in 1:60) {
      moved <- FALSE
      for (i in seq_len(length(a) - 1)) {
        g <- a[i + 1] - a[i]
        if (g < min_gap) {
          sh <- (min_gap - g) / 2
          a[i] <- a[i] - sh; a[i + 1] <- a[i + 1] + sh; moved <- TRUE
        }
      }
      if (!moved) break
    }
  }
  ang[o] <- a
  lr <- pmax(rad, 0.34) + 0.16
  L$tx <- cos(ang) * lr
  L$ty <- sin(ang) * lr

  ggplot2::ggplot(L) +
    ggplot2::annotate("path", x = cos(th), y = sin(th), colour = mid, linewidth = .3) +
    ggplot2::geom_hline(yintercept = 0, colour = mid, linewidth = .25) +
    ggplot2::geom_vline(xintercept = 0, colour = mid, linewidth = .25) +
    ggplot2::geom_segment(ggplot2::aes(0, 0, xend = x, yend = y),
                          arrow = grid::arrow(length = grid::unit(3.5, "pt"), type = "closed"),
                          colour = if (dark) "#9AA0AA" else "grey35", linewidth = .35) +
    ggplot2::geom_text(ggplot2::aes(tx, ty, label = var, hjust = ifelse(tx >= 0, 0, 1)),
                       size = 2.3, colour = fg, fontface = "bold") +
    ggplot2::coord_fixed(ratio = 1, xlim = c(-1.9, 1.9), ylim = c(-1.25, 1.25), expand = FALSE) +
    ggplot2::labs(title = "What drives the axes") +
    ggplot2::theme_void(base_size = 9) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(size = 8, face = "bold", colour = fg,
                                               hjust = .5, margin = ggplot2::margin(b = 2)),
      panel.border     = ggplot2::element_rect(colour = if (dark) "#3A3E47" else "grey80",
                                               fill = NA, linewidth = .4),
      plot.background  = ggplot2::element_rect(fill = bg, colour = NA),
      plot.margin      = ggplot2::margin(3, 3, 3, 3)
    )
}

#' Map with the compass tucked into the bottom-right corner.
compose_map <- function(m, xpc = "PC1", ypc = "PC2", ..., bodies = NULL,
                        xlim = NULL, ylim = NULL, show_compass = TRUE, dark = FALSE) {
  d <- m$cars
  if (!is.null(bodies)) d <- d[d$body %in% bodies, , drop = FALSE]
  if (!nrow(d)) return(build_map(m, xpc = xpc, ypc = ypc, ..., bodies = bodies, dark = dark))
  if (is.null(xlim)) xlim <- map_limits(d[[xpc]])
  if (is.null(ylim)) ylim <- map_limits(d[[ypc]])
  dev <- list(...)$dev_in; if (is.null(dev)) dev <- c(11, 9)
  fa <- fit_aspect(xlim, ylim, dev); xlim <- fa$x; ylim <- fa$y

  # Park the compass where it cannot cover a car.
  corner <- if (show_compass) best_inset_corner(d[[xpc]], d[[ypc]], xlim, ylim) else NULL

  main <- build_map(m, xpc = xpc, ypc = ypc, ..., bodies = bodies,
                    xlim = xlim, ylim = ylim, dark = dark, drop_quad = corner)
  if (is.null(corner)) return(main)

  b <- inset_bounds(corner)
  main + patchwork::inset_element(build_compass(m, xpc = xpc, ypc = ypc, dark = dark),
                                  left = b[["left"]], bottom = b[["bottom"]],
                                  right = b[["right"]], top = b[["top"]])
}
