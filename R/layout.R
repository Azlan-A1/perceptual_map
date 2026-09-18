# ---------------------------------------------------------------------------
# Geometry helpers: axis limits, and label placement that avoids both other
# labels and the car glyphs.
#
# GeomCar sizes cars in POINTS, but positions live in DATA units, so the repel
# works in normalised panel space (0..1) where the two are commensurable, then
# converts back. Device size is therefore an input, not a guess.
# ---------------------------------------------------------------------------

#' Axis limits from the data, padded. Deliberately NOT forced symmetric --
#' a symmetric range on asymmetric data wastes half the panel.
map_limits <- function(v, pad = 0.10, include_zero = TRUE) {
  r <- range(v, na.rm = TRUE)
  if (include_zero) r <- range(c(r, 0))
  d <- diff(r)
  if (d == 0) d <- 1
  c(r[1] - d * pad, r[2] + d * pad)
}

#' Panel size in points, approximating the fraction of the device the panel uses.
panel_pt <- function(dev_in, panel_frac = c(0.84, 0.82)) {
  c(dev_in[1] * 72 * panel_frac[1], dev_in[2] * 72 * panel_frac[2])
}

#' Label placement: a greedy search over candidate spots.
#'
#' Works in points on the panel, where car glyphs (sized in points) and text
#' (measured in points) are commensurable. Each label tries rings of candidate
#' positions around its car, nearest ring first; within a ring, directions
#' pointing away from neighbouring cars come first. It takes the first spot that
#' overlaps no placed label, no car glyph and no obstacle and stays on the
#' panel.
#'
#' The most distinctive cars -- furthest from the centre -- choose first. On a
#' perceptual map distance from the centre IS distinctiveness, so those are the
#' names most worth keeping when space runs out; the crowd near the middle is
#' the average car, and zooming in separates it anyway. Measured across 16
#' small-window layouts this kept the outliers (MX-5, Suburban, Odyssey) that a
#' crowded-first order was losing, while hiding no labels at 900px and up.
#'
#' This replaced a force-directed repel that did not converge in dense
#' clusters: measured on the rendered plot it left 20-50 overlaps on small
#' windows, because pushed labels bounced between neighbours instead of
#' finding the gap. A search finds a clear spot whenever one exists in reach.
#'
#' A label with no clear spot within `max_pt` of its car is left OFF rather
#' than parked across the map: a name 150pt from its car, joined by a leader
#' crossing half the cluster, reads worse than no name. `placed` reports which,
#' so the caller can say how many were hidden.
#'
#' @param ax,ay anchor (car) positions in data units
#' @param labels character vector
#' @param xlim,ylim limits of the region labels must stay inside, data units
#' @param dev_in device size in inches; only used for the default `ppt`
#' @param car_pt car glyph width in points (matches geom_car)
#' @param font_pt label font size in points
#' @param ppt size of the xlim/ylim region in points. Pass the MEASURED size.
#' @param label_w measured label widths in points; estimated from nchar if NULL
#' @param obstacles data.frame(xmin, xmax, ymin, ymax), data units, to keep off
#' @param max_pt how far past touching its car a label may travel; by default
#'   a tenth of the panel diagonal, so a big window keeps every label
#' @return data.frame(lx, ly, placed) label centres in data units, input order
repel_labels <- function(ax, ay, labels, xlim, ylim, dev_in = c(11, 9),
                         car_pt = 26, aspect = 2.4, font_pt = 7.5,
                         ppt = panel_pt(dev_in), label_w = NULL, obstacles = NULL,
                         max_pt = NULL, step_pt = 3) {
  n <- length(ax)
  if (!n) return(data.frame(lx = numeric(0), ly = numeric(0), placed = logical(0)))
  if (is.null(max_pt)) max_pt <- max(24, 0.10 * sqrt(sum(ppt^2)))
  sx <- ppt[1] / diff(xlim); sy <- ppt[2] / diff(ylim)
  cx <- (ax - xlim[1]) * sx; cy <- (ay - ylim[1]) * sy                # cars, in points

  # Label boxes. Measured widths come from the PDF font metrics, which run
  # ~4% narrower than the screen renderer's, hence the 1.05.
  lw <- (if (is.null(label_w)) nchar(labels) * font_pt * 0.62 else label_w * 1.05) + 6
  lh <- font_pt * 1.55
  gw <- car_pt + 2; gh <- car_pt / aspect + 2

  # Everything fixed: the car glyphs, then any obstacles.
  fx0 <- cx - gw / 2; fx1 <- cx + gw / 2; fy0 <- cy - gh / 2; fy1 <- cy + gh / 2
  if (!is.null(obstacles) && nrow(obstacles)) {
    fx0 <- c(fx0, (obstacles$xmin - xlim[1]) * sx); fx1 <- c(fx1, (obstacles$xmax - xlim[1]) * sx)
    fy0 <- c(fy0, (obstacles$ymin - ylim[1]) * sy); fy1 <- c(fy1, (obstacles$ymax - ylim[1]) * sy)
  }

  # Preferred direction for each label: away from its nearest neighbours.
  away <- vapply(seq_len(n), function(i) {
    dx <- cx[i] - cx; dy <- cy[i] - cy; d2 <- dx^2 + dy^2; d2[i] <- Inf
    near <- order(d2)[seq_len(min(5, n - 1))]
    if (!length(near) || !is.finite(d2[near[1]])) return(-pi / 2)
    w <- 1 / (sqrt(d2[near]) + 1e-6)
    vx <- sum(dx[near] * w); vy <- sum(dy[near] * w)
    if (vx^2 + vy^2 < 1e-12) -pi / 2 else atan2(vy, vx)
  }, 0)
  turn <- c(0, as.vector(rbind(seq(15, 180, 15), -seq(15, 180, 15))))[1:24] * pi / 180

  # Tie-break: how many glyphs are close enough to compete for the same space.
  crowd <- vapply(seq_len(n), function(i)
    sum(abs(cx - cx[i]) < (gw + lw[i]) & abs(cy - cy[i]) < 3 * (gh + lh)) - 1, 0)
  ox <- (0 - xlim[1]) * sx; oy <- (0 - ylim[1]) * sy               # the average car
  far <- (cx - ox)^2 + (cy - oy)^2
  ord <- order(-far, crowd)

  px <- rep(NA_real_, n); py <- rep(NA_real_, n)
  for (i in ord) {
    th <- away[i] + turn
    placed <- which(!is.na(px))
    rx0 <- c(fx0, px[placed] - lw[placed] / 2); rx1 <- c(fx1, px[placed] + lw[placed] / 2)
    ry0 <- c(fy0, py[placed] - lh / 2);         ry1 <- c(fy1, py[placed] + lh / 2)
    best <- NULL
    for (d in seq(0, max_pt, by = step_pt)) {
      # Candidate centres on a rectangle around the glyph, so the d = 0 ring
      # touches the car without covering it, whatever the direction.
      A <- (gw + lw[i]) / 2 + d; B <- (gh + lh) / 2 + d
      t <- pmin(A / abs(cos(th)), B / abs(sin(th)))
      qx <- cx[i] + t * cos(th); qy <- cy[i] + t * sin(th)
      x0 <- qx - lw[i] / 2; x1 <- qx + lw[i] / 2; y0 <- qy - lh / 2; y1 <- qy + lh / 2
      inb <- x0 >= 0 & x1 <= ppt[1] & y0 >= 0 & y1 <= ppt[2]
      # pmax keeps the FIRST argument's attributes, so the matrix goes first.
      ovw <- pmax(outer(x1, rx1, pmin) - outer(x0, rx0, pmax), 0)
      ovh <- pmax(outer(y1, ry1, pmin) - outer(y0, ry0, pmax), 0)
      cost <- rowSums(ovw * ovh)
      ok <- which(inb & cost == 0)
      if (length(ok)) { best <- ok[1]; px[i] <- qx[best]; py[i] <- qy[best]; break }
    }
  }

  data.frame(lx = px / sx + xlim[1], ly = py / sy + ylim[1], placed = !is.na(px))
}

#' Measure the layout instead of guessing it.
#'
#' Returns the space, in points, that everything OUTSIDE the panel takes up
#' (titles, axes, legend, key), read from the real gtable. coord_fixed() makes
#' the panel a "null" unit, so the other rows and columns are absolute and can
#' be summed. Also measures text widths for each entry of `texts`
#' (list(label, fontsize, fontface)). Earlier this was guessed as a fixed
#' fraction of the device, which put the panel ~16% taller than it really was
#' and let labels overlap on small screens.
#'
#' Uses a throwaway null PDF device and restores the current one, so it is
#' safe to call while Shiny is mid-render.
measure_layout <- function(skel, dev_in, texts = list()) {
  old <- grDevices::dev.cur()
  grDevices::pdf(NULL, width = dev_in[1], height = dev_in[2])
  on.exit({ grDevices::dev.off(); if (old > 1) grDevices::dev.set(old) }, add = TRUE)
  g  <- ggplot2::ggplotGrob(skel)
  np <- c(sum(grid::convertWidth(g$widths, "pt", valueOnly = TRUE)),
          sum(grid::convertHeight(g$heights, "pt", valueOnly = TRUE)))
  width_of <- function(s, gp) max(vapply(strsplit(s, "\n", fixed = TRUE)[[1]], function(l)
    grid::convertWidth(grid::grobWidth(grid::textGrob(l, gp = gp)), "pt", valueOnly = TRUE), 0))
  w <- lapply(texts, function(t) {
    gp <- grid::gpar(fontsize = t$fontsize, fontface = t$fontface)
    stats::setNames(vapply(t$label, width_of, 0, gp = gp), names(t$label))
  })
  list(nonpanel = np, widths = w)
}

#' Place the quadrant tags in the inner corners, from measured widths.
#'
#' Two tags sharing a row wrap onto two lines ("LOW WEIGHT /" over "HIGH
#' QUICKNESS") when they would not fit side by side. A tag that would sit on a
#' car glyph is dropped: it is only an orientation aid, and the car is the data.
#' Returns the text placement plus each tag's rectangle in data units, which
#' build_map uses to drop any tag that would cover a car label.
quad_tags <- function(quads, ixl, iyl, inner_pt, w1, w2, font_pt,
                      drop_quad = NULL, cars = NULL, car_pt = 26, aspect = 2.4) {
  keys <- setdiff(c("tl", "tr", "bl", "br"), drop_quad)
  if (!length(keys)) return(NULL)
  dpx <- diff(ixl) / inner_pt[1]; dpy <- diff(iyl) / inner_pt[2]   # data units per point
  gap <- 4
  wrap <- vapply(c(top = "t", bottom = "b"), function(r) {
    ks <- keys[substr(keys, 1, 1) == r]
    length(ks) > 0 && sum(w1[ks]) + 16 > inner_pt[1] - 2 * gap
  }, TRUE)
  out <- do.call(rbind, lapply(keys, function(k) {
    top <- substr(k, 1, 1) == "t"; right <- substr(k, 2, 2) == "r"
    wr  <- wrap[[if (top) "top" else "bottom"]]
    w   <- if (wr) w2[[k]] else w1[[k]]
    h   <- font_pt * (if (wr) 2.15 else 1.1)
    x   <- if (right) ixl[2] - gap * dpx else ixl[1] + gap * dpx
    y   <- if (top)   iyl[2] - gap * dpy else iyl[1] + gap * dpy
    data.frame(key = k, x = x, y = y, hjust = as.numeric(right), vjust = as.numeric(top),
               label = if (wr) sub(" / ", " /\n", quads[[k]], fixed = TRUE) else quads[[k]],
               xmin = if (right) x - w * dpx else x, xmax = if (right) x else x + w * dpx,
               ymin = if (top) y - h * dpy else y,   ymax = if (top) y else y + h * dpy)
  }))
  if (!is.null(cars) && nrow(cars)) {
    gw <- car_pt / 2 * dpx; gh <- car_pt / aspect / 2 * dpy
    on_car <- vapply(seq_len(nrow(out)), function(i) any(
      cars$x + gw > out$xmin[i] & cars$x - gw < out$xmax[i] &
      cars$y + gh > out$ymin[i] & cars$y - gh < out$ymax[i]), TRUE)
    out <- out[!on_car, , drop = FALSE]
  }
  out
}

#' Widen limits by a band of `band_pt` points on each side.
#' Widening a range makes every point worth more data units, so simply adding
#' band_pt's current worth comes out short. Solved for the width AFTER widening.
add_band <- function(lim, band_pt, panel_pt) {
  f <- band_pt / panel_pt
  if (!is.finite(f) || f <= 0 || f >= 0.25) return(lim)
  b <- f * diff(lim) / (1 - 2 * f)
  lim + c(-b, b)
}

#' Estimated rendered width of a string in points (same heuristic as repel_labels).
text_pt <- function(s, font_pt) nchar(s) * font_pt * 0.60 + 6

#' The longest leading run of `words` that fits in `avail_pt`, joined by a
#' middle dot. Words arrive strongest-first, so it is the weakest that get
#' dropped on a narrow screen. NULL if not even the first word fits.
fit_words <- function(words, avail_pt, font_pt, sep = " \u00b7 ") {
  for (k in rev(seq_along(words))) {
    s <- paste(words[seq_len(k)], collapse = sep)
    if (text_pt(s, font_pt) <= avail_pt) return(s)
  }
  NULL
}

#' Centre a label on `centre` (the crosshair), sliding it along the edge just
#' far enough to stay inside [lo, hi]. Falls back to the middle of the span when
#' the crosshair is out of view, e.g. after zooming into one corner.
slide_into <- function(centre, half, lo, hi) {
  if (!is.finite(centre) || centre < lo || centre > hi) centre <- (lo + hi) / 2
  min(max(centre, lo + half), hi - half)
}

#' Convex hull rows per group, for optional segment shading.
hull_rows <- function(df, x, y, group) {
  do.call(rbind, lapply(split(df, df[[group]]), function(g) {
    if (nrow(g) < 3) return(NULL)
    g[grDevices::chull(g[[x]], g[[y]]), , drop = FALSE]
  }))
}

#' Which corner of the panel holds the fewest cars?
#' Used to park the compass inset where it cannot cover data.
best_inset_corner <- function(x, y, xlim, ylim, w = 0.36, h = 0.30) {
  nx <- (x - xlim[1]) / diff(xlim)
  ny <- (y - ylim[1]) / diff(ylim)
  cnt <- c(
    tr = sum(nx > 1 - w & ny > 1 - h),
    tl = sum(nx <     w & ny > 1 - h),
    br = sum(nx > 1 - w & ny <     h),
    bl = sum(nx <     w & ny <     h)
  )
  names(cnt)[which.min(cnt)]   # ties resolve tr > tl > br > bl
}

#' patchwork::inset_element bounds for a corner tag.
inset_bounds <- function(corner, w = 0.34, h = 0.265, m = 0.012) {
  switch(corner,
    tr = c(left = 1 - w - m, bottom = 1 - h - m, right = 1 - m, top = 1 - m),
    tl = c(left = m,         bottom = 1 - h - m, right = w + m, top = 1 - m),
    br = c(left = 1 - w - m, bottom = m,         right = 1 - m, top = h + m),
    bl = c(left = m,         bottom = m,         right = w + m, top = h + m))
}

#' Expand the shorter axis so the data aspect matches the panel aspect.
#'
#' coord_fixed() guarantees 1 x-unit == 1 y-unit, but when the data aspect and
#' the panel aspect disagree it letterboxes, leaving most of the panel empty.
#' Padding the shorter axis first means coord_fixed has nothing left to
#' letterbox, so the map fills the space AND the units stay honest.
fit_aspect <- function(xlim, ylim, dev_in, panel_frac = c(0.84, 0.70)) {
  pw <- dev_in[1] * panel_frac[1]
  ph <- dev_in[2] * panel_frac[2]
  if (!is.finite(pw) || !is.finite(ph) || pw <= 0 || ph <= 0) return(list(x = xlim, y = ylim))
  target <- pw / ph
  cur    <- diff(xlim) / diff(ylim)
  if (!is.finite(target) || !is.finite(cur) || cur <= 0) return(list(x = xlim, y = ylim))
  if (cur < target) {
    w  <- diff(ylim) * target; cx <- mean(xlim)
    xlim <- c(cx - w / 2, cx + w / 2)
  } else {
    h  <- diff(xlim) / target; cy <- mean(ylim)
    ylim <- c(cy - h / 2, cy + h / 2)
  }
  list(x = xlim, y = ylim)
}
