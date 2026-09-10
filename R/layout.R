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

#' Force-directed label placement.
#'
#' @param ax,ay anchor (car) positions in data units
#' @param labels character vector
#' @param xlim,ylim panel limits in data units
#' @param dev_in device size in inches, c(width, height)
#' @param car_pt car glyph width in points (matches geom_car)
#' @param font_pt label font size in points
#' @return data.frame(lx, ly) label positions in data units
repel_labels <- function(ax, ay, labels, xlim, ylim, dev_in = c(11, 9),
                         car_pt = 26, aspect = 2.4, font_pt = 7.5, iter = 1400) {
  n <- length(ax)
  if (!n) return(data.frame(lx = numeric(0), ly = numeric(0)))
  ppt <- panel_pt(dev_in)

  # data -> npc
  nx <- (ax - xlim[1]) / diff(xlim)
  ny <- (ay - ylim[1]) / diff(ylim)

  gw <- car_pt / ppt[1]                       # glyph half-extents in npc
  gh <- (car_pt / aspect) / ppt[2]
  lw <- (nchar(labels) * font_pt * 0.62 + 4) / ppt[1]
  lh <- (font_pt * 1.65) / ppt[2]

  # Seed each label in the direction that points AWAY from its neighbours.
  # Starting every label directly underneath its car makes dense clusters settle
  # into a vertical stack the repel then struggles to break apart.
  dirx <- numeric(n); diry <- numeric(n)
  for (i in seq_len(n)) {
    dx <- nx[i] - nx; dy <- ny[i] - ny
    d2 <- dx^2 + dy^2; d2[i] <- Inf
    near <- order(d2)[seq_len(min(5, n - 1))]
    if (!length(near) || !is.finite(d2[near[1]])) { dirx[i] <- 0; diry[i] <- -1; next }
    w  <- 1 / (sqrt(d2[near]) + 1e-6)
    vx <- sum(dx[near] * w); vy <- sum(dy[near] * w)
    L  <- sqrt(vx^2 + vy^2)
    if (L < 1e-9) { dirx[i] <- 0; diry[i] <- -1 } else { dirx[i] <- vx / L; diry[i] <- vy / L }
  }
  off <- gh * 0.60 + lh * 0.70
  lx <- nx + dirx * off * 1.6
  ly <- ny + diry * off

  for (it in seq_len(iter)) {
    fx <- numeric(n); fy <- numeric(n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {                 # label vs label
        if (i == j) next
        dx <- lx[j] - lx[i]; dy <- ly[j] - ly[i]
        ox <- (lw[i] + lw[j]) / 2 - abs(dx); oy <- lh - abs(dy)
        if (ox > 0 && oy > 0) {
          if (oy < ox) { s <- if (dy >= 0) 1 else -1; fy[i] <- fy[i] - s * oy * 0.50 }
          else         { s <- if (dx >= 0) 1 else -1; fx[i] <- fx[i] - s * ox * 0.50 }
        }
      }
      for (k in seq_len(n)) {                 # label vs car glyph
        dx <- nx[k] - lx[i]; dy <- ny[k] - ly[i]
        ox <- (lw[i] + gw) / 2 - abs(dx); oy <- (lh + gh) / 2 - abs(dy)
        if (ox > 0 && oy > 0) {
          if (oy < ox) { s <- if (dy >= 0) 1 else -1; fy[i] <- fy[i] - s * oy * 0.80 }
          else         { s <- if (dx >= 0) 1 else -1; fx[i] <- fx[i] - s * ox * 0.80 }
        }
      }
    }
    fx <- fx + (nx - lx) * 0.004              # weak spring back to the car
    fy <- fy + (ny - ly) * 0.004
    if (max(abs(c(fx, fy))) < 1e-5) break
    lx <- pmin(pmax(lx + fx * 0.55, lw / 2),        1 - lw / 2)
    ly <- pmin(pmax(ly + fy * 0.55, lh * 0.60), 1 - lh * 0.60)
  }

  data.frame(lx = lx * diff(xlim) + xlim[1],
             ly = ly * diff(ylim) + ylim[1])
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
