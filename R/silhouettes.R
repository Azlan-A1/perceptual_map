# ---------------------------------------------------------------------------
# Car silhouettes drawn as grid polygons, and the ggplot2 Geom that places them.
#
# Why a custom Geom instead of annotation_custom():
#   * annotation_custom() is NOT vectorised. Passing vectors to xmin/xmax raises
#     "Aesthetics must be either length 1 or the same as the data (1)", so it
#     needs one layer per car -- 28 layers, and it replicates into every facet.
#   * More importantly, annotation_custom sizes the car in DATA units, so the
#     cars grow as you zoom in and a dense cluster never decongests. GeomCar
#     sizes in POINTS, so zooming genuinely spreads the cars apart.
# ---------------------------------------------------------------------------

# Each shape is drawn in a 0..1 x 0..1 box that is rendered at aspect 2.4:1.
shapes <- list(
  sedan  = list(body = list(x = c(.02,.03,.10,.28,.42,.62,.80,.92,.98,.98,.02),
                            y = c(.30,.46,.50,.52,.74,.76,.54,.50,.44,.30,.30)),
                win  = list(x = c(.32,.44,.60,.72), y = c(.51,.71,.72,.53))),
  hatch  = list(body = list(x = c(.02,.03,.12,.30,.44,.70,.86,.90,.90,.02),
                            y = c(.30,.46,.50,.52,.76,.78,.70,.52,.30,.30)),
                win  = list(x = c(.34,.46,.66,.80), y = c(.51,.73,.74,.60))),
  suv    = list(body = list(x = c(.03,.04,.12,.24,.30,.78,.88,.95,.95,.03),
                            y = c(.30,.48,.52,.54,.82,.84,.60,.52,.30,.30)),
                win  = list(x = c(.32,.38,.74,.76), y = c(.55,.79,.80,.58))),
  truck  = list(body = list(x = c(.02,.03,.10,.22,.28,.56,.60,.98,.98,.02),
                            y = c(.30,.46,.50,.52,.80,.82,.54,.54,.30,.30)),
                win  = list(x = c(.30,.35,.54,.56), y = c(.55,.78,.79,.56))),
  sports = list(body = list(x = c(.02,.02,.14,.34,.48,.66,.82,.96,.98,.02),
                            y = c(.28,.40,.44,.46,.62,.64,.48,.42,.28,.28)),
                win  = list(x = c(.38,.50,.62,.72), y = c(.46,.60,.61,.47))),
  van    = list(body = list(x = c(.03,.04,.12,.22,.28,.86,.94,.96,.96,.03),
                            y = c(.30,.48,.52,.56,.86,.88,.70,.54,.30,.30)),
                win  = list(x = c(.32,.36,.82,.84), y = c(.57,.84,.85,.62)))
)

BODY_LEVELS <- c("hatch", "sedan", "sports", "suv", "truck", "van")

# Colour-blind-safe, and distinguishable in greyscale.
pal <- c(sedan  = "#4C72B0", hatch = "#55A868", sports = "#CCB974",
         suv    = "#C44E52", truck = "#8172B2", van    = "#64B5CD")

#' Build one car silhouette grob.
#' @param vp optional viewport controlling position and physical size.
car_grob <- function(body, fill, outline = TRUE, vp = NULL) {
  s <- shapes[[body]]
  if (is.null(s)) s <- shapes[["sedan"]]
  gp_body <- if (outline) grid::gpar(fill = fill, col = "white", lwd = 1.1)
             else         grid::gpar(fill = fill, col = NA)
  grid::grobTree(
    grid::polygonGrob(s$body$x, s$body$y, gp = gp_body),
    grid::polygonGrob(s$win$x,  s$win$y,  gp = grid::gpar(fill = "#FFFFFF", alpha = .55, col = NA)),
    grid::circleGrob(.24, .22, r = .115, gp = grid::gpar(fill = "grey15", col = NA)),
    grid::circleGrob(.78, .22, r = .115, gp = grid::gpar(fill = "grey15", col = NA)),
    grid::circleGrob(.24, .22, r = .045, gp = grid::gpar(fill = "grey75", col = NA)),
    grid::circleGrob(.78, .22, r = .045, gp = grid::gpar(fill = "grey75", col = NA)),
    vp = vp
  )
}

# Legend key that draws a little car rather than a plain square.
draw_key_car <- function(data, params, size) {
  b <- if (!is.null(data$body) && !is.na(data$body)) as.character(data$body) else "sedan"
  car_grob(b, data$fill %||% "grey60", outline = FALSE,
           vp = grid::viewport(width = grid::unit(.9, "npc"), height = grid::unit(.9 / 2.4, "npc")))
}

GeomCar <- ggplot2::ggproto("GeomCar", ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes  = ggplot2::aes(fill = "grey60", body = "sedan"),
  draw_key     = draw_key_car,
  draw_panel = function(data, panel_params, coord, car_pt = 26, aspect = 2.4, outline = TRUE) {
    co <- coord$transform(data, panel_params)
    ok <- is.finite(co$x) & is.finite(co$y) &
          co$x > -0.15 & co$x < 1.15 & co$y > -0.15 & co$y < 1.15
    co <- co[ok, , drop = FALSE]
    if (!nrow(co)) return(grid::nullGrob())
    grobs <- lapply(seq_len(nrow(co)), function(i) {
      car_grob(as.character(co$body[i]), co$fill[i], outline,
        vp = grid::viewport(x      = grid::unit(co$x[i], "npc"),
                            y      = grid::unit(co$y[i], "npc"),
                            width  = grid::unit(car_pt, "pt"),
                            height = grid::unit(car_pt / aspect, "pt")))
    })
    do.call(grid::grobTree, grobs)
  }
)

#' Draw cars at (x, y). Size is in POINTS, so it is independent of zoom.
geom_car <- function(mapping = NULL, data = NULL, stat = "identity",
                     position = "identity", ..., car_pt = 26, aspect = 2.4,
                     outline = TRUE, na.rm = FALSE, show.legend = NA,
                     inherit.aes = TRUE) {
  ggplot2::layer(
    geom = GeomCar, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(car_pt = car_pt, aspect = aspect, outline = outline, na.rm = na.rm, ...)
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a
