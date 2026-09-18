# ---------------------------------------------------------------------------
# The statistical engine: PCA on scaled attributes.
# ---------------------------------------------------------------------------

# Attribute metadata. `transform` is applied BEFORE scaling.
#   negate -> bigger is "more", so every arrow points the intuitive way
#   log    -> compresses EV MPGe (122-132) against petrol MPG (18-36); without
#             this the variable degenerates into an "is it an EV" indicator and
#             contributes almost nothing to PC1/PC2.
ATTRS <- data.frame(
  var       = c("price_k","hp",   "mpg",       "weight","seats","sec060",   "cargo","length"),
  label     = c("price",  "power","efficiency","weight","seats","quickness","cargo","length"),
  transform = c("none",   "none", "log",       "none",  "none", "negate",   "none", "none"),
  # Words for each end of an attribute, used to say what moving along an axis
  # means. `more` is the high end AFTER the transform, so quickness -> "quicker".
  more = c("pricier", "more powerful", "more efficient", "heavier", "more seats",
           "quicker", "more cargo", "longer"),
  less = c("cheaper", "less powerful", "less efficient", "lighter", "fewer seats",
           "slower",  "less cargo", "shorter"),
  # What kind of thing each attribute measures, so a component can be given a
  # one-word name ("size", "performance") from the attributes that drive it.
  theme = c("price", "performance", "efficiency", "size", "size",
            "performance", "size", "size"),
  stringsAsFactors = FALSE
)

DEFAULT_VARS <- ATTRS$var

#' Build the transformed, renamed attribute matrix.
build_matrix <- function(cars, vars = DEFAULT_VARS) {
  vars <- intersect(vars, ATTRS$var)
  if (length(vars) < 3) stop("Need at least 3 attributes to build a map.")
  m <- ATTRS[match(vars, ATTRS$var), ]
  X <- as.matrix(cars[, m$var, drop = FALSE])
  for (i in seq_len(nrow(m))) {
    X[, i] <- switch(m$transform[i],
                     log    = log10(X[, i]),
                     negate = -X[, i],
                     X[, i])
  }
  colnames(X) <- m$label
  # A constant column has zero variance and makes prcomp(scale.=TRUE) fail.
  keep <- apply(X, 2, function(z) stats::sd(z) > 0)
  X[, keep, drop = FALSE]
}

#' Pin each axis's sign so the map is identical on every run.
#' Prefers a named anchor variable; falls back to the strongest loading, which
#' is always defined even when the user has deselected the anchors.
fix_signs <- function(pc, anchors = c(PC1 = "weight", PC2 = "price")) {
  for (k in seq_len(ncol(pc$rotation))) {
    key <- paste0("PC", k)
    nm  <- if (key %in% names(anchors)) anchors[[key]] else NULL
    v  <- if (!is.null(nm) && nm %in% rownames(pc$rotation)) nm
          else rownames(pc$rotation)[which.max(abs(pc$rotation[, k]))]
    if (pc$rotation[v, k] < 0) {
      pc$rotation[, k] <- -pc$rotation[, k]
      pc$x[, k]        <- -pc$x[, k]
    }
  }
  pc
}

#' Fit the perceptual map.
#'
#' Returns scores joined onto `cars`, plus loadings expressed as variable-score
#' CORRELATIONS. That is the standard correlation-biplot scaling: an arrow's
#' length is how well that attribute is represented on the two axes shown, and
#' everything sits inside the unit circle, so the compass is directly readable.
fit_map <- function(cars, vars = DEFAULT_VARS, anchors = c(PC1 = "weight", PC2 = "price")) {
  X  <- build_matrix(cars, vars)
  pc <- stats::prcomp(X, scale. = TRUE)
  pc <- fix_signs(pc, anchors)

  ve     <- 100 * pc$sdev^2 / sum(pc$sdev^2)
  npc    <- ncol(pc$x)
  scores <- as.data.frame(pc$x)
  names(scores) <- paste0("PC", seq_len(npc))

  corr <- stats::cor(X, pc$x)                       # variable x PC correlations
  loadings <- as.data.frame(corr)
  names(loadings) <- paste0("PC", seq_len(npc))
  loadings$var <- rownames(corr)

  list(
    cars     = cbind(cars, scores),
    loadings = loadings,
    ve       = ve,
    npc      = npc,
    vars     = colnames(X),
    prcomp   = pc
  )
}

#' What moving along an axis means, in words.
#'
#' Uses the attributes correlating with the axis at |r| >= thresh, strongest
#' first. A NEGATIVE correlation describes the opposite end, so weight at
#' r = -0.9 contributes "lighter" to the high end, not "heavier". If nothing
#' clears the threshold the strongest attribute is used anyway and `weak` is
#' set, so an end is never blank.
#' @return list(pos, neg, var, r, weak) -- pos/neg are the high/low end words.
axis_ends <- function(loadings, pc, n = 3, thresh = 0.45) {
  r <- loadings[[pc]]
  o <- order(abs(r), decreasing = TRUE)
  keep <- o[abs(r[o]) >= thresh]
  weak <- !length(keep)
  if (weak) keep <- o[1]
  keep <- keep[seq_len(min(n, length(keep)))]
  a  <- ATTRS[match(loadings$var[keep], ATTRS$label), ]
  up <- r[keep] > 0
  list(pos  = ifelse(up, a$more, a$less),
       neg  = ifelse(up, a$less, a$more),
       var  = loadings$var[keep],
       r    = r[keep],
       weak = weak)
}

#' A one-word name for a component: the theme whose attributes load on it most.
#'
#' Sums |r| per theme over the attributes at |r| >= thresh. A second theme is
#' named too if it carries at least 60% of the leader's weight, so a component
#' that is genuinely two things says so. NA when no attribute clears the
#' threshold -- the component has no clear meaning and should not pretend to.
axis_theme <- function(loadings, pc, thresh = 0.45) {
  r <- loadings[[pc]]
  strong <- abs(r) >= thresh
  if (!any(strong)) return(NA_character_)
  th <- ATTRS$theme[match(loadings$var, ATTRS$label)]
  w  <- sort(tapply(abs(r[strong]), th[strong], sum), decreasing = TRUE)
  if (length(w) > 1 && w[[2]] >= 0.6 * w[[1]]) paste(names(w)[1], "&", names(w)[2]) else names(w)[1]
}

#' Name an axis from the attributes that load most strongly on it.
axis_name <- function(loadings, pc, n = 2, thresh = 0.45) {
  v <- loadings[[pc]]
  o <- order(abs(v), decreasing = TRUE)
  hi <- o[abs(v[o]) >= thresh]
  if (!length(hi)) hi <- o[seq_len(min(n, length(o)))]
  hi <- hi[seq_len(min(n, length(hi)))]
  paste(ifelse(v[hi] < 0, paste0("less ", loadings$var[hi]), loadings$var[hi]), collapse = " & ")
}
