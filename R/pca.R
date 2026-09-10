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

#' Name an axis from the attributes that load most strongly on it.
axis_name <- function(loadings, pc, n = 2, thresh = 0.45) {
  v <- loadings[[pc]]
  o <- order(abs(v), decreasing = TRUE)
  hi <- o[abs(v[o]) >= thresh]
  if (!length(hi)) hi <- o[seq_len(min(n, length(o)))]
  hi <- hi[seq_len(min(n, length(hi)))]
  paste(ifelse(v[hi] < 0, paste0("less ", loadings$var[hi]), loadings$var[hi]), collapse = " & ")
}
