# ---------------------------------------------------------------------------
# The "How it works" tab.
#
# Numbers and the attribute table are derived from CARS / ATTRS at build time,
# so the prose cannot drift out of sync with the data or the transforms.
# Styling uses Bootstrap 5.3 semantic classes only (text-body-secondary,
# bg-body-tertiary, border, table) so the whole tab follows the dark-mode
# toggle without a second palette.
# ---------------------------------------------------------------------------

xp_css <- htmltools::HTML("
.xp { max-width: 980px; }
.xp h2 { font-size: 1.05rem; font-weight: 700; margin: 0; }
.xp p  { margin-bottom: .6rem; line-height: 1.6; }
.xp .lead-in { font-size: 1.05rem; line-height: 1.65; }
.xp code { font-size: .86em; }
.xp .num {
  display: inline-flex; align-items: center; justify-content: center;
  width: 1.6rem; height: 1.6rem; flex: 0 0 1.6rem; border-radius: 50%;
  background: var(--bs-primary); color: #fff; font-size: .8rem; font-weight: 700;
}
.xp .flow { display: flex; flex-wrap: wrap; align-items: stretch; gap: .35rem; }
.xp .flow .box {
  flex: 1 1 0; min-width: 110px; padding: .5rem .6rem; border-radius: .4rem;
  background: var(--bs-body-bg); border: 1px solid var(--bs-border-color);
}
.xp .flow .box .t { font-weight: 650; font-size: .82rem; }
.xp .flow .box .s { font-size: .72rem; color: var(--bs-secondary-color); font-family: var(--bs-font-monospace); }
.xp .flow .arr { align-self: center; color: var(--bs-secondary-color); font-size: 1rem; }
.xp .note {
  border-left: 3px solid var(--bs-primary); padding: .5rem .75rem;
  background: var(--bs-body-bg); border-radius: .25rem; font-size: .87rem;
}
.xp .note .h { font-weight: 650; }
.xp table { font-size: .85rem; }
.xp .mono { font-family: var(--bs-font-monospace); font-size: .82rem; }
.xp .compass {
  display: grid; gap: .6rem; grid-template-columns: repeat(3, minmax(0, 1fr));
  grid-template-areas: '. up .' 'left mid right' '. down .';
}
.xp .compass .up { grid-area: up; }     .xp .compass .down  { grid-area: down; }
.xp .compass .left { grid-area: left; } .xp .compass .right { grid-area: right; }
.xp .compass .mid {
  grid-area: mid; display: flex; flex-direction: column; justify-content: center;
  align-items: center; text-align: center; font-size: .78rem; color: var(--bs-secondary-color);
}
.xp .dir { border: 1px solid var(--bs-border-color); border-radius: .4rem;
  padding: .55rem .7rem; background: var(--bs-body-bg); }
.xp .dir .k { font-size: .7rem; font-weight: 700; letter-spacing: .05em;
  text-transform: uppercase; color: var(--bs-secondary-color); }
.xp .dir .w { font-weight: 700; font-size: .95rem; margin: .15rem 0 .3rem; }
.xp .dir .r { font-family: var(--bs-font-monospace); font-size: .72rem; color: var(--bs-secondary-color); }
.xp .dir .c { font-size: .8rem; margin-top: .35rem; }
.xp .quad td { width: 42%; vertical-align: top; }
.xp .quad th { font-size: .8rem; }
@media (max-width: 800px) {
  .xp .compass { grid-template-columns: 1fr; grid-template-areas: 'up' 'left' 'mid' 'right' 'down'; }
}
@media (max-width: 700px) { .xp .flow .arr { display: none; } }
")

xp_sec <- function(n, title, ...) {
  bslib::card(
    class = "mb-3",
    bslib::card_header(
      tags$div(class = "d-flex align-items-center gap-2",
               tags$span(class = "num", n), tags$h2(title))
    ),
    bslib::card_body(...)
  )
}

xp_note <- function(head, ...) {
  tags$div(class = "note my-2", tags$span(class = "h", head), " ", ...)
}

xp_flow <- function() {
  step <- function(t, s) tags$div(class = "box", tags$div(class = "t", t), tags$div(class = "s", s))
  arr  <- tags$div(class = "arr", HTML("&rarr;"))
  tags$div(class = "flow my-1",
    step("Specs",       "data/cars.csv"),
    arr, step("Transform",   "build_matrix()"),
    arr, step("Standardise", "scale. = TRUE"),
    arr, step("PCA",         "prcomp()"),
    arr, step("Pin signs",   "fix_signs()"),
    arr, step("Scores + r",  "cor(X, PC)"),
    arr, step("Draw",        "build_map()")
  )
}

#' The attribute table, generated from ATTRS so it always matches the engine.
xp_attr_table <- function(attrs) {
  why <- c(
    price_k = "Sticker price, $000s.",
    hp      = "Peak engine or motor output.",
    mpg     = "Logged first. EV MPGe (122-132) would otherwise dwarf petrol MPG (18-36) and collapse this into an \"is it an EV\" flag.",
    weight  = "Kerb weight, lb.",
    seats   = "Belted seating positions.",
    sec060  = "Negated, so a larger number means quicker and the arrow points the intuitive way.",
    cargo   = "Boot or bed volume, cu ft.",
    length  = "Overall length, inches."
  )
  tf <- c(none = "as measured", log = "log10", negate = "sign flipped")
  tags$div(class = "table-responsive",
    tags$table(class = "table table-sm align-middle mb-0",
      tags$thead(tags$tr(
        tags$th("Shown as"), tags$th("Column"), tags$th("Transform"), tags$th("Notes"))),
      tags$tbody(lapply(seq_len(nrow(attrs)), function(i) {
        v <- attrs$var[i]
        tags$tr(
          tags$td(tags$span(class = "fw-semibold", attrs$label[i])),
          tags$td(tags$code(v)),
          tags$td(if (attrs$transform[i] == "none")
                    tags$span(class = "text-body-secondary", tf[["none"]])
                  else tags$span(class = "badge text-bg-warning", tf[[attrs$transform[i]]])),
          tags$td(class = "text-body-secondary small", why[[v]] %||% "")
        )
      }))
    )
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' "What left, right, up and down mean" for the axes currently on screen.
#'
#' A plain function, not a Shiny output, so it can be checked from a script:
#' axes_explainer(fit_map(cars), fit_map(cars)$cars, "PC1", "PC2").
#' @param m a fit_map() result
#' @param d the cars currently shown (may be a body-type subset of m$cars)
axes_explainer <- function(m, d, xpc, ypc, thresh = 0.45) {
  if (!nrow(d)) return(tags$p(class = "text-body-secondary", "No cars match the current body-type filter."))
  ve <- function(pc) m$ve[as.integer(sub("PC", "", pc))]
  ex <- axis_ends(m$loadings, xpc, n = 4, thresh = thresh)
  ey <- axis_ends(m$loadings, ypc, n = 4, thresh = thresh)
  cap <- function(w) { w <- paste(w, collapse = ", "); paste0(toupper(substr(w, 1, 1)), substring(w, 2)) }
  far <- function(v, decreasing) paste(d$model[order(v, decreasing = decreasing)][seq_len(min(3, nrow(d)))],
                                       collapse = ", ")
  strength <- function(e) paste(sprintf("%s %.2f", e$var, abs(e$r)), collapse = "  \u00b7  ")

  box <- function(cls, key, words, e, cars) tags$div(class = paste("dir", cls),
    tags$div(class = "k", HTML(key)),
    tags$div(class = "w", cap(words)),
    tags$div(class = "r", title = "How strongly each attribute lines up with this axis (|r|, 0 to 1)",
             if (e$weak) paste("weak axis --", strength(e)) else strength(e)),
    tags$div(class = "c", tags$span(class = "text-body-secondary", "Furthest: "), cars))

  sx <- d[[xpc]]; sy <- d[[ypc]]
  compass <- tags$div(class = "compass my-2",
    box("up",    sprintf("&uarr; Top &middot; %s high", ypc),    ey$pos, ey, far(sy, TRUE)),
    box("left",  sprintf("&larr; Left &middot; %s low", xpc),    ex$neg, ex, far(sx, FALSE)),
    tags$div(class = "mid",
      tags$div(tags$strong("Centre = the average car")),
      tags$div(sprintf("%s across (%.1f%%)", xpc, ve(xpc))),
      tags$div(sprintf("%s up (%.1f%%)", ypc, ve(ypc)))),
    box("right", sprintf("%s high &middot; Right &rarr;", xpc),  ex$pos, ex, far(sx, TRUE)),
    box("down",  sprintf("&darr; Bottom &middot; %s low", ypc),  ey$neg, ey, far(sy, FALSE)))

  # Quadrants as segments, named from each axis's strongest word.
  q <- ifelse(sx >= 0, ifelse(sy >= 0, "tr", "br"), ifelse(sy >= 0, "tl", "bl"))
  cell <- function(k, h, v) tags$td(
    tags$div(class = "fw-semibold small", sprintf("%s & %s", h, v)),
    tags$div(class = "small text-body-secondary",
             if (any(q == k)) paste(d$model[q == k], collapse = ", ") else "(none)"))
  quads <- tags$div(class = "table-responsive",
    tags$table(class = "table table-sm quad mb-0",
      tags$thead(tags$tr(tags$th(""), tags$th(sprintf("Left: %s", ex$neg[1])),
                         tags$th(sprintf("Right: %s", ex$pos[1])))),
      tags$tbody(
        tags$tr(tags$th(sprintf("Top: %s", ey$pos[1])),
                cell("tl", ex$neg[1], ey$pos[1]), cell("tr", ex$pos[1], ey$pos[1])),
        tags$tr(tags$th(sprintf("Bottom: %s", ey$neg[1])),
                cell("bl", ex$neg[1], ey$neg[1]), cell("br", ex$pos[1], ey$neg[1])))))

  # Where each attribute lives on THIS pair of axes.
  L  <- m$loadings; rx <- abs(L[[xpc]]); ry <- abs(L[[ypc]])
  by <- function(keep, w) { i <- which(keep); L$var[i][order(w[i], decreasing = TRUE)] }
  across <- by(rx >= thresh & rx >= ry, rx)
  updown <- by(ry >= thresh & ry >  rx, ry)
  weak   <- by(rx <  thresh & ry <  thresh, pmax(rx, ry))
  line <- function(lab, v) if (length(v)) tags$li(tags$strong(lab), " ", paste(v, collapse = ", "))

  tagList(
    compass,
    tags$h6(class = "mt-3 mb-2 fw-bold", "The four corners"),
    quads,
    tags$h6(class = "mt-3 mb-2 fw-bold", "Which attributes this view shows well"),
    tags$ul(class = "mb-2",
      line("Mostly left to right:", across),
      line("Mostly up and down:", updown),
      if (length(weak)) tags$li(tags$strong("Weak on both:"), " ",
        paste0(paste(weak, collapse = ", "), " \u2014 where a car sits on this view says little about ",
               if (length(weak) == 1) "it" else "these", ". Try the other axes in the sidebar."))),
    xp_note("Relative, not absolute.",
      sprintf(paste0("The axes are fitted to all %d cars, and the crosshair is their average. ",
                     "\u201cLeft\u201d means %s than the average of this group, not in any absolute ",
                     "sense. Hiding body types only hides cars; it does not refit the axes."),
              nrow(m$cars), ex$neg[1]))
  )
}

#' Build the whole tab.
explain_ui <- function(cars, attrs) {
  n_car   <- nrow(cars)
  n_brand <- length(unique(cars$brand))
  n_body  <- length(unique(cars$body))
  n_attr  <- nrow(attrs)

  tags$div(class = "xp",
    tags$style(xp_css),

    tags$p(class = "lead-in",
      "A perceptual map answers one question: ",
      tags$strong("which cars does the market treat as substitutes for each other?"),
      " Two cars sit close together here because their ", n_attr, " specifications are similar overall ",
      "— not because of any single number. Everything below is computed live from ",
      tags$code("data/cars.csv"), "; nothing is hard-coded."),

    xp_flow(),
    tags$p(class = "text-body-secondary small mt-2 mb-3",
           "Every stage re-runs the moment you tick an attribute, so the map you are looking at ",
           "is always a fresh fit, never a cached image."),

    xp_sec(1, "The data",
      tags$p(n_car, " current models from ", n_brand, " brands, across ", n_body,
             " body types, each measured on ", n_attr, " attributes."),
      xp_attr_table(attrs),
      xp_note("Hand-compiled.",
        "These are approximate real-world figures assembled for illustration, not a licensed ",
        "manufacturer dataset. Swap in your own rows and everything downstream adapts — ",
        "the only requirement is the same column names.")
    ),

    xp_sec(2, "Putting the attributes on a level playing field",
      tags$p("Price runs into the tens of thousands, seat count runs from 2 to 8. Fed in raw, ",
             "price would decide the entire map purely because its numbers are bigger. So each ",
             "column is converted to a ", tags$strong("z-score"), " — subtract the mean, divide by ",
             "the standard deviation — via ", tags$code("prcomp(scale. = TRUE)"),
             ". Afterwards every attribute has mean 0 and SD 1, and contributes on equal terms."),
      tags$p("Two columns are adjusted first, for the reasons in the table above: ",
             tags$code("mpg"), " is logged, and ", tags$code("sec060"),
             " is negated so that the axis reads as ", tags$em("quickness"),
             " rather than ", tags$em("seconds taken"), "."),
      xp_note("Edge case.",
        "If a filter leaves a column with zero variance it is dropped before fitting — ",
        "a constant column has no standard deviation to divide by and would make ",
        tags$code("prcomp"), " fail.")
    ),

    xp_sec(3, "The PCA itself",
      tags$p("With ", n_attr, " attributes each car is a point in ", n_attr,
             "-dimensional space. Principal Component Analysis finds the direction through that ",
             "cloud along which the cars are most spread out — that becomes ", tags$strong("PC1"),
             ". It then finds the direction of greatest remaining spread that is at right angles ",
             "to the first — ", tags$strong("PC2"), " — and so on."),
      tags$p("Because each component is perpendicular to the others, they carry non-overlapping ",
             "information. The percentages in the boxes at the top are each component's share of ",
             "total variance: ", tags$span(class = "mono", "100 × sdev² / Σ sdev²"),
             ". The ", tags$strong("Variance"), " tab plots all of them."),
      xp_note("Why two axes are enough.",
        "The first two components between them usually carry the majority of the variance, which is ",
        "why a flat picture is a fair summary. The exact figure for your current selection is in the ",
        "subtitle above the map — if it drops low, treat the layout with more caution.")
    ),

    xp_sec(4, "Making the map reproducible",
      tags$p("PCA fixes each axis's ", tags$em("orientation"), " but not its ", tags$em("sign"),
             ". Flipping a component left-to-right is mathematically identical, so the same data ",
             "can legitimately produce a mirror-image map on a different run or a different machine."),
      tags$p("To stop that, ", tags$code("fix_signs()"), " pins each axis to an anchor: PC1 is ",
             "oriented so ", tags$strong("weight"), " points positive, PC2 so ", tags$strong("price"),
             " does. If the anchor is deselected it falls back to whichever attribute loads most ",
             "strongly. Heavy is always right, expensive is always up."),
      xp_note("Consequence.", "The map is deterministic — no ", tags$code("set.seed"),
              " required, and two people running this get pixel-identical output.")
    ),

    xp_sec(5, "What left, right, up and down mean",
      tags$p("Each direction on the map stands for a mix of attributes. Moving right along ",
             "the default view means heavier, roomier cars; moving up means quicker, more ",
             "powerful, pricier ones. The exact mix depends on which attributes and axes you ",
             "have selected, so everything below is recomputed live from the current fit."),
      tags$p(class = "small text-body-secondary",
             "The words at the four ends of the crosshair on the map are the short form of this ",
             "section. The numbers are correlations with the axis (0 = unrelated, 1 = moves in ",
             "lockstep); only attributes at 0.45 or above count."),
      uiOutput("xp_axes")
    ),

    xp_sec(6, "Reading the map",
      tags$div(class = "row g-3",
        tags$div(class = "col-md-6",
          tags$p(tags$strong("Position."), " Distance is similarity. Cars near each other have ",
                 "comparable specs across the board and are the ones genuinely competing."),
          tags$p(tags$strong("Arrows."), " The compass shows each attribute's ",
                 tags$em("correlation"), " with the two axes on screen. Direction is what the ",
                 "attribute pulls toward; length is how well it is captured by this particular ",
                 "pair of axes. A short arrow means that attribute mostly lives in a component ",
                 "you are not currently looking at."),
          tags$p(tags$strong("Crosshair ends."), " The words at each end say what moving that ",
                 "way means, from ", tags$code("axis_ends()"), ". They get a band of their own ",
                 "around the edge, so car labels never land on them."),
          tags$p(tags$strong("Axis titles."), " Generated by ", tags$code("axis_name()"),
                 ", which names the attributes correlating above 0.45 with that axis. They are a ",
                 "readable summary, not an official label.")),
        tags$div(class = "col-md-6",
          tags$p(tags$strong("Quadrants."), " The shaded corners are tagged from whichever ",
                 "attribute dominates each axis — a quick orientation aid, nothing statistical."),
          tags$p(tags$strong("The origin."), " The crosshair is the average car, not zero. A model ",
                 "at the centre is unremarkable on the selected attributes; distance from the ",
                 "middle is distinctiveness."),
          xp_note("Correlation scaling.",
            "Arrows are ", tags$code("cor(X, scores)"), ", so every one fits inside a unit circle ",
            "and lengths are comparable to each other. Cars and arrows use different scales, so ",
            "read arrows for ", tags$em("direction"), ", not for which car scores highest."))
      ),
      tags$hr(class = "my-3"),
      uiOutput("xp_live")
    ),

    xp_sec(7, "How the picture gets drawn",
      tags$p("There are no image files anywhere in this project. Each car is a ",
             tags$code("grid"), " silhouette — polygons and circles assembled in ",
             tags$code("R/silhouettes.R"), " — wrapped in a custom ggplot2 geom, ",
             tags$code("GeomCar"), ". Six body shapes, coloured by type."),
      tags$div(class = "row g-3 mt-1",
        tags$div(class = "col-md-4",
          tags$p(class = "mb-1", tags$strong("Sized in points, not data units.")),
          tags$p(class = "small text-body-secondary",
                 "Cars keep their physical size when you zoom, so a dense cluster spreads apart ",
                 "instead of magnifying into a bigger overlap. This is also why ",
                 tags$code("annotation_custom"), " was not usable — it is not vectorised over rows.")),
        tags$div(class = "col-md-4",
          tags$p(class = "mb-1", tags$strong("Labels are force-directed.")),
          tags$p(class = "small text-body-secondary",
                 tags$code("repel_labels()"), " pushes overlapping names apart and draws a leader ",
                 "line back to the car. It is told the true rendered size in inches, so labels land ",
                 "correctly whatever the window size.")),
        tags$div(class = "col-md-4",
          tags$p(class = "mb-1", tags$strong("The aspect ratio is locked.")),
          tags$p(class = "small text-body-secondary",
                 tags$code("coord_fixed(ratio = 1)"), " keeps one unit on X equal to one unit on Y. ",
                 "Without it the axes would stretch independently and visual distance would stop ",
                 "meaning similarity — the one thing the map is for.")))
    ),

    xp_sec(8, "What this map cannot tell you",
      tags$ul(class = "mb-0",
        tags$li(class = "mb-2", tags$strong("It measures specifications, not opinions. "),
                "Classical marketing perceptual maps are built from survey data — how buyers ",
                "rate brands on prestige, reliability, styling. This map is built from objective ",
                "numbers, so it shows what cars ", tags$em("are"), ", not how they are ",
                tags$em("seen"), ". Brand image does not appear anywhere."),
        tags$li(class = "mb-2", tags$strong("You are seeing a shadow. "),
                "A 2-D picture of ", n_attr, "-dimensional data loses whatever the remaining ",
                "components hold. Two cars can look adjacent here yet differ sharply on PC3. ",
                "Switch the axis dropdowns to check."),
        tags$li(class = "mb-2", tags$strong("The axes are interpretations. "),
                "PCA yields directions, not names. The titles are a reasonable reading of the ",
                "loadings, and a different analyst might phrase them differently."),
        tags$li(tags$strong("The sample is small and curated. "),
                n_car, " models chosen for recognisability is not the market. Adding or removing ",
                "cars moves everything, because the components are fitted to whatever is present."))
    ),

    xp_sec(9, "Where the code lives",
      tags$div(class = "table-responsive",
        tags$table(class = "table table-sm mb-0",
          tags$tbody(
            tags$tr(tags$td(tags$code("R/pca.R")),
                    tags$td(class = "text-body-secondary", "Transforms, scaling, sign pinning, correlation loadings, axis names and direction words")),
            tags$tr(tags$td(tags$code("R/silhouettes.R")),
                    tags$td(class = "text-body-secondary", "Body shapes, the palette, and the GeomCar ggproto geom")),
            tags$tr(tags$td(tags$code("R/layout.R")),
                    tags$td(class = "text-body-secondary", "Axis limits, aspect fitting, the edge band, label repel, inset placement")),
            tags$tr(tags$td(tags$code("R/plot.R")),
                    tags$td(class = "text-body-secondary", "The map, the compass, quadrants, and the light/dark plot theme")),
            tags$tr(tags$td(tags$code("R/explain.R")),
                    tags$td(class = "text-body-secondary", "This tab")),
            tags$tr(tags$td(tags$code("app.R")),
                    tags$td(class = "text-body-secondary", "The Shiny layer — reactivity, zoom, click handling, downloads")),
            tags$tr(tags$td(tags$code("run_static.R")),
                    tags$td(class = "text-body-secondary", "Renders the map to PNG and PDF with no Shiny involved"))))),
      tags$p(class = "small text-body-secondary mt-2 mb-0",
             "Every plotting function works standalone — ", tags$code("source()"),
             " the four files in R/ and call ", tags$code("build_map(fit_map(cars))"),
             " from a plain script.")
    )
  )
}
