# ---------------------------------------------------------------------------
# Interactive perceptual map.  In RStudio: click "Run App", or shiny::runApp()
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(ggplot2); library(grid)
  library(patchwork); library(scales); library(DT)
})
for (f in c("silhouettes", "pca", "layout", "plot", "explain")) source(file.path("R", paste0(f, ".R")))

CARS <- read.csv("data/cars.csv", stringsAsFactors = FALSE)
# Only offer body types the data actually has, in the canonical order, so
# removing every car of a type does not leave a checkbox that filters nothing.
BODIES <- intersect(BODY_LEVELS, unique(CARS$body))
VAR_CHOICES <- stats::setNames(ATTRS$var, ATTRS$label)
PC_CHOICES  <- paste0("PC", 1:4)

ui <- page_sidebar(
  title = tags$div(
    class = "d-flex justify-content-between align-items-center w-100",
    tags$span("Car Perceptual Map"),
    input_dark_mode(id = "mode", mode = "light")
  ),
  theme = bs_theme(version = 5, preset = "flatly"),
  # The page scrolls rather than filling the window: with the map at its full
  # height and the compass stacked under it on a narrow window, a fill layout
  # squeezed the value boxes from 105px to 61px and clipped the numbers.
  fillable = FALSE,

  sidebar = sidebar(
    width = 300,
    checkboxGroupInput("vars", "Attributes in the PCA",
                       choices = VAR_CHOICES, selected = DEFAULT_VARS),
    helpText("At least 3. The map recomputes as you change these."),
    hr(),
    layout_columns(
      col_widths = c(6, 6),
      selectInput("xpc", "X axis", PC_CHOICES, selected = "PC1"),
      selectInput("ypc", "Y axis", PC_CHOICES, selected = "PC2")
    ),
    checkboxGroupInput("bodies", "Body types",
                       choices = BODIES, selected = BODIES, inline = TRUE),
    sliderInput("car_pt", "Car size (pt)", min = 12, max = 70, value = 34, step = 2),
    hr(),
    checkboxInput("labels",   "Model labels",   TRUE),
    checkboxInput("arrows",   "Attribute arrows", TRUE),
    checkboxInput("dirs",     "Direction labels", TRUE),
    checkboxInput("key",      "Axis key",         TRUE),
    checkboxInput("quads",    "Quadrant shading", TRUE),
    checkboxInput("compass",  "Loadings compass", TRUE),
    checkboxInput("hulls",    "Segment hulls",  FALSE),
    hr(),
    actionButton("reset", "Reset zoom", class = "btn-sm btn-outline-secondary"),
    downloadButton("dl", "Download PNG", class = "btn-sm btn-primary")
  ),

  # bslib floors the main area at 576px, so with a 300px sidebar the page needs
  # 892px and anything narrower clips on the right -- the third value box first.
  tags$head(tags$style(HTML(".bslib-page-main { min-width: 0 !important; }"))),

  layout_columns(
    col_widths = breakpoints(sm = 12, md = 6, lg = 4), row_heights = "auto",
    value_box("X axis variance", textOutput("vx"), theme = "primary",   height = "105px"),
    value_box("Y axis variance", textOutput("vy"), theme = "secondary", height = "105px"),
    value_box("Cars shown",      textOutput("nshown"), theme = "success", height = "105px")
  ),

  navset_card_tab(
    nav_panel(
      "Map",
      # fillable = FALSE so the plot keeps its 620px: once the compass stacks
      # underneath, a fillable body splits the height between them and the map
      # collapsed to 232px.
      card_body(
        fillable = FALSE,
        helpText("Drag a box then double-click to zoom in. Double-click empty space to reset. ",
                 "Click a car for its detail card."),
        layout_columns(
          # Below xl the compass drops under the map rather than squeezing it:
          # the map is the point, and a narrow panel is what hides car labels.
          # At a 1024px window, side-by-side left the map 490px and hid 19 of
          # 28 names; stacked it gets the full width and hides none.
          col_widths = breakpoints(sm = c(12, 12), xl = c(9, 3)),
          plotOutput("map", height = "620px",
                     click = "click", dblclick = "dblclick",
                     brush = brushOpts(id = "brush", resetOnNew = TRUE)),
          tagList(
            conditionalPanel("input.compass",
              card(card_header("What drives the axes"),
                   card_body(plotOutput("compass", height = "200px"), class = "p-1"))),
            uiOutput("detail")
          )
        )
      )
    ),
    nav_panel("Variance", card_body(plotOutput("scree", height = "380px"))),
    nav_panel("Loadings", card_body(uiOutput("loadkey"), DTOutput("loadtab"), fillable = FALSE)),
    nav_panel(
      "Data",
      card_body(
        # A flex row rather than a grid: on a narrow pane the grid pushed the
        # button past the card's edge, where it was clipped.
        tags$div(
          class = "d-flex justify-content-between align-items-start gap-3 mb-2 flex-wrap",
          helpText(class = "mb-0", textOutput("datacount", inline = TRUE),
                   " — the specs behind the map, exactly as the table shows them. ",
                   "The body-type filter in the sidebar applies here too."),
          downloadButton("dl_data", "Download CSV",
                         class = "btn-sm btn-outline-secondary flex-shrink-0")
        ),
        DTOutput("datatab"), fillable = FALSE
      )
    ),
    nav_panel("How it works", card_body(explain_ui(CARS, ATTRS), fillable = FALSE))
  )
)

server <- function(input, output, session) {

  rng <- reactiveValues(x = NULL, y = NULL)

  # input_dark_mode reports "light"/"dark"; NULL for one beat before the client
  # reports in, so default to light rather than letting the plot theme go NA.
  dark <- reactive(identical(input$mode, "dark"))

  # ---- model ----------------------------------------------------------------
  md <- reactive({
    validate(need(length(input$vars) >= 3, "Select at least 3 attributes."))
    fit_map(CARS, input$vars)
  })

  # Requested PCs may not exist when few attributes are selected.
  xpc <- reactive({ m <- md(); if (as.integer(sub("PC","",input$xpc)) <= m$npc) input$xpc else "PC1" })
  ypc <- reactive({ m <- md(); k <- as.integer(sub("PC","",input$ypc)); if (k <= m$npc) input$ypc else paste0("PC", min(2, m$npc)) })

  pdat <- reactive({
    m <- md(); d <- m$cars
    d <- d[d$body %in% input$bodies, , drop = FALSE]
    if (!nrow(d)) return(d)
    d$.x <- d[[xpc()]]; d$.y <- d[[ypc()]]
    d
  })

  # Match the repel to the real rendered size so labels land where they belong.
  dev_in <- reactive({
    cd <- session$clientData
    w <- cd[["output_map_width"]]; h <- cd[["output_map_height"]]
    if (is.null(w) || is.null(h) || w < 50) c(12, 8) else c(w / 96, h / 96)
  })

  # ---- zoom -----------------------------------------------------------------
  observeEvent(input$dblclick, {
    b <- input$brush
    if (!is.null(b)) { rng$x <- c(b$xmin, b$xmax); rng$y <- c(b$ymin, b$ymax) }
    else             { rng$x <- NULL; rng$y <- NULL }
  })
  observeEvent(input$reset, { rng$x <- NULL; rng$y <- NULL })
  observeEvent(list(input$vars, xpc(), ypc()), { rng$x <- NULL; rng$y <- NULL })

  # ---- the plot -------------------------------------------------------------
  build <- reactive({
    m <- md(); d <- pdat()
    validate(need(nrow(d) > 0, "No cars match the current body-type filter."))
    xl <- rng$x; yl <- rng$y
    if (is.null(xl)) xl <- map_limits(d$.x)
    if (is.null(yl)) yl <- map_limits(d$.y)
    # build_map fits the aspect itself, from the measured panel.
    build_map(m, xpc = xpc(), ypc = ypc(),
              car_pt = input$car_pt, dev_in = dev_in(),
              show_labels = input$labels, show_quadrants = input$quads,
              show_directions = input$dirs, show_key = input$key,
              show_arrows = input$arrows,
              show_hulls = input$hulls, bodies = input$bodies,
              xlim = xl, ylim = yl, dark = dark())
  })

  # coord_fixed() makes ggplot2 letterbox the gtable, so the DEVICE background is
  # visible on either side of the plot. Transparent hands that strip to the card
  # behind it, which is already the right colour in both themes.
  output$map <- renderPlot({ build() }, res = 96, bg = "transparent")

  output$compass <- renderPlot({ build_compass(md(), xpc(), ypc(), dark()) },
                                res = 96, bg = "transparent")

  output$vx <- renderText({ m <- md(); sprintf("%.1f%%", m$ve[as.integer(sub("PC","",xpc()))]) })
  output$vy <- renderText({ m <- md(); sprintf("%.1f%%", m$ve[as.integer(sub("PC","",ypc()))]) })
  output$nshown <- renderText({ as.character(nrow(pdat())) })

  # ---- click a car ----------------------------------------------------------
  output$detail <- renderUI({
    d <- pdat()
    if (is.null(input$click) || !nrow(d)) return(NULL)
    hit <- nearPoints(d, input$click, xvar = ".x", yvar = ".y", threshold = 40, maxpoints = 1)
    if (!nrow(hit)) return(helpText("No car at that spot - click a little closer."))
    spec <- function(lab, val) tags$div(class = "col",
      tags$div(class = "text-muted small", lab), tags$div(class = "fw-bold", val))
    card(class = "mt-3",
      card_header(sprintf("%s  -  %s", hit$model[1], tools::toTitleCase(hit$body[1]))),
      card_body(
        tags$div(class = "row row-cols-4 g-2",
          spec("Price",     sprintf("$%sk", hit$price_k[1])),
          spec("Power",     sprintf("%s hp", hit$hp[1])),
          spec("Efficiency",sprintf("%s mpg", hit$mpg[1])),
          spec("0-60",      sprintf("%.1f s", hit$sec060[1])),
          spec("Weight",    sprintf("%s lb", hit$weight[1])),
          spec("Seats",     hit$seats[1]),
          spec("Cargo",     sprintf("%s cu ft", hit$cargo[1])),
          spec("Length",    sprintf("%s in", hit$length[1]))
        ),
        tags$hr(),
        tags$div(class = "small text-muted",
                 sprintf("%s %.2f   |   %s %.2f", xpc(), hit$.x[1], ypc(), hit$.y[1]))
      )
    )
  })

  # ---- explainer, keyed to whatever is on screen right now -------------------
  output$xp_axes <- renderUI(axes_explainer(md(), pdat(), xpc(), ypc()))
  output$xp_pcs  <- renderUI(pcs_explainer(md(), xpc(), ypc()))

  output$xp_live <- renderUI({
    m <- md(); vx <- m$ve[as.integer(sub("PC", "", xpc()))]
    vy <- m$ve[as.integer(sub("PC", "", ypc()))]
    tags$div(class = "note",
      tags$span(class = "h", "Your current fit:"), " ",
      sprintf(paste0("%d attributes across %d cars. %s carries %.1f%% of the variance and reads as ",
                     "“%s”; %s carries %.1f%% and reads as “%s”. That puts %.1f%% of ",
                     "everything the specs contain on screen — the other %.1f%% sits in components ",
                     "you are not currently looking at."),
              length(m$vars), nrow(pdat()), xpc(), vx, axis_name(m$loadings, xpc()),
              ypc(), vy, axis_name(m$loadings, ypc()), vx + vy, 100 - (vx + vy)))
  })

  # ---- other tabs -----------------------------------------------------------
  output$scree <- renderPlot({
    m <- md()
    s <- data.frame(pc = factor(paste0("PC", seq_along(m$ve)), levels = paste0("PC", seq_along(m$ve))),
                    ve = m$ve)
    s$cum <- cumsum(s$ve)
    ink <- if (dark()) "#C7CCD4" else "grey25"
    cum_col <- if (dark()) "#9AA0AA" else "grey35"
    ggplot(s, aes(pc, ve)) +
      geom_col(fill = "#4C72B0", width = .65) +
      geom_line(aes(x = as.integer(pc), y = cum), colour = cum_col, linewidth = .5) +
      geom_point(aes(x = as.integer(pc), y = cum), colour = ink, size = 1.6) +
      geom_text(aes(label = sprintf("%.1f%%", ve)), vjust = -0.5, size = 3.2, colour = ink) +
      labs(title = "Variance explained by each component",
           subtitle = "Bars = individual, line = cumulative", x = NULL, y = "% of variance") +
      pm_theme(dark())
  }, res = 96, bg = "transparent")

  output$loadkey <- renderUI(loadings_key(md(), xpc(), ypc()))

  output$loadtab <- renderDT({
    m <- md()
    L <- m$loadings[, c("var", intersect(paste0("PC", 1:4), names(m$loadings)))]
    names(L)[1] <- "attribute"
    num <- setdiff(names(L), "attribute")
    cells <- if (dark()) c("#5C2B30", "transparent", "#22415F")
             else          c("#F6D8D9", "transparent", "#D6E3F2")
    datatable(L, rownames = FALSE, style = "bootstrap5", class = "table table-sm",
              options = list(dom = "t", pageLength = 20)) |>
      formatRound(num, 2) |>
      formatStyle(num, backgroundColor = styleInterval(c(-0.5, 0.5), cells))
  })

  # The table, the count and the download all read the same frame, so the file
  # can never disagree with what is on screen.
  data_shown <- reactive(pdat()[, c("model", "brand", "body", ATTRS$var)])

  output$datacount <- renderText(sprintf("%d cars, %d specs each", nrow(data_shown()), length(ATTRS$var)))

  output$dl_data <- downloadHandler(
    filename = function() sprintf("perceptual-map-cars-%d.csv", nrow(data_shown())),
    content = function(file) utils::write.csv(data_shown(), file, row.names = FALSE)
  )

  output$datatab <- renderDT({
    datatable(data_shown(), rownames = FALSE,
              style = "bootstrap5", class = "table table-sm",
              options = list(pageLength = 15, scrollX = TRUE))
  })

  # ---- export ---------------------------------------------------------------
  output$dl <- downloadHandler(
    filename = function() sprintf("perceptual-map-%s-%s.png", xpc(), ypc()),
    content = function(file) {
      # A downloaded file has no card behind it, so it needs a real fill.
      ggsave(file, build(), width = 12.5, height = 9.5, dpi = 200,
             device = ragg::agg_png, limitsize = FALSE, bg = pm_bg(dark()))
    }
  )
}

shinyApp(ui, server)
