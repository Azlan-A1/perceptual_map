# ---------------------------------------------------------------------------
# Static perceptual map. In RStudio: source() this file.
# The plot appears in the Plots pane and is written to outputs/.
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(ggplot2); library(grid); library(patchwork); library(scales)
})
for (f in c("silhouettes", "pca", "layout", "plot")) source(file.path("R", paste0(f, ".R")))

cars <- read.csv("data/cars.csv", stringsAsFactors = FALSE)
m    <- fit_map(cars)

cat(sprintf("%d cars | %d attributes | PC1 %.1f%%  PC2 %.1f%%  (%.1f%% total)\n",
            nrow(cars), length(m$vars), m$ve[1], m$ve[2], m$ve[1] + m$ve[2]))
cat("  PC1 =", axis_name(m$loadings, "PC1"), "\n")
cat("  PC2 =", axis_name(m$loadings, "PC2"), "\n")

DEV <- c(12.5, 9.5)
p <- compose_map(m, car_pt = 40, dev_in = DEV,
                 quads = c(tr = "BIG & POWERFUL", tl = "SMALL & QUICK",
                           bl = "VALUE & ECONOMY", br = "SIZE & UTILITY"))

dir.create("outputs", showWarnings = FALSE)
ggsave("outputs/map.png", p, width = DEV[1], height = DEV[2], dpi = 200,
       device = ragg::agg_png, limitsize = FALSE)
# cairo_pdf gives proper UTF-8 text but needs XQuartz on macOS. capabilities()
# can report TRUE while the DLL still fails to load at draw time, and that
# failure is a WARNING, not an error -- so verify the file actually landed
# rather than trusting the call to have worked.
save_pdf <- function(file, plot) {
  try_dev <- function(dev) {
    unlink(file)
    suppressWarnings(try(
      ggsave(file, plot, width = DEV[1], height = DEV[2], device = dev, limitsize = FALSE),
      silent = TRUE))
    file.exists(file) && file.size(file) > 1000
  }
  if (try_dev(grDevices::cairo_pdf)) return(invisible(TRUE))
  if (try_dev(NULL)) {
    message("note: cairo unavailable - PDF written with the base device, which ",
            "substitutes ASCII for the em-dash and arrow glyphs. Install XQuartz ",
            "(https://www.xquartz.org) if you need exact PDF typography.")
    return(invisible(TRUE))
  }
  warning("could not write ", file)
  invisible(FALSE)
}
save_pdf("outputs/map.pdf", p)

p_dark <- compose_map(m, car_pt = 40, dev_in = DEV, dark = TRUE,
                      quads = c(tr = "BIG & POWERFUL", tl = "SMALL & QUICK",
                                bl = "VALUE & ECONOMY", br = "SIZE & UTILITY"))
ggsave("outputs/map-dark.png", p_dark, width = DEV[1], height = DEV[2], dpi = 200,
       device = ragg::agg_png, limitsize = FALSE)

cat("\nWrote outputs/map.png, outputs/map.pdf, outputs/map-dark.png\n")
if (interactive()) print(p)   # <- Plots pane
