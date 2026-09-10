# ---------------------------------------------------------------------------
# Checks what this project needs. Reports only -- it never installs anything.
# ---------------------------------------------------------------------------
required <- c("ggplot2", "shiny", "bslib", "DT", "patchwork", "scales", "grid", "ragg")
optional <- c("ggrepel")   # nicer label placement than the built-in repel

have <- function(p) requireNamespace(p, quietly = TRUE)

cat("R", paste(R.version$major, R.version$minor, sep = "."), "\n\n")
cat("Required\n")
for (p in required)
  cat(sprintf("  [%s] %-10s %s\n", if (have(p)) "x" else " ", p,
              if (have(p)) as.character(utils::packageVersion(p)) else "MISSING"))

cat("\nOptional\n")
for (p in optional)
  cat(sprintf("  [%s] %-10s %s\n", if (have(p)) "x" else " ", p,
              if (have(p)) as.character(utils::packageVersion(p)) else "not installed"))

missing <- required[!vapply(required, have, logical(1))]
if (length(missing)) {
  cat("\nTo install what is missing, run:\n")
  cat(sprintf('  install.packages(c(%s))\n', paste0('"', missing, '"', collapse = ", ")))
} else {
  cat("\nEverything required is present. Run:\n")
  cat("  source(\"run_static.R\")   # static map -> Plots pane + outputs/\n")
  cat("  shiny::runApp()          # interactive app\n")
}

if (getRversion() < "4.1.0")
  cat("\nNote: this project uses the native |> pipe, which needs R >= 4.1.\n")
