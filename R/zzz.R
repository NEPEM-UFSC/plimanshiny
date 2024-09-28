#' @importFrom utils  globalVariables
.onAttach <- function(libname, pkgname) {
  file <- readLines(system.file("app/www/version.json", package = "plimanshiny", mustWork = TRUE))
  versao <- sub('.*"version": *"(.*)".*', '\\1', grep('"version"', file, value = TRUE))
  packageStartupMessage("|============================================================|")
  packageStartupMessage("| Welcome to the plimanShiny package!                        |")
  packageStartupMessage("| A Shiny App for the pliman package (version ", versao, ")        |")
  packageStartupMessage("| Developed collaboratively by NEPEM - nepemufsc.com         |")
  packageStartupMessage("| Group lead: Prof. Tiago Olivoto                            |")
  packageStartupMessage("| For citation: type `citation('plimanshiny')`               |")
  packageStartupMessage("| We welcome your feedback and suggestions!                  |")
  packageStartupMessage("|============================================================|")

}

if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c("geometry", "name", "value", "plot_id", "block", "individual", "ind", "coverage_fraction", "cl",
      "label", "symptomatic", "vals", "unique_id", "unique_plot", "plot_layout", "edit_id",
      "model_results", "parms", "x", "y", "vindex", "parms", "doy", "flights", "q90", "volume")
  )
}
