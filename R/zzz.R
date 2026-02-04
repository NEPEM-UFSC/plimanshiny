#' @importFrom utils  globalVariables
.onAttach <- function(libname, pkgname) {
  file <- readLines(system.file("app/www/version.json", package = "plimanshiny", mustWork = TRUE))
  versao <- sub('.*"version": *"(.*)".*', '\\1', grep('"version"', file, value = TRUE))
  packageStartupMessage("|============================================================|")
  packageStartupMessage("| Welcome to the plimanshiny!                                |")
  packageStartupMessage("| A Shiny App for the pliman package (version ", versao, ")        |")
  packageStartupMessage("| Developed collaboratively by NEPEM - nepemufsc.com         |")
  packageStartupMessage("| Group lead: Prof. Tiago Olivoto                            |")
  packageStartupMessage("| For citation: type `citation('plimanshiny')`               |")
  packageStartupMessage("| We welcome your feedback and suggestions!                  |")
  packageStartupMessage("|============================================================|")

}

if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "CH_NC","CH_Utah","CH_Utah_accum","DATE","DFS","DOY","DY","ENV","FRUE","Freq","GDD","HR","ID",
      "LAT","LON","MO","RTA","T2M","T2M_MAX","T2M_MIN","YEAR","YYYYMMDD","abbrev_state","across",
      "aic","all_of","as","as.formula","b","bandx","bandy","ch_w","classes","color","count",
      "coverage","cv","delete","env","fr","g","index","lat","lon","maturity","mod","model",
      "name_muni","plot_area","q95","r","row_id","wavelength","where","xcut",
      "block","coverage_fraction","doy","edit_id","flights","geometry","ind","individual",
      "label","name","parms","symptomatic","unique_id","unique_plot","vals","value",
      "vindex","volume","x","y"
    )
  )
}
