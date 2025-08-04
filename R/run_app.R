#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#' @param upload_size Shiny limits file uploads. Defaults to 10000 MB (10 GB).
#' @export
#' @importFrom golem with_golem_options
#' @importFrom plotly ggplotly renderPlotly plotlyOutput
#' @importFrom shiny shinyApp
#' @importFrom tidyr unnest
#' @importFrom graphics abline par title layout legend matplot
#' @importFrom stats binomial glm reorder runif dist D coef integrate lm nls nls.control optimise setNames smooth approx loess
#' @importFrom grDevices dev.off jpeg adjustcolor col2rgb colorRampPalette rgb png terrain.colors
#' @importFrom utils data stack write.csv
#' @importFrom future plan multisession
#' @importFrom doFuture `%dofuture%`
#' @import shinyWidgets mapview leaflet mapedit sf esquisse waiter ggplot2 shinyFiles foreach datamods reactable pak mirai
#' @rawNamespace import(bs4Dash, except = c(progressBar, insertTab, actionButton, updateTabsetPanel, column, tabsetPanel, tabPanel, navbarMenu))
#' @rawNamespace import(terra, except = c(panel, shift, distance))
#' @rawNamespace import(shinyjs, except = c(alert, runExample, click))
#' @rawNamespace import(purrr, except = c(accumulate, when))
#' @rawNamespace import(pliman, except = c(`%>%`))
run_app <- function(upload_size = 10000,
                    ...) {
  options(shiny.maxRequestSize = 10000 * 1024^2)
  terra::terraOptions(memfrac = 0.8)
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      options = list(launch.browser = TRUE)
    ),
    print = TRUE,
    golem_opts = list(...)
  )
}

#' Load PlimanShiny Results from Temporary File
#'
#' This function searches for and loads the PlimanShiny output saved in a
#' temporary file. It looks for files in the temporary directory that match the
#' pattern `"plimanshiny_output"`, loads the first match found, and makes the
#' resulting object available in the R environment.
#'
#' @return The contents of the loaded file, typically a list or other object
#'   created by `plimanshiny`.
#'
#' @details The function assumes that a file containing `plimanshiny` results was
#'   saved with a filename matching the pattern `"plimanshiny_output"`. It
#'   searches for such files in the temporary directory (`tempdir()`), loads the
#'   first match, and returns the results.
#'
#' @note This function is intended to be used after running the `plimanshiny`
#'   app, which saves results to a temporary file. You should call this function
#'   after stopping the app to retrieve the saved results.
#'
#' @seealso [tempdir()], [load()]
#'
#' @examples
#' # Assuming you have run the plimanshiny app and results are saved in a temporary file
#' library(plimanshiny)
#' results <- get_plimanshiny_results()
#'
#' @export
get_plimanshiny_results <- function() {
  files <- list.files(pattern = "plimanshiny_output", path = tempdir())
  if (length(files) == 0) {
    warning("No 'plimanshiny_output' file found in the temporary directory.")
    return(NULL)
  } else{
    file_path <- paste0(tempdir(), "/", files[[1]])

    # Capture the name(s) of the object(s) loaded
    loaded_objects <- load(file_path)

    # If there's only one object loaded, return it directly
    if (length(loaded_objects) == 1) {
      return(get(loaded_objects))
    }
    # If there are multiple objects, return a list of all of them
    result_list <- lapply(loaded_objects, get)
    names(result_list) <- loaded_objects
    return(result_list)
  }
}
