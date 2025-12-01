#' manipulashp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_manipulashp_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bs4TabCard(
      id = "tabs",
      status = "success",
      width = 12,
      title = "",
      selected = "Geometry operations",
      solidHeader = FALSE,
      type = "tabs",
      icon = icon("screwdriver-wrench"),
      tabPanel(
        title = "Geometry operations",
        icon = icon("map"),
        mod_spatjoin_ui("spatjoin_1")
      ),
      tabPanel(
        title = "Shapefile Transformation",
        icon = icon("draw-polygon"),
        mod_shapefiletransform_ui("shapefiletransform_1")
      ),
      tabPanel(
        title = "Vectorize",
        icon = icon("vector-square"),
        mod_vectorize_ui("vectorize_1")
      )
    )
  )
}

#' manipulashp Server Functions
#'
#' @noRd
mod_manipulashp_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_manipulashp_ui("manipulashp_1")

## To be copied in the server
# mod_manipulashp_server("manipulashp_1")
