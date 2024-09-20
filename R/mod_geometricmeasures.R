#' geometricmeasures UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_geometricmeasures_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Geometric Measurements",
          collapsible = FALSE,
          width = 12,
          height = "760px",
          pickerInput(
            ns("dftofilter"),
            label = "Shapefile",
            choices = NULL
          ),
          textInput(
            ns("suffix"),
            label = "Suffix",
            value = "_measures"
          ),
          actionBttn(
            ns("donefiltering"),
            label = "Compute",
            icon = icon("check")
          )
        )
      ),
      col_9(
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "760px",
          title = "Computed measures",
          selected = "Computed measures",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Computed measures",
            reactable::reactableOutput(ns("measures"), height = "720px")

          )
        )
      )
    )
  )
}

#' geometricmeasures Server Functions
#'
#' @noRd
mod_geometricmeasures_server <- function(id, shapefile, settings){
  moduleServer( id, function(input, output, session){
    observe({
      updatePickerInput(session, "dftofilter",
                        choices = c("none", setdiff(names(shapefile), c("shapefile", "shapefileplot"))))
    })

    ns <- session$ns
    dfactive <- reactiveValues()

    observeEvent(input$dftofilter, {
      req(input$dftofilter)
      if(input$dftofilter != "none"){
        dfactive$df <- shapefile[[input$dftofilter]]$data |> convert_numeric_cols()
      }
    })
    resultss <- reactiveValues(shp = NULL)

    observe({
      req(dfactive$df)
      coords <-
        dfactive$df |>
        sf::st_centroid() |>
        sf::st_coordinates()

      shp <-
        dfactive$df |>
        dplyr::mutate(
          xcoord = coords[, 1],
          yooord = coords[, 2],
          area = sf::st_area(dfactive$df),
          perimeter = sf::st_perimeter(dfactive$df),
        )
      req(shp)
      output$measures <- renderReactable({
        render_reactable(shp)
      })
      resultss$shp <- shp

    })

    observeEvent(input$donefiltering, {
      newfile <- add_suffix(input$dftofilter, input$suffix)


      shapefile[[newfile]] <- create_reactval(newfile, resultss$shp)


      sendSweetAlert(
        session = session,
        title = "Geometric measurements computed",
        text = "The new shapefile is now available for use in the 'Shapefile' module.",
        type = "success"
      )
    })



  })
}

## To be copied in the UI
# mod_geometricmeasures_ui("geometricmeasures_1")

## To be copied in the server
# mod_geometricmeasures_server("geometricmeasures_1")
