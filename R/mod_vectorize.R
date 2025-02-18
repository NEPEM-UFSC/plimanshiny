#' vectorize UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_vectorize_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Segmentation Settings",
          collapsible = FALSE,
          width = 12,
          h3("Input"),
          selectInput(
            ns("masktovectorize"),
            label = "Mask to be vectorized",
            choices = NULL
          ),
          hl(),
          h3("Vectorization Options"),
          numericInput(
            inputId = ns("aggregate"),
            label = "Relative size (%)",
            value = 100
          ),
          prettyCheckbox(
            inputId = ns("watershed"),
            label = "Watershed",
            value = FALSE,
            icon = icon("check"),
            status = "success",
            animation = "rotate"
          ),
          conditionalPanel(
            condition = "input.watershed == true", ns = ns,
            fluidRow(
              col_6(
                numericInput(ns("tolerance"),
                             label = "Tolerance",
                             value = NULL)
              ),
              col_6(
                numericInput(ns("extension"),
                             label = "Extension",
                             value = NULL)

              )
            )
          ),
          prettyCheckbox(
            inputId = ns("fillhull"),
            label = "Fill Hull",
            value = TRUE,
            icon = icon("check"),
            status = "success",
            animation = "rotate"
          ),
          fluidRow(
            col_4(
              numericInput(
                inputId = ns("opening"),
                label = "Opening",
                value = 0
              )
            ),
            col_4(
              numericInput(
                inputId = ns("closing"),
                label = "Closing",
                value = 0
              )
            ),
            col_4(
              numericInput(
                inputId = ns("filter"),
                label = "Median filter",
                value = 0
              )
            )
          ),
          fluidRow(
            col_6(
              numericInput(
                inputId = ns("erode"),
                label = "Erode",
                value = 0
              )
            ),
            col_6(
              numericInput(
                inputId = ns("dilate"),
                label = "Dilate",
                value = 0
              )
            )
          ),
          fluidRow(
            col_6(
              numericInput(
                inputId = ns("lower_size"),
                label = "Lower size",
                value = NA
              )
            ),
            col_6(
              numericInput(
                inputId =ns("upper_size"),
                label = "Upper size",
                value = NA
              )
            )
          ),
          fluidRow(
            col_6(
              numericInput(
                inputId = ns("topn_lower"),
                label = "Top N lower",
                value = NA
              )
            ),
            col_6(
              numericInput(
                inputId =ns("topn_upper"),
                label = "Top N upper",
                value = NA
              )
            )
          ),
          hl(),
          h3("Output"),
          textInput(
            ns("vector"),
            label = "Vector name",
            value = NULL
          ),
          fluidRow(
            col_6(
              actionBttn(ns("vectorize"),
                         label = "Vectorize!",
                         style = "pill",
                         color = "success")
            )
          )
        )
      ),
      col_9(
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "720px",
          title = "Vectorization",
          selected = "Mask",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Mask",
            plotOutput(ns("mask"), height = "680px") |> add_spinner()
          ),
          tabPanel(
            title = "Vector",
            prettyCheckbox(
              inputId = ns("usebm"),
              label = "Plot basemap",
              value = FALSE,
              icon = icon("check"),
              status = "success",
              animation = "rotate"
            ),
            leafletOutput(ns("mosaiccropped"), height = "640px") |> add_spinner()
          )
        )
      )
    )
  )
}

#' vectorize Server Functions
#'
#' @noRd
mod_vectorize_server <- function(id, mosaic_data, shapefile, basemap){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    observe({
      req(mosaic_data)
      updateSelectInput(session, "masktovectorize", choices = c("Active mosaic", setdiff(names(mosaic_data), "mosaic")), selected = NULL)
    })
    observe({
      updateTextInput(session, "vector", value = paste0(input$masktovectorize, "_vectorized"))
    })
    # Reactive expression to store the cropped mosaic
    segmented_mosaic <- reactiveVal(NULL)
    observe({
      req(input$masktovectorize)
      if(input$masktovectorize == "Active mosaic"){
        req(mosaic_data$mosaic$data)
        segmented_mosaic(mosaic_data$mosaic$data)
      } else{
        req(mosaic_data[[input$masktovectorize]]$data)
        segmented_mosaic(mosaic_data[[input$masktovectorize]]$data)
      }
    })

    output$mask <- renderPlot({
      req(segmented_mosaic())
      mosaic_plot(segmented_mosaic())
    })

    # Observe event for mosaic crop action
    observeEvent(input$vectorize, {
      req(segmented_mosaic())
      if(is.na(input$lower_size)){
        lower_size <- NULL
      } else{
        lower_size <- input$lower_size
      }
      # For upper_size
      if (is.na(input$upper_size)) {
        upper_size <- NULL
      } else {
        upper_size <- input$upper_size
      }

      # For topn_lower
      if (is.na(input$topn_lower)) {
        topn_lower <- NULL
      } else {
        topn_lower <- input$topn_lower
      }

      if (is.na(input$topn_upper)) {
        topn_upper <- NULL
      } else {
        topn_upper <- input$topn_upper
      }
      waiter_show(
        html = tagList(
          spin_google(),
          h2("Vectorizing the mask. Please, wait.")
        ),
        color = "#228B227F"
      )
      if(input$aggregate == 100){
        agg <- NULL
      } else{
        agg <- input$aggregate
      }
      vectorized <- mosaic_vectorize(segmented_mosaic(),
                                     aggregate = agg,
                                     watershed = input$watershed,
                                     tolerance = input$tolerance,
                                     extension = input$extension,
                                     erode = input$erode,
                                     dilate = input$dilate,
                                     opening = input$opening,
                                     closing = input$closing,
                                     filter = input$filter,
                                     fill_hull = input$fillhull,
                                     topn_lower = topn_lower,
                                     topn_upper = topn_upper,
                                     lower_size = lower_size,
                                     upper_size = upper_size,
                                     smooth = 5) |>
        check_cols_shpinp() |>
        dplyr::relocate(block, plot_id, row, column, .after = unique_id)

      output$mosaiccropped <- renderLeaflet({
        req(vectorized)
        if(input$usebm){
          (basemap$map + shapefile_view(vectorized))@map
        } else{
          shapefile_view(vectorized)@map
        }
      })

      waiter_hide()
      # Update mosaic_data$mosaic$data when input$cropmosaic is clicked
      shapefile[[input$vector]] <- create_reactval(name = input$vector, data = vectorized)
      sendSweetAlert(
        session = session,
        title = "Mask successfylly vectorized!!",
        text = "The mask has been successfully vectorized and the shapefile is now available for further analysis.",
        type = "success"
      )
    })

  })
}

## To be copied in the UI
# mod_vectorize_ui("vectorize_1")

## To be copied in the server
# mod_vectorize_server("vectorize_1")
