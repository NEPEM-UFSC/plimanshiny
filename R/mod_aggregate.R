#' aggregate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_aggregate_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Mask Settings",
          collapsible = FALSE,
          width = 12,
          height = "620px",
          h3("Input"),
          selectInput(ns("mosaic_to_aggr"),
                      label = "Mosaic to aggregate",
                      choices = NULL),
          selectInput(ns("aggregatefun"),
                      label = "Resampling function",
                      choices = c('nearest', 'average', 'rms', 'bilinear', 'cubic', 'cubicspline', 'lanczos', 'mode'),
                      selected = "nearest"),
          numericInput(ns("aggregatefct"),
                       label = "Fraction of input raster",
                       value = 50),
          hl(),
          h3("Output"),
          textInput(ns("new_aggr"),
                    label = "New object",
                    value = NULL),
          col_6(
            actionBttn(ns("aggregate"),
                       label = "Aggregate!",
                       style = "pill",
                       no_outline = FALSE,
                       icon = icon("scissors"),
                       color = "success")
          )
        )
      ),
      col_9(
        fluidRow(
          col_6(
            h2("Original raster"),
            plimanshiny_viewer_ui(ns("mosori"), prefix = "mosori", width = 540, height = 500)
          ),
          col_6(
            h2("Aggregated raster"),
            plimanshiny_viewer_ui(ns("mosagg"), prefix = "mosagg", width = 540, height = 500)

          )
        )
      )
    )
  )
}

#' aggregate Server Functions
#'
#' @noRd
mod_aggregate_server <- function(id, mosaic_data, r, g, b, settings, zlim){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # This observer correctly updates the choices for the dropdown
    observe({
      req(mosaic_data)
      # Filter out any NULL mosaic data before populating choices
      valid_mosaics <- Filter(Negate(is.null), reactiveValuesToList(mosaic_data))
      updateSelectInput(session, "mosaic_to_aggr", choices = setdiff(names(valid_mosaics), "mosaic"))
    })

    # This observer updates the default name for the new object
    observe({
      req(input$mosaic_to_aggr)
      updateTextInput(session, "new_aggr", value = paste0(input$mosaic_to_aggr, "_aggregated"))
    })

    # NEW: This code is now wrapped in an observe block.
    # It will now re-run every time input$mosaic_to_aggr changes.
    observe({
      req(input$mosaic_to_aggr)
      req(mosaic_data[[input$mosaic_to_aggr]]$data)
      plimanshiny_viewer_server("mosori",
                                mosaic_data[[input$mosaic_to_aggr]]$data,
                                r = reactive({ suppressWarnings(as.numeric(r$r)) }),
                                g = reactive({ suppressWarnings(as.numeric(g$g)) }),
                                b = reactive({ suppressWarnings(as.numeric(b$b)) }),
                                usemargin = reactive({TRUE}),
                                zlim = reactive(zlim$zlim),
                                prefix = "mosori",
                                max_width = 540
      )
    })

    # The action button logic is separate and remains unchanged.
    observeEvent(input$aggregate, {
      req(input$mosaic_to_aggr, input$new_aggr)

      # Show a notification that the process has started
      show_toast("Aggregating...", "Aggregation can take a moment.", "info",  timer = 2500)

      myaggr <- mosaic_aggregate(mosaic_data[[input$mosaic_to_aggr]]$data,
                                 pct = chrv2numv(input$aggregatefct),
                                 fun = input$aggregatefun)

      plimanshiny_viewer_server("mosagg",
                                myaggr,
                                r = reactive({ suppressWarnings(as.numeric(r$r)) }),
                                g = reactive({ suppressWarnings(as.numeric(g$g)) }),
                                b = reactive({ suppressWarnings(as.numeric(b$b)) }),
                                usemargin = reactiveVal(TRUE),
                                zlim = reactive(zlim$zlim),
                                prefix = "mosagg",
                                max_width = 540
      )

      mosaic_data[[input$new_aggr]] <- create_reactval(name = input$new_aggr, data = myaggr)

      sendSweetAlert(
        session = session,
        title = "Mosaic successfully aggregated!",
        text = "The new aggregated mosaic is now available.",
        type = "success"
      )
    })
  })
}

## To be copied in the UI
# mod_aggregate_ui("aggregate_1")

## To be copied in the server
# mod_aggregate_server("aggregate_1")
