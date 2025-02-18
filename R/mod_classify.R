#' classify UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_classify_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Classification tools",
          collapsible = FALSE,
          width = 12,
          h3("Input"),
          selectInput(ns("indextoclassify"),
                      label = "Raster data (index file)",
                      choices = NULL),
          selectInput(ns("bandused"),
                      label = "Raster data (index band)",
                      choices = NULL),
          prettyCheckbox(
            inputId = ns("computefreq"),
            label = "Compute frequency table",
            value = TRUE,
            icon = icon("check"),
            status = "success",
            animation = "rotate"
          ),
          textInput(ns("breaks"),
                    label = "Breaks",
                    value = NA),
          actionBttn(ns("classify"),
                     label = "Classify!",
                     status = "success")
        )
      ),
      col_9(
        bs4TabCard(
          id = ns("tabsclass"),
          status = "success",
          width = 12,
          height = "760px",
          title = "Classification tools",
          selected = "Continuous plot",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Continuous plot",
            plotOutput(ns("cplot"), height = "660px")
          ),
          tabPanel(
            title = "Class plot",
            plotOutput(ns("classplot"), height = "660px")
          ),
          tabPanel(
            title = "Frequency table",
            reactable::reactableOutput(ns("frequency"), height = "660px")
          )
        )
      )
    )
  )
}

#' classify Server Functions
#'
#' @noRd
mod_classify_server <- function(id, mosaic_data, index, dfs){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    tmplist <- reactiveVal()
    observe({
      tmplist1 <-
        lapply(reactiveValuesToList(mosaic_data), function(x){
          reactiveValuesToList(x)$data
        })
      tmplist2 <-
        lapply(reactiveValuesToList(index), function(x){
          reactiveValuesToList(x)$data
        })
      tmplist(c(tmplist1, tmplist2))
    })


    observe({
      req(tmplist())
      updateSelectInput(session, "indextoclassify", choices = names(tmplist()))
    })
    observe({
      req(input$indextoclassify)
      updateSelectInput(session, "bandused", choices = names(tmplist()[[input$indextoclassify]]), selected = NA)
    })

    output$cplot <- renderPlot({
      req(input$indextoclassify, input$bandused)
      terra::plot(tmplist()[[input$indextoclassify]][[input$bandused]],
                  col = pliman::custom_palette(c("darkred",  "yellow", "darkgreen"), n = 100),
                  maxcell = 7e5,
                  smooth = TRUE)
    })


    observeEvent(input$classify, {
      classresults <-
        mosaic_classify(
          mosaic = tmplist()[[input$indextoclassify]][[input$bandused]],
          breaks = as.numeric(strsplit(input$breaks, ",")[[1]]),
          frequency = input$computefreq,
          plot = FALSE
        )

      output$classplot <- renderPlot({
        terra::plot(classresults$classified,
                    col = pliman::custom_palette(c("darkred",  "yellow", "darkgreen"), n = 100),
                    maxcell = 7e5,
                    smooth = TRUE)

      })


      if(input$computefreq){
        freqdata <-
          classresults$class_freq |>
          dplyr::select(-1) |>
          setNames(c("Class", "Pixels", "Area (ha)", "Proportion"))

        output$frequency <- reactable::renderReactable({
          req(freqdata)
          render_reactable(freqdata)
        })
        dfs[["class_results"]] <- create_reactval("class_results", freqdata)
      }

      mosaic_data[[paste0(input$indextoclassify, "_classified")]] <- create_reactval(paste0(input$indextoclassify, "_classified"), classresults$classified)

      updateTabsetPanel(session, "tabsclass", selected = "Class plot")

      sendSweetAlert(
        session = session,
        title = "Done!",
        text = "The mosaic has been successfully classified.",
        type = "success"
      )

    })
  })
}

## To be copied in the UI
# mod_classify_ui("classify_1")

## To be copied in the server
# mod_classify_server("classify_1")
