#' geeapps UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_geeapps_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$script(src = "https://cdn.tailwindcss.com"),
      tags$style(HTML("
        iframe {
          width: 80vw;
          height: 80vh;
          border: none;
          border-radius: 12px;
          box-shadow: 0 10px 20px rgba(0,0,0,0.1);
        }
        .gee-title {
          font-size: 1.25rem;
          font-weight: 600;
          margin-bottom: 10px;
        }
        .gee-disclaimer {
          font-size: 0.75rem;
          color: #6b7280;
          font-style: italic;
          margin-top: 0.75rem;
        }
        .gee-link {
          font-size: 0.75rem;
          color: #2563eb;
          margin-top: 0.25rem;
        }
        .gee-link a {
          text-decoration: underline;
        }
        .gee-link a:hover {
          color: #1e40af;
        }
      "))
    ),
    fluidRow(
      col_6(
        div(
          class = "gee-container",
          div(class = "gee-title", "Select the App"),
          selectInput(
            ns("selected_app"),
            label = NULL,
            choices = NULL,
            width = "100%"
          ),
        )
      ),
      col_6(
        div(
          class = "gee-disclaimer",
          tags$b("Disclaimer:"),
          " plimanshiny only embeds these applications.",
          " All credit for these apps belongs exclusively to their original authors."
        ),
        uiOutput(ns("gee_link"))
      )
    ),
    fluidRow(
      uiOutput(ns("gee_iframe"))
    )
  )
}

#' geeapps Server Functions
#'
#' @noRd
mod_geeapps_server <- function(id){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    apps_list <- c(
      "Sentinel2 Imagery" = "https://luciano-ufsc.projects.earthengine.app/view/rthseries",
      "Sentinel2 Monthly View" = "https://luciano-ufsc.projects.earthengine.app/view/rthseriesmonth",
      "Sentinel2 OP Downloader" = "https://luciano-ufsc.projects.earthengine.app/view/rthseriesopdownloader",
      "Sentinel2 Vegetation Index Timeseries" = "https://luciano-ufsc.projects.earthengine.app/view/rthseriesvegtime"
    )

    # Popula o selectInput dinamicamente
    updateSelectInput(session, "selected_app",
                      choices = names(apps_list),
                      selected = names(apps_list)[1])

    # Renderiza o iframe do app selecionado
    output$gee_iframe <- renderUI({
      req(input$selected_app)
      tags$iframe(
        src = apps_list[[input$selected_app]],
        sandbox = "allow-scripts allow-same-origin allow-popups allow-forms",
        loading = "lazy"
      )
    })
    output$gee_link <- renderUI({
      req(input$selected_app)
      tags$p(
        class = "text-xs text-blue-600 mt-2",
        "Original app by ",
        tags$a(
          href = apps_list[[input$selected_app]],
          target = "_blank",
          class = "underline hover:text-blue-800",
          apps_list[[input$selected_app]]
        )
      )
    })
  })
}

## To be copied in the UI
# mod_geeapps_ui("geeapps_1")

## To be copied in the server
# mod_geeapps_server("geeapps_1")
