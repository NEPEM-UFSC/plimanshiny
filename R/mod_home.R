#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bs4Dash bs4Card valueBox
#' @importFrom shinyWidgets actionBttn
mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 10,
        bs4Card(
          title = glue::glue("{plimanshiny} version [packageVersion('plimanshiny')]", .open = "[", .close = "]"),
          width = 12,
          solidHeader = FALSE,
          img(src = "www/plimanshiny.png", width = "100%", height = "80%")
        )
      ),
      column(
        width = 2,
        bs4Dash::valueBox(
          value = "MAIN REFERENCE",
          subtitle = "Olivoto, T. 2022. Lights, camera, pliman! An R package for plant image analysis. Methods in Ecology and Evolution 13(4): 789–7898. doi: 10.1111/2041-210X.13803.",
          width = 12,
          color = "success",
          elevation = 3,
          href = "https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13803",
          icon = shiny::icon("braille")
        ),
        bs4Dash::valueBox(
          value = "PAPER TROPICAL PLANT PATHOLOGY",
          subtitle = "Olivoto, T., S.M.P. Andrade, and E.M. Del Ponte. 2022. Measuring plant disease severity in R: introducing and evaluating the pliman package. Tropical Plant Pathology 1: 1–710. doi: 10.1007/S40858-021-00487-5.",
          width = 12,
          color = "success",
          elevation = 3,
          href = "https://link.springer.com/article/10.1007/s40858-021-00487-5",
          icon = shiny::icon("braille")
        ),
        fluidRow(
          actionBttn(
            inputId = ns("about"),
            label = "About",
            color = "success",
            icon = icon("circle-info")
          )
        ),
        fluidRow(
          actionBttn(
            inputId = ns("reload"),
            label = "Reload",
            color = "warning",
            icon = icon("rotate-right")
          )
        ),
        fluidRow(
          actionBttn(
            inputId = ns("nepem"),
            label = "NEPEM",
            color = "success",
            icon = icon("internet-explorer")
          )
        ),

        tags$head(
          tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
          tags$style(HTML("
            .modal-content {
              box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
            }
          "))
        )
      )
    )
  )
}

#' home Server Functions
#'
#' @noRd
#' @import shiny
#' @import waiter
#' @importFrom utils packageVersion browseURL
mod_home_server <- function(id, settings){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    json_file_path <- system.file("app/www/version.json", package = "plimanshiny", mustWork = TRUE)
    addResourcePath('myjson', dirname(json_file_path))



    # Reload app when reload button is clicked
    observeEvent(input$reload, {
      session$reload()
    })

    # Reload the app after successful installation
    observeEvent(input$reload_app, {
      session$reload()
    })

    observeEvent(input$nepem, {
      browseURL("https://nepemufsc.com/")
    })

    observeEvent(input$close_modal, {
      inst_dir <- file.path(system.file(package = "plimanshiny"), "app/www")
      licenseread <- file.path(inst_dir, "license.rds")
      saveRDS(list(licenseread = TRUE), licenseread)
      removeModal()
    })


    observeEvent(input$about, {
      showModal(
        modalDialog(
          title = glue::glue("Version: {packageVersion('plimanshiny')}"),
          fluidRow(
            col_7(
              img(src = "www/help_logo.png", width = "100%", height = "100%"),
            ),
            col_5(
              box(
                width = 12,
                headerBorder = FALSE,
                collapsible = TRUE,
                closable = TRUE,
                h2("About"),
                "{plimanshiny} provides an interactive Shiny-based graphical user interface for the pliman package,
                 facilitating user-friendly access to advanced plant image analysis tools without the need
                 for extensive programming knowledge. This package integrates a variety of functionalities
                 for high-throughput phenotyping, including but not limited to orthomosaic analysis from drone
                 and satellite imagery, shapefile creation and handling, time series analysis, image analysis,
                 and phytopathometry, into a cohesive and intuitive application.", br(),br(),
                h2("Developer"),
                a("Prof. Dr. Tiago Olivoto", href = "https://olivoto.netlify.app/", target = "_blank"), br(),
                "Department of Plant Science", br(),
                "Federal University of Santa Catarina", br(), br(),
                h2("Contribution"),
                a("Matheus Lopes Machado", href = "https://www.linkedin.com/in/matheus-lopesma/", target = "_blank"), br(),
                a("Dr. Leonardo Volpato", href = "https://www.linkedin.com/in/leonardo-volpato/", target = "_blank"), br(),
                a("Dr. Arthur Bernardeli", href = "https://www.linkedin.com/in/arthur-bernardeli-5a1a0b5a/", target = "_blank"), br(),br(),
                fluidRow(
                  col_6(
                    shiny::actionButton(inputId= ns("gitpliman"),
                                        label="pliman",
                                        icon = icon("github"),
                                        onclick ="window.open('https://github.com/NEPEM-UFSC/pliman', '_blank')"),
                  ),
                  col_6(
                    shiny::actionButton(inputId= ns("gitplimanshiny"),
                                        label="plimanshiny",
                                        icon = icon("github"),
                                        onclick ="window.open('https://github.com/NEPEM-UFSC/plimanshiny', '_blank')"),
                  )
                )
              )
            )
          ),


          footer = NULL,
          easyClose = TRUE,
          size = "xl"
        )
      )

    })

  })
}

## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")
