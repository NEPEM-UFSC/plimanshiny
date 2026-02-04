#' datasets UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_datasets_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Datasets",
          collapsible = FALSE,
          width = 12,
          actionBttn(
            ns("importdf"),
            label = "Import dataset",
            icon = icon("file-import")
          ),
          # New removal buttons
          hl(),
          strong("Manage datasets"),
          fluidRow(
              actionBttn(
                ns("removeone"),
                label = "Remove active",
                icon = icon("trash-can"),
                style = "material-flat",
                color = "danger"
              ),
              actionBttn(
                ns("removeall"),
                label = "Remove all",
                icon = icon("trash"),
                style = "material-flat",
                color = "danger"
              )
          ),
          hl(),
          pickerInput(
            ns("activedf"),
            label = "Active dataset",
            choices = NULL
          ),
          hl()
        )
      ),
      col_9(
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "760px",
          title = "Active dataset",
          selected = "Active dataset",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Active dataset",
            edit_data_ui(ns("editing"))
          )
        )
      )
    )
  )
}

#' datasets Server Functions
#'
#' @noRd
mod_datasets_server <- function(id, dfs, settings){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$importdf, {
      import_modal(
        id = ns("import"),
        from = c("env", "file", "googlesheets", "url"),
        title = "Import data"
      )
    })
    # Import data
    tmp <- import_server("import", return_class = "tbl_df")
    observe({
      req(tmp$data())
      dfs[[tmp$name()]] <- create_reactval(tmp$name(), tmp$data())
    })

    observe({
      current_dfs <- Filter(Negate(is.null), reactiveValuesToList(dfs))
      updatePickerInput(session,
                        "activedf",
                        choices = names(current_dfs))
    })


    # >> New: Remove the active dataset ----
    observeEvent(input$removeone, {
      # Require a dataset to be selected
      req(input$activedf)
      confirmSweetAlert(
        session = session,
        inputId = ns("confirm_remove_one"),
        title = "Remove active dataset?",
        text = paste0("Are you sure you want to remove the '", input$activedf, "' dataset? This action cannot be undone."),
        type = "warning",
        btn_labels = c("Cancel", "Yes, remove it!"),
        btn_colors = c("#d33", "#3085d6")
      )
    })
    # Observer for the confirmation callback
    observeEvent(input$confirm_remove_one, {
      if (isTRUE(input$confirm_remove_one)) {
        dfs[[input$activedf]] <- NULL
        # updatePickerInput(session, "activedf",
        #                   choices = names(dfs))
        show_toast("success", "Dataset removed", timer = 3000)
      }
    }, ignoreNULL = TRUE)


    # >> New: Remove all datasets ----
    observeEvent(input$removeall, {
      # Require at least one dataset to exist
      req(names(dfs))
      confirmSweetAlert(
        session = session,
        inputId = ns("confirm_remove_all"),
        title = "Remove all datasets?",
        text = "Are you sure you want to remove all datasets? This action cannot be undone.",
        type = "error",
        btn_labels = c("Cancel", "Yes, remove all!"),
        btn_colors = c("#d33", "#3085d6")
      )
    })
    # Observer for the confirmation callback
    observeEvent(input$confirm_remove_all, {
      if (isTRUE(input$confirm_remove_all)) {
        # Loop through names and NULL them out
        for(name in names(dfs)){
          dfs[[name]] <- NULL
        }
        show_toast("success", "All datasets have been removed", timer = 3000)
      }
    }, ignoreNULL = TRUE)


    # Display and edit the active dataset

    dfactive <- reactiveValues()
    observe({
      req(input$activedf)
      req(dfs[[input$activedf]]$data)
      dfactive$df <- dfs[[input$activedf]]$data
        res <- edit_data_server(
          id = "editing",
          file_name_export = input$activedf,
          data_r = reactive(dfs[[input$activedf]]$data),
          update = FALSE,
          delete = FALSE,
          add = FALSE,
          reactable_options = list(height = "680px",
                                   filterable = TRUE,
                                   searchable = TRUE,
                                   striped = TRUE,
                                   pagination = TRUE,
                                   defaultPageSize = 13)
        )

    })
  })
}

## To be copied in the UI
# mod_datasets_ui("datasets_1")

## To be copied in the server
# mod_datasets_server("datasets_1")
