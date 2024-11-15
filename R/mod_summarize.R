#' summarize UI Function
#'
#' @description A shiny Module for the summarization of datasets.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summarize_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      .drag-container {
        display: flex;
        flex-direction: column;
        justify-content: flex-start;
        margin-bottom: 10px;
        align-items: flex-start;
      }
      .dropzone {
        padding: 5px;
        text-align: center;
        background-color: #f9f9f9;
        border-radius: 15px;
        width: 600px; /* Default width */
        height: 100px;
        margin-bottom: 5px;
        box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.15); /* Add shadow effect */
      }
      .var-dropzone {
        background-color: #b2f2b2;
      }
      .group-dropzone {
        background-color: #b2d9f2;
      }
      .stats-dropzone {
        background-color: #d1d1f0;
      }
      .draggable {
        cursor: move;
        padding: 7px;
        margin: 2px;
        background-color: #e1e1e1;
        border: 1px solid #aaa;
        display: inline-block;
        border-radius: 15px;
        transition: transform 0.2s ease, opacity 0.2s ease;
      }
      .draggable.dropped {
        transform: scale(1.3);
        opacity: 1;
      }
      .numeric-var {
        background-color: #b2f2b2; /* Light green */
        box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.15); /* Add shadow effect */
      }
      .character-var {
        background-color: #b2d9f2; /* Light blue */
        box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.15); /* Add shadow effect */
      }
      .remove-btn {
        cursor: pointer;
        color: red;
        margin-left: 10px;
        font-weight: bold;
      }
      /* Bounce Effect with Scale for dragging */
      @keyframes bounce-scale {
        0% {
          transform: scale(1.5) translateY(0);
        }
        25% {
          transform: scale(1.5) translateY(-5px);
        }
        50% {
          transform: scale(1.5) translateY(0);
        }
        75% {
          transform: scale(1.5) translateY(-5px);
        }
        100% {
          transform: scale(1.5) translateY(0);
        }
      }
      .dragging {
        animation: bounce-scale 1.5s infinite; /* Slow bounce effect */
      }
      /* Explosion Effect for removal */
      @keyframes explosion {
        0% {
          transform: scale(1);
          opacity: 1;
        }
        25% {
          transform: scale(1.5);
          opacity: 0.75;
        }
        50% {
          transform: scale(2);
          opacity: 0.5;
        }
        75% {
          transform: scale(1.5);
          opacity: 0.4;
        }
        100% {
          transform: scale(0);
          opacity: 0;
        }
      }
      .exploding {
        animation: explosion 0.3s forwards; /* Trigger explosion animation */
      }
    ")),

    # Input Data Section
    fluidRow(
      col_5(
        bs4Card(
          title = "Input data",
          collapsible = FALSE,
          width = 12,
          height = "790px",
          prettyRadioButtons(
            inputId = ns("dforshape"),
            label = "Use",
            choices = c("data.frame", "shapefile"),
            icon = icon("check"),
            bigger = TRUE,
            status = "info",
            animation = "jelly",
            inline = TRUE
          ),
          fluidRow(
            col_6(
              pickerInput(
                ns("dftosummary"),
                label = "Dataset to summarize",
                choices = NULL
              )
            ),
            col_3(
              textInput(
                ns("suffix"),
                label = "Suffix",
                value = "_summarized"
              )
            ),
            col_3(
              br(),
              actionBttn(
                ns("doneupdating"),
                label = "Save",
                icon = icon("check")
              )
            )
          ),
          # Available Variables Section
          fluidRow(
            div(style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
                h4("Available variables:"),
                div(style = "display: flex; justify-content: flex-start; margin-left: auto; width: 60%;",
                    div(style = "background-color: #b2f2b2; padding: 5px; border-radius: 15px; width: 100px; text-align: center; display: inline-block;",
                        "Numeric"),
                    div(style = "background-color: #b2d9f2; padding: 5px; border-radius: 15px; width: 100px; text-align: center; display: inline-block;",
                        "Character")
                )
            ),
            uiOutput(ns("variables_ui"))
          ),
          # Drop Zones
          fluidRow(
            div(class = "drag-container",
                div(id = ns("var_dropzone"), class = "dropzone var-dropzone", "Variables:", style = "text-align: left;"),
                div(id = ns("group_dropzone"), class = "dropzone group-dropzone", "Grouping (optional)", style = "text-align: left;")
            )
          ),
          # Statistics Section
          fluidRow(
            h4("Statistics:"),
            uiOutput(ns("stats_ui"))
          ),
          fluidRow(
            div(class = "drag-container",
                div(id = ns("stats_dropzone"), class = "dropzone stats-dropzone", "Stats:", style = "text-align: left;")
            )
          )
        )
      ),
      # Summarized Data Display
      col_7(
        bs4Card(
          title = "Summarized data",
          collapsible = FALSE,
          width = 12,
          height = "790px",
          reactable::reactableOutput(ns("summary_table"), height = "750px") |> add_spinner()
        )
      )
    ),

    # JavaScript for Drag-and-Drop Animation and Interaction
    tags$script(HTML(paste0("
      var dragged;
      document.addEventListener('dragstart', function(event) {
        dragged = event.target;
        dragged.classList.add('dragging');  // Add bounce-scale effect when dragging
      });

      document.addEventListener('dragend', function(event) {
        dragged.classList.remove('dragging');  // Remove bounce-scale effect when dragging stops
      });

      // Prevent default behavior (Prevent file from being opened)
      document.addEventListener('dragover', function(event) {
        event.preventDefault();
      });

      // Handle the drop event
      document.addEventListener('drop', function(event) {
        event.preventDefault();
        var zoneId = event.target.id;

        if (zoneId === '", ns("var_dropzone"), "' && dragged.classList.contains('numeric-item')) {
          var newItem = document.createElement('div');
          newItem.className = 'draggable';
          newItem.innerHTML = dragged.innerText + '<span class=\"remove-btn\">&#10005;</span>';
          newItem.setAttribute('draggable', 'false');
          event.target.appendChild(newItem);

          Shiny.setInputValue('", ns("dropped_item"), "', {
            zone: event.target.id,
            value: dragged.innerText
          });
        } else if (zoneId === '", ns("group_dropzone"), "') {
          var newItem = document.createElement('div');
          newItem.className = 'draggable';
          newItem.innerHTML = dragged.innerText + '<span class=\"remove-btn\">&#10005;</span>';
          newItem.setAttribute('draggable', 'false');
          event.target.appendChild(newItem);

          Shiny.setInputValue('", ns("dropped_item"), "', {
            zone: event.target.id,
            value: dragged.innerText
          });
        } else if (zoneId === '", ns("stats_dropzone"), "' && dragged.classList.contains('stat-item')) {
          var newItem = document.createElement('div');
          newItem.className = 'draggable';
          newItem.innerHTML = dragged.innerText + '<span class=\"remove-btn\">&#10005;</span>';
          newItem.setAttribute('draggable', 'false');
          event.target.appendChild(newItem);

          Shiny.setInputValue('", ns("dropped_item"), "', {
            zone: event.target.id,
            value: dragged.innerText
          });
        }
      });

      // Handle removal of items with explosion effect
      document.addEventListener('click', function(event) {
        if (event.target.classList.contains('remove-btn')) {
          var parent = event.target.parentElement;
          var value = parent.innerText.replace('✕', '').trim();
          var zoneId = parent.parentElement.id;

          parent.classList.add('exploding');
          setTimeout(function() {
            parent.remove();  // Remove element after explosion animation
            Shiny.setInputValue('", ns("removed_item"), "', {
              zone: zoneId,
              value: value
            });
          }, 500);
        }
      });
    ")))
  )
}

#' summarize Server Functions
#'
#' @noRd
mod_summarize_server <- function(id, dfs, shapefile){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    vars <- reactiveVal(c())
    group_vars <- reactiveVal(c())
    stats <- reactiveVal(c())
    availablevars <- reactiveVal(c())
    availablestats <- reactiveVal(c("mean", "median", "min", "max", "sum", "sd", "cv", "n"))

    observe({
      if(input$dforshape == "data.frame"){
        updatePickerInput(session, "dftosummary",
                          choices = c("none", names(dfs)))
      } else{
        updatePickerInput(session, "dftosummary",
                          choices = c("none", setdiff(names(shapefile), c("shapefileplot"))))
      }
    })

    dfactive <- reactiveValues()
    observeEvent(input$dftosummary, {
      # Clear reactive values when dataset changes
      vars(c())
      group_vars(c())
      stats(c())

      # Clear the UI drop zones
      removeUI(selector = paste0("#", ns("var_dropzone"), " > .draggable"), immediate = TRUE)
      removeUI(selector = paste0("#", ns("group_dropzone"), " > .draggable"), immediate = TRUE)
      removeUI(selector = paste0("#", ns("stats_dropzone"), " > .draggable"), immediate = TRUE)

      # Handle case when dftosummary is set to 'none'
      if (input$dftosummary == "none") {
        availablevars(c())
        dfactive$df <- NULL  # Reset the active dataframe
      } else {
        req(input$dftosummary)
        if (input$dforshape == "data.frame") {
          dfactive$df <- dfs[[input$dftosummary]]$data |> convert_numeric_cols()
        } else {
          dfactive$df <- shapefile[[input$dftosummary]]$data |> convert_numeric_cols()
        }
      }
    })


    # Dynamically render the list of statistics with renderUI
    output$stats_ui <- renderUI({
      req(availablestats())  # Ensure stats are available before rendering
      div(id = ns("statistics"),
          lapply(availablestats(), function(stat) {
            div(class = "draggable stat-item", stat, draggable = "true", style = "background-color: #d1d1f0")  # Add a class 'stat-item' to identify stats
          })
      )
    })

    # Add dragged item
    observeEvent(input$dropped_item, {
      zone <- input$dropped_item$zone
      value <- input$dropped_item$value
      if (zone == ns("var_dropzone")) {
        vars(unique(c(vars(), value)))
      } else if (zone == ns("group_dropzone")) {
        group_vars(unique(c(group_vars(), value)))
      } else if (zone == ns("stats_dropzone")) {
        stats(unique(c(stats(), value)))
        availablestats(setdiff(availablestats(), value))  # Remove dropped stat from available list
      }
    })



    observe({
      req(dfactive$df)
      availablevars(setdiff(names(dfactive$df), c(vars(), group_vars())))
    })


    # Dynamically render the list of variables with renderUI
    output$variables_ui <- renderUI({

      div(id = ns("variables"),
          lapply(availablevars(), function(var) {
            # Classify the variables (numeric, character, no special class for binary)
            var_class <- switch(class(dfactive$df[[var]])[1],
                                "numeric" = "numeric",
                                "factor" = "character",
                                "character" = "character",
                                "unknown")

            # Only add the class for numeric variables
            color_class <- switch(var_class,
                                  "numeric" = "numeric-var numeric-item",  # Use color and add 'numeric-item' class for numeric
                                  "character" = "character-var",  # Use color for character
                                  NULL)  # No color or special class for binary

            div(class = paste("draggable", color_class), var, draggable = "true")
          })
      )
    })

    # Remove item
    observeEvent(input$removed_item, {
      zone <- input$removed_item$zone
      value <- input$removed_item$value

      if (zone == ns("var_dropzone")) {
        vars(setdiff(vars(), value))
      } else if (zone == ns("group_dropzone")) {
        group_vars(setdiff(group_vars(), value))
      } else if (zone == ns("stats_dropzone")) {
        stats(setdiff(stats(), value))  # Remove stat from the selected list
        availablestats(unique(c(availablestats(), value)))  # Re-add stat to available list
      }
    })



    summary_data <- reactive({
      req(vars(), stats())
      showNotification(
        ui = "Summarizing the data... Please, wait!",
        type = "message",
        duration = NULL,   # Infinite duration until manually removed
        id = "summarydata"
      )
      # Selected variables and statistics
      selected_vars <- vars()
      selected_stats <- stats()
      selected_group <- group_vars()
      # Chosen statistical functions
      stat_fns <- custom_stats(dfactive$df, selected_stats)
      # Calculate the summary with or without grouping
      if (length(selected_group) == 0) {
        summary_data <-
          dfactive$df |>
          dplyr::summarise(dplyr::across(all_of(selected_vars), stat_fns, .names = "{col}_{fn}"), .groups = "drop")
      } else {
        summary_data <- dfactive$df |>
          dplyr::group_by(dplyr::across(dplyr::all_of(selected_group))) |>
          dplyr::summarise(dplyr::across(dplyr::all_of(selected_vars), stat_fns, .names = "{col}_{fn}", .groups = "drop"))
      }
      removeNotification(id = "summarydata")
      summary_data
    })



    # Render summary table
    output$summary_table <- reactable::renderReactable({
      render_reactable(summary_data())
    })

    observeEvent(input$doneupdating, {
      newfile <- paste0(file_name(input$dftosummary), input$suffix)
      if(input$dforshape == "data.frame"){
        dfs[[newfile]] <- create_reactval(newfile, summary_data())
      } else{
        shapefile[[newfile]] <-  create_reactval(newfile, summary_data())
      }
      sendSweetAlert(
        session = session,
        title = "You have now a summarized data!",
        text = "The dataset has been successfully summarized and is now available for further analysis or download.",
        type = "success"
      )
    })

  })
}

## To be copied in the UI
# mod_summarize_ui("summarize_1")

## To be copied in the server
# mod_summarize_server("summarize_1")
