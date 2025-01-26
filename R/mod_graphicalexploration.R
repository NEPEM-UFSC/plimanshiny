#' graphicalexploration UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_graphicalexploration_ui <- function(id) {
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
        background-color: #b2d9f2;
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
            inputId = ns("dforshape2"),
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
                ns("dftoexplore"),
                label = "Dataset to explore",
                choices = NULL
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
            h4("Available variables:"),
            uiOutput(ns("variables_ui"))
          ),
          # Drop Zones
          fluidRow(
            div(class = "drag-container",
                div(id = ns("var_dropzone"), class = "dropzone var-dropzone", "Variable (Y-Axis):", style = "text-align: left;"),
                div(id = ns("group_dropzone"), class = "dropzone group-dropzone", "Grouping (X-Axis, optional):", style = "text-align: left;"),
                div(id = ns("color_dropzone"), class = "dropzone stats-dropzone", "Color (optional):", style = "text-align: left;")
            )
          )
        )
      ),
      # Graph Display Section
      col_7(
        bs4Card(
          title = "Graphical Exploration",
          collapsible = FALSE,
          width = 12,
          height = "790px",
          plotOutput(ns("exploration_plot"), height = "750px") |> add_spinner()
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
        } if (zoneId === '", ns("color_dropzone"), "') {
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
  )
}

#' graphicalexploration Server Functions
#'
#' @noRd
mod_graphicalexploration_server <- function(id, dfs, shapefile) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    vars <- reactiveVal(c())
    group_vars <- reactiveVal(c())
    color_vars <- reactiveVal(c())
    availablevars <- reactiveVal(c())

    observe({
      if (input$dforshape2 == "data.frame") {
        updatePickerInput(session, "dftoexplore",
                          choices = c("none", names(dfs)))
      } else {
        updatePickerInput(session, "dftoexplore",
                          choices = c("none", setdiff(names(shapefile), c("shapefileplot"))))
      }
    })

    dfactive <- reactiveValues()
    observeEvent(input$dftoexplore, {
      vars(c())
      group_vars(c())
      color_vars(c())

      removeUI(selector = paste0("#", ns("var_dropzone"), " > .draggable"), immediate = TRUE)
      removeUI(selector = paste0("#", ns("group_dropzone"), " > .draggable"), immediate = TRUE)
      removeUI(selector = paste0("#", ns("color_dropzone"), " > .draggable"), immediate = TRUE)

      if (input$dftoexplore == "none") {
        availablevars(c())
        dfactive$df <- NULL
      } else {
        req(input$dftoexplore)
        if (input$dforshape2 == "data.frame") {
          dfactive$df <- dfs[[input$dftoexplore]]$data |> convert_numeric_cols()
        } else {
          dfactive$df <- shapefile[[input$dftoexplore]]$data |> convert_numeric_cols()
        }
      }
    })

    output$variables_ui <- renderUI({
      req(dfactive$df)
      availablevars(setdiff(names(dfactive$df), c(vars(), group_vars(), color_vars())))
      div(id = ns("variables"),
          lapply(availablevars(), function(var) {
            var_class <- switch(class(dfactive$df[[var]])[1],
                                "numeric" = "numeric",
                                "factor" = "character",
                                "character" = "character",
                                "unknown")
            color_class <- switch(var_class,
                                  "numeric" = "numeric-var numeric-item",
                                  "character" = "character-var",
                                  NULL)
            div(class = paste("draggable", color_class), var, draggable = "true")
          })
      )
    })

    observeEvent(input$dropped_item, {
      zone <- input$dropped_item$zone
      value <- input$dropped_item$value

      if (zone == ns("var_dropzone")) {
        vars(unique(c(vars(), value)))
      } else if (zone == ns("group_dropzone")) {
        group_vars(unique(c(group_vars(), value)))
      } else if (zone == ns("color_dropzone")) {
        color_vars(unique(c(color_vars(), value)))
      }
    })

    observeEvent(input$removed_item, {
      zone <- input$removed_item$zone
      value <- input$removed_item$value

      if (zone == ns("var_dropzone")) {
        vars(setdiff(vars(), value))
      } else if (zone == ns("group_dropzone")) {
        group_vars(setdiff(group_vars(), value))
      } else if (zone == ns("color_dropzone")) {
        color_vars(setdiff(color_vars(), value))
      }
    })

    exploration_plot <- reactive({
      req(vars())
      p <- ggplot(dfactive$df, aes(x = .data[[vars()[1]]]))

      if (length(group_vars()) > 0) {
        p <- ggplot(dfactive$df, aes(y = .data[[vars()[1]]], x = .data[[group_vars()[1]]], group = .data[[group_vars()[1]]]))
      }
      if (length(color_vars()) > 0) {
        p <- p + aes(color = .data[[color_vars()[1]]], group = .data[[color_vars()[1]]])
      }

      if (length(group_vars()) > 0) {
        p <- p + geom_boxplot()
      } else {
        p <- p + geom_density()
      }
      p + theme_minimal(base_size = 24)
    })

    output$exploration_plot <- renderPlot({
      exploration_plot()
    })


  })
}


## To be copied in the UI
# mod_graphicalexploration_ui("graphicalexploration_1")

## To be copied in the server
# mod_graphicalexploration_server("graphicalexploration_1")
