#' growthmodelscurves UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_growthmodelscurves_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Nonlinear Models",
          collapsible = FALSE,
          width = 12,
          height = "auto",
          pickerInput(
            ns("models"),
            label = "Select a model",
            choices = c("Logistic L3", "Logistic L4", "Logistic L5"),
          ),
          numericRangeInput(
            ns("range"),
            label = "Range x axis",
            value = c(0, 150),
            min = 0,
            max = Inf
          ),
          conditionalPanel(
            condition = "input.models == 'Logistic L3'",  ns = ns,
            #slider for b0
            sliderInput(
              ns("b0l3"),
              label = "b0",
              min = -1,
              max = 1,
              value = 0.6,
              step = 0.001
            ),
            # slider for b1
            sliderInput(
              ns("b1l3"),
              label = "b1",
              min = 0,
              max = 150,
              value = 70,
              step = 0.001
            ),
            # slider for b2
            sliderInput(
              ns("b2l3"),
              label = "b2",
              min = 0,
              max = 20,
              value = 8,
              step = 0.001
            )
          ),
          conditionalPanel(
            condition = "input.models == 'Logistic L4'", ns = ns,
            # slider for b3
            sliderInput(
              ns("b0l4"),
              label = "b0",
              min = -1,
              max = 1,
              value = -0.1,
              step = 0.001
            ),
            # slider for b1
            sliderInput(
              ns("b1l4"),
              label = "b1",
              min = 0,
              max = 1,
              value = 0.05,
              step = 0.001
            ),
            # slider for b2
            sliderInput(
              ns("b2l4"),
              label = "b2",
              min = 0,
              max = 5,
              value = 1,
              step = 0.001
            ),
            sliderInput(
              ns("b3l4"),
              label = "b3",
              min = 0,
              max = 150,
              value = 75,
              step = 0.001
            )
          ),
          conditionalPanel(
            condition = "input.models == 'Logistic L5'", ns = ns,
            # slider for b3
            sliderInput(
              ns("b0l5"),
              label = "b0",
              min = -1,
              max = 1,
              value = -0.1,
              step = 0.001
            ),
            # slider for b1
            sliderInput(
              ns("b1l5"),
              label = "b1",
              min = 0,
              max = 1,
              value = 0.05,
              step = 0.001
            ),
            # slider for b2
            sliderInput(
              ns("b2l5"),
              label = "b2",
              min = 0,
              max = 5,
              value = 0.6,
              step = 0.001
            ),
            sliderInput(
              ns("b3l5"),
              label = "b3",
              min = 0,
              max = 150,
              value = 75,
              step = 0.001
            ),
            sliderInput(
              ns("b4l5"),
              label = "b4",
              min = 0,
              max = 5,
              value = 1,
              step = 0.001
            )
          )

        )

      ),
      col_9(
        bs4TabCard(
          id = "tabsmodels",
          status = "success",
          width = 12,
          height = "790px",
          title = "Nonlinear models",
          selected = "Equations",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Equations",
            uiOutput(ns("equations"))
          ),
          tabPanel(
            title = "Curves",
            fluidRow(
              col_4(
                plotOutput(ns("curve"))
              ),
              col_4(
                plotOutput(ns("fdcurve"))
              ),
              col_4(
                plotOutput(ns("sdcurve"))
              )
            )
          )
        )
      )
    )
  )
}

#' growthmodelscurves Server Functions
#'
#' @noRd
mod_growthmodelscurves_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$equations <- renderUI({
      if(input$models == "Logistic L3"){
        eq <- help_mod_L3_eq()
        fd <- help_mod_L3_fd()
        sd <- help_mod_L3_sd()
      } else if(input$models == "Logistic L4"){
        eq <- help_mod_L4_eq()
        fd <- help_mod_L4_fd()
        sd <- help_mod_L4_sd()
      } else if(input$models == "Logistic L5"){
        eq <- help_mod_L5_eq()
        fd <- help_mod_L5_fd()
        sd <- help_mod_L5_sd()
      }
      div(
        style = "font-family: Arial, sans-serif; line-height: 1.5;",
        h2(style = "color: #2E86C1;", "Growth Curve"),
        withMathJax(),
        p(eq),
        style = "font-family: Arial, sans-serif; line-height: 1.5;",
        h2(style = "color: #2E86C1;", "First derivative:"),
        p(fd),
        style = "font-family: Arial, sans-serif; line-height: 1.5;",
        h2(style = "color: #2E86C1;", "Second derivative:"),
        p(sd)
      )
    })

    output$curve <- renderPlot({
      if(input$models == "Logistic L3"){
        ggplot() +
          stat_function(
            fun = modfun_L3,
            xlim = c(input$range[1], input$range[2]),
            args = list(b0 = input$b0l3, b1 = input$b1l3, b2 = input$b2l3),
            linewidth = 1.5,
            n = 500
          ) +
          labs(title = "Fitted curve",
               x = "Days after planting",
               y = "Plant height") +
          theme_minimal(base_size = 20)
      } else if(input$models == "Logistic L4"){
        ggplot() +
          stat_function(
            fun = modfun_L4,
            xlim = c(input$range[1], input$range[2]),
            args = list(b0 = input$b0l4, b1 = input$b1l4, b2 = input$b2l4, b3 = input$b3l4),
            linewidth = 1.5,
            n = 500
          ) +
          labs(title = "Fitted curve",
               x = "Days after planting",
               y = "Plant height") +
          theme_minimal(base_size = 20)
      } else if(input$models == "Logistic L5"){
        ggplot() +
          stat_function(
            fun = modfun_L5,
            xlim = c(input$range[1], input$range[2]),
            args = list(b0 = input$b0l5, b1 = input$b1l5, b2 = input$b2l5, b3 = input$b3l5, b4 = input$b4l5),
            linewidth = 1.5,
            n = 500
          ) +
          labs(title = "Fitted curve",
               x = "Days after planting",
               y = "Plant height") +
          theme_minimal(base_size = 20)

      }
    })
    output$fdcurve <- renderPlot({
      if(input$models == "Logistic L3"){
        ggplot() +
          stat_function(
            fun = fdfun_L3,
            xlim = c(input$range[1], input$range[2]),
            args = list(b0 = input$b0l3, b1 = input$b1l3, b2 = input$b2l3),
            linewidth = 1.5,
            n = 500
          ) +
          labs(title = "First derivative",
               x = "Days after planting",
               y = "Velocity (Growing rate)") +
          theme_minimal(base_size = 20)
      } else if(input$models == "Logistic L4"){
        ggplot() +
          stat_function(
            fun = fdfun_L4,
            xlim = c(input$range[1], input$range[2]),
            args = list(b0 = input$b0l4, b1 = input$b1l4, b2 = input$b2l4, b3 = input$b3l4),
            linewidth = 1.5,
            n = 500
          ) +
          labs(title = "First derivative",
               x = "Days after planting",
               y = "Velocity (Growing rate)") +
          theme_minimal(base_size = 20)
      } else if(input$models == "Logistic L5"){
        ggplot() +
          stat_function(
            fun = fdfun_L5,
            xlim = c(input$range[1], input$range[2]),
            args = list(b0 = input$b0l5, b1 = input$b1l5, b2 = input$b2l5, b3 = input$b3l5, b4 = input$b4l5),
            linewidth = 1.5,
            n = 500
          ) +
          labs(title = "First derivative",
               x = "Days after planting",
               y = "Velocity (Growing rate)") +
          theme_minimal(base_size = 20)

      }

    })

    output$sdcurve <- renderPlot({
      if(input$models == "Logistic L3"){
        ggplot() +
          stat_function(
            fun = sdfun_L3,
            xlim = c(input$range[1], input$range[2]),
            args = list(b0 = input$b0l3, b1 = input$b1l3, b2 = input$b2l3),
            linewidth = 1.5,
            n = 500
          ) +
          labs(title = "Second derivative",
               x = "Days after planting",
               y = "Acceleration (change in growing rate)") +
          theme_minimal(base_size = 20)
      } else if(input$models == "Logistic L4"){
        ggplot() +
          stat_function(
            fun = sdfun_L4,
            xlim = c(input$range[1], input$range[2]),
            args = list(b0 = input$b0l4, b1 = input$b1l4, b2 = input$b2l4, b3 = input$b3l4),
            linewidth = 1.5,
            n = 500
          ) +
          labs(title = "Second derivative",
               x = "Days after planting",
               y = "Acceleration (change in growing rate)") +
          theme_minimal(base_size = 20)
      } else if(input$models == "Logistic L5"){
        ggplot() +
          stat_function(
            fun = sdfun_L5,
            xlim = c(input$range[1], input$range[2]),
            args = list(b0 = input$b0l5, b1 = input$b1l5, b2 = input$b2l5, b3 = input$b3l5, b4 = input$b4l5),
            linewidth = 1.5,
            n = 500
          ) +
          labs(title = "Second derivative",
               x = "Days after planting",
               y = "Acceleration (change in growing rate)") +
          theme_minimal(base_size = 20)

      }
    })

  })
}

## To be copied in the UI
# mod_growthmodelscurves_ui("growthmodelscurves_1")

## To be copied in the server
# mod_growthmodelscurves_server("growthmodelscurves_1")
