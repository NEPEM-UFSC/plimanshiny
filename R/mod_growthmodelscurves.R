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
            choices = list(
              Sigmoid = c("Logistic 3P", "Logistic 4P", "Gompertz", "Trans-Gompertz", "Weibull"),
              Exponential = c("Von Bertalanffy", "Exponential", "Janoschek"),
              Sinusoidal = c("Sinusoidal")
            ),
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE
            )
          ),
          numericRangeInput(
            ns("range"),
            label = "Range x axis",
            value = c(0, 150),
            min = 0,
            max = Inf
          ),
          conditionalPanel(
            condition = "input.models == 'Logistic 3P'",  ns = ns,
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
            condition = "input.models == 'Gompertz'",  ns = ns,
            #slider for b0
            sliderInput(
              ns("Asym"),
              label = "Asym",
              min = 0,
              max = 1,
              value = 0.7,
              step = 0.001
            ),
            # slider for b1
            sliderInput(
              ns("b2"),
              label = "b2",
              min = 0,
              max = 200,
              value = 120,
              step = 0.001
            ),
            # slider for b2
            sliderInput(
              ns("b3"),
              label = "b3",
              min = 0,
              max = 1,
              value = 0.9,
              step = 0.001
            )
          ),
          conditionalPanel(
            condition = "input.models == 'Trans-Gompertz'",  ns = ns,
            #slider for b0
            sliderInput(
              ns("Atg"),
              label = "A",
              min = 0,
              max = 1,
              value = 0.7,
              step = 0.001
            ),
            # slider for b1
            sliderInput(
              ns("btg"),
              label = "b",
              min = 0,
              max = 200,
              value = 20,
              step = 0.001
            ),
            # slider for b2
            sliderInput(
              ns("ctg"),
              label = "c",
              min = 0,
              max = 1,
              value = 0.07,
              step = 0.001
            )
          ),
          conditionalPanel(
            condition = "input.models == 'Weibull'",  ns = ns,
            sliderInput(
              ns("Asymwei"),
              label = "Asym (Maximum Response)",
              min = 0,
              max = 10,
              value = 2,
              step = 0.001
            ),
            sliderInput(
              ns("Drop"),
              label = "Drop (Range of Response)",
              min = 0,
              max = 10,
              value = 2,
              step = 0.001
            ),
            sliderInput(
              ns("lrc"),
              label = "lrc (Logarithmic Growth Rate)",
              min = -30,
              max = 0,
              value = -13,
              step = 0.01
            ),
            sliderInput(
              ns("pwr"),
              label = "pwr (Power Parameter)",
              min = 0.1,
              max = 10,
              value = 3,
              step = 0.01
            )
          ),
          conditionalPanel(
            condition = "input.models == 'Logistic 4P'", ns = ns,
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
            condition = "input.models == 'Logistic 5P'", ns = ns,
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
          ),
          # Von Bertalanffy Model Inputs
          conditionalPanel(
            condition = "input.models == 'Von Bertalanffy'", ns = ns,
            sliderInput(
              ns("Linf"),
              label = "Linf (Asymptotic Length)",
              min = 0,
              max = 100,
              value = 50,
              step = 0.1
            ),
            sliderInput(
              ns("k"),
              label = "k (Growth Rate)",
              min = 0,
              max = 1,
              value = 0.1,
              step = 0.001
            ),
            sliderInput(
              ns("t0"),
              label = "t0 (Time at Zero Growth)",
              min = -10,
              max = 10,
              value = 0,
              step = 0.1
            )
          ),
          # Exponential Model Inputs
          conditionalPanel(
            condition = "input.models == 'Exponential'", ns = ns,
            sliderInput(
              ns("a_exp"),
              label = "a (Initial Value)",
              min = 0,
              max = 10,
              value = 1,
              step = 0.1
            ),
            sliderInput(
              ns("b_exp"),
              label = "b (Growth Rate)",
              min = -2,
              max = 2,
              value = 0.1,
              step = 0.01
            )
          ),
          # Janoschek Model Inputs
          conditionalPanel(
            condition = "input.models == 'Janoschek'", ns = ns,
            sliderInput(
              ns("Asym"),
              label = "Asym (Asymptote)",
              min = 0,
              max = 100,
              value = 50,
              step = 0.1
            ),
            sliderInput(
              ns("y0"),
              label = "y0 (Initial Value)",
              min = 0,
              max = 50,
              value = 10,
              step = 0.1
            ),
            sliderInput(
              ns("k"),
              label = "k (Rate Parameter)",
              min = 0.001,
              max = 2,
              value = 0.1,
              step = 0.001
            ),
            sliderInput(
              ns("m"),
              label = "m (Shape Parameter)",
              min = 0.1,
              max = 5,
              value = 1.5,
              step = 0.1
            )
          ),
          conditionalPanel(
            condition = "input.models == 'Sinusoidal'", ns = ns,
            sliderInput(
              ns("y0sin"),
              label = "y0 (Baseline Value)",
              min = -50,
              max = 50,
              value = 0,
              step = 0.1
            ),
            sliderInput(
              ns("asin"),
              label = "a (Amplitude)",
              min = 0,
              max = 50,
              value = 10,
              step = 0.1
            ),
            sliderInput(
              ns("bsin"),
              label = "b (Period)",
              min = 1,
              max = 365,
              value = 30,
              step = 1
            ),
            sliderInput(
              ns("csin"),
              label = "c (Phase Shift)",
              min = -pi,
              max = pi,
              value = 0,
              step = 0.01
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
      if(input$models == "Logistic 3P"){
        eq <- help_mod_L3_eq()
        fd <- help_mod_L3_fd()
        sd <- help_mod_L3_sd()
      } else if(input$models == "Logistic 4P"){
        eq <- help_mod_L4_eq()
        fd <- help_mod_L4_fd()
        sd <- help_mod_L4_sd()
      } else if(input$models == "Logistic 5P"){
        eq <- help_mod_L5_eq()
        fd <- help_mod_L5_fd()
        sd <- help_mod_L5_sd()
      } else if(input$models == "Gompertz"){
        eq <- help_mod_gompertz_eq()
        fd <- help_mod_gompertz_fd()
        sd <- help_mod_gompertz_sd()
      } else if(input$models == "Weibull"){
        eq <- help_mod_weibull_eq()
        fd <- help_mod_weibull_fd()
        sd <- help_mod_weibull_sd()
      } else if (input$models == "Von Bertalanffy") {
        eq <- help_mod_vonbert_eq()
        fd <- help_mod_vonbert_fd()
        sd <- help_mod_vonbert_sd()
      } else if (input$models == "Exponential") {
        eq <- help_mod_exp_eq()
        fd <- help_mod_exp_fd()
        sd <- help_mod_exp_sd()
      } else if (input$models == "Janoschek") {
        eq <- help_mod_janoschek_eq()
        fd <- help_mod_janoschek_fd()
        sd <- help_mod_janoschek_fd()
      } else if (input$models == "Trans-Gompertz") {
        eq <- help_mod_transgompertz_eq()
        fd <- help_mod_transgompertz_fd()
        sd <- help_mod_transgompertz_sd()
      } else if (input$models == "Sinusoidal") {
        eq <- help_mod_sinusoidal_eq()
        fd <- help_mod_sinusoidal_fd()
        sd <- help_mod_sinusoidal_sd()
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
      if(input$models == "Logistic 3P"){
        ggplot() +
          stat_function(
            fun = modfun_L3,
            xlim = c(input$range[1], input$range[2]),
            args = list(b0 = input$b0l3, b1 = input$b1l3, b2 = input$b2l3),
            linewidth = 1.5,
            n = 500
          ) +
          labs(title = "Logistic 3P curve",
               x = "Time",
               y = "Response") +
          theme_minimal(base_size = 20)
      } else if(input$models == "Logistic 4P"){
        ggplot() +
          stat_function(
            fun = modfun_L4,
            xlim = c(input$range[1], input$range[2]),
            args = list(b0 = input$b0l4, b1 = input$b1l4, b2 = input$b2l4, b3 = input$b3l4),
            linewidth = 1.5,
            n = 500
          ) +
          labs(title = "Logistic 4P curve",
               x = "Time",
               y = "Response") +
          theme_minimal(base_size = 20)
      } else if(input$models == "Logistic 5P"){
        ggplot() +
          stat_function(
            fun = modfun_L5,
            xlim = c(input$range[1], input$range[2]),
            args = list(b0 = input$b0l5, b1 = input$b1l5, b2 = input$b2l5, b3 = input$b3l5, b4 = input$b4l5),
            linewidth = 1.5,
            n = 500
          ) +
          labs(title = "Logistic 5P curve",
               x = "Time",
               y = "Response") +
          theme_minimal(base_size = 20)

      } else if(input$models == "Gompertz"){
        ggplot() +
          stat_function(
            fun = modfun_gompertz,
            xlim = c(input$range[1], input$range[2]),
            args = list(Asym = input$Asym, b2 = input$b2, b3 = input$b3),
            linewidth = 1.5,
            n = 500
          ) +
          labs(title = "Gompertz curve",
               x = "Time",
               y = "Response") +
          theme_minimal(base_size = 20)
      } else if(input$models == "Weibull"){
        ggplot() +
          stat_function(
            fun =  modfun_weibull,
            xlim = c(input$range[1], input$range[2]),
            args = list(Asym = input$Asymwei, Drop = input$Drop, lrc = input$lrc, pwr = input$pwr),
            linewidth = 1.5,
            n = 500
          ) +
          labs(title = "Weibull curve",
               x = "Time",
               y = "Response") +
          theme_minimal(base_size = 20)
      } else  if (input$models == "Von Bertalanffy") {
        ggplot() +
          stat_function(
            fun = modfun_vonbert,
            xlim = c(input$range[1], input$range[2]),
            args = list(Linf = input$Linf, k = input$k, t0 = input$t0),
            linewidth = 1.5,
            n = 500
          ) +
          labs(title = "Von Bertalanffy Curve",
               x = "Time",
               y = "Response") +
          theme_minimal(base_size = 20)
      } else if (input$models == "Exponential") {
        ggplot() +
          stat_function(
            fun = modfun_exp,
            xlim = c(input$range[1], input$range[2]),
            args = list(a = input$a_exp, b = input$b_exp),
            linewidth = 1.5,
            n = 500
          ) +
          labs(title = "Exponential Curve",
               x = "Time",
               y = "Response") +
          theme_minimal(base_size = 20)
      } else if (input$models == "Janoschek") {
        ggplot() +
          stat_function(
            fun = modfun_janoschek,
            xlim = c(input$range[1], input$range[2]),
            args = list(Asym = input$Asym, y0 = input$y0, k = input$k, m = input$m),
            linewidth = 1.5,
            n = 500
          ) +
          labs(title = "Janoschek Curve",
               x = "Time",
               y = "Response") +
          theme_minimal(base_size = 20)
      } else if (input$models == "Trans-Gompertz") {
        ggplot() +
          stat_function(
            fun = modfun_transgompertz,
            xlim = c(input$range[1], input$range[2]),
            args = list(A = input$Atg, b = input$btg, c = input$ctg),
            linewidth = 1.5,
            n = 500
          ) +
          labs(title = "Trans-Gompertz Curve",
               x = "Time",
               y = "Response") +
          theme_minimal(base_size = 20)
      } else if (input$models == "Sinusoidal") {
        ggplot() +
          stat_function(
            fun = modfun_sinusoidal,
            xlim = c(input$range[1], input$range[2]),
            args = list(y0 = input$y0sin, a = input$asin, b = input$bsin, c = input$csin),
            linewidth = 1.5,
            n = 500
          ) +
          labs(title = "Sinusoidal Curve",
               x = "Time",
               y = "Response") +
          theme_minimal(base_size = 20)
      }
    })


    output$fdcurve <- renderPlot({
      if(input$models == "Logistic 3P"){
        ggplot() +
          stat_function(
            fun = fdfun_L3,
            xlim = c(input$range[1], input$range[2]),
            args = list(b0 = input$b0l3, b1 = input$b1l3, b2 = input$b2l3),
            linewidth = 1.5,
            n = 500
          ) +
          labs(
            title = "First Derivative",
            x = "Time",
            y = "Growth Rate"
          ) +
          theme_minimal(base_size = 20)
      } else if(input$models == "Logistic 4P"){
        ggplot() +
          stat_function(
            fun = fdfun_L4,
            xlim = c(input$range[1], input$range[2]),
            args = list(b0 = input$b0l4, b1 = input$b1l4, b2 = input$b2l4, b3 = input$b3l4),
            linewidth = 1.5,
            n = 500
          ) +
          labs(
            title = "First Derivative",
            x = "Time",
            y = "Growth Rate"
          ) +
          theme_minimal(base_size = 20)
      } else if(input$models == "Logistic 5P"){
        ggplot() +
          stat_function(
            fun = fdfun_L5,
            xlim = c(input$range[1], input$range[2]),
            args = list(b0 = input$b0l5, b1 = input$b1l5, b2 = input$b2l5, b3 = input$b3l5, b4 = input$b4l5),
            linewidth = 1.5,
            n = 500
          ) +
          labs(
            title = "First Derivative",
            x = "Time",
            y = "Growth Rate"
          ) +
          theme_minimal(base_size = 20)

      } else if(input$models == "Gompertz"){
        ggplot() +
          stat_function(
            fun = fdfun_gompertz,
            xlim = c(input$range[1], input$range[2]),
            args = list(Asym = input$Asym, b2 = input$b2, b3 = input$b3),
            linewidth = 1.5,
            n = 500
          ) +
          labs(
            title = "First Derivative",
            x = "Time",
            y = "Growth Rate"
          ) +
          theme_minimal(base_size = 20)
      } else if(input$models == "Weibull"){
        ggplot() +
          stat_function(
            fun = fdfun_weibull,
            xlim = c(input$range[1], input$range[2]),
            args = list(Asym = input$Asymwei, Drop = input$Drop, lrc = input$lrc, pwr = input$pwr),
            linewidth = 1.5,
            n = 500
          ) +
          labs(
            title = "First Derivative",
            x = "Time",
            y = "Growth Rate"
          ) +
          theme_minimal(base_size = 20)
      } else if (input$models == "Von Bertalanffy") {
        ggplot() +
          stat_function(
            fun = fdfun_vonbert,
            xlim = c(input$range[1], input$range[2]),
            args = list(Linf = input$Linf, k = input$k, t0 = input$t0),
            linewidth = 1.5,
            n = 500
          ) +
          labs(
            title = "First Derivative",
            x = "Time",
            y = "Growth Rate"
          ) +
          theme_minimal(base_size = 20)
      } else if (input$models == "Exponential") {
        ggplot() +
          stat_function(
            fun = fdfun_exp,
            xlim = c(input$range[1], input$range[2]),
            args = list(a = input$a_exp, b = input$b_exp),
            linewidth = 1.5,
            n = 500
          ) +
          labs(
            title = "First Derivative",
            x = "Time",
            y = "Growth Rate"
          ) +
          theme_minimal(base_size = 20)
      } else if (input$models == "Janoschek") {
        ggplot() +
          stat_function(
            fun = fdfun_janoschek,
            xlim = c(input$range[1], input$range[2]),
            args = list(Asym = input$Asym, y0 = input$y0, k = input$k, m = input$m),
            linewidth = 1.5,
            n = 500
          ) +
          labs(
            title = "First Derivative",
            x = "Time",
            y = "Growth Rate"
          ) +
          theme_minimal(base_size = 20)
      } else if (input$models == "Trans-Gompertz") {
        ggplot() +
          stat_function(
            fun = fdfun_transgompertz,
            xlim = c(input$range[1], input$range[2]),
            args = list(A = input$Atg, b = input$btg, c = input$ctg),
            linewidth = 1.5,
            n = 500
          ) +
          labs(
            title = "First Derivative",
            x = "Time",
            y = "Growth Rate"
          ) +
          theme_minimal(base_size = 20)
      } else if (input$models == "Sinusoidal") {
        ggplot() +
          stat_function(
            fun = fdfun_sinusoidal,
            xlim = c(input$range[1], input$range[2]),
            args = list(y0 = input$y0sin, a = input$asin, b = input$bsin, c = input$csin),
            linewidth = 1.5,
            n = 500
          ) +
          labs(
            title = "First Derivative",
            x = "Time",
            y = "Growth Rate"
          ) +
          theme_minimal(base_size = 20)
      }
    })

    output$sdcurve <- renderPlot({
      if(input$models == "Logistic 3P"){
        ggplot() +
          stat_function(
            fun = sdfun_L3,
            xlim = c(input$range[1], input$range[2]),
            args = list(b0 = input$b0l3, b1 = input$b1l3, b2 = input$b2l3),
            linewidth = 1.5,
            n = 500
          ) +
          labs(
            title = "Second Derivative",
            x = "Time",
            y = "Acceleration"
          ) +
          theme_minimal(base_size = 20)
      } else if(input$models == "Logistic 4P"){
        ggplot() +
          stat_function(
            fun = sdfun_L4,
            xlim = c(input$range[1], input$range[2]),
            args = list(b0 = input$b0l4, b1 = input$b1l4, b2 = input$b2l4, b3 = input$b3l4),
            linewidth = 1.5,
            n = 500
          ) +
          labs(
            title = "Second Derivative",
            x = "Time",
            y = "Acceleration"
          ) +
          theme_minimal(base_size = 20)
      } else if(input$models == "Logistic 5P"){
        ggplot() +
          stat_function(
            fun = sdfun_L5,
            xlim = c(input$range[1], input$range[2]),
            args = list(b0 = input$b0l5, b1 = input$b1l5, b2 = input$b2l5, b3 = input$b3l5, b4 = input$b4l5),
            linewidth = 1.5,
            n = 500
          ) +
          labs(
            title = "Second Derivative",
            x = "Time",
            y = "Acceleration"
          ) +
          theme_minimal(base_size = 20)

      } else if(input$models == "Gompertz"){
        ggplot() +
          stat_function(
            fun = sdfun_gompertz,
            xlim = c(input$range[1], input$range[2]),
            args = list(Asym = input$Asym, b2 = input$b2, b3 = input$b3),
            linewidth = 1.5,
            n = 500
          ) +
          labs(
            title = "Second Derivative",
            x = "Time",
            y = "Acceleration"
          ) +
          theme_minimal(base_size = 20)
      } else if(input$models == "Weibull"){
        ggplot() +
          stat_function(
            fun = sdfun_weibull,
            xlim = c(input$range[1], input$range[2]),
            args = list(Asym = input$Asymwei, Drop = input$Drop, lrc = input$lrc, pwr = input$pwr),
            linewidth = 1.5,
            n = 500
          ) +
          labs(
            title = "Second Derivative",
            x = "Time",
            y = "Acceleration"
          ) +
          theme_minimal(base_size = 20)
      } else if (input$models == "Von Bertalanffy") {
        ggplot() +
          stat_function(
            fun = sdfun_vonbert,
            xlim = c(input$range[1], input$range[2]),
            args = list(Linf = input$Linf, k = input$k, t0 = input$t0),
            linewidth = 1.5,
            n = 500
          ) +
          labs(
            title = "Second Derivative",
            x = "Time",
            y = "Acceleration"
          ) +
          theme_minimal(base_size = 20)
      } else if (input$models == "Exponential") {
        ggplot() +
          stat_function(
            fun = sdfun_exp,
            xlim = c(input$range[1], input$range[2]),
            args = list(a = input$a_exp, b = input$b_exp),
            linewidth = 1.5,
            n = 500
          ) +
          labs(
            title = "Second Derivative",
            x = "Time",
            y = "Acceleration"
          ) +
          theme_minimal(base_size = 20)
      } else if (input$models == "Janoschek") {
        ggplot() +
          stat_function(
            fun = sdfun_janoschek,
            xlim = c(input$range[1], input$range[2]),
            args = list(Asym = input$Asym, y0 = input$y0, k = input$k, m = input$m),
            linewidth = 1.5,
            n = 500
          ) +
          labs(
            title = "Second Derivative",
            x = "Time",
            y = "Acceleration"
          ) +
          theme_minimal(base_size = 20)
      } else if (input$models == "Trans-Gompertz") {
        ggplot() +
          stat_function(
            fun = fdfun_transgompertz,
            xlim = c(input$range[1], input$range[2]),
            args = list(A = input$Atg, b = input$btg, c = input$ctg),
            linewidth = 1.5,
            n = 500
          ) +
          labs(
            title = "Second Derivative",
            x = "Time",
            y = "Acceleration"
          ) +
          theme_minimal(base_size = 20)
      } else if (input$models == "Sinusoidal") {
        ggplot() +
          stat_function(
            fun = sdfun_sinusoidal,
            xlim = c(input$range[1], input$range[2]),
            args = list(y0 = input$y0sin, a = input$asin, b = input$bsin, c = input$csin),
            linewidth = 1.5,
            n = 500
          ) +
          labs(
            title = "Second Derivative",
            x = "Time",
            y = "Acceleration"
          ) +
          theme_minimal(base_size = 20)
      }


    })
  })
}

## To be copied in the UI
# mod_growthmodelscurves_ui("growthmodelscurves_1")

## To be copied in the server
# mod_growthmodelscurves_server("growthmodelscurves_1")
