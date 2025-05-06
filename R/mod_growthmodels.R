#' growthmodels UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_growthmodels_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Nonlinear modeling for growth curves",
          collapsible = FALSE,
          width = 12,
          awesomeRadio(
            inputId = ns("inputdsmtype"),
            label = "Entry method",
            choices = c("Imported Dataset", "Example Dataset"),
            selected = "Imported Dataset",
            status = "success",
            inline = TRUE
          ),
          pickerInput(
            ns("dftoedit"),
            label = "Time series data",
            choices = NULL
          ),
          hl(),
          prettyCheckbox(
            inputId = ns("useclimate"),
            label = "Fetch weather data",
            value = FALSE,
            shape = "curve",
            status = "success",
            icon = icon("check"),
            animation = "rotate"
          ),
          conditionalPanel(
            condition = "input.useclimate == true", ns = ns,
            fluidRow(
              col_4(
                numericInput(ns("basemin"),
                             label = "Tbase lower",
                             value = 10,
                             step = 0.1)
              ),
              col_4(
                numericInput(ns("baseupp"),
                             label = "Tbase upper",
                             value = 40,
                             step = 0.1)
              ),
              col_4(
                actionBttn(
                  inputId = ns("getweather"),
                  label = "Fetch",
                  icon = icon("cloud-sun-rain"),
                  status = "success",
                  gradient = TRUE,
                  flat = TRUE
                )
              )
            )
          ),
          hl(),
          fluidRow(
            col_6(
              pickerInput(
                ns("flightdate"),
                label = "Predictor",
                choices = NULL
              )
            ),
            col_6(
              pickerInput(
                ns("traittomodel"),
                label = "Trait to model",
                choices = NULL
              )
            )
          ),
          pickerInput(
            ns("growthmodel"),
            label = "Growth Model",
            choices = list(
              Sigmoid = c("Logistic 3P", "Logistic 4P", "Gompertz", "Trans-Gompertz", "Weibull", "Beta growth", "Hill"),
              Exponential = c("Von Bertalanffy", "Exponential", "Janoschek", "Asymptotic", "Exponential-Plateau", "Expolinear"),
              Peak = c("Asymmetric Gaussian", ""),
              Cyclical = c("Sinusoidal", "")
            ),
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE
            ),
            multiple = TRUE,
            selected = "Logistic 4P"
          ),
          prettyCheckbox(
            inputId = ns("parallel"),
            label = "Parallel Processing",
            value = FALSE,
            icon = icon("check"),
            status = "success",
            animation = "rotate"
          ),
          textInput(
            ns("saveto"),
            label = "Save results to...",
            value = "growth_model_results"
          ),
          actionBttn(
            ns("predictmat"),
            label = "Compute growth models!",
            icon = icon("check")
          ),
          hl(),
          actionButton(
            inputId = ns("savetoglobalenv"),
            label = "Save a temporary file",
            icon = icon("share-from-square"),
            status = "success",
            gradient = TRUE,
            flat = TRUE
          )
        )
      ),
      col_9(
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "790px",
          title = "Growth models",
          selected = "Home",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Home",
            fluidRow(
              col_9(
                img(src = "www/logogrowth.png", width = "100%", height = "100%")
              ),
              col_3(
                h2("About"),
                "This module provies tools for fitting growth models (Logistic non-linear curve) to model growth traits like plant height and volume.", br(), br(),
                h2("Disclaimer"),
                "We welcome feedback and suggestions about the usefulness of the application and make no guarantee of the correctness,
          reliability, or utility of the results if incorrect recomendations are made based on the results.",
                br(), br(),
                shiny::actionButton(inputId= ns("details"),
                                    label="Growth Model details",
                                    icon = icon("circle-info"))
              )
            )
          ),
          tabPanel(
            title = "Overview",
            plotlyOutput(ns("overview"), height = "700px") |> add_spinner()
          ),
          tabPanel(
            title = "Growth models",
            reactable::reactableOutput(ns("summaryresults"), height = "720px")  |> add_spinner()
          ),
          tabPanel(
            title = "Fitted model",
            fluidRow(
              col_4(
                prettyRadioButtons(
                  inputId = ns("modelorplot"),
                  label = "Plot type",
                  choices = c("One model, multiple plots", "One plot, multiple models"),
                  icon = icon("check"),
                  bigger = TRUE,
                  status = "info",
                  animation = "jelly",
                  inline = TRUE
                )
              ),
              col_8(
                conditionalPanel(
                  condition = "input.modelorplot == 'One model, multiple plots'", ns = ns,
                  fluidRow(
                    col_6(
                      pickerInput(
                        inputId = ns("plotmultiple"),
                        label = "Select unique_id(s) to plot the growth curve:",
                        choices = NULL,
                        multiple = TRUE,
                        options = list(
                          "actions-box" = TRUE,
                          "live-search" = TRUE,
                          "max-options" = 4,
                          "max-options-text" = "No more curves allowed"
                        )
                      )
                    ),
                    col_6(
                      pickerInput(
                        inputId = ns("growthmodelunique"),
                        label = "Select growth models to plot the curve:",
                        choices = NULL,
                        multiple = FALSE
                      )
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.modelorplot == 'One plot, multiple models'", ns = ns,
                  fluidRow(
                    col_6(
                      pickerInput(
                        inputId = ns("plotunique"),
                        label = "Unique plot:",
                        choices = NULL,
                        multiple = FALSE
                      )
                    ),
                    col_6(
                      pickerInput(
                        inputId = ns("growthmodelmult"),
                        label = "Select growth models to plot the curve:",
                        choices = NULL,
                        multiple = TRUE
                      )
                    )
                  )
                )
              )
            ),

            bs4TabCard(
              width = 12,
              icon = icon("chart-line"),
              id = ns("derivs"),
              type = "tabs",
              status = "success",
              height = "600px",
              title = "Growth model curves",
              selected = "Fitted curve",
              tabPanel("Fitted curve",
                       plotOutput(ns("fittedplot"), height = "570px") |> add_spinner()
              ),
              tabPanel("First derivative",
                       plotOutput(ns("fderivate"), height = "570px") |> add_spinner()
              ),
              tabPanel("Second derivative",
                       plotOutput(ns("sderivate"), height = "570px") |> add_spinner()
              )
            )
          ),
          tabPanel(
            title = "Trait distribution",
            pickerInput(
              ns("histotraitsgm"),
              label = "Select trait(s) to plot:",
              choices = NULL,
              multiple = TRUE
            ),
            plotlyOutput(ns("histogramsgm"), height = "680px")  |> add_spinner()
          ),
          tabPanel(
            title = "Summary",
            bs4TabCard(
              width = 12,
              icon = icon("chart-line"),
              id = ns("summarytabs"),
              type = "tabs",
              status = "success",
              height = "720px",
              title = "Growth model curves",
              selected = "Summary by plot",
              tabPanel("Summary by plot",
                       reactable::reactableOutput(ns("summaryplot"), height = "700px")  |> add_spinner()
              ),
              tabPanel("Summary by model",
                       reactable::reactableOutput(ns("summarymodel"), height = "700px")  |> add_spinner()
              )
            )
          )
        )
      )
    )
  )
}

#' growthmodels Server Functions
#'
#' @noRd
mod_growthmodels_server <- function(id, dfs){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$details, {
      showModal(
        modalDialog(
          title = "Details about growth models",
          width = 12,
          headerBorder = FALSE,
          collapsible = TRUE,
          closable = TRUE,
          div(
            h2(style = "color: #2E86C1;", "Description of Returned Variables"),
            tags$ul(
              tags$li(tags$b("block:"), " The identifier for the experimental block, typically used to group plots spatially or temporally."),
              tags$li(tags$b("plot_id:"), " The unique identifier for individual plots within a block."),
              tags$li(tags$b("unique_plot:"), " A combined identifier that uniquely identifies each plot across all blocks (e.g., combining ", tags$i("block"), " and ", tags$i("plot_id"), ")."),
              tags$li(tags$b("model:"), "The name of the growth model used to fit the data."),
              tags$li(tags$b("asymptote:"), "The growth model's asymptote, representing the upper limit of the response variable."),
              tags$li(tags$b("auc:"), " The area under the curve (AUC), representing the total response accumulated over the range of the independent variable."),
              tags$li(tags$b("xinfp:"), " The x-value at the inflection point, providing another representation of the inflection coordinate."),
              tags$li(tags$b("yinfp:"), " The y-value at the inflection point, corresponding to the response variable's value at the inflection."),
              tags$li(tags$b("xmace:"), " The x-value where the second derivative is maximum, reflecting the maximum acceleration of growth rate."),
              tags$li(tags$b("ymace:"), " The y-value showing the response at the maximum acceleration point."),
              tags$li(tags$b("xmdes:"), " The x-value where the second derivative is minimal, reflecting the maximum deceleration of growth rate."),
              tags$li(tags$b("ymdes:"), " The y-value showing the response at the maximum deceleration point."),
              tags$li(tags$b("aic:"), " The Akaike Information Criterion (AIC) value, used to compare the goodness of fit of different models."),
              tags$li(tags$b("rmse:"), " The Root Mean Squared Error (RMSE) value.."),
              tags$li(tags$b("mae:"), " The Mean Absolute Error (MAE) value, representing the average absolute difference between the observed and predicted values."),
            )
          ),
          # Replace bs4TabCard with box for simpler rendering in the modal
          h2("S-shaped curves"),
          box(
            title = "Logistic Model L3",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsed = TRUE,
            collapsible = TRUE,
            help_mod_L3_gm()
          ),
          box(
            title = "Logistic Model L4",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsed = TRUE,
            collapsible = TRUE,
            help_mod_L4_gm()
          ),
          box(
            title = "Gompertz Growth Model",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsed = TRUE,
            collapsible = TRUE,
            help_mod_gompertz()
          ),
          box(
            title = "Trans-Gompertz Growth Model",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsed = TRUE,
            collapsible = TRUE,
            help_mod_transgompertz()
          ),
          box(
            title = "Weibull Growth Curve Model",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsed = TRUE,
            collapsible = TRUE,
            help_mod_weibull()
          ),
          box(
            title = "Beta growth",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsed = TRUE,
            collapsible = TRUE,
            help_mod_beta()
          ),
          box(
            title = "Hill Growth Model",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsed = TRUE,
            collapsible = TRUE,
            help_mod_hill()
          ),
          h2("Exponential"),
          box(
            title = "Von Bertalanffy Growth Model",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsed = TRUE,
            collapsible = TRUE,
            help_mod_vonbertalanffy()
          ),
          box(
            title = "Exponential Growth Model",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsed = TRUE,
            collapsible = TRUE,
            help_mod_exponential()
          ),
          box(
            title = "Janoschek Growth Model",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsed = TRUE,
            collapsible = TRUE,
            help_mod_janoschek()
          ),
          box(
            title = "Asymptotic curve",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsed = TRUE,
            collapsible = TRUE,
            help_mod_asymptotic()
          ),
          box(
            title = "Exponential-Plateau",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsed = TRUE,
            collapsible = TRUE,
            help_mod_exponential_plateau()
          ),
          box(
            title = "Expolinear",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsed = TRUE,
            collapsible = TRUE,
            help_mod_exponential_linear()
          ),
          h2("Cyclical"),
          box(
            title = "Sinusoidal Growth Model",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsed = TRUE,
            collapsible = TRUE,
            help_mod_sinusoidal()
          ),
          h2("Peak-Based"),
          box(
            title = "Asymmetric Gaussian Model",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsed = TRUE,
            collapsible = TRUE,
            help_mod_asymmetric_gaussian()
          ),
          footer = NULL,
          easyClose = TRUE,
          size = "xl"
        )
      )
    })


    # The example dataset is a sample of 4 plots obtained after time series processing using the
    # bisonfly DSM dataset, by Filipe Matias https://github.com/filipematias23/Bison-Fly
    observeEvent(input$inputdsmtype, {
      if(input$inputdsmtype == "Example Dataset"){
        filepath <- file.path(system.file(package = "plimanshiny"), "app/www/timeseries_dsm.csv")
        dfs[["df_growth_model"]] <- create_reactval("df_growth_model",  read.csv(filepath))
        observe({
          updatePickerInput(session, "dftoedit",
                            choices = c("none", names(dfs)),
                            selected = "df_growth_model")
        })
      }
    })


    observe({
      updatePickerInput(session, "dftoedit",
                        choices = c("none", names(dfs)))
    })

    dfactive <- reactiveValues()
    observeEvent(input$dftoedit, {
      req(input$dftoedit)
      if(input$dftoedit != "none"){
        dfactive$df <-
          dfs[[input$dftoedit]]$data |>
          dplyr::mutate(unique_plot = paste0(block, "_", plot_id)) |>
          convert_numeric_cols()
      }
    })


    observeEvent(input$getweather, {
      req(dfactive$df)
      climateinfo <- get_weather_info(dfactive$df)

      dfclimate <- get_climate(lat = climateinfo$lat,
                               lon = climateinfo$lon,
                               env = "Timeseries",
                               start = climateinfo$start,
                               end = climateinfo$end,
                               scale = "daily",
                               params = c("T2M", "T2M_MIN", "T2M_MAX"),
                               environment = "shiny",
                               progress = TRUE) |>
        gdd_ometto_frue(Tbase = input$basemin,
                        Tceil = input$baseupp) |>
        tidyr::unite("date", c("YEAR", "MO", "DY"), sep = "-") |>
        dplyr::mutate(date = lubridate::ymd(date))
      dfactive$df <- dfactive$df |> dplyr::mutate(date = lubridate::ymd(date))
      dfactive$df <- dplyr::left_join(dfactive$df, dfclimate, by = dplyr::join_by(date))
      dfs[[paste0(file_name(input$dftoedit), "_updated")]] <- create_reactval(paste0(input$dftoedit, "_updated"), dfactive$df)

    })


    observe({
      req(dfactive$df)
      updatePickerInput(session, "traittomodel",
                        choices = colnames(dfactive$df),
                        selected = NA)
    })
    observe({
      req(dfactive$df)
      updatePickerInput(session, "flightdate",
                        choices = colnames(dfactive$df),
                        selected = "date")
    })



    output$overview <- renderPlotly({
      req(dfactive$df)
      req(input$traittomodel)
      p <-
        ggplot(dfactive$df, aes(x = .data[[input$flightdate]], y = .data[[input$traittomodel]], group = 1)) +
        geom_boxplot(fill = "#28a745") +
        geom_smooth(method = 'loess', formula = 'y ~ x') +
        labs(x = input$flightdate, y = input$traittomodel) +
        theme_bw(base_size = 24) +
        theme(panel.grid.minor = element_blank())
      plotly::ggplotly(p)
    })



    models <- reactiveVal()

    observeEvent(input$predictmat, {

      req(dfactive$df)
      req(input$traittomodel)

      req(dfactive$df)
      dftomodel <-
        dfactive$df |>
        sf::st_drop_geometry() |>
        as.data.frame()

      waiter_show(
        html = tagList(
          spin_google(),
          h2("{plimanshiny} is now fitting the growth models. Please wait while we finalize everything...")
        ),
        color = "#228B227F"
      )

      purrr::map(input$growthmodel, ~{
        switch(
          .x,
          "Logistic 3P" = mod_logistic_3P(dftomodel,
                                          predictor = input$flightdate,
                                          dependent = input$traittomodel,
                                          sowing_date = min(dftomodel$date),
                                          parallel = input$parallel),
          "Logistic 4P" = mod_logistic_4P(dftomodel,
                                          predictor = input$flightdate,
                                          dependent = input$traittomodel,
                                          sowing_date = min(dftomodel$date),
                                          parallel = input$parallel),
          "Gompertz" = mod_gompertz(dftomodel,
                                    predictor = input$flightdate,
                                    dependent = input$traittomodel,
                                    sowing_date = min(dftomodel$date),
                                    parallel = input$parallel),
          "Weibull" = mod_weibull(dftomodel,
                                  predictor = input$flightdate,
                                  dependent = input$traittomodel,
                                  sowing_date = min(dftomodel$date),
                                  parallel = input$parallel),
          "Von Bertalanffy" = mod_vonbert(dftomodel,
                                          predictor = input$flightdate,
                                          dependent = input$traittomodel,
                                          sowing_date = min(dftomodel$date),
                                          parallel = input$parallel),
          "Exponential" = mod_exponential(dftomodel,
                                          predictor = input$traittomodel,
                                          sowing_date = min(dftomodel$date),
                                          parallel = input$parallel),
          "Janoschek" = mod_janoschek(dftomodel,
                                      predictor = input$traittomodel,
                                      sowing_date = min(dftomodel$date),
                                      parallel = input$parallel),
          "Trans-Gompertz" = mod_transgompertz(dftomodel,
                                               predictor = input$flightdate,
                                               dependent = input$traittomodel,
                                               sowing_date = min(dftomodel$date),
                                               parallel = input$parallel),
          "Sinusoidal" = mod_sinusoidal(dftomodel,
                                        predictor = input$flightdate,
                                        dependent = input$traittomodel,
                                        sowing_date = min(dftomodel$date),
                                        parallel = input$parallel),
          "Asymptotic" = mod_asymptotic(dftomodel,
                                        predictor = input$flightdate,
                                        dependent = input$traittomodel,
                                        sowing_date = min(dftomodel$date),
                                        parallel = input$parallel),
          "Asymmetric Gaussian" = mod_agauss(dftomodel,
                                             predictor = input$flightdate,
                                             dependent = input$traittomodel,
                                             sowing_date = min(dftomodel$date),
                                             parallel = input$parallel),
          "Beta growth" = mod_beta(dftomodel,
                                   predictor = input$flightdate,
                                   dependent = input$traittomodel,
                                   sowing_date = min(dftomodel$date),
                                   parallel = input$parallel),
          "Hill" = mod_hill(dftomodel,
                            predictor = input$flightdate,
                            dependent = input$traittomodel,
                            sowing_date = min(dftomodel$date),
                            parallel = input$parallel),
          "Exponential-Plateau" = mod_expplat(dftomodel,
                                              predictor = input$flightdate,
                                              dependent = input$traittomodel,
                                              sowing_date = min(dftomodel$date),
                                              parallel = input$parallel),
          "Expolinear" = mod_explinear(dftomodel,
                                       predictor = input$flightdate,
                                       dependent = input$traittomodel,
                                       sowing_date = min(dftomodel$date),
                                       parallel = input$parallel)
        )
      }) |>
        purrr::map_dfr(~.x) |>
        models()
    })

    observe({
      req(models())
      dfs[[input$saveto]] <- create_reactval(input$saveto, models() |> dplyr::select(-parms))

      nplots <- nrow(models())
      converged <- nrow(models() |> dplyr::filter(!is.na(aic)))
      notconverged <- nplots - converged
      convergencerate <- round(converged / nplots * 100, 2)

      content <- tags$span(
        tags$h1(icon("info"), "Summary", style = "color: orange;"),
        tags$p("Growth Models Fitted Successfully. The results are ready for further analysis in the 'Datasets' module.",
               style = "margin-top: 10px;"),
        icon("grip"), tags$b("Number of predictions: "), paste0(nplots), tags$br(),
        icon("circle-check"), tags$b("Converged Models: "), paste0(converged), tags$br(),
        icon("circle-xmark"), tags$b("Not Converged Models: "), paste0(notconverged), tags$br(),
        icon("chart-bar"), tags$b("Convergence Rate: "), paste0(convergencerate, "%"), tags$br()
      )
      waiter_hide()
      show_alert(
        title = NULL,
        text = div(content, style = "text-align: left; line-height: 1.5;"),
        html = TRUE,
        width = 720,
        type = "success"
      )
    })


    output$summaryresults <- reactable::renderReactable({
      req(models())
      models() |>
        dplyr::select(-parms) |>
        roundcols(digits = 3) |>
        render_reactable()
    })

    observe({
      req(models())
      levels <- sort(unique(dfactive$df[["unique_plot"]]))
      updatePickerInput(session, "fittedmodel",
                        choices = levels,
                        selected = levels[[1]])
    })
    observe({
      req(models())
      updatePickerInput(session, "histotraitsgm",
                        choices = colnames(models()),
                        selected = NA)
    })

    observe({
      updatePickerInput(session, "plotunique",
                        choices = sort(unique(dfactive$df[["unique_plot"]])),
                        selected = sort(unique(dfactive$df[["unique_plot"]]))[1])
    })
    observe({
      updatePickerInput(session, "growthmodelmult",
                        choices = sort(unique(models()[["model"]])),
                        selected = sort(unique(models()[["model"]]))[1])
    })
    observe({
      updatePickerInput(session, "plotmultiple",
                        choices = sort(unique(dfactive$df[["unique_plot"]])),
                        selected = sort(unique(dfactive$df[["unique_plot"]]))[1])
    })
    observe({
      updatePickerInput(session, "growthmodelunique",
                        choices = sort(unique(models()[["model"]])),
                        selected = sort(unique(models()[["model"]]))[1])
    })


    observe({
      colorlevels <- reactiveVal(c())
      req(models())
      if(input$modelorplot == 'One model, multiple plots'){

        req(input$plotmultiple, input$growthmodelunique)

        dfpars <-
          models() |>
          dplyr::filter(unique_plot %in% input$plotmultiple,
                        model == input$growthmodelunique)

        dfplot <-
          dfactive$df |>
          dplyr::filter(unique_plot %in% input$plotmultiple) |>
          dplyr::select(dplyr::all_of(c("unique_plot", input$flightdate, input$traittomodel))) |>
          setNames(c("color", "doy", "vindex"))

        if(input$flightdate == "date"){
          dfplot <-
            dfplot |>
            dplyr::mutate(doy = as.integer(difftime(doy, min(doy), units = "days")) + 1)
        }

        colorlevels(dfpars$unique_plot)

        pmod <-
          ggplot() +
          geom_point(aes(x = doy, y = vindex, color = color),
                     data = dfplot,
                     size = 3) +
          lapply(1:nrow(dfpars), function(i) {
            geom_function(fun = get_data_info(dfpars, i, "model"),
                          args = get_data_info(dfpars, i, "coefs"),
                          aes(colour = colorlevels()[[i]]),
                          n = 501,
                          linewidth = 1.5)

          }) +
          labs(x = input$flightdate,
               y = input$traittomodel,
               color = "") +
          theme_bw(base_size = 24) +
          theme(panel.grid.minor = element_blank(),
                legend.position = "bottom")

      } else if(input$modelorplot == 'One plot, multiple models'){


        req(input$plotunique, input$growthmodelmult)

        dfpars <-
          models() |>
          dplyr::filter(unique_plot  == input$plotunique,
                        model %in% input$growthmodelmult)

        dfplot <-
          dfactive$df |>
          dplyr::filter(unique_plot == input$plotunique) |>
          dplyr::select(dplyr::all_of(c("unique_plot", input$flightdate, input$traittomodel))) |>
          setNames(c("color", "doy", "vindex"))

        if(input$flightdate == "date"){
          dfplot <-
            dfplot |>
            dplyr::mutate(doy = as.integer(difftime(doy, min(doy), units = "days")) + 1)
        }

        colorlevels(dfpars$model)

        pmod <-
          ggplot() +
          geom_point(aes(x = doy, y = vindex),
                     data = dfplot,
                     size = 3) +
          lapply(1:nrow(dfpars), function(i) {
            geom_function(fun = get_data_info(dfpars, i, "model"),
                          args = get_data_info(dfpars, i, "coefs"),
                          aes(colour = colorlevels()[[i]]),
                          n = 501,
                          linewidth = 1.5)

          }) +
          labs(x = input$flightdate,
               y = input$traittomodel,
               color = "") +
          theme_bw(base_size = 24) +
          theme(panel.grid.minor = element_blank(),
                legend.position = "bottom")

      }

      output$fittedplot <- renderPlot({
        pmod
      })


      # first derivative
      pfd <-
        ggplot() +
        lapply(1:nrow(dfpars), function(i) {
          geom_function(fun = get_data_info(dfpars, i, "fd"),
                        args = get_data_info(dfpars, i, "coefs"),
                        aes(colour = colorlevels()[[i]]),
                        xlim = c(get_data_info(dfpars, i, "xmin"), get_data_info(dfpars, i, "xmax")),
                        n = 501,
                        linewidth = 1)

        }) +
        labs(
          color = NULL,
          x = input$flightdate,
          y = "1st Derivative (Units/Day)"
        ) +
        theme_bw(base_size = 24) +
        theme(panel.grid.minor = element_blank(),
              legend.position = "bottom")


      output$fderivate <- renderPlot({
        pfd
      })

      # second derivative
      psd <-
        ggplot() +
        lapply(1:nrow(dfpars), function(i) {
          geom_function(fun = get_data_info(dfpars, i, "sd"),
                        args = get_data_info(dfpars, i, "coefs"),
                        aes(colour = colorlevels()[[i]]),
                        xlim = c(get_data_info(dfpars, i, "xmin"), get_data_info(dfpars, i, "xmax")),
                        n = 501,
                        linewidth = 1)

        }) +
        labs(
          color = NULL,
          x = input$flightdate,
          y = "2nd Derivative (Units/Day²)"
        ) +
        theme_bw(base_size = 24) +
        theme(panel.grid.minor = element_blank(),
              legend.position = "bottom")

      output$sderivate <- renderPlot({
        psd
      })

    })


    ### trait distribution
    # Plot the histograms
    output$histogramsgm <- renderPlotly({
      req(input$histotraitsgm)
      req(models())

      dfhist <-
        models() |>
        dplyr::select(dplyr::all_of(input$histotraitsgm)) |>
        dplyr::ungroup() |>
        tidyr::pivot_longer(dplyr::all_of(input$histotraitsgm))


      p <-
        ggplot(dfhist, aes(x = value)) +
        geom_histogram(position = "identity",
                       fill = "forestgreen") +
        facet_wrap(~name, scales = "free") +
        labs(x = "Observed value",
             y = "Number of plots") +
        theme_bw(base_size = 18) +
        theme(panel.grid.minor = element_blank(),
              legend.position = "bottom")
      plotly::ggplotly(p)
    })


    # summary results
    output$summaryplot <- reactable::renderReactable({
      req(models())
      models() |>
        dplyr::select(-parms) |>
        dplyr::group_by(unique_plot) |>
        dplyr::summarise(
          across(
            where(is.numeric),
            list(
              mean = ~mean(., na.rm = TRUE),
              min = ~min(., na.rm = TRUE),
              max = ~max(., na.rm = TRUE),
              n = ~sum(!is.na(.))
            )
          )
        ) |>
        round_cols(digits = 3) |>
        render_reactable()
    })

    output$summarymodel <- reactable::renderReactable({
      req(models())
      models() |>
        dplyr::select(-parms) |>
        dplyr::group_by(model) |>
        dplyr::summarise(
          across(
            where(is.numeric),
            list(
              mean = ~mean(., na.rm = TRUE),
              min = ~min(., na.rm = TRUE),
              max = ~max(., na.rm = TRUE),
              n = ~sum(!is.na(.))
            )
          )
        ) |>
        round_cols(digits = 3) |>
        render_reactable()
    })


    observeEvent(input$savetoglobalenv, {
      req(models())
      tf <- tempfile(pattern = "plimanshiny_output", fileext = ".RData")
      plimanshiny_growth_models <- models()
      save(plimanshiny_growth_models, file = tf)
      ask_confirmation(
        inputId = "myconfirmation",
        type = "warning",
        title = "Close the App?",
        text = glue::glue("The results were saved in a temporary file ({basename(tf)}).
              To access the created object, you need first to stop the App and run
              get_plimanshiny_results()
              to load the list into your R environment.
              Do you really want to close the app now?"),
        btn_labels = c("Nope", "Yep"),
        btn_colors = c("#FE642E", "#04B404")
      )
    })

    observe({
      if (!is.null(input$myconfirmation)) {
        if (input$myconfirmation) {
          stopApp()
        } else {
          return()
        }
      }
    })

  })
}

## To be copied in the UI
# mod_growthmodels_ui("growthmodels_1")

## To be copied in the server
# mod_growthmodels_server("growthmodels_1")
