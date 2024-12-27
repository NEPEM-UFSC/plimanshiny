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
          height = "790px",
          pickerInput(
            ns("dftoedit"),
            label = "Time series data",
            choices = NULL
          ),
          pickerInput(
            ns("flightdate"),
            label = "Flight date",
            choices = NULL
          ),
          pickerInput(
            ns("traittomodel"),
            label = "Trait to model",
            choices = NULL
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
            label = "Predict!",
            icon = icon("check")
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
                pickerInput(
                  inputId = ns("fittedmodel"),
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
          title = "Details about the growth model",
          width = 12,
          headerBorder = FALSE,
          collapsible = TRUE,
          closable = TRUE,
          withMathJax(),
          help_mod_L4_gm (),
          footer = NULL,
          easyClose = TRUE,
          size = "xl"
        )
      )
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



    observeEvent(input$predictmat, {

      req(dfactive$df)
      req(input$traittomodel)

      waiter_show(
        html = tagList(
          spin_google(),
          h2("{plimanshiny} is now fitting the growth models. Please wait while we finalize everything...")
        ),
        color = "#228B227F"
      )


      # summary results
      models <- reactive({
        req(dfactive$df)
        dfactive$df |>
          sf::st_drop_geometry() |>
          as.data.frame() |>
          mod_L4(predictor = input$traittomodel,
                 sowing_date = min(dfactive$df$date),
                 parallel = input$parallel) |>
          dplyr::select(-c(heading, maturity, repr_period, auc_repr_period)) |>
          dplyr::rowwise() |>
          dplyr::mutate(
            # Run optimise for the 'fd' function once and capture the result
            yinfp_result = list(optimise(
              f = function(x) parms[[1]]$fd(x, b0 = b0, b1 = b1, b2 = b2, b3 = inflection),
              interval = c(parms[[1]]$xmin, parms[[1]]$xmax - 20),
              maximum = TRUE
            )),
            xinfp = yinfp_result$maximum,  # Extract the maximum point
            yinfp = yinfp_result$objective,  # Extract the objective for maximum

            # Run optimise for the 'sd' function to get the maximum
            sd_result = list(optimise(
              f = function(x) parms[[1]]$sd(x, b0 = b0, b1 = b1, b2 = b2, b3 = inflection),
              interval = c(parms[[1]]$xmin, parms[[1]]$xmax - 20),
              maximum = TRUE
            )),
            xmace = sd_result$maximum,  # Maximum point for sd
            ymace = sd_result$objective,  # Maximum value for sd

            # Calculate the minimum using -sd
            pmdes_result = list(optimise(
              f = function(x) parms[[1]]$sd(x, b0 = b0, b1 = b1, b2 = b2, b3 = inflection),
              interval = c(parms[[1]]$xmin, parms[[1]]$xmax - 20)
            )),
            xmdes = pmdes_result$minimum,  # Minimum point for sd (from -sd)
            ymdes = pmdes_result$objective,  # Minimum value for sd (negated)
            .after = auc
          )  |>
          dplyr::ungroup()  |>
          dplyr::select(-c(yinfp_result, sd_result, pmdes_result))
      })

      observe({
        req(models())
        dfs[["growth_models"]] <- create_reactval("growth_models", models() |> dplyr::select(-parms))
        waiter_hide()
        sendSweetAlert(
          session = session,
          title = "Growth Models Fitted Successfully!",
          text = "The canopy height model time series has been analyzed. Explore the results in the tabs to gain insights from your data.",
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
        dfactive$df <- dfactive$df |> sf::st_drop_geometry()
        req(input$fittedmodel)

        dfpars <-
          models() |>
          dplyr::filter(unique_plot %in% input$fittedmodel)

        df_int <-
          dplyr::tibble(flights = seq(dfpars$parms[[1]][[1]]$xmin, dfpars$parms[[1]][[1]]$xmax-20, length.out = 1000)) |>
          as.data.frame()
        ypred <- predict(dfpars$parms[[1]][[1]]$modeladj, newdata = df_int)
        df_int <- dplyr::bind_cols(df_int, data.frame(y = ypred))
        dfplot <-
          dfactive$df |>
          dplyr::filter(unique_plot %in% input$fittedmodel) |>
          dplyr::select(dplyr::all_of(c("unique_plot", "date", input$traittomodel))) |>
          dplyr::mutate(date = as.integer(difftime(date, min(date), units = "days")) + 1) |>
          setNames(c("unique_plot", "doy", "vindex"))

        colorsss <- ggplot_color(4)
        pmod <-
          ggplot() +
          geom_point(aes(x = doy, y = vindex, color = unique_plot),
                     data = dfplot,
                     size = 3)

        for (i in 1:nrow(dfpars)) {
          pmod <-
            pmod +
            stat_function(
              fun = dfpars$parms[[i]][[1]]$model,
              xlim = c(dfpars$parms[[i]][[1]]$xmin, dfpars$parms[[i]][[1]]$xmax-20),
              args = dfpars$parms[[i]][[1]]$coefs,
              color = colorsss[i],
              linewidth = 1.5,
              n = 500
            )
        }

        pmod <-
          pmod +
          scale_color_manual(values = colorsss[1:length(input$fittedmodel)]) +
          labs(x = "Days after first flight",
               y = input$traittomodel,
               color = "") +
          theme_bw(base_size = 24) +
          theme(panel.grid.minor = element_blank(),
                legend.position = "bottom")

        output$fittedplot <- renderPlot({
          pmod
        })

        # first derivative
        pfd <- ggplot()
        for (i in 1:nrow(dfpars)) {
          pfd <-
            pfd +
            stat_function(
              fun = dfpars$parms[[i]][[1]]$fd,
              xlim = c(dfpars$parms[[i]][[1]]$xmin, dfpars$parms[[i]][[1]]$xmax-20),
              args = dfpars$parms[[i]][[1]]$coefs,
              color = colorsss[i],
              linewidth = 1.5,
              n = 500
            )
        }
        pfd <-
          pfd +
          labs(x = "Days after first flight",
               y = "Growth rate (units of y per day)"
          ) +
          scale_color_manual(
            name = "Unique Plot",                   # Legend title
            values = setNames(colorsss, input$fittedmodel) # Set colors and labels for the legend
          ) +
          theme_bw(base_size = 24) +
          theme(panel.grid.minor = element_blank())

        output$fderivate <- renderPlot({
          pfd
        })

        # second derivative
        psd <- ggplot()
        for (i in 1:nrow(dfpars)) {
          psd <-
            psd +
            stat_function(
              fun = dfpars$parms[[i]][[1]]$sd,
              xlim = c(dfpars$parms[[i]][[1]]$xmin, dfpars$parms[[i]][[1]]$xmax-20),
              args = dfpars$parms[[i]][[1]]$coefs,
              color = colorsss[i],
              linewidth = 1.5,
              n = 500
            )
        }
        psd <-
          psd +
          labs(x = "Days after first flight",
               y = "Second derivative") +
          scale_color_manual(
            name = "Unique Plot",
            values = setNames(colorsss, input$fittedmodel)
          ) +
          theme_bw(base_size = 24) +
          theme(panel.grid.minor = element_blank())

        output$sderivate <- renderPlot({
          psd
        })

      })

    })


  })
}

## To be copied in the UI
# mod_growthmodels_ui("growthmodels_1")

## To be copied in the server
# mod_growthmodels_server("growthmodels_1")
