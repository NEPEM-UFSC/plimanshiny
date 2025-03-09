#' matanalyze UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_matanalyze_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Plant Maturity and Heading",
          collapsible = FALSE,
          awesomeRadio(
            inputId = ns("inputmat"),
            label = "Entry method",
            choices = c("Imported Dataset", "Example Dataset"),
            selected = "Imported Dataset",
            status = "success",
            inline = TRUE
          ),
          hl(),
          width = 12,
          dateInput(
            ns("sowing"),
            label = "Sowing Date",
            value = Sys.Date()
          ),
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
            ns("vegetindex"),
            label = "Predictor Vegetation Index",
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
          prettyCheckbox(
            inputId = ns("usethresh"),
            label = "Threshold-based methods",
            value = FALSE,
            icon = icon("check"),
            status = "success",
            animation = "rotate"
          ),
          conditionalPanel(
            condition = "input.usethresh == false", ns = ns,
            pickerInput(
              ns("method"),
              label = "Method for prediction",
              choices = c("Logistic Model L3",
                          "Logistic Model L4",
                          "Logistic Model L5",
                          "Segmented Regression")
            )
          ),
          conditionalPanel(
            condition = "input.usethresh == true", ns = ns,
            pickerInput(
              ns("methodthresh"),
              label = "Method for prediction",
              choices = c("LOESS (Volpato et al., 2021)",
                          "Segmented Regression (Volpato et al., 2021)")
            ),
            numericInput(
              ns("thresh"),
              label = "Threshold",
              value = 0
            ),
            conditionalPanel(
              condition = "input.methodthresh == 'LOESS (Volpato et al., 2021)'", ns = ns,
              numericInput(
                ns("span"),
                label = "Span",
                value = 0.75
              )
            )

          ),

          textInput(
            ns("saveto"),
            label = "Save results to...",
            value = "time_series_maturity"
          ),
          pickerInput(
            ns("shapefiletoexplore"),
            label = "Shapefile to explore",
            choices = NULL
          ),
          actionBttn(
            ns("predictmat"),
            label = "Predict!",
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
        uiOutput(ns("uimat"))
      )
    )
  )
}

#' matanalyze Server Functions
#'
#' @noRd
mod_matanalyze_server <- function(id, dfs, shapefile, basemap, settings){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #
    observeEvent(input$details, {
      showModal(
        modalDialog(
          title = "Details about the prediction",
          width = 12,
          headerBorder = FALSE,
          collapsible = TRUE,
          closable = TRUE,
          # Replace bs4TabCard with box for simpler rendering in the modal
          box(
            title = "Logistic Model L3",
            status = "primary",
            width = 12,
            collapsed = TRUE,
            collapsible = TRUE,
            solidHeader = TRUE,
            help_mod_L3()  # Add content for Model L3
          ),
          box(
            title = "Logistic Model L4",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsed = TRUE,
            collapsible = TRUE,
            help_mod_L4()
          ),
          box(
            title = "Logistic Model L5",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsed = TRUE,
            collapsible = TRUE,
            help_mod_L5()
          ),
          box(
            title = "locally estimated scatterplot smoothing (LOESS)",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsed = TRUE,
            collapsible = TRUE,
            help_mod_loess()
          ),
          box(
            title = "Segmented Regression",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsed = TRUE,
            collapsible = TRUE,
            help_mod_segmented()
          ),
          footer = NULL,
          easyClose = TRUE,
          size = "xl"
        )
      )
    })

    refs <- tabPanel(
      title = "Home",
      fluidRow(
        col_9(
          img(src = "www/logomat.jpg", width = "100%", height = "90%")
        ),
        col_3(
          h2("About"),
          "This module provides the implementation of several methods for estimating plant heading/maturity.
                Methods are grouped into those that uses or not a threshold value.", br(),br(),
          h2("Disclaimer"),
          "We welcome feedback and suggestions about the usefulness of the application and make no guarantee of the correctness,
          reliability, or utility of the results if incorrect selections are made during the steps of DM estimation.", br(),br(),
          shiny::actionButton(inputId= ns("details"),
                              label="Method details",
                              icon = icon("circle-info"))
        )
      )
    )

    output$uimat <- renderUI({
      if(input$usethresh){
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "790px",
          title = "Overview",
          selected = "Home",
          solidHeader = FALSE,
          type = "tabs",
          refs,
          tabPanel(
            title = "Overview",
            plotlyOutput(ns("overview"), height = "700px") |> add_spinner()
          ),
          tabPanel(
            title = "Fitted model",
            pickerInput(
              inputId = ns("fittedmodel"),
              label = "Select a unique_id to plot the model:",
              choices = NULL,
              options = list(
                "actions-box" = TRUE,
                "live-search" = TRUE,
                "max-options" = 3,
                "max-options-text" = "No more levels allowed"
              )
            ),
            tabPanel("Fitted curve",
                     plotOutput(ns("fittedplot"), height = "550px") |> add_spinner()
            )
          ),
          tabPanel(
            title = "Results",
            reactable::reactableOutput(ns("tabresult"), height = "720px")  |> add_spinner()
          ),
          tabPanel(
            title = "Trait distribution",
            pickerInput(
              ns("histotraits"),
              label = "Select trait(s) to plot:",
              choices = NULL,
              multiple = TRUE
            ),
            plotlyOutput(ns("histograms"), height = "680px")  |> add_spinner()
          ),
          tabPanel(
            title = "Explore field",
            fluidRow(
              col_3(
                selectInput(ns("plotattribute"),
                            label = "Attribute",
                            choices = NULL)
              ),
              col_3(
                pickerpalette(id, "palplot", selected = "set2"),

              ),
              col_2(
                prettyCheckbox(
                  inputId = ns("palplotrev"),
                  label = "Reverse",
                  value = FALSE,
                  icon = icon("check"),
                  status = "success",
                  animation = "rotate"
                )
              ),
              col_4(
                sliderInput(ns("alpharesplot"),
                            label = "Fill opacity",
                            min = 0,
                            max = 1,
                            value = 0.75),
              )
            ),
            leafletOutput(ns("map"), height = "640px") |> add_spinner()
          )
        )
      } else if(!input$usethresh){
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "790px",
          title = "Overview",
          selected = "Home",
          solidHeader = FALSE,
          type = "tabs",
          refs,
          tabPanel(
            title = "Overview",
            plotlyOutput(ns("overview"), height = "700px") |> add_spinner()
          ),
          tabPanel(
            title = "Fitted model",
            pickerInput(
              inputId = ns("fittedmodel"),
              label = "Select a unique_id to plot the model:",
              choices = NULL,
              options = list(
                "actions-box" = TRUE,
                "live-search" = TRUE,
                "max-options" = 3,
                "max-options-text" = "No more levels allowed"
              )
            ),
            bs4TabCard(
              id = "tabs",
              status = "success",
              width = 12,
              height = "600px",
              title = "Results for the fitted model",
              type = "tabs",
              selected = "Fitted curve",
              tabPanel("Fitted curve",
                       plotOutput(ns("fittedplot"), height = "550px") |> add_spinner()
              ),
              tabPanel("First derivative",
                       plotOutput(ns("fderivate"), height = "550px") |> add_spinner()
              ),
              tabPanel("Second derivative",
                       plotOutput(ns("sderivate"), height = "550px") |> add_spinner()
              )
            )
          ),
          tabPanel(
            title = "Results",
            reactable::reactableOutput(ns("tabresult"), height = "720px")  |> add_spinner()
          ),
          tabPanel(
            title = "Trait distribution",
            pickerInput(
              ns("histotraits"),
              label = "Select trait(s) to plot:",
              choices = NULL,
              multiple = TRUE
            ),
            plotlyOutput(ns("histograms"), height = "680px")  |> add_spinner()
          ),

          tabPanel(
            title = "Explore field",
            fluidRow(
              col_3(
                selectInput(ns("plotattribute"),
                            label = "Attribute",
                            choices = NULL)
              ),
              col_3(
                pickerpalette(id, "palplot", selected = "set2"),

              ),
              col_2(
                prettyCheckbox(
                  inputId = ns("palplotrev"),
                  label = "Reverse",
                  value = FALSE,
                  icon = icon("check"),
                  status = "success",
                  animation = "rotate"
                )
              ),
              col_4(
                sliderInput(ns("alpharesplot"),
                            label = "Fill opacity",
                            min = 0,
                            max = 1,
                            value = 0.75),
              )
            ),
            leafletOutput(ns("map"), height = "640px") |> add_spinner()
          )
        )
      }
    })

    # The example dataset is a sample of 10 plots obtained after time series analysis
    # of vegetation indexes, kindly provided by Leonardo Volpato https://www.linkedin.com/in/leonardo-volpato/
    observeEvent(input$inputmat, {
      if(input$inputmat == "Example Dataset"){
        filepath <- file.path(system.file(package = "plimanshiny"), "app/www/timeseries_vi.csv")
        dfs[["df_timeseries_vi"]] <- create_reactval("df_timeseries_vi",  read.csv(filepath))
        observe({
          updatePickerInput(session, "dftoedit",
                            choices = c("none", names(dfs)),
                            selected = "df_timeseries_vi")
          updateDateInput(session, "sowing",
                          value = as.Date("2024-11-07"))
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

    observe({
      req(dfactive$df)
      updatePickerInput(session, "vegetindex",
                        choices = colnames(dfactive$df),
                        selected = NA)
    })
    observe({
      req(dfactive$df)
      updatePickerInput(session, "flightdate",
                        choices = colnames(dfactive$df),
                        selected = "date")
    })
    observe({
      updatePickerInput(session, "shapefiletoexplore",
                        choices = setdiff(names(shapefile), c("shapefile", "shapefileplot")))
    })




    output$overview <- renderPlotly({
      req(dfactive$df)
      req(input$vegetindex)
      p <-
        ggplot(dfactive$df, aes(x = .data[[input$flightdate]], y = .data[[input$vegetindex]], group = 1)) +
        geom_boxplot(fill = "#28a745") +
        geom_smooth(method = 'loess', formula = 'y ~ x', se = FALSE) +
        labs(x = input$flightdate, y = input$vegetindex) +
        theme_bw(base_size = 18) +
        theme(panel.grid.minor = element_blank())
      plotly::ggplotly(p)
    })


    modl <- reactiveVal()
    observeEvent(input$predictmat, {
      req(dfactive$df)
      req(input$vegetindex)

      waiter_show(
        html = tagList(
          spin_google(),
          h2("Predictions in progress. Please, wait.")
        ),
        color = "#228B227F"
      )

      # modl <- reactive({
      if(input$method == "Logistic Model L3" & !input$usethresh){
        # modl <-
        dfactive$df |>
          mod_L3(predictor = input$vegetindex,
                 flight_date = input$flightdate,
                 sowing_date = input$sowing,
                 parallel = input$parallel) |>
          modl()
      } else if(input$method == "Logistic Model L4" & !input$usethresh){
        # modl <-
        dfactive$df |>
          mod_L4(predictor = input$vegetindex,
                 flight_date = input$flightdate,
                 sowing_date = input$sowing,
                 parallel = input$parallel) |>
          modl()
      } else if(input$method == "Logistic Model L5" & !input$usethresh){
        # modl <-
        dfactive$df |>
          mod_L5(predictor = input$vegetindex,
                 flight_date = input$flightdate,
                 sowing_date = input$sowing,
                 parallel = input$parallel) |>
          modl()
      } else if(input$methodthresh == "LOESS (Volpato et al., 2021)" & input$usethresh){
        # modl <-
        dfactive$df |>
          mod_loess(predictor = input$vegetindex,
                    flight_date = input$flightdate,
                    sowing_date = input$sowing,
                    threshold = input$thresh,
                    span = input$span,
                    parallel = input$parallel) |>
          modl()

      } else if(input$method == "Segmented Regression" & !input$usethresh){
        dfactive$df |>
          mod_segmented2(predictor = input$vegetindex,
                         flight_date = input$flightdate,
                         sowing_date = input$sowing,
                         parallel = input$parallel) |>
          modl()

      } else if(input$methodthresh == "Segmented Regression (Volpato et al., 2021)" & input$usethresh){
        # modl <-
        dfactive$df |>
          mod_segmented(predictor = input$vegetindex,
                        flight_date = input$flightdate,
                        sowing_date = input$sowing,
                        threshold = input$thresh,
                        parallel = input$parallel) |>
          modl()
      }
      req(modl())
      waiter_hide()
    })

    # send to dataset
    observe({
      req(modl())
      dfs[[input$saveto]] <- create_reactval(input$saveto, modl() |> dplyr::select(-parms))
    })

    # Save to a temp file
    observeEvent(input$savetoglobalenv, {
      req(modl())
      tf <- tempfile(pattern = "plimanshiny_output", fileext = ".RData")
      plimanshiny_growth_models <- modl()
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

    observe({
      req(modl())
      nplots <- nrow(modl())
      converged <- nrow(modl() |> dplyr::filter(!is.na(maturity)))
      notconverged <- nplots - converged
      convergencerate <- round(converged / nplots * 100, 2)

      content <- tags$span(
        tags$h1(icon("info"), "Prediction Summary", style = "color: orange;"),
        tags$p("The predictions have been successfully completed and are ready for further analysis in the 'Datasets' module.",
               style = "margin-top: 10px;"),
        icon("grip"), tags$b("Number of plots: "), paste0(nplots), tags$br(),
        icon("circle-check"), tags$b("Converged Models: "), paste0(converged), tags$br(),
        icon("circle-xmark"), tags$b("Not Converged Models: "), paste0(notconverged), tags$br(),
        icon("chart-bar"), tags$b("Convergence Rate: "), paste0(convergencerate, "%"), tags$br()
      )

      show_alert(
        title = NULL,
        text = div(content, style = "text-align: left; line-height: 1.5;"),
        html = TRUE,
        width = 720,
        type = "success"
      )
    })



    observe({
      req(modl())
      updateSelectInput(session, "plotattribute",
                        choices = colnames(modl()),
                        selected = "q90")
    })

    observe({
      req(modl())
      updatePickerInput(session, "histotraits",
                        choices = colnames(modl()),
                        selected = NA)
    })


    # Plot the results
    output$tabresult <- reactable::renderReactable({
      req(modl())
      modl() |>
        dplyr::select(-parms) |>
        roundcols(digits = 3) |>
        render_reactable()
    })




    # Plot the histograms
    output$histograms <- renderPlotly({
      req(input$histotraits)
      req(modl())

      dfhist <-
        modl() |>
        dplyr::select(dplyr::all_of(input$histotraits)) |>
        dplyr::ungroup() |>
        tidyr::pivot_longer(dplyr::all_of(input$histotraits))


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


    observe({
      levels <- sort(unique(dfactive$df[["unique_plot"]]))
      updatePickerInput(session, "fittedmodel",
                        choices = levels)

    })



    observe({
      req(modl())
      # if threshold is not used
      # models L3, L4 and L5
      if(input$usethresh){
        req(input$fittedmodel)
        req(modl())
        dfplot <-
          dfactive$df |>
          dplyr::select(dplyr::all_of(c("unique_plot", input$flightdate, input$vegetindex))) |>
          setNames(c("unique_plot", "doy", "vindex")) |>
          dplyr::mutate(doy = as.numeric(round(difftime(to_datetime(doy), to_datetime(input$sowing), units = "days")))) |>
          dplyr::filter(!!dplyr::sym("unique_plot") %in% input$fittedmodel)


        dfpars <-
          modl() |>
          dplyr::filter(unique_plot == input$fittedmodel)

        output$fittedplot <- renderPlot({
          ggplot(dfplot, aes(x = doy, y = vindex)) +
            geom_point() +
            geom_smooth(method = 'loess',
                        formula = "y ~ x",
                        se = FALSE) +
            geom_vline(xintercept = dfpars$maturity, color = "salmon") +
            geom_hline(yintercept = input$thresh, linetype = 2, color = "salmon") +
            labs(x = "Days after sowing",
                 y = input$vegetindex) +
            theme_bw(base_size = 18) +
            theme(panel.grid.minor = element_blank(),
                  legend.position = "bottom")

        })
        # waiter_hide()
      } else if(!input$usethresh){
        req(input$fittedmodel)
        req(modl())
        dfplot <-
          dfactive$df |>
          dplyr::select(dplyr::all_of(c("unique_plot", input$flightdate, input$vegetindex))) |>
          setNames(c("unique_plot", "doy", "vindex")) |>
          dplyr::mutate(doy = as.numeric(round(difftime(to_datetime(doy), to_datetime(input$sowing), units = "days")))) |>
          dplyr::filter(!!dplyr::sym("unique_plot") %in% input$fittedmodel)

        dfpars <-
          modl() |>
          dplyr::filter(unique_plot == input$fittedmodel)

        #
        # Plot the fitted models
        observe({
          if(input$method != "Segmented Regression"){
            output$fittedplot <- renderPlot({

              df_int <-
                dplyr::tibble(flights = seq(dfpars$parms[[1]][[1]]$xmin, dfpars$parms[[1]][[1]]$xmax, length.out = 1000),
                              class = ifelse(flights < dfpars$heading, "Vegetative", "Reproductive")) |>
                as.data.frame()
              ypred <- predict(dfpars$parms[[1]][[1]]$modeladj, newdata = df_int)
              df_int <- dplyr::bind_cols(df_int, data.frame(y = ypred))


              pmod <-
                ggplot() +
                geom_point(aes(x = doy, y = vindex),
                           data = dfplot) +
                geom_function(fun = get_data_info(dfpars, 1, "model"),
                              args = get_data_info(dfpars, 1, "coefs"),
                              xlim = c(get_data_info(dfpars, 1, "xmin"), get_data_info(dfpars, 1, "xmax"))) +
                geom_ribbon(data = df_int,
                            aes(x = flights,
                                ymin = min(y),
                                ymax =  y,
                                fill = class),
                            alpha = 0.5) +
                geom_vline(aes(xintercept = dfpars$heading), color = "red") +
                geom_vline(xintercept = dfpars$maturity, color = "salmon") +
                labs(x = "Days after sowing",
                     y = input$vegetindex,
                     fill = "Phase") +
                theme_bw(base_size = 18) +
                theme(panel.grid.minor = element_blank(),
                      legend.position = "bottom")

              fd <-
                ggplot() +
                geom_function(fun = get_data_info(dfpars, 1, "fd"),
                              args = get_data_info(dfpars, 1, "coefs"),
                              xlim = c(get_data_info(dfpars, 1, "xmin"), get_data_info(dfpars, 1, "xmax"))) +
                labs(x = "Days after sowing",
                     y = "First derivative") +
                geom_vline(aes(xintercept = dfpars$heading), color = "red") +
                geom_vline(xintercept = dfpars$maturity, color = "salmon") +
                theme_bw(base_size = 18) +
                theme(panel.grid.minor = element_blank())
              #
              sd <-
                ggplot() +
                geom_function(fun = get_data_info(dfpars, 1, "sd"),
                              args = get_data_info(dfpars, 1, "coefs"),
                              xlim = c(get_data_info(dfpars, 1, "xmin"), get_data_info(dfpars, 1, "xmax"))) +
                labs(x =  "Days after sowing",
                     y = "Second derivative") +
                geom_vline(aes(xintercept = dfpars$heading), color = "red") +
                geom_vline(xintercept = dfpars$maturity, color = "salmon") +
                theme_bw(base_size = 18) +
                theme(panel.grid.minor = element_blank())

              output$fderivate <- renderPlot({
                fd
              })

              output$sderivate <- renderPlot({
                sd
              })
              pmod
            })
          } else{
            output$fittedplot <- renderPlot({
              ggplot() +
                geom_point(aes(x = doy, y = vindex),
                           data = dfplot) +
                # add two lines from segmented regression
                geom_abline(intercept = dfpars$parms[[1]][[1]]$coefs$intercepts[1],
                            slope = dfpars$parms[[1]][[1]]$coefs$slopes[1],
                            color = "red") +
                geom_abline(intercept = dfpars$parms[[1]][[1]]$coefs$intercepts[2],
                            slope = dfpars$parms[[1]][[1]]$coefs$slopes[2],
                            color = "red") +
                geom_vline(xintercept = dfpars$maturity, color = "salmon", linetype = 2) +
                labs(x = "Days after sowing",
                     y = input$vegetindex,
                     fill = "Phase") +
                theme_bw(base_size = 18) +
                theme(panel.grid.minor = element_blank(),
                      legend.position = "bottom")

            })
          }
        })

        # waiter_hide()
      }



    })



    # Explore the results Map
    output$map <- renderLeaflet({
      req(shapefile[[input$shapefiletoexplore]]$data)
      req(input$plotattribute)
      if(input$plotattribute %in% colnames(shapefile[[input$shapefiletoexplore]]$data)){
        if(is.null(basemap$map)){
          shapefile_view(shapefile[[input$shapefiletoexplore]]$data,
                         attribute = input$plotattribute,
                         color_regions = return_colors(input$palplot, reverse = input$palplotrev),
                         alpha.regions = input$alpharesplot)
        } else{
          mshp <- shapefile_view(shapefile[[input$shapefiletoexplore]]$data,
                                 attribute = input$plotattribute,
                                 color_regions = return_colors(input$palplot, reverse = input$palplotrev),
                                 alpha.regions = input$alpharesplot)
          (basemap$map +  mshp)@map
        }
      }

    })
  })
}

## To be copied in the UI
# mod_matanalyze_ui("matanalyze_1")

## To be copied in the server
# mod_matanalyze_server("matanalyze_1")
