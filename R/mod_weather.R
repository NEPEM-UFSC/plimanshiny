#' weather UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_weather_ui <- function(id) {
  ns <- NS(id)

  tagList(
    bs4TabCard(
      width = 12,
      selected = "Coordinate selection",
      icon = icon("gears"),
      status  = "success",
      type = "tabs",
      tabPanel(
        title = "Coordinate selection",
        fluidRow(
          col_7(
            leafletOutput(ns("map2"), height = "740px")
          ),
          col_5(
            bs4TabCard(
              width = 12,
              selected = "Input Parameters",
              icon = icon("gears"),
              status  = "success",
              type = "tabs",
              tabPanel(
                title = "Input Parameters",
                dateRangeInput(ns("dates"), "Select the period", start = Sys.Date() - 30, end = Sys.Date()),
                # botão para condição de usar ou nao nome de municipios
                # checkboxInput(ns("use_mun"), "Use municipality names", value = FALSE),
                prettySwitch(
                  inputId = ns("use_mun"),
                  label = "Search by municipality (Brazil only)",
                  value = FALSE,
                  status = "success",
                  fill = TRUE
                ),
                conditionalPanel(
                  condition = "input.use_mun == true", ns = ns,
                  fluidRow(
                    col_6(
                      pickerInput(
                        inputId = ns("state"),
                        label = "Select the state",
                        choices = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO",
                                    "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE",
                                    "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP",
                                    "SE", "TO"),
                        multiple = TRUE,
                        options = list(
                          `actions-box` = TRUE,
                          `live-search` = TRUE
                        )
                      )
                    ),
                    col_6(
                      pickerInput(
                        inputId = ns("mun"),
                        label = "Select the municipality",
                        choices = NULL,
                        multiple = TRUE,
                        options = list(
                          `actions-box` = TRUE,
                          `live-search` = TRUE
                        )
                      )
                    )
                  )

                ),
                textInput(ns("envname"),
                          label = "Environment name",
                          value = ""),
                prettyRadioButtons(
                  inputId = ns("scale"),
                  label = "Select the scale",
                  choices = c("hourly", "daily", "monthly", "climatology"),
                  selected = "daily",
                  inline = TRUE,
                  status = "success"
                ),
                pickerInput(
                  inputId = ns("params"),
                  label = "Select the parameters",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE,
                    size = 20
                  )
                ),
                fluidRow(
                  col_4(
                    shinyWidgets::actionBttn(
                      inputId = ns("get_weather"),
                      label = "Fetch data",
                      style = "material-flat",
                      color = "primary", # azul (bootstrap)
                      icon = icon("cloud-sun"),
                      size = "md"
                    ),
                  ),
                  col_4(
                    shinyWidgets::actionBttn(
                      inputId = ns("clear_points"),
                      label = "Clear points",
                      style = "material-flat",
                      color = "primary",
                      icon = icon("eraser"),
                      size = "md"
                    ),
                  ),
                  col_2(
                    prettyCheckbox(
                      inputId = ns("parallel"),
                      label = "Parallel",
                      value = FALSE,
                    )
                  ),
                  col_2(
                    numericInput(
                      inputId = ns("ncores"),
                      label = "Cores",
                      value = 1,
                      min = 1,
                      max = 6,
                      step = 1
                    )
                  )
                ),

              ),
              tabPanel(
                title = "Selected points",
                DT::dataTableOutput(ns("latlondata"), height = "560px")
              )
            )
          )
        )
      ),
      tabPanel(
        title = "Weather data",
        reactable::reactableOutput(ns("weather_table"), height = "720px")
      ),
      tabPanel(
        title = "Distribution",
        fluidRow(
          col_4(
            pickerInput(ns("variable"),
                        label = "Select the variable",
                        choices = NULL,
                        selected = NULL,
                        options = list(
                          `actions-box` = TRUE,
                          `live-search` = TRUE
                        )
            )
          ),
          col_4(
            pickerInput(ns("facet"),
                        label = "Facet by",
                        choices = NULL,
                        selected = NULL,
                        options = list(
                          `actions-box` = TRUE,
                          `live-search` = TRUE
                        )
            )
          )
        ),
        plotOutput(ns("envirotypes_dist"), height = "560px")
      ),
      tabPanel(
        title = "Envirotypes",
        fluidRow(
          col_6(
            "Envirotypes",
            fluidRow(
              col_3(
                textInput(ns("quantiles"),
                          label = "Quantiles")
              ),
              col_3(
                textInput(ns("quantiles_label"),
                          label = "Quantile labels (labels)")
              ),
              col_3(
                textInput(ns("cropdates"),
                          label = "Crop stages")
              ),
              col_3(
                textInput(ns("cropdates_label"),
                          label = "Crop stages (labels)")
              )
            ),
            plotOutput(ns("envirotypes"), height = "700px")
          ),
          col_6(
            "Dataset",
            reactable::reactableOutput(ns("dataenviro"), height = "720px")
          )
        )
      )
    )
  )
}

#' weather Server Functions
#'
#' @noRd
mod_weather_server <- function(id, dfs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      if(input$parallel) {
        shinyjs::enable("ncores")
        ncore <- parallel::detectCores()
        updateNumericInput(session, "ncores",
                           value = min(c(ncore, 5)),
                           max = ncore)
      } else {
        shinyjs::disable("ncores")
      }
    })
    # Armazena todos os pontos clicados
    coords <- reactive({
      if (length(points$data) == 0) return(NULL)
      df <- do.call(rbind, points$data)
      as.data.frame(df)
    })

    points <- reactiveValues(data = list())

    # Renderiza o mapa inicial
    output$map2 <- renderLeaflet({
      leaflet() |>
        addProviderTiles(providers$OpenStreetMap) |>
        setView(lng = 0, lat = 0, zoom = 2)
    })

    # Adiciona um novo ponto ao data.frame existente
    observeEvent(input$map2_click, {
      click <- input$map2_click
      new_point <- data.frame(
        env = input$envname,
        lat = round(click$lat, 4),
        lon = round(click$lng, 4),
        start = input$dates[[1]],
        end = input$dates[[2]]
      )
      points$data[[length(points$data) + 1]] <- new_point
    })

    observe({
      if(input$use_mun) {
        req(input$state)
        listmun <-
          read.csv(file = system.file("app/www/coords_muni.csv", package = "plimanshiny", mustWork = TRUE), sep = ",") |>
          dplyr::filter(abbrev_state %in% input$state)

        # Atualiza a lista de municípios com base no estado selecionado
        updatePickerInput(
          session,
          "mun",
          choices = listmun$name_muni,
          selected = NA
        )
      }
    })
    observeEvent(input$clear_points, {
      points$data <- list()
      leafletProxy("map2") |>
        clearMarkers()
    })
    observeEvent(input$mun, {
      req(input$state, input$mun)

      mun_df <- read.csv(file = system.file("app/www/coords_muni.csv", package = "plimanshiny", mustWork = TRUE), sep = ",")

      selected_mun <- dplyr::filter(mun_df,
                                    abbrev_state %in% input$state,
                                    name_muni %in% input$mun) |>
        dplyr::arrange(abbrev_state, name_muni)

      new_points <- purrr::pmap(
        list(selected_mun$name_muni, selected_mun$lat, selected_mun$lon),
        function(env, lat, lon) {
          data.frame(
            env = env,
            lat = round(lat, 4),
            lon = round(lon, 4),
            start = input$dates[[1]],
            end = input$dates[[2]],
            stringsAsFactors = FALSE
          )
        }
      )

      # Adiciona cada município selecionado como um novo ponto
      for (pt in new_points) {
        points$data[[length(points$data) + 1]] <- pt
      }
    })



    # Obtém o clima apenas para o último ponto clicado
    output$latlondata <- DT::renderDT({
      req(length(points$data) > 0)

      # monta data.frame dos pontos
      points_df <- isolate(do.call(rbind, points$data))
      points_df <- as.data.frame(points_df, stringsAsFactors = FALSE)
      colnames(points_df) <- c("env", "lat", "lon", "start", "end")

      # round coords
      points_df$lat <- round(as.numeric(points_df$lat), 4)
      points_df$lon <- round(as.numeric(points_df$lon), 4)

      # Botões JS
      points_df <- transform(
        points_df,
        Delete = sprintf('<button class="delete-point-btn" id="delete_point_%s">🗑️</button>', seq_len(nrow(points_df)))
      )

      rownames(points_df) <- NULL

      DT::datatable(
        points_df,
        escape = FALSE,
        selection = "none",
        options = list(
          scrollY = "560px",
          scrollCollapse = TRUE,
          paging = FALSE
        ),
        callback = DT::JS(sprintf("
      table.on('click', '.delete-point-btn', function() {
        var id = $(this).attr('id');
        Shiny.setInputValue('%s', {id: id}, {priority: 'event'});
      });
    ", ns("delete_point_click")))
      )
    })

    observeEvent(input$delete_point_click, {
      req(input$delete_point_click$id)
      row_id <- as.numeric(gsub("delete_point_", "", input$delete_point_click$id))
      isolate({
        points$data <- points$data[-row_id]
      })
    })

    # Atualiza os marcadores sempre que pontos mudarem
    observe({
      if(is.null(nrow(coords()))){
        updateTextInput(session, "envname", value = "ENV_1")
      } else{
        updateTextInput(session, "envname", value = paste0("ENV_", nrow(coords()) + 1))
      }
      df <- coords()
      req(nrow(df) > 0)
      leafletProxy("map2") |>
        clearMarkers() |>
        addMarkers(
          lng = df$lon,
          lat = df$lat,
          popup = paste0("Env:", df$env, "<br>Lat: ", df$lat, "<br>Lon: ", df$lon)
        )
    })


    observe({
      nasaparams <- read.csv(file = system.file("app/www/nasaparams.csv", package = "plimanshiny", mustWork = TRUE), sep = ",")
      if(input$scale == "hourly") {
        suitableparams <- nasaparams[nasaparams$level == "hourly", ]$abbreviation
        updatePickerInput(session, "params",
                          choices = suitableparams,
                          selected = c("T2M", "RH2M", "PRECTOTCORR", "PS", "WS2M"))
      } else if (input$scale == "daily") {
        suitableparams <- nasaparams[nasaparams$level == "daily", ]$abbreviation
        updatePickerInput(session, "params",
                          choices = suitableparams,
                          selected = c("T2M", "T2M_MIN", "T2M_MAX", "T2M_RANGE",  "RH2M", "PRECTOTCORR", "PS", "WS2M", "WD2M", "GWETTOP", "GWETROOT"))
      } else  if(input$scale == "monthly"){
        suitableparams <- nasaparams[nasaparams$level == "monthly", ]$abbreviation
        updatePickerInput(session, "params",
                          choices = suitableparams,
                          selected = c("T2M", "T2M_MIN", "T2M_MAX", "T2M_RANGE",  "RH2M", "PRECTOTCORR", "PS", "WS2M", "WD2M", "GWETTOP", "GWETROOT"))
      }

    })

    resclimate <- reactiveVal()

    observeEvent(input$get_weather, {
      df <- coords()
      req(nrow(df) > 0)
      weather <- get_climate(
        env = df$env,
        params = input$params,
        lat = df$lat,
        lon = df$lon,
        start = df$start,
        end = df$end,
        scale = input$scale,
        parallel = input$parallel,
        workers = input$ncores,
        environment = "shiny"
      )
      resclimate(weather)
      assign("weather", weather, envir = .GlobalEnv)
      dfs[["weather"]] <- create_reactval("weather", weather)
      sendSweetAlert(
        session = session,
        title = "Weather data successfully retrieved!",
        text = "The climate information has been loaded and is now available for visualization.",
        type = "success"
      )
    })

    ### SHOW CLIMATE DATA
    output$weather_table <- reactable::renderReactable({
      req(resclimate())
      resclimate() |>
        roundcols(digits = 3) |>
        render_reactable(max_width = NULL)
    })

    observe({
      req(resclimate())  # assegura que ambos existem
      updatePickerInput(session, "variable",
                        choices = colnames(resclimate()),
                        selected = NULL)
    })
    observe({
      updatePickerInput(session, "facet",
                        choices = colnames(resclimate()),
                        selected = NULL)
    })


    output$envirotypes_dist <- renderPlot({
      req(input$variable, input$facet)
      ggplot(resclimate(), aes(x = !!rlang::sym(input$variable))) +
        facet_wrap(as.formula(paste0("~", input$facet)), ncol = 1) +
        geom_density(fill = "steelblue", alpha = 0.6) +
        theme_minimal(base_size = 16) +
        theme(axis.text.y = element_text(angle = 0)) +
        labs(
          x = input$variable,
          y = "Densidade",
          fill = NULL
        )
    })


    output$envirotypes <- renderPlot({
      req(input$quantiles, input$cropdates, input$cropdates_label)
      quantiles <- as.numeric(unlist(strsplit(input$quantiles, ",")))
      quantiles_label <- unlist(strsplit(input$quantiles_label, ","))
      cropdates <- as.numeric(unlist(strsplit(input$cropdates, ",")))
      cropdates_label <- unlist(strsplit(input$cropdates_label, ","))


      dfenviro <-
        envirotype(
          resclimate() |> drop_na(),
          datas = cropdates,
          fases = cropdates_label,
          var = input$variable,
          breaks = quantiles,
          labels = NULL
        ) |>
        tidyr::drop_na()


      output$dataenviro <- reactable::renderReactable({
        dfenviro |>
          dplyr::select(ENV, stage, xcut, Freq, fr) |>
          setNames(c("Environment", "Crop stage", "Envirotype", "Frequency", "Relative frequency")) |>
          roundcols(digits = 3) |>
          render_reactable(max_width = NULL)
      })

      ggplot(dfenviro) +
        geom_bar(aes(x=Freq,  y = ENV, fill = xcut),
                 position = "fill",
                 stat = "identity",
                 width = 1,
                 color = "white",
                 size=.2) +
        facet_wrap(~stage, ncol = 1) +
        theme_minimal(base_size = 16) +
        scale_y_discrete(expand = c(0,0))+
        scale_x_continuous(expand = c(0,0))+
        labs(x = 'Relative frequency',
             y = "Environment",
             fill='Envirotype')+
        theme(axis.title = element_text(size=12),
              legend.text = element_text(size=9),
              strip.text = element_text(size=12),
              legend.title = element_text(size=12),
              strip.background = element_rect(fill="gray95",size=1)) +
        theme(legend.position = "bottom")
    })


  })
}



## To be copied in the UI
# mod_weather_ui("weather_1")

## To be copied in the server
# mod_weather_server("weather_1")
