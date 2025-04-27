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
                dateRangeInput(ns("dates"), "Select the period", start = Sys.Date() - 35, end = Sys.Date() - 5),
                # botão para condição de usar ou nao nome de municipios
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
                hl(),
                prettyCheckbox(
                  inputId = ns("computegdd"),
                  label = "Compute thermal parameters",
                  value = FALSE,
                  shape = "curve",
                  status = "success",
                  icon = icon("check"),
                  animation = "rotate"
                ),
                conditionalPanel(
                  condition = "input.computegdd", ns = ns,
                  fluidRow(
                    col_3(
                      numericInput(ns("basemin"),
                                   label = "Tbase lower (ºC)",
                                   value = 10,
                                   step = 0.1)
                    ),
                    col_3(
                      numericInput(ns("baseupp"),
                                   label = "Tbase upper (ºC)",
                                   value = 40,
                                   step = 0.1)
                    ),
                    col_3(
                      numericInput(ns("optimallower"),
                                   label = "Topt lower (ºC)",
                                   value = 26,
                                   step = 0.1)
                    ),
                    col_3(
                      numericInput(ns("optimalupper"),
                                   label = "Topt upper (ºC)",
                                   value = 32,
                                   step = 0.1)
                    )
                  )
                ),
                hl(),
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
        plotlyOutput(ns("envirotypes_dist"), height = "560px")
      ),
      tabPanel(
        title = "Envirotypes",
        fluidRow(
          col_6(
            "Envirotypes",
            fluidRow(
              col_4(
                textInput(ns("quantiles"),
                          label = "Quantiles")
              ),
              col_4(
                textInput(ns("cropdates"),
                          label = "Crop stages")
              ),
              col_4(
                textInput(ns("cropdates_label"),
                          label = "Crop stages (labels)")
              )
            ),
            plotlyOutput(ns("envirotypes"), height = "640px")
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

    # Definir todos os reactiveValues logo no início
    points <- reactiveValues(data = list())
    resclimate <- reactiveVal(NULL)  # Inicialização da variável reativa
    
    # Adicionar variáveis para controle de estado e cache
    rv <- reactiveValues(
      api_in_progress = FALSE,
      last_api_call = NULL,
      cache = list(),
      processing_error = NULL
    )
    
    # Função para verificar duplicatas
    is_duplicate_point <- function(new_lat, new_lon) {
      if (length(points$data) == 0) return(FALSE)
      
      existing_points <- do.call(rbind, points$data)
      if (is.null(existing_points)) return(FALSE)
      
      existing_points <- as.data.frame(existing_points)
      any(round(as.numeric(existing_points$lat), 4) == round(new_lat, 4) & 
          round(as.numeric(existing_points$lon), 4) == round(new_lon, 4))
    }

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

    # Renderiza o mapa inicial
    output$map2 <- renderLeaflet({
      leaflet() |>
        addTiles(group = "OpenStreetMap") |>
        addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") |>
        setView(lng = 0, lat = 0, zoom = 2) |>
        addLayersControl(
          baseGroups = c(
            "OpenStreetMap",
            "Esri World Imagery"
          ),
          options = layersControlOptions(collapsed = TRUE)
        ) |>
        hideGroup("Esri World Imagery")
    })

    observeEvent(input$map2_click, {
      click <- input$map2_click
      new_lat <- round(click$lat, 4)
      new_lon <- round(click$lng, 4)

      # Verifica se já existe um ponto com essas coordenadas
      if (!is_duplicate_point(new_lat, new_lon)) {
        new_point <- data.frame(
          env = input$envname,
          lat = new_lat, 
          lon = new_lon,
          start = input$dates[[1]],
          end = input$dates[[2]],
          stringsAsFactors = FALSE
        )
        points$data[[length(points$data) + 1]] <- new_point
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

      # Processar cada município selecionado
      for (i in 1:nrow(selected_mun)) {
        mun_info <- selected_mun[i, ]
        new_lat <- round(mun_info$lat, 4)
        new_lon <- round(mun_info$lon, 4)
        
        # Verificar duplicatas usando a função auxiliar
        if (!is_duplicate_point(new_lat, new_lon)) {
          new_point <- data.frame(
            env = mun_info$name_muni,
            lat = new_lat,
            lon = new_lon,
            start = input$dates[[1]],
            end = input$dates[[2]],
            stringsAsFactors = FALSE
          )
          points$data[[length(points$data) + 1]] <- new_point
        }
      }
    })

    observe({
      req(input$state)
      listmun <-
        read.csv(file = system.file("app/www/coords_muni.csv", package = "plimanshiny", mustWork = TRUE), sep = ",") |>
        dplyr::filter(abbrev_state %in% input$state)

      # Atualiza a lista de municípios com base no estado selecionado
      updatePickerInput(
        session,
        "mun",
        choices = listmun$name_muni,
        selected = NULL
      )
    })

    # Obtém o clima apenas para o último ponto clicado
    output$latlondata <- DT::renderDT({
      req(length(points$data) > 0)
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

    observe({
      if(is.null(nrow(coords()))){
        updateTextInput(session, "envname", value = "ENV_1")
      } else{
        updateTextInput(session, "envname", value = paste0("ENV_", nrow(coords()) + 1))
      }
      df <- coords()
      req(df, nrow(df) > 0)
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
      } else if(input$scale == "monthly"){
        suitableparams <- nasaparams[nasaparams$level == "monthly", ]$abbreviation
        updatePickerInput(session, "params",
                          choices = suitableparams,
                          selected = c("T2M", "T2M_MIN", "T2M_MAX", "T2M_RANGE",  "RH2M", "PRECTOTCORR", "PS", "WS2M", "WD2M", "GWETTOP", "GWETROOT"))
      }
    })

    # Função para gerar uma chave de cache única para cada consulta climática
    generate_cache_key <- function(df, params, scale) {
      # Combina todos os parâmetros da consulta em uma string única
      coords_str <- paste(df$lat, df$lon, df$start, df$end, collapse = "_")
      params_str <- paste(params, collapse = "_")
      paste(coords_str, params_str, scale, sep = "_")
    }
    
    # Função para verificar e utilizar o cache
    get_cached_weather <- function(cache_key) {
      if (!is.null(rv$cache[[cache_key]])) {
        return(rv$cache[[cache_key]])
      }
      return(NULL)
    }
    
    # Função para salvar no cache
    save_to_cache <- function(cache_key, data) {
      # Limitar o tamanho do cache (manter apenas os últimos 5 resultados)
      if (length(rv$cache) > 5) {
        oldest_key <- names(rv$cache)[1]
        rv$cache[[oldest_key]] <- NULL
      }
      rv$cache[[cache_key]] <- data
    }

    # Adicionar elementos de UI ao início do módulo para feedback visual
    output$loading_indicator <- renderUI({
      div(
        id = ns("loading-weather-table"),
        style = "display: none;",
        div(
          class = "text-center",
          tags$div(class = "spinner-border text-primary", role = "status"),
          tags$p("Processando dados...")
        )
      )
    })
    
    # Agrupamento inteligente de dados para visualização eficiente
    smart_group_data <- function(data, max_points = 5000) {
      if (nrow(data) <= max_points) {
        return(data)
      }
      
      # Se tivermos muitos pontos, agrupar por data e ambiente
      grouped_data <- data %>%
        dplyr::group_by(ENV, YYYYMMDD) %>%
        dplyr::summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
        dplyr::ungroup()
      
      return(grouped_data)
    }

    observeEvent(input$get_weather, {
      df <- coords()
      req(df, nrow(df) > 0)
      
      # Evitar múltiplas chamadas simultâneas
      if (rv$api_in_progress) {
        sendSweetAlert(
          session = session,
          title = "Processamento em andamento",
          text = "Já existe uma solicitação de dados em andamento. Por favor, aguarde.",
          type = "warning"
        )
        return()
      }
      
      # Gerar chave de cache
      cache_key <- generate_cache_key(df, input$params, input$scale)
      
      # Verificar se os dados já estão no cache
      cached_data <- get_cached_weather(cache_key)
      if (!is.null(cached_data)) {
        resclimate(cached_data)
        dfs[["weather"]] <- create_reactval("weather", cached_data)
        sendSweetAlert(
          session = session,
          title = "Dados obtidos do cache!",
          text = "As informações climáticas foram recuperadas do cache.",
          type = "success"
        )
        return()
      }
      
      # Definir estado de processamento
      rv$api_in_progress <- TRUE
      rv$processing_error <- NULL
      
      # Adicionar feedback visual
      shinybusy::show_modal_spinner(
        spin = "orbit", 
        text = paste0("Obtendo dados climáticos para ", nrow(df), " locais. Por favor, aguarde...")
      )
      
      # Determinar número de lotes baseado no número de pontos (otimização para grandes conjuntos)
      batch_size <- ifelse(nrow(df) > 10, 5, nrow(df))
      total_batches <- ceiling(nrow(df) / batch_size)
      
      # Estrutura para armazenar resultados
      all_weather_data <- NULL
      
      # Processamento em lotes para grandes conjuntos de pontos
      withCallingHandlers({
        # Se for um conjunto grande, processar em lotes
        if (nrow(df) > batch_size) {
          for (i in 1:total_batches) {
            # Atualizar mensagem do spinner
            shinybusy::update_modal_spinner(
              text = sprintf("Processando lote %d de %d...", i, total_batches)
            )
            
            # Determinar índices do lote atual
            start_idx <- (i-1) * batch_size + 1
            end_idx <- min(i * batch_size, nrow(df))
            batch_df <- df[start_idx:end_idx, ]
            
            # Buscar dados para o lote
            batch_weather <- get_climate(
              env = batch_df$env,
              params = input$params,
              lat = batch_df$lat,
              lon = batch_df$lon,
              start = batch_df$start,
              end = batch_df$end,
              scale = input$scale,
              parallel = input$parallel,
              workers = input$ncores,
              environment = "shiny"
            )
            
            # Acumular resultados
            if (is.null(all_weather_data)) {
              all_weather_data <- batch_weather
            } else {
              all_weather_data <- rbind(all_weather_data, batch_weather)
            }
          }
        } else {
          # Para conjuntos pequenos, processar normalmente
          all_weather_data <- get_climate(
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
        }
        
        # Processamento de GDD se necessário
        if (input$computegdd) {
          shinybusy::update_modal_spinner(
            text = "Calculando parâmetros térmicos..."
          )
          
          # Verificar se os parâmetros necessários estão disponíveis
          if (!all(c("T2M_MIN", "T2M_MAX") %in% colnames(all_weather_data))) {
            stop("Para calcular GDD, certifique-se de que T2M_MIN e T2M_MAX estão listados nos parâmetros selecionados.")
          }
          
          all_weather_data <- gdd_ometto_frue(
            all_weather_data,
            Tbase = input$basemin,
            Tceil = input$baseupp,
            Topt1 = input$optimallower,
            Topt2 = input$optimalupper
          )
        }
        
        # Salvar no cache e atualizar variáveis reativas
        save_to_cache(cache_key, all_weather_data)
        resclimate(all_weather_data)
        dfs[["weather"]] <- create_reactval("weather", all_weather_data)
        
        # Desativar estado de processamento
        rv$api_in_progress <- FALSE
        
        # Remover spinner e notificar sucesso
        shinybusy::remove_modal_spinner()
        sendSweetAlert(
          session = session,
          title = "Dados obtidos com sucesso!",
          text = sprintf("Foram processados dados para %d locais com %d parâmetros climáticos.", 
                         length(unique(all_weather_data$ENV)), 
                         length(input$params)),
          type = "success"
        )
      },
      error = function(e) {
        # Desativar estado de processamento
        rv$api_in_progress <- FALSE
        rv$processing_error <- e$message
        
        # Remover spinner e notificar erro
        shinybusy::remove_modal_spinner()
        sendSweetAlert(
          session = session,
          title = "Erro ao obter dados",
          text = paste("Ocorreu um erro ao buscar dados climáticos:", e$message),
          type = "error"
        )
      })
    })

    # Melhorar a atualização da tabela com dados climáticos
    output$weather_table <- reactable::renderReactable({
      req(resclimate())
      
      # Indicar que a renderização está em andamento
      shinyjs::show("loading-weather-table")
      
      tryCatch({
        # Otimizar dados para a exibição
        formatted_data <- resclimate() |>
          roundcols(digits = 3)
        
        # Renderizar a tabela com configurações otimizadas
        table <- formatted_data |>
          render_reactable(
            filterable = TRUE,
            searchable = TRUE,
            sortable = TRUE,
            resizable = TRUE,
            max_width = NULL,
            defaultPageSize = 15,
            paginationType = "jump",
            highlight = TRUE,
            striped = TRUE,
            compact = TRUE,
            wrap = FALSE,
            showPageSizeOptions = TRUE,
            pageSizeOptions = c(10, 15, 25, 50, 100)
          )
        
        # Remover a indicação de carregamento
        shinyjs::hide("loading-weather-table")
        
        return(table)
      }, error = function(e) {
        # Remover a indicação de carregamento em caso de erro
        shinyjs::hide("loading-weather-table")
        
        # Exibir mensagem de erro na tabela
        render_reactable(
          data.frame(Erro = paste("Erro ao renderizar dados:", e$message)),
          max_width = NULL
        )
      })
    })

    observe({
      req(resclimate(), nrow(resclimate()) > 0)
      
      updatePickerInput(session, "variable",
                        choices = colnames(resclimate()),
                        selected = NULL)
    })
    
    observe({
      # Somente atualize o facet quando temos dados de clima disponíveis
      if (!is.null(resclimate()) && nrow(resclimate()) > 0) {
        updatePickerInput(session, "facet",
                          choices = c("none", colnames(resclimate())),
                          selected = "none")
      } else {
        updatePickerInput(session, "facet",
                          choices = c("none"),
                          selected = "none")
      }
    })

    output$envirotypes_dist <- renderPlotly({
      req(input$variable, input$facet, resclimate(), nrow(resclimate()) > 0)
      
      # Aplicar agrupamento inteligente para gráficos com muitos pontos
      plot_data <- smart_group_data(resclimate())
      
      # Verificar se a variável selecionada existe no conjunto de dados
      if (!input$variable %in% colnames(plot_data)) {
        return(
          plotly::plot_ly() %>%
            plotly::add_annotations(
              text = paste("Variável", input$variable, "não encontrada no conjunto de dados"),
              showarrow = FALSE,
              font = list(size = 16)
            )
        )
      }
      
      # Tratar o caso de todos os valores NA para a variável selecionada
      if (all(is.na(plot_data[[input$variable]]))) {
        return(
          plotly::plot_ly() %>%
            plotly::add_annotations(
              text = paste("Todos os valores para", input$variable, "são NA"),
              showarrow = FALSE,
              font = list(size = 16)
            )
        )
      }
      
      # Tratar o caso de variáveis com valor constante
      if (length(unique(na.omit(plot_data[[input$variable]]))) == 1) {
        const_value <- unique(na.omit(plot_data[[input$variable]]))
        return(
          plotly::plot_ly() %>%
            plotly::add_annotations(
              text = paste("Variável", input$variable, "tem valor constante:", const_value),
              showarrow = FALSE,
              font = list(size = 16)
            )
        )
      }
      
      p <- ggplot(plot_data, aes(x = !!rlang::sym(input$variable))) +
        geom_density(fill = "steelblue", alpha = 0.6) +
        theme_minimal(base_size = 16) +
        theme(axis.text.y = element_text(angle = 0)) +
        labs(
          x = input$variable,
          y = "Densidade",
          fill = NULL
        )
      
      if (input$facet != "none") {
        # Verificar se a variável de facet existe e tem dados válidos
        if (input$facet %in% colnames(plot_data) && 
            length(unique(na.omit(plot_data[[input$facet]]))) > 1) {
          p <- p + facet_wrap(as.formula(paste("~", input$facet)), ncol = 1)
        } else {
          # Adicionar anotação ao gráfico se o facet não for válido
          p <- p + 
            annotate("text", x = mean(range(plot_data[[input$variable]], na.rm = TRUE)), 
                   y = 0, label = "Facet inválido ou com valores constantes", 
                   color = "red", size = 4, vjust = -1)
        }
      }
      
      # Converter para plotly com configurações otimizadas
      plotly::ggplotly(p) %>%
        plotly::config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = list('sendDataToCloud', 'zoom2d', 'pan2d', 
                                       'select2d', 'lasso2d', 'autoScale2d'),
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "png",
            filename = paste0("distribuicao_", input$variable),
            width = 800,
            height = 600
          )
        )
    })

    output$envirotypes <- renderPlotly({
      req(input$quantiles, input$cropdates, input$cropdates_label, 
          resclimate(), nrow(resclimate()) > 0, input$variable)
      
      # Adicionar indicador de carregamento para processamento 
      withProgress(message = 'Gerando envirotypes...', {
        # Validar os dados de entrada para evitar erros
        tryCatch({
          # Converter e validar as entradas do usuário
          quantiles <- as.numeric(unlist(strsplit(input$quantiles, ",")))
          cropdates <- as.numeric(unlist(strsplit(input$cropdates, ",")))
          cropdates_label <- unlist(strsplit(input$cropdates_label, ","))
          
          # Verificações de validação
          req(length(cropdates) == length(cropdates_label))
          req(length(quantiles) > 0)
          
          # Otimizar conjunto de dados para análise - evitar reamostragem desnecessária
          plot_data <- resclimate()
          
          # Se houver muitos dados, reduzir o tamanho do conjunto para processamento mais rápido
          if (nrow(plot_data) > 10000) {
            incProgress(0.2, detail = "Otimizando conjunto de dados...")
            # Apenas manter pontos relevantes para o período de interesse
            plot_data <- plot_data %>%
              dplyr::filter(!is.na(!!rlang::sym(input$variable)))
            
            # Se ainda tivermos muitos pontos após filtrar NAs, amostragem estratificada
            if (nrow(plot_data) > 10000) {
              # Amostragem estratificada por ambiente
              env_list <- unique(plot_data$ENV)
              sampled_data <- lapply(env_list, function(env) {
                env_data <- plot_data[plot_data$ENV == env, ]
                if (nrow(env_data) > 5000) {
                  env_data[sample(nrow(env_data), min(5000, nrow(env_data))), ]
                } else {
                  env_data
                }
              })
              plot_data <- do.call(rbind, sampled_data)
            }
          }
          
          incProgress(0.3, detail = "Calculando envirotypes...")
          
          # Usar processamento com tratamento de erro
          dfenviro <- withCallingHandlers(
            envirotype(
              plot_data %>% tidyr::drop_na(),
              datas = cropdates,
              fases = cropdates_label,
              var = input$variable,
              breaks = quantiles,
              labels = NULL
            ) %>%
              tidyr::drop_na(),
            error = function(e) {
              # Capturar erros específicos e fornecer mensagens mais informativas
              if (grepl("breaks", e$message)) {
                stop("Erro nos quantis especificados. Certifique-se de que são valores numéricos crescentes.", call. = FALSE)
              } else if (grepl("date", e$message)) {
                stop("Erro nas datas especificadas. Verifique o formato e a sequência.", call. = FALSE)
              } else {
                stop(paste("Erro ao calcular envirotypes:", e$message), call. = FALSE)
              }
            }
          )
          
          # Verificar se temos resultados válidos
          req(dfenviro, nrow(dfenviro) > 0)
          
          incProgress(0.7, detail = "Atualizando visualização...")
          
          # Atualizar a tabela com dados processados
          output$dataenviro <- reactable::renderReactable({
            dfenviro %>%
              dplyr::select(ENV, stage, xcut, Freq, fr) %>%
              setNames(c("Environment", "Crop stage", "Envirotype", "Frequency", "Relative frequency")) %>%
              roundcols(digits = 3) %>%
              render_reactable(
                filterable = TRUE,
                searchable = TRUE,
                sortable = TRUE,
                max_width = NULL,
                highlight = TRUE,
                compact = TRUE
              )
          })
          
          # Criar gráfico com cores consistentes
          p <- ggplot(dfenviro) +
            geom_bar(aes(x=Freq, y = ENV, fill = xcut),
                    position = "fill",
                    stat = "identity",
                    width = 1,
                    color = "white",
                    size=.2) +
            facet_wrap(~stage, ncol = 1) +
            theme_minimal(base_size = 16) +
            scale_y_discrete(expand = c(0,0))+
            scale_x_continuous(expand = c(0,0))+
            # Cores adaptativas para diferentes quantis
            scale_fill_viridis_d(option = "viridis") +
            labs(x = 'Frequência relativa',
                y = "Ambiente",
                fill='Envirotipo')+
            theme(axis.title = element_text(size=12),
                  legend.text = element_text(size=9),
                  strip.text = element_text(size=12),
                  legend.title = element_text(size=12),
                  strip.background = element_rect(fill="gray95",size=1)) +
            theme(legend.position = "bottom")
            
          incProgress(1.0, detail = "Concluído!")
          
          # Converter para plotly com configurações otimizadas para interatividade
          plotly::ggplotly(p) %>%
            plotly::config(
              displayModeBar = TRUE,
              displaylogo = FALSE,
              modeBarButtonsToRemove = list('sendDataToCloud'),
              toImageButtonOptions = list(
                format = "png",
                filename = paste0("envirotypes_", input$variable),
                width = 800,
                height = 600
              )
            )
        }, error = function(e) {
          # Retornar um gráfico vazio com mensagem de erro detalhada
          plotly::plot_ly() %>%
            plotly::add_annotations(
              text = paste("Erro ao gerar o gráfico:", e$message),
              showarrow = FALSE,
              font = list(size = 16, color = "red")
            )
        })
      })
    })
  })
}



## To be copied in the UI
# mod_weather_ui("weather_1")

## To be copied in the server
# mod_weather_server("weather_1")
