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
                prettySwitch(
                  inputId = ns("computegdd"),
                  label = "Compute thermal parameters",
                  value = FALSE,
                  status = "success",
                  fill = TRUE
                ),
                conditionalPanel(
                  condition = "input.computegdd == true", ns = ns,
                  fluidRow(
                    col_12(
                      div(
                        style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
                        HTML("<b>Degree-days</b>: A measure of heat accumulation needed for plant/insect development. It's calculated using base temperatures below which development stops.")
                      )
                    )
                  ),
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
                  ),
                  hl(),
                  fluidRow(
                    col_12(
                      div(
                        style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
                        HTML("<b>Chilling hours</b>: Hours accumulated when temperatures are in specific ranges needed for breaking dormancy in fruit trees and other perennial plants.")
                      )
                    )
                  ),
                  fluidRow(
                    col_4(
                      prettySwitch(
                        inputId = ns("ch_weinberger"),
                        label = "Chilling hours - Weinberger",
                        value = FALSE,
                        status = "success",
                        fill = TRUE
                      )
                    ),
                    col_4(
                      prettySwitch(
                        inputId = ns("ch_utah"),
                        label = "Chilling hours - Utah",
                        value = FALSE,
                        status = "success",
                        fill = TRUE
                      )
                    ),
                    col_4(
                      prettySwitch(
                        inputId = ns("ch_northcarolina"),
                        label = "Chilling hours - North Carolina",
                        value = FALSE,
                        status = "success",
                        fill = TRUE
                      )
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
                  col_6(
                    shinyWidgets::actionBttn(
                      inputId = ns("get_weather"),
                      label = "Fetch data",
                      style = "gradient",
                      color = "primary",
                      icon = icon("cloud-download-alt"),
                      size = "md",
                      block = TRUE
                    ),
                  ),
                  col_6(
                    shinyWidgets::actionBttn(
                      inputId = ns("clear_points"),
                      label = "Clear points",
                      style = "gradient",
                      color = "danger",
                      icon = icon("trash-alt"),
                      size = "md",
                      block = TRUE
                    ),
                  )
                ),
                br(),
                fluidRow(
                  col_12(
                    prettySwitch(
                      inputId = ns("parallel"),
                      label = "Parallel processing",
                      value = FALSE,
                      status = "success",
                      fill = TRUE
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.parallel == true", ns = ns,
                  fluidRow(
                    col_12(
                      numericInput(
                        inputId = ns("ncores"),
                        label = "Number of cores",
                        value = 1,
                        min = 1,
                        max = 6,
                        step = 1,
                        width = "100%"
                      )
                    )
                  )
                ),
                uiOutput(ns("cache_status_ui"))
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
      # Formatar datas consistentemente
      formatted_dates <- lapply(df, function(row) {
        if (is.data.frame(row)) {
          # Para dataframes completos
          starts <- as.Date(row$start)
          ends <- as.Date(row$end)
          paste(format(starts, "%Y%m%d"), format(ends, "%Y%m%d"), sep = "_")
        } else {
          # Para linhas individuais
          start_date <- as.Date(row["start"])
          end_date <- as.Date(row["end"])
          paste(format(start_date, "%Y%m%d"), format(end_date, "%Y%m%d"), sep = "_")
        }
      })
      
      # Combinar coordenadas e datas formatadas em um identificador único
      location_date_str <- paste(
        paste(df$lat, df$lon, sep = ":"),
        unlist(formatted_dates),
        sep = "_"
      )
      location_date_str <- paste(location_date_str, collapse = "|")
      
      # Adicionar parâmetros climáticos e escala à chave
      params_str <- paste(sort(params), collapse = "_") # Ordenar para consistência
      
      # Criar hash MD5 para chave mais curta e confiável
      digest::digest(paste(location_date_str, params_str, scale, sep = "::"), algo = "md5")
    }
    
    # Função para verificar e utilizar o cache de forma mais robusta
    get_cached_weather <- function(cache_key, current_df) {
      # Verificar primeiro na memória atual
      in_memory_cache <- rv$cache[[cache_key]]
      if (!is.null(in_memory_cache)) {
        # Validar se as datas no cache correspondem às datas solicitadas
        if (validate_cache_dates(in_memory_cache, current_df)) {
          return(list(
            data = in_memory_cache,
            source = "memory",
            valid = TRUE
          ))
        }
      }
      
      # Verificar no cache persistente em disco
      disk_cache <- get_disk_cache(cache_key)
      if (!is.null(disk_cache)) {
        if (validate_cache_dates(disk_cache, current_df)) {
          # Atualizar o cache em memória também
          rv$cache[[cache_key]] <- disk_cache
          return(list(
            data = disk_cache,
            source = "disk",
            valid = TRUE
          ))
        }
      }
      
      # Nenhum cache válido encontrado
      return(NULL)
    }
    
    # Função para validar as datas no cache
    validate_cache_dates <- function(cached_data, requested_df) {
      # Se não temos datas na estrutura, não podemos validar
      if (!("YYYYMMDD" %in% colnames(cached_data))) {
        return(FALSE)
      }
      
      # Verificar cada combinação única de local e datas
      for (i in 1:nrow(requested_df)) {
        env_name <- requested_df$env[i]
        start_date <- as.Date(requested_df$start[i])
        end_date <- as.Date(requested_df$end[i])
        
        # Filtrar dados do cache por ambiente
        env_data <- cached_data[cached_data$ENV == env_name, ]
        if (nrow(env_data) == 0) {
          return(FALSE)  # Ambiente não encontrado no cache
        }
        
        # Converter datas para comparação
        env_dates <- as.Date(env_data$YYYYMMDD, format = "%Y%m%d")
        
        # Verificar se todas as datas solicitadas estão no cache
        if (!all(seq.Date(start_date, end_date, by = "day") %in% env_dates)) {
          return(FALSE)  # Faltam datas no cache
        }
      }
      
      # Todos os ambientes e datas foram encontrados no cache
      return(TRUE)
    }
    
    # Função para gerenciar o cache persistente em disco
    get_disk_cache <- function(cache_key) {
      # Diretório para armazenar os caches
      cache_dir <- file.path(rappdirs::user_cache_dir("plimanshiny"), "weather_cache")
      if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE)
      }
      
      # Caminho do arquivo de cache específico
      cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))
      
      # Verificar se o arquivo existe
      if (file.exists(cache_file)) {
        # Verificar a idade do cache (invalidar após 30 dias)
        file_info <- file.info(cache_file)
        cache_age <- difftime(Sys.time(), file_info$mtime, units = "days")
        
        # Se o cache for mais antigo que 30 dias, invalidar
        if (cache_age > 30) {
          file.remove(cache_file)
          return(NULL)
        }
        
        # Carregar dados do cache
        tryCatch({
          cached_data <- readRDS(cache_file)
          return(cached_data)
        }, error = function(e) {
          # Em caso de erro ao ler o cache, remover o arquivo corrompido
          file.remove(cache_file)
          return(NULL)
        })
      }
      
      return(NULL)
    }
    
    # Função para salvar no cache de forma persistente
    save_to_cache <- function(cache_key, data) {
      # Salvar em memória (limitando o tamanho)
      if (length(rv$cache) > 5) {
        # Remover o cache mais antigo
        oldest_key <- names(rv$cache)[1]
        rv$cache[[oldest_key]] <- NULL
      }
      rv$cache[[cache_key]] <- data
      
      # Salvar em disco para persistência
      cache_dir <- file.path(rappdirs::user_cache_dir("plimanshiny"), "weather_cache")
      if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE)
      }
      
      cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))
      
      # Salvar de forma segura
      tryCatch({
        saveRDS(data, cache_file)
      }, error = function(e) {
        # Em caso de erro ao salvar, garantir que não deixamos arquivos corrompidos
        if (file.exists(cache_file)) {
          file.remove(cache_file)
        }
      })
      
      # Retornar metadados do cache
      list(
        size = format(object.size(data), units = "MB"),
        file = cache_file,
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
    }
    
    # Função para limpar o cache (útil para diagnóstico)
    clean_weather_cache <- function(age_days = NULL) {
      # Limpar cache em memória
      rv$cache <- list()
      
      # Limpar cache em disco
      cache_dir <- file.path(rappdirs::user_cache_dir("plimanshiny"), "weather_cache")
      if (!dir.exists(cache_dir)) {
        return(list(memory_cleared = TRUE, disk_cleared = FALSE, reason = "Cache directory does not exist"))
      }
      
      # Listar todos os arquivos de cache
      cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
      
      # Se especificado um número de dias, remover apenas caches mais antigos
      if (!is.null(age_days)) {
        files_to_remove <- cache_files[file.info(cache_files)$mtime < Sys.time() - as.difftime(age_days, units = "days")]
      } else {
        files_to_remove <- cache_files
      }
      
      # Remover arquivos
      if (length(files_to_remove) > 0) {
        unlink(files_to_remove)
      }
      
      list(
        memory_cleared = TRUE,
        disk_cleared = TRUE,
        files_removed = length(files_to_remove)
      )
    }

    # Remover elementos de UI redundantes e confiar apenas no progressR para feedback
    observeEvent(input$get_weather, {
      df <- coords()
      req(df, nrow(df) > 0)
      
      # Evitar múltiplas chamadas simultâneas
      if (rv$api_in_progress) {
        sendSweetAlert(
          session = session,
          title = "Processing in progress",
          text = "There is already a data request in progress. Please wait.",
          type = "warning"
        )
        return()
      }
      
      # Gerar chave de cache
      cache_key <- generate_cache_key(df, input$params, input$scale)
      
      # Verificar se os dados já estão no cache
      cached_data <- get_cached_weather(cache_key, df)
      if (!is.null(cached_data) && cached_data$valid) {
        resclimate(cached_data$data)
        dfs[["weather"]] <- create_reactval("weather", cached_data$data)
        sendSweetAlert(
          session = session,
          title = "Data retrieved from cache!",
          text = paste("Climate information was retrieved from cache (source:", cached_data$source, ")."),
          type = "success"
        )
        return()
      }
      
      # Definir estado de processamento
      rv$api_in_progress <- TRUE
      rv$processing_error <- NULL
      
      # Determinar número de lotes baseado no número de pontos
      batch_size <- ifelse(nrow(df) > 10, 5, nrow(df))
      total_batches <- ceiling(nrow(df) / batch_size)
      
      # Estrutura para armazenar resultados
      all_weather_data <- NULL
      
      # Usar withProgress para aproveitar o progressR existente
      withProgress(message = 'Retrieving climate data...', value = 0, {
        # Processamento em lotes para grandes conjuntos de pontos
        tryCatch({
          # Se for um conjunto grande, processar em lotes
          if (nrow(df) > batch_size) {
            for (i in 1:total_batches) {
              # Atualizar barra de progresso
              incProgress(1/total_batches, 
                          detail = sprintf("Processing batch %d of %d...", i, total_batches))
              
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
            incProgress(0.5, detail = "Fetching data from API...")
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
          
          # Processamento de GDD e chilling hours se necessário
          if (input$computegdd) {
            incProgress(0.2, detail = "Calculating thermal parameters...")
            
            # Verificar se os parâmetros necessários estão disponíveis para GDD
            if (!all(c("T2M_MIN", "T2M_MAX") %in% colnames(all_weather_data))) {
              stop("To calculate GDD, make sure that T2M_MIN and T2M_MAX are listed in the selected parameters.")
            }
            
            # Calcular degree-days
            all_weather_data <- gdd_ometto_frue(
              all_weather_data,
              Tbase = input$basemin,
              Tceil = input$baseupp,
              Topt1 = input$optimallower,
              Topt2 = input$optimalupper
            )
            
            # Calcular chilling hours pelos diferentes métodos quando selecionados
            if (input$ch_weinberger) {
              incProgress(0.05, detail = "Calculating Weinberger chilling hours...")
              all_weather_data <- calculate_weinberger_ch(all_weather_data)
            }
            
            if (input$ch_utah) {
              incProgress(0.05, detail = "Calculating Utah chilling hours...")
              all_weather_data <- calculate_utah_ch(all_weather_data)
            }
            
            if (input$ch_northcarolina) {
              incProgress(0.05, detail = "Calculating North Carolina chilling hours...")
              all_weather_data <- calculate_nc_ch(all_weather_data)
            }
          }
          
          incProgress(0.8, detail = "Finalizing...")
          
          # Salvar no cache e atualizar variáveis reativas
          save_to_cache(cache_key, all_weather_data)
          resclimate(all_weather_data)
          dfs[["weather"]] <- create_reactval("weather", all_weather_data)
          
          # Desativar estado de processamento
          rv$api_in_progress <- FALSE
          
          # Notificar sucesso
          sendSweetAlert(
            session = session,
            title = "Data successfully retrieved!",
            text = sprintf("Data processed for %d locations with %d climate parameters.", 
                          length(unique(all_weather_data$ENV)), 
                          length(input$params)),
            type = "success"
          )
        },
        error = function(e) {
          # Desativar estado de processamento
          rv$api_in_progress <- FALSE
          rv$processing_error <- e$message
          
          # Notificar erro
          sendSweetAlert(
            session = session,
            title = "Error retrieving data",
            text = paste("An error occurred while fetching climate data:", e$message),
            type = "error"
          )
        })
      })
    })

    # Melhorar a atualização da tabela com dados climáticos
    output$weather_table <- reactable::renderReactable({
      req(resclimate())
      
      # Usar withProgress para aproveitar a barra de progresso existente
      withProgress(message = 'Loading data...', value = 0, {
        incProgress(0.3)
        
        # Otimizar dados para a exibição
        formatted_data <- resclimate() |>
          roundcols(digits = 3)
        
        incProgress(0.7)
        
        # Renderizar a tabela com configurações otimizadas
        formatted_data |>
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
              text = paste("Variable", input$variable, "not found in the dataset"),
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
              text = paste("All values for", input$variable, "are NA"),
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
              text = paste("Variable", input$variable, "has constant value:", const_value),
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
          y = "Density",
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
                   y = 0, label = "Invalid facet or no data", 
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
      withProgress(message = 'Generating envirotypes...', {
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
            incProgress(0.2, detail = "Optimizing dataset...")
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
          
          incProgress(0.3, detail = "Calculating envirotypes...")
          
          # Usar processamento com tratamento de erro
            dfenviro <- withCallingHandlers(
            envirotype(
              plot_data %>% tidyr::drop_na(),
              datas = cropdates,
              fases = cropdates_label,
              var = input$variable,
              breaks = quantiles,
              labels = NULL
            ) %>% tidyr::drop_na(),
            error = function(e) {
              # Capture specific errors and provide more informative messages
              if (grepl("breaks", e$message)) {
              stop("Error in the specified quantiles. Ensure they are increasing numeric values.", call. = FALSE)
              } else if (grepl("date", e$message)) {
              stop("Error in the specified dates. Check the format and sequence.", call. = FALSE)
              } else {
              stop(paste("Error in calculating envirotypes:", e$message), call. = FALSE)
              }
            }
            )
          
          # Verificar se temos resultados válidos
          req(dfenviro, nrow(dfenviro) > 0)
          
          incProgress(0.7, detail = "Updating visualization...")
          
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
            labs(x = 'Relative frequency',
                y = "Environment",
                fill='Envirotype')+
            theme(axis.title = element_text(size=12),
                  legend.text = element_text(size=9),
                  strip.text = element_text(size=12),
                  legend.title = element_text(size=12),
                  strip.background = element_rect(fill="gray95",size=1)) +
            theme(legend.position = "bottom")
            
          incProgress(1.0, detail = "Completed!")
          
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
              text = paste("Error while trying to generate the graphic:", e$message),
              showarrow = FALSE,
              font = list(size = 16, color = "red")
            )
        })
      })
    })

    # Adicionar botão de limpeza de cache na UI
    output$cache_status_ui <- renderUI({
      if (!is.null(rv$cache) && length(rv$cache) > 0) {
        div(
          style = "margin-top: 10px;",
          fluidRow(
            col_12(
              dropdown(
                inputId = ns("cache_dropdown"),
                label = "Cache Manager",
                icon = icon("database"),
                status = "info",
                size = "xs",
                right = TRUE,
                tooltip = tooltipOptions(title = "Weather cache management options"),
                fluidRow(
                  col_12(
                    tags$b("Cache Status:"),
                    tags$p(paste("Items in memory:", length(rv$cache))),
                    actionButton(
                      inputId = ns("clear_cache"),
                      label = "Clear Cache",
                      icon = icon("trash"),
                      class = "btn-danger btn-sm",
                      width = "100%"
                    )
                  )
                )
              )
            )
          )
        )
      } else {
        NULL
      }
    })

    # Evento para limpar o cache quando o botão for pressionado
    observeEvent(input$clear_cache, {
      result <- clean_weather_cache()
      sendSweetAlert(
        session = session,
        title = "Cache Cleared",
        text = paste0("Memory and disk cache has been removed. ",
                     if (result$disk_cleared) paste0(result$files_removed, " files deleted.") else ""),
        type = "success"
      )
    })

    # Atualizar a função de obtenção de dados para usar corretamente o cache
    observeEvent(input$get_weather, {
      df <- coords()
      req(df, nrow(df) > 0)
      
      # Evitar múltiplas chamadas simultâneas
      if (rv$api_in_progress) {
        sendSweetAlert(
          session = session,
          title = "Processing in progress",
          text = "There is already a data request in progress. Please wait.",
          type = "warning"
        )
        return()
      }
      
      # Gerar chave de cache
      cache_key <- generate_cache_key(df, input$params, input$scale)
      
      # Verificar se os dados já estão no cache
      cached_result <- get_cached_weather(cache_key, df)
      if (!is.null(cached_result) && cached_result$valid) {
        cached_data <- cached_result$data
        
        # Verificar se todas as datas solicitadas estão no cache
        date_ranges <- lapply(1:nrow(df), function(i) {
          start_date <- as.Date(df$start[i])
          end_date <- as.Date(df$end[i])
          env_name <- df$env[i]
          
          # Filtrar dados para este ambiente específico
          env_data <- cached_data[cached_data$ENV == env_name, ]
          
          # Extrair datas únicas do cache para este ambiente
          if (nrow(env_data) > 0 && "YYYYMMDD" %in% colnames(env_data)) {
            env_dates <- as.Date(env_data$YYYYMMDD, format = "%Y%m%d")
            
            # Verificar a cobertura de datas
            all_requested_dates <- seq.Date(start_date, end_date, by = "day")
            missing_dates <- all_requested_dates[!all_requested_dates %in% env_dates]
            
            list(
              env = env_name,
              start = start_date,
              end = end_date,
              covered = length(missing_dates) == 0,
              missing_count = length(missing_dates),
              cache_count = length(unique(env_dates))
            )
          } else {
            list(
              env = env_name,
              start = start_date,
              end = end_date,
              covered = FALSE,
              missing_count = difftime(end_date, start_date, units = "days") + 1,
              cache_count = 0
            )
          }
        })
        
        # Verificar se todas as datas estão cobertas para todos os ambientes
        all_covered <- all(sapply(date_ranges, function(x) x$covered))
        
        if (all_covered) {
          # Tudo já está no cache, podemos usar
          resclimate(cached_data)
          dfs[["weather"]] <- create_reactval("weather", cached_data)
          
          # Informações detalhadas sobre o cache
          cache_info <- paste0(
            "Source: ", cached_result$source, "\n",
            "Environments: ", length(unique(cached_data$ENV)), "\n",
            "Parameters: ", length(input$params), "\n",
            "Dates: ", min(as.Date(cached_data$YYYYMMDD, format = "%Y%m%d")), " to ", 
            max(as.Date(cached_data$YYYYMMDD, format = "%Y%m%d"))
          )
          
          sendSweetAlert(
            session = session,
            title = "Data retrieved from cache!",
            text = paste0("Climate information was retrieved from cache.\n\n", cache_info),
            type = "success"
          )
          return()
        } else {
          # Cache parcial ou inválido - precisamos buscar dados novos
          missing_summary <- paste(sapply(date_ranges[!sapply(date_ranges, function(x) x$covered)], 
                                        function(x) {
                                          paste0(x$env, ": ", x$missing_count, " days not found in cache")
                                        }), 
                                 collapse = "\n")
          
          sendSweetAlert(
            session = session,
            title = "Partial or invalid cache",
            text = paste0("Some requested data is not in the cache or is invalid. Fetching new data.\n\n", 
                         missing_summary),
            type = "info"
          )
        }
      }
    })
  })
}

# Função para calcular horas de frio pelo método Weinberger
    calculate_weinberger_ch <- function(data) {
      # O método Weinberger conta as horas abaixo de 7.2°C
      if (!all(c("T2M", "HOUR") %in% colnames(data))) {
        warning("T2M or HOUR columns required for Weinberger chilling hours calculation")
        return(data)
      }
      
      # Calcular horas de frio diárias
      ch_data <- data %>%
        dplyr::mutate(CH_Weinberger = ifelse(T2M < 7.2, 1, 0))
      
      # Se os dados forem horários, agrupar por dia
      if ("HOUR" %in% colnames(ch_data)) {
        ch_data <- ch_data %>%
          dplyr::group_by(ENV, YYYYMMDD) %>%
          dplyr::mutate(CH_Weinberger_daily = sum(CH_Weinberger, na.rm = TRUE)) %>%
          dplyr::ungroup()
      }
      
      # Adicionar acumulado
      ch_data <- ch_data %>%
        dplyr::group_by(ENV) %>%
        dplyr::mutate(CH_Weinberger_accum = cumsum(replace_na(CH_Weinberger, 0))) %>%
        dplyr::ungroup()
      
      return(ch_data)
    }
    
    # Função para calcular horas de frio pelo método Utah
    calculate_utah_ch <- function(data) {
      # O método Utah atribui diferentes pesos para diferentes faixas de temperatura
      if (!all(c("T2M", "HOUR") %in% colnames(data))) {
        warning("T2M or HOUR columns required for Utah chilling hours calculation")
        return(data)
      }
      
      # Pesos conforme o modelo Utah
      ch_data <- data %>%
        dplyr::mutate(
          CH_Utah = case_when(
            T2M < 1.4 ~ 0,
            T2M >= 1.4 & T2M < 2.4 ~ 0.5,
            T2M >= 2.4 & T2M < 9.1 ~ 1.0,
            T2M >= 9.1 & T2M < 12.4 ~ 0.5,
            T2M >= 12.4 & T2M < 15.9 ~ 0,
            T2M >= 15.9 & T2M < 18.0 ~ -0.5,
            T2M >= 18.0 ~ -1.0,
            TRUE ~ 0
          )
        )
      
      # Se os dados forem horários, agrupar por dia
      if ("HOUR" %in% colnames(ch_data)) {
        ch_data <- ch_data %>%
          dplyr::group_by(ENV, YYYYMMDD) %>%
          dplyr::mutate(CH_Utah_daily = sum(CH_Utah, na.rm = TRUE)) %>%
          dplyr::ungroup()
      }
      
      # Adicionar acumulado
      ch_data <- ch_data %>%
        dplyr::group_by(ENV) %>%
        dplyr::mutate(
          # No modelo Utah, o acumulado não pode ser negativo
          CH_Utah_accum = pmax(0, cumsum(replace_na(CH_Utah, 0)))
        ) %>%
        dplyr::ungroup()
      
      return(ch_data)
    }
    
    # Função para calcular horas de frio pelo método North Carolina
    calculate_nc_ch <- function(data) {
      # North Carolina model (adaptação do modelo Utah para climas mais amenos)
      if (!all(c("T2M", "HOUR") %in% colnames(data))) {
        warning("T2M or HOUR columns required for North Carolina chilling hours calculation")
        return(data)
      }
      
      # Pesos conforme o modelo North Carolina
      ch_data <- data %>%
        dplyr::mutate(
          CH_NC = case_when(
            T2M < 1.4 ~ 0,
            T2M >= 1.4 & T2M < 7.2 ~ 1.0,
            T2M >= 7.2 & T2M < 13.0 ~ 0.5,
            T2M >= 13.0 & T2M < 16.5 ~ 0,
            T2M >= 16.5 & T2M < 19.0 ~ -0.5,
            T2M >= 19.0 & T2M < 20.7 ~ -1.0,
            T2M >= 20.7 ~ -2.0,
            TRUE ~ 0
          )
        )
      
      # Se os dados forem horários, agrupar por dia
      if ("HOUR" %in% colnames(ch_data)) {
        ch_data <- ch_data %>%
          dplyr::group_by(ENV, YYYYMMDD) %>%
          dplyr::mutate(CH_NC_daily = sum(CH_NC, na.rm = TRUE)) %>%
          dplyr::ungroup()
      }
      
      # Adicionar acumulado
      ch_data <- ch_data %>%
        dplyr::group_by(ENV) %>%
        dplyr::mutate(
          # Acumulado não pode ser negativo
          CH_NC_accum = pmax(0, cumsum(replace_na(CH_NC, 0)))
        ) %>%
        dplyr::ungroup()
      
      return(ch_data)
    }
## To be copied in the UI
# mod_weather_ui("weather_1")

## To be copied in the server
# mod_weather_server("weather_1")
