#' geojson UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_geojson_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    tags$script(HTML("
    shinyjs.copyToClipboard = function(params) {
      const text = params;
      navigator.clipboard.writeText(text).then(function() {
        Shiny.setInputValue('clipboard_status', 'copied', {priority: 'event'});
      });
    }
  ")),

    fluidRow(
      column(3,
             bs4Card(
               title = "Controls",
               collapsible = FALSE,
               width = 12,
               height = "auto",
               prettyRadioButtons(
                 inputId = ns("geometry_source"),
                 label = "Choose Geometry Source:",
                 choices = c(
                   "Draw on Map" = "draw",
                   "Use a shapefile" = "shape"
                 ),
                 selected = "draw", # Opção padrão
                 icon = icon("check"),
                 bigger = TRUE,
                 status = "info",
                 animation = "jelly",
                 inline = TRUE
               ),
               conditionalPanel(
                 condition = "input.geometry_source == 'draw'", ns = ns,
                 h5("Drawn Geometry Tools"),
                 p("Use the drawing tools on the map to create polygons.")
               ),

               hr(),
               conditionalPanel(
                 condition = "input.geometry_source == 'shape'", ns = ns,
                 pickerInput(
                   ns("shapeinput"),
                   label = "Shapefile",
                   choices = NULL
                 ),
                 hr()
               ),
               downloadButton(ns("download_geojson"), "Download GeoJSON"),
               actionButton(ns("copy_wkt"), "Copy WKT"),
               hr() # Separator

             )
      ),
      column(9,
             bs4Card(
               title = "Interactive Map",
               collapsible = FALSE,
               width = 12,
               height = "auto",
               conditionalPanel(
                 condition = "input.geometry_source == 'draw'", ns = ns,
                 editModUI(ns("mapjson"), height = "720px") |> add_spinner()
               ),
               conditionalPanel(
                 condition = "input.geometry_source == 'shape'", ns = ns,
                 leafletOutput(ns("mapjson"), height = "720px") |> add_spinner()
               )
             )
      )
    )
  )
}

#' geojson Server Functions
#'
#' @noRd
mod_geojson_server <- function(id, shapefile){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    observe({
      if(input$geometry_source == 'shape'){
        updatePickerInput(session, "shapeinput",
                          choices = c("none", setdiff(names(shapefile), c("shapefile", "shapefileplot"))))
      }
    })

    # 3.2. Armazena o shapefile selecionado (Corrigido)
    selected_sf <- reactive({

      # Requisitos de entrada (Requisições da UI)
      req(input$geometry_source == 'shape',
          input$shapeinput,
          input$shapeinput != "none")

      # Inicialização
      base_sf <- shapefile[[input$shapeinput]]$data

      # --- 1. Separação e Contagem ---
      all_geoms_sfc <- sf::st_geometry(base_sf)
      existing_polygons_sf <- base_sf[sf::st_is(all_geoms_sfc, c("POLYGON", "MULTIPOLYGON")), ]
      lines_sfc <- sf::st_geometry(base_sf[sf::st_is(all_geoms_sfc, c("LINESTRING", "MULTILINESTRING")), ])
      points_count <- sum(sf::st_is(all_geoms_sfc, c("POINT", "MULTIPOINT")))
      new_polygons_sf <- NULL

      # --- 2. Tentativa de Poligonização (Linhas) ---
      if (length(lines_sfc) > 0) {
        polygonized_geoms <- try(sf::st_polygonize(lines_sfc), silent = TRUE)
        if (!inherits(polygonized_geoms, "try-error")) {
          new_polys_sfc <- sf::st_collection_extract(polygonized_geoms, "POLYGON")
          if (length(new_polys_sfc) > 0) {
            new_polygons_sf <- sf::st_sf(
              geometry = new_polys_sfc,
              crs = sf::st_crs(base_sf)
            )
            show_alert(
              title = "Geometry Conversion Successful",
              text = paste(
                "Success:", nrow(new_polygons_sf),
                "closed linestrings were converted to polygons. Original attributes for these lines were discarded."
              ),
              type = "success"
            )
          }
        } else {
          show_alert(
            title = "Polygonization Error",
            text = "An error occurred while attempting to convert linestring features to polygons. No lines were converted.",
            type = "error"
          )
        }
      }

      # --- 3. Combinação FINAL (Criação de 'final_sf') ---
      final_sf <- dplyr::bind_rows(existing_polygons_sf, new_polygons_sf)

      # --- 4. Alertas de Descarte e Verificação de Vazio ---

      # 4.1. Alerta de Pontos Descartados
      if (points_count > 0) {
        show_alert(
          title = "Features Discarded",
          text = paste(
            "Warning:", points_count,
            "POINT features (including Multipoints) were discarded. Only polygons are maintained."
          ),
          type = "warning"
        )
      }

      # 4.2. Alerta de Geometria Vazia
      if (nrow(final_sf) == 0) {
        show_alert(
          title = "No Valid Geometry Found",
          text = "No POLGON or MULTIPOLYGON features were found or generated after filtering/conversion. The active geometry is empty.",
          type = "error"
        )
        # Retorna um sf vazio ou NULL, dependendo de como você usa o resultado.
        # Retornar NULL é mais seguro para parar o processamento adiante:
        return(NULL)
      }

      # --- 5. Retorno ---
      return(final_sf)
    })

    observe({
      if(input$geometry_source == "shape"){
        output$mapjson <- renderLeaflet({
          req(selected_sf())
          shapefile_view(selected_sf())@map
        })
      }
    })



    # O mapa base pode ser estático, não precisa ser reativo.
    base_map <- leaflet() |>
      leaflet::addProviderTiles("OpenStreetMap", group = "OpenStreetMap") |>
      leaflet::addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") |>
      leaflet::addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") |>
      leaflet::setView(lng = 0, lat = 0, zoom = 2) |>
      leaflet::addLayersControl(
        baseGroups = c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron"),
        options = leaflet::layersControlOptions(collapsed = TRUE)
      )
    cpoints <- callModule(editMod, "mapjson", base_map, editor = "leafpm")


    # --- 2. LÓGICA PARA "Draw on Map" (Painel Condicional 1) ---
    # Esta seção lida com os botões download_geojson e copy_wkt.

    # --- 3. REATIVA CENTRAL (A CHAVE DA LÓGICA) ---
    # Esta reativa decide qual geometria está ativa
    active_geometry <- reactive({
      req(input$geometry_source) # Requer que uma fonte seja escolhida

      if (input$geometry_source == "draw") {
        # Fonte: Desenho no Mapa
        req(cpoints()$finished)
        if (is.null(cpoints()$finished) || nrow(cpoints()$finished) == 0) {
          return(NULL)
        }
        return(cpoints()$finished)

      } else { # input$geometry_source == "shape"
        # Fonte: Shapefile Selecionado
        # selected_sf() já tem a lógica de 'req'
        return(selected_sf())
      }
    })



    output$download_geojson <- downloadHandler(
      filename = function() {
        paste("active_geometry-", Sys.Date(), ".geojson", sep = "")
      },
      content = function(file) {
        # 'req' na geometria ativa
        req(active_geometry())
        geom_to_save <- active_geometry()

        if (is.null(geom_to_save) || nrow(geom_to_save) == 0) {
          showNotification("No active geometry to download.", type = "warning")
          return(NULL)
        }
        sf::st_write(geom_to_save, file)
      }
    )

    # 4.2. Copy WKT (Well-Known Text)
    observeEvent(input$copy_wkt, {
      tryCatch({
        # 'req' na geometria ativa
        req(active_geometry())
        geom_to_copy <- active_geometry()

        wkt_text <- sf_geom_to_geojson_string(geom_to_copy, row_index = 1) |> extract_geometry_string()
        wkt_text <- gsub("\\s+", "", wkt_text)

        # Código JS para copiar
        js_code <- paste0("
          if (navigator.clipboard) {
            navigator.clipboard.writeText('", wkt_text, "');
          } else {
            var tempInput = document.createElement('textarea');
            tempInput.value = '", wkt_text, "';
            document.body.appendChild(tempInput);
            tempInput.select();
            document.execCommand('copy');
            document.body.removeChild(tempInput);
          }
        ")

        # Executa o JS
        shinyjs::runjs(js_code)

        # Prepara o conteúdo do Alerta (HTML)
        content_wkt <- tags$span(
          style = "text-align: left; line-height: 1.2;",
          tags$p("The WKT for the ACTIVE geometry has been copied to the clipboard."),
          tags$textarea(
            wkt_text,
            rows = 8,
            readonly = "readonly",
            style = "width: 100%; font-family: monospace; resize: vertical; padding: 5px; border-radius: 5px; border: 1px solid #ccc;"
          )
        )

        # Chama o alerta
        show_alert(
          title = "Copied to Clipboard!",
          text = content_wkt,
          html = TRUE,
          type = "success",
          width = "auto"
        )
      }, error = function(e) {
        showNotification("Error converting active geometry to WKT. No features available?", type = "error")
        print(e)
      })
    }) # Fim do observeEvent(input$copy_wkt)



  })
}
## To be copied in the UI
# mod_geojson_ui("geojson_1")

## To be copied in the server
# mod_geojson_server("geojson_1")
