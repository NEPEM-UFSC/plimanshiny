#' mgrs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mgrs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "MGRS Selector",
          collapsible = FALSE,
          width = 12,
          height = "auto",
          numericInput(
            ns("latitude"),
            label = "Latitude:",
            value = NA
          ),
          numericInput(
            ns("longitude"),
            label = "Longitude:",
            value = NA
          ),
          actionButton(ns("update_map"), "Update Map"),
          textInput(
            ns("mgrs_code"),
            label = "MGRS Code",
            value = ""
          ),
          textOutput(ns("mgrs_zone")),
          textOutput(ns("mgrs_square")),
          textOutput(ns("mgrs_easting")),
          textOutput(ns("mgrs_northing"))
        )
      ),
      col_9(
        bs4Card(
          title = "Interactive Map",
          collapsible = FALSE,
          width = 12,
          height = "auto",
          leafletOutput(ns("map"), height = "700px")
        )
      )
    )
  )
}

#' mgrs Server Functions
#'
#' @noRd
mod_mgrs_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$map <- renderLeaflet({
      mapview::mapview(
        map.types = c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")
      )@map |>
        setView(lng = 0, lat = 0, zoom = 2)
    })

    # --- Conversão Lat/Lon -> MGRS + cálculo do quadrado 100 km ---
    calculate_mgrs_details <- function(lat, lon) {
      # Determina zona UTM e EPSG
      utm_zone <- floor((lon + 180) / 6) + 1
      epsg <- ifelse(lat >= 0, 32600 + utm_zone, 32700 + utm_zone)

      # Converte para UTM
      pt <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
      pt_utm <- sf::st_transform(pt, epsg)
      coords <- sf::st_coordinates(pt_utm)

      easting <- coords[1]
      northing <- coords[2]

      # Faixa de latitude (letra)
      lat_bands <- strsplit("CDEFGHJKLMNPQRSTUVWX", "")[[1]]
      band_index <- floor((lat + 80) / 8) + 1
      lat_band <- ifelse(band_index >= 1 & band_index <= length(lat_bands),
                         lat_bands[band_index], "Z")

      # Quadrado de 100 km
      e100kLetters <- c(
        "ABCDEFGH", "JKLMNPQR", "STUVWXYZ",
        "ABCDEFGH", "JKLMNPQR", "STUVWXYZ"
      )
      n100kLetters <- c(
        "ABCDEFGHJKLMNPQRSTUV", "FGHJKLMNPQRSTUVABCDE"
      )

      set_index <- (utm_zone - 1) %% 6 + 1
      e100kSet <- e100kLetters[set_index]
      n100kSet <- n100kLetters[ifelse(lat >= 0, 1, 2)]

      e100k <- floor(easting / 100000)
      n100k <- floor((northing %% 2000000) / 100000)

      e_letter <- substr(e100kSet, e100k + 1, e100k + 1)
      n_letter <- substr(n100kSet, n100k + 1, n100k + 1)

      mgrs_square <- paste0(e_letter, n_letter)

      # Coordenadas reduzidas
      e_str <- sprintf("%05d", round(easting %% 100000))
      n_str <- sprintf("%05d", round(northing %% 100000))

      mgrs_code <- paste0(utm_zone, lat_band, mgrs_square)

      # Coordenadas do quadrado (100 km)
      e_min <- easting - (easting %% 100000)
      n_min <- northing - (northing %% 100000)
      e_max <- e_min + 100000
      n_max <- n_min + 100000

      square_utm <- sf::st_polygon(list(rbind(
        c(e_min, n_min),
        c(e_max, n_min),
        c(e_max, n_max),
        c(e_min, n_max),
        c(e_min, n_min)
      ))) |>
        sf::st_sfc(crs = epsg) |>
        sf::st_transform(4326)

      list(
        mgrs_code = mgrs_code,
        zone = paste0(utm_zone, lat_band),
        square = mgrs_square,
        easting = easting,
        northing = northing,
        polygon = square_utm
      )
    }

    # --- Clique no mapa ---
    observeEvent(input$map_click, {
      click <- input$map_click
      lon <- click$lng
      lat <- click$lat

      if (lon < -180 || lon > 180) {
        showNotification("Invalid longitude! Please select a value between -180 and 180.", type = "error")
        return()
      }

      mgrs_details <- calculate_mgrs_details(lat, lon)

      updateNumericInput(session, "latitude", value = lat)
      updateNumericInput(session, "longitude", value = lon)
      updateTextInput(session, "mgrs_code", value = mgrs_details$mgrs_code)

      output$mgrs_zone <- renderText(paste("Zone:", mgrs_details$zone))
      output$mgrs_square <- renderText(paste("Square:", mgrs_details$square))
      output$mgrs_easting <- renderText(paste("Easting:", round(mgrs_details$easting, 2), "m"))
      output$mgrs_northing <- renderText(paste("Northing:", round(mgrs_details$northing, 2), "m"))

      leafletProxy("map") |>
        clearShapes() |>
        clearMarkers() |>
        addPolygons(
          data = mgrs_details$polygon,
          fillColor = "blue",
          color = "blue",
          fillOpacity = 0.3,
          weight = 2,
          label = paste("Square:", mgrs_details$square)
        ) |>
        addMarkers(
          lng = lon,
          lat = lat,
          popup = paste("MGRS:", mgrs_details$mgrs_code)
        ) |>
        setView(lng = lon, lat = lat, zoom = 8)
    })

    # --- Atualização manual via botão ---
    observeEvent(input$update_map, {
      lon <- as.numeric(input$longitude)
      lat <- as.numeric(input$latitude)
      if (is.na(lat) || is.na(lon)) return()

      mgrs_details <- calculate_mgrs_details(lat, lon)

      updateTextInput(session, "mgrs_code", value = mgrs_details$mgrs_code)
      output$mgrs_zone <- renderText(paste("Zone:", mgrs_details$zone))
      output$mgrs_square <- renderText(paste("Square:", mgrs_details$square))
      output$mgrs_easting <- renderText(paste("Easting:", round(mgrs_details$easting, 2), "m"))
      output$mgrs_northing <- renderText(paste("Northing:", round(mgrs_details$northing, 2), "m"))

      leafletProxy("map") |>
        clearShapes() |>
        clearMarkers() |>
        addPolygons(
          data = mgrs_details$polygon,
          fillColor = "blue",
          color = "blue",
          fillOpacity = 0.3,
          weight = 2,
          label = paste("Square:", mgrs_details$square)
        ) |>
        addMarkers(
          lng = lon,
          lat = lat,
          popup = paste("MGRS:", mgrs_details$mgrs_code)
        ) |>
        setView(lng = lon, lat = lat, zoom = 8)
    })
  })
}

## To be copied in the UI
# mod_mgrs_ui("mgrs_1")

## To be copied in the server
# mod_mgrs_server("mgrs_1")
