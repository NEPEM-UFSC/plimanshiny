#' utmzonesel UI Function
#'
#' @description A shiny Module for UTM zone and EPSG code selection.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_utmzonesel_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "UTM Zone Selector",
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
            value = NA,
          ),
          actionButton(ns("update_map"), "Update Map"),

          textInput(
            ns("epsg"),
            label = "EPSG Code",
            value = ""
          ),
          textOutput(ns("utm_zone")),
          textOutput(ns("utm_band")),
          textOutput(ns("central_meridian")),
          textOutput(ns("easting")),
          textOutput(ns("northing")),
          textOutput(ns("utmcoord"))
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

#' utmzonesel Server Functions
#'
#' @noRd
mod_utmzonesel_server <- function(id, settings){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$map <- renderLeaflet({
      mapview::mapview(map.types = c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron"))@map |>
        setView(lng = 0, lat = 0, zoom = 2)
    })


    # Example function to calculate UTM details using sf
    calculate_utm_details <- function(lat, lon) {
      # Create an sf point in WGS84
      point <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)

      # Calculate UTM zone based on longitude
      utm_zone <- floor((lon + 180) / 6) + 1
      epsg_code <- ifelse(lat >= 0, 32600 + utm_zone, 32700 + utm_zone)

      # Transform to UTM
      point_utm <- sf::st_transform(point, crs = epsg_code)
      coords_utm <- sf::st_coordinates(point_utm)

      # Determine Latitude Band
      lat_bands <- c(letters[3:8], letters[10:13], letters[15:23], letters[25:26]) # Exclude I and O
      band_index <- floor((lat + 80) / 8) + 1
      band_letter <- ifelse(band_index >= 1 & band_index <= length(lat_bands),
                            toupper(lat_bands[band_index]), "Unknown")

      # Central Meridian
      central_meridian <- (utm_zone - 1) * 6 - 180 + 3
      list(
        zone = utm_zone,
        band = band_letter,
        central_meridian = central_meridian,
        easting = coords_utm[1],
        northing = coords_utm[2],
        epsg = epsg_code
      )
    }

    observeEvent(input$map_click, {
      click <- input$map_click
      lon <- click$lng
      lat <- click$lat
      # Validate longitude
      if (lon < -180 || lon > 180) {
        showNotification("Invalid longitude! Please select a value between -180 and 180.", type = "error")
        return()
      }
      utm_details <- calculate_utm_details(lat, lon)

      updateNumericInput(session, "latitude", value = lat)
      updateNumericInput(session, "longitude", value = lon)
      updateTextInput(session, "epsg", value = utm_details$epsg)
      output$utm_zone <- renderText(paste("UTM Zone:", utm_details$zone))
      output$utm_band <- renderText(paste("Band:", utm_details$band))
      output$central_meridian <- renderText(paste("Central Meridian:", utm_details$central_meridian, "°"))
      output$easting <- renderText(paste("Easting:", round(utm_details$easting, 3), "m"))
      output$northing <- renderText(paste("Northing:", round(utm_details$northing, 3), "m"))
      output$utmcoord <- renderText(paste("UTM Coordinates:", round(utm_details$easting, 3), ", ", round(utm_details$northing, 3)))

      leafletProxy("map") |>
        clearShapes() |>
        clearMarkers() |>
        addMarkers(lng = lon, lat = lat, popup = paste("Zone:", utm_details$zone, "Band:", utm_details$band)) |>
        addRectangles(
          lng1 = utm_details$central_meridian - 3, lat1 = -80,
          lng2 = utm_details$central_meridian + 3, lat2 = 84,
          fillColor = "salmon",
          color = "salmon",
          fillOpacity = 0.5,
          weight = 2
        )
    })

    observeEvent(input$update_map, {
      lon <- as.numeric(input$longitude)
      lat <- as.numeric(input$latitude)
      # Validate longitude
      if (lon < -180 || lon > 180) {
        showNotification("Invalid longitude! Please select a value between -180 and 180.", type = "error")
        return()
      }
      if (!is.na(lon) & !is.na(lat)) {
        utm_details <- calculate_utm_details(lat, lon)

        updateTextInput(session, "epsg", value = utm_details$epsg)
        output$utm_zone <- renderText(paste("UTM Zone:", utm_details$zone))
        output$utm_band <- renderText(paste("Band:", utm_details$band))
        output$central_meridian <- renderText(paste("Central Meridian:", utm_details$central_meridian, "°"))
        output$easting <- renderText(paste("Easting:", round(utm_details$easting, 3), "m"))
        output$northing <- renderText(paste("Northing:", round(utm_details$northing, 3), "m"))
        output$utmcoord <- renderText(paste("UTM Coordinates:", round(utm_details$easting, 3), ", ", round(utm_details$northing, 3)))

        leafletProxy("map") |>
          clearShapes() |>
          clearMarkers() |>
          addMarkers(lng = lon, lat = lat, popup = paste("Zone:", utm_details$zone, "Band:", utm_details$band)) |>
          setView(lng = lon, lat = lat, zoom = 8)
      }
    })
  })
}

## To be copied in the UI
# mod_utmzonesel_ui("utmzonesel_1")

## To be copied in the server
# mod_utmzonesel_server("utmzonesel_1")
