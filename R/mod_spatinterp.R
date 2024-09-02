#' spatinterp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_spatinterp_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_4(
        bs4TabCard(
          id = "tabs",
          width = 12,
          height = "790px",
          status = "success",
          title = "Interpolation settings",
          selected = "Input data",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Input data",
            prettyRadioButtons(
              inputId = ns("dforshape"),
              label = "Use",
              choices = c("data.frame", "shapefile"),
              icon = icon("check"),
              bigger = TRUE,
              status = "info",
              animation = "jelly",
              inline = TRUE
            ),
            pickerInput(
              ns("dfinterp"),
              label = "Input data",
              choices = NULL
            ),
            fluidRow(
              col_4(
                pickerInput(
                  ns("xval"),
                  label = "x",
                  choices = NULL
                )
              ),
              col_4(
                pickerInput(
                  ns("yval"),
                  label = "y",
                  choices = NULL
                )
              ),
              col_4(
                pickerInput(
                  ns("zval"),
                  label = "z",
                  choices = NULL
                )
              )
            ),
            awesomeRadio(
              inputId = ns("interpmethod"),
              label = "Method",
              choices = c("Kriging", "Tps"),
              selected = "Kriging",
              inline = TRUE,
              status = "success"
            ),
            fluidRow(
              col_6(
                sliderInput(
                  ns("resx"),
                  label = "x resolution",
                  min = 10,
                  max = 1000,
                  value = 200
                )
              ),
              col_6(
                sliderInput(
                  ns("resy"),
                  label = "y resolution",
                  min = 10,
                  max = 1000,
                  value = 200
                )
              )
            ),
            prettyCheckbox(
              inputId = ns("contours"),
              label = "Contour line",
              value = TRUE,
              icon = icon("check"),
              status = "success",
              animation = "rotate"
            ),
            actionBttn(
              ns("interpolate"),
              label = "Interpolate",
              icon = icon("check")
            ),
            conditionalPanel(
              condition = "input.interpolate%2==1", ns = ns,
              hl(),
              mod_download_mosaic_ui(ns("downloadinterp"), "Download Raster")

            )

          )
        )
      ),
      col_8(
        bs4TabCard(
          id = "tabs",
          width = 12,
          height = "790px",
          status = "success",
          selected = "Points",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            "Points",
            fluidRow(
              col_10(
                pickerpalette(id, "colorpalette2", selected = "RdYlGn"),
              ),
              col_2(
                prettyCheckbox(
                  inputId = ns("revert2"),
                  label = "Reverse",
                  value = FALSE,
                  icon = icon("check"),
                  status = "success",
                  animation = "rotate"
                )
              )
            ),
            plotOutput(ns("oripoints"),  height = "660px") |> add_spinner()
          ),
          tabPanel(
            title = "Interpolated",
            fluidRow(
              col_2(
                numericInput(
                  ns("colnumber"),
                  label = "Number of colors",
                  value = 100,
                )
              ),
              col_6(
                pickerpalette(id, "colorpalette", selected = "RdYlGn"),
              ),
              col_2(
                prettyCheckbox(
                  inputId = ns("revert"),
                  label = "Reverse",
                  value = FALSE,
                  icon = icon("check"),
                  status = "success",
                  animation = "rotate"
                )
              ),
              col_2(
                downloadBttn(ns("downloadinterp"),
                             label = "Take a shot",
                             style = "pill")
              )
            ),
            plotOutput(ns("interpplot"),  height = "660px") |> add_spinner()
          )
        )
      )
    )
  )
}

#' spatinterp Server Functions
#'
#' @noRd
mod_spatinterp_server <- function(id, dfs, shapefile){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      if(input$dforshape == "data.frame"){
        updatePickerInput(session, "dfinterp",
                          choices = c("none", names(dfs)))
      } else{
        updatePickerInput(session, "dfinterp",
                          choices = c("none", setdiff(names(shapefile), c("shapefile", "shapefileplot"))))
      }
    })

    dfactive <- reactiveValues()
    observeEvent(input$dfinterp, {
      if(input$dfinterp != "none"){
        if(input$dforshape == "data.frame"){
          dfactive$df <- dfs[[input$dfinterp]]$data |> convert_numeric_cols()
        } else{
          dfactive$df <- shapefile[[input$dfinterp]]$data |> convert_numeric_cols()
        }
      }
    })
    observe({
      req(dfactive$df)
      updatePickerInput(session, "xval", choices = colnames(dfactive$df))
    })
    observe({
      req(dfactive$df)
      updatePickerInput(session, "yval", choices = colnames(dfactive$df))
    })
    observe({
      req(dfactive$df)
      updatePickerInput(session, "zval", choices = colnames(dfactive$df))
    })

    output$oripoints <- renderPlot({
      req(dfactive$df)
      x <- dfactive$df |> dplyr::pull(!!sym(input$xval))
      y <- dfactive$df |> dplyr::pull(!!sym(input$yval))
      z <- dfactive$df |> dplyr::pull(!!sym(input$zval))

      ggplot(data.frame(x, y, z), aes(x = x, y = y, color = z)) +
        scale_color_gradientn(colors = return_colors(input$colorpalette2, reverse = input$revert2, n = 10)) +
        geom_point(size = 3) +
        labs(x = "Longitute (UTM)",
             y = "Latitude (UTM)",
             color = "") +
        theme_bw(base_size = 20)
    })


    observeEvent(input$interpolate, {
      fitted <- reactiveValues(mod = NULL)
      x <- dfactive$df |> dplyr::pull(!!sym(input$xval))
      y <- dfactive$df |> dplyr::pull(!!sym(input$yval))
      z <- dfactive$df |> dplyr::pull(!!sym(input$zval))

      if(input$interpmethod == "Kriging"){
        fit <- fields::Krig(cbind(x, y), z, aRange=20)
        fitted$mod <- fit
      }
      if(input$interpmethod == "Tps"){
        fit <- fields::Tps(cbind(x, y), z)
        fitted$mod <- fit
      }

      output$interpplot <- renderPlot({
        fields::surface(fitted$mod,
                        asp = 1,
                        type=ifelse(input$contours, "C", "I"),
                        nx = input$resx,
                        ny = input$resy,
                        xlab = "Longitute (UTM)",
                        ylab = "Latitude (UTM)",
                        col = return_colors(input$colorpalette, reverse = input$revert, n = input$colnumber))

      })
      output$downloadinterp <- downloadHandler(
        filename = "surface.jpg",
        content = function(file) {
          jpeg(file, height = 1280, width = 1280)
          fields::surface(fitted$mod,
                          asp = 1,
                          type=ifelse(input$contours, "C", "I"),
                          nx = input$resx,
                          ny = input$resy,
                          xlab = "Longitute (UTM)",
                          ylab = "Latitude (UTM)",
                          col = return_colors(input$colorpalette, reverse = input$revert, n = input$colnumber))
          dev.off()
        }
      )


      xy <- cbind(x, y)
      # Create a grid for predictions
      grid_x <- seq(min(xy[, 1]), max(xy[, 1]), length.out = input$resx)
      grid_y <- seq(min(xy[, 2]), max(xy[, 2]), length.out = input$resy)

      grid <- expand.grid(grid_x, grid_y)

      # Predict using the fitted TPS model
      predictions <- predict(fitted$mod, grid)

      # Reshape the predictions to match the grid
      z_matrix <- matrix(predictions,
                         nrow = length(grid_x),
                         ncol = length(grid_y),
                         byrow = TRUE)

      # Convert to a SpatRaster
      rast <- terra::rast(nrows = length(grid_y),
                          ncols = length(grid_x),
                          xmin = min(grid_x),
                          xmax = max(grid_x),
                          ymin = min(grid_y),
                          ymax = max(grid_y))
      terra::values(rast) <- z_matrix
      mod_download_mosaic_server("downloadinterp", rast)



    })

  })
}

## To be copied in the UI
# mod_spatinterp_ui("spatinterp_1")

## To be copied in the server
# mod_spatinterp_server("spatinterp_1")
