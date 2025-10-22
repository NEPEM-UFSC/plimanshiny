#' crop UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_crop_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Crop Settings",
          collapsible = FALSE,
          width = 12,
          footer = "Draw points that defines the cropping area pressing the left-mouse button for 1 second.'",
          h3("Input"),
          selectInput(ns("mosaic_to_crop"),
                      label = "Mosaic to be cropped",
                      choices = NULL),
          prettyCheckbox(
            inputId = ns("shapemanipula"),
            label = "Crop using a shapefile?",
            value = FALSE,
            icon = icon("check"),
            status = "success",
            animation = "rotate"
          ),
          conditionalPanel("input.shapemanipula == true",  ns = ns,
                           selectInput(ns("shape_to_crop"),
                                       label = "Shapefile",
                                       choices = NULL),
          ),
          awesomeRadio(
            inputId = ns("cropormask"),
            label = "Type",
            choices = c("crop", "mask"),
            selected = "crop",
            inline = FALSE,
            status = "success"
          ),
          hl(),
          h3("Output"),
          textInput(ns("new_cropped"),
                    label = "New object",
                    value = NULL),

          actionBttn(ns("cropmosaic"),
                     label = "Crop!",
                     style = "pill",
                     no_outline = FALSE,
                     icon = icon("scissors"),
                     color = "success")
        )
      ),
      col_9(
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          title = "Cropping a mosaic",
          selected = "Original mosaic",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Original mosaic",
            conditionalPanel("input.shapemanipula == true",  ns = ns,
                             plotOutput(ns("plotoriginalraster"), height = "640px")
            ),
            conditionalPanel("input.shapemanipula == false",  ns = ns,
                             plimanshiny_canvas_ui(ns("cropmos"))
            ),
          ),
          tabPanel(
            title = "Cropped mosaic",
            plotOutput(ns("mosaiccropped"), height = "640px") |> add_spinner()
          )
        )
      )
    )
  )
}

#' crop Server Functions
#'
#' @noRd
mod_crop_server <- function(id, mosaic_data, shapefile, r, g, b, basemap, settings, zlim){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observe({
      req(mosaic_data)
      updateSelectInput(session, "mosaic_to_crop", choices = setdiff(names(mosaic_data), "mosaic"))
      updateSelectInput(session, "shape_to_crop", choices = setdiff(names(shapefile), c("shapefile", "shapefileplot")))
    })

    observe({
      updateTextInput(session, "new_cropped", value = paste0(input$mosaic_to_crop, "_cropped"))
    })

    # --- MODE 1: Crop by drawing (canvas) ---
    observe({
      req(!input$shapemanipula)
      req(input$mosaic_to_crop)
      req(mosaic_data[[input$mosaic_to_crop]]$data)

      mosaic_to_plot <- reactive({
        mosaic_data[[input$mosaic_to_crop]]$data
      })

      pointsvals <- plimanshiny_canvas_server(
        id = "cropmos",
        mosaic_to_plot(),
        r = suppressWarnings(as.numeric(r$r)),
        g = suppressWarnings(as.numeric(g$g)),
        b = suppressWarnings(as.numeric(b$b)),
        zlim = zlim$zlim,
        max_width = 1000
      )

      observeEvent(input$cropmosaic, {
        if(!input$shapemanipula){
          points_df <- pointsvals()
          if (!is.null(points_df) && nrow(points_df) > 2) {
            coords_matrix <- as.matrix(points_df[, c("x", "y")])
            if (!all(coords_matrix[1, ] == coords_matrix[nrow(coords_matrix), ])) {
              coords_matrix <- rbind(coords_matrix, coords_matrix[1, ])
            }
            points_vector <- terra::vect(
              x = list(coords_matrix),
              type = "polygon",
              crs = terra::crs(mosaic_to_plot())
            )
            cropped_mosaic <- mosaic_crop(
              mosaic_to_plot(),
              shapefile = sf::st_as_sf(points_vector),
              type = input$cropormask
            )

            output$mosaiccropped <- renderPlot({
              check_and_plot(
                cropped_mosaic,
                r = suppressWarnings(as.numeric(r$r)),
                g = suppressWarnings(as.numeric(g$g)),
                b = suppressWarnings(as.numeric(b$b)),
                zlim = zlim$zlim
              )
            })
            mosaic_data[[input$new_cropped]] <- create_reactval(input$new_cropped, cropped_mosaic)
            show_alert("Done",
                       text = "The mosaic has been cropped and is now available for use in the 'Raster file(s)' tab.",
                       type = "success")
          } else {
            showNotification("No points have been drawn yet. Make sure you draw at least 3 points.", type = "warning")
          }
        }
      })
    })

    # --- MODE 2: Crop using shapefile ---
    observe({
      req(input$shapemanipula)
      req(input$mosaic_to_crop)
      req(input$shape_to_crop)
      req(mosaic_data[[input$mosaic_to_crop]]$data)

      output$plotoriginalraster <- renderPlot({
        check_and_plot(
          mosaic_data[[input$mosaic_to_crop]]$data,
          r = suppressWarnings(as.numeric(r$r)),
          g = suppressWarnings(as.numeric(g$g)),
          b = suppressWarnings(as.numeric(b$b)),
          zlim = zlim$zlim
        )
        shapefile_plot(shapefile[[input$shape_to_crop]]$data, add = TRUE)
      })

      observeEvent(input$cropmosaic, {
        if(input$shapemanipula){
          cropped_mosaic <- mosaic_crop(
            mosaic_data[[input$mosaic_to_crop]]$data,
            shapefile = shapefile[[input$shape_to_crop]]$data,
            type = input$cropormask
          )
          output$mosaiccropped <- renderPlot({
            check_and_plot(
              cropped_mosaic,
              r = suppressWarnings(as.numeric(r$r)),
              g = suppressWarnings(as.numeric(g$g)),
              b = suppressWarnings(as.numeric(b$b)),
              zlim = zlim$zlim
            )
          })
          mosaic_data[[input$new_cropped]] <- create_reactval(input$new_cropped, cropped_mosaic)
          show_alert("Done",
                     text = "The mosaic has been cropped and is now available for use in the 'Raster file(s)' tab.",
                     type = "success")
        }
      })
    })
  })
}


## To be copied in the UI
# mod_crop_ui("crop_1")

## To be copied in the server
# mod_crop_server("crop_1")
