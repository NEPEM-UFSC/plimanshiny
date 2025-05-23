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
          footer = "To start cropping a mosaic, click on 'Start!' and use the
          'Draw Polygon, or 'Draw Rectangle' tools. To crop the mosaic,
          click on 'Crop'",
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
            choices = c("Crop", "Mask"),
            selected = "Crop",
            inline = FALSE,
            status = "success"
          ),
          hl(),
          h3("Output"),
          textInput(ns("new_cropped"),
                    label = "New object",
                    value = NULL),

          fluidRow(
            col_6(
              actionBttn(ns("startcrop"),
                         label = "Start cropping!",
                         style = "pill",
                         color = "success")
            ),
            col_6(
              actionBttn(ns("cropmosaic"),
                         label = "Crop!",
                         style = "pill",
                         no_outline = FALSE,
                         icon = icon("scissors"),
                         color = "success")
            )
          )
        )
      ),
      col_9(
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "720px",
          title = "Cropping a mosaic",
          selected = "Original mosaic",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Original mosaic",
            conditionalPanel("input.shapemanipula != true",  ns = ns,
                             h3("Original mosaic"),
                             editModUI(ns("mosaic_crop"), height = "640px") |> add_spinner()
            ),
            conditionalPanel("input.shapemanipula == true",  ns = ns,
                             h3("Original mosaic and shapefile"),
                             leafletOutput(ns("mosaic_cropshp"), height = "640px") |> add_spinner()
            )
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
mod_crop_server <- function(id, mosaic_data, shapefile, r, g, b, basemap, settings){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observe({
      req(shapefile)
      req(mosaic_data)
      updateSelectInput(session, "mosaic_to_crop", choices = c("Active mosaic", setdiff(names(mosaic_data), "mosaic")), selected = "Active mosaic")
      updateSelectInput(session, "shape_to_crop", choices = setdiff(names(shapefile), c("shapefile", "shapefileplot")))
    })
    observe({
      updateTextInput(session, "new_cropped", value = paste0(input$mosaic_to_crop, "_cropped"))
    })


    # Observe event for mosaic crop action
    observeEvent(input$startcrop, {

      cropped_mosaic <- reactiveVal(NULL)
      if(input$shapemanipula){
        shptocrop <- shapefile[[input$shape_to_crop]]$data
        output$mosaic_cropshp <- renderLeaflet({
          if(input$mosaic_to_crop == "Active mosaic" && !is.null(basemap$map)){
            bcrop <- basemap$map
          } else{
            bcrop <-
              mosaic_view(
                mosaic_data[[input$mosaic_to_crop]]$data,
                r = suppressWarnings(as.numeric(r$r)),
                g = suppressWarnings(as.numeric(g$g)),
                b = suppressWarnings(as.numeric(b$b)),
                max_pixels = 500000
              )
          }

          (bcrop + shapefile_view(shptocrop))@map
        })

        mosaiccr <- terra::crop(mosaic_data$mosaic$data, terra::ext(shapefile_input(shptocrop, as_sf = FALSE, info = FALSE)))
        cropped_mosaic(mosaiccr)

      } else{
        sendSweetAlert(
          session = session,
          title = "Cropping a mosaic",
          text = "Use the 'Draw Rectangle' or 'Draw Polygon' tools to crop the mosaic. The mosaic will be cropped to the extend of the created shapes.",
          type = "info"
        )
        # Reactive expression to store the cropped mosaic
        if(input$mosaic_to_crop == "Active mosaic" && !is.null(basemap$map)){
          mapcrop <- basemap$map@map
        } else{
          mapcrop <-
            mosaic_view(
              mosaic_data[[input$mosaic_to_crop]]$data,
              r = suppressWarnings(as.numeric(r$r)),
              g = suppressWarnings(as.numeric(g$g)),
              b = suppressWarnings(as.numeric(b$b)),
              max_pixels = 300000
            )@map
        }
        # # Attempt to get edits
        edits <- callModule(editMod, "mosaic_crop", mapcrop, editor = "leafpm")
        #
        observe({
          # Check if edits()$finished is not NULL
          if (!is.null(edits()$finished)) {
            grids <-
              edits()$finished |>
              sf::st_transform(sf::st_crs(mosaic_data$mosaic$data)) |>
              terra::vect()
              mosaiccr <- terra::crop(mosaic_data$mosaic$data, grids |> terra::ext())
            if(input$cropormask == "Mask"){
              mosaiccr <- terra::mask(mosaiccr, grids)
            }
            cropped_mosaic(mosaiccr)
          }
        })
      }

      output$mosaiccropped <- renderPlot({
        req(cropped_mosaic())  # Ensure cropped_mosaic is not NULL
        terra::plotRGB(cropped_mosaic())
      })

      # Observe event for mosaic crop action
      observeEvent(input$cropmosaic, {
        # Update mosaic_data$mosaic$data when input$cropmosaic is clicked
        mosaic_data[[input$new_cropped]] <- create_reactval(name = input$new_cropped, data = cropped_mosaic())
        sendSweetAlert(
          session = session,
          title = "Mosaic successfully cropped!!",
          text = "The mosaic has been successfully cropped and is now available for further analysis.",
          type = "success"
        )
      })
    })
  })
}

## To be copied in the UI
# mod_crop_ui("crop_1")

## To be copied in the server
# mod_crop_server("crop_1")
