#' compslider UI Function
#'
#' @description A shiny Module that nests the slider module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_compslider_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          title = "",
          selected = "Raster Selection",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Raster Selection",
            hl(),
            selectInput(ns("beforeimg"), label = "Left raster", choices = NULL),
            prettyCheckbox(
              inputId = ns("rgb1"),
              label = "RGB",
              value = FALSE
            ),
            conditionalPanel(
              condition = "input.rgb1 == true", ns = ns,
              fluidRow(
                col_4(
                  pickerInput(ns("r1"), "Red", choices = NULL),
                ),
                col_4(
                  pickerInput(ns("g1"), "Green", choices = NULL),
                ),
                col_4(
                  pickerInput(ns("b1"), "Blue", choices = NULL)
                )
              ),
              prettyCheckbox(
                inputId = ns("minmaximg1"),
                label = "Set range of values to plot ",
                value = FALSE,
                icon = icon("check"),
                status = "success",
                animation = "rotate"
              ),
              conditionalPanel(
                condition = "input.minmaximg1 == true", ns = ns,
                mod_histo_slider_ui(ns("histoimg1"),
                                    data = rnorm(100),
                                    width = "100%",
                                    height = "150px",
                                    n_bins = 75),
                br(),br(),
              )
            ),
            conditionalPanel(
              condition = "input.rgb1 == false", ns = ns,
              pickerInput(ns("layer1"), "Layer to plot", choices = NULL)
            ),
            hl(),
            selectInput(ns("afterimg"), label = "Right raster", choices = NULL),
            prettyCheckbox(
              inputId = ns("rgb2"),
              label = "RGB",
              value = FALSE
            ),
            conditionalPanel(
              condition = "input.rgb2 == true", ns = ns,
              fluidRow(
                col_4(
                  pickerInput(ns("r2"), "Red", choices = NULL),
                ),
                col_4(
                  pickerInput(ns("g2"), "Green", choices = NULL),
                ),
                col_4(
                  pickerInput(ns("b2"), "Blue", choices = NULL)
                )
              ),
              prettyCheckbox(
                inputId = ns("minmaximg2"),
                label = "Set range of values to plot ",
                value = FALSE,
                icon = icon("check"),
                status = "success",
                animation = "rotate"
              ),
              conditionalPanel(
                condition = "input.minmaximg2 == true", ns = ns,
                mod_histo_slider_ui(ns("histoimg2"),
                                    data = rnorm(100),
                                    width = "100%",
                                    height = "150px",
                                    n_bins = 75),
                br(),br(),
              )
            ),
            conditionalPanel(
              condition = "input.rgb2 == false", ns = ns,
              pickerInput(ns("layer2"), "Layer to plot", choices = NULL)
            ),
            hl(),
            prettyCheckbox(
              inputId = ns("compare"),
              label = "Selection done. Put them side-by-side!",
              value = FALSE,
              icon = icon("check"),
              status = "success",
              animation = "rotate"
            )
          )
        )
      ),
      col_9(
        mod_slider_zoom_ui(ns("sliderui"))
      )
    )

  )
}

#' compslider Server Function
#'
#' @noRd
mod_compslider_server <- function(id, mosaic_data, index) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tmplist <- reactiveVal()
    observe({
      tmplist1 <-
        lapply(reactiveValuesToList(mosaic_data), function(x){
          reactiveValuesToList(x)$data
        })
      tmplist2 <-
        lapply(reactiveValuesToList(index), function(x){
          reactiveValuesToList(x)$data
        })
      tmplist(c(tmplist1, tmplist2))
    })


    observe({
      req(tmplist())
      updateSelectInput(session, "beforeimg", choices = names(tmplist()), selected = NULL)
    })
    observe({
      req(tmplist())
      updateSelectInput(session, "afterimg", choices = names(tmplist()), selected = NULL)
    })
    #

    zlim1 <- reactiveVal(NULL)
    zlim2 <- reactiveVal(NULL)
    imgleft <- reactiveVal(NULL)
    imgright <- reactiveVal(NULL)
    nameleft <- reactiveVal(NULL)
    nameright <- reactiveVal(NULL)
    ############## LEFT IMAGE ##################
    observe({
      req(input$beforeimg)
      if (input$rgb1) {
        nlr1 <- nlyr(tmplist()[[input$beforeimg]])
        updatePickerInput(session, "r1", choices = 1:nlr1)
        updatePickerInput(session, "g1", choices = 1:nlr1, selected = "2")
        updatePickerInput(session, "b1", choices = 1:nlr1, selected = "3")
        imgleft(tmplist()[[input$beforeimg]])
        nameleft(input$beforeimg)
      }
    })
    observe({
      if (!input$rgb1) {
        updatePickerInput(session, "layer1", choices = names(tmplist()[[input$beforeimg]]), selected = NA)
      }
    })
    observe({
      if (!input$rgb1) {
        req(input$layer1)
        try(imgleft(tmplist()[[input$beforeimg]][[input$layer1]]))
        nameleft(input$layer1)
      }
    })
    observeEvent(input$minmaximg1, {
      if(input$minmaximg1){
        req(imgleft())
        vals <- terra::spatSample(imgleft()[[c(as.numeric(input$r1),
                                               as.numeric(input$g1),
                                               as.numeric(input$b1))]], 3000)
        slider_range <- mod_histo_slider_server("histoimg1", data_reactive = reactiveVal(as.numeric(as.matrix(vals))))
        observe({
          zlim1(c(slider_range()$min, slider_range()$max))
        })
      }
    })


    ############## RIGHT IMAGE ##################
    observe({
      if (input$rgb2) {
        req(input$afterimg)
        nlr2 <- nlyr(tmplist()[[input$afterimg]])
        updatePickerInput(session, "r2", choices = 1:nlr2)
        updatePickerInput(session, "g2", choices = 1:nlr2, selected = "2")
        updatePickerInput(session, "b2", choices = 1:nlr2, selected = "3")
        imgleft(tmplist()[[input$afterimg]])
        nameright(input$afterimg)
      }
    })
    observe({
      if (!input$rgb2) {
        updatePickerInput(session, "layer2", choices = names(tmplist()[[input$afterimg]]), selected = NA)
      }
    })
    observe({
      if (!input$rgb2) {
        req(input$layer2)
        try(imgright(tmplist()[[input$afterimg]][[input$layer2]]))
        nameright(input$layer2)
      }
    })
    observeEvent(input$minmaximg2, {
      if(input$minmaximg2){
        req(imgright())
        vals <- terra::spatSample(imgright()[[c(as.numeric(input$r2),
                                                as.numeric(input$g2),
                                                as.numeric(input$b2))]], 3000)
        slider_range <- mod_histo_slider_server("histoimg2", data_reactive = reactiveVal(as.numeric(as.matrix(vals))))
        observe({
          zlim2(c(slider_range()$min, slider_range()$max))
        })
      }
    })
    #
    observe({
      if(input$compare){
        # observe({
        invisible(zlim1())
        invisible(zlim2())
        invisible(input$r1)
        invisible(input$g1)
        invisible(input$b1)
        invisible(input$r2)
        invisible(input$g2)
        invisible(input$b2)
        invisible(input$minmaximg1)
        invisible(input$minmaximg2)
        req(input$beforeimg, input$afterimg)
        servervals <- mod_slider_zoom_server("sliderui",
                                             img1 = imgleft(),
                                             name1 = nameleft(),
                                             rgb1 = input$rgb1,
                                             r1 = as.numeric(input$r1),
                                             g1 = as.numeric(input$g1),
                                             b1 = as.numeric(input$b1),
                                             zlim1 =  zlim1(),
                                             img2 = imgright(),
                                             name2 = nameright(),
                                             rgb2 = input$rgb2,
                                             r2 = as.numeric(input$r2),
                                             g2 = as.numeric(input$g2),
                                             b2 = as.numeric(input$b2),
                                             zlim2 = zlim2())
        observeEvent(servervals()$drawn_rectangle, {
          rect <- servervals()$drawn_rectangle
          req(rect$startX)
          if (rect$startX == rect$endX || rect$startY == rect$endY) {
            return(NULL)
          }
          if(rect$startX == 0 & rect$startY == 0) {
            mod_slider_zoom_server("sliderui",
                                   img1 = imgleft(),
                                   name1 =  nameleft(),
                                   rgb1 = input$rgb1,
                                   r1 = as.numeric(input$r1),
                                   g1 = as.numeric(input$g1),
                                   b1 = as.numeric(input$b1),
                                   zlim1 =  zlim1(),
                                   img2 = imgright(),
                                   name2 = nameright(),
                                   rgb2 = input$rgb2,
                                   r2 = as.numeric(input$r2),
                                   g2 = as.numeric(input$g2),
                                   b2 = as.numeric(input$b2),
                                   zlim2 =  zlim2())
          } else{
            xmin_val <- terra::xmin(imgleft())
            xmax_val <- terra::xmax(imgleft())
            ymin_val <- terra::ymin(imgleft())
            ymax_val <- terra::ymax(imgleft())

            fact_canva_rast_x <- servervals()$canvas_size$width / (xmax_val - xmin_val)
            fact_canva_rast_y <- servervals()$canvas_size$height / (ymax_val - ymin_val)
            xmin <- xmin_val + rect$startX / fact_canva_rast_x
            xmax <- xmin_val + rect$endX / fact_canva_rast_x
            ymin <- ymin_val + (servervals()$canvas_size$height - rect$endY) / fact_canva_rast_y
            ymax <- ymin_val + (servervals()$canvas_size$height - rect$startY) / fact_canva_rast_y

            img1cropped <- terra::crop(imgleft(), terra::ext(xmin, xmax, ymin, ymax))
            img2cropped <- terra::crop(imgright(), terra::ext(xmin, xmax, ymin, ymax))

            mod_slider_zoom_server("sliderui",
                                   img1 = img1cropped,
                                   name1 =  nameleft(),
                                   rgb1 = input$rgb1,
                                   r1 = as.numeric(input$r1),
                                   g1 = as.numeric(input$g1),
                                   b1 = as.numeric(input$b1),
                                   zlim1 =   zlim1(),
                                   img2 = img2cropped,
                                   name2 = nameright(),
                                   rgb2 = input$rgb2,
                                   r2 = as.numeric(input$r2),
                                   g2 = as.numeric(input$g2),
                                   b2 = as.numeric(input$b2),
                                   zlim2 =  zlim2())
          }
        })
        # })
      }
    })
  })
}
