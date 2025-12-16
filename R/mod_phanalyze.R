#' phanalyze UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_phanalyze_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Canopy Height Models and more...",
          collapsible = FALSE,
          width = 12,
          icon = icon("mountain-sun"),
          awesomeRadio(
            inputId = ns("inputdsmtype"),
            label = "Entry method",
            choices = c("Imported rasters", "Example raster"),
            selected = "Imported rasters",
            status = "success",
            inline = TRUE
          ),
          hl(),
          prettyRadioButtons(
            inputId = ns("strategy"),
            label = "Strategy",
            choices = c("I have DSM and DTM", "Build DTM using sampling points", "Build DTM using a moving window"),
            icon = icon("check"),
            bigger = TRUE,
            status = "info",
            animation = "jelly"
          ),
          conditionalPanel(
            condition = "input.strategy != 'I have DSM and DTM'", ns = ns,
            sliderInput(
              ns("ground_quantile"),
              label = "Ground quantile",
              min = 0,
              max = 1,
              value = 0,
              step = 0.01
            ),
          ),
          pickerInput(
            ns("dsm"),
            label = "Digital Surface Model",
            choices = NULL
          ),
          conditionalPanel(
            condition = "input.strategy == 'I have DSM and DTM'", ns = ns,
            pickerInput(
              ns("dtm"),
              label = "Digital Terrain Model",
              choices = NULL
            )
          ),
          conditionalPanel(
            condition = "input.strategy == 'Build DTM using a moving window'", ns = ns,
            textInput(
              ns("windowsize"),
              label = "Window size (meters)",
              value = c("5,5")
            )
          ),
          conditionalPanel(
            condition = "input.strategy != 'I have DSM and DTM'", ns = ns,
            prettyRadioButtons(
              inputId = ns("interpmethod"),
              label = "Interpolation method",
              choices = c("Thin Plate Spline", "Kriging"),
              icon = icon("check"),
              bigger = TRUE,
              status = "info",
              animation = "jelly"
            )
          ),
          pickerInput(
            ns("shapefile"),
            label = "Shapefile",
            choices = NULL
          ),
          prettyRadioButtons(
            ns("masktype"),
            label = "Mask type",
            choices = c("none", "raster", "threshold"),
            selected = "none",
            inline = TRUE
          ),
          conditionalPanel(
            condition = "input.masktype == 'raster'", ns = ns,
            pickerInput(
              ns("maskfile"),
              label = "Mask (optional)",
              choices = "none"
            ),
            prettyCheckbox(
              inputId = ns("masksoil"),
              label = "Mask shows soil?",
              value = FALSE,
            )
          ),
          conditionalPanel(
            condition = "input.masktype == 'threshold'", ns = ns,
            numericInput(
              ns("dsmthresh"),
              label = "CHM threshold",
              value = 0,
            )
          ),
          prettyCheckbox(
            inputId = ns("croptoext"),
            label = "Crop to the shapefile extend?",
            value = FALSE,
            icon = icon("check"),
            status = "success",
            animation = "rotate"
          ),
          prettyCheckbox(
            ns("plotquality"),
            label = "Plot quality",
            value = FALSE
          ),
          conditionalPanel(
            condition = "input.plotquality == true", ns = ns,
            numericInput(
              ns("chm_threshold"),
              label = "CHM threshold",
              value = 0.1,
            ),
            sliderInput(
              ns("chm_quantile"),
              label = "CHM Quantile for gap identification",
              min = 0,
              max = 1,
              value = 0.3
            ),
            prettyRadioButtons(
              ns("qualitytype"),
              label = "Quality type",
              choices = c("absolute", "relative"),
              selected = "absolute",
              inline = TRUE
            )
          ),
          hl(),
          uiOutput(ns("computearea"))
        )
      ),
      col_9(
        uiOutput(ns("resultphmodel"))
      )
    )
  )
}

#' phanalyze Server Functions
#'
#' @noRd
mod_phanalyze_server <- function(id, mosaic_data, shapefile, basemap, dfs, settings){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$resultphmodel <- renderUI({
      if(input$strategy == "I have DSM and DTM"){
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "790px",
          title = "Results",
          selected = "Home",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Home",
            fluidRow(
              col_9(
                img(src = "www/logodsm.jpg", width = "100%", height = "90%")
              ),
              col_3(
                h2("About"),
                "This module offers strategies for computing Canopy Height Models (CHM), providing valuable tools for computing canopy-related measures, such as height and volume.", br(), br(),
                h2("Disclaimer"),
                "We welcome your feedback and suggestions on the application's usefulness. Please note that we do not guarantee the correctness, reliability, or utility of the results, especially if incorrect settings are applied during the CHM estimation process.", br(), br(),
                h2("Image Reference"),
                "The image used in this module was generated using advanced AI tools to represent the digital surface model (DSM)."
              )
            )
          ),
          tabPanel(
            title = "Overview",
            fluidRow(
              valueBoxOutput(ns("vbnplots"), width = 3),
              valueBoxOutput(ns("vbnminabs"), width = 3),
              valueBoxOutput(ns("vbnmaxabs"), width = 3),
              valueBoxOutput(ns("vbnmeanq95"), width = 3),
              valueBoxOutput(ns("vbnminvol"), width = 3),
              valueBoxOutput(ns("vbnmeanvol"), width = 3),
              valueBoxOutput(ns("vbnmaxvol"), width = 3),
              valueBoxOutput(ns("vbntotvol"), width = 3),
              valueBoxOutput(ns("vbncoveredmin"), width = 3),
              valueBoxOutput(ns("vbncoveredmax"), width = 3),
              valueBoxOutput(ns("vbncovermed"), width = 3),
              valueBoxOutput(ns("vbnentropy"), width = 3),
            ),

            # Só mostra se plotquality == TRUE
            conditionalPanel(
              condition = "input.plotquality == true", ns = ns,
              fluidRow(
                valueBoxOutput(ns("vbnmingap"), width = 3),
                valueBoxOutput(ns("vbnmaxgap"), width = 3),
                valueBoxOutput(ns("vbntotalgap"), width = 3)
              )
            ),
          ),
          tabPanel(
            title = "Trait distribution",
            plotlyOutput(ns("overview"), height = "520px") |> add_spinner()
          ),
          tabPanel(
            title = "Explore the field",
            fluidRow(
              col_2(
                selectInput(ns("basemapplot"),
                            label = "Basemap",
                            choices = c("basemap", "DSM", "DTM", "CHM"))
              ),
              col_3(
                selectInput(ns("plotattribute"),
                            label = "Attribute",
                            choices = NULL)
              ),
              col_3(
                pickerpalette(id, "palplot", selected = "set2"),
              ),
              col_1(
                prettyCheckbox(
                  inputId = ns("palplotrev"),
                  label = "Reverse",
                  value = FALSE,
                  icon = icon("check"),
                  status = "success",
                  animation = "rotate"
                )
              ),
              col_3(
                sliderInput(ns("alpharesplot"),
                            label = "Fill opacity",
                            min = 0,
                            max = 1,
                            value = 0.75),
              )
            ),
            leafletOutput(ns("resultsplotmap"), height = "640px")  |> add_spinner()
          ),
          tabPanel(
            title = "Raw results",
            reactable::reactableOutput(ns("resultplottab"), height = "700px")  |> add_spinner()
          ),
          tabPanel(
            "Plant Height Profile",
            fluidRow(
              col_6(
                actionBttn(
                  inputId = ns("startprofile"),
                  label = "Start profiling",
                  style = "pill",
                  color = "success",
                  icon = icon("edit")
                )
              ),
              col_6(
                actionBttn(
                  inputId = ns("createprofile"),
                  label = "Create profile",
                  style = "pill",
                  color = "success",
                  icon = icon("edit")
                )
              )
            ),
            editModUI(ns("indexprofile"), height = "660px") |> add_spinner()
          )
        )
      } else if(input$strategy == "Build DTM using a moving window"){
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "790px",
          title = "Results",
          selected = "Home",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Home",
            fluidRow(
              col_9(
                img(src = "www/logodsm.jpg", width = "100%", height = "90%")
              ),
              col_3(
                h2("About"),
                "This module offers strategies for computing Canopy Height Models (CHM), providing valuable tools for computing canopy-related measures, such as height and volume.", br(), br(),
                h2("Disclaimer"),
                "We welcome your feedback and suggestions on the application's usefulness. Please note that we do not guarantee the correctness, reliability, or utility of the results, especially if incorrect settings are applied during the CHM estimation process.", br(), br(),
                h2("Image Reference"),
                "The image used in this module was generated using advanced AI tools to represent the digital surface model (DSM)."
              )
            )
          ),
          tabPanel(
            title = "Overview",
            fluidRow(
              valueBoxOutput(ns("vbnplots"), width = 3),
              valueBoxOutput(ns("vbnminabs"), width = 3),
              valueBoxOutput(ns("vbnmaxabs"), width = 3),
              valueBoxOutput(ns("vbnmeanq95"), width = 3),
              valueBoxOutput(ns("vbnminvol"), width = 3),
              valueBoxOutput(ns("vbnmeanvol"), width = 3),
              valueBoxOutput(ns("vbnmaxvol"), width = 3),
              valueBoxOutput(ns("vbntotvol"), width = 3),
              valueBoxOutput(ns("vbncoveredmin"), width = 3),
              valueBoxOutput(ns("vbncoveredmax"), width = 3),
              valueBoxOutput(ns("vbncovermed"), width = 3),
              valueBoxOutput(ns("vbnentropy"), width = 3),
            ),

            # Só mostra se plotquality == TRUE
            conditionalPanel(
              condition = "input.plotquality == true", ns = ns,
              fluidRow(
                valueBoxOutput(ns("vbnmingap"), width = 3),
                valueBoxOutput(ns("vbnmaxgap"), width = 3),
                valueBoxOutput(ns("vbntotalgap"), width = 3)
              )
            ),
          ),
          tabPanel(
            title = "Trait distribution",
            plotlyOutput(ns("overview"), height = "520px") |> add_spinner()
          ),
          tabPanel(
            title = "Explore the field",
            fluidRow(
              col_2(
                selectInput(ns("basemapplot"),
                            label = "Basemap",
                            choices = c("basemap", "DSM", "DTM", "CHM"))
              ),
              col_3(
                selectInput(ns("plotattribute"),
                            label = "Attribute",
                            choices = NULL)
              ),
              col_3(
                pickerpalette(id, "palplot", selected = "set2"),
              ),
              col_1(
                prettyCheckbox(
                  inputId = ns("palplotrev"),
                  label = "Reverse",
                  value = FALSE,
                  icon = icon("check"),
                  status = "success",
                  animation = "rotate"
                )
              ),
              col_3(
                sliderInput(ns("alpharesplot"),
                            label = "Fill opacity",
                            min = 0,
                            max = 1,
                            value = 0.75),
              )
            ),
            leafletOutput(ns("resultsplotmap"), height = "640px")  |> add_spinner()
          ),
          tabPanel(
            title = "Sampled Points",
            leafletOutput(ns("sampledsf"), height = "640px")  |> add_spinner()
          ),
          tabPanel(
            title = "Digital Terrain Model",
            plotOutput(ns("dtmplot"), height = "640px")  |> add_spinner()
          ),
          tabPanel(
            title = "Raw results",
            reactable::reactableOutput(ns("resultplottab"), height = "700px")  |> add_spinner()
          ),
          tabPanel(
            "Plant Height Profile",
            fluidRow(
              col_6(
                actionBttn(
                  inputId = ns("startprofile"),
                  label = "Start profiling",
                  style = "pill",
                  color = "success",
                  icon = icon("edit")
                )
              ),
              col_6(
                actionBttn(
                  inputId = ns("createprofile"),
                  label = "Create profile",
                  style = "pill",
                  color = "success",
                  icon = icon("edit")
                )
              )
            ),
            editModUI(ns("indexprofile"), height = "660px") |> add_spinner()
          )
        )
      } else {
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "790px",
          title = "Results",
          selected = "Home",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Home",
            fluidRow(
              col_9(
                img(src = "www/logodsm.jpg", width = "100%", height = "90%")
              ),
              col_3(
                h2("About"),
                "This module offers strategies for computing Canopy Height Models (CHM), providing valuable tools for computing canopy-related measures, such as height and volume.", br(), br(),
                h2("Disclaimer"),
                "We welcome your feedback and suggestions on the application's usefulness. Please note that we do not guarantee the correctness, reliability, or utility of the results, especially if incorrect settings are applied during the CHM estimation process.", br(), br(),
                h2("Image Reference"),
                "The image used in this module was generated using advanced AI tools to represent the digital surface model (DSM)."
              )
            )
          ),
          tabPanel(
            title = "Interpolation",
            actionBttn(
              inputId = ns("interpolate"),
              label = "Start sampling",
              style = "pill",
              color = "success",
              icon = icon("edit")
            ),
            editModUI(ns("pointpicksample"), height = "740px") |> add_spinner()

          ),
          tabPanel(
            title = "Overview",
            fluidRow(
              valueBoxOutput(ns("vbnplots"), width = 3),
              valueBoxOutput(ns("vbnminabs"), width = 3),
              valueBoxOutput(ns("vbnmaxabs"), width = 3),
              valueBoxOutput(ns("vbnmeanq95"), width = 3),
              valueBoxOutput(ns("vbnminvol"), width = 3),
              valueBoxOutput(ns("vbnmeanvol"), width = 3),
              valueBoxOutput(ns("vbnmaxvol"), width = 3),
              valueBoxOutput(ns("vbntotvol"), width = 3),
              valueBoxOutput(ns("vbncoveredmin"), width = 3),
              valueBoxOutput(ns("vbncoveredmax"), width = 3),
              valueBoxOutput(ns("vbncovermed"), width = 3),
              valueBoxOutput(ns("vbnentropy"), width = 3),
            ),

            # Só mostra se plotquality == TRUE
            conditionalPanel(
              condition = "input.plotquality == true", ns = ns,
              fluidRow(
                valueBoxOutput(ns("vbnmingap"), width = 3),
                valueBoxOutput(ns("vbnmaxgap"), width = 3),
                valueBoxOutput(ns("vbntotalgap"), width = 3)
              )
            ),
          ),
          tabPanel(
            title = "Trait distribution",
            plotlyOutput(ns("overview"), height = "520px") |> add_spinner()
          ),
          tabPanel(
            title = "Explore the field",
            fluidRow(
              col_2(
                selectInput(ns("basemapplot"),
                            label = "Basemap",
                            choices = c("basemap", "DSM", "DTM", "CHM"))
              ),
              col_3(
                selectInput(ns("plotattribute"),
                            label = "Attribute",
                            choices = NULL)
              ),
              col_3(
                pickerpalette(id, "palplot", selected = "set2"),
              ),
              col_1(
                prettyCheckbox(
                  inputId = ns("palplotrev"),
                  label = "Reverse",
                  value = FALSE,
                  icon = icon("check"),
                  status = "success",
                  animation = "rotate"
                )
              ),
              col_3(
                sliderInput(ns("alpharesplot"),
                            label = "Fill opacity",
                            min = 0,
                            max = 1,
                            value = 0.75),
              )
            ),
            leafletOutput(ns("resultsplotmap"), height = "640px")  |> add_spinner()
          ),
          tabPanel(
            title = "Raw results",
            reactable::reactableOutput(ns("resultplottab"), height = "700px")  |> add_spinner()
          ),
          tabPanel(
            "Plant Height Profile",
            fluidRow(
              col_6(
                actionBttn(
                  inputId = ns("startprofile"),
                  label = "Start profiling",
                  style = "pill",
                  color = "success",
                  icon = icon("edit")
                )
              ),
              col_6(
                actionBttn(
                  inputId = ns("createprofile"),
                  label = "Create profile",
                  style = "pill",
                  color = "success",
                  icon = icon("edit")
                )
              )
            ),
            editModUI(ns("indexprofile"), height = "660px") |> add_spinner()
          )
        )
      }
    })
    # if example mosaic is used
    # mosaic provided by Arthur Bernardeli https://www.linkedin.com/in/arthur-bernardeli-5a1a0b5a/
    observeEvent(input$inputdsmtype, {
      if(input$inputdsmtype == "Example raster"){
        pathdsm <- file.path(system.file(package = "plimanshiny"), "app/www/soy_dsm.tif")
        pathdtm <- file.path(system.file(package = "plimanshiny"), "app/www/soy_dtm.tif")
        pathshp <- file.path(system.file(package = "plimanshiny"), "app/www/soy_shape.rds")
        mosaic_data[["dsm"]] <- create_reactval("dsm", mosaic_input(pathdsm, info = FALSE))
        mosaic_data[["dtm"]] <- create_reactval("dtm", mosaic_input(pathdtm, info = FALSE))
        shapefile[["chm_shape"]] <- create_reactval("chm_shape", shapefile_input(pathshp, info = FALSE))
        updatePickerInput(session, "dsm", choices = c(setdiff(names(mosaic_data), "mosaic")), selected = "dsm")
        updatePickerInput(session, "dtm", choices = c(setdiff(names(mosaic_data), "mosaic")), selected = "dtm")
        updatePickerInput(session, "shapefile", choices = c(setdiff(names(shapefile), c("shapefile", "shapefileplot"))), selected = "chm_shape")
      }
    })


    # update selec input
    observe({
      updatePickerInput(session, "dsm", choices = c(setdiff(names(mosaic_data), "mosaic")))
    })
    observe({
      updatePickerInput(session, "dtm", choices = c(setdiff(names(mosaic_data), "mosaic")))
    })
    observe({
      updatePickerInput(session, "maskfile", choices = c("none", setdiff(names(mosaic_data), "mosaic")),
                        selected = "none")
    })
    observe({
      updatePickerInput(session, "shapefile",
                        choices = c(setdiff(names(shapefile), c("shapefile", "shapefileplot"))))
    })

    bmchm <- reactiveValues(map = NULL)
    chmreact <- reactiveValues(rast = NULL)
    dfres <- reactiveValues(df = NULL)
    sampledpoints <- reactiveValues(sf = NULL)

    output$computearea <- renderUI({
      req(input$shapefile)
      if (!is.null(shapefile[[input$shapefile]]$data) & !is.null(mosaic_data[[input$dsm]]$data)) {
        tagList(
          textInput(
            ns("savedfas"),
            label = "Save Results as...",
            value = "canopy_height_model",
          ),
          actionBttn(
            ns("computeph"),
            label = "Compute Canopy Height Model!",
            icon = icon("check")
          )
        )

      }
    })



    observeEvent(input$computeph, {
      if(input$strategy == "I have DSM and DTM"){
        req(input$dsm, input$dtm)
        dsm <- mosaic_data[[input$dsm]]$data
        dtm <- mosaic_data[[input$dtm]]$data
        ch1 <- !inherits(dsm,"SpatRaster") || !terra::nlyr(dsm) == 1 || terra::is.bool(dsm) || is.list(dsm)
        ch2 <- !inherits(dtm,"SpatRaster") || !terra::nlyr(dtm) == 1 || terra::is.bool(dtm) || is.list(dtm)
        if(input$croptoext){
          req(shapefile[[input$shapefile]]$data)
          dsm <- terra::crop(dsm, shapefile[[input$shapefile]]$data |> terra::vect() |> terra::buffer(5))
          dtm <- terra::crop(dtm, shapefile[[input$shapefile]]$data |> terra::vect() |> terra::buffer(5))
        }

        if (ch1 | ch2) {
          sendSweetAlert(
            session = session,
            title = "Invalid file format",
            text = paste("DSM and DTM must be an SpatRaster object with one layer."),
            type = "error"
          )
          return()
        } else{

          waiter_show(
            html = tagList(
              spin_google(),
              h2("Building the Canopy Height Model. Please, wait.")
            ),
            color = "#228B227F"
          )
          if(input$masktype == "none" | input$masktype == "threshold"){
            chmres <- mosaic_chm(dsm, dtm,
                                 verbose = FALSE)
          } else if(input$masktype == "raster"){
            chmres <- mosaic_chm(dsm = dsm,
                                 dtm = dtm,
                                 mask = mosaic_data[[input$maskfile]]$data,
                                 mask_soil = input$masksoil,
                                 verbose = FALSE)
          }
          chmreact$rast <- chmres$chm
        }
      }

      if(input$strategy == "Build DTM using sampling points"){
        req(input$dsm, input$shapefile)
        dsm <- mosaic_data[[input$dsm]]$data
        ch1 <- !inherits(dsm,"SpatRaster") || !terra::nlyr(dsm) == 1 || terra::is.bool(dsm) || is.list(dsm)
        if (ch1) {
          sendSweetAlert(
            session = session,
            title = "Invalid file format",
            text = paste("DSM and DTM must be an SpatRaster object with one layer."),
            type = "error"
          )
          return()
        } else{
          dsm <- terra::crop(dsm, shapefile[[input$shapefile]]$data |> terra::vect() |> terra::buffer(5))
          mapsampl <- mosaic_view(dsm,
                                  alpha = 1,
                                  show = "index")
          cpoints <- callModule(editMod, "pointpicksample", mapsampl@map , editor = "leafpm")
          observeEvent(input$interpolate, {
            observeEvent(cpoints()$finished, {
              if(!is.null(cpoints()$edited)){
                cpo <- cpoints()$edited |> sf::st_transform(sf::st_crs(dsm))
              } else{
                cpo <- cpoints()$finished |> sf::st_transform(sf::st_crs(dsm))
              }
              req(cpo)

              waiter_show(
                html = tagList(
                  spin_google(),
                  h2("Building the Canopy Height Model. Please, wait.")
                ),
                color = "#228B227F"
              )
              interpmethod <- switch(input$interpmethod,
                                     "Thin Plate Spline" = "Tps",
                                     "Kriging" = "Kriging")
              if(input$masktype == "none" | input$masktype == "threshold"){

                chmres <- mosaic_chm(dsm,
                                     points = cpo,
                                     interpolation = interpmethod,
                                     ground_quantile = input$ground_quantile,
                                     verbose = FALSE)
              } else if (input$masktype == "raster"){
                chmres <- mosaic_chm(dsm = dsm,
                                     points = cpo,
                                     mask = mosaic_data[[input$maskfile]]$data,
                                     mask_soil = input$masksoil,
                                     interpolation = interpmethod,
                                     ground_quantile = input$ground_quantile,
                                     verbose = FALSE)
              }
              chmreact$rast <- chmres$chm
            })
          })
        }
      }

      if(input$strategy == "Build DTM using a moving window"){
        req(input$dsm, input$windowsize, input$shapefile)
        dsm <- mosaic_data[[input$dsm]]$data
        ch1 <- !inherits(dsm,"SpatRaster") || !terra::nlyr(dsm) == 1 || terra::is.bool(dsm) || is.list(dsm)
        if (ch1) {
          sendSweetAlert(
            session = session,
            title = "Invalid file format",
            text = paste("DSM and DTM must be an SpatRaster object with one layer."),
            type = "error"
          )
          return()
        } else{
          waiter_show(
            html = tagList(
              spin_google(),
              h2("Building the Canopy Height Model. Please, wait.")
            ),
            color = "#228B227F"
          )
          interpmethod <- switch(input$interpmethod,
                                 "Thin Plate Spline" = "Tps",
                                 "Kriging" = "Kriging")
          dsm <- terra::crop(dsm, shapefile[[input$shapefile]]$data |> terra::vect() |> terra::buffer(5))
          if(input$masktype == "none" | input$masktype == "threshold"){
            chmres <- mosaic_chm(dsm,
                                 window_size = input$windowsize |> chrv2numv(),
                                 interpolation = interpmethod,
                                 ground_quantile = input$ground_quantile,
                                 verbose = FALSE)
          } else if(input$masktype == "raster"){
            chmres <- mosaic_chm(dsm = dsm,
                                 window_size = input$windowsize |> chrv2numv(),
                                 mask = mosaic_data[[input$maskfile]]$data,
                                 mask_soil = input$masksoil,
                                 interpolation = interpmethod,
                                 ground_quantile = input$ground_quantile,
                                 verbose = FALSE)
          }
          chmreact$rast <- chmres$chm
          sampledpoints$sf <- chmres$sampling_points
          output$dtmplot <- renderPlot({
            terra::plot(chmres$chm[[1]], col = grDevices::colorRampPalette(c("darkred", "yellow", "darkgreen"))(100))
          })

        }
      }


      req(chmreact$rast)
      req(input$basemapplot)

      if(input$masktype == "threshold"){
        dftmp <- mosaic_chm_extract(chmres, shapefile[[input$shapefile]]$data, chm_threshold = input$dsmthresh)
      } else{
        dftmp <- mosaic_chm_extract(chmres, shapefile[[input$shapefile]]$data, chm_threshold = 0.1)
      }
      print
      if(input$plotquality){
        pq <- mosaic_chm_quality(chmres, shapefile[[input$shapefile]]$data,
                                 chm_quantile = input$chm_quantile,
                                 plot_quality = input$qualitytype,
                                 chmvals = dftmp)
        dftmp <-
          dplyr::left_join(
            dftmp,
            pq |> dplyr::select(-c(cv, entropy, coverage)) |> sf::st_drop_geometry(),
            by = dplyr::join_by(unique_id, block, plot_id, row, column)
          ) |>
          dplyr::mutate(covered_area = plot_area * coverage)
      }
      dfres$df <- dftmp
      updateSelectInput(session, "plotattribute",
                        choices = colnames(dftmp),
                        selected = "q95")

      # store the results in dataset module
      dfs[[input$savedfas]] <- create_reactval(input$savedfas, dftmp |> sf::st_drop_geometry())
      shapefile[[input$savedfas]] <- create_reactval(input$savedfas, dftmp)
      mosaic_data[[input$savedfas]] <- create_reactval(input$savedfas, chmreact$rast)

      output$resultplottab <- renderReactable({
        dftmp |>
          sf::st_drop_geometry() |>
          roundcols(digits = 3) |>
          render_reactable()

      })
      waiter_hide()
      sendSweetAlert(
        session = session,
        title = "Canopy Height Model computed.",
        text = "The Canopy Height Model computed has been computed and the results are now available for download in the 'Datasets > Input' menu.",
        type = "success"
      )

    })

    # plot the results
    observe({
      req(chmreact$rast)
      if(input$basemapplot == "basemap"){
        bmchm$map <- basemap$map
      } else if(input$basemapplot == "DSM"){
        req(chmreact$rast)
        bmchm$map <- mosaic_view(mosaic_data[[input$dsm]]$data,
                                 color_regions = return_colors("PlantSoil", reverse = TRUE),
                                 alpha = 1,
                                 show = "index")
      } else if(input$basemapplot == "DTM"){
        req(chmreact$rast)
        bmchm$map <- mosaic_view(chmreact$rast[[1]],
                                 color_regions = return_colors("PlantSoil", reverse = TRUE),
                                 alpha = 1,
                                 show = "index")
      } else if(input$basemapplot == "CHM"){
        req(chmreact$rast)
        bmchm$map <- mosaic_view(chmreact$rast[[2]],
                                 color_regions = return_colors("PlantSoil", reverse = TRUE),
                                 alpha = 1,
                                 show = "index")
      }

    })

    output$sampledsf <- renderLeaflet({
      if(input$strategy == "Build DTM using a moving window"){
        req(sampledpoints$sf)
        (bmchm$map + shapefile_view(sampledpoints$sf, attribute = "dtm"))@map
      }
    })

    output$resultsplotmap <- renderLeaflet({
      req(dfres$df)
      req(input$plotattribute)
      (bmchm$map + shapefile_view(dfres$df,
                                  attribute = input$plotattribute,
                                  color_regions = return_colors(input$palplot, reverse = input$palplotrev),
                                  alpha.regions = input$alpharesplot))@map
    })


    output$vbnmingap <- renderValueBox({
      req(input$plotquality)
      req(dfres$df)
      valueBox(
        value = tags$p(nrow(dfres$df |> dplyr::filter(gaps > 0)), style = "font-size: 300%;"),
        subtitle = "Number of plots with gaps",
        color = "success",
        icon = icon("arrows-left-right-to-line")
      )
    })
    output$vbnmaxgap <- renderValueBox({
      req(input$plotquality)
      req(dfres$df)
      valueBox(
        value = tags$p(round(max(dfres$df$gaps, na.rm = TRUE), 2), style = "font-size: 300%;"),
        subtitle = "Maximum number of gaps",
        color = "success",
        icon = icon("arrows-left-right-to-line")
      )
    })
    output$vbntotalgap <- renderValueBox({
      req(input$plotquality)
      req(dfres$df)
      valueBox(
        value = tags$p(round(sum(dfres$df$gaps, na.rm = TRUE), 2), style = "font-size: 300%;"),
        subtitle = "Total number of gaps",
        color = "success",
        icon = icon("arrows-left-right-to-line")
      )
    })
    output$vbnplots <- renderValueBox({
      req(dfres$df)
      valueBox(
        value = tags$p(nrow(dfres$df), style = "font-size: 300%;"),
        subtitle = "Number of plots",
        color = "success",
        icon = icon("table-cells")
      )
    })
    output$vbnminabs <- renderValueBox({
      req(dfres$df)
      valueBox(
        value = tags$p(round(min(dfres$df$q95, na.rm = TRUE), 2), style = "font-size: 300%;"),
        subtitle = "Minimum height q95",
        color = "success",
        icon = icon("table-cells")
      )
    })
    output$vbnmaxabs <- renderValueBox({
      req(dfres$df)
      valueBox(
        value = tags$p(round(max(dfres$df$q95, na.rm = TRUE), 2), style = "font-size: 300%;"),
        subtitle = "Maximum height q95",
        color = "success",
        icon = icon("table-cells")
      )
    })
    output$vbnmeanq95 <- renderValueBox({
      req(dfres$df)
      valueBox(
        value = tags$p(round(mean(dfres$df$q95, na.rm = TRUE), 2), style = "font-size: 300%;"),
        subtitle = "Mean height q95",
        color = "success",
        icon = icon("ruler-vertical")
      )
    })

    output$vbnminvol <- renderValueBox({
      req(dfres$df)
      valueBox(
        value = tags$p(round(min(dfres$df$volume, na.rm = TRUE), 2), style = "font-size: 300%;"),
        subtitle = "Minimum volume",
        color = "success",
        icon = icon("box-open")
      )
    })
    output$vbnmaxvol <- renderValueBox({
      req(dfres$df)
      valueBox(
        value = tags$p(round(max(dfres$df$volume, na.rm = TRUE), 2), style = "font-size: 300%;"),
        subtitle = "Maximum volume",
        color = "success",
        icon = icon("box-open")
      )
    })
    output$vbnmeanvol <- renderValueBox({
      req(dfres$df)
      valueBox(
        value = tags$p(round(mean(dfres$df$volume, na.rm = TRUE), 2), style = "font-size: 300%;"),
        subtitle = "Mean volume",
        color = "success",
        icon = icon("box-open")
      )
    })
    output$vbntotvol <- renderValueBox({
      req(dfres$df)
      valueBox(
        value = tags$p(round(sum(dfres$df$volume), 2), style = "font-size: 300%;"),
        subtitle = "Total volume",
        color = "success",
        icon = icon("box-open")
      )
    })
    output$vbncoveredmin <- renderValueBox({
      req(dfres$df)
      valueBox(
        value = tags$p(round(min(dfres$df$coverage), 2), style = "font-size: 300%;"),
        subtitle = "Minimum coverage",
        color = "success",
        icon = icon("box-open")
      )
    })
    output$vbncoveredmax <- renderValueBox({
      req(dfres$df)
      valueBox(
        value = tags$p(round(max(dfres$df$coverage), 2), style = "font-size: 300%;"),
        subtitle = "Maximum coverage",
        color = "success",
        icon = icon("box-open")
      )
    })
    output$vbncovermed <- renderValueBox({
      req(dfres$df)
      valueBox(
        value = tags$p(round(mean(dfres$df$coverage), 2), style = "font-size: 300%;"),
        subtitle = "Average coverage",
        color = "success",
        icon = icon("percent")
      )
    })
    output$vbnentropy <- renderValueBox({
      req(dfres$df)
      valueBox(
        value = tags$p(round(mean(dfres$df$entropy), 2), style = "font-size: 300%;"),
        subtitle = "Average entropy",
        color = "success",
        icon = icon("table-cells")
      )
    })



    output$overview <- renderPlotly({
      req(dfres$df)
      dfhist <-
        dfres$df |>
        sf::st_drop_geometry() |>
        dplyr::select(q95, volume, coverage, entropy) |>
        tidyr::pivot_longer(dplyr::everything())
      p <-
        ggplot(dfhist, aes(x = value)) +
        geom_histogram(position = "identity",
                       fill = "forestgreen") +
        facet_wrap(~name, scales = "free") +
        labs(x = "Observed value",
             y = "Number of plots") +
        theme_bw(base_size = 18) +
        theme(panel.grid.minor = element_blank(),
              legend.position = "bottom")
      plotly::ggplotly(p)
    })


    # Plant Height Profile
    # Index profile
    observeEvent(input$startprofile, {
      req(chmreact$rast)
      drawn <- reactiveValues()
      cpoints <- callModule(editMod, "indexprofile",
                            leafmap = bmchm$map@map,
                            editor = "leafpm")
      observeEvent(c(cpoints()$finished, cpoints()$edited), {
        if(!is.null(cpoints()$finished)){
          drawn$finished <- cpoints()$finished |> dplyr::slice_tail(n = 1)
        }
        if(!is.null(cpoints()$edited) & !is.null(cpoints()$finished)){
          idedit <- cpoints()$edited |> dplyr::slice_tail(n = 1) |> dplyr::pull(edit_id)
          drawnedit <- cpoints()$finished |> dplyr::slice_tail(n = 1) |> dplyr::pull(edit_id)
          if(idedit == drawnedit){
            drawn$finished <- cpoints()$edited |> dplyr::slice_tail(n = 1)
          }
        }
        observeEvent(input$createprofile, {
          on.exit(layout(1))
          if(!is.null(drawn$finished)){
            output$plotinfop <- renderPlot({
              polygons <- drawn$finished$geometry
              polygons_spv <-
                sf::st_transform(polygons, crs = sf::st_crs(chmreact$rast[[input$whatprofile]]))


              if(inherits(polygons, "sfc_LINESTRING")){
                coords <-
                  polygons_spv |>
                  sf::st_coordinates()
                coordsext <- terra::vect(coords[, 1:2], "lines")
                exts <-
                  terra::vect(sf::st_transform(polygons, crs = sf::st_crs(chmreact$rast[[input$whatprofile]]))) |>
                  terra::buffer(input$buffer) |>
                  terra::ext()
                indexccr <- terra::crop(chmreact$rast[[input$whatprofile]], exts)
                polygons_ext <-  terra::vect(polygons_spv)
                vals <- terra::extractAlong(indexccr, coordsext, xy = TRUE)

                coordsdist <- as.matrix(polygons_spv |> sf::st_coordinates())
                n <- nrow(coordsdist)
                distances <- NULL
                for (j in 1:(n - 1)) {
                  x1 <- coordsdist[j, 1]
                  y1 <- coordsdist[j, 2]
                  x2 <- coordsdist[j + 1, 1]
                  y2 <- coordsdist[j + 1, 2]
                  distance <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
                  distances[j] <- distance
                }
                # distances
                dists <- cumsum(distances)
                dist <- max(dists)
                layout(
                  matrix(c(1, 2), nrow = 2, byrow = TRUE),
                  heights = c(3, 3)
                )
                if(input$whatplot == "raster"){
                  terra::plot(indexccr,
                              axes = FALSE,
                              maxcell=5000000,
                              mar = c(2, 2, 2, 2),
                              col = return_colors(input$plaindex, reverse = input$revindex, n = 100),
                              smooth=TRUE)
                } else{
                  mosaic_plot_rgb(terra::crop(mosaic_data$mosaic$data, exts))
                }
                lines(coords,
                      col = "red",
                      lwd = 3)

                data_long <-
                  data.frame(x = seq(0, dist, length.out = nrow(vals))) |>
                  dplyr::bind_cols(vals |> dplyr::select(dplyr::all_of(input$whatprofile))) |>
                  dplyr::filter(!is.na(!!dplyr::sym(input$whatprofile)))
                if(input$smooth){
                  data_long <-
                    data_long |>
                    dplyr::mutate(dplyr::across(2, ~ {
                      fit <- smooth.spline(data_long$x, .x, spar = input$spar)
                      predict(fit, data_long$x)$y
                    }))
                }

                matplot(data_long[, 1],
                        data_long[, -1],
                        type = "l",
                        lty = 1,
                        col = "red",
                        ylab = input$whatprofile,
                        xlab = "Distance",
                        cex = 1.5,
                        lwd = 1.5,
                        xlim = c(0, dist))
                legend("topright",
                       legend = input$whatprofile,
                       col = "red",
                       lty = 1)


                # Download handler to generate the CSV file
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste("data-", Sys.Date(), ".csv", sep="")
                  },
                  content = function(file) {
                    # Generate a sample data frame
                    data <- data_long
                    # Write the data frame to a CSV file
                    write.csv(data, file, row.names = FALSE)
                  }
                )

              }

            })


            showModal(
              modalDialog(
                title = "Canopy Height Profile",
                fluidRow(
                  col_3(
                    pickerpalette(id, "plaindex", selected = "PlantSoil"),
                  ),
                  col_3(
                    prettyRadioButtons(
                      inputId = ns("whatprofile"),
                      label = "Create profile for...",
                      choices = c("dtm", "height", "volume"),
                      selected = "height",
                      icon = icon("check"),
                      status = "success",
                      animation = "rotate"
                    )
                  ),
                  col_3(
                    prettyRadioButtons(
                      inputId = ns("whatplot"),
                      label = "Plot to show...",
                      choices = c("basemap", "raster"),
                      selected = "raster",
                      icon = icon("check"),
                      status = "success",
                      animation = "rotate"
                    )
                  ),
                  col_2(
                    prettyCheckbox(
                      inputId = ns("revindex"),
                      label = "Reverse",
                      value = TRUE,
                      icon = icon("check"),
                      status = "success",
                      animation = "rotate"
                    ),
                    prettyCheckbox(
                      inputId = ns("smooth"),
                      label = "Smooth",
                      value = TRUE,
                      icon = icon("check"),
                      status = "success",
                      animation = "rotate"
                    ),
                    conditionalPanel(
                      condition = "input.smooth == true", ns = ns,
                      sliderInput(ns("spar"),
                                  label = "Spline parameter",
                                  min = 0,
                                  max = 1,
                                  value = 0.3)
                    )
                  ),
                  col_1(
                    numericInput(ns("buffer"),
                                 label = "Buffer",
                                 value = 2)
                  )
                ),
                fluidRow(
                  plotOutput(ns("plotinfop"), height = "600px")
                ),
                downloadButton(ns("downloadData"), "Download profile"),
                footer = NULL,
                easyClose = TRUE,
                size = "xl"
              )
            )
          }
        })
      })
    })

  })
}

## To be copied in the UI
# mod_phanalyze_ui("phanalyze_1")

## To be copied in the server
# mod_phanalyze_server("phanalyze_1")
