#' mosaic_prepare UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom utils read.csv
mod_mosaic_prepare_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    col_3(
      bs4TabCard(
        id = "tabs",
        width = 12,
        status = "success",
        title = "Settings",
        selected = "Input",
        solidHeader = FALSE,
        type = "tabs",
        tabPanel(
          title = "Input",
          width = 12,
          status = "success",
          fluidRow(
            col_4(
              conditionalPanel(
                condition = "input['config_1-introjs'] === true",
                actionButton(
                  inputId = ns("guidemosaic"),
                  label = tagList(
                    icon = icon("question-circle", verify_fa = FALSE), "Guide"
                  ),
                  style = "color: white ; background-color: #dd4b39",
                  class = "btn-danger"
                )
              )
            ),
            col_3(
              dropdown(
                tags$h3("Settings"),
                fluidRow(
                  col_3(
                    selectInput(
                      inputId = ns("r_band"),
                      label = "R",
                      choices = 1:5,
                      selected = 1
                    )
                  ),
                  col_3(
                    selectInput(
                      inputId = ns("g_band"),
                      label = "G",
                      choices = 1:5,
                      selected = 2,
                    )
                  ),
                  col_3(
                    selectInput(
                      inputId = ns("b_band"),
                      label = "B",
                      choices = 1:5,
                      selected = 3,
                    )
                  ),
                  col_3(
                    selectInput(
                      inputId = ns("re_band"),
                      label = "RE",
                      choices = NA
                    )
                  )
                ),
                fluidRow(
                  col_4(
                    selectInput(
                      inputId = ns("nir_band"),
                      label = "NIR",
                      choices = NA
                    )
                  ),
                  col_4(
                    selectInput(
                      inputId = ns("swir_band"),
                      label = "SWIR",
                      choices = NA
                    )
                  ),
                  col_4(
                    selectInput(
                      inputId = ns("tir_band"),
                      label = "TIR",
                      choices = NA
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.intmap == true", ns = ns,
                  sliderInput(ns("quantileplot"),
                              label = "Quantiles",
                              min = 0,
                              max = 1,
                              value = c(0, 1),
                              step = 0.001),
                  numericInput(ns("maxpixels"),
                               label = "Maximum Pixels",
                               value = 1e6)
                ),
                conditionalPanel(
                  condition = "input.intmap == false", ns = ns,
                  prettyCheckbox(
                    inputId = ns("useminmax"),
                    label = "Set range of values to plot ",
                    value = FALSE,
                    icon = icon("check"),
                    status = "success",
                    animation = "rotate"
                  ),
                  conditionalPanel(
                    condition = "input.useminmax == true", ns = ns,
                    mod_histo_slider_ui(ns("historaster"),
                                        data = rnorm(100),
                                        width = "100%",
                                        height = "250px",
                                        n_bins = 75),
                    br(),br()
                  )
                ),

                actionBttn(
                  ns("donebands"),
                  label = "Done",
                  no_outline = FALSE,
                  icon = icon("check"),
                  color = "success"
                ),
                circle = FALSE,
                status = "success",
                style = "unite",
                width = "550px",
                icon = icon("gear"),
                animate = animateOptions(enter = "fadeInLeft", exit = "fadeOutRight", duration = 1),
                tooltip = tooltipOptions(title = "Configure the bands")
              ) |>
                add_help(step = 1,
                         intro = "Here, the bands are configured. You can configure up
                       to 7 layers and also define the maximum number of pixels to be rendered in the basemap.")
            ),
            col_5(
              actionButton(
                inputId = ns("mosaicinfomosaic"),
                label = tagList(
                  icon = icon("circle-info", verify_fa = FALSE), "Mosaic Info"
                ),
                status = "info"
              ) |> add_help(step = 2,
                            intro = "Here, you can see the information about the mosaic.")
            )
          ),
          hl(),
          awesomeRadio(
            inputId = ns("inputrastertype"),
            label = "Entry method",
            choices = c("load rasters", "example raster"),
            selected = "load rasters",
            status = "success",
            inline = TRUE
          ),
          conditionalPanel(
            condition = "input.inputrastertype == 'load rasters'", ns = ns,
            add_help(
              shinyFilesButton(id=ns("filemosaic"),
                               label="Raster file(s)",
                               title="Raster file(s)",
                               buttonType = "primary",
                               multiple = TRUE,
                               class = NULL,
                               icon = icon("magnifying-glass"),
                               style = NULL),
              fluidRow(
                textInput(
                  ns("filemosaicpath"),
                  label = "Choosen file(s)",
                  value = "",
                  width = "100%"
                )
              ),
              step = 3,
              intro = "Search by rasters files to be imported."),
            conditionalPanel(
              condition = "input.filemosaicpath != ''", ns = ns,
              fluidRow(
                actionBttn(ns("importmosaic"),
                           label = "Import the choosen file(s)",
                           no_outline = FALSE,
                           icon = icon("file-import"),
                           style = "material-flat",
                           color = "primary")
              )
            )
          ),
          hl(),
          selectInput(ns("mosaictoanalyze"),
                      label = "Active Mosaic",
                      choices = NULL) |>
            add_help(step = 4,
                     intro = "Here you can define which raster file is active.
                   It will define the basemap for further modules"),
          add_help(
            fluidRow(
              col_6(
                awesomeRadio(
                  inputId = ns("showmosaic"),
                  label = "Show",
                  choices = c("rgb", "bands", "hist"),
                  selected = "rgb",
                  inline = FALSE
                )
              ),
              col_6(
                selectInput(
                  ns("stretch"),
                  label = "Stretch",
                  choices = c("none", "lin", "hist")
                )
              )
            ),
            step = 5,
            intro = "Choose how to display the mosaic. You can select the display
            mode (RGB, individual bands, or histogram) and apply optional stretch
            techniques to enhance the image visualization."
          ),
          prettyCheckbox(
            inputId = ns("showlegend"),
            label = "Show plot legend?",
            value = FALSE,
            icon = icon("check"),
            status = "success",
            animation = "rotate"
          ) |>
            add_help(step = 6,
                     intro = "Enable this option to display the legend.
                     To crop the raster accurately, disable this option to
                     ensure the canvas area matches the raster's extents
                     without additional padding."),
          hl(),
          prettyCheckbox(
            inputId = ns("intmap"),
            label = "Create leaflet map (needed for 'Analyze' step)",
            value = FALSE,
            icon = icon("check"),
            status = "success",
            animation = "rotate"
          ) |>
            add_help(step = 7,
                     intro = "Enable this option to create an interactive basemap.
                   Depending on the size of the mosaic and the `maxpixel` value
                   set in the configuration, this process might take a while."),

          conditionalPanel(
            condition = "input.intmap == true", ns = ns,
            selectInput(ns("howtoplot"),
                        label = "How to plot",
                        choices = NULL)
          ),
        ),
        tabPanel(
          title = "Download",
          mod_download_mosaic_ui(ns("downloadmosaic")) |>
            add_help(step = 8,
                     intro = "Download the active mosaic.")
        )
      )
    ),
    col_9(
      conditionalPanel(
        condition = "input.showmosaic == 'rgb' & input.intmap == false", ns = ns,
        plimanshiny_viewer_ui(ns("mosaic_viewer"))
      ),
      conditionalPanel(
        condition = "(input.showmosaic == 'bands' | input.showmosaic == 'hist') & input.intmap == false", ns = ns,
        bs4Card(
          width = 12,
          height = "760px",
          title = "Mosaic band view",
          color = "success",
          status = "success",
          maximizable = TRUE,
          plotOutput(ns("mosaic_plot"), height = "740px") |> add_spinner()
        )
      ),
      conditionalPanel(
        condition = "input.intmap == true", ns = ns,
        bs4Card(
          width = 12,
          height = "760px",
          title = "Leaflet view",
          color = "success",
          status = "success",
          maximizable = TRUE,
          leafletOutput(ns("mosaic_mapview"), height = "740px") |> add_spinner()
        )
      )
    )
  )
}

#' mosaic_prepare Server Functions
#'
#' @noRd
mod_mosaic_prepare_server <- function(id, mosaic_data, r, g, b, re, nir, swir, tir, basemap, pathmosaic, quantiles, maxpixel, activemosaic, zlim, settings) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$guidemosaic,
                 if(read_pars()$introjs){
                   rintrojs::introjs(session,
                                     options = list("nextLabel"="Next",
                                                    "prevLabel"="Previous",
                                                    "skipLabel"="Skip"),
                                     events = list("oncomplete"=I('alert("Hope it helped!")')))
                 }
    )
    slider_range <- reactiveVal()

    # GUIA

    observeEvent(input$donebands, {
      # Update reactiveValues for color bands
      r$r <- input$r_band |> as.numeric()
      g$g <- input$g_band |> as.numeric()
      b$b <- input$b_band |> as.numeric()
      re$re <- input$re_band |> as.numeric()
      nir$nir <- input$nir_band |> as.numeric()
      swir$swir <- input$swir_band |> as.numeric()
      tir$tir <- input$tir_band |> as.numeric()
      quantiles$q <- input$quantileplot
      maxpixel$mp <- input$maxpixels
      if(!input$useminmax){
        zlim$zlim <- NULL
      }
      showNotification(
        ui = "Configuring the layers and re-building the basemap. Please, wait!",
        type = "message",
        duration = NULL,   # Infinite duration until manually removed
        id = "rebuilding"
      )
    })


    input_file_selected <- reactiveValues(paths = NULL)
    observe({
      shinyFileChoose(input, "filemosaic",
                      root = getVolumes()(),
                      filetypes = c('tif', 'jp2', 'tiff', 'jpeg', "dat", "jpg"),
                      session = session)
      if(!is.null(input$filemosaic)){
        input_file_selected$paths <- parseFilePaths(getVolumes()(), input$filemosaic)
        if(length(input_file_selected$paths$datapath) != 0){
          updateTextInput(session, "filemosaicpath", value = paste0(input_file_selected$paths$datapath, collapse = ", "))
        }
      }
    })
    observeEvent(input$importmosaic, {
      if(length(input_file_selected$paths$datapath) != 0){
        new_mosaic_name <- sapply(input_file_selected$paths$datapath, file_name)
        pathmosaic$path <- input_file_selected$paths$datapath
        # Check if the mosaic already exists in mosaic_data
        if (any(new_mosaic_name %in% names(mosaic_data))) {
          # If it exists, update the existing reactiveValues
          moname <- new_mosaic_name[new_mosaic_name %in% names(mosaic_data)]
          ask_confirmation(
            inputId = "confirmmosaicname",
            type = "warning",
            title = "Mosaic already imported",
            text = paste0("The object '", paste0(moname, collapse = ", "), "' is already available in the list of imported mosaics. Do you really want to overwrite it?"),
            btn_labels = c("Nope", "Yep"),
            btn_colors = c("#FE642E", "#04B404")
          )
          observe({
            if (!is.null(input$confirmmosaicname)) {
              if (input$confirmmosaicname) {
                for (i in 1:length(new_mosaic_name)) {
                  mosaic_data[[new_mosaic_name[[i]]]] <- create_reactval(new_mosaic_name[[i]], mosaic_input(input_file_selected$paths$datapath[[i]], info = FALSE))
                }
              } else {
                return()
              }
            }
          })
        } else {
          # If it doesn't exist, create a new reactiveValues and add it to mosaic_data
          for (i in 1:length(new_mosaic_name)) {
            mosaic_data[[new_mosaic_name[[i]]]] <- create_reactval(new_mosaic_name[[i]], mosaic_input(input_file_selected$paths$datapath[[i]], info = FALSE))
          }
        }


        observe({
          mosaicnames <-  setdiff(names(mosaic_data), "mosaic")
          if ("mosaic" %in% mosaicnames) {
            sendSweetAlert(
              session = session,
              title = "Reserved name used",
              text = "'mosaic' is a reserved name and is strongly suggested to not use this name for files. It may be omitted in some dropdown menus.",
              type = "warning"
            )
          }

        })
      }
    })

    # if example mosaic is used
    # mosaic provided by Arthur Bernardeli https://www.linkedin.com/in/arthur-bernardeli-5a1a0b5a/
    observeEvent(input$inputrastertype, {
      if(input$inputrastertype == "example raster"){
        filepath <- file.path(system.file(package = "plimanshiny"), "app/www/soy_ortho.tif")
        mosaic_data[["example_raster"]] <- create_reactval("example_raster", mosaic_input(filepath, info = FALSE))
      }
    })
    observe({
      if(length(names(mosaic_data) > 0)){
        mosaicnames <-  setdiff(names(mosaic_data), "mosaic")
        updateSelectInput(session, "mosaictoanalyze",
                          choices = mosaicnames,
                          selected = mosaicnames[[1]])
      }
    })

    observe({
      req(input$mosaictoanalyze)
      activemosaic$name <- input$mosaictoanalyze
      mosaic_data[["mosaic"]] <- create_reactval("mosaic", mosaic_data[[input$mosaictoanalyze]]$data)
      nl <- terra::nlyr(mosaic_data[[input$mosaictoanalyze]]$data)
      updateSelectInput(session, "r_band",
                        choices = paste0(c("NA", paste(1:nl))),
                        selected = "1")
      updateSelectInput(session, "g_band",
                        choices = paste0(c("NA", paste(1:nl))),
                        selected = ifelse(nl > 1, "2", "NA"))
      updateSelectInput(session, "b_band",
                        choices = paste0(c("NA", paste(1:nl))),
                        selected = ifelse(nl > 2, "3", "NA"))
      updateSelectInput(session, "re_band",
                        choices = paste0(c("NA", paste(1:nl))))
      updateSelectInput(session, "nir_band",
                        choices = paste0(c("NA", paste(1:nl))))
      updateSelectInput(session, "swir_band",
                        choices = paste0(c("NA", paste(1:nl))))
      updateSelectInput(session, "tir_band",
                        choices = paste0(c("NA", paste(1:nl))))
    })

    observe({
      # Check if a mosaic is selected
      req(input$mosaictoanalyze)
      showNotification(
        ui = "Importing the mosaic and generating the basemap. Please, wait!",
        type = "message",
        duration = NULL,   # Infinite duration until manually removed
        id = "importmosaic"
      )
      updateSelectInput(session, "howtoplot",
                        choices = c("RGB", names(mosaic_data[[input$mosaictoanalyze]]$data)))
    })
    observe({
      req(input$mosaictoanalyze)
      crsmo <- terra::crs(mosaic_data[[input$mosaictoanalyze]]$data) != ""
      if(crsmo && terra::is.lonlat(mosaic_data[[input$mosaictoanalyze]]$data)){
        eps <- mosaic_epsg(mosaic_data[[input$mosaictoanalyze]]$data)
        text <- paste0("The current raster is in the lat/lon coordinate system, which may result in processing errors when trying to segment individuals in the `mosaic_analyze()` function. It is highly suggested to reproject the raster using mosaic_project() with ", eps)
        show_alert("CRS is on Lon/Lat format.",
                   text = text,
                   type = "warning")
      }
    })
    observeEvent(input$mosaicinfomosaic, {
      req(input$mosaictoanalyze)
      req(mosaic_data[[input$mosaictoanalyze]]$data)
      mosaic_info(mosaic_data[[input$mosaictoanalyze]]$data)
    })


    observeEvent(input$useminmax, {
      if(input$useminmax){
        req(input$mosaictoanalyze)
        req(mosaic_data[[input$mosaictoanalyze]]$data)
        vals <- terra::spatSample(mosaic_data[[input$mosaictoanalyze]]$data[[c(suppressWarnings(as.numeric(r$r)),
                                                                               suppressWarnings(as.numeric(g$g)),
                                                                               suppressWarnings(as.numeric(b$b)))]], 2000)
        slider_range <- mod_histo_slider_server("historaster", data_reactive = reactiveVal(as.numeric(as.matrix(vals))))

        observeEvent(input$donebands, {
          if(!input$useminmax){
            zlim$zlim <- NULL
          } else{
            zlim$zlim <- c(slider_range()$min, slider_range()$max)
          }
        })
      }
    })


    output$mosaic_plot <- renderPlot({
      req(input$mosaictoanalyze)
      if (input$showmosaic == "bands") {
        terra::plot(mosaic_data[[input$mosaictoanalyze]]$data)
      }
      if(input$showmosaic == "hist"){
        terra::hist(mosaic_data[[input$mosaictoanalyze]]$data)
      }
      removeNotification(id = "importmosaic")
      removeNotification(id = "rebuilding")
    })




    observe({
      if(!input$intmap & input$showmosaic == "rgb"){
        req(input$mosaictoanalyze)
        req(mosaic_data[[input$mosaictoanalyze]]$data)
        plimanshiny_viewer_server("mosaic_viewer",
                                  mosaic_data[[input$mosaictoanalyze]]$data,
                                  r = reactive({ suppressWarnings(as.numeric(r$r)) }),
                                  g = reactive({ suppressWarnings(as.numeric(g$g)) }),
                                  b = reactive({ suppressWarnings(as.numeric(b$b)) }),
                                  usemargin = reactive({input$showlegend}),
                                  zlim = reactive(zlim$zlim)
        )
      }
      removeNotification(id = "importmosaic")
    })
    removeNotification(id = "rebuilding")

    #
    observe({
      if(input$intmap){
        req(input$mosaictoanalyze)
        req(input$howtoplot)
        if(input$howtoplot == "RGB"){
          if(nlyr(mosaic_data[[input$mosaictoanalyze]]$data) >= 3){
            bmtmp <- mosaic_view(
              mosaic_data[[input$mosaictoanalyze]]$data,
              r = suppressWarnings(as.numeric(r$r)),
              g = suppressWarnings(as.numeric(g$g)),
              b = suppressWarnings(as.numeric(b$b)),
              quantiles = quantiles$q,
              max_pixels = maxpixel$mp
            )
          } else{
            show_alert("Ops, too few bands",
                       text = "The current mosaic has too few bands and an RGB image cannot be rendered. Plotting the first layer of the raster image. Change `Show` to `bands` to choose which band to plot.",
                       type = "warning")
            bmtmp <- mosaic_view(
              mosaic_data[[input$mosaictoanalyze]]$data[[1]],
              color_regions  = scales::brewer_pal(palette = "RdYlGn")(8),
              max_pixels = maxpixel$mp,
              na.color = "transparent"
            )
          }

        } else{
          bmtmp <-
            mosaic_view(mosaic_data[[input$mosaictoanalyze]]$data[input$howtoplot],
                        show = "index",
                        color_regions  = scales::brewer_pal(palette = "RdYlGn")(8),
                        max_pixels = maxpixel$mp,
                        na.color = "transparent")
        }

        basemap$map <- bmtmp
        output$mosaic_mapview <- renderLeaflet({
          req(bmtmp)
          removeNotification(id = "importmosaic")
          removeNotification(id = "rebuilding")
          bmtmp@map
        })
      }
    })

    mod_download_mosaic_server("downloadmosaic", mosaic_data[["mosaic"]]$data)
  })
}


## To be copied in the UI
# mod_mosaic_prepare_ui("mosaic_prepare_1")

## To be copied in the server
# mod_mosaic_prepare_server("mosaic_prepare_1")
