#' shapefile_prepare UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_shapefile_prepare_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    col_3(
      bs4TabCard(
        id = "tabsindex",
        width = 12,
        status = "success",
        title = "Settings",
        selected = "Input",
        solidHeader = FALSE,
        type = "tabs",
        tabPanel(
          title = "Input",
          fluidRow(
            col_6(
              prettyRadioButtons(
                inputId = ns("shapetype"),
                label = "I want to...",
                choices = c("Build", "Import"),
                icon = icon("check"),
                bigger = TRUE,
                status = "info",
                animation = "jelly"
              )
            ),
            col_6(
              fluidRow(
                col_4(
                  br(),
                  dropdown(
                    fluidRow(
                      col_6(
                        colorPickr(
                          inputId = ns("colorfill"),
                          label = "Fill color",
                          swatches = scales::viridis_pal()(10),
                          theme = "monolith",
                          useAsButton = TRUE,
                          selected = "#0361FC",
                        )
                      ),
                      col_6(
                        colorPickr(
                          inputId = ns("colorstroke"),
                          label = "Stroke color",
                          swatches = scales::viridis_pal()(10),
                          theme = "monolith",
                          useAsButton = TRUE,
                          selected = "darkred",
                        )
                      )
                    ),
                    selectInput(
                      ns("fillid"),
                      label = "Fill color",
                      choices = c("none", "unique_id", "block", "plot_id"),
                      selected = "none",
                    ),
                    pickerpalette(id, "palplot", selected = "RdYlGn"),
                    sliderInput(ns("ncolors"),
                                label = "Number of colors",
                                min = 0,
                                max = 100,
                                value = 4),
                    sliderInput(ns("alphacolorfill"),
                                label = "Fill opacity",
                                min = 0,
                                max = 1,
                                value = 0.7),
                    sliderInput(ns("lwdt"),
                                label = "Line width",
                                min = 0,
                                max = 5,
                                value = 2),
                    style = "unite",
                    icon = icon("gear"),
                    status = "success",
                    width = "360px",
                    animate = animateOptions(enter = "fadeInLeft", exit = "fadeOutRight", duration = 1),
                    tooltip = tooltipOptions(title = "Settings")
                  )
                ),
                col_8(
                  conditionalPanel(
                    condition = "input.shapetype == 'Build'", ns = ns,
                    br(),
                    actionButton(
                      inputId = ns("guideshape"),
                      label = tagList(
                        icon = icon("question-circle", verify_fa = FALSE), "Guide"
                      ),
                      style = "color: white ; background-color: #dd4b39",
                      class = "btn-danger"
                    )
                  ),
                  conditionalPanel(
                    condition = "input.shapetype == 'Import'", ns = ns,
                    br(),
                    actionButton(
                      inputId = ns("guideshapeimput"),
                      label = tagList(
                        icon = icon("question-circle", verify_fa = FALSE), "Guide"
                      ),
                      style = "color: white ; background-color: #dd4b39",
                      class = "btn-danger"
                    )
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.shapetype == 'Build'", ns = ns,
            textInput(ns("shapenamebuild"),
                      label = "Shapefile Name",
                      value = "Shapefile Build"),
            hl(),
            fluidRow(
              col_3(
                selectInput(
                  ns("plotlayout"),
                  label = "Layout",
                  choices = c("tblr", "tbrl", "btlr", "btrl", "lrtb", "lrbt", "rltb", "rlbt"),
                  selected = "lrtb",
                )
              ),
              col_5(
                prettyCheckbox(
                  inputId = ns("serpentine"),
                  label = "Serpentine?",
                  value = TRUE,
                  status = "info",
                  icon = icon("thumbs-up"),
                  plain = TRUE,
                  outline = TRUE,
                  animation = "rotate"
                )
              ),
              col_4(
                prettyCheckbox(
                  inputId = ns("buildblocks"),
                  label = "Blocks?",
                  value = FALSE,
                  status = "info",
                  icon = icon("thumbs-up"),
                  plain = TRUE,
                  outline = TRUE,
                  animation = "rotate"
                )
              )
            ),
            conditionalPanel(
              condition = "input.buildblocks == true", ns = ns,
              fluidRow(
                col_6(
                  actionBttn(
                    ns("doneblock"),
                    label = "Store block",
                    icon = icon("plus"),
                    color = "success",
                    style = "jelly"
                  )
                ),
                col_6(
                  textOutput(ns("nblocksdone"))
                )
              )
            ),
            hl(),
            fluidRow(
              style = "margin-top: -10px;",
              col_7(
                actionBttn(
                  ns("createupdate"),
                  label = "Create-update",
                  icon = icon("arrows-rotate"),
                  color = "success",
                  style = "jelly"
                )
              ),
              col_5(
                conditionalPanel(
                  condition = "input['config_1-plotinfo'] === true",
                  actionBttn(
                    ns("plotinfo"),
                    label = "Plot info",
                    icon = icon("info"),
                    color = "success",
                    style = "jelly"
                  )
                )
              )
            ),
            fluidRow(
              col_6(
                textInput(ns("ncols"),
                          label = "Number of columns",
                          value = 1)
              ),
              col_6(
                textInput(ns("nrows"),
                          label = "Number of rows",
                          value = 1)
              )
            ),
            fluidRow(
              style = "margin-top: -10px;",
              col_6(
                textInput(ns("plot_width"),
                          label = "Plot width",
                          value = NA)
              ),
              col_6(
                textInput(ns("plot_height"),
                          label = "Plot height",
                          value = NA)
              )
            ),
            fluidRow(
              style = "margin-top: -10px;",
              col_6(
                textInput(ns("buffercol"),
                          label = "Plot buffer",
                          value = 0)
              ),
              col_6(
                textInput(ns("numplots"),
                          label = "Number of plots",
                          value = "")
              )
            ),
            hl(),
            fluidRow(
              style = "margin-top: -10px;",
              prettyCheckbox(
                inputId = ns("shapedone"),
                label = "Shapefile finished",
                value = FALSE,
                status = "info",
                icon = icon("thumbs-up"),
                plain = TRUE,
                outline = TRUE,
                animation = "rotate"
              )
            ),
            conditionalPanel(
              condition = "input.shapedone == true", ns = ns,
              divclass("shapeimp3",
                       fluidRow(
                         col_6(
                           materialSwitch(
                             inputId = ns("editplots"),
                             label = "Edit plots?",
                             value = FALSE,
                             status = "danger"
                           ),
                         ),
                         col_6(
                           conditionalPanel(
                             condition = "input.editplots == true", ns = ns,
                             prettyCheckbox(
                               inputId = ns("editdone"),
                               label = "Edition finished!",
                               value = FALSE,
                               status = "info",
                               icon = icon("thumbs-up"),
                               plain = TRUE,
                               outline = TRUE,
                               animation = "rotate"
                             )
                           )
                         )
                       )
              )
            )
          ),
          conditionalPanel(
            condition = "input.shapetype == 'Import'", ns = ns,
            tags$hr(),
            fluidRow(
              col_6(
                shinyFilesButton(id=ns("shapefileinput"),
                                 label="Shapefile",
                                 title="Shapefile",
                                 buttonType = "primary",
                                 multiple = TRUE,
                                 class = NULL,
                                 icon = icon("magnifying-glass"),
                                 style = NULL)
              ),
              col_6(
                prettyCheckbox(
                  inputId = ns("multiline"),
                  label = "Multilinestring",
                  value = FALSE,
                  icon = icon("check"),
                  status = "success",
                  animation = "rotate"
                )
              )
            ),
            textInput(
              ns("filemosaicpath"),
              label = "Choosen file(s)",
              value = "",
              width = "100%"
            ),
            conditionalPanel(
              condition = "input.filemosaicpath != ''", ns = ns,
              fluidRow(
                actionBttn(ns("importshapefile"),
                           label = "Import the choosen file(s)",
                           no_outline = FALSE,
                           icon = icon("file-import"),
                           style = "material-flat",
                           color = "primary")
              )
            ),
            tags$hr(),
            selectInput(ns("colorshapeimport"),
                        label = "Fill color",
                        choices = NULL),
            fluidRow(
              col_6(
                materialSwitch(
                  inputId = ns("editplotsimpo"),
                  label = "Edit Shapefile?",
                  value = FALSE,
                  status = "danger"
                )
              ),
              col_6(
                conditionalPanel(
                  condition = "input['config_1-plotinfo'] === true",
                  actionBttn(
                    ns("plotinfo2"),
                    label = "Plot info",
                    icon = icon("info"),
                    color = "success",
                    style = "jelly"
                  )
                )
              )
            ),
            conditionalPanel(
              condition = "input.editplotsimpo == true", ns = ns,
                       prettyCheckbox(
                         inputId = ns("editdoneimpo"),
                         label = "Edition finished!",
                         value = FALSE,
                         status = "info",
                         icon = icon("thumbs-up"),
                         plain = TRUE,
                         outline = TRUE,
                         animation = "rotate"
                       ),
                       br()
            )
          ),
          selectInput(ns("shapefiletoanalyze"),
                      label = "Active Shapefile",
                      choices = NULL)
        ),
        tabPanel(
          title = "Download",
          mod_download_shapefile_ui(ns("downloadshapefile"), label = "Download")
        )
      )
    ),
    col_9(
      uiOutput(ns("uishape"))
    )
  )
}

helpshp <-
  read.csv(file = system.file("app/www/helps.csv", package = "plimanshiny", mustWork = TRUE), sep = ";") |>
  dplyr::filter(type == "shape")
shapeimp <-
  read.csv(file = system.file("app/www/helps.csv", package = "plimanshiny", mustWork = TRUE), sep = ";") |>
  dplyr::filter(type == "shapeimp")
#' shapefile_prepare Server Functions
#'
#' @noRd
mod_shapefile_prepare_server <- function(id, mosaic_data, basemap, shapefile, activemosaic, r, g, b, settings){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    output$uishape <- renderUI({
      if(input$shapetype == "Build" & !input$editplots & !input$shapedone){
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "760px",
          title = "Results",
          selected = "Control points",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            "Control points",
            editModUI(ns("shapefile_build"), height = "720px") |> add_spinner()
          ),
          tabPanel(
            "Built shapefile",
            leafletOutput(ns("createdshapes"), height = "720px") |> add_spinner()
          )
        )
      } else if(input$shapetype == "Build" & input$shapedone & !input$editplots){
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "790px",
          title = "Results",
          selected = "Built shapefile",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            "Built shapefile",
            leafletOutput(ns("plotshapedone"), height = "700px") |> add_spinner()
          )
        )
      } else if(input$shapetype == "Build" & input$editplots & !input$editdone){
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "790px",
          title = "Results",
          selected = "Built shapefile",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            "Built shapefile",
            leafletOutput(ns("plotshapedone"), height = "700px") |> add_spinner()
          ),
          tabPanel(
            "Plot edition",
            editModUI(ns("plotedit"), height = "700px") |> add_spinner()
          )
        )

      } else if(input$shapetype == "Import" & !input$editplotsimpo){
        bs4Card(
          width = 12,
          height = "780px",
          title = "Shapefile",
          color = "success",
          status = "success",
          maximizable = TRUE,
          leafletOutput(ns("shapefile_mapview"), height = "700px") |> add_spinner()
        )
      } else if(input$shapetype == "Import" & input$editplotsimpo){
        bs4Card(
          width = 12,
          height = "780px",
          title = "Building the shapefile",
          color = "success",
          status = "success",
          maximizable = TRUE,
          fluidRow(
            col_6(
              leafletOutput(ns("shapefile_mapview"), height = "700px") |> add_spinner()
            ),
            col_6(
              conditionalPanel(
                condition = "input.editplotsimpo == true & input.editdoneimpo == false", ns = ns,
                editModUI(ns("ploteditimpo"), height = "700px") |> add_spinner()
              )
            )
          )
        )
      }

    })

    observeEvent(input$buildshapefile, {
      if(!input$buildshapefile){
        updatePrettyCheckbox(session, "shapedone", value = TRUE)
      } else{
        updatePrettyCheckbox(session, "shapedone", value = FALSE)
      }
    })


    shp <- reactiveValues()
    createdshape <- reactiveValues()
    layout_params <- reactiveValues()
    tmpshape <- reactiveValues()
    drawn <- reactiveValues()
    hascpoint <- reactiveValues(has = FALSE)
    observe({
      # create a basemap if mosaic is not available
      if(is.null(activemosaic$name)){
        if(is.null(basemap$map)){
          basemap$map <- mapview::mapview(map.types = c("Esri.WorldImagery", "OpenStreetMap", "CartoDB.Positron"))
        }
      } else{
        mosaic_data$mosaic <- mosaic_data[[activemosaic$name]]$data
      }
    })


    observeEvent(input$shapetype, {
      observeEvent(c(basemap$map, mosaic_data$mosaic, input$shapedone), {
        req(basemap$map)
        if (input$shapetype == "Build") {
          cpoints <- callModule(editMod, "shapefile_build",
                                basemap$map@map,
                                editor = "leafpm",
                                editorOptions = list(toolbarOptions = list(
                                  drawMarker = TRUE,
                                  drawPolygon = TRUE,
                                  drawPolyline = TRUE,
                                  drawCircle = TRUE,
                                  drawRectangle = TRUE,
                                  editMode = TRUE,
                                  cutPolygon = TRUE,
                                  removalMode = TRUE,
                                  position = "topleft"
                                )))
          observeEvent(c(cpoints()$finished, cpoints()$edited), {
            if(!is.null(cpoints()$finished)){
              drawn$finished <-
                cpoints()$finished |>
                convert_to_metric() |>
                point_to_polygon() |>
                dplyr::slice_tail(n = 1)
            }
            if(!is.null(cpoints()$edited) & !is.null(cpoints()$finished)){
              idedit <-
                cpoints()$edited |>
                convert_to_metric() |>
                point_to_polygon() |>
                dplyr::slice_tail(n = 1) |>
                dplyr::pull(edit_id)
              drawnedit <-
                cpoints()$finished |>
                convert_to_metric() |>
                point_to_polygon() |>
                dplyr::slice_tail(n = 1) |>
                dplyr::pull(edit_id)
              if(idedit == drawnedit){
                drawn$finished <-
                  cpoints()$edited |>
                  convert_to_metric() |>
                  point_to_polygon() |>
                  dplyr::slice_tail(n = 1)
              }
            }
          })

          observeEvent(input$createupdate, {

            if(is.null(mosaic_data$mosaic)){
              mosaic_data$mosaic <- rast(nrows=180, ncols=360, nlyrs=3, crs = "EPSG:3857")
            }
            req(drawn$finished)
            nr <- input$nrows |> chrv2numv()
            nc <- input$ncols |> chrv2numv()
            pw <- input$plot_width |> chrv2numv()
            ph <- input$plot_height |> chrv2numv()
            t1 <- nr == 1
            t2 <- nc == 1
            ps <- any(t2 & t1 == TRUE)
            if(length(ph) == 0 | ps){
              ph <- NULL
            }
            if(length(pw) == 0 | ps){
              pw <- NULL
            }

            shpt <- shapefile_build(
              mosaic_data$mosaic,
              basemap$map,
              controlpoints = drawn$finished,
              nrow = nr,
              ncol = nc,
              layout = input$plotlayout,
              serpentine = input$serpentine,
              buffer_col = input$buffercol |> chrv2numv(),
              buffer_row = input$buffercol |> chrv2numv(),
              plot_width = pw,
              plot_height = ph,
              crop_to_shape_ext = FALSE,
              verbose = FALSE
            )[[1]]
            if(input$numplots != ""){
              shpt <-
                shpt |>
                dplyr::filter(plot_id %in% paste0("P", leading_zeros(1:chrv2numv(input$numplots), 4)))
            }
            shapefile[["shapefileplot"]] <- shpt
            tmpshape$tmp <- shpt
            output$createdshapes <- renderLeaflet({
              if(input$fillid == "none"){
                mapp <-
                  basemap$map + mapview::mapview(
                    shpt |> extract_number(block, plot_id),
                    color = input$colorstroke,
                    col.regions = input$colorfill,
                    alpha.regions = input$alphacolorfill,
                    legend = FALSE,
                    lwd = input$lwdt,
                    layer.name = "shapes"
                  )
              } else{
                nlv <- length(unique(shpt |> sf::st_drop_geometry() |> dplyr::pull(input$fillid)))
                if(nlv > 8){
                  shpt <- shpt |> extract_number(block, plot_id)
                }
                mapp <-
                  basemap$map + mapview::mapview(
                    shpt |> extract_number(block, plot_id),
                    zcol = input$fillid,
                    col.regions = return_colors(input$palplot, n = input$ncolors),
                    alpha.regions = input$alphacolorfill,
                    lwd = input$lwdt,
                    layer.name = "shapes"
                  )
              }
              mapp@map
            })
          })

          nblock <- reactiveVal(0)
          observe({
            if(input$buildblocks){
              observeEvent(input$doneblock, {
                nblock(nblock() + 1)
                output$nblocksdone <- renderText({
                  glue::glue("Built blocks: {nblock()}")
                })
                block_name <- paste0("B", sprintf("%02d", nblock()))
                createdshape[[block_name]] <- tmpshape$tmp
                sendSweetAlert(
                  session = session,
                  title = "Block built",
                  text = "The shapes in the current block have been built. Just insert a new polygon with the 'Draw polygon' tool to delineate another block. To finish the shapefile construction, check the box 'Shapefile finished'.",
                  type = "info"
                )
              })
            } else{
              req(tmpshape$tmp)
              createdshape[["B01"]] <- tmpshape$tmp
            }
          })


          observeEvent(input$shapedone,{
            if(input$shapedone){
              cpoints <- NULL
              drawn$finished <- NULL
              req(input$shapenamebuild)
              req(createdshape)
              shptemp <-
                reactiveValuesToList(createdshape) |>
                dplyr::bind_rows(.id = "block") |>
                dplyr::mutate(unique_id = dplyr::row_number(), .before = 1)
              shapefile[["shapefileplot"]] <- shptemp
              shapefile[[input$shapenamebuild]] <- create_reactval(input$shapenamebuild, shptemp)

              observe({
                shapefilenames <- setdiff(c("none", names(shapefile)), c("shapefile", "shapefileplot"))
                # Update selectInput choices
                updateSelectInput(session, "shapefiletoanalyze",
                                  choices = shapefilenames,
                                  selected = shapefilenames[[length(shapefilenames)]])
              })

              output$plotshapedone <- renderLeaflet({
                if(input$fillid == "none"){
                  mapp <-
                    basemap$map +
                    mapview::mapview(shapefile[[input$shapefiletoanalyze]]$data,
                                     color = input$colorstroke,
                                     col.regions = input$colorfill,
                                     alpha.regions = input$alphacolorfill,
                                     legend = FALSE,
                                     lwd = input$lwdt,
                                     layer.name = "shapes")
                } else{
                  nlv <- length(unique(shapefile[[input$shapefiletoanalyze]]$data |> sf::st_drop_geometry() |> dplyr::pull(input$fillid)))
                  if(nlv > 7){
                    shptemp <- shapefile[[input$shapefiletoanalyze]]$data |> extract_number(block, plot_id)
                  } else{
                    shptemp <- shapefile[[input$shapefiletoanalyze]]$data
                  }

                  mapp <-
                    basemap$map +
                    mapview::mapview(shptemp,
                                     zcol = input$fillid,
                                     col.regions = return_colors(input$palplot, n = input$ncolors),
                                     alpha.regions = input$alphacolorfill,
                                     lwd = input$lwdt,
                                     layer.name = "shapes")
                }
                mapp@map
              })
            }
          })


          observeEvent(c(input$editplots, !input$editdone),{
            if(inherits(shapefile[[input$shapenamebuild]]$data, "list")){
              shptmp <- do.call(rbind, shapefile[[input$shapenamebuild]]$data)
            } else{
              shptmp <- shapefile[[input$shapenamebuild]]$data
            }
            if(input$editplots == TRUE){
              shapes <-
                shptmp |>
                dplyr::mutate(`_leaflet_id` = 1:nrow(shptmp),
                              feature_type = "polygon") |>
                dplyr::relocate(geometry, .after = 2) |>
                sf::st_transform(crs = 4326)

              mapp <-
                basemap$map@map |>
                leaflet::addPolygons(
                  data = shapes,
                  weight = 3,
                  color = input$colorstroke,
                  fillColor = input$colorfill,
                  opacity = 1,
                  fillOpacity = input$alphacolorfill,
                  group = "editable"
                )

              editedpoints <- callModule(editMod, "plotedit",
                                         leafmap = mapp,
                                         targetLayerId = "editable")

              observeEvent(input$editdone,{
                if(input$editdone == TRUE){
                  if(!is.null(editedpoints()$all)){
                    editeshp <- editedpoints()$all |>
                      sf::st_transform(sf::st_crs(mosaic_data$mosaic)) |>
                      dplyr::select(geometry)

                    shapefile[[input$shapenamebuild]]$data <- editeshp |> check_cols_shpinp()

                    output$plotshapedone <- renderLeaflet({
                      mapp <-
                        basemap$map +
                        mapview::mapview(shapefile[[input$shapenamebuild]]$data,
                                         color = input$colorstroke,
                                         z.col = "plot_id",
                                         alpha.regions = input$alphacolorfill,
                                         lwd = input$lwdt,
                                         legend = FALSE,
                                         layer.name = "shapes")
                      mapp@map
                    })
                  }
                  updateMaterialSwitch(session, "editplots", value = FALSE)
                }
              })
            }
          })
        }
      })
    })


    # Import a shapefile
    observeEvent(input$shapetype, {
      if (input$shapetype == "Import") {
        # observeEvent(input$import_shapefile, {


        # shapefile
        pathshape <- reactiveValues(file = NULL)
        observe({
          shinyFileChoose(input, "shapefileinput",
                          root = getVolumes()(),
                          filetypes = c("rds",  "shp",  "json", "kml",  "gml",  "dbf",  "sbn",  "sbx",  "shx",  "prj", "cpg"),
                          session = session)
          if(!is.null(input$shapefileinput)){
            pathshape$file <- parseFilePaths(getVolumes()(), input$shapefileinput)
            if(length(pathshape$file$datapath) != 0){
              updateTextInput(session, "filemosaicpath", value = paste0(pathshape$file$datapath, collapse = ", "))
            }
          }
        })


        observeEvent(input$importshapefile, {
          showNotification(
            ui = "Importing the shapefile(s)... Please, wait!",
            type = "message",
            duration = NULL,   # Infinite duration until manually removed
            id = "importshp"
          )
          if(length(pathshape$file$datapath) != 0){
            new_mosaic_name <- sapply(pathshape$file$datapath, file_name)
            # Check if the mosaic already exists in mosaic_data
            if (any(new_mosaic_name %in% names(shapefile))) {
              # If it exists, update the existing reactiveValues
              moname <- new_mosaic_name[new_mosaic_name %in% names(shapefile)]
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
                      shapefile[[new_mosaic_name[[i]]]] <- create_reactval(new_mosaic_name[[i]], shapefile_input(pathshape$file$datapath[[i]], info = FALSE, multilinestring = input$multiline))
                    }
                  } else {
                    return()
                  }
                }
              })
            } else {
              # If it doesn't exist, create a new reactiveValues and add it to mosaic_data
              for (i in 1:length(new_mosaic_name)) {
                shapefile[[new_mosaic_name[[i]]]] <- create_reactval(new_mosaic_name[[i]], shapefile_input(pathshape$file$datapath[[i]], info = FALSE, multilinestring = input$multiline))
              }
            }

            observe({
              mosaicnames <-  setdiff(names(shapefile), c("mosaic", "shapefileplot"))
              updateSelectInput(session, "shapefiletoanalyze",
                                choices = mosaicnames,
                                selected = mosaicnames[[1]])
              removeNotification(id = "importshp")
              #

            })
          }
        })


        observe({
          shapefilenames <- setdiff(c("none", names(shapefile)), c("shapefile", "shapefileplot"))
          # Update selectInput choices
          updateSelectInput(session, "shapefiletoanalyze",
                            choices = shapefilenames,
                            selected = shapefilenames[[length(shapefilenames)]])
          if ("shapefile" %in% names(shapefile)) {
            sendSweetAlert(
              session = session,
              title = "Reserved name used",
              text = "'shapefile' is a reserved name and is strongly suggested to not use this name for files. It may be omitted in some dropdown menus.",
              type = "warning"
            )
          }

        })


        observe({
          req(input$shapefiletoanalyze)  # Ensure mosaic_data$mosaic is not NULL

          updateSelectInput(session, "colorshapeimport", choices = c("none", names(shapefile[[input$shapefiletoanalyze]]$data)))
          updateSelectInput(session, "fillid", choices = c("none", names(shapefile[[input$shapefiletoanalyze]]$data)))

          if(!is.null(mosaic_data$mosaic) & input$shapefiletoanalyze != "none"){
            if(sf::st_crs(shapefile[[input$shapefiletoanalyze]]$data) != sf::st_crs(mosaic_data$mosaic)){
              sendSweetAlert(
                session = session,
                title = "Invalid CRS",
                text = "The Coordinate Reference System (CRS) of the shapefile does
            not match the input mosaic. Trying to set the shapefile's CRS to match the mosaic one.",
                type = "warning"
              )
              shp <- shapefile[[input$shapefiletoanalyze]]$data |> sf::st_transform(crs = sf::st_crs(mosaic_data$mosaic))
              shapefile[[input$shapefiletoanalyze]] <- create_reactval(input$shapefiletoanalyze, shp)
            }
          }
          output$shapefile_mapview <- renderLeaflet({
            req(input$colorshapeimport)
            req(shapefile[[input$shapefiletoanalyze]]$data)
            if(is.null(basemap$map)){
              if(input$colorshapeimport == "none"){
                mapp <- mapview::mapview(shapefile[[input$shapefiletoanalyze]]$data,
                                         color = input$colorstroke,
                                         col.regions = input$colorshapeimport,
                                         alpha.regions = input$alphacolorfill,
                                         legend = FALSE,
                                         lwd = input$lwdt,
                                         layer.name = "shapes")
              } else {
                nlv <- length(unique(shapefile[[input$shapefiletoanalyze]]$data |> sf::st_drop_geometry() |> dplyr::pull(input$colorshapeimport)))
                if(nlv > 8){
                  shptemp <- shapefile[[input$shapefiletoanalyze]]$data |> extract_number(block, plot_id)
                } else{
                  shptemp <- shapefile[[input$shapefiletoanalyze]]$data
                }
                mapp <-
                  mapview::mapview(shptemp,
                                   zcol = input$colorshapeimport,
                                   col.regions = return_colors(input$palplot, n = input$ncolors),
                                   alpha.regions = input$alphacolorfill,
                                   lwd = input$lwdt,
                                   layer.name = "shapes")
              }
            } else{
              if(input$colorshapeimport == "none"){
                mapp <-
                  basemap$map +
                  mapview::mapview(shapefile[[input$shapefiletoanalyze]]$data,
                                   color = input$colorstroke,
                                   col.regions = input$colorfill,
                                   alpha.regions = input$alphacolorfill,
                                   legend = FALSE,
                                   lwd = input$lwdt,
                                   layer.name = "shapes")
              } else {
                nlv <- length(unique(shapefile[[input$shapefiletoanalyze]]$data |> sf::st_drop_geometry() |> dplyr::pull(input$colorshapeimport)))
                if(nlv > 8){
                  shptemp <- shapefile[[input$shapefiletoanalyze]]$data |> extract_number(block, plot_id)
                } else{
                  shptemp <- shapefile[[input$shapefiletoanalyze]]$data
                }
                mapp <-
                  basemap$map +
                  mapview::mapview(shptemp,
                                   zcol = input$colorshapeimport,
                                   col.regions = return_colors(input$palplot, n = input$ncolors),
                                   alpha.regions = input$alphacolorfill,
                                   lwd = input$lwdt,
                                   layer.name = "shapes")
              }
            }
            mapp@map
          })

          observeEvent(c(input$editplotsimpo, !input$editdoneimpo),{
            if(input$editplotsimpo == TRUE){
              shapes <-
                shapefile[[input$shapefiletoanalyze]]$data |>
                dplyr::mutate(`_leaflet_id` = 1:nrow(shapefile[[input$shapefiletoanalyze]]$data),
                              feature_type = "polygon") |>
                dplyr::relocate(geometry, .after = 2) |>
                sf::st_transform(crs = 4326)


              if(is.null(basemap$map)){
                mapp <-
                  leaflet::leaflet() |>
                  addTiles(options = tileOptions(minZoom = 1, maxZoom = 30)) |>
                  leaflet::addPolygons(
                    data = shapes,
                    color = input$colorstroke,
                    fillColor = input$colorfill,
                    opacity = 1,
                    fillOpacity = input$alphacolorfill,
                    group = "editable"
                  )
              } else{
                mapp <-
                  basemap$map@map |>
                  leaflet::addPolygons(
                    data = shapes,
                    color = input$colorstroke,
                    fillColor = input$colorfill,
                    opacity = 1,
                    fillOpacity = input$alphacolorfill,
                    group = "editable"
                  )
              }

              editedpoints <- callModule(editMod, "ploteditimpo",
                                         leafmap = mapp,
                                         targetLayerId = "editable")

              observeEvent(input$editdoneimpo,{
                if(input$editdoneimpo == TRUE){
                  if(!is.null(editedpoints()$all)){

                    shapefile[[input$shapefiletoanalyze]]$data <-
                      editedpoints()$all |>
                      sf::st_transform(crs = sf::st_crs(mosaic_data$mosaic)) |>
                      dplyr::select(geometry)
                    output$plotshapedone <- renderLeaflet({
                      if(is.null(basemap$map)){
                        mapp <-
                          basemap$map +
                          mapview::mapview(shapefile[[input$shapefiletoanalyze]]$data,
                                           color = input$colorstroke,
                                           col.regions = input$colorfill,
                                           alpha.regions = input$alphacolorfill,
                                           lwd = input$lwdt,
                                           layer.name = "shapes")
                      } else{
                        mapp <-
                          mapview::mapview(shapefile[[input$shapefiletoanalyze]]$data,
                                           color = input$colorstroke,
                                           col.regions = input$colorfill,
                                           alpha.regions = input$alphacolorfill,
                                           lwd = input$lwdt,
                                           layer.name = "shapes")
                      }
                      mapp@map
                    })
                  }
                  updateMaterialSwitch(session, "editplotsimpo", value = FALSE)
                }
              })
            }
          })
        })
        # })
      }
    })



    # Plot information
    observeEvent(c(input$plotinfo, input$plotinfo2), {
      if(is.null(activemosaic$name)){
        mos <- rast(nrows=180, ncols=360, nlyrs=3, crs = "EPSG:3857")
      } else{
        mos <- mosaic_data$mosaic
      }
      distsss <- reactiveValues()
      perim <- reactiveValues()
      area <- reactiveValues()
      wid <- reactiveValues()
      hei <- reactiveValues()
      req(shapefile$shapefileplot)
      if(input$plotinfo | input$plotinfo2){
        updateSelectInput(session, "uniqueidinfo",
                          choices = 1:nrow(shapefile$shapefileplot),
                          selected = 1)
        output$plotinfop <- renderPlot({
          shpinfo <- shapefile$shapefileplot[input$uniqueidinfo, ]
          npoints <- sf::st_coordinates(shpinfo) |> nrow()
          coords <- sf::st_coordinates(shpinfo)[, 1:2]
          buff <- diff(range(coords[, 1])) * 0.15
          measures <- shapefile_measures(shpinfo)
          dists <-  suppressWarnings(as.matrix(sf::st_distance(sf::st_cast(shpinfo, "POINT")$geometry)))
          seq_dists <- c()
          for (i in 1:(ncol(dists) - 1)) {
            seq_dists <- c(seq_dists, dists[i, i + 1])
          }
          perim$val <- measures$perimeter
          area$val <- measures$area
          ncoors <-
            do.call(
              rbind,
              lapply(1:(nrow(coords) - 1), function(i){
                p1 <- coords[i, ]
                p2 <- coords[i + 1, ]
                p1 - (p1 - p2) / 2
              })
            )
          if(!is.null(mos) & (nrow(mos) != 180) & (nrow(mos) != 360)){
            mcro <- terra::crop(mos, terra::vect(shpinfo) |> terra::buffer(buff))
            if(terra::nlyr(mcro) > 2){
              if(input$stretch == "none"){
                terra::plotRGB(
                  mcro ^ input$gammacorr,
                  r = suppressWarnings(as.numeric(r$r)),
                  g = suppressWarnings(as.numeric(g$g)),
                  b = suppressWarnings(as.numeric(b$b)),
                  maxcell = 1e6
                )

              } else{
                terra::plotRGB(
                  mcro ^ input$gammacorr,
                  r = suppressWarnings(as.numeric(r$r)),
                  g = suppressWarnings(as.numeric(g$g)),
                  b = suppressWarnings(as.numeric(b$b)),
                  stretch = input$stretch,
                  maxcell = 1e6
                )
              }
            } else{
              terra::plot(mcro[[1]],
                          smooth = TRUE,
                          maxcell = 2e6)
            }
            shapefile_plot(shpinfo, add = TRUE, col = adjustcolor("salmon", 0.7))
          } else{
            shapefile_plot(shpinfo, col = adjustcolor("salmon", 0.7))
          }
          wid$val <- ifelse(npoints > 5, "-", paste0(round(measures$width, 3), " m"))
          hei$val <- ifelse(npoints > 5, "-", paste0(round(measures$height, 3), " m"))
          if(npoints < 15){
            boxtext(x =  ncoors[, 1],
                    y =  ncoors[, 2],
                    labels = paste0(round(seq_dists, 2), " m"),
                    col.bg = "salmon",
                    cex = 1.5)
          }
          cmass <- poly_mass(ncoors)
          boxtext(x =  mean(cmass[1]),
                  y =  mean(cmass[2]),
                  labels = paste0(round(area$val, 2), " m2"),
                  col.bg = "salmon",
                  cex = 1.5)
        })

        output$nplots <- renderValueBox({
          valueBox(
            value = tags$p(nrow(shapefile$shapefileplot), style = "font-size: 200%;"),
            subtitle = "Number of shapes",
            color = "success",
            icon = icon("list-ol")
          )
        })
        output$exparea <- renderValueBox({
          valueBox(
            value = tags$p(round(sum(sf::st_area(shapefile$shapefileplot)), 3), style = "font-size: 200%;"),
            subtitle = "Covered area (m2)",
            color = "success",
            icon = icon("square")
          )
        })
        output$pwidth <- renderValueBox({
          valueBox(
            value = tags$p(wid$val, style = "font-size: 200%;"),
            subtitle = "Plot width (m)",
            color = "success",
            icon = icon("arrows-left-right")
          )
        })
        output$phight <- renderValueBox({
          valueBox(
            value = tags$p(hei$val, style = "font-size: 200%;"),
            subtitle = "Plot height (m)       ",
            color = "success",
            icon = icon("arrows-up-down")
          )
        })

        output$pperimeter <- renderValueBox({
          valueBox(
            value = tags$p(round(perim$val, 3), style = "font-size: 200%;"),
            subtitle = "Perimeter (m)       ",
            color = "success",
            icon = icon("draw-polygon")
          )
        })
        output$parea <- renderValueBox({
          valueBox(
            value = tags$p(round(area$val, 3), style = "font-size: 200%;"),
            subtitle = "Area (m2)       ",
            color = "success",
            icon = icon("draw-polygon")
          )
        })

        showModal(
          modalDialog(
            title = "Plot measures",
            fluidRow(
              col_5(
                h4("Overview"),
                fluidRow(
                  col_6(
                    valueBoxOutput(ns("nplots"), width = 12)
                  ),
                  col_6(
                    valueBoxOutput(ns("exparea"), width = 12),
                  )
                ),
                h4("Individual measures"),
                fluidRow(
                  col_6(
                    valueBoxOutput(ns("pwidth"), width = 12)
                  ),
                  col_6(
                    valueBoxOutput(ns("phight"), width = 12)
                  )
                ),
                fluidRow(
                  col_6(
                    valueBoxOutput(ns("parea"), width = 12)
                  ),
                  col_6(
                    valueBoxOutput(ns("pperimeter"), width = 12)
                  )
                )
              ),
              col_7(
                fluidRow(
                  col_8(
                    selectInput(ns("uniqueidinfo"),
                                label = "Unique id",
                                choices = NULL,
                                width = "100%")
                  ),
                  col_4(
                    selectInput(
                      ns("stretch"),
                      label = "Stretch",
                      choices = c("none", "lin", "hist")
                    )
                  )
                ),
                sliderInput(
                  ns("gammacorr"),
                  label = "Gamma correction",
                  min = -5,
                  max = 5,
                  value = 1,
                  step = 0.1,
                  width = "100%"
                ),
                plotOutput(ns("plotinfop"), height = "600px")
              )
            ),
            footer = NULL,
            easyClose = TRUE,
            size = "xl"
          )
        )
      }
    })

    observe({
      req(input$shapefiletoanalyze)
      if(input$shapefiletoanalyze == "none"){
        shapefile[["shapefileplot"]] <- NULL
      } else{
        shapefile[["shapefileplot"]] <- shapefile[[input$shapefiletoanalyze]]$data
      }
    })

    mod_download_shapefile_server("downloadshapefile", terra::vect(shapefile$shapefileplot), name = "created_shp")




  })
}

## To be copied in the UI
# mod_shapefile_prepare_ui("shapefile_prepare_1")

## To be copied in the server
# mod_shapefile_prepare_server("shapefile_prepare_1")
