#' shapefilenative UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_shapefilenative_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4TabCard(
          id = ns("tabsshape"),
          width = 12,
          collapsible = FALSE,
          status = "success",
          selected = "Plots",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Plots",
            awesomeRadio(
              inputId = ns("inputshptypenat"),
              label = "Entry method",
              choices = c("load shapefile", "example shapefile"),
              selected = "load shapefile",
              status = "success",
              inline = TRUE
            ),
            conditionalPanel(
              condition = "input.inputshptypenat == 'load shapefile'", ns = ns,
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
                              selected = "#0361FC80",
                              opacity = TRUE
                            )
                          ),
                          col_6(
                            colorPickr(
                              inputId = ns("colorstroke"),
                              label = "Stroke color",
                              swatches = scales::viridis_pal()(10),
                              theme = "monolith",
                              useAsButton = TRUE,
                              selected = "#FFFFFF2E",
                              opacity = TRUE
                            )
                          )
                        ),
                        selectInput(
                          ns("fillid"),
                          label = "Fill color",
                          choices = c("none", "unique_id", "block", "plot_id"),
                          selected = "none",
                        ),
                        sliderInput(ns("lwdt"),
                                    label = "Line width",
                                    min = 0,
                                    max = 5,
                                    value = 2),
                        sliderInput(ns("alpha"),
                                    label = "Color opacity",
                                    min = 0,
                                    max = 1,
                                    value = 1),
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
                          value = "field_shape"),
                hl(),
                fluidRow(
                  col_4(
                    selectInput(
                      ns("plotlayout"),
                      label = "Layout",
                      choices = c("tblr", "tbrl", "btlr", "btrl", "lrtb", "lrbt", "rltb", "rlbt"),
                      selected = "lrtb",
                    )
                  ),
                  col_4(
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
                hl(),
                fluidRow(
                  style = "margin-top: -10px;",
                  col_7(
                    actionBttn(
                      ns("createupdate2"),
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
                        ns("plotinfonat"),
                        label = "Plot info",
                        icon = icon("info"),
                        color = "success",
                        style = "jelly"
                      )
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
                fluidRow(
                  col_3(
                    textInput(ns("ncols"),
                              label = "Columns",
                              value = 1)
                  ),
                  col_3(
                    textInput(ns("nrows"),
                              label = "Rows",
                              value = 1)
                  ),
                  col_3(
                    textInput(ns("plot_width"),
                              label = "Width",
                              value = NA)
                  ),
                  col_3(
                    textInput(ns("plot_height"),
                              label = "Height",
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
                prettyCheckbox(
                  inputId = ns("showplotid"),
                  label = "Show plot ID?",
                  value = TRUE,
                  status = "info",
                  icon = icon("thumbs-up"),
                  plain = TRUE,
                  outline = TRUE,
                  animation = "rotate"
                ),
                hl(),
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
                conditionalPanel(
                  condition = "input['config_1-plotinfo'] === true",
                  actionBttn(
                    ns("plotinfo2nat"),
                    label = "Plot info",
                    icon = icon("info"),
                    color = "success",
                    style = "jelly"
                  )
                )
              )
            ),
            selectInput(ns("shapefiletoanalyze"),
                        label = "Active Shapefile",
                        choices = NULL)
          ),
          tabPanel("Control Points",
                   DT::dataTableOutput(ns("points_table")),
                   conditionalPanel(
                     condition = sprintf("output['%s']", ns("show_panel")),
                     actionBttn(ns("editiondone"), "Edition Done", style = "pill", color = "success"),
                   ),
                   conditionalPanel(
                     condition = sprintf("output['%s']", ns("show_panel2")),
                     actionBttn(
                       ns("clearpoints"),
                       label = "Clear points",
                       icon = icon("trash"),
                       color = "danger",
                       style = "jelly"
                     )
                   ),

          ),
          tabPanel(
            title = "Download",
            mod_download_shapefile_ui(ns("downloadshapefile2"), label = "Download")
          )

        )
      ),
      col_9(
        plimanshiny_canvas_output(prefix = "shp", ns = ns)
      )
    )
  )
}

#' shapefilenative Server Functions
#'
mod_shapefilenative_server <- function(id, mosaic_data,  r, g, b, activemosaic, shapefile, zlim) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    mosaitoshape <- reactiveVal()
    observe({
      if(input$shapetype == "Import"){
        hideTab(inputId = "tabsshape", target = "Control Points")
      } else{
        showTab(inputId = "tabsshape", target = "Control Points")
      }
    })
    # force a false color
    if(is.na(r$r)){
      r$r <- 1
    }
    if(is.na(g$g)){
      g$g <- 2
    }
    if(is.na(b$b)){
      b$b <- 3
    }
    observe({
      if (input$shapetype == "Build") {
        observe({
          req(activemosaic$name)
          if(!is.null(activemosaic$name)){
            mosaitoshape(mosaic_data[[activemosaic$name]]$data)
          }
        })
        req(mosaitoshape())
        ############################################# BUILD A SHAPEFILE ##########################################################
        original_image_path <- file.path(tempdir(), "originalimage.png")
        basepolygon <- file.path(tempdir(), "imagewithpolygon.png")
        original_ext <- reactiveVal(ext(mosaitoshape()))
        polygeom <- reactiveVal()
        createdshape <- reactiveValues()
        tmpshape <- reactiveValues()
        nblock <- reactiveVal(0)

        # Reactive value to store the current cropped extent
        current_extent <- reactiveVal(ext(mosaitoshape()))
        wid <- reactiveVal()
        hei <- reactiveVal()
        widori <- reactiveVal()
        heiori <- reactiveVal()

        observe({
          sizes <- adjust_canvas(mosaitoshape())
          widori(sizes[[1]])
          heiori(sizes[[2]])
          # Send the adjusted canvas size to the client
          session$sendCustomMessage("adjustcanvas_shpSize", list(
            width = as.integer(widori()),
            height = as.integer(heiori())
          ))
        })

        # Generate and cache the original image on startup
        observe({
          sizes <- adjust_canvas(mosaitoshape())
          png(original_image_path, width = sizes[[1]], height = sizes[[2]])
          tryCatch({
            check_and_plot(mosaitoshape(), r = r$r, g = g$g, b = b$b, zlim = zlim$zlim)
          }, error = function(e) {
            message("An error occurred during plotting6: ", e$message)
          }, finally = {
            dev.off()
          })

          current_extent(ext(mosaitoshape()))
          session$sendCustomMessage("updateTiles_shp", list(
            img = base64enc::base64encode(original_image_path)
          ))
        })

        # Handle rectangle drawing and cropping
        observeEvent(input$drawn_rectangle_shp, {
          rect <- input$drawn_rectangle_shp
          req(rect$startX)
          if(rect$startX == rect$endX | rect$startY == rect$endY){
            return(NULL)
          }
          # Convert canvas coordinates to raster spatial extent
          xmin_val <- terra::xmin(current_extent())
          xmax_val <- terra::xmax(current_extent())
          ymin_val <- terra::ymin(current_extent())
          ymax_val <- terra::ymax(current_extent())
          fact_canva_rast_x <- input$canvas_size_shp$width / (xmax_val - xmin_val)
          fact_canva_rast_y <- input$canvas_size_shp$height / (ymax_val - ymin_val)
          xmin <- xmin_val + rect$startX / fact_canva_rast_x
          xmax <- xmin_val + rect$endX / fact_canva_rast_x
          ymin <- ymin_val + (input$canvas_size_shp$height - rect$endY) / fact_canva_rast_y
          ymax <- ymin_val + (input$canvas_size_shp$height - rect$startY) / fact_canva_rast_y
          current_extent(ext(c(xmin, xmax, ymin, ymax)))  # Update the extent
          xrange <- abs(xmax - xmin)
          originalres <- res(mosaitoshape())[[1]]
          newres <- max(c(xrange / 720, originalres))
          tfc <- file.path(tempdir(), "tempcropped.png")
          session$onSessionEnded(function() {
            if (file.exists(tfc)) {
              file.remove(tfc)
            }
          })
          if(terra::nlyr(mosaitoshape()) < 3){
            cropped_ras <- crop(mosaitoshape(), ext(xmin, xmax, ymin, ymax))
            sizes <- adjust_canvas(cropped_ras)
            wid(sizes[[1]])
            hei(sizes[[2]])
            png(tfc, width = wid(), height = hei())

            tryCatch({
              terra::plot(cropped_ras,
                          col = pliman::custom_palette(c("darkred", "yellow", "darkgreen"), n = 100),
                          maxcell = 1e6,
                          smooth = TRUE,
                          legend = "bottomleft",
                          mar = 0)
            }, error = function(e) {
              message("Error in plotting: ", e$message)
            }, finally = {
              dev.off()
            })

          } else{

            if (is.null(zlim$zlim) & length(points$data) == 0 & nchar(input$shapefiletoanalyze) == 0) {
              trycrop <-
                try(
                  suppressMessages(
                    suppressWarnings(
                      sf::gdal_utils(
                        util = "warp",
                        source = terra::sources(mosaitoshape()),
                        destination = tfc,
                        options = c(
                          strsplit(paste("-te", xmin, ymin, xmax, ymax), split = "\\s")[[1]],
                          "-tr", paste0(newres), paste0(newres),
                          "-overwrite"
                        )
                      )
                    )
                  )
                )
              if(inherits(trycrop, "try-error")){
                cropped_ras <- crop(mosaitoshape(), ext(xmin, xmax, ymin, ymax))
                sizes <- adjust_canvas(cropped_ras)
                wid(sizes[[1]])
                hei(sizes[[2]])
                png(tfc, width = wid(), height = hei())
                tryCatch({
                  check_and_plot(cropped_ras, r = r$r, g = g$g, b = b$b, zlim = zlim$zlim)
                }, error = function(e) {
                  message("Error in plotting: ", e$message)
                }, finally = {
                  dev.off()
                })

              } else{
                cropped_ras <- terra::rast(tfc)
                sizes <- adjust_canvas(cropped_ras)
                wid(sizes[[1]])
                hei(sizes[[2]])
              }

            } else {
              # Handle case when zlim is not NULL
              cropped_ras <- crop(mosaitoshape(), ext(xmin, xmax, ymin, ymax))
              sizes <- adjust_canvas(cropped_ras)
              wid(sizes[[1]])
              hei(sizes[[2]])
              png(tfc, width = wid(), height = hei())
              check_and_plot(cropped_ras, r = r$r, g = g$g, b = b$b, zlim = zlim$zlim)

              if(length(points$data) > 0 | nchar(input$shapefiletoanalyze) > 0){
                if (length(points$data) > 0) {
                  points_df <- do.call(rbind, points$data)
                  points_df <- as.data.frame(points_df, stringsAsFactors = FALSE)[, 1:2]

                  points(x = points_df[, 1],
                         y = points_df[, 2],
                         col = "red",
                         cex = 4,
                         pch = 10)

                  text(
                    x = points_df[, 1],
                    y = points_df[, 2],  # You can adjust the offset if needed
                    labels = 1:nrow(points_df),
                    col = "red",
                    cex = 2,
                    pos = 3
                  )
                }

                # If a shapefile has been selected, add it to the plot
                if (nchar(input$shapefiletoanalyze) > 0) {
                  if(input$fillid == "none"){
                    plot(
                      shapefile[[input$shapefiletoanalyze]]$data["plot_id"],
                      add = TRUE,
                      border = input$colorstroke,
                      lwd = input$lwdt,
                      col = input$colorfill
                    )
                  } else{
                    plot(
                      shapefile[[input$shapefiletoanalyze]]$data[input$fillid],
                      add = TRUE,
                      border = input$colorstroke,
                      lwd = input$lwdt,
                      col = input$colorfill
                    )
                  }


                  if (input$showplotid) {
                    centrs <- suppressWarnings(
                      suppressMessages(suppressWarnings(sf::st_centroid(shapefile[[input$shapefiletoanalyze]]$data) |> sf::st_coordinates()))
                    )
                    text(
                      x = centrs[, 1],
                      y = centrs[, 2],
                      labels = shapefile[[input$shapefiletoanalyze]]$data$plot_id,
                      col = "black",
                      cex = 1
                    )
                  }
                }
                if(!is.null(tmpshape$tmp) && nrow(tmpshape$tmp) > 0){
                  # Combine created shapes and temporary shape
                  alreadybuilt <- dplyr::bind_rows(reactiveValuesToList(createdshape), .id = "block")
                  if (nrow(alreadybuilt) > 0) {
                    shptoplot <- dplyr::bind_rows(alreadybuilt, tmpshape$tmp |> dplyr::mutate(block = "block_temp"))
                  } else {
                    shptoplot <- tmpshape$tmp
                  }
                  # Extract number from plot ID for plotting
                  shptoplot <- shptoplot |> extract_number(plot_id)
                  if(input$fillid == "none"){
                    plot(
                      shptoplot["plot_id"],
                      add = TRUE,
                      border = input$colorstroke,
                      lwd = input$lwdt,
                      col = input$colorfill
                    )
                  } else{
                    plot(
                      shptoplot[input$fillid],
                      add = TRUE,
                      border = input$colorstroke,
                      lwd = input$lwdt
                    )
                  }


                  # Add text labels at centroids if enabled
                  if (input$showplotid) {
                    centrs <- suppressMessages(sf::st_centroid(shptoplot) |> sf::st_coordinates())
                    text(x = centrs[, 1], y = centrs[, 2], labels = shptoplot$plot_id, col = "black", cex = 1)
                  }
                }
              }

              dev.off()
            }

          }

          session$sendCustomMessage("updateTiles_shp", list(
            img = base64enc::base64encode(tfc)
          ))

          session$sendCustomMessage("adjustcanvas_shpSize", list(
            width = as.integer(wid()),
            height = as.integer(hei())
          ))
        })



        # Reactive values to store points
        points <- reactiveValues(data = list())


        # Handle point selection input
        observeEvent(input$picked_point_shp, {
          point <- input$picked_point_shp
          req(point)
          xmin_val <- terra::xmin(current_extent())
          xmax_val <- terra::xmax(current_extent())
          ymin_val <- terra::ymin(current_extent())
          ymax_val <- terra::ymax(current_extent())
          canvas_width <- input$canvas_size_shp$width
          canvas_height <- input$canvas_size_shp$height
          x_canvas <- point[1]
          y_canvas <- point[2]
          x_raster <- xmin_val + (x_canvas / canvas_width) * (xmax_val- xmin_val)
          y_raster <- ymin_val + ((canvas_height - y_canvas) / canvas_height) * (ymax_val - ymin_val)
          coords <- data.frame(xmim = x_raster, ymin = y_raster, edited = FALSE)
          # # Add the point to the list of points
          points$data <- append(points$data, list(coords))

        })

        output$show_panel2 <- reactive({
          length(points$data) > 0
        })
        outputOptions(output, "show_panel2", suspendWhenHidden = FALSE)

        output$points_table <- DT::renderDT({
          if (length(points$data) > 0) {
            points_df <- isolate(do.call(rbind, points$data))
            points_df <- as.data.frame(points_df, stringsAsFactors = FALSE) |> pliman::round_cols(digits = 2)
            colnames(points_df) <- c("xcoord", "ycoord", "edited")

            # Add Edit/Delete buttons
            points_df <- points_df |>
              transform(
                Edit = sprintf('<button class="edit-btn" id="edit_point_%s">✏️</button>', seq_len(nrow(points_df))),
                Delete = sprintf('<button class="delete-point-btn" id="delete_point_%s">🗑️</button>', seq_len(nrow(points_df)))
              )
            rownames(points_df) <- NULL
            DT::datatable(
              points_df,
              escape = FALSE,
              selection = "none",
              options = list(
                dom = "t",
                paging = FALSE,
                columnDefs = list(
                  # Hide the "edited" column. Adjust the index because DT is zero-indexed.
                  list(visible = FALSE, targets = which(names(points_df) == "edited"))
                )
              ),
              callback = DT::JS(sprintf("
      table.on('click', '.delete-point-btn', function() {
        var id = $(this).attr('id');
        Shiny.setInputValue('%s', {id: id}, {priority: 'event'});
      });
      table.on('click', '.edit-btn', function() {
        var id = $(this).attr('id');
        Shiny.setInputValue('%s', {id: id}, {priority: 'event'});
      });
    ", ns("delete_point_click"), ns("edit_point_click")))
            ) |> DT::formatStyle(
              'edited',
              target = 'row',
              backgroundColor = DT::styleEqual(c(TRUE), c('#f0f0f0')),
              color = DT::styleEqual(c(TRUE), c('gray')),
              fontStyle = DT::styleEqual(c(TRUE), c('italic')),
              textDecoration = DT::styleEqual(c(TRUE), c('line-through'))
            )


          }
        })

        observeEvent(input$delete_point_click, {
          btn_info <- input$delete_point_click
          row_id <- as.numeric(strsplit(btn_info$id, "_")[[1]][3])
          points$data <- points$data[-row_id]
        })


        row_id <- reactiveVal()
        observeEvent(input$edit_point_click, {
          btn_info <- input$edit_point_click
          row_id(as.numeric(strsplit(btn_info$id, "_")[[1]][3]))
          points$data[[row_id()]]["edited"] <- TRUE
          editedpoint <- points$data[[row_id()]]
          output$show_panel <- reactive({
            TRUE
          })

          # Ensure the reactive output is available to the UI
          outputOptions(output, "show_panel", suspendWhenHidden = FALSE)

          x <- editedpoint[[1]]
          y <- editedpoint[[2]]
          extedit <- ext(x - 4, x + 4, y - 4, y + 4)
          current_extent(extedit)
          cropped_ras <- crop(mosaitoshape(), extedit)
          # Create an extent with a 2 m buffer around the point
          sizes <- adjust_canvas(cropped_ras)
          wid(sizes[[1]])
          hei(sizes[[2]])
          # Generate the cropped image
          cropped_image_path <- paste(tempdir(), "croppedimage.png")
          png(cropped_image_path, width = wid(), height = hei())

          tryCatch({
            check_and_plot(cropped_ras, r = r$r, g = g$g, b = b$b, zlim = zlim$zlim)

            points(x = x, y = y, col = "red", cex = 5, pch = 13)

            text(
              x = x,
              y = y,  # Apply dynamic offset if needed
              labels = row_id(),
              col = "black",
              cex = 2,
              pos = 3
            )
          }, error = function(e) {
            message("An error occurred during plotting2: ", e$message)
          }, finally = {
            dev.off()
          })


          # Send the cropped image to the client
          session$sendCustomMessage("updateTiles_shp", list(
            img = base64enc::base64encode(cropped_image_path)
          ))
          # Send the adjusted canvas size to the client
          session$sendCustomMessage("adjustcanvas_shpSize", list(
            width = as.integer(wid()),
            height = as.integer(hei())
          ))
        })



        observeEvent(input$editiondone, {
          points$data[[row_id()]][[1]] <- points$data[[length(points$data)]][[1]]
          points$data[[row_id()]][[2]] <- points$data[[length(points$data)]][[2]]
          points$data[[row_id()]][[3]] <- FALSE
          points$data[[length(points$data)]] <- NULL
          output$show_panel <- reactive({
            FALSE
          })
        })


        ####### CLEAR POINTS ##########
        observeEvent(input$clearpoints, {
          points$data <- list()
          polygeom(NULL)
          session$sendCustomMessage("updateTiles_shp", list(
            img = base64enc::base64encode(original_image_path)
          ))
          # Send the adjusted canvas size to the client
          session$sendCustomMessage("adjustcanvas_shpSize", list(
            width = widori(),
            height = heiori()
          ))
        })




        ########## CREATE SHAPEFILES ############
        # Define reactive values globally
        tmpshape <- reactiveValues(tmp = NULL)
        createdshape <- reactiveValues()
        nblock <- reactiveVal(0)

        # 1. Shape Creation Logic (no nested observers)
        observeEvent(input$createupdate2, {
          if(length(points$data) < 3){
            sendSweetAlert(
              session = session,
              title = "No control points available",
              text = "You need to insert at least three points to build a shapefile.",
              type = "error"
            )
            return(NULL)
          }
          showNotification(
            ui = tagList(
              tags$i(class = "fa fa-spinner fa-spin"), " Building plots at lightning speed..."
            ),
            type = "message",
            duration = NULL,  # Remains until manually removed
            id = "buildingplot"
          )
          pdata <- isolate(do.call(rbind, points$data))
          contrpoints <- sf::st_as_sf(vect(as.matrix(rbind(pdata, pdata[1, ])[, 1:2]), type = "polygon"))
          sf::st_crs(contrpoints) <- sf::st_crs(mosaitoshape())
          # # create the shapes
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
          # Build shape and store it temporarily
          shpt <-
            shapefile_build(
              basemap = FALSE,
              mosaitoshape(),
              controlpoints = contrpoints,
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
          tmpshape$tmp <- shpt
          shapefile[["shapefileplot"]] <- shpt
        })


        # 2. Handle Block Completion and Save Shapes
        observeEvent(input$doneblock, {
          if(input$buildblocks){
            req(tmpshape$tmp)

            nblock(nblock() + 1)
            block_name <- paste0("B", sprintf("%02d", nblock()))
            createdshape[[block_name]] <- tmpshape$tmp

            output$nblocksdone <- renderText({
              glue::glue("Built blocks: {nblock()}")
            })

            sendSweetAlert(
              session = session,
              title = "Block built",
              text = "The shapes in the current block have been built. Go to 'Control points' tab, clear the points and draw the controlpoints for another block, if needed.",
              type = "info"
            )
          }
        })

        # 3. Handle Final Shape Plotting and Canvas Update
        observe({
          req(tmpshape$tmp, mosaitoshape())

          # Adjust canvas size
          sizes <- adjust_canvas(mosaitoshape())
          png(basepolygon, width = sizes[[1]], height = sizes[[2]])

          tryCatch({
            # Plot the base shape
            check_and_plot(mosaitoshape(), r = r$r, g = g$g, b = b$b, zlim = zlim$zlim)

            # Combine created shapes and temporary shape
            alreadybuilt <- dplyr::bind_rows(reactiveValuesToList(createdshape), .id = "block")
            if (nrow(alreadybuilt) > 0) {
              shptoplot <- dplyr::bind_rows(alreadybuilt, tmpshape$tmp |> dplyr::mutate(block = "block_temp"))
            } else {
              shptoplot <- tmpshape$tmp
            }
            # Extract number from plot ID for plotting
            shptoplot <- shptoplot |> extract_number(plot_id)
            if(input$fillid == "none"){
              plot(
                shptoplot["plot_id"],
                add = TRUE,
                border = input$colorstroke,
                lwd = input$lwdt,
                col = input$colorfill
              )
            } else{
              plot(
                shptoplot[input$fillid],
                add = TRUE,
                border = input$colorstroke,
                lwd = input$lwdt
              )
            }

            # Add text labels at centroids if enabled
            if (input$showplotid) {
              centrs <- suppressMessages(sf::st_centroid(shptoplot) |> sf::st_coordinates())
              text(x = centrs[, 1], y = centrs[, 2], labels = shptoplot$plot_id, col = "black", cex = 1)
            }

          }, error = function(e) {
            message("An error occurred during plotting3: ", e$message)
          }, finally = {
            dev.off()
          })

          # Update the canvas with the new image
          session$sendCustomMessage("updateTiles_shp", list(img = base64enc::base64encode(basepolygon)))
          removeNotification(id = "buildingplot")
        })





        observeEvent(input$shapedone,{
          if(input$shapedone){
            req(input$shapenamebuild)
            req(createdshape)
            shptemp <- reactiveValuesToList(createdshape)
            if(length(shptemp) == 0){
              shptemp <- shapefile[["shapefileplot"]]
            } else{
              shptemp <-
                shptemp |>
                dplyr::bind_rows(.id = "block") |>
                dplyr::mutate(unique_id = dplyr::row_number(), .before = 1)

            }
            if(nrow(shptemp) == 0){
              show_alert(
                "Oops, something went wrong!",
                text = "The shapefile appears to be empty. Did you forget to click 'Create/Update'?",
                type = "error"
              )
              return()
            } else{
              observe({
                shapefilenames <- setdiff(c("none", names(shapefile)), c("shapefile", "shapefileplot"))
                shapefile[[input$shapenamebuild]] <- create_reactval(input$shapenamebuild, shptemp)
                # Update selectInput choices
                updateSelectInput(session, "shapefiletoanalyze",
                                  choices = shapefilenames,
                                  selected = shapefilenames[[length(shapefilenames)]])
              })
              sendSweetAlert(
                session = session,
                title = "Shapefile built",
                text = "The shapefile has been successfully built. You can now download or use it for further analysis.",
                type = "success"
              )
            }
          }
        })




        # RESET VIEW
        observeEvent(c(input$reset_view_shp, input$delete_point_click, input$editiondone), {
          wid(widori())
          hei(heiori())
          current_extent(ext(mosaitoshape()))  # Reset extent to the full raster
          if (length(points$data) > 0) {
            sizes <- adjust_canvas(mosaitoshape())
            png(basepolygon, width = sizes[[1]], height = sizes[[2]])

            tryCatch({
              check_and_plot(mosaitoshape(), r = r$r, g = g$g, b = b$b, zlim = zlim$zlim)

              points_df <- do.call(rbind, points$data)
              points_df <- as.data.frame(points_df)

              # Plot the points
              points(x = points_df[, 1],
                     y = points_df[, 2],
                     col = "red",
                     cex = 2,
                     pch = 16)

              # Label the points
              text(
                x = points_df[, 1],
                y = points_df[, 2],  # Apply dynamic offset if necessary
                labels = 1:nrow(points_df),
                col = "black",
                cex = 2,
                pos = 3
              )
              # Optionally add a shapefile layer if specified
              if (nchar(input$shapefiletoanalyze) > 0) {
                if(input$fillid == "none"){
                  plot(
                    shapefile[[input$shapefiletoanalyze]]$data["plot_id"],
                    add = TRUE,
                    border = input$colorstroke,
                    lwd = input$lwdt,
                    col = input$colorfill
                  )
                } else{
                  plot(
                    shapefile[[input$shapefiletoanalyze]]$data[input$fillid],
                    add = TRUE,
                    border = input$colorstroke,
                    lwd = input$lwdt
                  )
                }

                if (input$showplotid) {
                  centrs <- suppressMessages(
                    sf::st_centroid(shapefile[[input$shapefiletoanalyze]]$data) |> sf::st_coordinates()
                  )
                  text(
                    x = centrs[, 1],
                    y = centrs[, 2],
                    labels = shapefile[[input$shapefiletoanalyze]]$data$plot_id,
                    col = "black",
                    cex = 1
                  )
                }
              }
              # Combine created shapes and temporary shape
              alreadybuilt <- dplyr::bind_rows(reactiveValuesToList(createdshape), .id = "block")
              if (nrow(alreadybuilt) > 0) {
                shptoplot <- dplyr::bind_rows(alreadybuilt, tmpshape$tmp |> dplyr::mutate(block = "block_temp"))
                # Extract number from plot ID for plotting
                shptoplot <- shptoplot |> extract_number(plot_id)
                if(input$fillid == "none"){
                  plot(
                    shptoplot["plot_id"],
                    add = TRUE,
                    border = input$colorstroke,
                    lwd = input$lwdt,
                    col = input$colorfill
                  )
                } else{
                  plot(
                    shptoplot[input$fillid],
                    add = TRUE,
                    border = input$colorstroke,
                    lwd = input$lwdt
                  )
                }

                # Add text labels at centroids if enabled
                if (input$showplotid) {
                  centrs <- suppressMessages(sf::st_centroid(shptoplot) |> sf::st_coordinates())
                  text(x = centrs[, 1], y = centrs[, 2], labels = shptoplot$plot_id, col = "black", cex = 1)
                }
              }
              if (nrow(tmpshape$tmp) > 0) {
                shptoplot <- dplyr::bind_rows(tmpshape$tmp |> dplyr::mutate(block = "block_temp"))
                # Extract number from plot ID for plotting
                shptoplot <- shptoplot |> extract_number(plot_id)
                if(input$fillid == "none"){
                  plot(
                    shptoplot["plot_id"],
                    add = TRUE,
                    border = input$colorstroke,
                    lwd = input$lwdt,
                    col = input$colorfill
                  )
                } else{
                  plot(
                    shptoplot[input$fillid],
                    add = TRUE,
                    border = input$colorstroke,
                    lwd = input$lwdt
                  )
                }


                # Add text labels at centroids if enabled
                if (input$showplotid) {
                  centrs <- suppressMessages(sf::st_centroid(shptoplot) |> sf::st_coordinates())
                  text(x = centrs[, 1], y = centrs[, 2], labels = shptoplot$plot_id, col = "black", cex = 1)
                }
              }

            }, error = function(e) {
              # Handle the error (you can log it, notify the user, etc.)
              message("An error occurred during plotting4: ", e$message)
            }, finally = {
              # Ensure that the graphics device is closed no matter what
              dev.off()
            })

            session$sendCustomMessage("updateTiles_shp", list(
              img = base64enc::base64encode(basepolygon)
            ))
          } else{
            session$sendCustomMessage("updateTiles_shp", list(
              img = base64enc::base64encode(original_image_path)
            ))
          }
          session$sendCustomMessage("adjustcanvas_shpSize", list(
            width = as.integer(widori()),
            height = as.integer(heiori())
          ))
        })
      }
    })


    # observe if example shapefile is used
    # shapefile created using a sample raster provided by Arthur Bernardeli https://www.linkedin.com/in/arthur-bernardeli-5a1a0b5a/
    observeEvent(input$inputshptypenat, {
      if(input$inputshptypenat == "example shapefile"){
        filepath <- file.path(system.file(package = "plimanshiny"), "app/www/soy_shape.rds")
        shapefile[["example_shp"]] <- create_reactval("example_shp", shapefile_input(filepath, info = FALSE))
        mosaicnames <-  setdiff(names(shapefile), c("mosaic", "shapefileplot"))
        updateSelectInput(session, "shapefiletoanalyze",
                          choices = mosaicnames,
                          selected = mosaicnames[[1]])


        observe({
          req(input$shapefiletoanalyze)
          req(shapefile[[input$shapefiletoanalyze]]$data)
          req(activemosaic$name)
          req(mosaic_data[[activemosaic$name]]$data)
          sizes <- adjust_canvas(mosaic_data[[activemosaic$name]]$data)

          original_image_path <- file.path(tempdir(), "originalimage.png")
          png(original_image_path, width = sizes[[1]], height = sizes[[2]])

          tryCatch({
            if (!is.null(mosaitoshape())) {
              check_and_plot(mosaitoshape(), r = r$r, g = g$g, b = b$b, zlim = zlim$zlim)
              centrs <- suppressMessages(
                sf::st_centroid(shapefile[[input$shapefiletoanalyze]]$data) |> sf::st_coordinates()
              )
              plot(
                shapefile[[input$shapefiletoanalyze]]$data["plot_id"],
                add = TRUE,
                border = "red",
                lwd = input$lwdt,
                col = input$colorfill
              )
              text(
                x = centrs[, 1],
                y = centrs[, 2],
                labels = shapefile[[input$shapefiletoanalyze]]$data$plot_id,
                col = "black",
                cex = 1
              )

            } else {
              if (input$fillid == "none") {
                plot(shapefile[[input$shapefiletoanalyze]]$data)
              } else {
                plot(shapefile[[input$shapefiletoanalyze]]$data[input$fillid])
              }
            }
          }, error = function(e) {
            message("An error occurred during plotting5: ", e$message)
          }, finally = {
            dev.off()
          })

          session$sendCustomMessage("updateTiles_shp", list(
            img = base64enc::base64encode(original_image_path)
          ))
        })

      }
    })








    ############################################# IMPORT A SHAPEFILE ##########################################################
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
            new_shp_name <- sapply(pathshape$file$datapath, file_name)
            # Check if the mosaic already exists in mosaic_data
            if (any(new_shp_name %in% names(shapefile))) {
              # If it exists, update the existing reactiveValues
              moname <- new_shp_name[new_shp_name %in% names(shapefile)]
              ask_confirmation(
                inputId = "confirmmosaicname",
                type = "warning",
                title = "Shapefile already imported",
                text = paste0("The object '", paste0(moname, collapse = ", "), "' is already available in the list of imported shapefiles. Do you really want to overwrite it?"),
                btn_labels = c("Nope", "Yep"),
                btn_colors = c("#FE642E", "#04B404")
              )
              observe({
                if (!is.null(input$confirmmosaicname)) {
                  if (input$confirmmosaicname) {
                    for (i in 1:length(new_shp_name)) {
                      shapefile[[new_shp_name[[i]]]] <- create_reactval(new_shp_name[[i]], shapefile_input(pathshape$file$datapath[[i]], info = FALSE, multilinestring = input$multiline))
                    }
                  } else {
                    return()
                  }
                }
              })
            } else {
              # If it doesn't exist, create a new reactiveValues and add it to mosaic_data
              for (i in 1:length(new_shp_name)) {
                shapefile[[new_shp_name[[i]]]] <- create_reactval(new_shp_name[[i]], shapefile_input(pathshape$file$datapath[[i]], info = FALSE, multilinestring = input$multiline))
              }
            }

            observe({
              mosaicnames <-  setdiff(names(shapefile), c("mosaic", "shapefileplot"))
              updateSelectInput(session, "shapefiletoanalyze",
                                choices = mosaicnames,
                                selected = mosaicnames[[1]])
              removeNotification(id = "importshp")

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
          req(input$shapefiletoanalyze)  # Ensure mosaic_data$mosaic$data is not NULL

          updateSelectInput(session, "colorshapeimport", choices = c("none", names(shapefile[[input$shapefiletoanalyze]]$data)))
          updateSelectInput(session, "fillid", choices = c("none", names(shapefile[[input$shapefiletoanalyze]]$data)))

          if(!is.null(mosaic_data$mosaic$data) & input$shapefiletoanalyze != "none"){
            if(sf::st_crs(shapefile[[input$shapefiletoanalyze]]$data) != sf::st_crs(mosaic_data$mosaic$data)){
              sendSweetAlert(
                session = session,
                title = "Invalid CRS",
                text = "The Coordinate Reference System (CRS) of the shapefile does
            not match the input mosaic. Trying to set the shapefile's CRS to match the mosaic one.",
                type = "warning"
              )
              shp <- shapefile[[input$shapefiletoanalyze]]$data |> sf::st_transform(crs = sf::st_crs(mosaic_data$mosaic$data))
              shapefile[[input$shapefiletoanalyze]] <- create_reactval(input$shapefiletoanalyze, shp)
            }
          }
        })

        observe({
          req(input$shapefiletoanalyze)
          req(shapefile[[input$shapefiletoanalyze]]$data)
          req(mosaic_data[[activemosaic$name]]$data)
          sizes <- adjust_canvas(mosaic_data[[activemosaic$name]]$data)

          original_image_path <- file.path(tempdir(), "originalimage.png")
          png(original_image_path, width = sizes[[1]], height = sizes[[2]])

          tryCatch({
            if (!is.null(mosaitoshape())) {
              check_and_plot(mosaitoshape(), r = r$r, g = g$g, b = b$b, zlim = zlim$zlim)

              if(input$fillid == "none"){
                plot(
                  shapefile[[input$shapefiletoanalyze]]$data["plot_id"],
                  add = TRUE,
                  lwd = input$lwdt,
                  col = input$colorfill
                )
              } else{
                plot(
                  shapefile[[input$shapefiletoanalyze]]$data[input$fillid],
                  add = TRUE,
                  border = input$colorstroke,
                  lwd = input$lwdt
                )
              }
            } else {
              if (input$fillid == "none") {
                plot(shapefile[[input$shapefiletoanalyze]]$data)
              } else {
                plot(shapefile[[input$shapefiletoanalyze]]$data[input$fillid])
              }
            }
          }, error = function(e) {
            message("An error occurred during plotting5: ", e$message)
          }, finally = {
            dev.off()
          })

          session$sendCustomMessage("updateTiles_shp", list(
            img = base64enc::base64encode(original_image_path)
          ))
        })
      }
    })








    ############################################# DOWNLOAD SHAPEFILE ##########################################################
    observe({
      req(input$shapefiletoanalyze)
      if(input$shapefiletoanalyze == "none"){
        shapefile[["shapefileplot"]] <- NULL
      } else{
        shapefile[["shapefileplot"]] <- shapefile[[input$shapenamebuild]]$data
      }
    })
    mod_download_shapefile_server("downloadshapefile2", terra::vect(shapefile[["shapefileplot"]]), name = "created_shp")


    ############################################# PLOT INFO ##########################################################
    observeEvent(c(input$plotinfonat, input$plotinfo2nat), {
      mos <- mosaitoshape()
      distsss <- reactiveValues()
      perim <- reactiveValues()
      area <- reactiveValues()
      wid <- reactiveValues()
      hei <- reactiveValues()
      req(shapefile$shapefileplot)
      if(input$plotinfonat | input$plotinfo2nat){
        updateSelectizeInput(session, "uniqueidinfo",
                             choices = 1:nrow(shapefile$shapefileplot),
                             server = TRUE)
        output$plotinfop <- renderPlot({
          shpinfo <- shapefile$shapefileplot[input$uniqueidinfo, ]
          npoints <- sf::st_coordinates(shpinfo) |> nrow()
          coords <- sf::st_coordinates(shpinfo)[, 1:2]
          buff <- diff(range(coords[, 1])) * 0.15
          measures <- shapefile_measures(shpinfo, n = 1)
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
            check_and_plot(mcro, r = r$r, g = g$g, b = b$b, zlim = zlim$zlim)
            plot(shpinfo[ifelse(input$fillid == "none", "plot_id", input$fillid)], add = TRUE, border = "salmon", lwd = input$lwdt, col = rgb(250/255, 128/255, 114/255, alpha = 0.5))
          } else{
            plot(shpinfo[ifelse(input$fillid == "none", "plot_id", input$fillid)], add = TRUE, border = "salmon", lwd = input$lwdt, col = rgb(250/255, 128/255, 114/255, alpha = 0.5))
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
                selectizeInput(ns("uniqueidinfo"),
                               label = "Unique id",
                               choices = NULL,
                               options = list(maxOptions = 50000),
                               width = "100%"),
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



  })
}
