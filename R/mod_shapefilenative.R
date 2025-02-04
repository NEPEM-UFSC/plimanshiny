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
          id = "tabsshape",
          width = 12,
          collapsible = FALSE,
          status = "success",
          selected = "Control Points",
          solidHeader = FALSE,
          type = "tabs",
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
            title = "Plots",
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
                      ns("plotinfo"),
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
                  # shinyFilesButton(id=ns("shapefileinput"),
                  #                  label="Shapefile",
                  #                  title="Shapefile",
                  #                  buttonType = "primary",
                  #                  multiple = TRUE,
                  #                  class = NULL,
                  #                  icon = icon("magnifying-glass"),
                  #                  style = NULL)
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
                  ns("plotinfo2"),
                  label = "Plot info",
                  icon = icon("info"),
                  color = "success",
                  style = "jelly"
                )
              )
            ),
            selectInput(ns("shapefiletoanalyze"),
                        label = "Active Shapefile",
                        choices = NULL)
          ),
          tabPanel(
            title = "Download",
            mod_download_shapefile_ui(ns("downloadshapefile2"), label = "Download")
          )

        )
      ),
      col_9(
        bs4TabCard(
          id = "tabsshape2",
          width = 12,
          collapsible = FALSE,
          status = "success",
          selected = "Pliman viewer",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel("Pliman viewer",

                   tagList(
                     tags$head(
                       tags$style(HTML("
        #rasterCanvas {
          margin-top: 20px;
          border: 1px solid #ddd;
          box-shadow: 3px 3px 8px rgba(0, 0, 0, 0.3);
        }
      ")),
                       tags$script(HTML(sprintf("
        let canvas, ctx, drawing = false;
        let rectStartX, rectStartY, rectEndX, rectEndY;
        let selectedPoints = [];
        let rasterImage = null;
        let canvasWidth = 1280;
        let canvasHeight = 720;

        function initCanvas() {
          canvas = document.getElementById('%s');
          ctx = canvas.getContext('2d');
          canvas.width = canvasWidth;
          canvas.height = canvasHeight;

          canvas.addEventListener('mousedown', handleMouseDown);
          canvas.addEventListener('mousemove', handleMouseMove);
          canvas.addEventListener('mouseup', handleMouseUp);
          canvas.addEventListener('dblclick', handleDoubleClick);
        }

        function adjustCanvasSize(width, height) {
          canvasWidth = width;
          canvasHeight = height;
          canvas.width = canvasWidth;
          canvas.height = canvasHeight;
          Shiny.setInputValue('%s', { width: canvasWidth, height: canvasHeight }, { priority: 'event' });
          drawCanvas();
        }

        function handleMouseDown(e) {
          const rect = canvas.getBoundingClientRect();
          rectStartX = e.clientX - rect.left;
          rectStartY = e.clientY - rect.top;
          timeoutID = setTimeout(() => {
            drawPoint(rectStartX, rectStartY);
            selectedPoints.push({ x: rectStartX, y: rectStartY });
            Shiny.setInputValue('%s', [rectStartX, rectStartY]);
          }, 1000);
          drawing = true;
        }

        function handleMouseMove(e) {
          if (!drawing) return;
          clearTimeout(timeoutID);
          const rect = canvas.getBoundingClientRect();
          rectEndX = e.clientX - rect.left;
          rectEndY = e.clientY - rect.top;
          drawCanvas();
          drawRectangle(rectStartX, rectStartY, rectEndX, rectEndY);
        }

        function handleMouseUp() {
          clearTimeout(timeoutID);
          drawing = false;
          Shiny.setInputValue('%s', {
            startX: Math.min(rectStartX, rectEndX),
            startY: Math.min(rectStartY, rectEndY),
            endX: rectEndX,
            endY: rectEndY,
            width: Math.abs(rectEndX - rectStartX),
            height: Math.abs(rectEndY - rectStartY)
          });
          rectStartX = rectStartY = rectEndX = rectEndY = 0;
        }

        function drawPoint(x, y) {
          if (!ctx) return;
          ctx.strokeStyle = 'red';
          ctx.lineWidth = 2;
          ctx.beginPath();
          ctx.arc(x, y, 10, 0, 2 * Math.PI);
          ctx.stroke();
          ctx.beginPath();
          ctx.moveTo(x - 10, y);
          ctx.lineTo(x + 10, y);
          ctx.moveTo(x, y - 10);
          ctx.lineTo(x, y + 10);
          ctx.stroke();
        }

        function drawRectangle(x1, y1, x2, y2) {
          ctx.strokeStyle = 'red';
          ctx.lineWidth = 2;
          ctx.strokeRect(x1, y1, x2 - x1, y2 - y1);
        }

        function drawCanvas() {
          ctx.clearRect(0, 0, canvas.width, canvas.height);
          drawRaster();
        }

        function drawRaster() {
          if (rasterImage) {
            ctx.drawImage(rasterImage, 0, 0, canvas.width, canvas.height);
          }
        }

        Shiny.addCustomMessageHandler('updateTiles', function(data) {
          rasterImage = new Image();
          rasterImage.src = 'data:image/png;base64,' + data.img;
          rasterImage.onload = drawCanvas;
        });

        Shiny.addCustomMessageHandler('adjustCanvasSize', function(data) {
          adjustCanvasSize(data.width, data.height);
        });

        function handleDoubleClick() {
          Shiny.setInputValue('%s', new Date().getTime());
        }

        window.addEventListener('load', initCanvas);
      ", ns("rasterCanvas"), ns("canvas_size"), ns("picked_point"), ns("drawn_rectangle"), ns("reset_view"))))
                     ),
                     tags$canvas(id = ns("rasterCanvas"))
                   )
          )
        )


      )
    )
  )
}

#' shapefilenative Server Functions
#'
mod_shapefilenative_server <- function(id, r, g, b, shapefile) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # req(activemosaic$name)
    # print(activemosaic)
    # mosaic <- mosaic_data[[activemosaic$name]]$data
    mosaic <- terra::rast("D:/dsm.tif")
    ############################################# BUILD A SHAPEFILE ##########################################################
    needstretch <- reactiveVal(FALSE)
    original_image_path <- file.path(tempdir(), "originalimage.png")
    basepolygon <- file.path(tempdir(), "imagewithpolygon.png")
    original_ext <- reactiveVal(ext(mosaic))
    polygeom <- reactiveVal()
    createdshape <- reactiveValues()
    tmpshape <- reactiveValues()

    # Reactive value to store the current cropped extent
    current_extent <- reactiveVal(ext(mosaic))
    wid <- reactiveVal()
    hei <- reactiveVal()
    widori <- reactiveVal()
    heiori <- reactiveVal()

    observe({
      sizes <- adjust_canvas(mosaic)
      widori(sizes[[1]])
      heiori(sizes[[2]])
      # Send the adjusted canvas size to the client
      session$sendCustomMessage("adjustCanvasSize", list(
        width = as.integer(widori()),
        height = as.integer(heiori())
      ))
    })

    # Generate and cache the original image on startup
    observe({
      sizes <- adjust_canvas(mosaic)
      png(original_image_path, width = sizes[[1]], height = sizes[[2]])
      check_and_plot(mosaic, r = r$r, g = g$g, b = b$b)
      dev.off()
      current_extent(ext(mosaic))
      session$sendCustomMessage("updateTiles", list(
        img = base64enc::base64encode(original_image_path)
      ))
    })

    # Handle rectangle drawing and cropping
    observeEvent(input$drawn_rectangle, {
      rect <- input$drawn_rectangle
      req(rect$startX)
      if(rect$startX == rect$endX | rect$startY == rect$endY){
        return(NULL)
      }
      # Convert canvas coordinates to raster spatial extent
      xmin_val <- terra::xmin(current_extent())
      xmax_val <- terra::xmax(current_extent())
      ymin_val <- terra::ymin(current_extent())
      ymax_val <- terra::ymax(current_extent())
      fact_canva_rast_x <- input$canvas_size$width / (xmax_val - xmin_val)
      fact_canva_rast_y <- input$canvas_size$height / (ymax_val - ymin_val)
      xmin <- xmin_val + rect$startX / fact_canva_rast_x
      xmax <- xmin_val + rect$endX / fact_canva_rast_x
      ymin <- ymin_val + (input$canvas_size$height - rect$endY) / fact_canva_rast_y
      ymax <- ymin_val + (input$canvas_size$height - rect$startY) / fact_canva_rast_y
      current_extent(ext(c(xmin, xmax, ymin, ymax)))  # Update the extent
      xrange <- abs(xmax - xmin)
      originalres <- res(mosaic)[[1]]
      newres <- max(c(xrange / 720, originalres))
      # Crop the raster and update the current extent
      tfc <- file.path(tempdir(), "tempcropped.png")
      session$onSessionEnded(function() {
        if (file.exists(tfc)) {
          file.remove(tfc)
        }
      })
      if(terra::nlyr(mosaic) < 3){
        cropped_ras <- crop(mosaic, ext(xmin, xmax, ymin, ymax))
        sizes <- adjust_canvas(cropped_ras)
        wid(sizes[[1]])
        hei(sizes[[2]])
        png(tfc, width = wid(), height = hei())
        # print(marg())
        terra::plot(cropped_ras,
                    col = pliman::custom_palette(c("darkred",  "yellow", "darkgreen"), n = 100),
                    maxcell = 1e6,
                    smooth = TRUE,
                    legend = "bottomleft",
                    mar = 0)
        dev.off()
      } else{
        trycrop <-
          try(
            suppressMessages(
              suppressWarnings(
                sf::gdal_utils(
                  util = "warp",
                  source = terra::sources(mosaic),
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
          cropped_ras <- crop(mosaic, ext(xmin, xmax, ymin, ymax))
          sizes <- adjust_canvas(cropped_ras)
          wid(sizes[[1]])
          hei(sizes[[2]])
          png(tfc, width = wid(), height = hei())
          check_and_plot(mosaic, r = r$r, g = g$g, b = b$b)
          dev.off()
        } else{
          cropped_ras <- rast(tfc)
          sizes <- adjust_canvas(cropped_ras)
          wid(sizes[[1]])
          hei(sizes[[2]])
        }
      }


      # Generate the cropped image
      if(length(points$data) > 0 | nchar(input$shapefiletoanalyze) > 0){
        cropped_image_path <- paste(tempdir(), "croppedimage.png")
        png(cropped_image_path, width = wid(), height = hei())
        check_and_plot(cropped_ras, r = r$r, g = g$g, b = b$b)
        if (length(points$data) > 0) {
          points_df <- do.call(rbind, points$data)
          points_df <- as.data.frame(points_df, stringsAsFactors = FALSE)[,1:2]
          points(x = points_df[, 1],
                 y = points_df[, 2],
                 col = "red",
                 cex=4,
                 pch=10)
          text(
            x = points_df[, 1],
            y = points_df[, 2],  # Apply dynamic offset
            labels = 1:nrow(points_df),
            col = "red",
            cex = 2,
            pos = 3
          )
        }
        # shapefile[[input$shapefiletoanalyze]]$data
        if(nchar(input$shapefiletoanalyze) > 0){
          plot(shapefile[[input$shapefiletoanalyze]]$data["plot_id"], add = TRUE, border = "red", lwd = 3, col = rgb(0, 0, 1, alpha = 0))
          if(input$showplotid){
            centrs <- suppressWarnings(sf::st_centroid(shapefile[[input$shapefiletoanalyze]]$data) |> sf::st_coordinates())
            text(
              x = centrs[, 1],
              y = centrs[, 2],
              labels = shapefile[[input$shapefiletoanalyze]]$data$plot_id,
              col = "black",
              cex = 1,
            )
          }
        }
        dev.off()
        session$sendCustomMessage("updateTiles", list(
          img = base64enc::base64encode(cropped_image_path)
        ))
      } else{
        session$sendCustomMessage("updateTiles", list(
          img = base64enc::base64encode(tfc)
        ))
      }
      session$sendCustomMessage("adjustCanvasSize", list(
        width = as.integer(wid()),
        height = as.integer(hei())
      ))
    })



    # Reactive values to store points
    points <- reactiveValues(data = list())


    # Handle point selection input
    observeEvent(input$picked_point, {
      point <- input$picked_point
      req(point)
      xmin_val <- terra::xmin(current_extent())
      xmax_val <- terra::xmax(current_extent())
      ymin_val <- terra::ymin(current_extent())
      ymax_val <- terra::ymax(current_extent())
      canvas_width <- input$canvas_size$width
      canvas_height <- input$canvas_size$height
      x_canvas <- point[1]
      y_canvas <- point[2]
      x_raster <- xmin_val + (x_canvas / canvas_width) * (xmax_val- xmin_val)
      y_raster <- ymin_val + ((canvas_height - y_canvas) / canvas_height) * (ymax_val - ymin_val)
      coords <- data.frame(xmim = x_raster, ymin = y_raster, edited = FALSE)
      # # Add the point to the list of points
      # print(coords)
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
      cropped_ras <- crop(mosaic, extedit)
      # Create an extent with a 2 m buffer around the point
      sizes <- adjust_canvas(cropped_ras)
      wid(sizes[[1]])
      hei(sizes[[2]])
      # Generate the cropped image
      cropped_image_path <- paste(tempdir(), "croppedimage.png")
      png(cropped_image_path, width = wid(), height = hei())
      check_and_plot(cropped_ras, r = r$r, g = g$g, b = b$b)
      points(x = x, y = y, col = "red", cex = 5, pch = 13)
      text(
        x = x,
        y = y,  # Apply dynamic offset
        labels = row_id(),
        col = "black",
        cex = 2,
        pos = 3
      )
      dev.off()

      # Send the cropped image to the client
      session$sendCustomMessage("updateTiles", list(
        img = base64enc::base64encode(cropped_image_path)
      ))
      # Send the adjusted canvas size to the client
      session$sendCustomMessage("adjustCanvasSize", list(
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
      session$sendCustomMessage("updateTiles", list(
        img = base64enc::base64encode(original_image_path)
      ))
      # Send the adjusted canvas size to the client
      session$sendCustomMessage("adjustCanvasSize", list(
        width = widori(),
        height = heiori()
      ))
    })



    ########## CREATE SHAPEFILES ############
    observeEvent(input$createupdate2, {
      pdata <- isolate(do.call(rbind, points$data))
      contrpoints <- sf::st_as_sf(vect(as.matrix(rbind(pdata, pdata[1, ])[, 1:2]), type = "polygon"))
      sf::st_crs(contrpoints) <- sf::st_crs(mosaic)
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
      shpt <-
        shapefile_build(
          basemap = FALSE,
          mosaic,
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
        )
      rv_list <- reactiveValuesToList(createdshape)
      if (length(rv_list) > 0) {
        element_names <- names(rv_list)
        last_name <- tail(element_names, 1)
        createdshape[[last_name]] <- NULL
      }
      tmpshape$tmp <- shpt[[1]]
      sizes <- adjust_canvas(mosaic)
      png(basepolygon, width = sizes[[1]], height = sizes[[2]])
      check_and_plot(mosaic, r = r$r, g = g$g, b = b$b)
      alreadybuilt <-
        reactiveValuesToList(createdshape) |>
        dplyr::bind_rows(.id = "block")
      if(nrow(alreadybuilt) > 0){
        shptoplot <-
          dplyr::bind_rows(
            alreadybuilt,
            tmpshape$tmp |> dplyr::mutate(block = "block_temp")
          )
      } else{
        shptoplot <- tmpshape$tmp
      }
      shapefile[[input$shapenamebuild]] <- create_reactval(input$shapenamebuild, shptoplot)
      shapefilenames <- setdiff(c("none", names(shapefile)), c("shapefile", "shapefileplot"))
      # Update selectInput choices
      updateSelectInput(session, "shapefiletoanalyze",
                        choices = shapefilenames,
                        selected = shapefilenames[[length(shapefilenames)]])
      plot(shptoplot["plot_id"], add = TRUE, border = "red", lwd = 3, col = rgb(0, 0, 1, alpha = 0))
      if(input$showplotid){
        centrs <- suppressMessages(sf::st_centroid(shptoplot) |> sf::st_coordinates())
        text(
          x = centrs[, 1],
          y = centrs[, 2],
          labels = shptoplot$plot_id,
          col = "black",
          cex = 1,
        )
      }
      dev.off()
      session$sendCustomMessage("updateTiles", list(
        img = base64enc::base64encode(basepolygon)
      ))
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
        req(input$shapenamebuild)
        req(createdshape)
        shptemp <-
          reactiveValuesToList(createdshape) |>
          dplyr::bind_rows(.id = "block") |>
          dplyr::mutate(unique_id = dplyr::row_number(), .before = 1)
        observe({
          shapefilenames <- setdiff(c("none", names(shapefile)), c("shapefile", "shapefileplot"))
          shapefile[[input$shapenamebuild]] <- create_reactval(input$shapenamebuild, shptemp)
          # Update selectInput choices
          updateSelectInput(session, "shapefiletoanalyze",
                            choices = shapefilenames,
                            selected = shapefilenames[[length(shapefilenames)]])
        })
      }
    })




    # RESET VIEW
    observeEvent(c(input$reset_view, input$delete_point_click, input$editiondone), {
      wid(widori())
      hei(heiori())
      current_extent(ext(mosaic))  # Reset extent to the full raster
      if (length(points$data) > 0) {
        sizes <- adjust_canvas(mosaic)
        png(basepolygon, width = sizes[[1]], height = sizes[[2]])
        check_and_plot(mosaic, r = r$r, g = g$g, b = b$b)
        points_df <- do.call(rbind, points$data)
        points_df <- as.data.frame(points_df)
        points(x = points_df[, 1],
               y = points_df[, 2],
               col = "red",
               cex=2,
               pch=16)
        text(
          x = points_df[, 1],
          y = points_df[, 2],  # Apply dynamic offset
          labels = 1:nrow(points_df),
          col = "black",
          cex = 2,
          pos = 3
        )
        if(nchar(input$shapefiletoanalyze) > 0){
          plot(shapefile[[input$shapefiletoanalyze]]$data["plot_id"], add = TRUE, border = "red", lwd = 3, col = rgb(0, 0, 1, alpha = 0))
          if(input$showplotid){
            centrs <- suppressMessages(sf::st_centroid(shapefile[[input$shapefiletoanalyze]]$data) |> sf::st_coordinates())
            text(
              x = centrs[, 1],
              y = centrs[, 2],
              labels = shapefile[[input$shapefiletoanalyze]]$data$plot_id,
              col = "black",
              cex = 1,
            )
          }
        }
        dev.off()
        session$sendCustomMessage("updateTiles", list(
          img = base64enc::base64encode(basepolygon)
        ))
      } else{
        session$sendCustomMessage("updateTiles", list(
          img = base64enc::base64encode(original_image_path)
        ))
      }
      session$sendCustomMessage("adjustCanvasSize", list(
        width = as.integer(widori()),
        height = as.integer(heiori())
      ))
    })




    ############################################# IMPORT A SHAPEFILE ##########################################################
    observe({
      req(input$shapefiletoanalyze)
      if(input$shapefiletoanalyze == "none"){
        shapefile[["shapefileplot"]] <- NULL
      } else{
        shapefile[["shapefileplot"]] <- shapefile[[input$shapefiletoanalyze]]$data
      }
    })
    mod_download_shapefile_server("downloadshapefile2", terra::vect(shapefile$shapefileplot), name = "created_shp")


  })
}
