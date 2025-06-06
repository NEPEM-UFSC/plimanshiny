#' georeference UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_georeference_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      col_4(
        bs4TabCard(
          id = "tabsgeoref",
          width = 12,
          collapsible = FALSE,
          status = "success",
          selected = "Input",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel("Input",
                   selectInput(
                     ns("mosaic_to_georef"),
                     label = "Mosaic to be georeferenced",
                     choices = NULL
                   ),
                   pickerInput(
                     ns("dfgcp"),
                     label = "Control points (GPS)",
                     choices = NULL
                   ),
                   fluidRow(
                     col_4(
                       pickerInput(
                         ns("lat"),
                         label = "Lat",
                         choices = NULL
                       )
                     ),
                     col_4(
                       pickerInput(
                         ns("lon"),
                         label = "Lon",
                         choices = NULL
                       )
                     ),
                     pickerInput(
                       ns("alt"),
                       label = "Alt",
                       choices = NULL
                     )
                   ),
                   actionBttn(
                     ns("doneconfigpoints"),
                     label = "Selection finished",
                     style = "pill",
                     color = "success"
                   ),
                   numericInput(ns("epsgcode"),
                                label = "EPSG code (use 'Toolbox > UTM Zone selector')",
                                value = NA),
                   h4("Control points (GPS)"),
                   DT::dataTableOutput(ns("points_table")),
          ),
          tabPanel("Control points (field)",
                   DT::dataTableOutput(ns("points_table_geo")),
                   conditionalPanel(
                     condition = sprintf("output['%s']", ns("show_panel3")),
                     actionBttn(ns("editiondonegeo"), "Edition Done", style = "pill", color = "success"),
                   )
          ),
          tabPanel("Output",
                   shinyDirButton(
                     id = ns("outfolder"),
                     label = "Output folder",
                     title = "Output folder"
                   ),
                   textInput(ns("outdir"),
                             label = "Output folder",
                             value = ""),
                   textInput(
                     ns("newname"),
                     label = "Raster name",
                     value = NULL
                   ),
                   actionBttn(
                     ns("georreference"),
                     label = "Start georeferencing!",
                     style = "pill",
                     color = "success"
                   )
          )
        )
      ),
      col_8(
        plimanshiny_canvas_output(prefix = "geor", ns = ns)
      )
    )
  )
}


#' georeference Server Functions
#'
#' @noRd
mod_georeference_server <- function(id, mosaic_data, r, g, b, dfs, zlim){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mosaitoshape <- reactiveVal()
    dftemp <- reactiveVal()
    control_points <- reactiveVal()
    has_control_points <- reactiveVal(FALSE)
    cpoints <- reactiveVal()

    observe({
      req(mosaic_data)
      updateSelectInput(session, "mosaic_to_georef", choices = c(setdiff(names(mosaic_data), "mosaic")),
                        selected = names(mosaic_data)[1])
    })

    observe({
      updateTextInput(session, "new_cropped", value = paste0(input$mosaic_to_georef, "_cropped"))
    })
    observe({
      if(nchar(input$mosaic_to_georef) > 0){
        mosaitoshape(mosaic_data[[input$mosaic_to_georef]]$data)
      }
    })

    observe({
      updatePickerInput(session, "dfgcp",
                        choices = c("none", names(dfs)))
    })
    observe({
      updatePickerInput(session, "dfgcp",
                        choices = c("none", names(dfs)))
    })

    observeEvent(input$dfgcp, {
      req(input$dfgcp)
      dftemp(dfs[[input$dfgcp]]$data)
    })


    observe({
      updatePickerInput(session, "lat", choices = colnames(dftemp()))
    })
    observe({
      updatePickerInput(session, "lon", choices = colnames(dftemp()))
    })
    observe({
      updatePickerInput(session, "alt", choices = c("NA", colnames(dftemp())))
    })

    # select the points
    observe({
      req(input$lat)
      req(input$lon)
      cpoints(dftemp() |> dplyr::select(dplyr::any_of(c(input$lon, input$lat, input$alt))))
      control_points(cpoints())
    })

    observeEvent(input$doneconfigpoints, {
      control_points(NULL)
      if(ncol(cpoints()) > 1){
        if(guess_coordinate_type(cpoints()) == "lat/lon"){
          utmpoints <- to_utm(as.matrix(cpoints()[, 1:2]))
          control_points(as.data.frame(utmpoints$utm))
          updateNumericInput(session, "epsgcode", value = utmpoints$epsg)
          has_control_points(TRUE)
          # message alert that the points were converted to UTM
          showNotification(
            ui = tagList(
              tags$i(class = "fa fa-check"), "Control points converted to UTM"
            ),
            type = "message",
            duration = 5000,  # Remains until manually removed
            id = "georef"
          )
        } else{
          control_points(as.data.frame(cpoints()))
          has_control_points(TRUE)
        }
      }
    })

    observe({
      volumes <- c(getVolumes()())
      shinyDirChoose(input, "outfolder",
                     roots = volumes,
                     session = session,
                     restrictions = system.file(package = "base"))
      diroutput <- parseDirPath(volumes, input$outfolder)
      req(diroutput)
      updateTextInput(session, "outdir", value = diroutput)
    })

    observe({



      output$points_table <- DT::renderDT({
        req(control_points())
        # Add Edit/Delete buttons
        points_df <- control_points()
        points_df <-
          points_df |>
          transform(
            Delete = sprintf('<button class="delete-point-btn" id="delete_point_%s">🗑️</button>', seq_len(nrow(points_df)))
          ) |>
          round_cols(digits = 3)
        rownames(points_df) <- NULL
        DT::datatable(
          points_df,
          escape = FALSE,
          selection = "none",
          options = list(
            dom = "t",
            paging = FALSE
          ),
          callback = DT::JS(sprintf("
      table.on('click', '.delete-point-btn', function() {
        var id = $(this).attr('id');
        Shiny.setInputValue('%s', {id: id}, {priority: 'event'});
      });
    ", ns("delete_point")))
        )
      })

      observeEvent(input$delete_point, {
        btn_info <- input$delete_point
        row_id <- as.numeric(strsplit(btn_info$id, "_")[[1]][3])
        control_points(control_points() |> dplyr::slice(-row_id))
      })

      req(mosaitoshape())
      original_image_path <- file.path(tempdir(), "originalimage.png")
      basepolygon <- file.path(tempdir(), "imagewithpolygon.png")
      current_extent <- reactiveVal()
      observe({
        req(mosaitoshape())
        current_extent(ext(mosaitoshape()))
      })
      polygeom <- reactiveVal()
      points <- reactiveValues(data = list())
      createdshape <- reactiveValues()
      tmpshape <- reactiveValues()
      # Reactive values to store points
      points <- reactiveValues(data = list())

      # Reactive value to store the current cropped extent
      wid <- reactiveVal()
      hei <- reactiveVal()
      widori <- reactiveVal()
      heiori <- reactiveVal()

      observe({
        sizes <- adjust_canvas(mosaitoshape())
        widori(sizes[[1]])
        heiori(sizes[[2]])
        # Send the adjusted canvas_geor size to the client
        session$sendCustomMessage("adjustcanvas_georSize", list(
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
          if(has_control_points()){
            points(x = control_points()[, 1],
                   y = control_points()[, 2],
                   cex = 4,
                   pch = 13,
                   col = "salmon")
            text(
              x = control_points()[, 1],
              y = control_points()[, 2],  # You can adjust the offset if needed
              labels = 1:nrow(control_points()),
              col = "salmon",
              cex = 2,
              pos = 3
            )
          }

        }, error = function(e) {
          message("An error occurred during plotting 1: ", e$message)
        }, finally = {
          dev.off()
        })

        current_extent(ext(mosaitoshape()))
        session$sendCustomMessage("updateTiles_geor", list(
          img = base64enc::base64encode(original_image_path)
        ))
      })





      observeEvent(input$drawn_rectangle_geor, {
        rect <- input$drawn_rectangle_geor
        req(rect$startX)
        if(rect$startX == rect$endX | rect$startY == rect$endY){
          return(NULL)
        }
        # Convert canvas_geor coordinates to raster spatial extent
        xmin_val <- terra::xmin(current_extent())
        xmax_val <- terra::xmax(current_extent())
        ymin_val <- terra::ymin(current_extent())
        ymax_val <- terra::ymax(current_extent())
        fact_canva_rast_x <- input$canvas_size_geor$width / (xmax_val - xmin_val)
        fact_canva_rast_y <- input$canvas_size_geor$height / (ymax_val - ymin_val)
        xmin <- xmin_val + rect$startX / fact_canva_rast_x
        xmax <- xmin_val + rect$endX / fact_canva_rast_x
        ymin <- ymin_val + (input$canvas_size_geor$height - rect$endY) / fact_canva_rast_y
        ymax <- ymin_val + (input$canvas_size_geor$height - rect$startY) / fact_canva_rast_y
        current_extent(ext(c(xmin, xmax, ymin, ymax)))  # Update the extent
        xrange <- abs(xmax - xmin)
        originalres <- res(mosaitoshape())[[1]]
        newres <- max(c(xrange / 720, originalres))
        # Crop the raster and update the current extent
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
            check_and_plot(mosaitoshape(), r = r$r, g = g$g, b = b$b, zlim = zlim$zlim)
            dev.off()
          } else{
            cropped_ras <- rast(tfc)
            sizes <- adjust_canvas(cropped_ras)
            wid(sizes[[1]])
            hei(sizes[[2]])
          }
        }


        # Generate the cropped image
        if(length(points$data) > 0 | has_control_points()){
          cropped_image_path <- paste(tempdir(), "croppedimage.png")
          png(cropped_image_path, width = wid(), height = hei())

          tryCatch({
            # Plot the raster with custom color settings
            check_and_plot(cropped_ras, r = r$r, g = g$g, b = b$b, zlim = zlim$zlim)

            # If there are point data, process and plot them
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
            if(has_control_points()){
              points(x = control_points()[, 1],
                     y = control_points()[, 2],
                     cex = 4,
                     pch = 13,
                     col = "salmon")
              text(
                x = control_points()[, 1],
                y = control_points()[, 2],  # You can adjust the offset if needed
                labels = 1:nrow(control_points()),
                col = "salmon",
                cex = 2,
                pos = 3
              )
            }



          }, error = function(e) {
            message("An error occurred during plotting: ", e$message)
          }, finally = {
            dev.off()
          })

          session$sendCustomMessage("updateTiles_geor", list(
            img = base64enc::base64encode(cropped_image_path)
          ))
        } else{
          session$sendCustomMessage("updateTiles_geor", list(
            img = base64enc::base64encode(tfc)
          ))
        }
        session$sendCustomMessage("adjustcanvas_georSize", list(
          width = as.integer(wid()),
          height = as.integer(hei())
        ))
      })






      # Handle point selection input
      observeEvent(input$picked_point_geor, {
        point <- input$picked_point_geor
        req(point)
        xmin_val <- terra::xmin(current_extent())
        xmax_val <- terra::xmax(current_extent())
        ymin_val <- terra::ymin(current_extent())
        ymax_val <- terra::ymax(current_extent())
        canvas_geor_width <- input$canvas_size_geor$width
        canvas_geor_height <- input$canvas_size_geor$height
        x_canvas_geor <- point[1]
        y_canvas_geor <- point[2]
        x_raster <- xmin_val + (x_canvas_geor / canvas_geor_width) * (xmax_val- xmin_val)
        y_raster <- ymin_val + ((canvas_geor_height - y_canvas_geor) / canvas_geor_height) * (ymax_val - ymin_val)
        coords <- data.frame(xmim = x_raster, ymin = y_raster, edited = FALSE)
        # # Add the point to the list of points
        points$data <- append(points$data, list(coords))

      })
      #
      output$show_panel3 <- reactive({
        any(control_points()$edited)
      })
      #
      outputOptions(output, "show_panel3", suspendWhenHidden = FALSE)

      output$points_table_geo <- DT::renderDT({
        if (length(points$data) > 0) {
          points_df <- isolate(do.call(rbind, points$data))
          points_df <- as.data.frame(points_df, stringsAsFactors = FALSE) |> pliman::round_cols(digits = 2)
          colnames(points_df) <- c("xcoord", "ycoord", "edited")

          # Add Edit/Delete buttons
          points_df <-
            points_df |>
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
        output$show_panel3 <- reactive({
          TRUE
        })

        # Ensure the reactive output is available to the UI
        outputOptions(output, "show_panel3", suspendWhenHidden = FALSE)

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
            col = "salmon",
            cex = 2,
            pos = 3
          )
          if(has_control_points()){
            points(x = control_points()[, 1],
                   y = control_points()[, 2],
                   cex = 4,
                   pch = 13,
                   col = "salmon")
            text(
              x = control_points()[, 1],
              y = control_points()[, 2],  # You can adjust the offset if needed
              labels = 1:nrow(control_points()),
              col = "salmon",
              cex = 2,
              pos = 3
            )
          }
        }, error = function(e) {
          message("An error occurred during plotting: ", e$message)
        }, finally = {
          dev.off()
        })


        # Send the cropped image to the client
        session$sendCustomMessage("updateTiles_geor", list(
          img = base64enc::base64encode(cropped_image_path)
        ))
        # Send the adjusted canvas_geor size to the client
        session$sendCustomMessage("adjustcanvas_georSize", list(
          width = as.integer(wid()),
          height = as.integer(hei())
        ))
      })



      observeEvent(input$editiondonegeo, {
        points$data[[row_id()]][[1]] <- points$data[[length(points$data)]][[1]]
        points$data[[row_id()]][[2]] <- points$data[[length(points$data)]][[2]]
        points$data[[row_id()]][[3]] <- FALSE
        points$data[[length(points$data)]] <- NULL
        output$show_panel3 <- reactive({
          FALSE
        })
      })


      ####### CLEAR POINTS ##########
      observeEvent(input$clearpoints, {
        points$data <- list()
        polygeom(NULL)
        session$sendCustomMessage("updateTiles_geor", list(
          img = base64enc::base64encode(original_image_path)
        ))
        # Send the adjusted canvas_geor size to the client
        session$sendCustomMessage("adjustcanvas_georSize", list(
          width = widori(),
          height = heiori()
        ))
      })

      observeEvent(c(input$reset_view_geor, input$delete_point_click, input$editiondonegeo), {
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
              col = "salmon",
              cex = 2,
              pos = 3
            )
            if(has_control_points()){
              points(x = control_points()[, 1],
                     y = control_points()[, 2],
                     cex = 4,
                     pch = 13,
                     col = "salmon")
              text(
                x = control_points()[, 1],
                y = control_points()[, 2],  # You can adjust the offset if needed
                labels = 1:nrow(control_points()),
                col = "salmon",
                cex = 2,
                pos = 3
              )
            }

          }, error = function(e) {
            # Handle the error (you can log it, notify the user, etc.)
            message("An error occurred during plotting: ", e$message)
          }, finally = {
            # Ensure that the graphics device is closed no matter what
            dev.off()
          })

          session$sendCustomMessage("updateTiles_geor", list(
            img = base64enc::base64encode(basepolygon)
          ))
        } else{
          session$sendCustomMessage("updateTiles_geor", list(
            img = base64enc::base64encode(original_image_path)
          ))
        }
        session$sendCustomMessage("adjustcanvas_georSize", list(
          width = as.integer(widori()),
          height = as.integer(heiori())
        ))
      })









      ########################################### GEORREFERENCE THE MOSAIC #################################
      observeEvent(input$georreference, {
        points_field <- do.call(rbind, points$data)[, 1:2]
        points_gps <- control_points()
        # show a message
        showNotification(
          ui = tagList(
            tags$i(class = "fa fa-spinner fa-spin"), "Georeferencing in progress..."
          ),
          type = "message",
          duration = NULL,  # Remains until manually removed
          id = "georefe"
        )
        # Guess the coordinate type for gps_coords
        if (guess_coordinate_type(points_field) == "lat/lon") {
          # If gps_coords are in lat/lon, compute the UTM EPSG code
          utmcoords <- to_utm(points_field)
          points_field <- utmcoords$coords |> as.data.frame()
          utm_crs <- utmcoords$epsg
        } else {
          utm_crs <- paste0("EPSG:", input$epsgcode)
        }
        # alert if req(input$epsgcode) is null
        if(is.na(input$epsgcode)){
          sendSweetAlert(
            session = session,
            title = "Oops!",
            text = "Please provide the EPSG code for the UTM projection.",
            type = "error"
          )
          return()
        }

        # Define mosaic metadata
        xmin <- terra::xmin(mosaitoshape())
        ymax <- terra::ymax(mosaitoshape())
        mosres <- terra::res(mosaitoshape())
        # Apply conversion to all control points
        points_field <- t(apply(points_field, 1, convert_to_pixel_line, xmin, mosres[1], ymax, mosres[2]))
        # Validate control points and GPS coordinates
        if (nrow(points_field) != nrow(points_gps)) {
          sendSweetAlert(
            session = session,
            title = "Oops!",
            text = "The number of control points and GPS coordinates must match.",
            type = "error"
          )
          return()
        }

        # Prepare GDAL control points arguments
        gcp_args <- unlist(
          mapply(
            function(cp, gps) {
              c("-gcp", cp[1], cp[2], gps[1], gps[2])
            },
            split(points_field, row(points_field)),
            split(points_gps, row(points_gps)),
            SIMPLIFY = FALSE
          )
        )

        vals <- terra::spatSample(mosaitoshape(), 5000)
        min_val <- min(vals)
        max_val <- max(vals)
        if (all(vals == floor(vals))) {
          # Values are integers
          if (min_val >= 0 && max_val <= 255) {
            data_type <- "Byte"
          } else if (min_val >= -128 && max_val <= 127) {
            data_type <- "Int8"
          } else if (min_val >= 0 && max_val <= 65535) {
            data_type <- "UInt16"
          } else if (min_val >= -32768 && max_val <= 32767) {
            data_type <- "Int16"
          } else if (min_val >= 0 && max_val <= 4294967295) {
            data_type <- "UInt32"
          } else if (min_val >= -2147483648 && max_val <= 2147483647) {
            data_type <- "Int32"
          } else {
            data_type <- "Int64" # Default for large integers
          }
        } else {
          if (max_val < 3.4e38 && min_val > -3.4e38) {
            data_type <- "Float32"
          } else {
            data_type <- "Float64"
          }
        }

        temp_output <- tempfile(fileext = ".tif")
        gdal_utils(
          util = "translate",
          source = terra::sources(mosaitoshape()),
          destination = temp_output,
          options = c(gcp_args, "-a_srs", utm_crs, "-ot", data_type, "-of",  "GTiff")
        )

        gdal_utils(
          util = "warp",
          source = temp_output,
          destination = paste0(input$outdir, "/", input$newname, ".tif"),
          options = c("-t_srs", utm_crs, "-r", "near", "-co", "COMPRESS=DEFLATE", "-of", "GTiff")
        )

        removeNotification("georefe")

      })
    })
  })
}
#
# #
# # Load required libraries
# library(shiny)
# library(shinyFiles)
# library(bs4Dash)
# library(DT)
# library(dplyr)
# library(shinyWidgets)
# library(terra)
#
# # Define UI
# ui <- fluidPage(
#   titlePanel("Georeferencing Module Demo"),
#   mod_georeference_ui("georef_ui")
# )
#
# # Define Server logic
# server <- function(input, output, session) {
#
#   # Sample mosaic data
#   mosaic_data <- terra::rast("D:/resultttt.tif")
#
#   # Call the georeference module
#   mod_georeference_server("georef_ui", mosaic_data = mosaic_data)
# }
# #
# # # Run the app
# shinyApp(ui, server, options = list(launch.browser = TRUE))
#
# ## To be copied in the UI
# # mod_georeference_ui("georeference_1")
#
# ## To be copied in the server
# # mod_georeference_server("georeference_1")
