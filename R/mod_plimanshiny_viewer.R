#' matanalyze UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
plimanshiny_viewer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
        #rasterCanvasAlt {
          margin-top: 20px;
          border: 1px solid #ddd;
          box-shadow: 3px 3px 8px rgba(0, 0, 0, 0.3);
        }
      ")),
      tags$script(HTML(sprintf("
        let canvasAlt, ctxAlt, drawingAlt = false;
        let rectStartXAlt, rectStartYAlt, rectEndXAlt, rectEndYAlt;
        let selectedPointsAlt = [];
        let rasterImageAlt = null;
        let canvasWidthAlt = 1080;
        let canvasHeightAlt = 608;

        function initCanvasAlt() {
          canvasAlt = document.getElementById('%s');
          ctxAlt = canvasAlt.getContext('2d');
          canvasAlt.width = canvasWidthAlt;
          canvasAlt.height = canvasHeightAlt;

          canvasAlt.addEventListener('mousedown', handleMouseDownAlt);
          canvasAlt.addEventListener('mousemove', handleMouseMoveAlt);
          canvasAlt.addEventListener('mouseup', handleMouseUpAlt);
          canvasAlt.addEventListener('dblclick', handleDoubleClickAlt);
        }

        function adjustCanvasSizeAlt(width, height) {
          canvasWidthAlt = width;
          canvasHeightAlt = height;
          canvasAlt.width = canvasWidthAlt;
          canvasAlt.height = canvasHeightAlt;
          Shiny.setInputValue('%s', { width: canvasWidthAlt, height: canvasHeightAlt }, { priority: 'event' });
          drawCanvasAlt();
        }

        function handleMouseDownAlt(e) {
          const rect = canvasAlt.getBoundingClientRect();
          rectStartXAlt = e.clientX - rect.left;
          rectStartYAlt = e.clientY - rect.top;
          drawingAlt = true;
        }

        function handleMouseMoveAlt(e) {
          if (!drawingAlt) return;
          const rect = canvasAlt.getBoundingClientRect();
          rectEndXAlt = e.clientX - rect.left;
          rectEndYAlt = e.clientY - rect.top;
          drawCanvasAlt();
          drawRectangleAlt(rectStartXAlt, rectStartYAlt, rectEndXAlt, rectEndYAlt);
        }

        function handleMouseUpAlt() {
          drawingAlt = false;
          Shiny.setInputValue('%s', {
            startX: Math.min(rectStartXAlt, rectEndXAlt),
            startY: Math.min(rectStartYAlt, rectEndYAlt),
            endX: rectEndXAlt,
            endY: rectEndYAlt,
            width: Math.abs(rectEndXAlt - rectStartXAlt),
            height: Math.abs(rectEndYAlt - rectStartYAlt)
          });
          rectStartXAlt = rectStartYAlt = rectEndXAlt = rectEndYAlt = 0;
        }

        function drawRectangleAlt(x1, y1, x2, y2) {
          ctxAlt.strokeStyle = 'red';
          ctxAlt.lineWidth = 2;
          ctxAlt.strokeRect(x1, y1, x2 - x1, y2 - y1);
        }

        function drawCanvasAlt() {
          ctxAlt.clearRect(0, 0, canvasAlt.width, canvasAlt.height);
          drawRasterAlt();
        }

        function drawRasterAlt() {
          if (rasterImageAlt) {
            ctxAlt.drawImage(rasterImageAlt, 0, 0, canvasAlt.width, canvasAlt.height);
          }
        }

        Shiny.addCustomMessageHandler('updateTilesAlt', function(data) {
          rasterImageAlt = new Image();
          rasterImageAlt.src = 'data:image/png;base64,' + data.img;
          rasterImageAlt.onload = drawCanvasAlt;
        });

        Shiny.addCustomMessageHandler('adjustCanvasSizeAlt', function(data) {
          adjustCanvasSizeAlt(data.width, data.height);
        });

        function handleDoubleClickAlt() {
          Shiny.setInputValue('%s', new Date().getTime());
        }

        window.addEventListener('load', initCanvasAlt);
      ", ns("rasterCanvasAlt"), ns("canvas_size_alt"), ns("drawn_rectangle_alt"), ns("reset_view_alt"))))
    ),
    tags$canvas(id = ns("rasterCanvasAlt"))
  )

}

# Server module for the viewer
plimanshiny_viewer_server <- function(id, mosaic,
                                      r = reactiveVal(1),
                                      g = reactiveVal(2),
                                      b = reactiveVal(3),
                                      usemargin = reactiveVal(TRUE)) {
  if(usemargin()){
    marg <- reactiveVal(c(3.1, 3.1, 2.1, 7.1))
  } else{
    marg <- reactiveVal(0)
  }
  # print(marg)
  moduleServer(id, function(input, output, session) {

    needstretch <- reactiveVal(FALSE)
    original_image_path <- file.path(tempdir(), "originalimage.png")

    # Cleanup when the session ends
    session$onSessionEnded(function() {
      if (file.exists(original_image_path)) {
        file.remove(original_image_path)
      }
    })
    nlyrs <- reactiveVal(terra::nlyr(mosaic))
    # other reactiveVals
    original_ext <- reactiveVal(ext(mosaic))
    current_extent <- reactiveVal(ext(mosaic))
    wid <- reactiveVal()
    hei <- reactiveVal()
    widori <- reactiveVal()
    heiori <- reactiveVal()

    # Adjust the canvas size based on the mosaic
    observe({
      sizes <- adjust_canvas(mosaic)
      widori(sizes[[1]])
      heiori(sizes[[2]])
      # Send the adjusted canvas size to the client
      session$sendCustomMessage("adjustCanvasSizeAlt", list(
        width = as.integer(widori()),
        height = as.integer(heiori())
      ))
    })

    # Generate and cache the original image.
    # the image will be redrawn.
    observe({
      req(mosaic)
      # Register reactive dependencies on the colors:
      sizes <- adjust_canvas(mosaic)
      png(original_image_path, width = sizes[[1]], height = sizes[[2]])
      if(nlyrs() < 3){
        terra::plot(mosaic[[1]],
                    col = pliman::custom_palette(c("darkred",  "yellow", "darkgreen"), n = 100),
                    maxcell = 1e6,
                    mar = marg(),
                    legend = "bottomleft",
                    smooth = TRUE)
      } else{
        current_r <- as.numeric(r())
        current_g <- as.numeric(g())
        current_b <- as.numeric(b())
        a <- try(plotRGB(mosaic, smooth = FALSE, r = current_r, g = current_g, b = current_b))
        if (inherits(a, "try-error")) {
          needstretch(TRUE)
          plotRGB(mosaic, smooth = FALSE, r = current_r, g = current_g, b = current_b, stretch = "lin")
        }
      }
      dev.off()
      current_extent(ext(mosaic))
      session$sendCustomMessage("updateTilesAlt", list(
        img = base64enc::base64encode(original_image_path)
      ))
    })

    # Reset view to the original raster on button click
    observeEvent(input$reset_view_alt, {
      wid(widori())
      hei(heiori())
      current_extent(ext(mosaic))  # Reset extent to the full raster
      session$sendCustomMessage("updateTilesAlt", list(
        img = base64enc::base64encode(original_image_path)
      ))
      session$sendCustomMessage("adjustCanvasSizeAlt", list(
        width = as.integer(widori()),
        height = as.integer(heiori())
      ))
    })

    # Handle rectangle drawing and cropping
    observeEvent(input$drawn_rectangle_alt, {
      rect <- input$drawn_rectangle_alt
      req(rect$startX)
      if (rect$startX == rect$endX || rect$startY == rect$endY) {
        return(NULL)
      }
      # Convert canvas coordinates to raster spatial extent using terra accessors:
      xmin_val <- terra::xmin(current_extent())
      xmax_val <- terra::xmax(current_extent())
      ymin_val <- terra::ymin(current_extent())
      ymax_val <- terra::ymax(current_extent())
      fact_canva_rast_x <- input$canvas_size_alt$width / (xmax_val - xmin_val)
      fact_canva_rast_y <- input$canvas_size_alt$height / (ymax_val - ymin_val)
      xmin <- xmin_val + rect$startX / fact_canva_rast_x
      xmax <- xmin_val + rect$endX / fact_canva_rast_x
      ymin <- ymin_val + (input$canvas_size_alt$height - rect$endY) / fact_canva_rast_y
      ymax <- ymin_val + (input$canvas_size_alt$height - rect$startY) / fact_canva_rast_y

      # Update the current extent using terra::ext()
      current_extent(ext(c(xmin, xmax, ymin, ymax)))
      xrange <- abs(xmax - xmin)
      originalres <- res(mosaic)[[1]]
      newres <- max(c(xrange / 720, originalres))

      tfc <- file.path(tempdir(), "tempcropped.png")
      session$onSessionEnded(function() {
        if (file.exists(tfc)) {
          file.remove(tfc)
        }
      })
      if(nlyrs() < 3){
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
                    mar = marg())
        dev.off()


      } else{
        # Use the reactive colors when re-plotting the cropped image.
        current_r <- as.numeric(r())
        current_g <- as.numeric(g())
        current_b <- as.numeric(b())

        trycrop <- try(
          suppressMessages(
            suppressWarnings(
              sf::gdal_utils(
                util = "warp",
                source = terra::sources(mosaic),
                destination = tfc,
                options = c(
                  strsplit(paste("-te", xmin, ymin, xmax, ymax), split = "\\s")[[1]],
                  "-tr", paste0(newres), paste0(newres),
                  "-b", as.character(current_r),
                  "-b", as.character(current_g),
                  "-b", as.character(current_b),
                  "-overwrite"
                )
              )
            )
          )
        )

        if (inherits(trycrop, "try-error")) {
          cropped_ras <- crop(mosaic, ext(xmin, xmax, ymin, ymax))
          sizes <- adjust_canvas(cropped_ras)
          wid(sizes[[1]])
          hei(sizes[[2]])
          png(tfc, width = wid(), height = hei())
          if (needstretch()) {
            plotRGB(cropped_ras, smooth = FALSE,
                    r = current_r, g = current_g, b = current_b, stretch = "lin")
          } else {
            plotRGB(cropped_ras, smooth = FALSE,
                    r = current_r, g = current_g, b = current_b)
          }
          dev.off()
        } else {
          cropped_ras <- rast(tfc)
          sizes <- adjust_canvas(cropped_ras)
          wid(sizes[[1]])
          hei(sizes[[2]])
        }
      }


      session$sendCustomMessage("updateTilesAlt", list(
        img = base64enc::base64encode(tfc)
      ))
      session$sendCustomMessage("adjustCanvasSizeAlt", list(
        width = as.integer(wid()),
        height = as.integer(hei())
      ))
    })

  })
}
