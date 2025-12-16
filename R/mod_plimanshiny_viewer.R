#' matanalyze UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
# plimanshiny_viewer_ui <- function(id, prefix = "mainviewer", width = 1080, height = 608) {
#   ns <- NS(id)
#
#   # Helper to create namespaced and prefixed IDs
#   ids <- function(x) ns(paste0(x, "_", prefix))
#
#   # JavaScript template
#   js_template <- "
#     let canvas_{{PREFIX}}, ctx_{{PREFIX}}, drawing_{{PREFIX}} = false;
#     let rectStartX_{{PREFIX}}, rectStartY_{{PREFIX}}, rectEndX_{{PREFIX}}, rectEndY_{{PREFIX}};
#     let selectedPoints_{{PREFIX}} = [];
#     let rasterImage_{{PREFIX}} = null;
#     let canvasWidth_{{PREFIX}}, canvasHeight_{{PREFIX}}; // DELETED: Hardcoded values
#
#     function initCanvas_{{PREFIX}}() {
#       canvas_{{PREFIX}} = document.getElementById('%s');
#       ctx_{{PREFIX}} = canvas_{{PREFIX}}.getContext('2d');
#
#       // NEW: Read the initial width and height from the canvas element's attributes
#       canvasWidth_{{PREFIX}} = canvas_{{PREFIX}}.width;
#       canvasHeight_{{PREFIX}} = canvas_{{PREFIX}}.height;
#
#       canvas_{{PREFIX}}.addEventListener('mousedown', handleMouseDown_{{PREFIX}});
#       canvas_{{PREFIX}}.addEventListener('mousemove', handleMouseMove_{{PREFIX}});
#       canvas_{{PREFIX}}.addEventListener('mouseup', handleMouseUp_{{PREFIX}});
#       canvas_{{PREFIX}}.addEventListener('dblclick', handleDoubleClick_{{PREFIX}});
#     }
#
#     function adjustCanvasSize_{{PREFIX}}(width, height) {
#       canvasWidth_{{PREFIX}} = width;
#       canvasHeight_{{PREFIX}} = height;
#       canvas_{{PREFIX}}.width = canvasWidth_{{PREFIX}};
#       canvas_{{PREFIX}}.height = canvasHeight_{{PREFIX}};
#       Shiny.setInputValue('%s', { width: canvasWidth_{{PREFIX}}, height: canvasHeight_{{PREFIX}} }, { priority: 'event' });
#       drawCanvas_{{PREFIX}}();
#     }
#
#     function handleMouseDown_{{PREFIX}}(e) {
#       const rect = canvas_{{PREFIX}}.getBoundingClientRect();
#       rectStartX_{{PREFIX}} = e.clientX - rect.left;
#       rectStartY_{{PREFIX}} = e.clientY - rect.top;
#       drawing_{{PREFIX}} = true;
#     }
#
#     function handleMouseMove_{{PREFIX}}(e) {
#       if (!drawing_{{PREFIX}}) return;
#       const rect = canvas_{{PREFIX}}.getBoundingClientRect();
#       rectEndX_{{PREFIX}} = e.clientX - rect.left;
#       rectEndY_{{PREFIX}} = e.clientY - rect.top;
#       drawCanvas_{{PREFIX}}();
#       drawRectangle_{{PREFIX}}(rectStartX_{{PREFIX}}, rectStartY_{{PREFIX}}, rectEndX_{{PREFIX}}, rectEndY_{{PREFIX}});
#     }
#
#     function handleMouseUp_{{PREFIX}}() {
#       drawing_{{PREFIX}} = false;
#       Shiny.setInputValue('%s', {
#         startX: Math.min(rectStartX_{{PREFIX}}, rectEndX_{{PREFIX}}),
#         startY: Math.min(rectStartY_{{PREFIX}}, rectEndY_{{PREFIX}}),
#         endX: rectEndX_{{PREFIX}},
#         endY: rectEndY_{{PREFIX}},
#         width: Math.abs(rectEndX_{{PREFIX}} - rectStartX_{{PREFIX}}),
#         height: Math.abs(rectEndY_{{PREFIX}} - rectStartY_{{PREFIX}})
#       });
#       rectStartX_{{PREFIX}} = rectStartY_{{PREFIX}} = rectEndX_{{PREFIX}} = rectEndY_{{PREFIX}} = 0;
#     }
#
#     function drawRectangle_{{PREFIX}}(x1, y1, x2, y2) {
#       ctx_{{PREFIX}}.strokeStyle = 'red';
#       ctx_{{PREFIX}}.lineWidth = 2;
#       ctx_{{PREFIX}}.strokeRect(x1, y1, x2 - x1, y2 - y1);
#     }
#
#     function drawCanvas_{{PREFIX}}() {
#       ctx_{{PREFIX}}.clearRect(0, 0, canvas_{{PREFIX}}.width, canvas_{{PREFIX}}.height);
#       drawRaster_{{PREFIX}}();
#     }
#
#     function drawRaster_{{PREFIX}}() {
#       if (rasterImage_{{PREFIX}}) {
#         ctx_{{PREFIX}}.drawImage(rasterImage_{{PREFIX}}, 0, 0, canvas_{{PREFIX}}.width, canvas_{{PREFIX}}.height);
#       }
#     }
#
#     Shiny.addCustomMessageHandler('updateTiles_{{PREFIX}}', function(data) {
#       rasterImage_{{PREFIX}} = new Image();
#       rasterImage_{{PREFIX}}.src = 'data:image/png;base64,' + data.img;
#       rasterImage_{{PREFIX}}.onload = drawCanvas_{{PREFIX}};
#     });
#
#     Shiny.addCustomMessageHandler('adjustCanvasSize_{{PREFIX}}', function(data) {
#       adjustCanvasSize_{{PREFIX}}(data.width, data.height);
#     });
#
#     function handleDoubleClick_{{PREFIX}}() {
#       Shiny.setInputValue('%s', new Date().getTime());
#     }
#
#     window.addEventListener('load', initCanvas_{{PREFIX}});
#   "
#
#   # Replace {{PREFIX}} with the actual prefix
#   js_code <- gsub("\\{\\{PREFIX\\}\\}", prefix, js_template)
#
#   # Insert dynamic IDs using sprintf for remaining placeholders
#   js_code <- sprintf(js_code, ids("rasterCanvas"), ids("canvas_size"), ids("drawn_rectangle"), ids("reset_view"))
#
#   tagList(
#     tags$head(
#       tags$style(HTML(sprintf(
#         "#%s {
#           border: 1px solid #ddd;
#           box-shadow: 3px 3px 8px rgba(40, 167,69, 0.3);
#         }", ids("rasterCanvas")
#       ))),
#       tags$script(HTML(js_code))
#     ),
#     # NEW: Pass the width and height arguments directly to the canvas tag
#     tags$canvas(id = ids("rasterCanvas"), width = width, height = height)
#   )
# }
plimanshiny_viewer_ui <- function(id, prefix = "mainviewer", width = 1080, height = 608) {
  ns <- NS(id)

  # Helper to create namespaced and prefixed IDs
  ids <- function(x) ns(paste0(x, "_", prefix))

  # JavaScript template (Lógica Original Mantida)
  js_template <- "
    let canvas_{{PREFIX}}, ctx_{{PREFIX}}, drawing_{{PREFIX}} = false;
    let rectStartX_{{PREFIX}}, rectStartY_{{PREFIX}}, rectEndX_{{PREFIX}}, rectEndY_{{PREFIX}};
    let selectedPoints_{{PREFIX}} = [];
    let rasterImage_{{PREFIX}} = null;
    let canvasWidth_{{PREFIX}}, canvasHeight_{{PREFIX}};

    function initCanvas_{{PREFIX}}() {
      canvas_{{PREFIX}} = document.getElementById('%s');
      if (!canvas_{{PREFIX}}) return; // Segurança adicionada

      ctx_{{PREFIX}} = canvas_{{PREFIX}}.getContext('2d');

      canvasWidth_{{PREFIX}} = canvas_{{PREFIX}}.width;
      canvasHeight_{{PREFIX}} = canvas_{{PREFIX}}.height;

      canvas_{{PREFIX}}.addEventListener('mousedown', handleMouseDown_{{PREFIX}});
      canvas_{{PREFIX}}.addEventListener('mousemove', handleMouseMove_{{PREFIX}});
      canvas_{{PREFIX}}.addEventListener('mouseup', handleMouseUp_{{PREFIX}});
      canvas_{{PREFIX}}.addEventListener('dblclick', handleDoubleClick_{{PREFIX}});
    }

    function adjustCanvasSize_{{PREFIX}}(width, height) {
      canvasWidth_{{PREFIX}} = width;
      canvasHeight_{{PREFIX}} = height;
      canvas_{{PREFIX}}.width = canvasWidth_{{PREFIX}};
      canvas_{{PREFIX}}.height = canvasHeight_{{PREFIX}};
      Shiny.setInputValue('%s', { width: canvasWidth_{{PREFIX}}, height: canvasHeight_{{PREFIX}} }, { priority: 'event' });
      drawCanvas_{{PREFIX}}();
    }

    function handleMouseDown_{{PREFIX}}(e) {
      const rect = canvas_{{PREFIX}}.getBoundingClientRect();
      rectStartX_{{PREFIX}} = e.clientX - rect.left;
      rectStartY_{{PREFIX}} = e.clientY - rect.top;
      drawing_{{PREFIX}} = true;
    }

    function handleMouseMove_{{PREFIX}}(e) {
      if (!drawing_{{PREFIX}}) return;
      const rect = canvas_{{PREFIX}}.getBoundingClientRect();
      rectEndX_{{PREFIX}} = e.clientX - rect.left;
      rectEndY_{{PREFIX}} = e.clientY - rect.top;
      drawCanvas_{{PREFIX}}();
      drawRectangle_{{PREFIX}}(rectStartX_{{PREFIX}}, rectStartY_{{PREFIX}}, rectEndX_{{PREFIX}}, rectEndY_{{PREFIX}});
    }

    function handleMouseUp_{{PREFIX}}() {
      drawing_{{PREFIX}} = false;
      Shiny.setInputValue('%s', {
        startX: Math.min(rectStartX_{{PREFIX}}, rectEndX_{{PREFIX}}),
        startY: Math.min(rectStartY_{{PREFIX}}, rectEndY_{{PREFIX}}),
        endX: rectEndX_{{PREFIX}},
        endY: rectEndY_{{PREFIX}},
        width: Math.abs(rectEndX_{{PREFIX}} - rectStartX_{{PREFIX}}),
        height: Math.abs(rectEndY_{{PREFIX}} - rectStartY_{{PREFIX}})
      });
      rectStartX_{{PREFIX}} = rectStartY_{{PREFIX}} = rectEndX_{{PREFIX}} = rectEndY_{{PREFIX}} = 0;
    }

    function drawRectangle_{{PREFIX}}(x1, y1, x2, y2) {
      ctx_{{PREFIX}}.strokeStyle = 'red';
      ctx_{{PREFIX}}.lineWidth = 2;
      ctx_{{PREFIX}}.strokeRect(x1, y1, x2 - x1, y2 - y1);
    }

    function drawCanvas_{{PREFIX}}() {
      if(!ctx_{{PREFIX}} || !canvas_{{PREFIX}}) return;
      ctx_{{PREFIX}}.clearRect(0, 0, canvas_{{PREFIX}}.width, canvas_{{PREFIX}}.height);
      drawRaster_{{PREFIX}}();
    }

    function drawRaster_{{PREFIX}}() {
      if (rasterImage_{{PREFIX}}) {
        ctx_{{PREFIX}}.drawImage(rasterImage_{{PREFIX}}, 0, 0, canvas_{{PREFIX}}.width, canvas_{{PREFIX}}.height);
      }
    }

    Shiny.addCustomMessageHandler('updateTiles_{{PREFIX}}', function(data) {
      rasterImage_{{PREFIX}} = new Image();
      rasterImage_{{PREFIX}}.src = 'data:image/png;base64,' + data.img;
      rasterImage_{{PREFIX}}.onload = drawCanvas_{{PREFIX}};
    });

    Shiny.addCustomMessageHandler('adjustCanvasSize_{{PREFIX}}', function(data) {
      adjustCanvasSize_{{PREFIX}}(data.width, data.height);
    });

    function handleDoubleClick_{{PREFIX}}() {
      Shiny.setInputValue('%s', new Date().getTime());
    }

    window.addEventListener('load', initCanvas_{{PREFIX}});
    setTimeout(initCanvas_{{PREFIX}}, 100); // Fallback para carregamento dinâmico
  "

  # Replace {{PREFIX}} with the actual prefix
  js_code <- gsub("\\{\\{PREFIX\\}\\}", prefix, js_template)

  # Insert dynamic IDs using sprintf for remaining placeholders
  js_code <- sprintf(js_code, ids("rasterCanvas"), ids("canvas_size"), ids("drawn_rectangle"), ids("reset_view"))

  tagList(
    tags$head(
      # CSS Estilizado (Card + Checkerboard)
      tags$style(HTML(sprintf("
        /* Container Card Style */
        .pliman-viewer-container-%s {
           background-color: #ffffff;
           border: 1px solid #e2e8f0;
           border-radius: 8px;
           box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06);
           padding: 4px;
           display: inline-block;
           overflow: hidden;
        }

        /* Canvas com fundo Checkerboard */
        #%s {
          display: block;
          cursor: crosshair;
          border-radius: 4px;
          background-image:
            linear-gradient(45deg, #f0f0f0 25%%, transparent 25%%),
            linear-gradient(-45deg, #f0f0f0 25%%, transparent 25%%),
            linear-gradient(45deg, transparent 75%%, #f0f0f0 75%%),
            linear-gradient(-45deg, transparent 75%%, #f0f0f0 75%%);
          background-size: 20px 20px;
          background-position: 0 0, 0 10px, 10px -10px, -10px 0px;
          background-color: white;

          /* Prevenção de seleção de texto */
          -webkit-user-select: none;
          -moz-user-select: none;
          -ms-user-select: none;
          user-select: none;
        }
      ", prefix, ids("rasterCanvas")))),
      tags$script(HTML(js_code))
    ),

    # Envolve o canvas no container estilizado
    div(class = paste0("pliman-viewer-container-", prefix),
        tags$canvas(id = ids("rasterCanvas"), width = width, height = height)
    )
  )
}

# Server module for the viewer
plimanshiny_viewer_server <- function(id, mosaic,
                                      r = reactiveVal(1),
                                      g = reactiveVal(2),
                                      b = reactiveVal(3),
                                      usemargin = reactiveVal(TRUE),
                                      zlim = NULL,
                                      prefix = "mainviewer",
                                      max_width = 1180,
                                      max_height = 800) {
  if(is.na(r())){
    r <- reactiveVal(1)
  }
  if(is.na(g())){
    g <- reactiveVal(2)
  }
  if(is.na(b())){
    b <- reactiveVal(3)
  }
  if(usemargin()){
    marg <- reactiveVal(c(3.1, 3.1, 2.1, 7.1))
  } else{
    marg <- reactiveVal(0)
  }
  moduleServer(id, function(input, output, session) {

    needstretch <- reactiveVal(FALSE)
    original_image_path <- file.path(tempdir(), paste0(id, "_originalimage.png"))

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
      sizes <- adjust_canvas(mosaic, max_width = max_width, max_height = max_height)
      widori(sizes[[1]])
      heiori(sizes[[2]])
      # Send the adjusted canvas size to the client
      session$sendCustomMessage(paste0("adjustCanvasSize_", prefix), list(
        width = as.integer(widori()),
        height = as.integer(heiori())
      ))
    })

    # Generate and cache the original image.
    # the image will be redrawn.
    observe({
      req(g())
      req(r())
      req(b())
      req(mosaic)
      # Register reactive dependencies on the colors:
      sizes <- adjust_canvas(mosaic, max_width = max_width, max_height = max_height)
      tryCatch({
        png(original_image_path, width = sizes[[1]], height = sizes[[2]])
        check_and_plot(mosaic, r = as.numeric(r()), g = as.numeric(g()), b = as.numeric(b()), zlim = zlim())
      }, error = function(e) {
        message("Error in plotting: ", e$message)
      }, finally = {
        dev.off()
      })

      current_extent(ext(mosaic))
      session$sendCustomMessage(paste0("updateTiles_", prefix), list(
        img = base64enc::base64encode(original_image_path)
      ))
    })

    # Reset view to the original raster on button click
    observeEvent(input[[paste0("reset_view_", prefix)]], {
      wid(widori())
      hei(heiori())
      current_extent(ext(mosaic))  # Reset extent to the full raster
      session$sendCustomMessage(paste0("updateTiles_", prefix), list(
        img = base64enc::base64encode(original_image_path)
      ))
      session$sendCustomMessage(paste0("adjustCanvasSize_", prefix), list(
        width = as.integer(widori()),
        height = as.integer(heiori())
      ))
    })

    # Handle rectangle drawing and cropping
    observeEvent(input[[paste0("drawn_rectangle_", prefix)]], {
      rect <- input[[paste0("drawn_rectangle_", prefix)]]
      req(rect$startX)
      if (rect$startX == rect$endX || rect$startY == rect$endY) {
        return(NULL)
      }

      # FIX: Get canvas size from the dynamic input
      canvas_size <- input[[paste0("canvas_size_", prefix)]]

      # Convert canvas coordinates to raster spatial extent
      xmin_val <- terra::xmin(current_extent())
      xmax_val <- terra::xmax(current_extent())
      ymin_val <- terra::ymin(current_extent())
      ymax_val <- terra::ymax(current_extent())
      fact_canva_rast_x <- canvas_size$width / (xmax_val - xmin_val)
      fact_canva_rast_y <- canvas_size$height / (ymax_val - ymin_val)
      xmin <- xmin_val + rect$startX / fact_canva_rast_x
      xmax <- xmin_val + rect$endX / fact_canva_rast_x
      ymin <- ymin_val + (canvas_size$height - rect$endY) / fact_canva_rast_y
      ymax <- ymin_val + (canvas_size$height - rect$startY) / fact_canva_rast_y

      current_extent(ext(c(xmin, xmax, ymin, ymax)))
      xrange <- abs(xmax - xmin)
      originalres <- res(mosaic)[[1]]
      newres <- max(c(xrange / 720, originalres))

      tfc <- file.path(tempdir(), paste0(id, "_", prefix, "_tempcropped.png"))
      session$onSessionEnded(function() {
        if (file.exists(tfc)) {
          resultado_remocao <- suppressWarnings(try(file.remove(tfc), silent = TRUE))
        }
      })
      if(nlyrs() < 3){
        cropped_ras <- crop(mosaic, ext(xmin, xmax, ymin, ymax))
        sizes <- adjust_canvas(cropped_ras, max_width = max_width, max_height = max_height)
        wid(sizes[[1]])
        hei(sizes[[2]])
        tryCatch({
          png(tfc, width = wid(), height = hei())
          terra::plot(
            cropped_ras,
            col = pliman::custom_palette(c("darkred", "yellow", "darkgreen"), n = 100),
            maxcell = 1e6,
            smooth = TRUE,
            legend = "bottomleft",
            mar = marg()
          )
        }, error = function(e) {
          message("Error in plotting: ", e$message)
        }, finally = {
          dev.off()
        })


      } else{
        # Use the reactive colors when re-plotting the cropped image.
        current_r <- as.numeric(r())
        current_g <- as.numeric(g())
        current_b <- as.numeric(b())

        if (is.null(zlim())) {
          # Try using gdal_utils when zlim is NULL
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
            # Fallback if gdal_utils fails
            cropped_ras <- crop(mosaic, ext(xmin, xmax, ymin, ymax))
            sizes <- adjust_canvas(cropped_ras, max_width = max_width, max_height = max_height)
            wid(sizes[[1]])
            hei(sizes[[2]])
            tryCatch({
              png(tfc, width = wid(), height = hei())
              check_and_plot(cropped_ras, r = current_r, g = current_g, b = current_b)
            }, error = function(e) {
              message("Error in plotting: ", e$message)
            }, finally = {
              dev.off()
            })

          } else {
            # Successfully processed by gdal_utils
            cropped_ras <- rast(tfc)
            sizes <- adjust_canvas(cropped_ras, max_width = max_width, max_height = max_height)
            wid(sizes[[1]])
            hei(sizes[[2]])
          }

        } else {
          # Handle case when zlim is not NULL
          cropped_ras <- crop(mosaic, ext(xmin, xmax, ymin, ymax))
          sizes <- adjust_canvas(cropped_ras, max_width = max_width, max_height = max_height)
          wid(sizes[[1]])
          hei(sizes[[2]])
          tryCatch({
            png(tfc, width = wid(), height = hei())
            check_and_plot(cropped_ras, r = current_r, g = current_g, b = current_b, zlim = zlim())
          }, error = function(e) {
            message("Error in plotting: ", e$message)
          }, finally = {
            dev.off()
          })
        }
      }

      session$sendCustomMessage(paste0("updateTiles_", prefix), list(
        img = base64enc::base64encode(tfc)
      ))
      session$sendCustomMessage(paste0("adjustCanvasSize_", prefix), list(
        width = as.integer(wid()),
        height = as.integer(hei())
      ))
    })

  })
}
