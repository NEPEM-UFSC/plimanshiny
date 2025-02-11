mod_slider_zoom_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("slider"))
  )
}

mod_slider_zoom_server <- function(id, img1,name1, rgb1, r1, g1, b1, zlim1, img2, name2, rgb2, r2, g2, b2, zlim2) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    reactive_values <- reactiveValues(drawn_rectangle = NULL, canvas_size = NULL)

    output$slider <- renderUI({

      req(img1)
      dims <- round(adjust_canvas(img1, max_width = 1080, max_height = 750))

      tfbef <- file.path(tempdir(), "beforeimg.png")

      png(tfbef, width = dims[1], height = dims[2])
      stretch1 <- if(is.null(zlim1)){NULL} else {"lin"}
      if (rgb1) {
        plotRGB(img1,
                r = r1,
                g = g1,
                b = b1,
                zlim = zlim1,
                stretch = stretch1)
      } else {
        plot(img1,
             col = pliman::custom_palette(c("darkred",  "yellow", "darkgreen"), n = 100),
             maxcell = 1e6,
             mar = 0,
             smooth = TRUE)
      }
      dev.off()



      ############# RIGHT IMAGE ############

      tfaft <- file.path(tempdir(), "afterimg.png")
      png(tfaft, width = dims[1], height = dims[2])
      stretch2 <- if(is.null(zlim2)){NULL} else {"lin"}
      if (rgb2) {
        plotRGB(img2,
                r = r2,
                g = g2,
                b = b2,
                zlim = zlim2,
                stretch = stretch2)
      } else {
        plot(img2,
             col = pliman::custom_palette(c("darkred",  "yellow", "darkgreen"), n = 100),
             maxcell = 1e6,
             mar = 0,
             smooth = TRUE)
      }
      dev.off()

      img_before_base64 <- base64enc::base64encode(tfbef)
      img_after_base64 <- base64enc::base64encode(tfaft)

      session$sendCustomMessage("adjustCanvasSize_dualviewer",
                                list(width = dims[1], height = dims[2]))

      HTML(
        paste0(
          '<div id="comparison" style="width:', dims[1], 'px; height:', dims[2], 'px; margin-bottom: 10px;">
            <style>
              div#comparison {
                overflow: hidden;
                position: relative;
                box-shadow: 3px 3px 8px rgba(40, 167,69, 0.3);
              }
              div#comparison figure {
                background-image: url("data:image/png;base64,', img_after_base64, '");
                background-size: cover;
                position: relative;
                width: 100%;
                height: 100%;
                margin: 0;
              }
              div#comparison figure div#divisor {
                background-image: url("data:image/png;base64,', img_before_base64, '");
                background-size: cover;
                position: absolute;
                width: 50%;
                height: 100%;
                overflow: hidden;
              }
              div#divisionBar {
                position: absolute;
                right: 0;
                top: 0;
                width: 4px;
                height: 100%;
                background: rgba(40, 167, 69, 0.6);
                z-index: 20;
              }
              canvas#drawCanvas {
                position: absolute;
                top: 0;
                left: 0;
                z-index: 10;
                pointer-events: auto;
                width: 100%;
                height: 100%;
              }
              input[type=range] {
                -webkit-appearance: none;
                -moz-appearance: none;
                width: ', dims[1], 'px;
                margin-top: 10px;
                height: 12px;
                background: rgba(40, 167, 69, 0.4);
                border-radius: 4px;

              }
              input[type=range]::-webkit-slider-thumb {
                -webkit-appearance: none;
                width: 20px;
                height: 20px;
                border: none !important;
                border-radius: 50% !important;
                background: rgba(40, 167, 69, 1);
                cursor: pointer;
                transition: transform 0.3s ease;
              }
              input[type=range]::-webkit-slider-thumb:hover {
                transform: scale(2);
              }
              input[type=range]:focus {
                outline: none;
              }
            /* Style for labels */
              .canvas-label {
                position: absolute;
                font-weight: bold;
                font-size: 16px;
                color: white;
                background: rgba(40, 167, 69, 0.6);
                padding: 4px 8px;
                border-radius: 4px;
                z-index: 30;
              }
              #label-left {
                top: 10px;
                left: 10px;
              }
              #label-right {
                top: 10px;
                right: 10px;
              }
            </style>
      <!-- Labels for the images -->
      <div id="label-left" class="canvas-label">', name1, '</div>
      <div id="label-right" class="canvas-label">', name2, '</div>
            </style>
            <figure>
              <div id="divisor">
                <div id="divisionBar"></div>
              </div>
              <canvas id="drawCanvas"></canvas>
            </figure>
          </div>
          <!-- The slider placed below the canvas -->
          <input type="range" min="0" max="100" value="50" id="slider" oninput="moveDivisor()" style="width:', dims[1], 'px;">
          <script>
            function moveDivisor() {
              var slider = document.getElementById("slider");
              var divisor = document.getElementById("divisor");
              var divisionBar = document.getElementById("divisionBar");
              divisor.style.width = slider.value + "%";
              divisionBar.style.right = "0";
            }

            function resizeCanvas() {
              var comparison = document.getElementById("comparison");
              var drawCanvas = document.getElementById("drawCanvas");
              drawCanvas.width = comparison.clientWidth;
              drawCanvas.height = comparison.clientHeight;
              Shiny.setInputValue("', ns("canvas_size"), '", {
                width: drawCanvas.width,
                height: drawCanvas.height
              }, {priority: "event"});
            }

            Shiny.addCustomMessageHandler("adjustCanvasSize_dualviewer", function(data) {
              var comparison = document.getElementById("comparison");
              comparison.style.width = data.width + "px";
              comparison.style.height = data.height + "px";
              setTimeout(resizeCanvas, 100);
            });

            resizeCanvas();
            window.addEventListener("resize", resizeCanvas);

            var drawCanvas = document.getElementById("drawCanvas");
            var ctx = drawCanvas.getContext("2d");
            var drawing = false;
            var rectStartX, rectStartY, rectEndX, rectEndY;

            drawCanvas.addEventListener("mousedown", function(e) {
              var rect = drawCanvas.getBoundingClientRect();
              rectStartX = e.clientX - rect.left;
              rectStartY = e.clientY - rect.top;
              drawing = true;
              ctx.clearRect(0, 0, drawCanvas.width, drawCanvas.height);
            });

            drawCanvas.addEventListener("mousemove", function(e) {
              if (!drawing) return;
              var rect = drawCanvas.getBoundingClientRect();
              rectEndX = e.clientX - rect.left;
              rectEndY = e.clientY - rect.top;
              ctx.clearRect(0, 0, drawCanvas.width, drawCanvas.height);
              ctx.strokeStyle = "red";
              ctx.lineWidth = 2;
              ctx.strokeRect(rectStartX, rectStartY, rectEndX - rectStartX, rectEndY - rectStartY);
            });

drawCanvas.addEventListener("mouseup", function() {
  if (!drawing) return;
  drawing = false;

  // Calculate rectangle dimensions
  const rectWidth = Math.abs(rectEndX - rectStartX);
  const rectHeight = Math.abs(rectEndY - rectStartY);

  // Check if rectangle is valid (width and height >= 10px)
  if (rectWidth < 10 || rectHeight < 10) {
    console.log("Invalid rectangle: Too small");
    Shiny.setInputValue("', ns("drawn_rectangle"), '", {
      startX: 0,
      startY: 0,
      endX: 0,
      endY: 0,
      width: 0,
      height: 0
    }, {priority: "event"});
  } else {
    // Valid rectangle
    Shiny.setInputValue("', ns("drawn_rectangle"), '", {
      startX: Math.min(rectStartX, rectEndX),
      startY: Math.min(rectStartY, rectEndY),
      endX: Math.max(rectStartX, rectEndX),
      endY: Math.max(rectStartY, rectEndY),
      width: rectWidth,
      height: rectHeight
    }, {priority: "event"});
  };
  rectStartX = rectStartY = rectEndX = rectEndY = 0;

});

          </script>'
        )
      )
    })

    session$onSessionEnded(function() {
      f1 <- list.files(path = paste0(system.file("app", package = "plimanshiny" ), "/www/"),
                       pattern = "beforeimg_")
      f2 <- list.files(path = paste0(system.file("app", package = "plimanshiny" ), "/www/"),
                       pattern = "afterimg_")
      if(any(c(length(f1), length(f2)) != 0)){
        tmpimages <- paste0(paste0(system.file("app", package = "plimanshiny" ), "/www/"), c(f1, f2))
        a <- sapply(tmpimages, file.remove)
      }
    })
    # Observe inputs and store them in reactive_values
    observeEvent(input$drawn_rectangle, {
      reactive_values$drawn_rectangle <- input$drawn_rectangle
    })
    observeEvent(input$canvas_size, {
      reactive_values$canvas_size <- input$canvas_size
    })
    reactive(reactive_values)

  })
}
