#' shapefiletransform UI Function
#' @noRd
mod_shapefiletransform_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      col_4(
        bs4Card(
          title = "Shapefile Manipulation",
          collapsible = FALSE,
          width = 12,
          status = "success",
          selectInput(ns("mosaic_input"), "Background Mosaic", choices = NULL),
          selectInput(ns("shapefile_input"), "Grid to Align", choices = NULL),
          hl(),
          radioGroupButtons(
            inputId = ns("tuning_mode"),
            label = NULL,
            choices = c("Manual Tuning", "Point Alignment"),
            selected = "Manual Tuning",
            justified = TRUE,
            status = "success",
            size = "normal",
            checkIcon = list(yes = icon("check"))
          ),
          # --- TAB 1: MANUAL TUNING ---
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Manual Tuning'", ns("tuning_mode")),
            materialSwitch(
              inputId = ns("use_slider_rot"),
              label = "Use Slider for Rotation",
              value = TRUE,
              status = "success",
              right = TRUE
            ),

            conditionalPanel(
              condition = "input.use_slider_rot == true", ns = ns,
              sliderInput(ns("angle"), "Rotation (°):",
                          min = -180,
                          max = 180,
                          value = 0,
                          step = 0.1,
              )
            ),
            conditionalPanel(
              condition = "input.use_slider_rot == false", ns = ns,
              numericInput(ns("angle"), "Rotation (°):",
                           value = 0,
                           min = -180,
                           max = 180,
                           step = 0.01)
            ),
            hl(),
            materialSwitch(
              inputId = ns("use_slider_shift"),
              label = "Use Slider for Shift",
              value = TRUE,
              status = "success",
              right = TRUE
            ),

            conditionalPanel(
              condition = "input.use_slider_shift == true", ns = ns,
              fluidRow(
                col_6(sliderInput(ns("off_x"), "Shift X (m):", value = 0, min = -10, max = 10, step = 0.01)),
                col_6(sliderInput(ns("off_y"), "Shift Y (m):", value = 0, min = -10, max = 10, step = 0.01))
              )
            ),
            conditionalPanel(
              condition = "input.use_slider_shift == false", ns = ns,
              fluidRow(
                col_6(numericInput(ns("off_x"), "Shift X (m):", value = 0, min = -10, max = 10, step = 0.01)),
                col_6(numericInput(ns("off_y"), "Shift Y (m):", value = 0, min = -10, max = 10, step = 0.01))
              )
            ),
            hl(),
            materialSwitch(
              inputId = ns("use_slider_scale"),
              label = "Use Slider for Scale",
              value = TRUE,
              status = "success",
              right = TRUE
            ),
            materialSwitch(
              inputId = ns("lock_aspect"),
              label = "Lock Aspect Ratio",
              value = FALSE,
              status = "success",
              right = TRUE
            ),
            conditionalPanel(
              condition = "input.use_slider_scale == true", ns = ns,
              fluidRow(
                col_6(sliderInput(ns("scale_x"), "Scale X:", min = 0.01, max = 3, value = 1.0, step = 0.001)),
                col_6(sliderInput(ns("scale_y"), "Scale Y:", min = 0.01, max = 3, value = 1.0, step = 0.001))
              )
            ),
            conditionalPanel(
              condition = "input.use_slider_scale == false", ns = ns,
              fluidRow(
                col_6(numericInput(ns("scale_x"), "Scale X:", value = 1.0, min = 0.01, max = 3, step = 0.001)),
                col_6(numericInput(ns("scale_y"), "Scale Y:", value = 1.0, min = 0.01, max = 3, step = 0.001))
              )
            )
          ),

          # --- TAB 2: POINT ALIGNMENT ---
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Point Alignment'", ns("tuning_mode")),
            div(
              class = "alert alert-info",
              icon("info-circle"), " Instructions:", br(),
              "1. Click 'point1' & select 2 points on Grid.", br(),
              "2. Click 'point2' & select 2 points on Mosaic."
            ),

            fluidRow(
              actionBttn(ns("btn_start_capture"), "Start Sequential Capture", style = "gradient", color = "primary", block = TRUE, icon = icon("crosshairs"))
            ),
            br(),
            uiOutput(ns("points_status_ui")),
            br(),
            actionBttn(ns("btn_calc_transform"), "Apply Transformation", style = "gradient", color = "success", block = TRUE, icon = icon("magic"))
          ),

          hl(),
          materialSwitch(
            inputId = ns("showplotid"),
            label = "Show plot ID?",
            value = FALSE,
            status = "success",
            right = TRUE
          ),
          fluidRow(
            col_6(colorPickr(ns("colorstroke"), "Stroke Color", selected = "#FF0000", theme = "monolith", useAsButton = TRUE)),
            col_6(sliderInput(ns("lwdt"), "Width", min = 1, max = 5, value = 2))
          ),
          hl(),
          textInput(ns("new_shape_name"), "New Object Name", value = ""),
          fluidRow(
            col_6(actionBttn(ns("reset"), "Reset", style = "pill", color = "warning", icon = icon("undo"))),
            col_6(actionBttn(ns("save"), "Save", style = "pill", color = "success", icon = icon("save")))
          ),
          hl(),
          mod_download_shapefile_ui(ns("downloadshpmodified"), label = "Download")
        )
      ),
      col_8(
        div(
          style = "position: relative; width: 100%; height: auto;",
          div(
            style = "position: absolute; top: 0; left: 0; z-index: 1; width: 100%; height: 100%;",
            imageOutput(ns("raster_bg"), height = "100%", width = "100%")
          ),
          div(
            style = "position: relative; z-index: 2;",
            plimanshiny_canvas_output(prefix = "grid_trans", ns = ns, transparent = TRUE, width = 1000, height = 720)
          ),
          conditionalPanel(
            condition = "output.is_capturing_points == true", ns = ns,
            div(
              style = "position: absolute; top: 20px; right: 20px; z-index: 100;
             background-color: rgba(0, 0, 0, 0.75); color: white;
             padding: 15px; border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.3);
             font-family: 'Segoe UI', sans-serif; backdrop-filter: blur(4px); border: 1px solid rgba(255,255,255,0.2);",
              tags$h5(icon("info-circle"), " Instruction", style = "margin-top:0; border-bottom: 1px solid #555; padding-bottom: 5px;"),
              tags$div(
                style = "font-size: 1.1em; font-weight: bold;",
                textOutput(ns("capture_instruction_text"))
              )
            )
          )
        )
      )
    )
  )
}
#' shapefiletransform Server Functions
#' @noRd
mod_shapefiletransform_server <- function(id, mosaic_data, shapefile_data, r, g, b, zlim){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # --- 1. State Variables ---
    current_extent <- reactiveVal()
    canvas_dims <- reactiveValues(w = 1000, h = 720) # Fallback seguro

    # ESTADO SEQUENCIAL: S1 -> T1 -> S2 -> T2
    align_pts <- reactiveValues(
      P1s = NULL, # S1 (Source 1 - Grid)
      P1t = NULL, # T1 (Target 1 - Mosaic)
      P2s = NULL, # S2 (Source 2 - Grid)
      P2t = NULL, # T2 (Target 2 - Mosaic)
      mode = "none" # Estados: "P1s" -> "P1t" -> "P2s" -> "P2t" -> "none"
    )

    output$is_capturing_points <- reactive({ align_pts$mode != "none" })
    outputOptions(output, "is_capturing_points", suspendWhenHidden = FALSE)

    # --- 2. Initial Setup ---
    observe({
      req(mosaic_data)
      valid_mosaics <- Filter(Negate(is.null), reactiveValuesToList(mosaic_data))
      updateSelectInput(session, "mosaic_input", choices = setdiff(names(valid_mosaics), "mosaic"))
    })

    observe({
      req(shapefile_data)
      valid_shapes <- Filter(Negate(is.null), reactiveValuesToList(shapefile_data))
      choices <- setdiff(names(valid_shapes), c("shapefile", "shapefileplot"))
      updateSelectInput(session, "shapefile_input", choices = choices)
    })

    observe({
      req(input$shapefile_input)
      updateTextInput(session, "new_shape_name", value = paste0(input$shapefile_input, "_aligned"))
    })

    observeEvent(input$mosaic_input, {
      req(req(input$mosaic_input))
      req(mosaic_data[[input$mosaic_input]]$data)
      current_extent(terra::ext(mosaic_data[[input$mosaic_input]]$data))
    })

    # --- 3. REACTIVE: TRANSFORMED GRID CALCULATION ---
    transformed_grid_r <- reactive({
      req(input$shapefile_input)
      req(shapefile_data[[input$shapefile_input]]$data)

      # Helper seguro para numéricos
      safe_num <- function(val, default) {
        if (is.null(val) || length(val) == 0 || is.na(val) || !is.numeric(val)) return(default)
        return(val)
      }

      sx  <- safe_num(if(input$use_slider_shift) input$off_x else input$off_x, 0)
      sy  <- safe_num(if(input$use_slider_shift) input$off_y else input$off_y, 0)
      scx <- safe_num(if(input$use_slider_scale) input$scale_x else input$scale_x, 1)
      scy <- safe_num(if(input$use_slider_scale) input$scale_y else input$scale_y, 1)
      ang <- safe_num(input$angle, 0)

      shapefile_transform(
        shapefile = shapefile_data[[input$shapefile_input]]$data,
        shift_x = sx, shift_y = sy, angle = ang, scale_x = scx, scale_y = scy
      )
    })

    # --- 4. VISUALIZATION LOOP (Mosaic + Grid + Points) ---
    observe({
      req(input$mosaic_input, current_extent())
      req(mosaic_data[[input$mosaic_input]]$data)

      # Trigger dependencies
      grid_sf <- tryCatch(transformed_grid_r(), error = function(e) NULL)
      req(grid_sf)

      input$showplotid; input$colorstroke; input$lwdt
      force(align_pts$P1s); force(align_pts$P1t)
      force(align_pts$P2s); force(align_pts$P2t)

      # Preparar Mosaico
      mosaic <- mosaic_data[[input$mosaic_input]]$data
      curr_ext <- current_extent()

      if(curr_ext != terra::ext(mosaic)){
        mosaic_viz <- terra::crop(mosaic, curr_ext)
      } else {
        mosaic_viz <- mosaic
      }

      sizes <- adjust_canvas(mosaic_viz, max_width = 1000, max_height = 720)
      canvas_dims$w <- sizes[[1]]
      canvas_dims$h <- sizes[[2]]

      # Gerar Imagem Única
      tmpfile <- tempfile(fileext = ".png")
      png(tmpfile, width = sizes[[1]], height = sizes[[2]])

      tryCatch({
        # A. Fundo (Mosaico)
        check_and_plot(
          mosaic_viz,
          ifelse(is.na(r$r), 1, r$r), ifelse(is.na(g$g), 2, g$g), ifelse(is.na(b$b), 3, b$b),
          zlim = zlim$zlim
        )

        # B. Grid Vectorial
        plot(sf::st_geometry(grid_sf), add = TRUE, border = input$colorstroke, lwd = input$lwdt, col = NA)

        if(input$showplotid){
          shptoplot <- grid_sf |> extract_number(plot_id)
          centrs <- suppressMessages(sf::st_centroid(grid_sf) |> sf::st_coordinates())
          boxtext(x = centrs[, 1], y = centrs[, 2], labels = shptoplot$plot_id, col.bg = "salmon", cex = 1.5)
        }

        # C. Pontos e Setas (S1->T1 e S2->T2)
        pts <- list(P1s = align_pts$P1s, P1t = align_pts$P1t, P2s = align_pts$P2s, P2t = align_pts$P2t)

        # S1
        if(!is.null(pts$P1s)) {
          points(pts$P1s[1], pts$P1s[2], pch = 3, col = "blue", cex = 4, lwd = 4)
          text(pts$P1s[1], pts$P1s[2], labels = "S1", col = "blue", pos = 3, font=2, cex=2)
        }
        # T1
        if(!is.null(pts$P1t)) {
          points(pts$P1t[1], pts$P1t[2], pch = 4, col = "red", cex = 4, lwd = 4)
          text(pts$P1t[1], pts$P1t[2], labels = "T1", col = "red", pos = 3, font=2, cex=2)
        }
        # Seta S1 -> T1
        if(!is.null(pts$P1s) && !is.null(pts$P1t)) {
          arrows(x0=pts$P1s[1], y0=pts$P1s[2], x1=pts$P1t[1], y1=pts$P1t[2], col="black", lwd=2, length=0.15, angle=25, code=2, lty="dashed")
        }

        # S2
        if(!is.null(pts$P2s)) {
          points(pts$P2s[1], pts$P2s[2], pch = 3, col = "blue", cex = 4, lwd = 4)
          text(pts$P2s[1], pts$P2s[2], labels = "S2", col = "blue", pos = 3, font=2, cex=2)
        }
        # T2
        if(!is.null(pts$P2t)) {
          points(pts$P2t[1], pts$P2t[2], pch = 4, col = "red", cex = 4, lwd = 4)
          text(pts$P2t[1], pts$P2t[2], labels = "T2", col = "red", pos = 3, font=2, cex=2)
        }
        # Seta S2 -> T2
        if(!is.null(pts$P2s) && !is.null(pts$P2t)) {
          arrows(x0=pts$P2s[1], y0=pts$P2s[2], x1=pts$P2t[1], y1=pts$P2t[2], col="black", lwd=2, length=0.15, angle=25, code=2, lty="dashed")
        }

      }, finally = {
        dev.off()
      })

      session$sendCustomMessage("updateTiles_grid_trans", list(img = base64enc::base64encode(tmpfile)))
      session$sendCustomMessage("adjustcanvas_grid_transSize", list(width = as.integer(sizes[[1]]), height = as.integer(sizes[[2]])))
    })

    # --- 5. POINT CAPTURE LOGIC ---
    observeEvent(input$picked_point_grid_trans, {
      req(align_pts$mode != "none")

      point <- input$picked_point_grid_trans # c(x, y) pixels
      req(point)

      # Dimensões e Extensão
      curr_ext <- current_extent()
      xmin_val <- terra::xmin(curr_ext); xmax_val <- terra::xmax(curr_ext)
      ymin_val <- terra::ymin(curr_ext); ymax_val <- terra::ymax(curr_ext)

      # FIX: Fallback se o input do cliente for NULL
      cw <- if(!is.null(input$canvas_size_grid_trans$width)) input$canvas_size_grid_trans$width else canvas_dims$w
      ch <- if(!is.null(input$canvas_size_grid_trans$height)) input$canvas_size_grid_trans$height else canvas_dims$h

      # Conversão de Coordenadas
      x_canvas <- point[1]
      y_canvas <- point[2]

      x_raster <- xmin_val + (x_canvas / cw) * (xmax_val - xmin_val)
      y_raster <- ymin_val + ((ch - y_canvas) / ch) * (ymax_val - ymin_val)

      pt <- c(x = x_raster, y = y_raster)

      # Armazenar Pontos
      # Máquina de Estados
      if(align_pts$mode == "P1s"){
        align_pts$P1s <- pt
        align_pts$mode <- "P1t"
        showNotification("S1 (Grid) Captured. Now click T1 (Mosaic).", type = "warning", duration = 4)

      } else if(align_pts$mode == "P1t"){
        align_pts$P1t <- pt
        align_pts$mode <- "P2s"
        showNotification("T1 (Mosaic) Captured. Now click S2 (Grid).", type = "message", duration = 4)

      } else if(align_pts$mode == "P2s"){
        align_pts$P2s <- pt
        align_pts$mode <- "P2t"
        showNotification("S2 (Grid) Captured. Now click T2 (Mosaic).", type = "warning", duration = 4)

      } else if(align_pts$mode == "P2t"){
        align_pts$P2t <- pt
        align_pts$mode <- "none"
        sendSweetAlert(session, title = "Ready", text = "All points captured. Click 'Apply Transformation'.", type = "success")
      }
    })

    # --- 6. Zoom Logic (FIXED) ---
    observeEvent(input$drawn_rectangle_grid_trans, {
      rect <- input$drawn_rectangle_grid_trans

      # FIX CRÍTICO: Removido 'if(align_pts$mode != "none") return()'
      # Isso permite zoom mesmo durante a captura de pontos

      req(rect$startX, current_extent())

      # Ignora cliques pequenos (confusão com pontos)
      if(abs(rect$startX - rect$endX) < 5 || abs(rect$startY - rect$endY) < 5) return()

      curr_ext <- current_extent()
      cw <- if(!is.null(input$canvas_size_grid_trans$width)) input$canvas_size_grid_trans$width else canvas_dims$w
      ch <- if(!is.null(input$canvas_size_grid_trans$height)) input$canvas_size_grid_trans$height else canvas_dims$h

      fact_x <- cw / (terra::xmax(curr_ext) - terra::xmin(curr_ext))
      fact_y <- ch / (terra::ymax(curr_ext) - terra::ymin(curr_ext))

      new_xmin <- terra::xmin(curr_ext) + rect$startX / fact_x
      new_xmax <- terra::xmin(curr_ext) + rect$endX / fact_x
      new_ymin <- terra::ymin(curr_ext) + (ch - rect$endY) / fact_y
      new_ymax <- terra::ymin(curr_ext) + (ch - rect$startY) / fact_y

      # Atualiza extensão (isso dispara o Observer visual)
      current_extent(terra::ext(c(new_xmin, new_xmax, new_ymin, new_ymax)))

      # Atualiza dimensões do canvas no servidor para garantir que o próximo clique seja preciso
      canvas_dims$w <- cw
      canvas_dims$h <- ch
    })

    observeEvent(input$reset_view_grid_trans, {
      req(input$mosaic_input)
      current_extent(terra::ext(mosaic_data[[input$mosaic_input]]$data))
    })

    # --- 7. Setup Buttons ---
    observeEvent(input$btn_start_capture, {
      align_pts$P1s <- NULL; align_pts$P1t <- NULL; align_pts$P2s <- NULL; align_pts$P2t <- NULL;
      align_pts$mode <- "P1s"
      showNotification("Click S1 on the Vector Grid", type = "message", duration = 5)
    })

    output$points_status_ui <- renderUI({
      p1s <- !is.null(align_pts$P1s); p1t <- !is.null(align_pts$P1t)
      p2s <- !is.null(align_pts$P2s); p2t <- !is.null(align_pts$P2t)

      tagList(div(style = "display: flex; justify-content: space-around;",
                  span(class = ifelse(p1s, "badge badge-success", "badge badge-primary"), "S1", icon(ifelse(p1s, "check", "circle"))),
                  span(class = ifelse(p1t, "badge badge-success", "badge badge-danger"), "T1", icon(ifelse(p1t, "check", "circle"))),
                  span(class = ifelse(p2s, "badge badge-success", "badge badge-primary"), "S2", icon(ifelse(p2s, "check", "circle"))),
                  span(class = ifelse(p2t, "badge badge-success", "badge badge-danger"), "T2", icon(ifelse(p2t, "check", "circle")))))
    })

    output$capture_instruction_text <- renderText({
      switch(align_pts$mode,
             "P1s" = "Click S1 (Grid)",
             "P1t" = "Click T1 (Mosaic)",
             "P2s" = "Click S2 (Grid)",
             "P2t" = "Click T2 (Mosaic)",
             "Ready")
    })

    # --- 8. Calculation Logic ---
    observeEvent(input$btn_calc_transform, {
      req(align_pts$P1s, align_pts$P1t, align_pts$P2s, align_pts$P2t)

      P1s <- as.numeric(align_pts$P1s); P2s <- as.numeric(align_pts$P2s)
      P1m <- as.numeric(align_pts$P1t); P2m <- as.numeric(align_pts$P2t)

      # Cálculos de Escala e Rotação
      len_s <- sqrt((P2s[1] - P1s[1])^2 + (P2s[2] - P1s[2])^2)
      len_m <- sqrt((P2m[1] - P1m[1])^2 + (P2m[2] - P1m[2])^2)
      new_scale <- len_m / len_s

      angle_s <- atan2(P2s[2] - P1s[2], P2s[1] - P1s[1])
      angle_m <- atan2(P2m[2] - P1m[2], P2m[1] - P1m[1])
      rot_rad <- angle_m - angle_s
      new_angle_deg <- rot_rad * (180 / pi)
      if(new_angle_deg > 180) new_angle_deg <- new_angle_deg - 360
      if(new_angle_deg < -180) new_angle_deg <- new_angle_deg + 360

      # Cálculo do Shift (com pivô no centróide)
      req(shapefile_data[[input$shapefile_input]]$data)
      shp <- shapefile_data[[input$shapefile_input]]$data
      centroid_geom <- sf::st_centroid(sf::st_geometry(sf::st_union(shp)))
      centroid <- sf::st_coordinates(centroid_geom) |> as.numeric()

      vec_C_to_P1s_x <- P1s[1] - centroid[1]
      vec_C_to_P1s_y <- P1s[2] - centroid[2]

      cos_t <- cos(rot_rad);
      sin_t <- sin(rot_rad)

      vec_final_x <- (vec_C_to_P1s_x * cos_t - vec_C_to_P1s_y * sin_t) * new_scale
      vec_final_y <- (vec_C_to_P1s_x * sin_t + vec_C_to_P1s_y * cos_t) * new_scale

      P1s_predicted_x <- centroid[1] + vec_final_x
      P1s_predicted_y <- centroid[2] + vec_final_y

      shift_x <- P1m[1] - P1s_predicted_x
      shift_y <- P1m[2] - P1s_predicted_y

      # Atualiza UI
      updateSliderInput(session, "scale_x", value = new_scale)
      updateSliderInput(session, "scale_y", value = new_scale)
      updateSliderInput(session, "angle", value = -new_angle_deg)

      if(abs(shift_x) > 10 || abs(shift_y) > 10){
        updatePrettyCheckbox(session, "use_slider_shift", value = FALSE)
      }
      updateNumericInput(session, "off_x", value = shift_x)
      updateNumericInput(session, "off_y", value = shift_y)
      updateSliderInput(session, "off_x", value = shift_x)
      updateSliderInput(session, "off_y", value = shift_y)

      updateRadioButtons(session, "tuning_mode", selected = "Manual Tuning")

      align_pts$point1 <- list(); align_pts$point2 <- list(); align_pts$mode <- "none"
    })

    # --- 9. Helpers & Save ---
    # 1. Observer: Listen only for changes in scale_x
    observeEvent(input$scale_x, {
      req(input$lock_aspect, input$scale_y)
      if (input$scale_y != input$scale_x) {
        updateSliderInput(session, "scale_y", value = input$scale_x)
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    observeEvent(input$scale_y, {
      req(input$lock_aspect, input$scale_x)
      if (input$scale_x != input$scale_y) {
        updateSliderInput(session, "scale_x", value = input$scale_y)
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)


    observeEvent(input$reset, {
      updateSliderInput(session, "angle", value = 0)
      updateNumericInput(session, "off_x", value = 0); updateNumericInput(session, "off_y", value = 0)
      updateNumericInput(session, "scale_x", value = 1.0); updateNumericInput(session, "scale_y", value = 1.0)
      align_pts$P1s=NULL; align_pts$P1t=NULL; align_pts$P2s=NULL; align_pts$P2t=NULL; align_pts$mode="none"
    })

    observeEvent(input$save, {
      req(input$new_shape_name, transformed_grid_r())
      shapefile_data[[input$new_shape_name]] <- create_reactval(name = input$new_shape_name, data = transformed_grid_r())
      sendSweetAlert(session, title = "Saved!", text = "Grid updated successfully.", type = "success")
    })

    observeEvent(input$new_shape_name, {
      req(transformed_grid_r())
      mod_download_shapefile_server("downloadshpmodified", data = terra::vect(transformed_grid_r()), name = input$new_shape_name)
    })


  })
}
