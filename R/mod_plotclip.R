#' plotclip UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plotclip_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Mask / Clip Settings",
          collapsible = FALSE,
          width = 12,
          h3("Input"),
          selectInput(ns("mosaic_to_clip"),
                      label = "Mosaic to be clipped",
                      choices = NULL),
          selectInput(ns("shape_to_clip"),
                      label = "Shapefile",
                      choices = NULL),
          selectInput(ns("uniqueid"),
                      label = "Unique ID (column in shapefile)",
                      choices = NULL),

          fluidRow(
            col_7(
              switchInput(
                inputId = ns("clipinparallel"),
                label = "Parallel",
                onLabel = "Yes",
                offLabel = "No",
                labelWidth = "80px"
              )
            ),
            col_5(
              conditionalPanel(
                condition = "input.clipinparallel == true",  ns = ns,
                numericInput(ns("numworkersclip"),
                             label = "Workers",
                             value = NULL, min = 1, step = 1)
              )
            )
          ),
          actionBttn(ns("startclip"),
                     label = "Generate Field Map",
                     style = "pill",
                     color = "success"),

          hl(),
          h3("Options"),
          prettyCheckbox(
            inputId = ns("exact_cut"),
            label = "Exact clip (GDAL cutline)",
            value = FALSE, status = "primary", outline = TRUE, animation = "pulse"
          ),
          prettyCheckbox(
            inputId = ns("overwrite_files"),
            label = "Overwrite existing files",
            value = TRUE, status = "primary", outline = TRUE, animation = "pulse"
          ),
          prettyCheckbox(
            inputId = ns("verbose_clip"),
            label = "Verbose messages",
            value = TRUE, status = "primary", outline = TRUE, animation = "pulse"
          ),

          hl(),
          h3("Output"),
          shinyDirButton(id=ns("folderclip"),
                         label="Select an output folder",
                         title="Select an output folder"),
          selectInput(ns("clipformat"),
                      label = "Format",
                      choices = c(".tif", ".png"),
                      selected = ".tif"),
          prettyCheckbox(
            inputId = ns("seeaclippedplot"),
            label = "Show me a clipped plot",
            value = FALSE,
            status = "info",
            icon = icon("thumbs-up"),
            plain = TRUE,
            outline = TRUE,
            animation = "rotate"
          ),
          conditionalPanel(
            condition = "input.seeaclippedplot == true", ns = ns,
            selectizeInput(ns("myclippedplot"),
                           label = "Clipped Plot",
                           choices = NULL)
          ),
          actionBttn(ns("clipmosaic"),
                     label = "Clip!",
                     style = "pill",
                     no_outline = FALSE,
                     icon = icon("scissors"),
                     color = "success")
        )
      ),
      col_9(
        bs4Card(
          title = "Clip Results",
          collapsible = FALSE,
          width = 12,
          height = "760px",
          conditionalPanel(
            condition = "input.seeaclippedplot == true", ns = ns,
            h3("Clipped Plot"),
            plotOutput(ns("clippedplot"), height = "670px") |> add_spinner()
          ),
          conditionalPanel(
            condition = "input.seeaclippedplot == false", ns = ns,
            leafletOutput(ns("mosaicandshape"), height = "670px") |> add_spinner()
          )
        )
      )
    )
  )
}


#' plotclip Server Functions
#'
#' @noRd
mod_plotclip_server <- function(id, mosaic_data, shapefile, r, g, b, basemap, settings){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # populate selectors
    observe({
      req(mosaic_data)
      updateSelectInput(session, "mosaic_to_clip",
                        choices = c("Active mosaic", setdiff(names(mosaic_data), "mosaic")),
                        selected = "Active mosaic")
      updateSelectInput(session, "shape_to_clip",
                        choices = setdiff(names(shapefile), c("shapefile", "shapefileplot")))
      availablecl <- parallel::detectCores()
      updateNumericInput(session, "numworkersclip",
                         value = max(1, round(availablecl * 0.5)),
                         max = max(1, availablecl - 1))
    })
    mosaictocrop <- reactiveVal(NULL)
    shptocrop <- reactiveVal(NULL)

    volumes <- c("R Installation" = R.home(), getVolumes()())
    shinyDirChoose(input, "folderclip", roots = volumes, session = session,
                   restrictions = system.file(package = "base"))

    # update the Unique ID choices whenever the shapefile selection changes
    observeEvent(input$shape_to_clip, {
      req(input$shape_to_clip)
      shp_sf <- shapefile[[input$shape_to_clip]]$data
      req(shp_sf)

      # attribute (non-geometry) columns only
      cols <- colnames(sf::st_drop_geometry(shp_sf))

      # pick a sensible default if present
      preferred <- c("unique_id", "plot_id", "id", "ID", "plot", "name")
      default_sel <- if (length(intersect(preferred, cols))) {
        intersect(preferred, cols)[1]
      } else {
        cols[1]
      }

      updateSelectInput(session, "uniqueid",
                        choices  = cols,
                        selected = default_sel)
    })


    observeEvent(input$startclip, {
      sendSweetAlert(
        session = session,
        title = "Clipping plots",
        text  = "Choose an output directory, select the unique ID column, then click 'Clip!'.",
        type  = "info"
      )
      req(input$mosaic_to_clip, input$shape_to_clip)
      shptocrop <- shapefile[[input$shape_to_clip]]$data
      if (input$mosaic_to_clip == "Active mosaic" && !is.null(basemap$map)) {
        mosaictocrop(mosaic_data$mosaic$data)
        bcrop        <- basemap$map
      } else {
        bcrop <-
          mosaic_view(
            mosaic_data[[input$mosaic_to_clip]]$data,
            r = ifelse(is.na(r$r), 1, suppressWarnings(as.numeric(r$r))),
            g = ifelse(is.na(g$g), 2, suppressWarnings(as.numeric(g$g))),
            b = ifelse(is.na(b$b), 3, suppressWarnings(as.numeric(b$b))),
            max_pixels = 500000
          )
        mosaictocrop(mosaic_data[[input$mosaic_to_clip]]$data)
      }

      output$mosaicandshape <- renderLeaflet({
        req(bcrop)
        (bcrop + shapefile_view(shptocrop))@map
      })
    })


    # main action
    observeEvent(input$clipmosaic, {
      req(input$mosaic_to_clip, input$shape_to_clip, input$uniqueid)
      diroutput <- parseDirPath(volumes, input$folderclip)
      if (is.null(diroutput) || identical(diroutput, "character(0)")) {
        sendSweetAlert(
          session = session,
          title = "Output folder not defined",
          text  = "Choose an output directory using the 'Select an output folder' button.",
          type  = "error"
        )
        return(invisible(NULL))
      }

      shptocrop(shapefile[[input$shape_to_clip]]$data)
      req(shptocrop())

      # ids & validation
      if (!input$uniqueid %in% names(shptocrop())) {
        sendSweetAlert(session, "Invalid unique ID",
                       paste0("Column '", input$uniqueid, "' not found in shapefile."),
                       type = "error")
        return(invisible(NULL))
      }
      ids <- as.character(shptocrop()[[input$uniqueid]])
      if (length(unique(ids)) != length(ids)) {
        sendSweetAlert(session, "Non-unique IDs",
                       "The selected Unique ID column must have unique values.",
                       type = "error")
        return(invisible(NULL))
      }

      # prepare mosaic path on disk
      tmpterra <- tempdir()
      tf_rast  <- file.path(tmpterra, "tmpclip_src.tif")
      if (terra::inMemory(mosaictocrop())) {
        terra::writeRaster(mosaictocrop(), tf_rast, overwrite = TRUE)
        src_path <- tf_rast
      } else {
        src_path <- terra::sources(mosaictocrop())
        if (length(src_path) > 1) {
          # write VRT if multiple sources
          src_path <- file.path(tmpterra, "tmpclip_src.vrt")
          terra::writeRaster(mosaictocrop(), src_path, overwrite = TRUE)
        }
      }

      # pre-check overwrite
      ext_chosen <- input$clipformat
      out_paths  <- file.path(diroutput, paste0(ids, ext_chosen))
      if (any(basename(out_paths) %in% list.files(diroutput)) && !isTRUE(input$overwrite_files)) {
        sendSweetAlert(session, "Files already exist",
                       "Some output tiles already exist. Enable 'Overwrite existing files' to replace.",
                       type = "error")
        return(invisible(NULL))
      }

      # sequential mode --------------------------------------------------------
      if (!isTRUE(input$clipinparallel)) {
        progressSweetAlert(
          session = session, id = "myprogressclip",
          title = "Plimanshiny is fitting the growth models...",
          display_pct = TRUE, value = 0, total = nrow(shptocrop())
        )

        for (i in seq_len(nrow(shptocrop()))) {
          if (!is.null(session) && requireNamespace("shinyWidgets", quietly = TRUE)) {
            shinyWidgets::updateProgressBar(
              session = session,
              id      = "myprogressclip",
              value   = i,
              title   = "Plimanshiny is fitting the growth models...",
              total   = nrow(shptocrop())
            )
          }

          # build options for gdal warp
          if (isTRUE(input$exact_cut)) {
            cutline_path <- tempfile(fileext = ".geojson")
            sf::st_write(shptocrop()[i, ], cutline_path, driver = "GeoJSON", quiet = TRUE)
            opts <- c("-cutline", cutline_path, "-crop_to_cutline", "-dstalpha",
                      if (isTRUE(input$overwrite_files)) "-overwrite")
            tmp_out <- if (ext_chosen == ".tif") out_paths[i] else tempfile(fileext = ".tif")
            suppressWarnings(
              sf::gdal_utils("warp", source = src_path, destination = tmp_out, options = opts)
            )
            if (ext_chosen == ".png") {
              sf::gdal_utils("translate", source = tmp_out, destination = out_paths[i], options = character())
              unlink(tmp_out)
            }
            unlink(cutline_path)
          } else {
            bb <- sf::st_bbox(shptocrop()[i, ])
            opts <- c("-te", bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]],
                      if (isTRUE(input$overwrite_files)) "-overwrite")
            tmp_out <- if (ext_chosen == ".tif") out_paths[i] else tempfile(fileext = ".tif")
            suppressWarnings(
              sf::gdal_utils("warp", source = src_path, destination = tmp_out, options = opts)
            )
            if (ext_chosen == ".png") {
              sf::gdal_utils("translate", source = tmp_out, destination = out_paths[i], options = character())
              unlink(tmp_out)
            }
          }
        }
        if (!is.null(session) && requireNamespace("shinyWidgets", quietly = TRUE)) {
          shinyWidgets::closeSweetAlert(session)
        }

        # parallel (mirai) -------------------------------------------------------
      } else {
        req(input$numworkersclip)
        nworkers <- input$numworkersclip

        waiter_show(
          html = tagList(
            spin_google(),
            h2(paste0("Clipping plots with mirai (", nworkers, " workers). Please, wait."))
          ),
          color = "#228B227F"
        )

        mirai::daemons(nworkers)
        on.exit({ mirai::daemons(0); waiter_hide() }, add = TRUE)

        idxs <- seq_len(nrow(shptocrop()))
        chunks <- split(idxs, rep(seq_len(nworkers), length.out = length(idxs)))

        results <- mirai::mirai_map(
          .x = chunks,
          .f = function(chunk, shp, ids, src_path, odir, ext_chosen, overwrite, exact){
            # ensure deps
            # requireNamespace("sf", quietly = TRUE)

            outv <- character(length(chunk))
            for (k in seq_along(chunk)) {
              i <- chunk[k]
              if (isTRUE(exact)) {
                cutline_path <- tempfile(fileext = ".geojson")
                sf::st_write(shp[i, ], cutline_path, driver = "GeoJSON", quiet = TRUE)
                opts <- c("-cutline", cutline_path, "-crop_to_cutline", "-dstalpha",
                          if (isTRUE(overwrite)) "-overwrite")
              } else {
                bb <- sf::st_bbox(shp[i, ])
                opts <- c("-te", bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]],
                          if (isTRUE(overwrite)) "-overwrite")
              }

              base_out <- file.path(odir, ids[i])
              if (identical(ext_chosen, ".tif")) {
                dest <- paste0(base_out, ".tif")
                suppressWarnings(sf::gdal_utils("warp", src_path, dest, options = opts))
                outv[k] <- dest
              } else {
                tmp_tif <- tempfile(fileext = ".tif")
                suppressWarnings(sf::gdal_utils("warp", src_path, tmp_tif, options = opts))
                dest_png <- paste0(base_out, ".png")
                suppressWarnings(sf::gdal_utils("translate", tmp_tif, dest_png, options = character()))
                unlink(tmp_tif)
                outv[k] <- dest_png
              }
              if (isTRUE(exact)) unlink(cutline_path)
            }
            outv
          },
          .args = list(
            shp         = shptocrop(),
            ids         = ids,
            src_path    = src_path,
            odir        = diroutput,
            ext_chosen  = ext_chosen,
            overwrite   = input$overwrite_files,
            exact       = input$exact_cut
          )
        )[.progress]

        invisible(results) # paths are returned if you need them
      }

      # cleanup aux PNG sidecars created by GDAL/terra (best-effort)
      aux <- list.files(diroutput, pattern = "png\\.aux(\\.xml)?$", full.names = TRUE)
      if (length(aux)) file.remove(aux)

      sendSweetAlert(
        session = session,
        title = "Clipping finished!",
        text  = paste0("Tiles saved at: ", diroutput),
        type  = "success"
      )
    })

    # preview of a single clipped tile
    observeEvent(input$seeaclippedplot, {
      req(input$seeaclippedplot)
      req(input$uniqueid, input$shape_to_clip, input$mosaic_to_clip)

      shptocrop <- shapefile[[input$shape_to_clip]]$data
      ncolid    <- which(colnames(shptocrop) == input$uniqueid)
      plots     <- shptocrop |> as.data.frame() |> dplyr::pull(ncolid)

      updateSelectizeInput(session, "myclippedplot",
                           options = list(maxOptions = 20000),
                           server  = TRUE,
                           choices = plots)

      req(input$myclippedplot)

      # display clipped area over original mosaic AND the clipped tile (if exists)
      shptoplot <- shptocrop[which(plots == input$myclippedplot), ]
      # if (input$mosaic_to_clip == "Active mosaic" && !is.null(basemap$map)) {
      #   bcrop <- basemap$map
      # } else {
      #   bcrop <-
      #     mosaic_view(
      #       mosaic_data[[input$mosaic_to_clip]]$data,
      #       r = ifelse(is.na(r$r), 1, suppressWarnings(as.numeric(r$r))),
      #       g = ifelse(is.na(g$g), 2, suppressWarnings(as.numeric(g$g))),
      #       b = ifelse(is.na(b$b), 3, suppressWarnings(as.numeric(b$b))),
      #       max_pixels = 500000
      #     )
      # }
      #
      # output$mosaicandshapeclipped <- renderLeaflet({
      #   (bcrop + shapefile_view(shptoplot))@map
      # })

      # If output exists, show it; otherwise show on-the-fly crop for preview
      observeEvent(input$myclippedplot, {
        diroutput <- parseDirPath(volumes, input$folderclip)
        if (!is.null(diroutput) && !identical(diroutput, "character(0)")) {
          path_try <- file.path(diroutput, paste0(input$myclippedplot, input$clipformat))
          if (file.exists(path_try)) {
            output$clippedplot <- renderPlot({
              mosaic_plot_rgb(
                terra::rast(path_try),
                r = ifelse(is.na(r$r), 1, suppressWarnings(as.numeric(r$r))),
                g = ifelse(is.na(g$g), 2, suppressWarnings(as.numeric(g$g))),
                b = ifelse(is.na(b$b), 3, suppressWarnings(as.numeric(b$b)))
              )
            })
            return(invisible(NULL))
          }
        }
        # fallback: compute quick crop for preview
        motemp <-
          terra::crop(mosaic_data[[input$mosaic_to_clip]]$data, shptoplot) |>
          terra::mask(shptoplot)
        output$clippedplot <- renderPlot({
          mosaic_plot_rgb(
            motemp,
            r = ifelse(is.na(r$r), 1, suppressWarnings(as.numeric(r$r))),
            g = ifelse(is.na(g$g), 2, suppressWarnings(as.numeric(g$g))),
            b = ifelse(is.na(b$b), 3, suppressWarnings(as.numeric(b$b)))
          )
        })
      })
    })
  })
}

## To be copied in the UI
# mod_plotclip_ui("plotclip_1")

## To be copied in the server
# mod_plotclip_server("plotclip_1")
