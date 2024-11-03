#' timeseriesdsm UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_timeseriesdsm_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_4(
        bs4TabCard(
          width = 12,
          icon = icon("gears"),
          status  = "success",
          type = "tabs",
          tabPanel(
            title = "Configure the analysis",
            actionButton(
              inputId = ns("guideanalyze"),
              label = tagList(
                icon = icon("question-circle", verify_fa = FALSE), "Guide"
              ),
              style = "color: white ; background-color: #dd4b39",
              class = "btn-danger"
            ),
            hl(),
            prettyRadioButtons(
              inputId = ns("strategy"),
              label = "Strategy",
              choices = c("I have DSM and DTM", "Build DTM using a moving window"),
              icon = icon("check"),
              bigger = TRUE,
              status = "info",
              animation = "jelly"
            ),
            conditionalPanel(
              condition = "input.strategy != 'I have DSM and DTM'", ns = ns,
              prettyRadioButtons(
                inputId = ns("interpmethod"),
                label = "Interpolation method",
                choices = c("Thin Plate Spline", "Kriging"),
                icon = icon("check"),
                bigger = TRUE,
                status = "info",
                animation = "jelly"
              )
            ),
            selectInput(
              ns("activeshape"),
              label = "Shapefile",
              choices = NULL
            ),
            conditionalPanel(
              condition = "input.strategy == 'I have DSM and DTM'", ns = ns,
              selectInput(
                ns("dtmfile"),
                label = "DTM raster file",
                choices = NULL
              ),
              selectizeInput(
                inputId = ns("dsmfiles"),
                label = "DSM raster files",
                choices = NULL,
                multiple=TRUE,
                options = list('plugins' = list('remove_button'),
                               'create' = TRUE,
                               'persist'= FALSE)
              )
            ),
            conditionalPanel(
              condition = "input.strategy == 'Build DTM using a moving window'", ns = ns,
              selectizeInput(
                inputId = ns("dsmfileswin"),
                label = "DSM raster files",
                choices = NULL,
                multiple=TRUE,
                options = list('plugins' = list('remove_button'),
                               'create' = TRUE,
                               'persist'= FALSE)
              ),
              textInput(
                ns("windowsize"),
                label = "Window size (meters)",
                value = c("5,5")
              )
            ),
            pickerInput(
              ns("maskfile"),
              label = "Mask (optional)",
              choices = NULL
            ),
            conditionalPanel(
              condition = "input.maskfile != 'none'", ns = ns,
              prettyCheckbox(
                inputId = ns("masksoil"),
                label = "Mask shows soil?",
                value = FALSE,
              )
            ),
            prettyCheckbox(
              inputId = ns("croptoext"),
              label = "Crop to the shapefile extend?",
              value = FALSE,
              icon = icon("check"),
              status = "success",
              animation = "rotate"
            ),
            actionBttn(ns("analyzemosaicts"),
                       label = "Compute canopy height model!",
                       status = "success")
          ),
          tabPanel(
            title = "Configure the output",
            actionButton(
              inputId = ns("guideoutput"),
              label = tagList(
                icon = icon("question-circle", verify_fa = FALSE), "Guide"
              ),
              style = "color: white ; background-color: #dd4b39",
              class = "btn-danger"
            ),
            hl(),
            selectInput(ns("plotattribute"),
                        label = "Attribute",
                        choices = NULL),
            hl(),
            pickerpalette(id, "palplot", selected = "RdYlGn"),
            prettyCheckbox(
              inputId = ns("palplotrev"),
              label = "Reverse",
              value = FALSE,
              icon = icon("check"),
              status = "success",
              animation = "rotate"
            ),
            sliderInput(ns("alpharesplot"),
                        label = "Fill opacity",
                        min = 0,
                        max = 1,
                        value = 0.75),
            h3("Export the results"),
            divclass("out4",
                     mod_download_shapefile_ui(ns("downresplot"), label = "Plot results")
            ),
            hl(),
            h3("Save the output to a temporary file"),
            actionButton(
              inputId = ns("savetoglobalenv"),
              label = "Save",
              icon = icon("share-from-square"),
              status = "success",
              gradient = TRUE,
              width = "150px",
              flat = TRUE
            )
          )
        )
      ),
      col_8(
        bs4TabCard(
          id = ns("tabs"),
          width = 12,
          height = "780px",
          status = "success",
          title = "Results",
          selected = "Home",
          solidHeader = FALSE,
          maximizable = TRUE,
          type = "tabs",
          tabPanel(
            title = "Home",
            fluidRow(
              col_9(
                img(src = "www/logodsm.jpg", width = "100%", height = "100%")
              ),
              col_3(
                h2("About"),
                "This module offers two strategies for computing a time series of canopy height models (CHM), providing valuable tools for monitoring vegetation growth and changes in canopy structure over time.", br(), br(),
                h2("Disclaimer"),
                "We welcome your feedback and suggestions on the application's usefulness. Please note that we do not guarantee the correctness, reliability, or utility of the results, especially if incorrect settings are applied during the CHM estimation process.", br(), br(),
                h2("Image Reference"),
                "The image used in this module was generated using advanced AI tools to represent the digital surface model (DSM)."
              )
            )
          ),
          tabPanel(
            title = "Overview",
            plotlyOutput(ns("timeserieoverview"), height = "720px") |> add_spinner()
          ),
          tabPanel(
            title = "Compare levels",
            fluidRow(
              col_6(
                pickerInput(
                  inputId = ns("groupingvar"),
                  label = "Select a grouping variable:",
                  choices = NULL,
                  multiple = FALSE
                )
              ),
              col_6(
                pickerInput(
                  inputId = ns("levelsvar"),
                  label = "Select up to six levels to compare:",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(
                    "actions-box" = TRUE,
                    "live-search" = TRUE,
                    "max-options" = 6,
                    "max-options-text" = "No more levels allowed"
                  )
                )
              )
            ),
            plotlyOutput(ns("timeseriecompare"), height = "640px") |> add_spinner()
          ),
          tabPanel(
            title = "Evolution plot",
            fluidRow(
              col_6(
                pickerInput(
                  inputId = ns("plottoshow"),
                  label = "'unique_id' to see the evolution across time:",
                  choices = NULL,
                  multiple = FALSE
                )
              ),
              col_3(
                numericInput(
                  ns("ncols"),
                  label = "Columns",
                  value = NA
                )
              ),
              col_3(
                numericInput(
                  ns("nrows"),
                  label = "Rows",
                  value = NA
                )
              )
            ),
            plotOutput(ns("evolutionplot"), height = "640px") |> add_spinner()
          ),
          tabPanel(
            title = "Compare plots",
            fluidRow(
              col_5(
                pickerInput(
                  inputId = ns("levelvarcp1"),
                  label = "Left:",
                  choices = NULL,
                  multiple = FALSE,
                  options = list(
                    "live-search" = TRUE
                  )
                )
              ),
              col_5(
                pickerInput(
                  inputId = ns("levelvarcp2"),
                  label = "Right:",
                  choices = NULL,
                  multiple = FALSE,
                  options = list(
                    "live-search" = TRUE
                  )
                )
              ),
              col_2(
                prettyCheckbox(
                  inputId = ns("compslider"),
                  label = "Slider?",
                  value = FALSE,
                  icon = icon("check"),
                  status = "success",
                  animation = "rotate"
                )
              )

            ),
            conditionalPanel(
              condition = "input.compslider == false", ns = ns,
              fluidRow(
                col_6(
                  plotOutput(ns("compplot1"), height = "540px") |> add_spinner()
                ),
                col_6(
                  plotOutput(ns("compplot2"), height = "540px") |> add_spinner()
                )
              )
            ),
            conditionalPanel(
              condition = "input.compslider == true", ns = ns,
              uiOutput(ns("sliderout")) |> add_spinner()
            )
          ),
          tabPanel(
            title = "Map plot (single data)",
            fluidRow(
              col_4(
                pickerInput(
                  inputId = ns("filterdate"),
                  label = "Date to explore",
                  choices = NULL,
                  multiple = FALSE
                )
              ),
              col_4(
                pickerInput(
                  inputId = ns("groupvarsd"),
                  label = "Select a grouping variable:",
                  choices = NULL,
                  multiple = FALSE
                )
              ),
              col_4(
                pickerInput(
                  inputId = ns("levelsvarsd"),
                  label = "Select levels to filter:",
                  choices = NULL,
                  multiple = TRUE
                )
              )
            ),
            leafletOutput(ns("resultsplotmap"), height = "680px")  |> add_spinner()
          ),
          tabPanel(
            title = "Raw results",
            reactable::reactableOutput(ns("rawresults"), height = "720px")  |> add_spinner()
          )
        )
      )
    )
  )
}

#' timeseriesdsm Server Functions
#'
#' @noRd
mod_timeseriesdsm_server <- function(id, shapefile, mosaiclist, basemap, dfs, settings){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      if(!settings()$tidyterra){
        hideTab(inputId = "tabs", target = "Evolution plot")
      }
    })

    # update selec input
    observe({
      req(mosaiclist$mosaics$data)
      updateSelectInput(session, "dtmfile", choices = c(setdiff(names(mosaiclist$mosaics$data), "mosaic")))
    })
    observe({
      req(mosaiclist$mosaics$data)
      vals <- c(setdiff(names(mosaiclist$mosaics$data), c("mosaic", input$dtmfile)))
      updateSelectizeInput(session, "dsmfiles", choices = vals, selected = vals)
      updateSelectizeInput(session, "dsmfileswin", choices = vals, selected = vals)
    })
    observe({
      req(mosaiclist$mosaics$data)
      updatePickerInput(session, "maskfile", choices = c("none", setdiff(names(mosaiclist$mosaics$data), c("mosaic", input$dtmfile))),
                        selected = "none")
    })
    observe({
      req(shapefile)
      updateSelectInput(session, "activeshape",
                        choices = setdiff(names(shapefile), "shape"))
    })

    bmchm <- reactiveValues(map = NULL)
    chmreact <- reactiveValues(rast = NULL)
    dfres <- reactiveValues(df = NULL)
    sampledpoints <- reactiveValues(sf = NULL)


    observeEvent(input$analyzemosaicts, {
      req(mosaiclist$mosaics$data)
      req(shapefile[[input$activeshape]]$data)
      if(is.null(mosaiclist$mosaics$data) | is.null(shapefile[[input$activeshape]]$data)){
        sendSweetAlert(
          session = session,
          title = "Did you skip any steps?",
          text = "To analyze the mosaic, ensure that mosaic and shapefile have been correctly imported.",
          type = "error"
        )
      }

      if(is.null(basemap$map)){
        bm <- mosaic_view(mosaiclist$mosaics$data[[1]])
      } else{
        bm <- basemap$map
      }

      # Loop
      progressSweetAlert(
        session = session,
        id = "myprogress",
        title = "Start",
        display_pct = TRUE,
        value = 0,
        total = length(mosaiclist$mosaics$data)
      )

      if(input$strategy == "I have DSM and DTM"){
        seqqq <- seq_along(input$dsmfiles)
      } else{
        seqqq <- seq_along(input$dsmfileswin)
      }
      # req(input$dsmfiles, input$dtmfile, input$shapefile)
      for (i in seqqq) {
        updateProgressBar(
          session = session,
          id = "myprogress",
          value = i,
          title = paste0("Working in progress, Please, wait."),
          total = length(seqqq)
        )

        if(input$strategy == "I have DSM and DTM"){
          # browser()
          dsm <- mosaiclist$mosaics$data[[input$dsmfiles[[i]]]]
          dtm <- mosaiclist$mosaics$data[[input$dtmfile]]
          ch1 <- !inherits(dsm,"SpatRaster") || !terra::nlyr(dsm) == 1 || terra::is.bool(dsm) || is.list(dsm)
          ch2 <- !inherits(dtm,"SpatRaster") || !terra::nlyr(dtm) == 1 || terra::is.bool(dtm) || is.list(dtm)

          if (ch1 | ch2) {
            sendSweetAlert(
              session = session,
              title = "Invalid file format",
              text = paste("DSM and DTM must be an SpatRaster object with one layer."),
              type = "error"
            )
            return()
          } else{

            if(input$maskfile == "none"){
              chmres <- mosaic_chm(dsm, dtm,
                                   verbose = FALSE)
            } else{
              chmres <- mosaic_chm(dsm = dsm,
                                   dtm = dtm,
                                   mask = mosaiclist[[input$maskfile]]$data,
                                   mask_soil = input$masksoil,
                                   verbose = FALSE)
            }
            chmreact$rast[[names(mosaiclist$mosaics$data[i])]] <- chmres
          }
        }

        if(input$strategy == "Build DTM using a moving window"){
          dsm <- mosaiclist$mosaics$data[[input$dsmfileswin[[i]]]]
          ch1 <- !inherits(dsm,"SpatRaster") || !terra::nlyr(dsm) == 1 || terra::is.bool(dsm) || is.list(dsm)
          if (ch1) {
            sendSweetAlert(
              session = session,
              title = "Invalid file format",
              text = paste("DSM and DTM must be an SpatRaster object with one layer."),
              type = "error"
            )
            return()
          } else{
            if(input$maskfile == "none"){
              chmres <- mosaic_chm(dsm,
                                   window_size = input$windowsize |> chrv2numv(),
                                   verbose = FALSE)
            } else{
              chmres <- mosaic_chm(dsm = dsm,
                                   window_size = input$windowsize |> chrv2numv(),
                                   mask = mosaiclist[[input$maskfile]]$data,
                                   mask_soil = input$masksoil,
                                   verbose = FALSE)
            }
            chmreact$rast[[names(mosaiclist$mosaics$data[i])]] <- chmres

          }
        }
      }

      closeSweetAlert(session = session)

      sendSweetAlert(
        session = session,
        title = "Almost done!",
        text = paste("{plimanshiny} is processing the results. Please wait while we finalize everything..."),
        type = "info",
        btn_labels = NA
      )


      result_plot <-
        map_dfr(chmreact$rast, ~.x |>
                  mosaic_chm_extract(shapefile = shapefile[[input$activeshape]]$data),
                .id = "date") |>
        dplyr::mutate(date = as.Date(date)) |>
        dplyr::arrange(date, block, plot_id) |>
        dplyr::mutate(unique_id = dplyr::row_number(),
                      unique_plot = paste0(block, "_", plot_id)) |>
        dplyr::relocate(unique_id, unique_plot, date, .before = block) |>
        sf::st_as_sf()



      #   ##### Show the results #######
      updateSelectInput(session, "plotattribute",
                        choices = names(result_plot),
                        selected = ifelse("individual" %in% colnames(result_plot), colnames(result_plot)[6], colnames(result_plot)[5]))
      #
      #   # overview plot
      output$timeserieoverview <- renderPlotly({
        req(input$plotattribute)
        plot_ind <-
          result_plot |>
          as.data.frame() |>
          dplyr::select(dplyr::all_of(c("date", input$plotattribute)))
        p <-
          suppressWarnings(
            ggplot(plot_ind, aes(x = .data[["date"]], y = .data[[input$plotattribute]], group = 1)) +
              geom_boxplot(fill = "#28a745") +
              geom_smooth(method = 'loess', formula = 'y ~ x') +
              theme_bw() +
              scale_x_date(date_labels = "%d/%m")
          )
        suppressWarnings(plotly::ggplotly(p, dynamicTicks = TRUE))
      })


      #
      # evolution plot
      # update the values
      observe({
        updatePickerInput(session, "groupingvar",
                          choices = colnames(result_plot),
                          selected = "plot_id")
      })

      observe({
        req(input$groupingvar)
        levels <- sort(unique(result_plot[[input$groupingvar]]))
        updatePickerInput(session, "levelsvar",
                          choices = levels)
      })

      observe({
        levels <- sort(unique(result_plot[["unique_id"]]))
        updatePickerInput(session, "levelvarcp1",
                          choices = levels)
        updatePickerInput(session, "levelvarcp2",
                          choices = levels)
      })

      # level comparision
      output$timeseriecompare <- renderPlotly({
        req(input$plotattribute)
        req(input$groupingvar)
        req(input$levelsvar)

        plot_ind <-
          result_plot |>
          as.data.frame() |>
          dplyr::select(dplyr::all_of(c("date", input$groupingvar, input$plotattribute))) |>
          dplyr::filter(!!dplyr::sym(input$groupingvar) %in% input$levelsvar) |>
          dplyr::group_by(date, !!dplyr::sym(input$groupingvar)) |>
          dplyr::summarise(dplyr::across(dplyr::where(is.numeric), \(x){mean(x, na.rm = TRUE)}))


        p2 <-
          suppressWarnings(
            ggplot(plot_ind, aes(x = .data[["date"]],
                                 y = .data[[input$plotattribute]],
                                 group = .data[[input$groupingvar]],
                                 color = .data[[input$groupingvar]])) +
              geom_point() +
              geom_smooth(se = FALSE,
                          method = 'loess',
                          formula = 'y ~ x') +
              theme_bw() +
              scale_x_date(date_labels = "%d/%m")
          )
        suppressWarnings(plotly::ggplotly(p2, dynamicTicks = TRUE))
      })




      # plot evolution
      shp <- reactiveValues()
      if(!"unique_id" %in% colnames(shapefile[[input$activeshape]]$data)){
        shp$shp <-
          shapefile[[input$activeshape]]$data |>
          dplyr::mutate(unique_id = dplyr::row_number(), .before = 1)
      } else if(!"unique_plot" %in% colnames(shapefile[[input$activeshape]]$data)){
        shp$shp <-
          shapefile[[input$activeshape]]$data |>
          dplyr::mutate(unique_plot = paste0(block, "_", plot_id))
      } else{
        shp$shp <- shapefile[[input$activeshape]]$data
      }

      #
      #   # Plot evolution
      req(shp$shp)
      observe({
        levels <- sort(unique(shp$shp[["unique_plot"]]))
        updatePickerInput(session, "plottoshow",
                          choices = levels)
      })
      #
      observe({
        num_plots <- length(mosaiclist$mosaics$data)
        ncol <- ceiling(sqrt(num_plots))
        nrow <- ceiling(num_plots/ncol)
        req(ncol)
        req(nrow)
        updateNumericInput(session, "nrows",
                           value = nrow)
        updateNumericInput(session, "ncols",
                           value = ncol)
      })

      output$evolutionplot <- renderPlot({
        req(input$plottoshow)
        shapetmp <-
          shp$shp |>
          dplyr::filter(unique_plot == input$plottoshow) |>
          shapefile_input(as_sf = FALSE, info = FALSE)


        req(input$nrows, input$ncols)
        list_mo <- list()
        for (i in seq_along(chmreact$rast)) {
          list_mo[[i]] <- chmreact$rast[[i]]$chm[[2]]
        }
        req(list_mo)
        if(length(list_mo) > 0){
          list_mo <-
            lapply(seq_along(list_mo), function(i){
              terra::resample(list_mo[[i]], list_mo[[1]])
            }) |>
            terra::rast() |>
            terra::crop(shapetmp, mask = TRUE)

          names(list_mo) <- names(chmreact$rast)

          ggplot() +
            tidyterra::geom_spatraster(data = list_mo,
                                       interpolate = TRUE,
                                       maxcell = 2.5e5) +
            scale_fill_gradientn(colors = return_colors(input$palplot, reverse = input$palplotrev, n = 8),
                                 na.value = "transparent") +
            facet_wrap(~lyr,
                       ncol =  input$ncols,
                       nrow = input$nrows) +
            theme_void() +
            theme(legend.position = "bottom",
                  panel.spacing = unit(0, "cm")) +
            guides(fill = guide_colourbar(theme = theme(
              legend.key.width = unit(450, "pt")
            ))) +
            labs(fill = "CHM")

        }
      })
      #




      ## Compare plots
      tmpplot1 <- reactiveValues()
      tmpplot2 <- reactiveValues()
      minmax1 <- reactiveValues()
      minmax2 <- reactiveValues()

      observe({
        req(input$levelvarcp1)
        shapetmp <-
          result_plot |>
          dplyr::filter(unique_id == input$levelvarcp1)
        mosaic1 <- chmreact$rast[[which(names(mosaiclist$mosaics$data) == shapetmp$date)]]
        tmpplot1$plot <- terra::crop(mosaic1$chm[[2]], terra::vect(shapetmp), mask = TRUE)
        minmax1$val <- terra::minmax(tmpplot1$plot)


        req(input$levelvarcp2)
        shapetmp <-
          result_plot |>
          dplyr::filter(unique_id == input$levelvarcp2)

        mosaic2 <- chmreact$rast[[which(names(mosaiclist$mosaics$data) == shapetmp$date)]]
        tmpplot2$plot <- terra::crop(mosaic2$chm[[2]], terra::vect(shapetmp), mask = TRUE)
        minmax2$val <- terra::minmax(tmpplot2$plot)
      })


      observe({
        if(!input$compslider){
          output$compplot1 <- renderPlot({
            req(tmpplot1$plot)
            rang <- c(min(c(minmax1$val, minmax2$val)), max(c(minmax1$val, minmax2$val)))
            terra::plot(tmpplot1$plot, col = return_colors(input$palplot, reverse = input$palplotrev, n = 100),
                        maxcell = 1e6,
                        smooth = TRUE,
                        axes = FALSE,
                        range = rang)
          })
          output$compplot2 <- renderPlot({
            req(tmpplot2$plot)
            rang <- c(min(c(minmax1$val, minmax2$val)), max(c(minmax1$val, minmax2$val)))
            terra::plot(tmpplot2$plot, col = return_colors(input$palplot, reverse = input$palplotrev, n = 100),
                        smooth = TRUE,
                        maxcell = 1e6,
                        axes = FALSE,
                        range = rang)
          })
        } else{

          pathslider1 <- reactiveValues(val=NULL)
          pathslider2 <- reactiveValues(val=NULL)


          f1 <- list.files(path = paste0(system.file("app", package = "plimanshiny" ), "/www/"),
                           pattern = "beforeimg_")
          f2 <- list.files(path = paste0(system.file("app", package = "plimanshiny" ), "/www/"),
                           pattern = "afterimg_")
          if(any(c(length(f1), length(f2)) != 0)){
            tmpimages <- paste0(paste0(system.file("app", package = "plimanshiny" ), "/www/"), c(f1, f2))
            a <- sapply(tmpimages, file.remove)
          }

          req(tmpplot1$plot)
          req(tmpplot2$plot)

          output$sliderout <- renderUI({
            # image1

            tfbef <- glue::glue(system.file("app", package = "plimanshiny" ), "/www/beforeimg_plot1_{sample(1:1000000, 1)}.jpg")
            pathslider1$val <- tfbef
            jpeg(tfbef, width = 920, height = 640)
            rang <- c(min(c(minmax1$val, minmax2$val)), max(c(minmax1$val, minmax2$val)))
            terra::plot(tmpplot1$plot, col = return_colors(input$palplot, reverse = input$palplotrev, n = 100),
                        maxcell = 1e6,
                        smooth = TRUE,
                        axes = FALSE,
                        range = rang)
            dev.off()

            # image2
            tfaft <- glue::glue(system.file("app", package = "plimanshiny" ), "/www/beforeimg_plot2_{sample(1:1000000, 1)}.jpg")
            pathslider2$val <- tfaft
            jpeg(tfaft, width = 920, height = 640)
            rang <- c(min(c(minmax1$val, minmax2$val)), max(c(minmax1$val, minmax2$val)))
            terra::plot(tmpplot2$plot, col = return_colors(input$palplot, reverse = input$palplotrev, n = 100),
                        maxcell = 1e6,
                        smooth = TRUE,
                        axes = FALSE,
                        range = rang)
            dev.off()

            # Regex pattern to match everything after "app/"
            pattern <- "app/(.*)"
            # Extract the captured group
            match1 <- regmatches(tfaft, regexec(pattern, tfaft))
            match2 <- regmatches(tfbef, regexec(pattern, tfbef))
            imgafter <- sub(match2, "\\1", match1[[1]][2])
            imgabefo <- sub(match2, "\\1", match2[[1]][2])

            HTML(
              paste0(
                '<div id="comparison">
           <style>
          div#comparison {
              width: 50vw; /* Increased width */
              height: 50vw; /* Increased height */
              max-width: 920px; /* Increased maximum width */
              max-height: 640px; /* Increased maximum height */
              overflow: hidden;
          }

          div#comparison figure {
              background-image: url(',imgafter,');
              background-size: cover;
              position: relative;
              font-size: 0;
              width: 100%;
              height: 100%;
              margin: 0;
          }

          div#comparison figure > img {
              position: relative;
              width: 100%;
          }

          div#comparison figure div {
              background-image: url(',imgabefo,');
              background-size: cover;
              position: absolute;
              width: 50%;
              box-shadow: 0 5px 10px -2px rgba(0, 0, 0, 0.7);
              overflow: hidden;
              bottom: 0;
              height: 100%;
          }

          input[type=range]{
              -webkit-appearance:none;
              -moz-appearance:none;
              position: relative;
              top: -2rem;
              left: -2%;
              background-color: rgba(25,255,25,0.2);
              width: 102%;
              }
              input[type=range]:focus {
              outline: none;
              }
              input[type=range]:active {
              outline: none;
              }

              input[type=range]::-moz-range-track {
              -moz-appearance:none;
              height:45px;
              width: 98%;
              background-color: rgba(25,255,25,0.1);
              position: relative;
              outline: none;
              }
              input[type=range]::active {
              border: none;
              outline: none;
              }
              input[type=range]::-webkit-slider-thumb {
              -webkit-appearance:none;
              width: 20px;
              height: 15px;
              border-radius: 5px;
              background: rgba(25,255,25,0.8);
              }
              input[type=range]::-moz-range-thumb {
              -moz-appearance: none;
              width: 20px;
              height: 15px;
              background: #fff;
              border-radius: 15;
              }
              input[type=range]:focus::-webkit-slider-thumb {
              height: 25px;
              border-radius: 5px;
              background: rgba(0,255,70,1);
              }
              input[type=range]:focus::-moz-range-thumb {
              height:45px;
              background: rgba(25,255,25,0.05);
              }
      </style>
           <figure>
              <div id="divisor"></div>
           </figure>
           <input type="range" min="0" max="100" value="50" id="slider" oninput="moveDivisor()">
         </div>'
              )
            )
          })
        }
      })

      # single data
      # filter by date
      # update the values
      observe({
        updatePickerInput(session, "groupvarsd",
                          choices = colnames(result_plot),
                          selected = "plot_id")
      })
      observe({
        req(input$groupvarsd)
        levels <- sort(unique(result_plot[[input$groupvarsd]]))
        updatePickerInput(session, "levelsvarsd",
                          choices = levels)
      })
      observe({
        levels <- unique(result_plot |> as.data.frame() |>  dplyr::select(date) |> dplyr::pull())
        updatePickerInput(session, "filterdate",
                          choices = levels)
      })


      output$resultsplotmap <- renderLeaflet({
        req(input$filterdate)

        dftemp <-
          result_plot |>
          dplyr::filter(date == input$filterdate)

        if(!is.null(input$levelsvarsd)){
          dftemp <-
            dftemp |>
            dplyr::filter(!!dplyr::sym(input$groupvarsd) %in% input$levelsvarsd)
        }


        mshp <- shapefile_view(dftemp,
                               attribute = input$plotattribute,
                               color_regions = return_colors(input$palplot, reverse = input$palplotrev),
                               alpha.regions = input$alpharesplot)
        (basemap$map +  mshp)@map
      })

      # raw results
      jsf <- JS("
        function(cellInfo) {
          var sum = 0;
          var count = 0;
          for (var i = 0; i < cellInfo.subRows.length; i++) {
            var row = cellInfo.subRows[i];
            var value = parseFloat(row[cellInfo.column.id]);
            if (!isNaN(value)) {
              sum += value;
              count++;
            }
          }
          var mean = count > 0 ? sum / count : null;
          return mean !== null ? mean.toFixed(2) : null;
        }
      ")
      output$rawresults <- reactable::renderReactable({
        reactable(
          result_plot |>
            sf::st_drop_geometry() |>
            roundcols(digits = 3),
          filterable = TRUE,
          searchable = TRUE,
          striped = TRUE,
          pagination = TRUE,
          defaultPageSize = 15,
          defaultColDef = colDef(
            aggregated = jsf
          ),
          theme = reactableTheme(
            cellPadding = "8px 10px",
            style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
            searchInputStyle = list(width = "100%")
          )
        )

      })



      closeSweetAlert(session = session)

      sendSweetAlert(
        session = session,
        title = "Time Series Analysis Complete!",
        text = "The canopy height model time series has been successfully analyzed. You can now explore the results in the tabs.",
        type = "success"
      )

      #   # Sent do datasets
      #   observe({
      #     req(result_plot)
      #     dfs[["result_plot"]] <- create_reactval("result_plot", result_plot |> sf::st_drop_geometry())
      #     shapefile[["result_plot"]] <- create_reactval("result_plot", result_plot)
      #     if(!is.null(result_indiv)){
      #       dfs[["result_indiv"]] <- create_reactval("result_indiv", result_indiv |> sf::st_drop_geometry())
      #       shapefile[["result_indiv"]] <- create_reactval("result_indiv", result_indiv)
      #     }
      #   })
      #
      #   mod_download_shapefile_server("downresplot", terra::vect(result_plot), name = "time_series_output")
      #
      #   closeSweetAlert(session = session)
      #
      #   # save to global env
      #   # send the results to the global environment
      #   observeEvent(input$savetoglobalenv, {
      #     req(result_plot)
      #     tf <- tempfile(pattern = "plimanshiny_output", fileext = ".RData")
      #     plimanshiny_timeseries <- list(result_plot = result_plot,
      #                                    result_indiv = result_indiv)
      #     save(plimanshiny_timeseries, file = tf)
      #     ask_confirmation(
      #       inputId = "myconfirmation",
      #       type = "warning",
      #       title = "Close the App?",
      #       text = glue::glue("The results were saved in a temporary file ({basename(tf)}).
      #           To access the created object, you need first to stop the App and run
      #           get_plimanshiny_results()
      #           to load the list into your R environment.
      #           Do you really want to close the app now?"),
      #       btn_labels = c("Nope", "Yep"),
      #       btn_colors = c("#FE642E", "#04B404")
      #     )
      #   })
      #
      #   observe({
      #     if (!is.null(input$myconfirmation)) {
      #       if (input$myconfirmation) {
      #         stopApp()
      #       } else {
      #         return()
      #       }
      #     }
      #   })


    })




  })
}

## To be copied in the UI
# mod_timeseriesdsm_ui("timeseriesdsm_1")

## To be copied in the server
# mod_timeseriesdsm_server("timeseriesdsm_1")
