#' hyperspectral UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_hyperspectral_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Hyperspectral tools",
          collapsible = FALSE,
          width = 12,
          h3("Input"),
          selectInput(ns("hypermosaic"),
                      label = "Raster data",
                      choices = NULL),
          selectInput(ns("summfun"),
                      label = "Summary fun (used for polygons)",
                      choices = c("median", "mean", "sum", "min", "max"))
        )
      ),
      col_9(
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "760px",
          title = "Sample points",
          selected = "Sample points",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Sample points",
            uiOutput(ns("pointsui")),
            editModUI(ns("samplehyper"), height = "660px") |> add_spinner()
          ),
          tabPanel(
            title = "Hyperspectral Profile",
            fluidRow(
              col_4(
                textInput(ns("xlab"),
                          label = "X axis label",
                          value = "Wavelength (nm)")
              ),
              col_4(
                textInput(ns("ylab"),
                          label = "Y axis label",
                          value = "Reflectance")
              ),
              col_2(
                pickerInput(ns("lineorsmooth"),
                            label = "Line type",
                            choices = c("smooth", "line"))
              ),
              col_2(
                numericInput(ns("spam"),
                             label = "Spam (smooth line)",
                             value = 0.2,
                             min = 0.1,
                             max = 1)
              ),

            ),
            plotlyOutput(ns("profile"), height = "640px") |> add_spinner()
          ),
          tabPanel(
            title = "Reflectance heatmap",
            fluidRow(
              col_3(
                pickerInput(ns("bandx"),
                            label = "Band in x axis",
                            choices = NULL)
              ),
              col_3(
                pickerInput(ns("bandy"),
                            label = "Band in y axis",
                            choices = NULL)
              ),
              col_2(
                pickerInput(ns("plottype"),
                            label = "Plot type",
                            choices = c("hexbin", "3D"))
              ),
              col_2(
                numericInput(ns("nbins"),
                             label = "Number of bins",
                             value = 30)
              ),
              col_2(
                actionBttn(ns("heatmapdone"),
                           label = "Generate heatmap")
              )
            ),
            plotlyOutput(ns("heatmap"), height = "640px") |> add_spinner()
          ),
          tabPanel(
            title = "Reflectance profile across a line",
            fluidRow(
              col_3(
                pickerInput(ns("plottypeline"),
                            label = "Plot type",
                            choices = c("heatmap", "surface"))
              ),
              col_3(
                actionBttn(ns("heatmapdoneline"),
                           label = "Generate heatmap")
              )
            ),
            plotlyOutput(ns("heatmapline"), height = "640px") |> add_spinner()
          )
        )
      )
    )
  )
}

#' hyperspectral Server Functions
#'
#' @noRd
mod_hyperspectral_server <- function(id, mosaic_data, r, g, b, maxpixel,  basemap, dfs){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    observe({
      req(mosaic_data)
      updateSelectInput(session, "hypermosaic", choices = c("Active mosaic", setdiff(names(mosaic_data), "mosaic")), selected = "Active mosaic")
    })


    doneback <- reactiveValues(done = FALSE)
    sampledpoints <- reactiveVal()
    mosfile <- reactiveVal()
    bmhyp <- reactiveVal()
    observe({
      req(input$hypermosaic)
      if(input$hypermosaic == "Active mosaic"){
        bmhyp(basemap$map)
        mosfile(mosaic_data[["mosaic"]]$data)
      } else{
        mosfile(mosaic_data[[input$hypermosaic]]$data)
        mosaic_view(
          mosaic_data[[input$hypermosaic]]$data,
          r = suppressWarnings(as.numeric(r$r)),
          g = suppressWarnings(as.numeric(g$g)),
          b = suppressWarnings(as.numeric(b$b)),
          max_pixels = maxpixel$mp
        ) |> bmhyp()
      }
    })

    observe({
      req(bmhyp())
      if(!doneback$done){
        output$pointsui <- renderUI({
          actionBttn(ns("samplingdone"),
                     label = "Sampling Finished")

        })
        points <- callModule(editMod, "samplehyper", bmhyp()@map, editor = "leafpm")
        observeEvent(input$samplingdone, {
          if (!is.null(points()$finished)) {
            sampledpoints(points()$finished |> sf::st_transform(sf::st_crs(mosfile())))
          }
          doneback$done <- TRUE
        })
      } else{
        bands <- names(mosfile())
        numericbands <- bands |> get_number()
        if(length(bands) != length(numericbands)){
          stop("Cannot get wavelength from mosaic bands. Please, verify if all band names have a numeric value indicating the wavelength.")
        }
        req(sampledpoints())

        bandvals <- do.call("rbind", lapply(1:nrow(sampledpoints()), function(i){
          terra::extract(mosfile(), terra::vect(sampledpoints()[i, ]), fun = "median")
        }))

        bandplot <-
          as.data.frame(t(bandvals[, -1])) |>
          rownames_to_column("wavelength") |>
          dplyr::mutate(wavelength =  get_number(wavelength))
        colnames(bandplot) <- c("wavelength", paste0("P", 1:(ncol(bandplot)-1)))


        dfplot <-
          bandplot |>
          tidyr::pivot_longer(-1)

        output$profile <- renderPlotly({
          p1 <-
            ggplot(dfplot, aes(x = wavelength, y = value, color = name)) +
            {if(input$lineorsmooth == "line")geom_line()} +
            {if(input$lineorsmooth != "line")
              geom_smooth(se = FALSE,
                          method = "loess",
                          formula = y ~ x,
                          span = input$spam,
                          n = 500)} +
            theme_minimal() +
            theme(legend.position = "bottom") +
            labs(x = input$xlab,
                 y = input$ylab,
                 color = "")
          plotly::ggplotly(p1, dynamicTicks = TRUE)
        })

        dfs[["hyperspectral_profile"]] <- create_reactval("hyperspectral_profile", dfplot |> setNames(c("wavelength", "point", "radiance")))


        sendSweetAlert(
          session = session,
          title = "Regions sampled",
          text = "The points have been sampled. See the spectral signature in the next tab",
          type = "success"
        )

      }
    })

    observe({
      updatePickerInput(session, "bandx", choices = names(mosfile()))
    })
    observe({
      updatePickerInput(session, "bandy", choices = names(mosfile()))
    })


    observeEvent(input$heatmapdone, {
      output$heatmap <- renderPlotly({
        req(input$bandx, input$bandy)
        polygons <-
          sampledpoints() |>
          dplyr::filter(sf::st_geometry_type(sampledpoints())=="POLYGON")

        if(nrow(polygons) == 0){
          sendSweetAlert(
            session = session,
            title = "No polygons",
            text = "At last one polygon is needed to generate the heatmap",
            type = "error"
          )
          return()
        }

        # pointsval <- terra::extract(mosfile()[[c(input$bandx, input$bandy)]], terrpol)
        pointsval <- do.call("rbind", lapply(1:nrow(polygons), function(i){
          terra::extract(mosfile(), terra::vect(polygons[i, ]))
        }))
        # assign("points", pointsval, envir = .GlobalEnv)

        if(input$plottype == "hexbin"){
          req(input$bandx, input$bandy)
          p <-
            ggplot(pointsval, aes(x = !!sym(input$bandx), y = !!sym(input$bandy))) +
            geom_hex(bins = input$nbins, linewidth = 1) +
            theme_minimal() +
            scale_fill_viridis_c() +
            labs(
              x = glue::glue("Radiance on {get_number(input$bandx)} nm"),
              y = glue::glue("Radiance on {get_number(input$bandy)} nm"),
              fill = "Number of pixels"
            )
          plotly::ggplotly(p, dynamicTicks = TRUE)


        } else{
          pointsval <-
            pointsval |>
            dplyr::mutate(
              dplyr::across(c(!!dplyr::sym(input$bandx), !!dplyr::sym(input$bandy)), ~ cut(.x, breaks = input$nbins, include.lowest = TRUE), .names = "bin_{.col}"),
              .keep = "used"
            ) |>
            dplyr::select(3, 4) |>
            setNames(c("bandx", "bandy")) |>
            count(bandx, bandy) |>
            dplyr::mutate(
              bandx_mid = as.numeric(sub("\\((.*),.*", "\\1", bandx)) + as.numeric(gsub(".*,(.*)\\]", "\\1", bandx)) / 2,
              bandy_mid = as.numeric(sub("\\((.*),.*", "\\1", bandy)) + as.numeric(gsub(".*,(.*)\\]", "\\1", bandy)) / 2
            )

          # Create hexbin plot with Plotly
          # assign("points", pointsval, envir = .GlobalEnv)
          plotly::plot_ly(
            data = pointsval,
            x = ~bandx_mid,
            y = ~bandy_mid,
            z = ~n,
            type = "scatter3d",
            mode = "markers",
            marker = list(
              size = 5,
              color = ~n,
              colorscale = "Viridis",
              showscale = TRUE
            )
          ) |>
            plotly::layout(
              title = "Reflectance Values",
              scene = list(
                xaxis = list(title = glue::glue("Radiance on {get_number(input$bandx)}nm")),
                yaxis = list(title = glue::glue("Radiance on {get_number(input$bandy)}nm")),
                zaxis = list(title = "Number of pixels")
              )
            )
        }
      })
    })




    observeEvent(input$heatmapdoneline, {
      output$heatmapline <- renderPlotly({


        #filter only first LINESTRING
        linecoord <-
          sampledpoints() |>
          dplyr::filter(sf::st_geometry_type(sampledpoints())=="LINESTRING") |>
          dplyr::slice(1)

        coordsdist <- as.matrix(linecoord |> sf::st_coordinates())
        n <- nrow(coordsdist)
        distances <- NULL
        for (j in 1:(n - 1)) {
          x1 <- coordsdist[j, 1]
          y1 <- coordsdist[j, 2]
          x2 <- coordsdist[j + 1, 1]
          y2 <- coordsdist[j + 1, 2]
          distance <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
          distances[j] <- distance
        }
        # distances
        dists <- cumsum(distances)
        dist <- max(dists)

        ## extract
        valsline <-
          terra::extractAlong(mosfile(), terra::vect(linecoord)) |>
          dplyr::mutate(dists = seq(0, dist, length.out = dplyr::n()))

        interpolated_data <-
          valsline |>
          tidyr::pivot_longer(2:(ncol(valsline)-1)) |>
          dplyr::mutate(wavelength = get_number(name)) |>
          dplyr::group_split(dists) |>
          purrr::map_df(interpolate_group)


        if(input$plottypeline == "heatmap"){
          req(interpolated_data)
          interpolated_data |>
            plotly::plot_ly(
              x = ~dists,
              y = ~wavelength,
              z = ~value,
              type = "heatmap",
              colorscale = "Viridis",
              colorbar = list(title = "Radiance")
            ) |>
            plotly::layout(
              xaxis = list(title = "Distance"),
              yaxis = list(title = "Wavelength (nm)")
            )
        } else{

          # Create 3D Surface Plot
          surface_matrix <-
            interpolated_data |>
            tidyr::pivot_wider(names_from = dists, values_from = value) |>
            dplyr::select(-wavelength) |>
            as.matrix()

          # Extract axis values
          x_vals <- sort(unique(interpolated_data$dists))
          y_vals <- sort(unique(interpolated_data$wavelength))

          plotly::plot_ly(
            x = x_vals,
            y = y_vals,
            z = surface_matrix,
            type = "surface",
            colorscale = "Viridis"
          ) |>
            plotly::layout(
              scene = list(
                xaxis = list(title = "Distance"),
                yaxis = list(title = "Wavelength (nm)"),
                zaxis = list(title = "Radiance")
              )
            )

        }
      })
    })














  })
}

## To be copied in the UI
# mod_hyperspectral_ui("hyperspectral_1")

## To be copied in the server
# mod_hyperspectral_server("hyperspectral_1")
