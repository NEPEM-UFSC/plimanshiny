#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Seu webhook do App Script
  # Executar o módulo
  user_info <- mod_userinfo_server("userinfo_1")

  output$user_header <- renderText({
    req(user_info())
    paste("Welcome,", user_info()$name)
  })
  settings <- reactiveVal()
  mosaic_data <- reactiveValues()
  shapefile <- reactiveValues()
  r <- reactiveValues(r = 1)
  g <- reactiveValues(g = 2)
  b <- reactiveValues(b = 3)
  re <- reactiveValues(re = NA)
  nir <- reactiveValues(nir = NA)
  swir <- reactiveValues(swir = NA)
  tir <- reactiveValues(tir = NA)
  quantiles <- reactiveValues(q = c(0, 1))
  zlim <- reactiveValues(zlim = NULL)
  maxpixel <- reactiveValues(mp = 1e6)
  basemap <- reactiveValues(map = NULL)
  bmap <- reactiveValues(map = NULL)
  index <- reactiveValues()
  pathmosaic <- reactiveValues(path = NULL)
  activemosaic <- reactiveValues(name = NULL)
  mosaiclist <- reactiveValues()
  imgdata <- reactiveValues()
  dfs <- reactiveValues()
  mod_config_server("config_1", settings)
  mod_home_server("home_1", settings)

  # Tracking module initialization
  module_loaded <- reactiveValues()



  observeEvent(input$tabshome, {
    # Datasets
    if(input$tabshome == "datasets" && is.null(module_loaded$datasets)){
      module_loaded$datasets <- TRUE
      mod_datasets_server("datasets_1", dfs, settings)
    }
    if(input$tabshome == "filter" && is.null(module_loaded$filter)){
      module_loaded$filter <- TRUE
      mod_dffilter_server("dffilter_1", dfs, shapefile, settings)
    }
    if(input$tabshome == "edit" && is.null(module_loaded$edit)){
      module_loaded$edit <- TRUE
      mod_dfedit_server("dfedit_1", dfs, shapefile, settings)
    }
    if(input$tabshome == "update" && is.null(module_loaded$update)){
      module_loaded$update <- TRUE
      mod_dfupdate_server("dfupdate_1", dfs, shapefile, settings)
    }

    # Import Rasters
    if(input$tabshome == "mosaicimport" && is.null(module_loaded$mosaicimport)){
      module_loaded$mosaicimport <- TRUE
      mod_mosaic_prepare_server("mosaic_prepare_1", mosaic_data, r, g, b, re, nir, swir, tir, basemap, pathmosaic, quantiles, maxpixel, activemosaic, zlim, settings)
    }

    # Import or Build Shapefiles
    if(input$tabshome == "shapefileimport" && is.null(module_loaded$shapefileimport)){
      module_loaded$shapefileimport <- TRUE
      mod_shapefile_prepare_server("shapefile_prepare_1", mosaic_data, basemap, shapefile, activemosaic, r, g, b, settings)
    }

    if(input$tabshome == "mosaicindex" && is.null(module_loaded$mosaicindex)){
      module_loaded$mosaicindex <- TRUE
      mod_indexes_server("indexes_1", mosaic_data, r, g, b, re, nir, swir, tir, basemap, index, shapefile, settings, quantiles)
    }

    # Analysis of Mosaic
    if(input$tabshome == "mosaicanalyze" && is.null(module_loaded$mosaicanalyze)){
      module_loaded$mosaicanalyze <- TRUE
      mod_analyze_server("analyze_1", mosaic_data, basemap, shapefile, index, pathmosaic, dfs, settings, r, g, b)
    }

    # Phytopathometry
    if(input$tabshome == "imageimportphyt" && is.null(module_loaded$imageimportphyt)){
      module_loaded$imageimportphyt <- TRUE
      mod_imageimport_server("imageimport_2", imgdata, settings)
    }
    if(input$tabshome == "createcolorpal" && is.null(module_loaded$createcolorpal)){
      module_loaded$createcolorpal <- TRUE
      mod_colorpalette_server("colorpalette_1", imgdata, settings)
    }
    if(input$tabshome == "measurediseaseind" && is.null(module_loaded$measurediseaseind)){
      module_loaded$measurediseaseind <- TRUE
      mod_measurediseaseind_server("measurediseaseind_1", imgdata, dfs, settings)
    }
    if(input$tabshome == "measurediseaseint" && is.null(module_loaded$measurediseaseint)){
      module_loaded$measurediseaseint <- TRUE
      mod_measurediseasepick_server("measurediseasepick_1", imgdata, settings)
    }
    if(input$tabshome == "measurediseasepal" && is.null(module_loaded$measurediseasepal)){
      module_loaded$measurediseasepal <- TRUE
      mod_measurediseasepal_server("measurediseasepal_1", imgdata, dfs, settings)
    }

    # Image Analysis
    if(input$tabshome == "imageimport" && is.null(module_loaded$imageimport)){
      module_loaded$imageimport <- TRUE
      mod_imageimport_server("imageimport_1", imgdata, settings)
    }
    if(input$tabshome == "imagebinary" && is.null(module_loaded$imagebinary)){
      module_loaded$imagebinary <- TRUE
      mod_imagesegment_server("imagesegment_1", imgdata, settings)
    }
    if(input$tabshome == "imagepalette" && is.null(module_loaded$imagepalette)){
      module_loaded$imagepalette <- TRUE
      mod_imagepalette_server("imagepalette_1", imgdata, settings)
    }
    if(input$tabshome == "imageanal" && is.null(module_loaded$imageanal)){
      module_loaded$imageanal <- TRUE
      mod_imageanal_server("imageanal_1", imgdata, dfs, settings)
    }

    # Toolbox
    if(input$tabshome == "utmzonesel" && is.null(module_loaded$utmzonesel)){
      module_loaded$utmzonesel <- TRUE
      mod_utmzonesel_server("utmzonesel_1", settings)
    }
    if(input$tabshome == "ggeapps" && is.null(module_loaded$ggeapps)){
      module_loaded$ggeapps <- TRUE
      mod_geeapps_server("geeapps_1")
    }
    if(input$tabshome == "mgrs" && is.null(module_loaded$mgrs)){
      module_loaded$mgrs <- TRUE
      mod_mgrs_server("mgrs_1")
    }
    if(input$tabshome == "geometricmeasures" && is.null(module_loaded$geometricmeasures)){
      module_loaded$geometricmeasures <- TRUE
      mod_geometricmeasures_server("geometricmeasures_1", shapefile, settings)
    }

  })
}


