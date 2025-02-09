#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
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
    if(input$tabshome == "join" && is.null(module_loaded$join)){
      module_loaded$join <- TRUE
      mod_dfjoin_server("dfjoin_1", dfs, shapefile, settings)
    }
    if(input$tabshome == "summary" && is.null(module_loaded$summary)){
      module_loaded$summary <- TRUE
      mod_summarize_server("summarize_1", dfs, shapefile)
    }
    if(input$tabshome == "boxplot" && is.null(module_loaded$boxplot)){
      module_loaded$boxplot <- TRUE
      mod_graphicalexploration_server("graphicalexploration_1", dfs, shapefile)
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

    if(input$tabshome == "shapefilenative" && is.null(module_loaded$shapefilenative)){
      module_loaded$shapefilenative <- TRUE
      mod_shapefilenative_server("shpnative", mosaic_data,  r, g, b, activemosaic, shapefile, zlim)
    }


    # Manipulation
    if(input$tabshome == "mosaicmanipula" && is.null(module_loaded$mosaicmanipula)){
      module_loaded$mosaicmanipula <- TRUE
      mod_crop_server("crop_1", mosaic_data, shapefile, r, g, b, basemap, settings)
      mod_plotclip_server("plotclip_1", mosaic_data, shapefile, r, g, b, basemap, settings)
      mod_bindlayer_server("bindlayer_1", mosaic_data, settings)
      mod_interpolate_server("interpolate_1", mosaic_data, r, g, b, basemap, settings)
      mod_aggregate_server("aggregate_1", mosaic_data, r, g, b, basemap, settings)
      mod_resample_server("resample_1", mosaic_data, settings)
      mod_segment_server("segment_1", mosaic_data, r, g, b, re, nir, swir, tir, settings, basemap)
      mod_sentinel_server("sentinel_1", mosaic_data, settings)
      mod_spatjoin_server("spatjoin_1", shapefile, settings)
      mod_vectorize_server("vectorize_1", mosaic_data, shapefile, basemap)
      mod_cropbatch_server("cropbatch_1", shapefile, mosaiclist, settings)
    }

    if(input$tabshome == "mosaicindex" && is.null(module_loaded$mosaicindex)){
      module_loaded$mosaicindex <- TRUE
      mod_indexes_server("indexes_1", mosaic_data, r, g, b, re, nir, swir, tir, basemap, index, shapefile, settings, quantiles)
    }

    # Analysis of Mosaic
    if(input$tabshome == "mosaicanalyze" && is.null(module_loaded$mosaicanalyze)){
      module_loaded$mosaicanalyze <- TRUE
      mod_analyze_server("analyze_1", mosaic_data, basemap, shapefile, index, pathmosaic, dfs, settings)
    }

    # Plant Measures
    if(input$tabshome == "phanalyze" && is.null(module_loaded$phanalyze)){
      module_loaded$phanalyze <- TRUE
      mod_phanalyze_server("phanalyze_1", mosaic_data, shapefile, basemap, dfs, settings)
    }

    # Import Rasters and Shapefiles for Time Series
    if(input$tabshome == "mosaicshape" && is.null(module_loaded$mosaicshape)){
      module_loaded$mosaicshape <- TRUE
      mod_timeseriesinput_server("timeseriesinput_1", shapefile, mosaiclist, r, g, b, re, nir, swir, tir, basemap, quantiles, settings)
    }

    # Analyze Time Series of Vegetation Indexes
    if(input$tabshome == "analyzeseries" && is.null(module_loaded$analyzeseries)){
      module_loaded$analyzeseries <- TRUE
      mod_timeseriesanalysis_server("timeseriesanalysis_1", shapefile, mosaiclist, r, g, b, re, nir, swir, tir, basemap, dfs, settings)
    }

    # Analyze Time Series of Digital Surface Models
    if(input$tabshome == "timeseriesdsm" && is.null(module_loaded$timeseriesdsm)){
      module_loaded$timeseriesdsm <- TRUE
      mod_timeseriesdsm_server("timeseriesdsm_1", shapefile, mosaiclist, basemap, dfs, settings)
    }

    # Plant Maturity
    if(input$tabshome == "matanalyzemod" && is.null(module_loaded$matanalyzemod)){
      module_loaded$matanalyzemod <- TRUE
      mod_matanalyze_server("matanalyze_1", dfs, shapefile, basemap, settings)
    }

    # Growth Models
    if(input$tabshome == "growthmodels" && is.null(module_loaded$growthmodels)){
      module_loaded$growthmodels <- TRUE
      mod_growthmodels_server("growthmodels_1", dfs)
    }

    # Spatial Interpolation
    if(input$tabshome == "spatinterpolate" && is.null(module_loaded$spatinterpolate)){
      module_loaded$spatinterpolate <- TRUE
      mod_spatinterp_server("spatinterp_1", dfs, shapefile, settings)
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
    if(input$tabshome == "slider" && is.null(module_loaded$slider)){
      module_loaded$slider <- TRUE
      mod_slider_server("slider_1", imgdata, settings)
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
    if(input$tabshome == "geometricmeasures" && is.null(module_loaded$geometricmeasures)){
      module_loaded$geometricmeasures <- TRUE
      mod_geometricmeasures_server("geometricmeasures_1", shapefile, settings)
    }
    if(input$tabshome == "growthcurves" && is.null(module_loaded$growthcurves)){
      module_loaded$growthcurves <- TRUE
      mod_growthmodelscurves_server("growthmodelscurves_1")
    }
    if(input$tabshome == "gcp" && is.null(module_loaded$gcp)){
      module_loaded$gcp <- TRUE
      mod_georeference_server("georef_ui", mosaic_data = mosaic_data, r, g, b, dfs, zlim)
    }

  })
}


