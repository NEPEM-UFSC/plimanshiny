#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Home

  mod_home_server("home_1")
  # Mosaic analysis
  mosaic_data <- reactiveValues()
  r <- reactiveValues(r = 1)
  g <- reactiveValues(g = 2)
  b <- reactiveValues(b = 3)
  re <- reactiveValues(re = NA)
  nir <- reactiveValues(nir = NA)
  swir <- reactiveValues(swir = NA)
  tir <- reactiveValues(tir = NA)
  quantiles <- reactiveValues(q = c(0, 1))
  maxpixel <- reactiveValues(mp = 1e6)
  basemap <- reactiveValues(map = NULL)
  bmap <- reactiveValues(map = NULL)
  index <- reactiveValues()
  pathmosaic <- reactiveValues(path = NULL)
  activemosaic <- reactiveValues(name = NULL)
  mod_mosaic_prepare_server("mosaic_prepare_1", mosaic_data, r, g, b, re, nir, swir, tir, basemap, pathmosaic, quantiles, maxpixel, activemosaic, settings)

  # shapefile
  shapefile <- reactiveValues()
  mod_shapefile_prepare_server("shapefile_prepare_1", mosaic_data, basemap, shapefile, activemosaic,  r, g, b, settings)
  mod_indexes_server("indexes_1", mosaic_data, r, g, b, re, nir, swir, tir, basemap, index, shapefile, settings)
  mod_analyze_server("analyze_1", mosaic_data, basemap, shapefile, index, pathmosaic, dfs, settings)
  mosaiclist <- reactiveValues()
  mod_cropbatch_server("cropbatch_1", shapefile, mosaiclist, settings)

  # time series
  mod_timeseriesinput_server("timeseriesinput_1", shapefile, mosaiclist, r, g, b, re, nir,  swir, tir,  basemap, quantiles, settings)
  mod_timeseriesanalysis_server("timeseriesanalysis_1", shapefile, mosaiclist, r, g, b, re, nir, swir, tir, basemap, dfs, settings)

  # manipulation
  mod_crop_server("crop_1", mosaic_data, shapefile, r, g, b, basemap, settings)
  mod_plotclip_server("plotclip_1", mosaic_data, shapefile, r, g, b, basemap, settings)
  mod_bindlayer_server("bindlayer_1", mosaic_data, settings)
  mod_interpolate_server("interpolate_1", mosaic_data, r, g, b, basemap, settings)
  mod_aggregate_server("aggregate_1", mosaic_data, r, g, b, basemap, settings)
  mod_resample_server("resample_1", mosaic_data, settings)
  mod_segment_server("segment_1", mosaic_data, r, g, b, re, nir, settings)
  mod_sentinel_server("sentinel_1", mosaic_data, settings)
  mod_spatjoin_server("spatjoin_1", shapefile, settings)

  # Image analysis
  imgdata <- reactiveValues()
  mod_imageimport_server("imageimport_1", imgdata, settings)
  mod_imageanal_server("imageanal_1", imgdata, dfs, settings)
  mod_imagesegment_server("imagesegment_1", imgdata, settings)
  mod_imagepalette_server("imagepalette_1", imgdata, settings)

  # Phytopathometry
  mod_measurediseaseind_server("measurediseaseind_1", imgdata, dfs, settings)
  mod_imageimport_server("imageimport_2", imgdata, settings)
  mod_colorpalette_server("colorpalette_1", imgdata, settings)
  mod_measurediseasepal_server("measurediseasepal_1", imgdata, dfs, settings)
  mod_measurediseasepick_server("measurediseasepick_1", imgdata, settings)

  # Datasets
  dfs <- reactiveValues()
  mod_datasets_server("datasets_1", dfs, settings)
  mod_dffilter_server("dffilter_1", dfs, shapefile, settings)
  mod_dfedit_server("dfedit_1", dfs, shapefile, settings)
  mod_dfupdate_server("dfupdate_1", dfs, shapefile, settings)
  mod_dfjoin_server("dfjoin_1", dfs, shapefile, settings)
  mod_summarize_server("summarize_1", dfs, shapefile)

  # Conditional modules
  # slider
  observeEvent(input[["config_1-compslider"]], {
    if (input[["config_1-compslider"]]) {
      mod_slider_server("slider_1", imgdata, settings)
    }
  })
  # Drone traits
  observeEvent(input[["config_1-plantmat"]], {
    if (input[["config_1-plantmat"]]) {
      mod_matanalyze_server("matanalyze_1", dfs, shapefile, basemap, settings)
    }
  })

  observeEvent(input[["config_1-plantmeas"]], {
    if (input[["config_1-plantmeas"]]) {
      mod_phanalyze_server("phanalyze_1", mosaic_data, shapefile, basemap, dfs, settings)
      mod_timeseriesdsm_server("timeseriesdsm_1", shapefile, mosaiclist, basemap, dfs, settings)
    }
  })
  # Spatial interpolation
  observeEvent(input[["config_1-geostats"]], {
    if (input[["config_1-geostats"]]) {
      mod_spatinterp_server("spatinterp_1", dfs, shapefile, settings)
    }
  })

  # Tools
  mod_utmzonesel_server("utmzonesel_1", settings)
  mod_geometricmeasures_server("geometricmeasures_1", shapefile, settings)

  # Settings
  settings <- reactiveVal()
  mod_config_server("config_1", settings)


}


