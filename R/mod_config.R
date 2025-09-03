#' config UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_config_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Card(
      title = tagList(icon("cogs"), tags$strong("Module and Tools Configuration Center")),
      collapsible = FALSE,
      width = 12,
      # Enhanced description for better clarity
      tags$div(
        tags$p("Use the switches below to enable or disable the modules. Changes will take effect immediately, and you can save your configuration for future sessions."),
        tags$p(tags$em("Note:"), " Ensure you save your changes to retain them for future use.")
      ),
      hl(),
      prettySwitch(
        inputId = ns("enableall"),
        label = "Enable all features",
        status = "success",
        fill = TRUE
      ),
      hl(),
      fluidRow(
        col_2(
          h4("Feature"),
        ),
        col_5(
          h4("Description"),
        ),
        col_3(
          h4("Dependencies"),
        ),
        col_2(
          h4("Check dependencies"),
        )
      ),
      h5("Full modules"),
      enable_module(mod_id = "geostats",
                    mod_name = "Spatial interpolation",
                    description = "Tools for spatial interpolation.",
                    deps = "fields",
                    ns = ns),
      enable_module(mod_id = "plantmat",
                    mod_name = "Plant Maturity",
                    description = "Nonlinear models for predicting plant maturity",
                    deps = c("minpack.lm", "segmented"),
                    ns = ns),
      enable_module(mod_id = "plantmeas",
                    mod_name = "Plant measures",
                    description = "Tools for digital computing canopy height models",
                    deps = "fields",
                    ns = ns),
      enable_module(mod_id = "growthmodels",
                    mod_name = "Growth Models",
                    description = "Nonlinear models for growth curves",
                    deps = "minpack.lm",
                    ns = ns),
      hl(),
      h5("Features"),
      enable_module(mod_id = "animatets",
                    mod_name = "Time series animation",
                    description = "Create an animation in the module 'Time series'",
                    deps = "magick",
                    ns = ns),
      enable_module(mod_id = "cssloaders",
                    mod_name = "cssloaders",
                    description = "Add a loading animation (spinner) to outputs instead of greying them out",
                    deps = "shinycssloaders",
                    ns = ns),
      enable_module(mod_id = "synckmaps",
                    mod_name = "Synchronized Maps",
                    description = "Enable synchronize the basemap (eg., RGB mosaic) with the index (eg., NDVI) layer.",
                    deps = "leafsync",
                    ns = ns),
      enable_module(mod_id = "tidyterra",
                    mod_name = "ggplot2-like plots for raster",
                    description = "Enable the 'plot attribute' option in 'see as' dropdown menu of 'Evolution plot' in the 'Analyze' tab of time series module. This uses the tidyterra::geom_spatraster() to produce a ggplot2-like plot for the vegetation indexes.",
                    deps = "tidyterra",
                    ns = ns),
      enable_module(mod_id = "introjs",
                    mod_name = "Step-by-step introduction",
                    description = "Enable step-by-step introductions, and clickable hints in the application",
                    deps = "rintrojs",
                    ns = ns),
      enable_module(mod_id = "sparkline",
                    mod_name = "Sparklines",
                    description = "Include interactive sparkline charts at the bottom of a reactable output.",
                    deps = "sparkline",
                    ns = ns),
      enable_module(mod_id = "slider",
                    mod_name = "Slider comparision",
                    description = "Allows using a comparision slider in the 'Map individuals' and 'Map plot' tabs of the 'Analyze' menu.",
                    deps = "leaflet.extras2",
                    ns = ns),
      enable_module(mod_id = "plotinfo",
                    mod_name = "Plot Information",
                    description = "Allows obtaining plot information such as area, perimeter, lengh and width.",
                    deps = "lwgeom",
                    ns = ns),
      enable_module(mod_id = "overlayindex",
                    mod_name = "Overlay Index and RGB",
                    description = "Allows to overlay vegetation indexes above a true color (RGB) map in the 'Index' module.",
                    deps = "leafem",
                    ns = ns),
      enable_module(mod_id = "shapefileleaflet",
                    mod_name = "Build shapefile using leaflet rendering",
                    description = "Enables shapefile creation using Leaflet's rendering tools. When disabled, the shapefile will be generated through a native approach, utilizing rectangle drawings to perform zoom-in operations.",
                    deps = "base64enc",
                    ns = ns),
      enable_module(mod_id = "nasapower",
                    mod_name = "Tools for request NASA POWER data",
                    description = "Tools for request NASA POWER data",
                    deps = "httr",
                    ns = ns),

      hl(),
      actionButton(ns("save_btn"), label = tagList(icon("save"), "Save Settings"), class = "btn btn-primary"),
      actionButton(ns("reset_btn"), label = tagList(icon("undo"), "Reset to default settings"), class = "btn btn-primary")
    )
  )
}

#' config Server Functions
#'
#' @noRd
mod_config_server <- function(id, settings){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Define paths: inst for default, user-specific for custom settings
    inst_dir <- file.path(system.file(package = "plimanshiny"), "app/www")
    settings_file_default <- file.path(inst_dir, "default_module_settings.rds")
    # Create default settings (will be saved during package release)
    default_settings <- list(geostats = FALSE,
                             plantmat = FALSE,
                             plantmeas = FALSE,
                             animatets =  FALSE,
                             cssloaders = FALSE,
                             synckmaps = FALSE,
                             tidyterra = FALSE,
                             introjs = FALSE,
                             sparkline = FALSE,
                             slider = FALSE,
                             plotinfo = FALSE,
                             license = FALSE,
                             growthmodels = FALSE,
                             overlayindex = FALSE,
                             shapefileleaflet = FALSE,
                             nasapower = FALSE)
    saveRDS(default_settings, settings_file_default)


    # Path for user-specific settings
    user_settings_dir <- tools::R_user_dir("plimanshiny", which = "config")
    if (!dir.exists(user_settings_dir)) {
      dir.create(user_settings_dir, recursive = TRUE)
    }
    settings_file_user <- file.path(user_settings_dir, "user_module_settings.rds")

    # Load user settings if they exist, otherwise load default settings

    if (file.exists(settings_file_user)) {
      settings(readRDS(settings_file_user))
    } else {
      settings(readRDS(settings_file_default))
    }


    # load all
    observeEvent(input$enableall, {
      if (input$enableall) {
        pkgs <- c("fields", "minpack.lm", "segmented", "magick", "shinycssloaders",
                  "leafsync",  "tidyterra", "rintrojs",
                  "sparkline", "leaflet.extras2", "lwgeom", "leafem", "terra")
        check_and_install_dependencies(pkgs, ns, input, "enableall")
        settings(lapply(default_settings, \(x){x = TRUE}))
      }
    })

    # Update UI based on loaded settings
    observe({
      updatePrettySwitch(session = session, inputId = "geostats", value = settings()$geostats)
      updatePrettySwitch(session = session, inputId = "plantmat", value = settings()$plantmat)
      updatePrettySwitch(session = session, inputId = "plantmeas", value = settings()$plantmeas)
      updatePrettySwitch(session = session, inputId = "animatets", value = settings()$animatets)
      updatePrettySwitch(session = session, inputId = "cssloaders", value = settings()$cssloaders)
      updatePrettySwitch(session = session, inputId = "synckmaps", value = settings()$synckmaps)
      updatePrettySwitch(session = session, inputId = "tidyterra", value = settings()$tidyterra)
      updatePrettySwitch(session = session, inputId = "introjs", value = settings()$introjs)
      updatePrettySwitch(session = session, inputId = "sparkline", value = settings()$sparkline)
      updatePrettySwitch(session = session, inputId = "slider", value = settings()$slider)
      updatePrettySwitch(session = session, inputId = "plotinfo", value = settings()$plotinfo)
      updatePrettySwitch(session = session, inputId = "plotinfo", value = settings()$plotinfo)
      updatePrettySwitch(session = session, inputId = "growthmodels", value = settings()$growthmodels)
      updatePrettySwitch(session = session, inputId = "overlayindex", value = settings()$overlayindex)
      updatePrettySwitch(session = session, inputId = "shapefileleaflet", value = settings()$shapefileleaflet)
      updatePrettySwitch(session = session, inputId = "nasapower", value = settings()$nasapower)
    })

    # Reactively save the settings whenever the switch is changed
    observeEvent(input$save_btn, {
      current_settings <- list(geostats = input$geostats,
                               plantmat = input$plantmat,
                               plantmeas = input$plantmeas,
                               animatets = input$animatets,
                               cssloaders = input$cssloaders,
                               synckmaps = input$synckmaps,
                               tidyterra = input$tidyterra,
                               introjs = input$introjs,
                               sparkline = input$sparkline,
                               slider = input$slider,
                               plotinfo = input$plotinfo,
                               growthmodels = input$growthmodels,
                               overlayindex = input$overlayindex,
                               shapefileleaflet = input$shapefileleaflet,
                               license = TRUE,
                               nasapower = input$nasapower)
      settings(current_settings)  # Update global reactive settings
      saveRDS(current_settings, settings_file_user)
      showNotification("Settings saved successfully! You may need to restart the app to apply changes.", type = "message")
    })

    # check for installed packages
    # Applying the function to different use cases
    observe_dependency("geostats", c("fields"), ns, input)
    observe_dependency("check_geostats", c("fields"), ns, input)

    observe_dependency("plantmat", c("minpack.lm", "segmented"), ns, input)
    observe_dependency("check_plantmat", c("minpack.lm", "segmented"), ns, input)

    observe_dependency("plantmeas", c("fields"), ns, input)
    observe_dependency("check_plantmeas", c("fields"), ns, input)

    observe_dependency("animatets", c("magick"), ns, input)
    observe_dependency("check_animatets", c("magick"), ns, input)

    observe_dependency("cssloaders", c("shinycssloaders"), ns, input)
    observe_dependency("check_cssloaders", c("shinycssloaders"), ns, input)

    observe_dependency("synckmaps", c("leafsync"), ns, input)
    observe_dependency("check_synckmaps", c("leafsync"), ns, input)

    observe_dependency("tidyterra", c("tidyterra"), ns, input)
    observe_dependency("check_tidyterra", c("tidyterra"), ns, input)

    observe_dependency("introjs", c("rintrojs"), ns, input)
    observe_dependency("check_introjs", c("rintrojs"), ns, input)

    observe_dependency("sparkline", c("sparkline"), ns, input)
    observe_dependency("check_sparkline", c("sparkline"), ns, input)

    observe_dependency("slider", c("leaflet.extras2"), ns, input)
    observe_dependency("check_slider", c("leaflet.extras2"), ns, input)

    observe_dependency("plotinfo", c("lwgeom"), ns, input)
    observe_dependency("check_plotinfo", c("lwgeom"), ns, input)

    observe_dependency("growthmodels", c("minpack.lm"), ns, input)
    observe_dependency("check_growthmodels", c("minpack.lm"), ns, input)

    observe_dependency("overlayindex", c("leafem"), ns, input)
    observe_dependency("check_overlayindex", c("leafem"), ns, input)

    observe_dependency("shapefileleaflet", c("base64enc"), ns, input)
    observe_dependency("check_shapefileleaflet", c("base64enc"), ns, input)

    observe_dependency("nasapower", c("httr2", "progressr"), ns, input)
    observe_dependency("check_nasapower", c("httr2", "progressr"), ns, input)

    # Option to reset to default settings
    observeEvent(input$reset_btn, {
      settings(readRDS(settings_file_default))  # Reset to default
      # Remove the user-specific settings file, if it exists
      if (file.exists(settings_file_user)) {
        file.remove(settings_file_user)
        showNotification("User-specific settings removed and reset to default!", type = "message")
      } else {
        showNotification("Settings reset to default!", type = "message")
      }
    })
  })
}

## To be copied in the UI
# mod_config_ui("config_1")

## To be copied in the server
# mod_config_server("config_1")
