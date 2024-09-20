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
      height = "760px",
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
      enable_module(mod_id = "geostats",
                    mod_name = "Spatial interpolation",
                    description = "Tools for spatial interpolation.",
                    deps = "fields",
                    ns = ns),
      enable_module(mod_id = "plantmat",
                    mod_name = "Plant Maturity",
                    description = "Nonlinear models for predicting plant maturity",
                    deps = c("drc", "segmented"),
                    ns = ns),
      enable_module(mod_id = "plantmeas",
                    mod_name = "Plant measures",
                    description = "Tools for digital computing canopy height models",
                    deps = "fields",
                    ns = ns),
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
      enable_module(mod_id = "histoslider",
                    mod_name = "Histogram Slider",
                    description = "Enable the histogram slider to adjust the range of values of computed indexes in the 'Index' module.",
                    deps = "histoslider",
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
                             histoslider = FALSE)
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
        pkgs <- c("fields", "drc", "segmented", "magick", "shinycssloaders", "leafsync", "histoslider")
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
      updatePrettySwitch(session = session, inputId = "histoslider", value = settings()$histoslider)
    })

    # Reactively save the settings whenever the switch is changed
    observeEvent(input$save_btn, {
      current_settings <- list(geostats = input$geostats,
                               plantmat = input$plantmat,
                               plantmeas = input$plantmeas,
                               animatets = input$animatets,
                               cssloaders = input$cssloaders,
                               synckmaps = input$synckmaps,
                               histoslider = input$histoslider)
      settings(current_settings)  # Update global reactive settings
      saveRDS(current_settings, settings_file_user)
      showNotification("Settings saved for further sections!", type = "message")
    })

    # check for installed packages
    # geostatistic
    observeEvent(input$geostats, {
      if (input$geostats) {
        check_and_install_dependencies(c("fields"), ns, input, "geostats")
      }
    })
    observeEvent(input$check_geostats, {
      if (input$check_geostats) {
        check_and_install_dependencies(c("fields"), ns, input, "check_geostats")
      }
    })
    # plant maturity
    observeEvent(input$plantmat, {
      if (input$plantmat) {
        check_and_install_dependencies(c("drc", "segmented"), ns, input, "plantmat")
      }
    }, ignoreInit = TRUE)
    observeEvent(input$check_plantmat, {
      if (input$check_plantmat) {
        check_and_install_dependencies(c("drc", "segmented"), ns, input, "check_plantmat")
      }
    }, ignoreInit = TRUE)

    # plant measures
    observeEvent(input$plantmeas, {
      if (input$plantmeas) {
        check_and_install_dependencies(c("fields"), ns, input, "plantmeas")
      }
    }, ignoreInit = TRUE)

    observeEvent(input$check_plantmeas, {
      if (input$check_plantmeas) {
        check_and_install_dependencies(c("fields"), ns, input, "check_plantmeas")
      }
    }, ignoreInit = TRUE)

    # Animate
    observeEvent(input$animatets, {
      if (input$animatets) {
        check_and_install_dependencies(c("magick"), ns, input, "animatets")
      }
    }, ignoreInit = TRUE)

    observeEvent(input$check_animatets, {
      if (input$check_animatets) {
        check_and_install_dependencies(c("magick"), ns, input, "check_animatets")
      }
    }, ignoreInit = TRUE)

    # cssloaders
    observeEvent(input$cssloaders, {
      if (input$cssloaders) {
        check_and_install_dependencies(c("shinycssloaders"), ns, input, "cssloaders")
      }
    }, ignoreInit = TRUE)

    observeEvent(input$check_cssloaders, {
      if (input$check_cssloaders) {
        check_and_install_dependencies(c("shinycssloaders"), ns, input, "check_cssloaders")
      }
    }, ignoreInit = TRUE)

    # synckmaps
    observeEvent(input$synckmaps, {
      if (input$synckmaps) {
        check_and_install_dependencies(c("leafsync"), ns, input, "synckmaps")
      }
    }, ignoreInit = TRUE)

    observeEvent(input$check_synckmaps, {
      if (input$check_synckmaps) {
        check_and_install_dependencies(c("leafsync"), ns, input, "check_synckmaps")
      }
    }, ignoreInit = TRUE)

    # histoslider
    observeEvent(input$histoslider, {
      if (input$histoslider) {
        check_and_install_dependencies(c("histoslider"), ns, input, "histoslider")
      }
    }, ignoreInit = TRUE)

    observeEvent(input$check_histoslider, {
      if (input$check_histoslider) {
        check_and_install_dependencies(c("histoslider"), ns, input, "check_histoslider")
      }
    }, ignoreInit = TRUE)





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
