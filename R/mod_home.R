#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bs4Dash bs4Card valueBox
#' @importFrom shinyWidgets actionBttn
mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 10,
        bs4Card(
          title = glue::glue("{plimanshiny} version [packageVersion('plimanshiny')]", .open = "[", .close = "]"),
          width = 12,
          solidHeader = FALSE,
          img(src = "www/plimanshiny.png", width = "100%", height = "80%")
        )
      ),
      column(
        width = 2,
        bs4Dash::valueBox(
          value = "MAIN REFERENCE",
          subtitle = "Olivoto, T. 2022. Lights, camera, pliman! An R package for plant image analysis. Methods in Ecology and Evolution 13(4): 789–7898. doi: 10.1111/2041-210X.13803.",
          width = 12,
          color = "success",
          elevation = 3,
          href = "https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13803",
          icon = shiny::icon("braille")
        ),
        bs4Dash::valueBox(
          value = "PAPER TROPICAL PLANT PATHOLOGY",
          subtitle = "Olivoto, T., S.M.P. Andrade, and E.M. Del Ponte. 2022. Measuring plant disease severity in R: introducing and evaluating the pliman package. Tropical Plant Pathology 1: 1–710. doi: 10.1007/S40858-021-00487-5.",
          width = 12,
          color = "success",
          elevation = 3,
          href = "https://link.springer.com/article/10.1007/s40858-021-00487-5",
          icon = shiny::icon("braille")
        ),
        fluidRow(
          actionBttn(
            inputId = ns("about"),
            label = "About",
            color = "success",
            icon = icon("circle-info")
          )
        ),
        fluidRow(
          actionBttn(
            inputId = ns("checkupdate"),
            label = "Check for updates",
            color = "success",
            icon = icon("code-compare")
          )
        ),
        fluidRow(
          actionBttn(
            inputId = ns("license"),
            label = "Licence",
            color = "royal",
            icon = icon("rectangle-list")
          )
        ),
        fluidRow(
          actionBttn(
            inputId = ns("reload"),
            label = "Reload",
            color = "warning",
            icon = icon("rotate-right")
          )
        ),
        fluidRow(
          actionBttn(
            inputId = ns("nepem"),
            label = "NEPEM",
            color = "success",
            icon = icon("internet-explorer")
          )
        ),

        tags$head(
          tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
          tags$style(HTML("
            .modal-content {
              box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
            }
            .icon-center {
              display: flex;
              justify-content: center;
              align-items: center;
              font-size: 4em; /* Icon size */
              color: orange;  /* Icon color */
              margin-top: -20px; /* Positioning adjustment */
            }
          "))
        ),

        # Bootstrap modal for update message
        tags$div(
          class = "modal fade", id = ns("messageModal"), tabindex = "-1", role = "dialog",
          tags$div(
            class = "modal-dialog", role = "document",
            tags$div(
              class = "modal-content",
              tags$div(
                class = "modal-header",
                tags$h5(class = "modal-title", "Update Check Result"),
                tags$button(type = "button", class = "close", `data-dismiss` = "modal", `aria-label` = "Close",
                            tags$span(`aria-hidden` = "true", HTML("&times;"))
                )
              ),
              tags$div(
                class = "modal-body",
                tags$div(class = "icon-center",
                         tags$i(id = ns("icon"), class = "") # Icon class will be set by JS
                ),
                tags$p(id = ns("popupMessage"), "") # Message text will be set by JS
              ),
              tags$div(
                class = "modal-footer",
                # New buttons for the update prompt
                tags$button(type = "button", class = "btn btn-secondary", `data-dismiss` = "modal", id = ns("remind-btn"), "Remind Me Later"),
                tags$button(type = "button", class = "btn btn-success", id = ns("install-btn"), "Install Now"),
                tags$button(type = "button", class = "btn btn-primary", `data-dismiss` = "modal", id = ns("ok-btn"), "OK")
              )
            )
          )
        ),

        # Include custom JavaScript
        tags$script(HTML(paste0("
          // Pass the shiny module namespace to JavaScript
          const nsPrefix = '", ns(""), "';

          const fetchCurrentVersion = async () => {
            try {
              const response = await fetch('/myjson/version.json');
              const data = await response.json();
              return data.version;
            } catch (error) {
              console.error('Error fetching the current version:', error);
              return null;
            }
          };

          const fetchLatestVersion = async () => {
            try {
              const response = await fetch('https://nepemufsc.com/.netlify/functions/verser?project=plimanshiny');
              const data = await response.json();
              return data.latest_version;
            } catch (error) {
              console.error('Error fetching the latest version:', error);
              return null;
            }
          };

          // Function to configure and show the modal
          const showUpdateModal = (config) => {
            document.getElementById(nsPrefix + 'icon').className = config.iconClass;
            document.getElementById(nsPrefix + 'popupMessage').innerHTML = config.message;

            // Show/hide buttons based on the context
            $('#' + nsPrefix + 'install-btn').toggle(config.showInstall);
            $('#' + nsPrefix + 'remind-btn').toggle(config.showRemind);
            $('#' + nsPrefix + 'ok-btn').toggle(config.showOk);

            $('#' + nsPrefix + 'messageModal').modal('show');
          };


          const CheckUpdates = async (is_start = false) => {
            try {
              const latestVersion = await fetchLatestVersion();
              const currentVersion = await fetchCurrentVersion();
              console.log('Latest version:', latestVersion);
              console.log('Current version:', currentVersion);

              if (!latestVersion || !currentVersion) {
                 if (!is_start) { // Only show error on manual check
                    showUpdateModal({
                      message: 'Error while checking for updates. Could not fetch version information.',
                      iconClass: 'fas fa-times-circle text-danger',
                      showInstall: false, showRemind: false, showOk: true
                    });
                 }
                 return;
              }

              if (latestVersion !== currentVersion) {
                // Update is available
                showUpdateModal({
                  message: `An update is available! You have version <strong>${currentVersion}</strong>, but version <strong>${latestVersion}</strong> is available. Do you want to install it now?`,
                  iconClass: 'fas fa-exclamation-triangle text-warning',
                  showInstall: true, showRemind: true, showOk: false
                });
              } else if (!is_start) {
                // App is up-to-date, only notify on manual check
                showUpdateModal({
                  message: 'Congratulations! You are using the latest version of plimanshiny!',
                  iconClass: 'fas fa-check-circle text-success',
                  showInstall: false, showRemind: false, showOk: true
                });
              }

            } catch (error) {
              console.error('Error checking for updates:', error);
              if (!is_start) {
                 showUpdateModal({
                   message: 'An error occurred while checking for updates.',
                   iconClass: 'fas fa-times-circle text-danger',
                   showInstall: false, showRemind: false, showOk: true
                 });
              }
            }
          };

          // Event listener for the manual 'Check for updates' button
          $(document).on('click', '#' + nsPrefix + 'checkupdate', function() {
            CheckUpdates(false); // is_start = false for manual check
          });

          // Event listener for the 'Install Now' button
          $(document).on('click', '#' + nsPrefix + 'install-btn', function() {
            $('#' + nsPrefix + 'messageModal').modal('hide');
            // Send a message to the R server to start the installation
            Shiny.setInputValue(nsPrefix + 'start_install', Math.random());
          });

          // Automatically check for updates when the app loads
          document.addEventListener('DOMContentLoaded', function() {
            CheckUpdates(true); // is_start = true for automatic check
          });
        ")))
      )
    )
  )
}

#' home Server Functions
#'
#' @noRd
#' @import shiny
#' @import waiter
#' @importFrom utils packageVersion browseURL
mod_home_server <- function(id, settings){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    json_file_path <- system.file("app/www/version.json", package = "plimanshiny", mustWork = TRUE)
    addResourcePath('myjson', dirname(json_file_path))

    # Observer to trigger the installation process
    observeEvent(input$start_install, {
      # Show a waiter screen to inform the user
      waiter_show(
        html = tagList(
          spin_solar(),
          h3("Installing update..."),
          p("This may take a few moments. The app will be unresponsive during the installation.")
        ),
        color = "#228B22" # ForestGreen
      )

      # Use a future to run installation asynchronously and avoid freezing the UI feedback
      # In practice, the R session is still blocked by pak, but this is a good pattern.
      result <- tryCatch({
        # Ensure pak is installed
        if (!requireNamespace("pak", quietly = TRUE)) {
          utils::install.packages("pak", repos = "https://r-lib.github.io/p/pak/dev/")
        }
        # Run the installation
        pak::pkg_install("NEPEM-UFSC/plimanshiny", ask = FALSE)
        list(success = TRUE, message = "Update installed successfully!")
      }, error = function(e) {
        list(success = FALSE, message = paste("An error occurred during installation:", e$message))
      })

      # Hide the waiter
      waiter_hide()

      # Show a confirmation modal
      if (result$success) {
        showModal(
          modalDialog(
            title = tagList(icon("check-circle"), "Update Successful"),
            "The new version has been installed. Please reload the application to apply the changes.",
            footer = tagList(
              actionButton(ns("reload_app"), "Reload Now", icon = icon("rotate-right"), class = "btn-success")
            ),
            easyClose = FALSE
          )
        )
      } else {
        showModal(
          modalDialog(
            title = tagList(icon("times-circle"), " Update Failed"),
            result$message,
            easyClose = TRUE,
            footer = modalButton("Close")
          )
        )
      }
    })

    # Reload app when reload button is clicked
    observeEvent(input$reload, {
      session$reload()
    })

    # Reload the app after successful installation
    observeEvent(input$reload_app, {
      session$reload()
    })

    observeEvent(input$nepem, {
      browseURL("https://nepemufsc.com/")
    })

    observeEvent(input$close_modal, {
      inst_dir <- file.path(system.file(package = "plimanshiny"), "app/www")
      licenseread <- file.path(inst_dir, "license.rds")
      saveRDS(list(licenseread = TRUE), licenseread)
      removeModal()
    })

    observeEvent(input$license, {
      show_licence(ns)
    })

    observeEvent(input$about, {
      showModal(
        modalDialog(
          title = glue::glue("Version: {packageVersion('plimanshiny')}"),
          fluidRow(
            col_7(
              img(src = "www/help_logo.png", width = "100%", height = "100%"),
            ),
            col_5(
              box(
                width = 12,
                headerBorder = FALSE,
                collapsible = TRUE,
                closable = TRUE,
                h2("About"),
                "{plimanshiny} provides an interactive Shiny-based graphical user interface for the pliman package,
                 facilitating user-friendly access to advanced plant image analysis tools without the need
                 for extensive programming knowledge. This package integrates a variety of functionalities
                 for high-throughput phenotyping, including but not limited to orthomosaic analysis from drone
                 and satellite imagery, shapefile creation and handling, time series analysis, image analysis,
                 and phytopathometry, into a cohesive and intuitive application.", br(),br(),
                h2("Developer"),
                a("Prof. Dr. Tiago Olivoto", href = "https://olivoto.netlify.app/", target = "_blank"), br(),
                "Department of Plant Science", br(),
                "Federal University of Santa Catarina", br(), br(),
                h2("Contribution"),
                a("Matheus Lopes Machado", href = "https://www.linkedin.com/in/matheus-lopesma/", target = "_blank"), br(),
                a("Dr. Leonardo Volpato", href = "https://www.linkedin.com/in/leonardo-volpato/", target = "_blank"), br(),
                a("Dr. Arthur Bernardeli", href = "https://www.linkedin.com/in/arthur-bernardeli-5a1a0b5a/", target = "_blank"), br(),br(),
                fluidRow(
                  col_6(
                    shiny::actionButton(inputId= ns("gitpliman"),
                                        label="pliman",
                                        icon = icon("github"),
                                        onclick ="window.open('https://github.com/NEPEM-UFSC/pliman', '_blank')"),
                  ),
                  col_6(
                    shiny::actionButton(inputId= ns("gitplimanshiny"),
                                        label="plimanshiny",
                                        icon = icon("github"),
                                        onclick ="window.open('https://github.com/NEPEM-UFSC/plimanshiny', '_blank')"),
                  )
                )
              )
            )
          ),


          footer = NULL,
          easyClose = TRUE,
          size = "xl"
        )
      )

    })

  })
}

## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")
