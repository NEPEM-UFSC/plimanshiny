#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
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
          subtitle = "Olivoto, T. 2022. Lights, camera, pliman! An R package for plant image analysis. Methods in Ecology and Evolution 13(4): 789\u20137898. doi: 10.1111/2041-210X.13803.",
          width = 12,
          color = "success",
          elevation = 3,
          href = "https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13803",
          icon = shiny::icon("braille")
        ),
        bs4Dash::valueBox(
          value = "PAPER TROPICAL PLANT PATHOLOGY",
          subtitle = "Olivoto, T., S.M.P. Andrade, and E.M. Del Ponte. 2022. Measuring plant disease severity in R: introducing and evaluating the pliman package. Tropical Plant Pathology 1: 1\u2013710. doi: 10.1007/S40858-021-00487-5.",
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
            inputId = NS("example", "checkupdate"),  # Ensure proper namespacing
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
            inputId = ns("reload"),  # Ensure proper namespacing
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
          # Include Font Awesome for icons and additional CSS for shadow effects
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
    .modal-header .close {
      color: #28A745; /* Close button color */
    }
    .modal-header .close:hover {
      color: #1e7e34; /* Hover color for close button */
    }
    .btn-secondary {
      background-color: #28A745; /* Button color */
      border-color: #28A745; /* Button border color */
    }
    .btn-secondary:hover {
      background-color: #218838; /* Hover color for button */
      border-color: #1e7e34; /* Hover border color for button */
    }
  "))
        ),

        # Bootstrap modal for popup message
        tags$div(
          class = "modal fade", id = NS("example", "messageModal"), tabindex = "-1", role = "dialog",
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
                # Center the icon at the top and allow its class to change
                tags$div(class = "icon-center",
                         tags$i(id = "example-icon", class = "fas fa-exclamation-triangle")  # Initial icon class with correct ID
                ),
                # Message text below the icon
                p(id = NS("example", "popupMessage"),
                  ""
                )
              ),
              tags$div(
                class = "modal-footer",
                tags$button(type = "button", class = "btn btn-secondary", `data-dismiss` = "modal", "Close")
              )
            )
          )
        ),


        # Include custom JavaScript
        tags$script(HTML("
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

    // Function to update the icon based on the result
    const updateIcon = (iconClass) => {
      const iconElement = document.getElementById('example-icon');
      if (iconElement) {
        iconElement.className = iconClass;
      } else {
        console.error('Icon element not found');
      }
    };

    // Function to copy text to clipboard
    const copyToClipboard = (text) => {
      if (navigator.clipboard) {
        navigator.clipboard.writeText(text).then(() => {
          alert('GitHub installation command copied to clipboard!');
        }).catch(err => {
          console.error('Error copying text via clipboard API:', err);
        });
      } else {
        // Fallback method for older browsers
        const tempInput = document.createElement('textarea');
        tempInput.value = text;
        document.body.appendChild(tempInput);
        tempInput.select();
        try {
          document.execCommand('copy');
          alert('GitHub installation command copied to clipboard!');
        } catch (err) {
          console.error('Error copying text via execCommand:', err);
        }
        document.body.removeChild(tempInput);
      }
    };

    // Attach event listener for dynamically generated copy button
    $(document).on('click', '#copy-button', function() {
      const textToCopy = document.getElementById('install-command').textContent;
      copyToClipboard(textToCopy);
    });

    const CheckUpdates = async (is_start) => {
      try {
        const latestVersion = await fetchLatestVersion();
        const currentVersion = await fetchCurrentVersion();
        console.log('Latest version:', latestVersion);
        console.log('Current version:', currentVersion);
        let message = '';
        let iconClass = '';

        if (is_start) {
          if (latestVersion && currentVersion && latestVersion != currentVersion) {
            message = `The application is outdated. You have version '${currentVersion}', but version '${latestVersion}' is now available.<br/><br/>
                       Use the package {pak} to install the latest version from GitHub:<br/>
                       <code id='install-command'>pak::pkg_install(\"NEPEM-UFSC/plimanshiny\")</code>
                       <button id='copy-button' class='btn btn-sm btn-primary'>Copy</button>`;
            iconClass = 'fas fa-exclamation-triangle text-warning';

            document.getElementById('example-popupMessage').innerHTML = message;
            updateIcon(iconClass);
            $('#example-messageModal').modal('show');
          }
        } else {
          if (latestVersion && currentVersion) {
            if (latestVersion === currentVersion) {
              message = 'Congratulations! You are using the latest version of plimanshiny!';
              iconClass = 'fas fa-check-circle text-success';
            } else {
              message = `The application is outdated. You have version '${currentVersion}', but version '${latestVersion}' is now available.<br/><br/>
                         Use the package {pak} to install the latest version from GitHub:<br/>
                         <code id='install-command'>pak::pkg_install(\"NEPEM-UFSC/plimanshiny\")</code>
                         <button id='copy-button' class='btn btn-sm btn-primary'>Copy</button>`;
              iconClass = 'fas fa-exclamation-triangle text-warning';
            }
          } else {
            message = 'Error while checking for updates';
            iconClass = 'fas fa-times-circle text-danger';
          }

          document.getElementById('example-popupMessage').innerHTML = message;
          updateIcon(iconClass);
          $('#example-messageModal').modal('show');
        }
      } catch (error) {
        console.error('Error checking for updates:', error);
        document.getElementById('example-popupMessage').innerHTML = 'Error while checking for updates';
        updateIcon('fas fa-times-circle text-danger');
        $('#example-messageModal').modal('show');
      }
    };

    $(document).on('click', '#example-checkupdate', function() {
      CheckUpdates();
    });

    document.addEventListener('DOMContentLoaded', function() {
      CheckUpdates(true);
    });
"))





      )
    )
  )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id, settings){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    json_file_path <- system.file("app/www/version.json", package = "plimanshiny", mustWork = TRUE)
    addResourcePath('myjson', dirname(json_file_path))

    # Reload app when reload button is clicked
    observeEvent(input$reload, {
      session$reload()
    })

    observeEvent(input$nepem, {
      browseURL("https://nepemufsc.com/")
    })

    observe({
      inst_dir <- file.path(system.file(package = "plimanshiny"), "app/www")
      licenseread <- file.path(inst_dir, "license.rds")
      if (!file.exists(licenseread)) {
        show_licence(ns)
      }
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
          title = "About plimanshiny",
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
