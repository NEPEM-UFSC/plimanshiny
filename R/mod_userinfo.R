#' userinfo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_userinfo_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' userinfo Server Functions
#'
#' @noRd
mod_userinfo_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # Load country and region data
    country_path <- system.file("app/www/country_data.rds", package = "plimanshiny", mustWork = FALSE)
    mun_df <- readRDS(country_path)

    user_dir <- tools::R_user_dir("plimanshiny", which = "config")
    if (!dir.exists(user_dir)){
      dir.create(user_dir, recursive = TRUE)
    }
    user_info_file <- file.path(user_dir, "user_info.rds")
    user_info <- reactiveVal()

    # Create modal UI with dynamic inputs
    if (file.exists(user_info_file)) {
      user_info(readRDS(user_info_file))
    } else {
      showModal(
        modalDialog(
          title = "Welcome to plimanshiny!",
          h2("User information"),
          fluidRow(
            col_4(
              textInput(ns("user_name"), label = tagList(icon("user"), "Name"), placeholder = "Enter your name")
            ),
            col_4(
              textInput(ns("user_email"), label = tagList(icon("envelope"), "Email"), placeholder = "Enter a valid email")
            ),
            col_4(
              textInput(ns("user_inst"), label = tagList(icon("university"), "Institution"), placeholder = "Enter your institution name")
            )
          ),
          fluidRow(
            col_4(
              pickerInput(ns("country"),
                          label = tagList(icon("globe"), "Country"),
                          choices = c("", unique(mun_df$country_name)),
                          selected = NA,
                          options = list(
                            `live-search` = TRUE
                          ))
            ),
            col_4(
              uiOutput(ns("region_ui"))
            ),
            col_4(
              pickerInput(ns("user_type"),
                          label = tagList(icon("user"), "User type"),
                          choices = c(
                            "",
                            "Student (Undergraduate)",
                            "Student (Graduate)",
                            "Professor / Lecturer",
                            "Researcher (Academic)",
                            "Postdoctoral Fellow",
                            "Technical Staff (University/Public Sector)",
                            "Extension Professional",
                            "Government Employee",
                            "Private Consultant (Commercial)",
                            "Company Representative (Commercial)",
                            "Researcher (Commercial)",
                            "Software Developer (Commercial)",
                            "Other"
                          ),
                          selected = NULL)
            )
          ),
          br(),
          h4("Privacy and Data Protection"),
          "Your personal data (name, email, and institution) are collected solely for internal use to improve the application and track non-commercial usage. ",
          "These data will never be shared with third parties, sold, or used for any commercial or advertising purpose. ",
          "All information is stored securely and treated with confidentiality.", br(),
          bs4TabCard(
            id = "tabs",
            status = "success",
            width = 12,
            title = "",
            selected = "About",
            solidHeader = FALSE,
            type = "tabs",
            tabPanel("About",
                     "plimanshiny provides an interactive Shiny-based graphical user interface for the pliman package,
            facilitating user-friendly access to advanced plant image analysis tools without the need
            for extensive programming knowledge. This package integrates a variety of functionalities
            for high-throughput phenotyping, including but not limited to orthomosaic analysis from drone
            and satellite imagery, shapefile creation and handling, time series analysis, image analysis,
            and phytopathometry, into a cohesive and intuitive application."
            ),
            tabPanel("License Agreement",
                     "This software is licensed under the Creative Commons Attribution-NonCommercial (CC BY-NC) license.
            Under this license, you are free to use, share, and adapt the software for non-commercial purposes,
            provided that you give appropriate credit to the developers. For more details on the license, see the ",
                     tags$a(href = "https://creativecommons.org/licenses/by-nc/4.0/", "Creative Commons BY-NC 4.0 License"), ".",
            ),
            tabPanel("Licence Terms",
                     strong("Non-Commercial Educational Use Only"),
                     "The use of this application is limited to non-commercial educational purposes only. This means
            it is available for free use by individuals, academic institutions, and organizations solely for academic
            research, or personal educational purposes. Commercial use of any kind, including by companies or
            other entities with the intent of profit or business gain, is strictly prohibited unless explicit
            permission is granted.",
                     br(), br(),
                     h3("Terms for Commercial Use"),
                     "If your company wishes to use plimanshiny for commercial purposes, please contact us at ",
                     tags$a(href="mailto:contato@nepemufsc.com", "contato@nepemufsc.com"),
                     " to discuss commercial terms and licensing fees.",
                     br(), br(),
                     h3("Penalty Clause"),
                     "Companies that use plimanshiny for commercial purposes without proper authorization will
            incur a penalty of 0.1% of their gross revenue for each violation. This penalty
            is intended to enforce compliance with the terms of use and to prevent unauthorized commercial
            exploitation of this software.",
                     br(), br(),
                     "By using this application, you agree to the following terms:",
                     tags$ul(
                       tags$li("You may not use this software for commercial purposes or for financial gain unless authorized."),
                       tags$li("Any redistribution of this software must include the original GNU General Public License text, as well as attribution to the original authors and developers."),
                       tags$li("This software is provided 'as is' without any warranty. The developers assume no liability for any consequences resulting from its use."),
                       tags$li("Violations of these terms may result in legal action or termination of access to the software.")
                     )
            ),
            tabPanel("Copyright",
                     "Copyright (C) 2025 - Núcleo de Estudos e Pesquisas em Experimentação e Melhoramento Vegetal - ",
                     tags$a(href="https://nepemufsc.com", "nepemufsc.com"),
            )
          ),
          footer = tagList(
            fluidRow(
              column(8, checkboxInput(ns("agree_terms"), "I agree with the terms and conditions", value = FALSE)),
              column(4, align = "right", actionButton(ns("save_user_info"), "Submit informations", class = "btn btn-primary", disabled = TRUE))
            )
          ),
          easyClose = FALSE,
          size = "xl"
        )
      )
    }

    # Update region dropdown based on selected country
    output$region_ui <- renderUI({
      req(input$country)
      choices <- c("", mun_df$subdivision_name[mun_df$country_name == input$country])
      pickerInput(ns("region"),
                  label = tagList(icon("map"), "Region / State"),
                  choices = choices,
                  selected = NULL,
                  options = list(
                    `live-search` = TRUE
                  ))
    })

    observe({
      shinyjs::toggleState(id = "save_user_info", condition = isTRUE(input$agree_terms))
    })



    observeEvent(input$save_user_info, {
      # req(input$user_name, input$user_email, input$user_inst, input$country, input$region, input$user_type, input$agree_terms)

      # Nome
      if (is.null(input$user_name) || trimws(input$user_name) == "") {
        show_alert(
          title = "Missing name",
          text = "Please enter your full name.",
          type = "warning"
        )
        return()
      }

      # E-mail
      if (is.null(input$user_email) || trimws(input$user_email) == "") {
        show_alert(
          title = "Missing email",
          text = "Please enter your email address.",
          type = "warning"
        )
        return()
      }
      if (!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", input$user_email)) {
        show_alert(
          title = "Invalid email",
          text = "Please enter a valid email address (e.g., yourname@example.com).",
          type = "error"
        )
        return()
      }

      # Instituição
      if (is.null(input$user_inst) || trimws(input$user_inst) == "") {
        show_alert(
          title = "Missing institution",
          text = "Please enter your institution or organization.",
          type = "warning"
        )
        return()
      }

      # País
      if (is.null(input$country) || trimws(input$country) == "") {
        show_alert(
          title = "Missing country",
          text = "Please select your country.",
          type = "warning"
        )
        return()
      }

      # Região
      if (is.null(input$region) || trimws(input$region) == "") {
        show_alert(
          title = "Missing region",
          text = "Please select your region.",
          type = "warning"
        )
        return()
      }

      # Tipo de usuário
      if (is.null(input$user_type) || trimws(input$user_type) == "") {
        show_alert(
          title = "Missing user type",
          text = "Please select your user type.",
          type = "warning"
        )
        return()
      }


      info <- list(
        name = input$user_name,
        email = input$user_email,
        institution = input$user_inst,
        country = input$country,
        region = input$region,
        user_type = input$user_type,
        token = generate_uuid(input$user_name, input$user_email)
      )

      # Mostrar mensagem de progresso
      sendSweetAlert(
        session = session,
        title = "Registering your data",
        text = "{plimanshiny} is sending your information. Please wait...",
        type = "info",
        btn_labels = NA
      )

      tryCatch({
        resp <-
          httr2::request("https://script.google.com/macros/s/AKfycbxSI5xxJVHY6hekiIiGNhE0WD4bic1g496P0cn_UqjJMFZZfzu2hYep4SyKySDhX5CF/exec") |>
          httr2::req_method("POST") |>
          httr2::req_headers(`Content-Type` = "application/json") |>
          httr2::req_body_json(info) |>
          httr2::req_perform()

        status <- httr2::resp_status(resp)
        if (status >= 200 && status < 300) {
          # Success (silent)
          invisible(TRUE)
        } else {
          show_alert(
            title = "Warning",
            text = sprintf("The request was made but returned status code: %s", status),
            type = "warning"
          )
        }

        closeSweetAlert(session = session)
        removeModal()

        saveRDS(info, user_info_file)
        user_info(info)

        if (grepl("\\(Commercial\\)", input$user_type)) {
          show_alert(
            title = "Information saved",
            text = paste(
              "Your information has been successfully saved.",
              "Please note that plimanshiny is free for non-commercial use only.",
              "If you intend to use it commercially, please contact us at:",
              "contato@nepemufsc.com"
            ),
            type = "warning"
          )
        } else {
          show_alert(
            title = "All set!",
            text = "Your information has been successfully saved. Enjoy using plimanshiny!",
            type = "success"
          )
        }




      }, error = function(e) {
        show_alert(
          title = "Connection error",
          text = paste("Failed to send data to the server:", conditionMessage(e)),
          type = "error"
        )
      })




    })
    return(user_info)
  })
}
