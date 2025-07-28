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

    user_dir <- tools::R_user_dir("plimanshiny", which = "config")
    if (!dir.exists(user_dir)){
      dir.create(user_dir, recursive = TRUE)
    }
    user_info_file <- file.path(user_dir, "user_info.rds")
    # file.remove(user_info_file)
    user_info <- reactiveVal()

    if (file.exists(user_info_file)) {
      user_info(readRDS(user_info_file))
    } else {
      showModal(
        modalDialog(
          title = "Welcome to plimanshiny!",
          h2("User information"),
          fluidRow(
            column(4,
                   textInput(ns("user_name"), label = tagList(icon("user"), "Name"))
            ),
            column(4,
                   textInput(ns("user_email"), label = tagList(icon("envelope"), "Email"))
            ),
            column(4,
                   textInput(ns("user_inst"), label = tagList(icon("university"), "Institution"))
            )
          ),
          br(), tags$hr(),
          tags$div(
            style = "max-height: 550px; overflow-y: auto; padding-right: 55px;",
            h2("About"),
            "plimanshiny provides an interactive Shiny-based graphical user interface for the pliman package,
            facilitating user-friendly access to advanced plant image analysis tools without the need
            for extensive programming knowledge. This package integrates a variety of functionalities
            for high-throughput phenotyping, including but not limited to orthomosaic analysis from drone
            and satellite imagery, shapefile creation and handling, time series analysis, image analysis,
            and phytopathometry, into a cohesive and intuitive application.",
            br(), tags$hr(),
            h2("License Agreement"),
            "This software is licensed under the Creative Commons Attribution-NonCommercial (CC BY-NC) license.
            Under this license, you are free to use, share, and adapt the software for non-commercial purposes,
            provided that you give appropriate credit to the developers. For more details on the license, see the ",
            tags$a(href = "https://creativecommons.org/licenses/by-nc/4.0/", "Creative Commons BY-NC 4.0 License"), ".",
            br(), br(),
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
            ),
            br(),
            h3("Privacy and Data Protection"),
            "Your personal data (name, email, and institution) are collected solely for internal use to improve the application and track non-commercial usage. ",
            "These data will never be shared with third parties, sold, or used for any commercial or advertising purpose. ",
            "All information is stored securely and treated with confidentiality.",
            br(), tags$hr(),
            h2("Copyright"),
            "Copyright (C) 2025 - Núcleo de Estudos e Pesquisas em Experimentação e Melhoramento Vegetal - ",
            tags$a(href="https://nepemufsc.com", "nepemufsc.com"),
            br(),
          ),
          checkboxInput(ns("agree_terms"), "I agree with the terms and conditions", value = FALSE),
          footer = tagList(
            actionButton(ns("save_user_info"), "Submit", class = "btn btn-primary", disabled = TRUE)
          ),
          easyClose = FALSE,
          size = "xl"
        )
      )
    }

    webhook_url <- "https://script.google.com/macros/s/AKfycby-pUjM15CK_RWX2Uw7-ZEFCIuquMgMdV-brF5OzMOUNl_zz-NSouYRLAcf4ZdY-BtG/exec"

    observe({
      shinyjs::toggleState(id = "save_user_info", condition = isTRUE(input$agree_terms))
    })

    observeEvent(input$save_user_info, {
      req(input$user_name, input$user_email, input$user_inst, input$agree_terms)

      info <- list(
        name = input$user_name,
        email = input$user_email,
        institution = input$user_inst
      )

      saveRDS(info, user_info_file)
      user_info(info)

      try({
        httr2::request(webhook_url) |>
          httr2::req_method("POST") |>
          httr2::req_headers(`Content-Type` = "application/json") |>
          httr2::req_body_json(info) |>
          httr2::req_perform()
      }, silent = TRUE)

      removeModal()
      showNotification("Information saved successfully! Enjoy the app!", type = "message")
    })

    return(user_info)
  })
}
