#' dfjoin UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dfjoin_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Join datasets",
          collapsible = FALSE,
          width = 12,
          height = "760px",
          prettyRadioButtons(
            inputId = ns("dforshape"),
            label = "Join",
            choices = c("data.frames", "shapefiles", "data.frames with a shapefile"),
            icon = icon("check"),
            bigger = TRUE,
            status = "info",
            animation = "jelly",
            inline = TRUE
          ),
          conditionalPanel(
            condition = "input.dforshape == 'data.frames'", ns = ns,
            pickerInput(
              ns("dftojoin"),
              label = "Dataset(s) to join",
              choices = NULL,
              multiple = TRUE
            )
          ),
          conditionalPanel(
            condition = "input.dforshape == 'shapefiles'", ns = ns,
            pickerInput(
              ns("shapetojoin"),
              label = "Shapefile",
              choices = NULL,
              multiple = TRUE
            )
          ),
          conditionalPanel(
            condition = "input.dforshape == 'data.frames with a shapefile'", ns = ns,
            pickerInput(
              ns("dftojoinshp"),
              label = "Dataset",
              choices = NULL,
              multiple = FALSE
            ),
            pickerInput(
              ns("shapetojoin2"),
              label = "Shapefile",
              choices = NULL,
              multiple = FALSE
            )
          ),
          awesomeRadio(
            inputId = ns("type"),
            label = "Mutating join",
            choices = c("left", "right", "full"),
            selected = "left",
            inline = TRUE,
            status = "success"
          ),
          textInput(
            ns("newset"),
            label = "New dataset",
            value = "df_joined"
          ),
          actionBttn(
            ns("donejoining"),
            label = "Done joining",
            icon = icon("check")
          )
        )
      ),
      col_9(
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "760px",
          title = "Merged data",
          selected = "Merged data",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Merged data",
            pickerInput(ns("varstojoin"),
                        label = "Variable(s) to join by",
                        choices = NULL,
                        multiple = TRUE),
            reactableOutput(ns("joined"), height = "640px")
          )
        )
      )
    )
  )
}

#' dfjoin Server Functions
#'
#' @noRd
mod_dfjoin_server <- function(id, dfs, shapefile, settings){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      updatePickerInput(session, "dftojoin",
                        choices = names(dfs))
    })
    observe({
      updatePickerInput(session, "dftojoinshp",
                        choices = names(dfs),
                        selected = NA)
    })
    observe({
      updatePickerInput(session, "shapetojoin",
                        choices = setdiff(names(shapefile), c("shapefile", "shapefileplot")),
                        selected = NA)
    })
    observe({
      updatePickerInput(session, "shapetojoin2",
                        choices = setdiff(names(shapefile), c("shapefile", "shapefileplot")),
                        selected = NA)
    })


    result <- reactiveValues()
    dfstojoin <- reactiveValues()
    observe({
      req(dfstojoin)
      commvar <- Reduce(base::intersect, lapply(dfstojoin$vals, colnames))
      updatePickerInput(session, "varstojoin",
                        choices = commvar)
    })
    observe({
      req(input$dftojoinshp)
      req(input$shapetojoin2)
      commvar <- intersect(colnames(shapefile[[input$shapetojoin2]]$data), colnames(dfs[[input$dftojoinshp]]$data))
      updatePickerInput(session, "varstojoin",
                        choices = commvar)
    })

    observe({
      if(input$dforshape == "data.frames"){
        req(input$dftojoin)
        dfstojoin$vals <- lapply(input$dftojoin, function(x){
          dfs[[x]]$data
        })

        # observeEvent(input$startjoining, {
        req(dfstojoin$vals)
        req(input$varstojoin)
        join_expr <- dplyr::join_by(!!!rlang::syms(input$varstojoin))
        if(input$type == "left"){
          result$res <- purrr::reduce(dfstojoin$vals, dplyr::left_join,  by = join_expr)
        } else if(input$type == "right"){
          result$res <- purrr::reduce(dfstojoin$vals, dplyr::right_join,  by = join_expr)
        } else{
          result$res <- purrr::reduce(dfstojoin$vals, dplyr::full_join,  by = join_expr)
        }

      } else if(input$dforshape == "shapefiles"){
        shpstojoin <- reactiveValues()
        observe({
          req(input$shapetojoin)
          shpstojoin$vals <- lapply(input$shapetojoin, function(x){
            shapefile[[x]]$data
          })
        })
        observe({
          req(input$shapetojoin)
          result$res <- result$res <- purrr::reduce(shpstojoin$vals, sf::st_join)
        })
      } else{
        observe({
          req(input$dftojoinshp)
          req(input$shapetojoin2)
          req(input$varstojoin)
          join_expr <- dplyr::join_by(!!!rlang::syms(input$varstojoin))
          if(input$type == "left"){
            result$res <- dplyr::left_join(shapefile[[input$shapetojoin2]]$data |> convert_numeric_cols(),
                                           dfs[[input$dftojoinshp]]$data |> convert_numeric_cols(),
                                           by = join_expr)
          } else if(input$type == "right"){
            result$res <- dplyr::right_join(shapefile[[input$shapetojoin2]]$data |> convert_numeric_cols(),
                                            dfs[[input$dftojoinshp]]$data |> convert_numeric_cols(),
                                            by = join_expr)
          } else{
            result$res <- dplyr::full_join(shapefile[[input$shapetojoin2]]$data |> convert_numeric_cols(),
                                           dfs[[input$dftojoinshp]]$data |> convert_numeric_cols(),
                                           by = join_expr)
          }
        })
      }



      output$joined <- reactable::renderReactable({
        req(result$res)
        reactable::reactable(
          result$res |> roundcols(),
          filterable = TRUE,
          searchable = TRUE,
          striped = TRUE,
          pagination = TRUE,
          defaultPageSize = 13
        )
      })

      observeEvent(input$donejoining, {
        if(input$dforshape %in% c("data.frames", "data.frames with a shapefile")){
          dfs[[input$newset]] <- create_reactval(input$newset, result$res)
        } else{
          shapefile[[input$newset]] <- create_reactval(input$newset, result$res)
        }

        sendSweetAlert(
          session = session,
          title = "Data merged!",
          text = "The data has been successfully merged and is now available for further processing.",
          type = "success"
        )
      })

    })



  })
}

## To be copied in the UI
# mod_dfjoin_ui("dfjoin_1")

## To be copied in the server
# mod_dfjoin_server("dfjoin_1")
