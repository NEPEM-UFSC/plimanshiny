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

    # These observers are fine, as they only update UI elements
    observe({
      df_choices <- Filter(Negate(is.null), reactiveValuesToList(dfs))
      updatePickerInput(session, "dftojoin",
                        choices = names(df_choices))
    })
    observe({
      df_choices <- Filter(Negate(is.null), reactiveValuesToList(dfs))
      updatePickerInput(session, "dftojoinshp",
                        choices = names(df_choices),
                        selected = NA)
    })
    observe({
      shp_choices <- Filter(Negate(is.null), reactiveValuesToList(shapefile))
      updatePickerInput(session, "shapetojoin",
                        choices = setdiff(names(shp_choices), c("shapefile", "shapefileplot")),
                        selected = NA)
    })
    observe({
      shp_choices <- Filter(Negate(is.null), reactiveValuesToList(shapefile))
      updatePickerInput(session, "shapetojoin2",
                        choices = setdiff(names(shp_choices), c("shapefile", "shapefileplot")),
                        selected = NA)
    })


    # This observer is for updating the join variable choices and is also fine
    observe({
      req(input$dforshape == "data.frames", input$dftojoin)
      df_list <- lapply(input$dftojoin, function(x) dfs[[x]]$data)
      req(length(df_list) > 1)
      commvar <- Reduce(base::intersect, lapply(df_list, colnames))
      updatePickerInput(session, "varstojoin",
                        choices = commvar)
    })
    observe({
      req(input$dforshape == "data.frames with a shapefile", input$dftojoinshp, input$shapetojoin2)
      commvar <- intersect(colnames(shapefile[[input$shapetojoin2]]$data), colnames(dfs[[input$dftojoinshp]]$data))
      updatePickerInput(session, "varstojoin",
                        choices = commvar)
    })


    # STEP 1: Create a reactive expression for the join preview
    # This will automatically re-calculate when inputs change, but only for the preview table.
    joined_data <- reactive({
      # Use a switch for cleaner logic
      switch(input$dforshape,
             "data.frames" = {
               req(input$dftojoin, length(input$dftojoin) > 1, input$varstojoin)
               df_list <- lapply(input$dftojoin, function(x) dfs[[x]]$data)
               join_expr <- dplyr::join_by(!!!rlang::syms(input$varstojoin))

               join_func <- switch(input$type,
                                   "left" = dplyr::left_join,
                                   "right" = dplyr::right_join,
                                   "full" = dplyr::full_join)

               purrr::reduce(df_list, join_func, by = join_expr)
             },
             "shapefiles" = {
               req(input$shapetojoin, length(input$shapetojoin) > 1)
               shp_list <- lapply(input$shapetojoin, function(x) shapefile[[x]]$data)

               purrr::reduce(shp_list, sf::st_join)
             },
             "data.frames with a shapefile" = {
               req(input$dftojoinshp, input$shapetojoin2, input$varstojoin)
               join_expr <- dplyr::join_by(!!!rlang::syms(input$varstojoin))

               join_func <- switch(input$type,
                                   "left" = dplyr::left_join,
                                   "right" = dplyr::right_join,
                                   "full" = dplyr::full_join)

               join_func(shapefile[[input$shapetojoin2]]$data |> convert_numeric_cols(),
                         dfs[[input$dftojoinshp]]$data |> convert_numeric_cols(),
                         by = join_expr)
             })
    })

    # STEP 2: The table output simply renders the reactive data
    output$joined <- reactable::renderReactable({
      req(joined_data())
      reactable::reactable(
        joined_data() |> roundcols(),
        filterable = TRUE,
        searchable = TRUE,
        striped = TRUE,
        pagination = TRUE,
        defaultPageSize = 13
      )
    })

    # STEP 3: The button action is now completely isolated.
    # It only runs when clicked.
    observeEvent(input$donejoining, {
      # Use the result from the reactive expression
      req(joined_data(), input$newset)

      if(input$dforshape %in% c("data.frames", "data.frames with a shapefile")){
        dfs[[input$newset]] <- create_reactval(input$newset, joined_data())
      } else{
        shapefile[[input$newset]] <- create_reactval(input$newset, joined_data())
      }

      sendSweetAlert(
        session = session,
        title = "Data merged!",
        text = "The new dataset is now available.",
        type = "success"
      )
    })
  })
}

## To be copied in the UI
# mod_dfjoin_ui("dfjoin_1")

## To be copied in the server
# mod_dfjoin_server("dfjoin_1")
