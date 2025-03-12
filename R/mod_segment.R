#' segment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_segment_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Segmentation Settings",
          collapsible = FALSE,
          width = 12,
          h3("Input"),
          selectInput(ns("mosaic_to_segment"),
                      label = "Mosaic to be segmented",
                      choices = NULL),
          switchInput(
            inputId = ns("usemaskorind"),
            label = "Segmentation",
            labelWidth = "80px",
            onLabel = "Index",
            offLabel = "Pick",
            value = TRUE
          ),
          conditionalPanel(
            condition = "input.usemaskorind == true", ns = ns,
            divclass("anal4",
                     pickerInput(ns("segmentindex"),
                                 label = "Index for segmentation",
                                 choices = NULL,
                                 options = list(
                                   `actions-box` = TRUE,
                                   `live-search` = TRUE
                                 )),
                     textInput(ns("myindex"),
                               label = "My index",
                               value = "")
            ),
            divclass("anal6",
                     selectInput(ns("threshold"),
                                 label = "Threshold method",
                                 choices = c("Otsu", "numeric")),
                     conditionalPanel(
                       condition = "input.threshold == 'numeric'", ns = ns,
                       numericInput(ns("threshvalue"),
                                    label = "Threshold",
                                    value = NA)

                     )
            ),
            prettyCheckbox(
              inputId = ns("invertmask"),
              label = "Invert the segmentation?",
              value = FALSE,
              icon = icon("check"),
              status = "success",
              animation = "rotate"
            )
          ),
          awesomeRadio(
            inputId = ns("maskorseg"),
            label = "Return",
            choices = c("Mask", "Segmented mosaic"),
            selected = "Mask",
            inline = FALSE,
            status = "success"
          ),
          hl(),
          h3("Output"),
          textInput(
            ns("new_segment"),
            label = "New object",
            value = NULL
          ),

          fluidRow(
            col_6(
              actionBttn(ns("startsegment"),
                         label = "Start!",
                         style = "pill",
                         color = "success")
            ),
            col_6(
              actionBttn(ns("segmentmosaic"),
                         label = "segment!",
                         style = "pill",
                         no_outline = FALSE,
                         icon = icon("scissors"),
                         color = "success")
            )
          )
        )
      ),
      col_9(
        uiOutput(ns("uiseg"))
      )
    )
  )
}

#' segment Server Functions
#'
#' @noRd
mod_segment_server <- function(id, mosaic_data, r, g, b, re, nir, swir, tir, settings, basemap){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$uiseg <- renderUI({
      if (input$usemaskorind) {
        bs4TabCard(
          id = ns("tabs"),
          status = "success",
          width = 12,
          height = "720px",
          type = "tabs",
          tabPanel(
            title = "Original mosaic",
            plotOutput(ns("orimosaic"), height = "640px") |> add_spinner()
          ),
          tabPanel(
            title = "Index",
            plotOutput(ns("mosaicindex"), height = "640px") |> add_spinner()
          ),
          tabPanel(
            title = "Mask",
            plotOutput(ns("mosaicsegmentedind"), height = "640px") |> add_spinner()
          )
        )
      } else {
        tagList(
          uiOutput(ns("uipick")),
          editModUI(ns("samplepoints"), height = "640px") |> add_spinner()
        )
      }
    })

    observe({
      req(mosaic_data)
      updateSelectInput(session, "mosaic_to_segment", choices = c("Active mosaic", setdiff(names(mosaic_data), "mosaic")), selected = NULL)
      updatePickerInput(session, "segmentindex", choices = sort(pliman_indexes()))
    })
    observe({
      updateTextInput(session, "new_segment", value = paste0(input$mosaic_to_segment, "_segmented"))
    })


    # Observe event for mosaic crop action
    observeEvent(input$startsegment, {
      # Reactive expression to store the cropped mosaic
      segmented_mosaic <- reactiveVal(NULL)
      if(input$mosaic_to_segment == "Active mosaic" && !is.null(basemap$map)){
        mtemp <- mosaic_data$mosaic$data
        basemap <- basemap$map
      } else{
        mtemp <- mosaic_data[[input$mosaic_to_segment]]$data
        basemap <-
          mosaic_view(
            mtemp,
            r = suppressWarnings(as.numeric(r$r)),
            g = suppressWarnings(as.numeric(g$g)),
            b = suppressWarnings(as.numeric(b$b))
          )
      }
      req(mtemp)
      if(input$usemaskorind){
        output$orimosaic <- renderPlot({
          if(terra::nlyr(mtemp) > 2){
            terra::plotRGB(
              mtemp,
              r = suppressWarnings(as.numeric(r$r)),
              g = suppressWarnings(as.numeric(g$g)),
              b = suppressWarnings(as.numeric(b$b)),
              stretch = "hist"
            )
          } else{
            terra::plot(mtemp)
          }
        })
        segmented_mosaic <- reactiveVal(NULL)
        finalmask <- reactiveVal(NULL)
        finalind <- c(strsplit(input$myindex, split = ",")[[1]], input$segmentindex)[[1]]

        segmented_mosaic(
          mosaic_index(
            mtemp,
            index = finalind,
            r = suppressWarnings(as.numeric(r$r)),
            g = suppressWarnings(as.numeric(g$g)),
            b = suppressWarnings(as.numeric(b$b)),
            re = suppressWarnings(as.numeric(re$re)),
            nir = suppressWarnings(as.numeric(nir$nir)),
            tir = suppressWarnings(as.numeric(tir$tir)),
            swir = suppressWarnings(as.numeric(swir$swir)),
            plot = FALSE
          )
        )

        output$mosaicindex <- renderPlot({
          req(segmented_mosaic())
          mosaic_plot(segmented_mosaic())
        })

       observe({
         if(input$threshold == "Otsu"){
           thresh <- otsu(na.omit(terra::values(segmented_mosaic())))
         } else{
           req(input$threshvalue)
           thresh <- input$threshvalue
         }
         if(input$invertmask){
           mask <- segmented_mosaic() > thresh
         } else{
           mask <- segmented_mosaic() < thresh
         }
         if(input$maskorseg != "Mask"){
           seg <- terra::mask(mtemp, mask, maskvalue = TRUE, inverse = TRUE)
         } else{
           seg <- mask
         }
         req(seg)
         output$mosaicsegmentedind <- renderPlot({
           if(terra::nlyr(seg) > 2){
             mosaic_plot_rgb(
               seg,
               r = suppressWarnings(as.numeric(r$r)),
               g = suppressWarnings(as.numeric(g$g)),
               b = suppressWarnings(as.numeric(b$b))
             )
           } else{
             terra::plot(seg)
           }
         })
         finalmask(seg)
       })

      } else{
        req(basemap)
        backpoints <- reactiveValues(back = NULL)
        forepoints <- reactiveValues(fore = NULL)

        doneback <- reactiveValues(done = FALSE)
        observe({
          if(!doneback$done){
            output$uipick <- renderUI({
              actionBttn(ns("donaback"),
                         label = "Done background")

            })
            sendSweetAlert(
              session = session,
              title = "Sampling",
              text = "Use the 'Draw Rectangle' or 'Draw Polygon' tools to sample background regions",
              type = "info"
            )
            back <- callModule(editMod, "samplepoints", basemap@map, editor = "leafpm")
            observeEvent(input$donaback, {
              if (!is.null(back()$finished)) {
                backpoints$back <- back()$finished |> sf::st_transform(sf::st_crs(mtemp))
              }
              doneback$done <- TRUE
            })
          } else{
            output$uipick <- renderUI({
              actionBttn(ns("donefore"),
                         label = "Done foreground")

            })
            sendSweetAlert(
              session = session,
              title = "Sampling",
              text = "Use the 'Draw Rectangle' or 'Draw Polygon' tools to sample foreground regions",
              type = "info"
            )
            fore <- callModule(editMod, "samplepoints", basemap@map, editor = "leafpm")
            observeEvent(input$donefore, {
              if (!is.null(fore()$finished)) {
                fore <- fore()$finished |> sf::st_transform(sf::st_crs(mtemp))
                fore <-  fore |>  sf::st_difference(backpoints$back) |> sf_to_polygon()
                forepoints$fore <- fore
              }

              req(backpoints$back)
              req(forepoints$fore)
              back_sample <-
                exactextractr::exact_extract(mtemp, backpoints$back, progress = FALSE) |>
                dplyr::bind_rows() |>
                dplyr::select(-coverage_fraction) |>
                dplyr::mutate(class = 0)

              fore_sample <-
                exactextractr::exact_extract(mtemp, forepoints$fore, progress = FALSE) |>
                dplyr::bind_rows() |>
                dplyr::select(-coverage_fraction) |>
                dplyr::mutate(class = 1)
              df_train <- dplyr::bind_rows(fore_sample, back_sample)
              if(ncol(df_train) == 2){
                names(df_train)[[1]] <- names(mtemp)
              }
              mod <- suppressWarnings(
                glm(class ~.,
                    data = df_train,
                    family = binomial("logit"))
              )
              mask <- terra::predict(mtemp, mod, type = "response")
              mask[mask < 0.5] <- 0
              mask[mask > 0.5] <- 1
              if(input$maskorseg == 'Mask'){
                finalmask(mask)
              } else{
                finalmask(terra::mask(mtemp, mask, maskvalue = TRUE, inverse = TRUE))
              }

              sendSweetAlert(
                session = session,
                title = "Sampling done",
                text = "The sampling procedure has been successfully finished. Click 'Segment' in the left panel to segment the mosaic",
                type = "info"
              )
            })
          }

        })

      }

      # Observe event for mosaic crop action
      observeEvent(input$segmentmosaic, {
        # Update mosaic_data$mosaic$data when input$cropmosaic is clicked
        mosaic_data[[input$new_segment]] <- create_reactval(name = input$new_segment, data = finalmask())
        sendSweetAlert(
          session = session,
          title = "Mosaic successfully segmented!!",
          text = "The mosaic has been successfully segmented and is now available for further analysis.",
          type = "success"
        )
      })
    })
  })
}

## To be copied in the UI
# mod_segment_ui("segment_1")

## To be copied in the server
# mod_segment_server("segment_1")
