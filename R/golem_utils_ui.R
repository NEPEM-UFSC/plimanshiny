#' Turn an R list into an HTML list
#'
#' @param list An R list
#' @param class a class for the list
#'
#' @return an HTML list
#' @noRd
#'
#' @examples
#' list_to_li(c("a", "b"))
#' @importFrom shiny tags tagAppendAttributes tagList
list_to_li <- function(list, class = NULL) {
  if (is.null(class)) {
    tagList(
      lapply(
        list,
        tags$li
      )
    )
  } else {
    res <- lapply(
      list,
      tags$li
    )
    res <- lapply(
      res,
      function(x) {
        tagAppendAttributes(
          x,
          class = class
        )
      }
    )
    tagList(res)
  }
}
#' Turn an R list into corresponding HTML paragraph tags
#'
#' @param list an R list
#' @param class a class for the paragraph tags
#'
#' @return An HTML tag
#' @noRd
#'
#' @examples
#' list_to_p(c("This is the first paragraph", "this is the second paragraph"))
#' @importFrom shiny tags tagAppendAttributes tagList
#'
list_to_p <- function(list, class = NULL) {
  if (is.null(class)) {
    tagList(
      lapply(
        list,
        tags$p
      )
    )
  } else {
    res <- lapply(
      list,
      tags$p
    )
    res <- lapply(
      res,
      function(x) {
        tagAppendAttributes(
          x,
          class = class
        )
      }
    )
    tagList(res)
  }
}

#' @importFrom shiny tags tagAppendAttributes tagList
named_to_li <- function(list, class = NULL) {
  if (is.null(class)) {
    res <- mapply(
      function(x, y) {
        tags$li(
          HTML(
            sprintf("<b>%s:</b> %s", y, x)
          )
        )
      },
      list,
      names(list),
      SIMPLIFY = FALSE
    )
    tagList(res)
  } else {
    res <- mapply(
      function(x, y) {
        tags$li(
          HTML(
            sprintf("<b>%s:</b> %s", y, x)
          )
        )
      },
      list,
      names(list),
      SIMPLIFY = FALSE
    )
    res <- lapply(
      res,
      function(x) {
        tagAppendAttributes(
          x,
          class = class
        )
      }
    )
    tagList(res)
  }
}

#' Remove a tag attribute
#'
#' @param tag the tag
#' @param ... the attributes to remove
#'
#' @return a new tag
#' @noRd
#'
#' @examples
#' a <- shiny::tags$p(src = "plop", "pouet")
#' tagRemoveAttributes(a, "src")
tagRemoveAttributes <- function(tag, ...) {
  attrs <- as.character(list(...))
  for (i in seq_along(attrs)) {
    tag$attribs[[attrs[i]]] <- NULL
  }
  tag
}

#' Hide or display a tag
#'
#' @param tag the tag
#'
#' @return a tag
#' @noRd
#'
#' @examples
#' ## Hide
#' a <- shiny::tags$p(src = "plop", "pouet")
#' undisplay(a)
#' b <- shiny::actionButton("go_filter", "go")
#' undisplay(b)
#' @importFrom shiny tagList
undisplay <- function(tag) {
  # if not already hidden
  if (
    !is.null(tag$attribs$style) &&
    !grepl("display:\\s+none", tag$attribs$style)
  ) {
    tag$attribs$style <- paste(
      "display: none;",
      tag$attribs$style
    )
  } else {
    tag$attribs$style <- "display: none;"
  }
  tag
}

#' @importFrom shiny tagList
display <- function(tag) {
  if (
    !is.null(tag$attribs$style) &&
    grepl("display:\\s+none", tag$attribs$style)
  ) {
    tag$attribs$style <- gsub(
      "(\\s)*display:(\\s)*none(\\s)*(;)*(\\s)*",
      "",
      tag$attribs$style
    )
  }
  tag
}

#' Hide an elements by calling jquery hide on it
#'
#' @param id the id of the element to hide
#'
#' @noRd
#'
#' @importFrom shiny tags
jq_hide <- function(id) {
  tags$script(sprintf("$('#%s').hide()", id))
}

#' Add a red star at the end of the text
#'
#' Adds a red star at the end of the text
#' (for example for indicating mandatory fields).
#'
#' @param text the HTLM text to put before the red star
#'
#' @return an html element
#' @noRd
#'
#' @examples
#' with_red_star("Enter your name here")
#' @importFrom shiny tags HTML
with_red_star <- function(text) {
  shiny::tags$span(
    HTML(
      paste0(
        text,
        shiny::tags$span(
          style = "color:red",
          "*"
        )
      )
    )
  )
}



#' Repeat tags$br
#'
#' @param times the number of br to return
#'
#' @return the number of br specified in times
#' @noRd
#'
#' @examples
#' rep_br(5)
#' @importFrom shiny HTML
rep_br <- function(times = 1) {
  HTML(rep("<br/>", times = times))
}

#' Create an url
#'
#' @param url the URL
#' @param text the text to display
#'
#' @return an a tag
#' @noRd
#'
#' @examples
#' enurl("https://www.thinkr.fr", "ThinkR")
#' @importFrom shiny tags
enurl <- function(url, text) {
  tags$a(href = url, text)
}

#' Columns wrappers
#'
#' These are convenient wrappers around
#' `column(12, ...)`, `column(6, ...)`, `column(4, ...)`...
#'
#' @noRd
#'
#' @importFrom shiny column
col_12 <- function(...) {
  column(12, ...)
}

#' @importFrom shiny column
col_10 <- function(...) {
  column(10, ...)
}

#' @importFrom shiny column
col_8 <- function(...) {
  column(8, ...)
}

#' @importFrom shiny column
col_9 <- function(...) {
  column(9, ...)
}

#' @importFrom shiny column
col_6 <- function(...) {
  column(6, ...)
}

#' @importFrom shiny column
col_7 <- function(...) {
  column(7, ...)
}


#' @importFrom shiny column
col_5 <- function(...) {
  column(5, ...)
}

#' @importFrom shiny column
col_4 <- function(...) {
  column(4, ...)
}


#' @importFrom shiny column
col_3 <- function(...) {
  column(3, ...)
}


#' @importFrom shiny column
col_2 <- function(...) {
  column(2, ...)
}


#' @importFrom shiny column
col_1 <- function(...) {
  column(1, ...)
}

#' @importFrom shiny div
divclass <- function(class, ...) {
  div(class = class, ...)
}


pickerpalette <- function(id, inputid, n = 50, ...){
  ns <- NS(id)
  palettePicker(
    inputId = ns(inputid),
    label = "Color palette",
    choices = list(
      "Viridis" = list(
        "viridis" = scales::viridis_pal(option = "viridis")(n),
        "magma" = scales::viridis_pal(option = "magma")(n),
        "inferno" = scales::viridis_pal(option = "inferno")(n),
        "plasma" = scales::viridis_pal(option = "plasma")(n),
        "cividis" = scales::viridis_pal(option = "cividis")(n),
        "set1" = grDevices::colorRampPalette(c("yellow", "#53CC67", "#009B95", "#00588B","#4B0055"))(n),
        "set2" = grDevices::colorRampPalette(c("darkred", "yellow", "darkgreen"))(n),
        "set3" = rev(grDevices::terrain.colors(n))
      ),
      "Brewer" = list(
        "Blues" = colorRampPalette(scales::brewer_pal(palette = "Blues")(8))(n),
        "Reds" = colorRampPalette(scales::brewer_pal(palette = "Reds")(8))(n),
        "Greens " = colorRampPalette(scales::brewer_pal(palette = "Greens")(8))(n),
        "Oranges" = colorRampPalette(scales::brewer_pal(palette = "Oranges")(8))(n),
        "Paired" = colorRampPalette(scales::brewer_pal(palette = "Paired")(8))(n)
      ),
      "Others" = list(
        "WaterSoil" = custom_palette(c("#00008B", "#D2B48C", "#8B4513"), n = n),
        "PlantSoil" = custom_palette(c("forestgreen", "#B2DF8A", "#8B4513"), n = n),
        "BrBG" = colorRampPalette(scales::brewer_pal(palette = "BrBG")(8))(n),
        "PiYG" = colorRampPalette(scales::brewer_pal(palette = "PiYG")(8))(n),
        "PRGn" = colorRampPalette(scales::brewer_pal(palette = "PRGn")(8))(n),
        "PuOr" = colorRampPalette(scales::brewer_pal(palette = "PuOr")(8))(n),
        "RdBu" = colorRampPalette(scales::brewer_pal(palette = "RdBu")(8))(n),
        "RdGy" = colorRampPalette(scales::brewer_pal(palette = "RdGy")(8))(n),
        "RdYlBu" = colorRampPalette(scales::brewer_pal(palette = "RdYlBu")(8))(n),
        "RdYlGn" =colorRampPalette(scales::brewer_pal(palette = "RdYlGn")(8))(n),
        "Spectral" = colorRampPalette(scales::brewer_pal(palette = "Spectral")(8))(n)
      ),
      "Qualitative" = list(
        "ggplot2" = c("#F8766D", "#B79F00", "#00BA38", "#00BFC4", "#619CFF", "#F564E3"),
        "Accent" = c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F"),
        "Dark2" = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"),
        "Paired" = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C"),
        "Pastel1" = c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC"),
        "Pastel2" = c("#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9", "#FFF2AE"),
        "Brewer1" = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33"),
        "Brewer2" = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F"),
        "Brewer3" = c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462")
      )
    ),
    textColor = c(rep("white", 5), "black", "white", rep("black", 6), rep("white", 10), rep("black", 10)),
    ...
  )
}


return_colors <- function(pal, reverse = FALSE, n = 8){
  pals <-
    switch(pal,
           "viridis" = scales::viridis_pal(option = "viridis")(n),
           "magma" = scales::viridis_pal(option = "magma")(n),
           "inferno" = scales::viridis_pal(option = "inferno")(n),
           "plasma" = scales::viridis_pal(option = "plasma")(n),
           "cividis" = scales::viridis_pal(option = "cividis")(n),
           "set1" = grDevices::colorRampPalette(c("yellow", "#53CC67", "#009B95", "#00588B","#4B0055"))(n),
           "set2" = grDevices::colorRampPalette(c("darkred", "yellow", "darkgreen"))(n),
           "set3" = rev(grDevices::terrain.colors(n)),
           "Blues" = colorRampPalette(scales::brewer_pal(palette = "Blues")(8))(n),
           "Reds" = colorRampPalette(scales::brewer_pal(palette = "Reds")(8))(n),
           "Greens " = colorRampPalette(scales::brewer_pal(palette = "Greens")(8))(n),
           "Oranges" = colorRampPalette(scales::brewer_pal(palette = "Oranges")(8))(n),
           "Paired" = colorRampPalette(scales::brewer_pal(palette = "Paired")(8))(n),
           "WaterSoil" = custom_palette(c("#00008B", "#D2B48C", "#8B4513"), n = n),
           "PlantSoil" = custom_palette(c("forestgreen", "#B2DF8A", "#8B4513"), n = n),
           "BrBG" = colorRampPalette(scales::brewer_pal(palette = "BrBG")(8))(n),
           "PiYG" = colorRampPalette(scales::brewer_pal(palette = "PiYG")(8))(n),
           "PRGn" = colorRampPalette(scales::brewer_pal(palette = "PRGn")(8))(n),
           "PuOr" = colorRampPalette(scales::brewer_pal(palette = "PuOr")(8))(n),
           "RdBu" = colorRampPalette(scales::brewer_pal(palette = "RdBu")(8))(n),
           "RdGy" = colorRampPalette(scales::brewer_pal(palette = "RdGy")(8))(n),
           "RdYlBu" = colorRampPalette(scales::brewer_pal(palette = "RdYlBu")(8))(n),
           "RdYlGn" =colorRampPalette(scales::brewer_pal(palette = "RdYlGn")(8))(n),
           "Spectral" = colorRampPalette(scales::brewer_pal(palette = "Spectral")(8))(n),
           "ggplot2" = sample(c("#F8766D", "#B79F00", "#00BA38", "#00BFC4", "#619CFF", "#F564E3"), n, replace = ifelse(n > 6, TRUE, FALSE)),
           "Accent" = sample(c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F"), n, replace = ifelse(n > 6, TRUE, FALSE)),
           "Dark2" = sample(c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"), n, replace = ifelse(n > 6, TRUE, FALSE)),
           "Paired" = sample(c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C"), n, replace = ifelse(n > 6, TRUE, FALSE)),
           "Pastel1" = sample(c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC"), n, replace = ifelse(n > 6, TRUE, FALSE)),
           "Pastel2" = sample(c("#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9", "#FFF2AE"), n, replace = ifelse(n > 6, TRUE, FALSE)),
           "Brewer1" = sample(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33"), n, replace = ifelse(n > 6, TRUE, FALSE)),
           "Brewer2" = sample(c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F"), n, replace = ifelse(n > 6, TRUE, FALSE)),
           "Brewer3" = sample(c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462"),  n, replace = ifelse(n > 6, TRUE, FALSE))
    )
  if(reverse){
    return(rev(pals))
  } else{
    return(pals)
  }
}
read_pars <- function(){
  # Access the settings reactively and determine if the spinner should be used
  user_settings_dir <- tools::R_user_dir("plimanshiny", which = "config")
  settings_file_user <- file.path(user_settings_dir, "user_module_settings.rds")
  if (file.exists(settings_file_user)) {
    pars <- readRDS(settings_file_user)
  } else {
    inst_dir <- file.path(system.file(package = "plimanshiny"), "app/www")
    settings_file_default <- file.path(inst_dir, "default_module_settings.rds")
    pars <- readRDS(settings_file_default)
  }
  return(pars)
}

add_spinner <- function(x, type = 4, color = "#28a745") {
  pars <- read_pars()
  if(pars$cssloaders){
    shinycssloaders::withSpinner(x, type = type, color = color)
  } else{
    x
  }
}

add_help <- function(...,
                     step = 1,
                     intro = "Intro here",
                     hint = "click"){
  pars <- read_pars()
  if(pars$introjs){
    rintrojs::introBox(...,
                       data.step = step,
                       data.intro = intro,
                       data.hint = hint)
  } else{
    return(list(...))
  }
}
#' Make the current tag behave like an action button
#'
#' Only works with compatible tags like button or links
#'
#' @param tag Any compatible tag.
#' @param inputId Unique id. This will host the input value to be used
#' on the server side.
#'
#' @return The modified tag with an extra id and the action button class.
#' @noRd
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   link <- a(href = "#", "My super link", style = "color: lightblue;")
#'
#'   ui <- fluidPage(
#'     make_action_button(link, inputId = "mylink")
#'   )
#'
#'   server <- function(input, output, session) {
#'     observeEvent(input$mylink, {
#'       showNotification("Pouic!")
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
make_action_button <- function(tag, inputId = NULL) {
  # some obvious checks
  if (!inherits(tag, "shiny.tag")) stop("Must provide a shiny tag.")
  if (!is.null(tag$attribs$class)) {
    if (grep("action-button", tag$attribs$class)) {
      stop("tag is already an action button")
    }
  }
  if (is.null(inputId) && is.null(tag$attribs$id)) {
    stop("tag does not have any id. Please use inputId to be able to
           access it on the server side.")
  }

  # handle id
  if (!is.null(inputId)) {
    if (!is.null(tag$attribs$id)) {
      warning(
        paste(
          "tag already has an id. Please use input$",
          tag$attribs$id,
          "to access it from the server side. inputId will be ignored."
        )
      )
    } else {
      tag$attribs$id <- inputId
    }
  }

  # handle class
  if (is.null(tag$attribs$class)) {
    tag$attribs$class <- "action-button"
  } else {
    tag$attribs$class <- paste(tag$attribs$class, "action-button")
  }
  # return tag
  tag
}





# Define the drag_ui function with customizable colors for numeric and character variables
drag_ui <- function(id,
                    label = "Drag-and-Drop Area",
                    numeric_color = "#b2f2b2",
                    character_color = "#b2d9f2",
                    animation_time = 0.3,
                    animation_time_drag = 1.5,
                    explosion_time = 0.5,
                    scale_factor = 1.3,  # Scale factor for bounce effect
                    dropzone_bg = "#f9f9f9",
                    dropzone_shadow = "0px 4px 6px rgba(0, 0, 0, 0.15)",
                    draggable_bg = "#e1e1e1",
                    draggable_shadow = "0px 4px 6px rgba(0, 0, 0, 0.15)",
                    dropzone_width = "600px",
                    dropzone_height = "100px") {

  ns <- NS(id)

  tagList(
    tags$style(HTML(sprintf("
      .drag-container {
        display: flex;
        flex-direction: column;
        justify-content: flex-start;
        margin-bottom: 10px;
        align-items: flex-start;
      }
      .dropzone {
        padding: 10px;
        text-align: center;
        background-color: %1$s !important;
        border-radius: 15px;
        width: %12$s;
        height: %13$s;
        margin-bottom: 10px;
        box-shadow: %2$s;
      }
      .draggable {
        cursor: move;
        padding: 5px;
        margin: 1px;
        background-color: %3$s;
        border: 1px solid #aaa;
        display: inline-block;
        border-radius: 15px;
        box-shadow: %4$s;
      }
      /* Add transition to dropped elements */
      .draggable.dropped {
        transition: transform %5$ss ease, opacity %5$ss ease;
        transform: scale(%6$s);
        opacity: 1;
      }
      .numeric-var {
        background-color: %7$s;
        box-shadow: %8$s;
      }
      .character-var {
        background-color: %9$s;
        box-shadow: %10$s;
      }
      .remove-btn {
        cursor: pointer;
        color: red;
        margin-left: 10px;
        font-weight: bold;
      }
      /* Flex container for draggable items */
      .flex-draggable-container {
        display: flex;
        flex-direction: row;
        flex-wrap: wrap;
        gap: 5px;
        margin-bottom: 5px;
        align-items: center;
      }
      /* Bounce Effect with Scale for dragging */
      @keyframes bounce-scale {
        0%% {
          transform: scale(%6$s) translateY(0);
        }
        25%% {
          transform: scale(%6$s) translateY(-5px);
        }
        50%% {
          transform: scale(%6$s) translateY(0);
        }
        75%% {
          transform: scale(%6$s) translateY(-5px);
        }
        100%% {
          transform: scale(%6$s) translateY(0);
        }
      }
      .dragging {
        animation: bounce-scale %14$ss infinite;
      }
      /* Zoom Effect for removal */
      @keyframes zoom-out {
        0%% {
          transform: scale(1);
          opacity: 1;
        }
        25%% {
          transform: scale(1.5);
          opacity: 0.75;
        }
        37%% {
          transform: scale(1.75);
          opacity: 0.60;
        }
        50%% {
          transform: scale(2);
          opacity: 0.5;
        }
        75%% {
          transform: scale(1.5);
          opacity: 0.4;
        }
        85%% {
          transform: scale(1);
          opacity: 0.2;
        }
        95%% {
          transform: scale(0.5);
          opacity: 0.25;
        }
        100%% {
          transform: scale(0);
          opacity: 0;
        }
      }
      .exploding {
        animation: zoom-out %11$ss forwards;
      }
    ", dropzone_bg, dropzone_shadow, draggable_bg, draggable_shadow, animation_time,
                            scale_factor, numeric_color, draggable_shadow,
                            character_color, draggable_shadow, explosion_time,
                            dropzone_width, dropzone_height, animation_time_drag))),

    fluidRow(
      div(class = "flex-draggable-container", # Flex container to arrange items side by side
          uiOutput(ns("draggables_ui"))  # Render draggables dynamically
      )
    ),

    fluidRow(
      div(class = "drag-container",
          div(id = ns("dropzone"), class = "dropzone", label, style = "text-align: left;")
      )
    ),

    # JavaScript to handle drag-and-drop with bounce-scale and zoom-out effects
    tags$script(HTML(sprintf("
      var dragged;
      document.addEventListener('dragstart', function(event) {
        dragged = event.target;
        dragged.classList.add('dragging');  // Add bounce-scale effect when dragging
      });

      document.addEventListener('dragend', function(event) {
        dragged.classList.remove('dragging');  // Remove bounce-scale effect when dragging stops
      });

      // Prevent default behavior (Prevent file from being opened)
      document.addEventListener('dragover', function(event) {
        event.preventDefault();
      });

      // Handle the drop event
      document.addEventListener('drop', function(event) {
        event.preventDefault();
        var zoneId = event.target.id;

        if (zoneId === '%s' && dragged.classList.contains('draggable')) {
          var newItem = document.createElement('div');
          newItem.className = 'draggable';
          newItem.innerHTML = dragged.innerHTML + '<span class=\"remove-btn\">&#10005;</span>';
          newItem.setAttribute('draggable', 'false');  // Disable dragging after adding
          event.target.appendChild(newItem);

          // Notify Shiny server about dropped item
          Shiny.setInputValue('%s', dragged.innerText.trim(), {priority: 'event'});

          // Add event listener to remove the dropped item
          newItem.querySelector('.remove-btn').addEventListener('click', function() {
            var parent = this.parentElement;
            parent.classList.add('exploding');  // Trigger zoom-out animation on removal
            setTimeout(function() {
              parent.remove();  // Remove item after zoom-out animation
              // Notify Shiny server about removed item
              Shiny.setInputValue('%s', newItem.innerText.replace('✕', '').trim(), {priority: 'event'});
            }, %d);
          });

          dragged.remove();  // Remove the original dragged item
        }
      });
    ", ns("dropzone"), ns("dropped_item"), ns("removed_item"), explosion_time * 1000)))
  )
}
canvas_with_actions_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
        #rasterCanvas {
          margin-top: 20px;
          border: 1px solid #ddd;
          box-shadow: 3px 3px 8px rgba(0, 0, 0, 0.3);
        }
      ")),
      tags$script(HTML(sprintf("
        let canvas, ctx, drawing = false;
        let rectStartX, rectStartY, rectEndX, rectEndY;
        let selectedPoints = [];
        let rasterImage = null;
        let canvasWidth = 1280;
        let canvasHeight = 720;

        function initCanvas() {
          canvas = document.getElementById('%s');
          ctx = canvas.getContext('2d');
          canvas.width = canvasWidth;
          canvas.height = canvasHeight;

          canvas.addEventListener('mousedown', handleMouseDown);
          canvas.addEventListener('mousemove', handleMouseMove);
          canvas.addEventListener('mouseup', handleMouseUp);
          canvas.addEventListener('dblclick', handleDoubleClick);
        }

        function adjustCanvasSize(width, height) {
          canvasWidth = width;
          canvasHeight = height;
          canvas.width = canvasWidth;
          canvas.height = canvasHeight;
          Shiny.setInputValue('%s', { width: canvasWidth, height: canvasHeight }, { priority: 'event' });
          drawCanvas();
        }

        function handleMouseDown(e) {
          const rect = canvas.getBoundingClientRect();
          rectStartX = e.clientX - rect.left;
          rectStartY = e.clientY - rect.top;
          timeoutID = setTimeout(() => {
            drawPoint(rectStartX, rectStartY);
            selectedPoints.push({ x: rectStartX, y: rectStartY });
            Shiny.setInputValue('%s', [rectStartX, rectStartY]);
          }, 1000);
          drawing = true;
        }

        function handleMouseMove(e) {
          if (!drawing) return;
          clearTimeout(timeoutID);
          const rect = canvas.getBoundingClientRect();
          rectEndX = e.clientX - rect.left;
          rectEndY = e.clientY - rect.top;
          drawCanvas();
          drawRectangle(rectStartX, rectStartY, rectEndX, rectEndY);
        }

        function handleMouseUp() {
          clearTimeout(timeoutID);
          drawing = false;
          Shiny.setInputValue('%s', {
            startX: Math.min(rectStartX, rectEndX),
            startY: Math.min(rectStartY, rectEndY),
            endX: rectEndX,
            endY: rectEndY,
            width: Math.abs(rectEndX - rectStartX),
            height: Math.abs(rectEndY - rectStartY)
          });
          rectStartX = rectStartY = rectEndX = rectEndY = 0;
        }

        function drawPoint(x, y) {
          if (!ctx) return;
          ctx.strokeStyle = 'red';
          ctx.lineWidth = 2;
          ctx.beginPath();
          ctx.arc(x, y, 10, 0, 2 * Math.PI);
          ctx.stroke();
          ctx.beginPath();
          ctx.moveTo(x - 10, y);
          ctx.lineTo(x + 10, y);
          ctx.moveTo(x, y - 10);
          ctx.lineTo(x, y + 10);
          ctx.stroke();
        }

        function drawRectangle(x1, y1, x2, y2) {
          ctx.strokeStyle = 'red';
          ctx.lineWidth = 2;
          ctx.strokeRect(x1, y1, x2 - x1, y2 - y1);
        }

        function drawCanvas() {
          ctx.clearRect(0, 0, canvas.width, canvas.height);
          drawRaster();
        }

        function drawRaster() {
          if (rasterImage) {
            ctx.drawImage(rasterImage, 0, 0, canvas.width, canvas.height);
          }
        }

        Shiny.addCustomMessageHandler('updateTiles', function(data) {
          rasterImage = new Image();
          rasterImage.src = 'data:image/png;base64,' + data.img;
          rasterImage.onload = drawCanvas;
        });

        Shiny.addCustomMessageHandler('adjustCanvasSize', function(data) {
          adjustCanvasSize(data.width, data.height);
        });

        function handleDoubleClick() {
          Shiny.setInputValue('%s', new Date().getTime());
        }

        window.addEventListener('load', initCanvas);
      ", ns("rasterCanvas"), ns("canvas_size"), ns("picked_point"), ns("drawn_rectangle"), ns("reset_view"))))
    ),
    tags$canvas(id = ns("rasterCanvas"))
  )
}
