#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#'
#' @noRd
#'
#' @example
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x) {
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
#' @examples
#' NA %|NA|% 1
"%|NA|%" <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}

find_aggrfact <- function(mosaic, max_pixels = 1e6){
  compute_downsample <- function(nr, nc, n) {
    if (n == 0) {
      invisible(nr * nc)
    } else if (n == 1) {
      invisible(ceiling(nr/2) * ceiling(nc/2))
    } else if (n > 1) {
      invisible(ceiling(nr/(n+1)) * ceiling(nc/(n+1)))
    } else {
      stop("Invalid downsampling factor. n must be a non-negative integer.")
    }
  }
  nr <- nrow(mosaic)
  nc <- ncol(mosaic)
  npixel <- nr * nc
  possible_downsamples <- 0:20
  possible_npix <- sapply(possible_downsamples, function(x){
    compute_downsample(nr, nc, x)
  })
  downsample <- which.min(abs(possible_npix - max_pixels))
  downsample <- ifelse(downsample == 1, 0, downsample)
  return(downsample)
}
sf_to_polygon <- function(shps) {
  if(inherits(shps, "list")){
    shps <- do.call(rbind, shps)
  }
  classes <- sapply(lapply(sf::st_geometry(shps$geometry), class), function(x){x[2]})
  shps[classes %in% c("POINT", "LINESTRING"), ] <-
    shps[classes %in% c("POINT", "LINESTRING"), ] |>
    sf::st_buffer(0.0000001) |>
    sf::st_cast("POLYGON") |>
    sf::st_simplify(preserveTopology = TRUE)
  return(shps)
}

roundcols <- function(df, ..., digits = 3){
  is_mat <- is.matrix(df)
  if (is_mat == TRUE) {
    df <- df |> as.data.frame() |> pliman::rownames_to_column()
  }
  has_rownames <- function(x) {
    Negate(is.null)(rownames(x))
  }
  if (has_rownames(df)) {
    rnames <- rownames(df)
  }
  if (missing(...)) {
    df <-
      df |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x){round(x, digits = digits)}))
  }
  else {
    df <-
      df |>
      dplyr::mutate(dplyr::across(c(...), \(x){round(x, digits = digits)}))
  }
  if (has_rownames(df)) {
    rownames(df) <- rnames
  }
  return(df)
}
import_shp <- function(shp){
  if(is.null(shp)){
    return()
  }
  previouswd <- getwd()
  uploaddirectory <- dirname(shp$datapath[1])
  setwd(uploaddirectory)
  for(i in 1:nrow(shp)){
    file.rename(shp$datapath[i], shp$name[i])
  }
  setwd(previouswd)
  filetemp <- paste(uploaddirectory, shp$name[grep(pattern="*.shp$", shp$name)], sep="/")
  shapefile_input(filetemp, info = FALSE)
}

import_shp_mod <- function(datapath, file, session){
  files <- datapath
  exts <- c(".rds",  ".shp",  ".json", ".kml",  ".gml",  ".dbf",  ".sbn",  ".sbx",  ".shx",  ".prj", ".cpg")
  if(!any(file_extension(files)  %in% sub(".", "", exts))){
    sendSweetAlert(
      session = session,
      title = "Invalid file format",
      text = paste("Invalid file format while uploading the shapefile. Ensure that the file extension are one of", paste0(exts, collapse = ", ")),
      type = "error"
    )
    return()
  } else{
    reqshp <- c("shp", "dbf", "prj", "shx")
    if(any(file_extension(files)  %in%  reqshp)){
      if (!all(reqshp %in% file_extension(files))) {
        sendSweetAlert(
          session = session,
          title = "Required files",
          text = "When importing a '.shp' file, make sure to also import the
              mandatory files companion *.dbf, *.prj, and *.shx. Select the multiple
              required files and try again.",
          type = "error"
        )
        return()
      } else{
        shp <- import_shp(file)
      }
    } else{
      shp <- shapefile_input(datapath, info = FALSE)
    }
    return(shp)
  }
}

write_shp <- function(data, file){
  temp_shp <- tempdir()
  # write shp files
  terra::writeVector(data, paste0(temp_shp, "/my_shp.shp"), overwrite = TRUE)
  # zip all the shp files
  zip_file <- file.path(temp_shp, "vector_shp.zip")
  shp_files <- list.files(temp_shp,
                          "my_shp",
                          full.names = TRUE)
  # the following zip method works for me in linux but substitute with whatever method working in your OS
  zip_command <- paste("zip -j",
                       zip_file,
                       paste(shp_files, collapse = " "))
  system(zip_command)
  # copy the zip file to the file argument
  file.copy(zip_file, file)
  # remove all the files created
  file.remove(zip_file, shp_files)
}
#' Typing reactiveValues is too long
#'
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#'
#' @noRd
rv <- function(...) shiny::reactiveValues(...)
rvtl <- function(...) shiny::reactiveValuesToList(...)

hl <- function(){
  tags$hr()
}

mosaic_info <- function(mo){
  if(terra::crs(mo) != ""){
    crsmo <- terra::crs(mo, describe = TRUE)
    crsname <- paste0(crsmo$name, " (", paste0(paste0(crsmo$authority), ":", paste0(crsmo$code)), ")")
  } else{
    crsname <- "CRS not available"
  }
  dt <- terra::datatype(mo)[[1]]
  content <- tags$span(
    tags$h1(icon("info"), "Mosaic information", style = "color: steelblue;"),
    icon("border-all"),tags$b("Number of columns: "), paste0(terra::ncol(mo)), tags$br(),
    icon("border-all"),tags$b("Number of rows: "), paste0(terra::nrow(mo)), tags$br(),
    icon("layer-group"),tags$b("Number of layers: "), paste0(terra::nlyr(mo), " (",paste0(names(mo), collapse = ", ") , ")"), tags$br(),
    icon("ruler-combined"),tags$b("Resolution: "), paste0(paste(round(terra::res(mo), 8), collapse = ", "), " (x, y)"), tags$br(),
    icon("ruler-combined"),tags$b("Extend: "), paste0(sub("^ext\\((.*)\\)$", "\\1", paste(terra::ext(mo))), " (xmin, xmax, ymin, ymax)"), tags$br(),
    icon("earth-americas"),tags$b("CRS: "), paste0(crsname), tags$br(),
    icon("database"),tags$b("Data Type: "), paste0(dt), tags$br()
  )

  show_alert(
    title = NULL,
    text = div(content, style = "text-align: left; line-height: 1.5"),
    html = TRUE,
    width = 850
  )
}


# Function to create a new reactiveValues object for a mosaic
create_reactval <- function(name, data) {
  reactiveValues(name = name, data = data)
}

chrv2numv <- function(chr){
  unlist(strsplit(as.character(chr), split = ','))
  as.numeric(gsub("[[:space:]]", "", unlist(strsplit(as.character(chr), split = ','))))
}

overlaps <- function(mosaic, shape){
  !is.null(terra::intersect(terra::ext(shape), terra::ext(mosaic)))
}


extract_number <- function(.data,
                           ...,
                           pattern = NULL){
  if(missing(pattern)){
    pattern <- "[^0-9.-]+"
  }
  if (inherits(.data, c("data.frame","tbl_df", "data.table"))){
    if(missing(...)){
      results <-
        dplyr::mutate(.data, dplyr::across(dplyr::where(~!is.numeric(.)), gsub, pattern = pattern, replacement = "")) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
    } else{
      results <-
        dplyr::mutate(.data, dplyr::across(c(...), gsub, pattern = pattern, replacement = ""))  |>
        dplyr::mutate(dplyr::across(c(...), as.numeric))
    }
    return(results)
  } else{
    return(as.numeric(gsub("[^0-9.-]+", "", .data)))
  }
}

str_split <- function(string){
  gsub("[[:space:]]", "", strsplit(string, split = ',')[[1]])
}

create_palette <- function(img, points, width = 150, height = 100, shape = "box", r = 1){
  nc <- ncol(img)
  colnames(points) <- c("x", "y")
  points[, 2] <- nc - points[, 2]
  bind <- NULL
  for (i in 1:nrow(points)) {
    xrmin <- trunc(points[, 1][i]) - r
    xrmax <- trunc(points[, 1][i]) + r
    yrmin <- trunc(points[, 2][i]) - r
    yrmax <- trunc(points[, 2][i]) + r
    sqr <- xrmax - xrmin + 1
    kern <- as.logical(pliman::make_brush(sqr, shape = shape))
    R <- img[xrmin:xrmax, yrmin:yrmax, 1][kern]
    G <- img[xrmin:xrmax, yrmin:yrmax, 2][kern]
    B <- img[xrmin:xrmax, yrmin:yrmax, 3][kern]
    bind <- rbind(bind, cbind(R, G, B))
  }
  dim_mat <- trunc(sqrt(nrow(bind)))
  bind <- bind[sample(1:nrow(bind)), ][1:dim_mat^2, ]
  pal <-
    pliman::as_image(c(bind[, 1], bind[, 2], bind[, 3]),
                     dim = c(dim_mat, dim_mat, 3),
                     colormode = "Color") |>
    image_resize(width = width, height = height)
  return(pal)
}

# gathered from https://stackoverflow.com/questions/45366243/text-labels-with-background-colour-in-r
boxtext <- function(x, y, labels = NA, col.text = NULL, col.bg = NA,
                    border.bg = NA, adj = NULL, pos = NULL, offset = 0.5,
                    padding = c(0.5, 0.5), cex = 1, font = graphics::par('font')){

  ## The Character expansion factro to be used:
  theCex <- graphics::par('cex')*cex

  ## Is y provided:
  if (missing(y)) y <- x

  ## Recycle coords if necessary:
  if (length(x) != length(y)){
    lx <- length(x)
    ly <- length(y)
    if (lx > ly){
      y <- rep(y, ceiling(lx/ly))[1:lx]
    } else {
      x <- rep(x, ceiling(ly/lx))[1:ly]
    }
  }

  ## Width and height of text
  textHeight <- graphics::strheight(labels, cex = theCex, font = font)
  textWidth <- graphics::strwidth(labels, cex = theCex, font = font)

  ## Width of one character:
  charWidth <- graphics::strwidth("e", cex = theCex, font = font)

  ## Is 'adj' of length 1 or 2?
  if (!is.null(adj)){
    if (length(adj == 1)){
      adj <- c(adj[1], 0.5)
    }
  } else {
    adj <- c(0.5, 0.5)
  }

  ## Is 'pos' specified?
  if (!is.null(pos)){
    if (pos == 1){
      adj <- c(0.5, 1)
      offsetVec <- c(0, -offset*charWidth)
    } else if (pos == 2){
      adj <- c(1, 0.5)
      offsetVec <- c(-offset*charWidth, 0)
    } else if (pos == 3){
      adj <- c(0.5, 0)
      offsetVec <- c(0, offset*charWidth)
    } else if (pos == 4){
      adj <- c(0, 0.5)
      offsetVec <- c(offset*charWidth, 0)
    } else {
      stop('Invalid argument pos')
    }
  } else {
    offsetVec <- c(0, 0)
  }

  ## Padding for boxes:
  if (length(padding) == 1){
    padding <- c(padding[1], padding[1])
  }

  ## Midpoints for text:
  xMid <- x + (-adj[1] + 1/2)*textWidth + offsetVec[1]
  yMid <- y + (-adj[2] + 1/2)*textHeight + offsetVec[2]

  ## Draw rectangles:
  rectWidth <- textWidth + 2*padding[1]*charWidth
  rectHeight <- textHeight + 2*padding[2]*charWidth
  graphics::rect(xleft = xMid - rectWidth/2,
                 ybottom = yMid - rectHeight/2,
                 xright = xMid + rectWidth/2,
                 ytop = yMid + rectHeight/2,
                 col = adjustcolor(col.bg, 0.5),
                 border = adjustcolor(border.bg, 0.5))

  ## Place the text:
  graphics::text(xMid, yMid, labels, col = col.text, cex = theCex, font = font,
                 adj = c(0.5, 0.5))

  ## Return value:
  if (length(xMid) == 1){
    invisible(c(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
                yMid + rectHeight/2))
  } else {
    invisible(cbind(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
                    yMid + rectHeight/2))
  }
}


convert_numeric_cols <- function(data) {
  # Function to check if a column can be converted to numeric
  can_convert_to_numeric <- function(x) {
    is.character(x) && all(!is.na(suppressWarnings(as.numeric(x))))
  }

  # Get the names of the columns that can be converted
  numeric_col_names <- names(data)[sapply(data, can_convert_to_numeric)]

  # Convert only those columns to numeric
  data <- data  |>
    dplyr::mutate(dplyr::across(dplyr::all_of(numeric_col_names), as.numeric))

  return(data)
}


add_suffix <- function(name, suffix){
  gsub(".character\\(0\\)", "", paste0(file_name(name), suffix, ".", file_extension(name)))
}

render_reactable <- function(df,
                             filterable = TRUE,
                             searchable = TRUE,
                             striped = TRUE,
                             pagination = TRUE,
                             defaultPageSize = 15,
                             max_width = 400,
                             theme = reactableTheme(
                               cellPadding = "8px 10px",
                               style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
                               searchInputStyle = list(width = "100%")
                             ),
                             ...){
  pars <- read_pars()
  if(pars$sparkline & nrow(df) > 1){
    dcd = colDef(
      maxWidth = max_width,
      footer = function(values) {
        if (!is.numeric(values)) return()
        sparkline::sparkline(values, type = "box", width = 100, height = 30)
      })
  } else{
    dcd = colDef(
      maxWidth = max_width
    )
  }

  reactable(
    df,
    filterable = filterable,
    searchable = searchable,
    striped = striped,
    pagination = pagination,
    defaultPageSize = defaultPageSize,
    defaultColDef = dcd,
    theme = theme,
    ...
  )
}
color_alpha <- function(color, alpha) {
  # Convert the color to RGB
  rgb_vals <- col2rgb(color) / 255

  # Check that alpha is between 0 and 1
  if (alpha < 0 || alpha > 1) {
    stop("Alpha value must be between 0 and 1")
  }

  # Create the color with the specified alpha
  alpha_color <- rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], alpha)

  return(alpha_color)
}


date_format <- function(date_vector) {
  possible_formats <- c("%Y%m%d", "%d-%m-%Y", "%m-%d-%Y", "%Y-%m-%d")

  # Helper function to check if a date matches a given format
  matches_format <- function(date_string, format) {
    if (format == "%Y%m%d") {
      return(grepl("^\\d{8}$", date_string))
    }
    if (format == "%d-%m-%Y") {
      if (grepl("^\\d{2}-\\d{2}-\\d{4}$", date_string)) {
        parts <- unlist(strsplit(date_string, "-"))
        if (as.numeric(parts[1]) <= 31 && as.numeric(parts[2]) <= 12) {
          return(TRUE)
        }
      }
    }
    if (format == "%m-%d-%Y") {
      if (grepl("^\\d{2}-\\d{2}-\\d{4}$", date_string)) {
        parts <- unlist(strsplit(date_string, "-"))
        if (as.numeric(parts[1]) <= 12 && as.numeric(parts[2]) <= 31) {
          return(TRUE)
        }
      }
    }
    if (format == "%Y-%m-%d") {
      return(grepl("^\\d{4}-\\d{2}-\\d{2}$", date_string))
    }
    return(FALSE)
  }

  # Check all dates for each format
  valid_formats <- list()
  for (format in possible_formats) {
    if (all(sapply(date_vector, matches_format, format = format))) {
      valid_formats <- c(valid_formats, format)
    }
  }

  return(valid_formats)
}

check_cols_shpinp <- function(shpimp){
  if(!"unique_id" %in% colnames(shpimp)){
    shpimp <- shpimp |> dplyr::mutate(unique_id = dplyr::row_number())
  }
  if(!"block" %in% colnames(shpimp)){
    shpimp <- shpimp |> dplyr::mutate(block = "B01")
  }
  if(!"plot_id" %in% colnames(shpimp)){
    shpimp <- shpimp |> dplyr::mutate(plot_id = paste0("P", leading_zeros(1:nrow(shpimp), 3)))
  }
  if(!"row" %in% colnames(shpimp)){
    shpimp <- shpimp |> dplyr::mutate(row = 1)
  }
  if(!"column" %in% colnames(shpimp)){
    shpimp <- shpimp |> dplyr::mutate(column = 1)
  }
  shpimp |> dplyr::relocate(geometry, .after = dplyr::last_col())
}
# Function to calculate the UTM zone based on longitude
get_utm_zone <- function(lon) {
  return(floor((lon + 180) / 6) + 1)
}

# Function to get the bounds of the UTM zone
get_utm_bounds <- function(zone) {
  lon_min <- (zone - 1) * 6 - 180
  lon_max <- zone * 6 - 180
  return(list(lon_min = lon_min, lon_max = lon_max, lat_min = -80, lat_max = 84))
}
epsg <- function(lat, lon) {
  utm_zone <- floor((lon + 180) / 6) + 1
  hemisphere <- ifelse(lat >= 0, "N", "S")
  epsg_code <- if (hemisphere == "N") {
    32600 + utm_zone
  } else {
    32700 + utm_zone
  }
  return(paste0("EPSG:", epsg_code))
}

check_and_install_dependencies <- function(pkg_list, ns, input, inputId_check) {
  missing_pkgs <- pkg_list[!sapply(pkg_list, require, character.only = TRUE, quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    # Ask for confirmation to install missing packages
    shinyWidgets::ask_confirmation(
      inputId = "myconfirmation",
      type = "warning",
      title = "Missing Packages",
      text = paste("The following packages are missing and need to be installed:",
                   paste(missing_pkgs, collapse = ", "), ". Do you want to install them?"),
      btn_labels = c("Nope", "Yep"),
      btn_colors = c("#FE642E", "#04B404")
    )
    # Observe the confirmation
    observe({
      if (!is.null(input$myconfirmation)) {
        if (input$myconfirmation) {
          showNotification("Installing packages, please wait...",
                           type = "message",
                           duration = NULL,
                           id = "installpkg")
          pak::pkg_install(missing_pkgs)
          suppressMessages(
            suppressWarnings(
              sapply(missing_pkgs, library, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
            )
          )
          removeNotification("installpkg")
          showNotification("Packages installed successfully!", type = "message")
        } else {
          showNotification("Package installation canceled.", type = "warning")
        }
      }
    })
  } else {
    if(grepl("check_", inputId_check)){
      showNotification("All required packages are already installed.", type = "message")
    }
  }
}

enable_module <- function(mod_id, mod_name, description, deps, ns) {
  # Create a fluid row with module switch, description, and dependencies
  fluidRow(
    col_3(
      prettySwitch(
        inputId = ns(mod_id),
        label = mod_name,
        status = "success",
        fill = TRUE
      )
    ),
    col_5(
      description  # Display the module description
    ),
    col_2(
      paste(paste(deps, collapse = ", "))  # Display the dependencies
    ),
    col_2(
      actionButton(ns(paste0("check", "_", mod_id)), label = tagList(icon("eye"), "Check"), class = "btn btn-primary"),
    )
  )
}
# Generic function to observe events and check/install dependencies
observe_dependency <- function(input_id, packages, ns, input) {
  observeEvent(input[[input_id]], {
    if (input[[input_id]]) {
      check_and_install_dependencies(packages, ns, input, input_id)
    }
  }, ignoreInit = TRUE)
}


# Corresponding statistical functions
custom_stats <- function(data, stats) {
  stat_fns <- list(
    mean = ~ mean(., na.rm = TRUE),
    median = ~ median(., na.rm = TRUE),
    min = ~ min(., na.rm = TRUE),
    max = ~ max(., na.rm = TRUE),
    sum = ~ sum(., na.rm = TRUE),
    sd = ~ sd(., na.rm = TRUE),
    cv = ~ sd(., na.rm = TRUE) / mean(., na.rm = TRUE) * 100,
    n = ~ length(na.omit(.))
  )

  selected_fns <- stat_fns[stats]

  return(selected_fns)
}

# Define the drag_server function
drag_server <- function(id, data = NULL, labels = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    available_items <- reactiveVal(if (!is.null(data)) names(data) else labels)

    dropped_items <- reactiveVal(c())  # Stores the items dropped into the dropzone

    # Render available draggable items
    output$draggables_ui <- renderUI({
      items <- available_items()
      div(id = ns("draggables"), class = "flex-draggable-container",
          lapply(items, function(item) {
            var_class <- if (is.null(data)) "character" else switch(class(data[[item]])[1],
                                                                    "numeric" = "numeric",
                                                                    "factor" = "character",
                                                                    "character" = "character",
                                                                    "unknown")

            color_class <- switch(var_class,
                                  "numeric" = "numeric-var",
                                  "character" = "character-var",
                                  NULL)

            div(class = paste("draggable", color_class), item, draggable = "true")
          })
      )
    })

    # Handle dropped items
    observeEvent(input$dropped_item, {
      item <- input$dropped_item
      available_items(setdiff(available_items(), item))  # Remove from available list
      dropped_items(c(dropped_items(), item))  # Add to dropped list
    })

    # Handle removed items and return them to the available list
    observeEvent(input$removed_item, {
      item <- input$removed_item
      available_items(c(available_items(), item))  # Return item to available list
      dropped_items(setdiff(dropped_items(), item))  # Remove from dropped list
    })

    # Return dropped items for access in the main server
    return(list(dropped_items = dropped_items))
  })
}

show_licence <- function(ns) {
  showModal(
    modalDialog(
      title = "License Agreement and Terms of Use for the plimanshiny Application",
      tags$div(
        style = "max-height: 550px; overflow-y: auto; padding-right: 55px;",
        h2("About"),
        "plimanshiny provides an interactive Shiny-based graphical user interface for the pliman package,
                facilitating user-friendly access to advanced plant image analysis tools without the need
                for extensive programming knowledge. This package integrates a variety of functionalities
                for high-throughput phenotyping, including but not limited to orthomosaic analysis from drone
                and satellite imagery, shapefile creation and handling, time series analysis, image analysis,
                and phytopathometry, into a cohesive and intuitive application.",
        br(),
        hl(),
        h2("License Agreement"),
        "This software is licensed under the Creative Commons Attribution-NonCommercial (CC BY-NC) license.
          Under this license, you are free to use, share, and adapt the software for non-commercial purposes,
          provided that you give appropriate credit to the developers. For more details on the license, see the ",
        tags$a(href="https://creativecommons.org/licenses/by-nc/4.0/", "Creative Commons BY-NC 4.0 License"),
        ".",
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
        hl(),
        h2("Copyright"),
        "Copyright (C) 2025 - Núcleo de Estudos e Pesquisas em Experimentação e Melhoramento Vegetal - ",
        tags$a(href="https://nepemufsc.com", "nepemufsc.com"),
        br(),
      ),

      br(),

      checkboxInput("agree_terms", "I agree with the terms and conditions", value = FALSE),
      footer = tagList(
        conditionalPanel(
          condition = "input.agree_terms == true",
          actionButton(ns("close_modal"), "Close")
        )
      ),
      easyClose = FALSE,
      size = "xl"
    )
  )
}

ggplot_color <- function(n){
  # adapted from https://stackoverflow.com/a/8197703
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

point_to_polygon <- function(sf_object, n_sides = 200) {
  # Extract CRS of the input sf object
  crsobj <- sf::st_crs(sf_object)
  # Create a new geometry list
  new_geometries <- lapply(seq_len(nrow(sf_object)), function(i) {
    geom_type <- sf::st_geometry_type(sf_object[i, ])
    if (geom_type == "POINT") {
      # Get the point coordinates
      point <- sf::st_coordinates(sf_object[i, ])
      radius <- sf_object[["radius"]][i]

      if (is.na(radius)) {
        stop("Radius is missing for a POINT geometry!")
      }
      angles <- seq(0, 2 * pi, length.out = n_sides + 1)
      circle_coords <- cbind(
        point[1] + radius * cos(angles),  # X coordinates
        point[2] + radius * sin(angles)   # Y coordinates
      )
      sf::st_polygon(list(circle_coords))
    } else {
      sf::st_geometry(sf_object[i, ])
    }
  })
  # Function to ensure all geometries in a list are valid sfg objects
  validate_geometries <- function(geometry_list) {
    lapply(geometry_list, function(geom) {
      if (inherits(geom, "sfg")) {
        return(geom)  # Valid sfg object, return as is
      } else if (inherits(geom, "sfc")) {
        return(geom[[1]])  # Unnest if it's an sfc object
      } else if (is.list(geom) && inherits(geom[[1]], "sfg")) {
        return(geom[[1]])  # Handle nested lists containing sfg objects
      } else {
        stop("Invalid geometry found in the list")
      }
    })
  }
  sf_object <-
    sf::st_set_geometry(sf_object, sf::st_sfc(validate_geometries(new_geometries))) |>
    sf::st_set_crs(crsobj)
  return(sf_object)
}
convert_to_metric <- function(sf_object) {
  if (sf::st_is_longlat(sf_object)) {
    # Get the centroid longitude to determine the UTM zone
    centroid <- sf::st_centroid(sf::st_union(sf_object))
    lon <- sf::st_coordinates(centroid)[1]
    lat <- sf::st_coordinates(centroid)[2]
    utm_zone <- floor((lon + 180) / 6) + 1
    hemisphere <- ifelse(lat >= 0, "", "+south")
    utm_crs <- paste0("+proj=utm +zone=", utm_zone, " ", hemisphere, " +datum=WGS84 +units=m +no_defs")
    sf_object <- sf::st_transform(sf_object, crs = utm_crs)
  }
  return(sf_object)
}

check_mosaic_layers <- function(mosaic, finalindex, r, g, b, re, nir, tir, swir) {
  # Safely attempt to extract mosaic layers
  layers <- list(
    R = try(mosaic[[suppressWarnings(as.numeric(r))]], silent = TRUE),
    G = try(mosaic[[suppressWarnings(as.numeric(g))]], silent = TRUE),
    B = try(mosaic[[suppressWarnings(as.numeric(b))]], silent = TRUE),
    NIR = try(mosaic[[suppressWarnings(as.numeric(nir))]], silent = TRUE),
    RE = try(mosaic[[suppressWarnings(as.numeric(re))]], silent = TRUE),
    SWIR = try(mosaic[[suppressWarnings(as.numeric(swir))]], silent = TRUE),
    TIR = try(mosaic[[suppressWarnings(as.numeric(tir))]], silent = TRUE)
  )

  # Retrieve the used layers and indexes
  usedlayers <- pliman_indexes_eq()
  me <- pliman_indexes_me()

  # Identify indexes for specific bands
  band_indexes <- list(
    NIR = usedlayers[grep("NIR", usedlayers$Equation), 1],
    RE = usedlayers[grep("RE", usedlayers$Equation), 1],
    SWIR = usedlayers[grep("SWIR", usedlayers$Equation), 1],
    TIR = usedlayers[grep("TIR", usedlayers$Equation), 1]
  )

  # Check for missing required bands
  missing_bands <-
    any(finalindex %in% band_indexes$NIR & inherits(layers$NIR, "try-error")) |
    any(finalindex %in% band_indexes$RE & inherits(layers$RE, "try-error")) |
    any(finalindex %in% band_indexes$SWIR & inherits(layers$SWIR, "try-error")) |
    any(finalindex %in% band_indexes$TIR & inherits(layers$TIR, "try-error"))
  if (missing_bands) {
    show_alert("Ops, an error occured.",
               text = "Multispectral indexes cannot be computed since needed bands are not available.",
               type = "error")
  }
}
# Function to round numeric columns in a data frame
round_cols <- function(df, digits = 2) {
  # Check if the input is a data frame
  if (!is.data.frame(df)) stop("Input must be a data frame.")

  # Apply rounding to numeric columns only
  df[] <- lapply(df, function(col) {
    if (is.numeric(col)) {
      round(col, digits = digits)
    } else {
      col
    }
  })

  return(df)
}
adjust_canvas <- function(raster, max_width = 1200, max_height = 800){
  nrows <- nrow(raster)
  ncols <- ncol(raster)
  aspect_ratio <- ncols / nrows
  # Limit canvas width and height

  if (aspect_ratio > 1) {
    # Width is the limiting factor
    width <- min(max_width, max_height * aspect_ratio)
    height <- width / aspect_ratio
  } else {
    # Height is the limiting factor
    height <- min(max_height, max_width / aspect_ratio)
    width <- height * aspect_ratio
  }
  return(c(width, height))
}
check_and_plot <- function(mosaic, r = 1, g = 2, b = 3, zlim = NULL){
  if(is.null(zlim)){
    if(terra::nlyr(mosaic) < 3){
      terra::plot(mosaic[[1]],
                  col = pliman::custom_palette(c("darkred",  "yellow", "darkgreen"), n = 100),
                  maxcell = 1e6,
                  mar = 0,
                  smooth = TRUE)
    } else{
      a <- try(plotRGB(mosaic, smooth = TRUE, r = r, g = g, b = b))
      if (inherits(a, "try-error")) {
        plotRGB(mosaic, smooth = TRUE, r = r, g = g, b = b, stretch = "lin")
      }
    }
  } else{
    plotRGB(mosaic, smooth = TRUE, r = r, g = g, b = b, stretch = "lin", zlim = zlim)
  }
}
guess_coordinate_type <- function(coords) {
  x <- coords[1, 1]
  y <- coords[1, 2]
  if (x >= -180 && x <= 180 && y >= -90 && y <= 90) {
    return("lat/lon")
  }
  if (x >= 160000 && x <= 834000 && y >= 0 && y <= 10000000) {
    return("UTM")
  }
  return("unknown")
}
# Convert UTM coordinates to pixel and line
convert_to_pixel_line <- function(utm_point, xmin, x_res, ymax, y_res) {
  pixel <- (utm_point[1] - xmin) / x_res
  line <- (ymax - utm_point[2]) / y_res
  return(round(c(pixel, line)))
}
to_utm <- function(latlon){
  colnames(latlon) <- c("X", "Y")
  lat <- latlon[, 2][1]
  lon <- latlon[, 1][1]
  utm_zone <- floor((lon + 180) / 6) + 1
  epsg_code <- ifelse(lat >= 0, 32600 + utm_zone, 32700 + utm_zone)

  # Convert gps_coords to UTM
  gps_sf <- sf::st_as_sf(as.data.frame(latlon), coords = c("X", "Y"), crs = 4326)
  utmcoords <-
    sf::st_transform(gps_sf, crs = epsg_code) |>
    sf::st_coordinates()
  return(list(utm  = utmcoords, epsg = epsg_code))
}

get_number <- function(string){
  as.numeric(regmatches(string, gregexpr("\\d+\\.?\\d*", string)))
}
# Function to interpolate values per group
interpolate_group <- function(df) {
  wavelength_seq <- seq(min(df$wavelength), max(df$wavelength), by = 1)
  df |>
    dplyr::arrange(wavelength) |>
    dplyr::distinct(wavelength, .keep_all = TRUE) |>
    (\(df) {
      interpolated_values <- approx(x = df$wavelength, y = df$value, xout = wavelength_seq, method = "linear")$y
      data.frame(dists = unique(df$dists), wavelength = wavelength_seq, value = as.numeric(smooth.spline(interpolated_values, spar = 0.2)$y))
    })()
}
to_datetime <- function(date_vector) {
  formats <- c(
    "%Y-%m-%d %H:%M:%OS",
    "%Y/%m/%d %H:%M:%OS",
    "%d-%m-%Y %H:%M:%OS",
    "%d/%m/%Y %H:%M:%OS",
    "%Y-%m-%d %H:%M",
    "%Y/%m/%d %H:%M",
    "%d-%m-%Y %H:%M",
    "%d/%m/%Y %H:%M",
    "%Y-%m-%d",
    "%Y/%m/%d",
    "%d-%m-%Y",
    "%d/%m/%Y",
    "%m-%d-%Y",
    "%m/%d/%Y",
    "%m-%d-%y",
    "%m/%d/%y"
  )
  suppressWarnings(as.POSIXlt(date_vector, tryFormats = formats))
}


# helper functions for plot quality


line_on_halfplot <- function(shape) {
  help_lines <- function(corners){
    # Compute pairwise distances
    d1 <- sqrt(sum((corners[1, ] - corners[2, ])^2))  # Edge 1-2
    d2 <- sqrt(sum((corners[2, ] - corners[3, ])^2))  # Edge 2-3
    d3 <- sqrt(sum((corners[3, ] - corners[4, ])^2))  # Edge 3-4
    d4 <- sqrt(sum((corners[4, ] - corners[1, ])^2))  # Edge 4-1
    if (d1 < d2) {
      mid1 <- (corners[1, ] + corners[2, ]) / 2
      mid2 <- (corners[3, ] + corners[4, ]) / 2
    } else {
      mid1 <- (corners[2, ] + corners[3, ]) / 2
      mid2 <- (corners[4, ] + corners[1, ]) / 2
    }
    # Define the coordinates of the two points
    coords <- matrix(c(mid1[1], mid1[2],  # First point (X, Y)
                       mid2[1], mid2[2]), # Second point (X, Y)
                     ncol = 2, byrow = TRUE)
    terra::vect(list(coords), type = "lines")
  }
  lines <-
    do.call(rbind,
            lapply(1:nrow(shape), function(i){
              sf::st_coordinates(shape[i, ]) |> help_lines()
            }))
  return(lines)
}

comput_gaps <- function(vals, chm_quantile = 0.25){
  alturas <- smooth.spline(vals$height, spar = 0.6)$y
  threshold <-  (max(alturas) + min(alturas)) / 2
  # threshold <-  quantile(alturas, chm_quantile, na.rm = TRUE)
  below_threshold <- alturas < threshold
  # Identificar descidas (cima -> baixo) e subidas (baixo -> cima)
  descidas <- which(diff(below_threshold) == 1)  # Onde cai
  subidas <- which(diff(below_threshold) == -1)  # Onde sobe

  # Coeficiente de variação
  if(length(descidas) == 0 | length(subidas) == 0){
    vales_fechados <- 0
    gap_area <- 0
  } else {
    # Contar apenas vales fechados (descidas que têm uma subida posterior)
    vales_fechados <- sum(sapply(descidas, function(d) any(subidas > d)))
    # Calcular a área dos gaps usando a Regra do Trapézio
    gap_area <- sum(sapply(seq_along(descidas), function(i) {
      d <- descidas[i]
      s <- subidas[subidas > d][1]  # Primeira subida após a descida
      if (!is.na(s)) {
        x <- d:s  # Índices da seção abaixo do limiar
        y <- alturas[x]  # Valores de altura na seção
        y[y > threshold] <- threshold  # Ajustar os valores para não ultrapassar o limiar

        # Aplicar a Regra do Trapézio: ∫ f(x)dx ≈ ∑ ((y_i + y_(i+1)) / 2) * Δx
        return(sum((y[-1] + y[-length(y)]) / 2))
      }
      return(0)
    }))
  }
  return(data.frame(gaps = vales_fechados,  gap_area = gap_area))
}

mosaic_chm_quality <- function(chm,
                               shapefile,
                               chm_threshold = 0.1,
                               chm_quantile = 0.3,
                               plot_quality = c("absolute", "relative")) {
  plot_quality <- match.arg(plot_quality)
  custom_summary <- function(values, coverage_fractions, ...) {
    valids <- na.omit(values)
    data.frame(
      cv = mean(valids) / sd(valids),
      entropy = entropy(valids),
      coverage = sum(valids > chm_threshold) / length(valids)
    )
  }
  height <- exactextractr::exact_extract(chm$chm[[2]],
                                         shapefile,
                                         fun = custom_summary,
                                         force_df = TRUE,
                                         progress = FALSE)
  lines <- line_on_halfplot(shapefile)
  vals_gaps <-
    terra::extractAlong(chm$chm$height, lines) |>
    dplyr::group_by(ID) |>
    tidyr::nest()

  gaps <- purrr::map_dfr(vals_gaps$data, \(x){ comput_gaps(x, chm_quantile = chm_quantile)})

  dftmp <-
    dplyr::bind_cols(height, gaps, shapefile) |>
    sf::st_as_sf() |>
    dplyr::relocate(unique_id, block, plot_id, row, column, .before = 1) |>
    dplyr::mutate(plot_quality = sqrt(cv^2  + gaps + (5 * (coverage - 1)^2)) / sqrt(7),
                  plot_quality = (plot_quality / max(plot_quality, na.rm = TRUE)),
                  plot_quality = 1 - plot_quality,
                  .before = geometry)

  # **Adjust `plot_quality` to be relative (0-1)**
  if(plot_quality == "relative"){
    min_quality <- min(dftmp$plot_quality, na.rm = TRUE)
    max_quality <- max(dftmp$plot_quality, na.rm = TRUE)

    dftmp <-
      dftmp |>
      dplyr::mutate(plot_quality = (plot_quality - min_quality) / (max_quality - min_quality))
  }
  return(dftmp)
}

gdd_ometto_frue <- function(df,
                                Tbase = 10,
                                Tceil = 40,
                                Topt1 = 26,
                                Topt2 = 32) {

      # Ensure required columns exist
      required_cols <- c("T2M_MAX", "T2M_MIN")
      if (!all(required_cols %in% names(df))) {
        # Try to derive from T2M if available (assuming aggregate_hourly_data was called)
        if ("T2M" %in% names(df)) {
           warning("T2M_MIN/T2M_MAX not found, attempting to use T2M assuming it represents daily mean.", call. = FALSE)
           # This is a fallback, ideally aggregate_hourly_data should create T2M_MIN/MAX
           df <- df |> dplyr::mutate(T2M_MAX = T2M, T2M_MIN = T2M)
        } else {
           stop("Required columns for GDD calculation are missing: T2M_MIN, T2M_MAX. ",
                "Ensure they are selected or derived (e.g., from hourly T2M).")
        }
      }


      Tb <- Tbase
      TB <- Tceil

      # Step 1: Calculate intermediate numeric columns explicitly
      df_intermediate <- df |>
        dplyr::mutate(
          # Ensure temps are numeric (use suppressWarnings for robustness)
          T2M_MAX_num = suppressWarnings(as.numeric(T2M_MAX)),
          T2M_MIN_num = suppressWarnings(as.numeric(T2M_MIN)),
          # Also pre-calculate T2M_num if T2M exists, otherwise NA
          T2M_num = if ("T2M" %in% names(df)) suppressWarnings(as.numeric(T2M)) else NA_real_
        )

      # Step 2: Calculate Tmed using pre-calculated numeric columns
      df_intermediate <- df_intermediate |>
        dplyr::mutate(
          Tmed = dplyr::case_when(
            # Use T2M_num if it's valid (not NA)
            !is.na(T2M_num) ~ T2M_num,
            # Fallback to MIN/MAX if they are valid numerics
            !is.na(T2M_MAX_num) & !is.na(T2M_MIN_num) ~ (T2M_MAX_num + T2M_MIN_num) / 2,
            # Default if neither T2M_num nor MIN/MAX are usable
            TRUE ~ NA_real_
          )
        )

      # Step 3: Calculate GDD and FRUE using Tmed and numeric temps
      df_out <- df_intermediate |>
        dplyr::mutate(
          # GDD Calculation (Ometto method, handling NAs and edge cases)
          GDD = dplyr::case_when(
            # Use pre-calculated numeric columns directly
            is.na(T2M_MAX_num) | is.na(T2M_MIN_num) ~ NA_real_, # Cannot calculate if temps are NA

            # Handle case where Tmin == Tmax first to avoid division by zero later
            T2M_MAX_num == T2M_MIN_num & T2M_MAX_num >= Tb & T2M_MAX_num <= TB ~ T2M_MAX_num - Tb,
            T2M_MAX_num == T2M_MIN_num & T2M_MAX_num < Tb ~ 0,
            T2M_MAX_num == T2M_MIN_num & T2M_MAX_num > TB ~ TB - Tb,

            # Ometto logic using Tmin/Tmax primarily (now safe from division by zero)
            TB > T2M_MAX_num & T2M_MAX_num > T2M_MIN_num & T2M_MIN_num >= Tb ~ (T2M_MAX_num + T2M_MIN_num) / 2 - Tb,
            TB > T2M_MAX_num & T2M_MAX_num > Tb & Tb > T2M_MIN_num ~ ((T2M_MAX_num - Tb)^2) / (2 * (T2M_MAX_num - T2M_MIN_num)),
            TB > Tb & Tb >= T2M_MAX_num & T2M_MAX_num >= T2M_MIN_num ~ 0, # Tmax below Tbase
            T2M_MAX_num > TB & TB > T2M_MIN_num & T2M_MIN_num >= Tb ~
              ( (T2M_MAX_num + T2M_MIN_num)/2 - Tb ) - ( (T2M_MAX_num - TB)^2 / (2*(T2M_MAX_num - T2M_MIN_num)) ),
            T2M_MAX_num > TB & TB > Tb & Tb > T2M_MIN_num ~
              ( (T2M_MAX_num - Tb)^2 - (T2M_MAX_num - TB)^2 ) / (2 * (T2M_MAX_num - T2M_MIN_num)),
            T2M_MAX_num >= Tb & T2M_MIN_num >= TB ~ TB - Tb, # Tmin above Tceil
            T2M_MAX_num >= TB & T2M_MIN_num < Tb ~ # Added case: Max > Ceil, Min < Base
                 ( (T2M_MAX_num - Tb)^2 - (T2M_MAX_num - TB)^2 ) / (2 * (T2M_MAX_num - T2M_MIN_num)), # Same as Tmax>TB>Tb>Tmin case

            TRUE ~ 0 # Default case (should be rare if NA handled)
          ),
          # Ensure GDD is not negative
          GDD = pmax(0, GDD),

          # FRUE: based on Tmed (handle NAs)
          FRUE = dplyr::case_when(
            is.na(Tmed) | Tmed <= Tb | Tmed >= TB ~ 0, # No growth outside base/ceil
            Tmed < Topt1 ~ (Tmed - Tb) / (Topt1 - Tb),
            Tmed > Topt2 ~ (TB - Tmed) / (TB - Topt2),
            TRUE ~ 1 # Optimal range Topt1 to Topt2
          ),
          # Clamp FRUE between 0 and 1
          FRUE = pmax(0, pmin(FRUE, 1))
        ) |>
        # Remove temporary numeric columns
        dplyr::select(-dplyr::any_of(c("T2M_MAX_num", "T2M_MIN_num", "T2M_num"))) # Remove T2M_num as well


      # Add cumulative sums per environment if ENV exists
      if ("ENV" %in% names(df_out) && "DATE" %in% names(df_out)) {
        df_out <- df_out |>
          dplyr::arrange(ENV, DATE) |> # Ensure order
          dplyr::group_by(ENV) |>
          dplyr::mutate(GDD_CUMSUM = cumsum(ifelse(is.na(GDD), 0, GDD))) |>
          dplyr::ungroup()

        if ("RTA" %in% names(df_out)) {
          df_out <- df_out |>
            dplyr::arrange(ENV, DATE) |> # Ensure order
            dplyr::group_by(ENV) |>
            dplyr::mutate(RTA_CUMSUM = cumsum(ifelse(is.na(RTA), 0, RTA))) |>
            dplyr::ungroup()
        }

      } else if ("DATE" %in% names(df_out)) {
         # Global cumulative sum if no ENV but DATE exists
         df_out <- df_out |>
           dplyr::arrange(DATE) |> # Ensure order
           dplyr::mutate(GDD_CUMSUM = cumsum(ifelse(is.na(GDD), 0, GDD)))
         if ("RTA" %in% names(df_out)) {
            df_out <- df_out |>
              dplyr::arrange(DATE) |> # Ensure order
              dplyr::mutate(RTA_CUMSUM = cumsum(ifelse(is.na(RTA), 0, RTA)))
         }
      } else {
          warning("Cannot calculate cumulative sums without ENV and/or DATE columns.", call. = FALSE)
      }

      return(df_out)
    }


envirotype <- function(data,
                       datas,
                       fases = c("01 Estabelecimento", "02 Vegetativo", "03 Floração", "04 Reprodutivo"),
                       var = "T2M_MAX",
                       breaks = c(10, 15, 20, 25, 30),
                       labels = NULL){

  create_stage_labels <- function(df, datas, fases) {
    stopifnot(length(fases) == length(datas))
    lim_inf <- datas
    lim_sup <- c(datas[-1] - 1, Inf)

    stage_exprs <- purrr::map2(lim_inf, lim_sup, function(from, to) {
      rlang::expr(dplyr::between(DFS, !!from, !!to) ~ !!fases[which(lim_inf == from)])
    })

    stage_case_when <- rlang::expr(dplyr::case_when(!!!stage_exprs))
    df |> dplyr::mutate(stage = !!stage_case_when)
  }

  create_class <- function(data, var, .breaks, .labels) {
    var <- rlang::enquo(var)
    .breaks <- unique(c(-Inf, .breaks, Inf))
    if (is.null(.labels)) {
      .labels <- purrr::map_chr(seq_along(.breaks[-1]), function(i) {
        left <- .breaks[i]
        right <- .breaks[i + 1]
        if (is.infinite(left)) glue::glue("< {right}")
        else if (is.infinite(right)) glue::glue("≥ {left}")
        else glue::glue("{left}–{right}")
      })
    }
    data |>
      dplyr::group_by(stage) |>
      dplyr::group_modify(~{
        x <- dplyr::pull(dplyr::select(.x, !!var)) |> na.omit()
        xcut <- cut(x, breaks = .breaks, labels = .labels, include.lowest = TRUE, right = FALSE)
        out <- data.frame(xcut = xcut)
        out |>
          dplyr::group_by(xcut) |>
          dplyr::summarise(Freq = dplyr::n(), .groups = "drop") |>
          dplyr::mutate(fr = Freq / sum(Freq))
      })
  }

  data |>
    create_stage_labels(datas = datas, fases = fases) |>
    dplyr::group_by(ENV) |>
    tidyr::nest() |>
    dplyr::mutate(classes = purrr::map(data, ~create_class(.x, var = var, .breaks = breaks, .labels = labels))) |>
    tidyr::unnest(classes)
}

# Função para agregar dados horários para dados diários
aggregate_hourly_data <- function(data) {
  # Assume que 'data' já possui colunas 'ENV' e 'YYYYMMDD'
  required_cols <- c("ENV", "YYYYMMDD", "T2M")
  if (!all(required_cols %in% colnames(data))) {
     stop(paste("Colunas necessárias para agregação horária ausentes:",
                paste(setdiff(required_cols, colnames(data)), collapse=", ")))
  }

  data |>
    dplyr::group_by(ENV, YYYYMMDD) |> # Agrupa por ENV e YYYYMMDD
    dplyr::summarise(
      # Calcula min e max para T2M, nomeando como T2M_MIN e T2M_MAX
      T2M_MIN = min(T2M, na.rm = TRUE),
      T2M_MAX = max(T2M, na.rm = TRUE),
      # Calcula outras agregações necessárias para colunas numéricas
      # Use across() se precisar agregar muitas colunas da mesma forma
      dplyr::across(
         dplyr::where(is.numeric) & !dplyr::all_of(c("T2M")), # Exclui T2M já processado
         list(mean = mean, sum = sum), # Adicione outras funções se necessário
         na.rm = TRUE,
         .names = "{.col}_{.fn}" # Mantém o padrão para outras colunas
       ),
      .groups = 'keep' # Mantém as colunas de agrupamento (ENV, YYYYMMDD)
    ) |>
    dplyr::ungroup() |> # Remove o agrupamento após summarise
    # Garante que Inf/-Inf de min/max em dias sem dados sejam NA
    dplyr::mutate(
        T2M_MIN = ifelse(is.infinite(T2M_MIN), NA, T2M_MIN),
        T2M_MAX = ifelse(is.infinite(T2M_MAX), NA, T2M_MAX)
    )
}

is_duplicate_point <- function(new_lat, new_lon, points_data) {
  # Ensure new lat/lon are valid numbers before proceeding
  new_lat_num <- suppressWarnings(as.numeric(new_lat))
  new_lon_num <- suppressWarnings(as.numeric(new_lon))
  if (is.na(new_lat_num) || is.na(new_lon_num)) {
    return(FALSE) # Treat NA input as non-duplicate
  }
  new_lat_rnd <- round(new_lat_num, 4)
  new_lon_rnd <- round(new_lon_num, 4)

  # Handle empty list case
  if (!is.list(points_data) || length(points_data) == 0) {
    return(FALSE)
  }

  # Iterate through the list, checking each point individually
  is_dup <- FALSE
  for (i in seq_along(points_data)) {
    point_df <- points_data[[i]]
    # Basic check: is it a data frame with lat/lon?
    if (!is.data.frame(point_df) || !all(c("lat", "lon") %in% names(point_df))) {
      next # Skip malformed list elements
    }
    # Ensure it has at least one row
    if (nrow(point_df) == 0) {
        next
    }

    # Extract and validate coordinates from the existing point
    existing_lat_num <- suppressWarnings(as.numeric(point_df$lat[1])) # Check first row only
    existing_lon_num <- suppressWarnings(as.numeric(point_df$lon[1])) # Check first row only

    if (is.na(existing_lat_num) || is.na(existing_lon_num)) {
      next # Skip points with NA coordinates
    }

    existing_lat_rnd <- round(existing_lat_num, 4)
    existing_lon_rnd <- round(existing_lon_num, 4)

    # Compare
    if (existing_lat_rnd == new_lat_rnd && existing_lon_rnd == new_lon_rnd) {
      is_dup <- TRUE
      break # Found a duplicate, no need to check further
    }
  }

  return(is_dup)
}

calculate_weinberger_ch <- function(data) {
  #The Weinberger method counts hours below 7.2°C
  if (!("T2M" %in% colnames(data))) {
    warning("T2M column is required for chilling hours calculation (Weinberger)")
    return(data)
  }

  #Check if ENV column exists for grouping
  has_env <- "ENV" %in% colnames(data)
  if (!has_env) {
    message("ENV column not found. Calculating accumulation globally.")
  }

  #Calculate hourly chilling contribution
  ch_data <- data |>
    dplyr::mutate(ch_w = ifelse(!is.na(T2M) & T2M < 7.2, 1, 0))

  #If the data is hourly, group by day to get daily sums (optional, but can be useful)
  if ("HR" %in% colnames(ch_data) || "HOUR" %in% colnames(ch_data)) {
    #Ensure a date column exists for grouping
    if (!"DATE" %in% colnames(ch_data)) {
        if (all(c("YEAR", "MO", "DY") %in% colnames(ch_data))) {
          date_str <- paste(ch_data$YEAR, formatC(as.numeric(ch_data$MO), width = 2, flag = "0"), formatC(as.numeric(ch_data$DY), width = 2, flag = "0"), sep = "-")
          ch_data$DATE <- tryCatch(as.Date(date_str), error = function(e) NA)
          if(any(is.na(ch_data$DATE))) warning("Could not create DATE column reliably for daily CH grouping.")
        } else {
          warning("Cannot group by day for daily CH sum (Weinberger): Missing YEAR, MO, DY or DATE columns.")
        }
    }

    if ("DATE" %in% colnames(ch_data) && !any(is.na(ch_data$DATE))) {
      grouping_vars <- if(has_env) c("ENV", "DATE") else "DATE"
      ch_data <- ch_data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) |>
        dplyr::mutate(ch_w_daily = sum(ch_w, na.rm = TRUE)) |>
        dplyr::ungroup()
    }
  }

  #Calculate accumulated chilling hours per environment
  if (has_env) {
      ch_data <- ch_data |>
        dplyr::arrange(ENV, DATE, HR) |> #Ensure correct order for cumsum
        dplyr::group_by(ENV) |>
        dplyr::mutate(ch_w_accum = cumsum(ch_w)) |>
        dplyr::ungroup()
  } else {
    #Calculate global accumulation if ENV is missing
    ch_data <- ch_data |>
      dplyr::arrange(DATE, HR) |> #Ensure correct order
      dplyr::mutate(ch_w_accum = cumsum(ch_w))
  }

  return(ch_data)
}
calculate_utah_ch <- function(data) {
  #The Utah method assigns different weights to different temperature ranges

  if (!("T2M" %in% colnames(data))) {
    warning("T2M column is required for chilling hours calculation (Utah)")
    return(data)
  }
  has_env <- "ENV" %in% colnames(data)
  if (!has_env) message("ENV column not found for Utah calculation. Calculating accumulation globally.")

  #Weights according to the Utah model
  ch_data <- data |>
    dplyr::mutate(
      CH_Utah = dplyr::case_when(
        is.na(T2M) ~ 0, #Handle NA
        T2M < 1.4 ~ 0,
        T2M >= 1.4 & T2M < 2.5 ~ 0.5,
        T2M >= 2.5 & T2M < 9.1 ~ 1.0,
        T2M >= 9.1 & T2M < 12.4 ~ 0.5,
        T2M >= 12.4 & T2M < 15.9 ~ 0,
        T2M >= 15.9 & T2M < 18.0 ~ -0.5,
        T2M >= 18.0 ~ -1.0,
        TRUE ~ 0 #Should not happen with NA handled
      )
    )

  #Optional: Calculate daily sums if hourly
  if ("HR" %in% colnames(ch_data) || "HOUR" %in% colnames(ch_data)) {
      if (!"DATE" %in% colnames(ch_data)) {
        if (all(c("YEAR", "MO", "DY") %in% colnames(ch_data))) {
          date_str <- paste(ch_data$YEAR, formatC(as.numeric(ch_data$MO), width = 2, flag = "0"), formatC(as.numeric(ch_data$DY), width = 2, flag = "0"), sep = "-")
          ch_data$DATE <- tryCatch(as.Date(date_str), error = function(e) NA)
          if(any(is.na(ch_data$DATE))) warning("Could not create DATE column reliably for daily CH grouping (Utah).")
        } else {
          warning("Cannot group by day for daily CH sum (Utah): Missing YEAR, MO, DY or DATE columns.")
        }
      }
      if ("DATE" %in% colnames(ch_data) && !any(is.na(ch_data$DATE))) {
        grouping_vars <- if(has_env) c("ENV", "DATE") else "DATE"
        ch_data <- ch_data |>
          dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) |>
          dplyr::mutate(CH_Utah_daily = sum(CH_Utah, na.rm = TRUE)) |>
          dplyr::ungroup()
      }
  }

  #Calculate accumulated chilling units per environment (cannot go below 0)
  if (has_env) {
      ch_data <- ch_data |>
        dplyr::arrange(ENV, DATE, HR) |>
        dplyr::group_by(ENV) |>
        dplyr::mutate(CH_Utah_accum = cumsum(CH_Utah)) |>
        dplyr::mutate(CH_Utah_accum = pmax(0, CH_Utah_accum)) |> #Ensure non-negative
        dplyr::ungroup()
  } else {
      ch_data <- ch_data |>
        dplyr::arrange(DATE, HR) |> #Ensure correct order
        dplyr::mutate(CH_Utah_accum = cumsum(CH_Utah)) |>
        dplyr::mutate(CH_Utah_accum = pmax(0, CH_Utah_accum)) #Ensure non-negative
  }

  return(ch_data)
}
calculate_nc_ch <- function(data) {
# Calculation of Cold Units (CU) based on the North Carolina model:
#
# The North Carolina model is an adaptation of the Utah model, used in milder climates,
# such as the southeastern United States. This model is specifically designed to account
# for the impact of higher temperatures during winter, which can **reduce or even reverse**
# part of the cold accumulation that occurred earlier. This is represented by negative
# cold units (CU), which occur when temperatures exceed certain critical thresholds.
#
# The model formulation uses temperature ranges to assign positive and negative values
# for Cold Units. For example, temperatures between 16.5°C and 19°C contribute negative
# values (from -0.5 to -2.0 CU), because warmth can impair plant dormancy, which is
# physiologically significant in warmer climates.
#
# The accumulation function is done using `cumsum(CH_NC)`, meaning the negative values
# **are not constrained to zero**. This allows the total CU value over time to become negative,
# reflecting the impact of higher temperatures on the cooling process. In other models,
# accumulation is limited to positive values to avoid this, but in the North Carolina model,
# this feature is intentional, as the model was developed to reflect mild climate conditions
# and the effect of excessive heat during winter.
#
# Therefore, unlike models like Utah’s or more conservative methods that **stop at zero**,
# the North Carolina model allows the accumulated Cold Units value to become negative,
# **representing the actual loss of chilling**.
#
# This is crucial in environments where chilling periods are followed by warm days, which
# is common in many subtropical and temperate regions, such as North Carolina in the USA.

  if (!("T2M" %in% colnames(data))) {
    warning("T2M column is required for chilling hours calculation (North Carolina)")
    return(data)
  }

  has_env <- "ENV" %in% colnames(data)
  if (!has_env) message("ENV column not found for North Carolina calculation. Calculating accumulation globally.")

  # Atribuição dos pesos conforme o modelo da Carolina do Norte
  ch_data <- data |>
    dplyr::mutate(
      CH_NC = dplyr::case_when(
        is.na(T2M) ~ 0,
        T2M < 1.4 ~ 0,
        T2M >= 1.4 & T2M < 7.2 ~ 1.0,
        T2M >= 7.2 & T2M < 13.0 ~ 0.5,
        T2M >= 13.0 & T2M < 16.5 ~ 0.0,
        T2M >= 16.5 & T2M < 19.0 ~ -0.5,
        T2M >= 19.0 & T2M < 20.7 ~ -1.0,
        T2M >= 20.7 ~ -2.0,
        TRUE ~ 0
      )
    )

  # Verificação e construção da coluna de data, se necessário
  if ("HR" %in% colnames(ch_data) || "HOUR" %in% colnames(ch_data)) {
    if (!"DATE" %in% colnames(ch_data)) {
      if (all(c("YEAR", "MO", "DY") %in% colnames(ch_data))) {
        date_str <- paste(ch_data$YEAR, formatC(as.numeric(ch_data$MO), width = 2, flag = "0"), formatC(as.numeric(ch_data$DY), width = 2, flag = "0"), sep = "-")
        ch_data$DATE <- tryCatch(as.Date(date_str), error = function(e) NA)
        if (any(is.na(ch_data$DATE))) warning("Could not create DATE column reliably for daily CH grouping (NC).")
      } else {
        warning("Cannot group by day for daily CH sum (NC): Missing YEAR, MO, DY or DATE columns.")
      }
    }

    # Cálculo opcional do total diário de CH_NC
    if ("DATE" %in% colnames(ch_data) && !any(is.na(ch_data$DATE))) {
      grouping_vars <- if (has_env) c("ENV", "DATE") else "DATE"
      ch_data <- ch_data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) |>
        dplyr::mutate(CH_NC_daily = sum(CH_NC, na.rm = TRUE)) |>
        dplyr::ungroup()
    }
  }

  # Acúmulo fiel dos valores de CH_NC, incluindo valores negativos
  if (has_env) {
    ch_data <- ch_data |>
      dplyr::arrange(ENV, DATE, HR) |>
      dplyr::group_by(ENV) |>
      dplyr::mutate(CH_NC_accum = cumsum(CH_NC)) |>
      dplyr::ungroup()
  } else {
    ch_data <- ch_data |>
      dplyr::arrange(DATE, HR) |>
      dplyr::mutate(CH_NC_accum = cumsum(CH_NC))
  }

  return(ch_data)
}
get_weather_info <- function(df){
  df2 <-
    df  |>
    sf::st_as_sf(coords = c("x", "y"), crs = 32721) |>
    sf::st_transform(crs = 4326) |>
    sf::st_bbox()
  start <- min(df$date)
  end <- max(df$date)
  return(list(lat = mean(df2[c("ymin", "ymax")]),
              lon = mean(df2[c("xmin", "xmax")]),
              start = as.character(start),
              end = as.character(end))
  )
}

get_climate <- function(env = NULL, lat, lon, start,
                        end,
                        params = c("T2M", "T2M_MIN", "T2M_MAX", "PRECTOT", "RH2M", "WS2M"),
                        scale = c("hourly", "daily", "monthly", "climatology"),
                        cache_service = NULL,
                        progress = TRUE,
                        parallel = FALSE,
                        workers = 2,
                        environment = c("r", "shiny")) {
      # Initial validations
      stopifnot(length(lat) == length(lon))
      if (is.null(env)) {
        env <- paste0("ENV", seq_along(lat))
      }
      stopifnot(length(env) == length(lat))
      scale <- match.arg(scale)
      environment <- match.arg(environment)

      # Validate parameters for selected scale
      nasaparams_path <- system.file("app/www/nasaparams.csv", package = "plimanshiny", mustWork = FALSE)
      if (nasaparams_path == "") stop("nasaparams.csv file not found")
      nasaparams <- read.csv(nasaparams_path)

      # Internal function to fetch data for a single point
      fetch_data_point <- function(lat_i, lon_i, env_i, start_i, end_i) {
        tryCatch({
          # Prepare scale and parameters
          scale_lowercase <- tolower(scale)
          api_scale <- scale_lowercase
          if(scale_lowercase == "climatology") api_scale = "climatology"

          # Check for future dates and adjust if needed
          current_date <- Sys.Date()
          start_date <- as.Date(start_i)
          end_date <- as.Date(end_i)

          if (start_date > current_date || end_date > current_date) {
            warning(paste("Future dates detected for point", lat_i, lon_i,
                         "- NASA POWER only provides historical data."))

            # For non-climatology requests, adjust dates to recent historical period
            if (api_scale != "climatology") {
              # Adjust to recent past (last 30 days up to yesterday)
              new_end_date <- current_date - 1
              new_start_date <- new_end_date - 30

              warning(paste("Adjusting request dates to recent historical period:",
                           format(new_start_date, "%Y-%m-%d"), "to",
                           format(new_end_date, "%Y-%m-%d")))

              start_date <- new_start_date
              end_date <- new_end_date
            }
          }

          # Validate parameters for the selected scale
          suitableparams <- nasaparams[nasaparams$level == scale_lowercase, ]$abbreviation
          valid_params <- intersect(params, suitableparams)
          if(length(valid_params) == 0) {
            warning(paste("No valid parameters for scale", scale_lowercase, "at point", lat_i, lon_i))
            return(NULL)
          }
          params_str_i <- paste(valid_params, collapse = ",")

            # Build API URL
            base_url <- "https://power.larc.nasa.gov/api/temporal"
            if (api_scale == "monthly") {
            # For monthly, use only the YYYY format (year only)
            start_fmt <- format(start_date, "%Y")
            end_fmt <- format(end_date, "%Y")

            # Ensure it does not exceed the current year for future data
            current_year <- as.numeric(format(current_date, "%Y"))
            if (as.numeric(end_fmt) > current_year) {
              end_fmt <- format(current_date, "%Y")
              warning(paste("End date adjusted to current year:", end_fmt))
            }

            url <- glue::glue("{base_url}/{api_scale}/point?parameters={params_str_i}&community=AG&longitude={lon_i}&latitude={lat_i}&start={start_fmt}&end={end_fmt}&format=CSV")
            } else if (api_scale %in% c("hourly", "daily")) {
            start_fmt <- format(start_date, "%Y%m%d")
            end_fmt <- format(end_date, "%Y%m%d")
            url <- glue::glue("{base_url}/{api_scale}/point?parameters={params_str_i}&community=AG&longitude={lon_i}&latitude={lat_i}&start={start_fmt}&end={end_fmt}&format=CSV")
            } else {
            url <- glue::glue("{base_url}/climatology/point?parameters={params_str_i}&community=AG&longitude={lon_i}&latitude={lat_i}&format=CSV")
            }

          req <- httr2::request(url) |> httr2::req_options(timeout = 60, ssl_verifypeer = 0)
          resp <- tryCatch(httr2::req_perform(req), error = function(e) {
            warning(paste("HTTP request error for point", lat_i, lon_i, ":", e$message))
            return(NULL)
          })

          if (is.null(resp)) {
            warning(paste("HTTP request failed for point", lat_i, lon_i))
            return(NULL)
          }

          if (httr2::resp_status(resp) >= 400) {
            warning(paste("Invalid API response for point", lat_i, lon_i, "status:",
                         httr2::resp_status(resp),
                         "- Reason:", httr2::resp_status_desc(resp)))

            # Try to extract more error details from response body if possible
            error_body <- tryCatch(httr2::resp_body_string(resp), error = function(e) NULL)
            if (!is.null(error_body) && nchar(error_body) > 0) {
              warning(paste("NASA POWER API error details:", substr(error_body, 1, 200), "..."))
            }

            return(NULL)
          }

          # Process response
          file <- tempfile(fileext = ".csv")
          on.exit(unlink(file), add = TRUE)
          content <- httr2::resp_body_string(resp)

          # Check if data exists
          if (grepl("No data was found that matched your query", content, ignore.case = TRUE)) {
            warning(paste("No data found for point", lat_i, lon_i))
            return(NULL)
          }

          writeLines(content, file)

          # Read CSV skipping NASA header
          linhas <- readLines(file)
          linha_inicio_dados <- which(grepl("-END HEADER-", linhas)) + 1

          if(length(linha_inicio_dados) == 0) {
            # Try reading without skipping header if marker not found
            dados <- tryCatch(read.csv(file, check.names = FALSE), error = function(e) {
              warning(paste("Error reading CSV for point", lat_i, lon_i, ":", e$message))
              return(NULL)
            })
          } else {
            dados <- tryCatch(read.csv(file, skip = linha_inicio_dados - 1, check.names = FALSE),
                             error = function(e) {
                               warning(paste("Error reading CSV with skip for point", lat_i, lon_i, ":", e$message))
                               return(NULL)
                             })
          }

          if (is.null(dados) || nrow(dados) == 0) {
            warning(paste("Empty data for point", lat_i, lon_i))
            return(NULL)
          }

          # Add metadata
          dados$ENV <- env_i
          dados$LAT <- lat_i
          dados$LON <- lon_i

          # Convert NASA fill values to NA
          dados[dados == -999] <- NA
          dados[dados == -99] <- NA

          # Create DATE column if possible
          if ("YEAR" %in% names(dados) && "DOY" %in% names(dados)) {
            dados$DATE <- tryCatch(as.Date(paste0(dados$YEAR, "-", dados$DOY), format = "%Y-%j"),
                                  error = function(e) NA)
          } else if ("YYYYMMDD" %in% names(dados)) {
            dados$DATE <- tryCatch(as.Date(as.character(dados$YYYYMMDD), format = "%Y%m%d"),
                                  error = function(e) NA)
          } else if (all(c("YEAR", "MO", "DY") %in% names(dados))) {
            date_str <- paste(dados$YEAR,
                             formatC(as.numeric(dados$MO), width = 2, flag = "0"),
                             formatC(as.numeric(dados$DY), width = 2, flag = "0"), sep = "-")
            dados$DATE <- tryCatch(as.Date(date_str), error = function(e) NA)
          }

          # Standardize common column names
          if("PRECTOTCORR" %in% names(dados))
            names(dados)[names(dados) == "PRECTOTCORR"] <- "PRECTOT"

          return(dados)
        }, error = function(e) {
          warning(paste("General error processing point", lat_i, lon_i, ":", e$message))
          return(NULL)
        })
      }

      # Check cache first, if provided
      if (!is.null(cache_service)) {
        # The cache_service should implement getOrFetch()
        request_params <- list(
          coordinates = data.frame(env = env, lat = lat, lon = lon, start = start, end = end),
          params = params,
          scale = scale
        )

        # Use cache service
        cache_result <- cache_service$getOrFetch(request_params)

        # If no fetch needed, return cache data
        if (!cache_result$needs_fetch) {
          return(cache_result$data)
        }

        # If fetch needed, store cache key for later use
        cache_key <- cache_result$cache_key
      }

      # Configure parallelism
      if (parallel) {
        future::plan(future::multisession, workers = workers)
      } else {
        future::plan(future::sequential)
      }
      on.exit(future::plan(future::sequential), add = TRUE)

      # Fetch data with progress (if needed)
      if (progress && environment == "shiny") {
        if(parallel){
          progressr::withProgressShiny({
            p <- progressr::progressor(steps = length(lat))
            result_list <- furrr::future_map(seq_along(lat), function(i) {
              p(message = sprintf("%s", env[i]))
              fetch_data_point(lat[i], lon[i], env[i], start[i], end[i])
            }, .options = furrr::furrr_options(seed = TRUE))
          }, message = "Fetching climate data for")
        } else{
          progressr::withProgressShiny({
            p <- progressr::progressor(steps = length(lat))
            result_list <- furrr::future_map(seq_along(lat), function(i) {
              p(message = sprintf("Fetching %s (%d/%d)", env[i], i, length(lat)))
              fetch_data_point(lat[i], lon[i], env[i], start[i], end[i])
            }, .options = furrr::furrr_options(seed = TRUE))
          }, message = "Fetching climate data")
        }
      } else if (progress && environment == "r") {
        progressr::handlers(global = TRUE)
        progressr::with_progress({
          p <- progressr::progressor(steps = length(lat))
          result_list <- furrr::future_map(seq_along(lat), function(i) {
            p(message = sprintf("Fetching %s (%d/%d)", env[i], i, length(lat)))
            fetch_data_point(lat[i], lon[i], env[i], start[i], end[i])
          }, .options = furrr::furrr_options(seed = TRUE))
        })
      } else {
        # No progress bar
        result_list <- furrr::future_map(seq_along(lat), function(i) {
          fetch_data_point(lat[i], lon[i], env[i], start[i], end[i])
        }, .options = furrr::furrr_options(seed = TRUE))
      }

      # Remove null results
      result_list <- result_list[!sapply(result_list, is.null)]

      if (length(result_list) == 0) {
        warning("No data was obtained for the requested points")
        return(NULL)
      }

      # Combine results
      final_df <- tryCatch(
        dplyr::bind_rows(result_list),
        error = function(e) {
          warning(paste("Error combining results:", e$message))
          return(NULL)
        }
      )

      # Save to cache if provided
      if (!is.null(cache_service) && !is.null(final_df) && nrow(final_df) > 0) {
        cache_service$save(cache_key, final_df)
      }

      return(final_df)
    }
