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
    df <- df %>% as.data.frame() %>% pliman::rownames_to_column()
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
                             theme = reactableTheme(
                               cellPadding = "8px 10px",
                               style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
                               searchInputStyle = list(width = "100%")
                             ),
                             ...){
  pars <- read_pars()
  if(pars$sparkline & nrow(df) > 1){
    dcd = colDef(
      maxWidth = 400,
      footer = function(values) {
        if (!is.numeric(values)) return()
        sparkline::sparkline(values, type = "box", width = 100, height = 30)
      })
  } else{
    dcd = colDef(
      maxWidth = 400
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
    col_2(
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
    col_3(
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
      title = "License Agreement and Terms of Use for the {plimanshiny} Application",
      tags$div(
        style = "max-height: 550px; overflow-y: auto; padding-right: 55px;",
        h2("About"),
        "{plimanshiny} provides an interactive Shiny-based graphical user interface for the pliman package,
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
        "If your company wishes to use {plimanshiny} for commercial purposes, please contact us at ",
        tags$a(href="mailto:contato@nepemufsc.com", "contato@nepemufsc.com"),
        " to discuss commercial terms and licensing fees.",
        br(), br(),

        h3("Penalty Clause"),
        "Companies that use {plimanshiny} for commercial purposes without proper authorization will
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
