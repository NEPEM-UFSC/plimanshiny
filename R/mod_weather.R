#' weather UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_weather_ui <- function(id) {
  ns <- NS(id)

  tagList(
    bs4TabCard(
      width = 12,
      selected = "Coordinate selection",
      icon = icon("gears"),
      status  = "success",
      type = "tabs",
      tabPanel(
        title = "Coordinate selection",
        fluidRow(
          col_7(
            leafletOutput(ns("map2"), height = "740px")
          ),
          col_5(
            bs4TabCard(
              width = 12,
              selected = "Input Parameters",
              icon = icon("gears"),
              status = "success",
              type = "tabs",
              tabPanel(
                title = "Input Parameters",
                dateRangeInput(ns("dates"), "Select the period", start = Sys.Date() - 35, end = Sys.Date() - 5),
                # botão para condição de usar ou nao nome de municipios
                prettySwitch(
                  inputId = ns("use_mun"),
                  label = "Search by municipality (Brazil only)",
                  value = FALSE,
                  status = "success",
                  fill = TRUE
                ),
                conditionalPanel(
                  condition = "input.use_mun == true", ns = ns,
                  fluidRow(
                    col_6(
                      pickerInput(
                        inputId = ns("state"),
                        label = "Select the state",
                        choices = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO",
                                    "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE",
                                    "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP",
                                    "SE", "TO"),
                        multiple = TRUE,
                        options = list(
                          `actions-box` = TRUE,
                          `live-search` = TRUE
                        )
                      )
                    ),
                    col_6(
                      pickerInput(
                        inputId = ns("mun"),
                        label = "Select the municipality",
                        choices = NULL,
                        multiple = TRUE,
                        options = list(
                          `actions-box` = TRUE,
                          `live-search` = TRUE
                        )
                      )
                    )
                  )

                ),
                textInput(ns("envname"),
                          label = "Environment name",
                          value = ""),
                prettyRadioButtons(
                  inputId = ns("scale"),
                  label = "Select the scale",
                  choices = c("hourly", "daily", "monthly", "climatology"),
                  selected = "daily",
                  inline = TRUE,
                  status = "success"
                ),
                hl(),
                fluidRow(
                  col_7(

                  ),
                  col_5(
                    actionButton(
                      inputId = ns("nasadict"),
                      label = tagList(
                        icon = icon("spell-check", verify_fa = FALSE), "NASAPOWER | Dictionary"
                      ),
                      status = "info"
                    )
                  )
                ),
                prettySwitch(
                  inputId = ns("show_thermal_opts"),
                  label = "Show thermal parameters options",
                  value = FALSE,
                  status = "success",
                  fill = TRUE
                ),
                conditionalPanel(
                  condition = "input.show_thermal_opts == true", ns = ns,

                    # GDD SECTION
                    div(
                    style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
                    h4(icon("sun"), "Growing Degree Days (GDD)", style = "margin-top: 0;"),
                    p("GDD quantifies the heat required for plant development based on daily temperatures, using the Ometto method for greater precision in thermal limits."),
                    ),
                  fluidRow(
                    col_12(
                      prettySwitch(
                        inputId = ns("computegdd"),
                        label = "Calculate Growing Degree Days (GDD)",
                        value = FALSE,
                        status = "success",
                        fill = TRUE
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.computegdd == true", ns = ns,
                    fluidRow(
                      col_6(
                        numericInput(ns("basemin"),
                                     label = "Base temp (°C)",
                                     value = 10,
                                     step = 0.1)
                      ),
                      col_6(
                        numericInput(ns("baseupp"),
                                     label = "Ceiling temp (°C)",
                                     value = 40,
                                     step = 0.1)
                      )
                    ),
                    fluidRow(
                      col_6(
                        numericInput(ns("optimallower"),
                                     label = "Optimal lower (°C)",
                                     value = 26,
                                     step = 0.1)
                      ),
                      col_6(
                        numericInput(ns("optimalupper"),
                                     label = "Optimal upper (°C)",
                                     value = 32,
                                     step = 0.1)
                      )
                    )
                  ),

                  hl(),

                    # CHILLING HOURS SECTION
                    div(
                    style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
                    h4(icon("snowflake"), "Chilling Hours", style = "margin-top: 0;"),
                    p("Accumulated hours in specific temperature ranges required for dormancy breaking in some perennial crops."),
                    p("Scale will be automatically set to hourly."),
                    ),
                  fluidRow(
                    col_12(
                      prettySwitch(
                        inputId = ns("ch_w"),
                        label = "Weinberger (< 7.2°C/45°F)",
                        value = FALSE,
                        status = "success",
                        fill = TRUE
                      )
                    )
                  ),
                  fluidRow(
                    col_12(
                      prettySwitch(
                        inputId = ns("ch_utah"),
                        label = "Utah model (weighted by temp. range)",
                        value = FALSE,
                        status = "success",
                        fill = TRUE
                      )
                    )
                  ),
                  fluidRow(
                    col_12(
                      prettySwitch(
                        inputId = ns("ch_nc"),
                        label = "North Carolina (mild climate adaptation)",
                        value = FALSE,
                        status = "success",
                        fill = TRUE
                      )
                    )
                  )
                ),
                hl(),
                pickerInput(
                  inputId = ns("params"),
                  label = "Select the parameters",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE,
                    size = 20
                  )
                ),
                fluidRow(
                  col_6(
                    shinyWidgets::actionBttn(
                      inputId = ns("get_weather"),
                      label = "Fetch data",
                      style = "material-flat",
                      color = "primary",
                      icon = icon("cloud-download-alt"),
                      size = "md",
                      block = TRUE
                    ),
                  ),
                  col_6(
                    shinyWidgets::actionBttn(
                      inputId = ns("clear_points"),
                      label = "Clear points",
                      style = "material-flat",
                      color = "danger",
                      icon = icon("trash-alt"),
                      size = "md",
                      block = TRUE
                    ),
                  )
                ),
                br(),
                fluidRow(
                  col_12(
                    prettySwitch(
                      inputId = ns("parallel"),
                      label = "Parallel processing",
                      value = FALSE,
                      status = "success",
                      fill = TRUE
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.parallel == true", ns = ns,
                  fluidRow(
                    col_12(
                      numericInput(
                        inputId = ns("ncores"),
                        label = "Number of cores",
                        value = 1,
                        min = 1,
                        max = 6,
                        step = 1,
                        width = "100%"
                      )
                    )
                  )
                ),
                uiOutput(ns("cache_status_ui"))
              ),
              tabPanel(
                title = "Selected points",
                DT::dataTableOutput(ns("latlondata"), height = "560px")
              )
            )
          )
        )
      ),
      tabPanel(
        title = "Weather data",
        reactable::reactableOutput(ns("weather_table"), height = "720px")
      ),
      tabPanel(
        title = "Distribution",
        fluidRow(
          col_4(
            pickerInput(ns("variable"),
                        label = "Select the variable",
                        choices = NULL,
                        selected = NULL,
                        options = list(
                          `actions-box` = TRUE,
                          `live-search` = TRUE
                        )
            )
          ),
          col_4(
            pickerInput(ns("facet"),
                        label = "Facet by",
                        choices = NULL,
                        selected = NULL,
                        options = list(
                          `actions-box` = TRUE,
                          `live-search` = TRUE
                        )
            )
          )
        ),
        plotlyOutput(ns("envirotypes_dist"), height = "560px")
      ),
      tabPanel(
        title = "Envirotypes",
        fluidRow(
          col_6(
            "Envirotypes",
            fluidRow(
              col_4(
                textInput(ns("quantiles"),
                          label = "Quantiles")
              ),
              col_4(
                textInput(ns("cropdates"),
                          label = "Crop stages")
              ),
              col_4(
                textInput(ns("cropdates_label"),
                          label = "Crop stages (labels)")
              )
            ),
            plotlyOutput(ns("envirotypes"), height = "640px")
          ),
          col_6(
            "Dataset",
            reactable::reactableOutput(ns("dataenviro"), height = "720px")
          )
        )
      )
    )
  )
}

# --- Builder Pattern ---
WeatherRequestBuilder <- R6::R6Class("WeatherRequestBuilder",
  private = list(
    .coordinates = NULL,
    .params = NULL,
    .scale = NULL,
    .parallel = FALSE,
    .workers = 1,
    .compute_gdd = FALSE,
    .gdd_params = list(),
    .compute_ch = list(w = FALSE, utah = FALSE, nc = FALSE)
  ),
  public = list(
    initialize = function() {
      # Default values can be set here if needed
      private$.scale <- "daily"
      private$.gdd_params <- list(basemin = 10, baseupp = 40, optimallower = 26, optimalupper = 32)
      return(self)
    },
    with_coordinates = function(coords_df) {
      stopifnot(is.data.frame(coords_df), all(c("env", "lat", "lon", "start", "end") %in% colnames(coords_df)))
      private$.coordinates <- coords_df
      return(self)
    },
    with_parameters = function(params) {
      stopifnot(is.character(params))
      private$.params <- params
      return(self)
    },
    with_scale = function(scale) {
      stopifnot(scale %in% c("hourly", "daily", "monthly", "climatology"))
      private$.scale <- scale
      return(self)
    },
    with_parallel = function(parallel, workers = 1) {
      stopifnot(is.logical(parallel), is.numeric(workers))
      private$.parallel <- parallel
      private$.workers <- if(parallel) workers else 1
      return(self)
    },
    with_gdd = function(compute, basemin, baseupp, optimallower, optimalupper) {
      stopifnot(is.logical(compute), is.numeric(basemin), is.numeric(baseupp), is.numeric(optimallower), is.numeric(optimalupper))
      private$.compute_gdd <- compute
      if(compute) {
        private$.gdd_params <- list(basemin = basemin, baseupp = baseupp, optimallower = optimallower, optimalupper = optimalupper)
      }
      return(self)
    },
    with_chilling_hours = function(ch_w, ch_utah, ch_nc) {
       stopifnot(is.logical(ch_w), is.logical(ch_utah), is.logical(ch_nc))
       private$.compute_ch <- list(w = ch_w, utah = ch_utah, nc = ch_nc)
       # If any CH method is enabled, force scale to hourly
       if(ch_w || ch_utah || ch_nc) {
         private$.scale <- "hourly"
       }
       return(self)
    },
    build = function() {
      # Validation before building
      if (is.null(private$.coordinates) || nrow(private$.coordinates) == 0) {
        stop("Coordinates are required.")
      }
      if (is.null(private$.params) || length(private$.params) == 0) {
        stop("At least one parameter is required.")
      }

      # Ensure required params for GDD/CH are included if requested
      if (private$.compute_gdd || private$.compute_ch$w || private$.compute_ch$utah || private$.compute_ch$nc) {
         required_temp_params <- character(0)
         if (private$.scale == "hourly") {
           required_temp_params <- "T2M"
         } else if (private$.scale %in% c("daily", "monthly")) {
           required_temp_params <- c("T2M_MIN", "T2M_MAX")
         }

         missing_req_params <- required_temp_params[!required_temp_params %in% private$.params]
         if(length(missing_req_params) > 0) {
            private$.params <- unique(c(private$.params, required_temp_params))
            message("Automatically added required parameters for thermal calculations: ", paste(missing_req_params, collapse=", "))
         }
      }


      return(list(
        coordinates = private$.coordinates,
        params = private$.params,
        scale = private$.scale,
        parallel = private$.parallel,
        workers = private$.workers,
        compute_gdd = private$.compute_gdd,
        gdd_params = private$.gdd_params,
        compute_ch = private$.compute_ch
      ))
    }
  )
)

# --- Cache Service Layer ---
WeatherCacheService <- R6::R6Class("WeatherCacheService",
  private = list(
    .memory_cache = list(), # In-memory cache store
    .cache_dir = NULL,     # Disk cache directory path

    # Moved cache functions become private methods
    .generate_cache_key = function(df, params, scale) {
      # Format dates consistently
      formatted_dates <- lapply(1:nrow(df), function(i) {
          start_date <- as.Date(df$start[i])
          end_date <- as.Date(df$end[i])
          paste(format(start_date, "%Y%m%d"), format(end_date, "%Y%m%d"), sep = "_")
      })

      # Combine coordinates and formatted dates into a unique identifier
      location_date_str <- paste(
        paste(round(df$lat, 4), round(df$lon, 4), sep = ":"), # Round coords for consistency
        unlist(formatted_dates),
        sep = "_"
      )
      location_date_str <- paste(location_date_str, collapse = "|")

      # Add climate parameters and scale to the key
      params_str <- paste(sort(params), collapse = "_") # Sort for consistency

      # Create MD5 hash for a shorter, reliable key
      digest::digest(paste(location_date_str, params_str, scale, sep = "::"), algo = "md5")
    },

    .get_disk_cache = function(cache_key) {
      cache_file <- file.path(private$.cache_dir, paste0(cache_key, ".rds"))
      if (file.exists(cache_file)) {
        file_info <- file.info(cache_file)
        cache_age <- difftime(Sys.time(), file_info$mtime, units = "days")

        # If cache is older than 30 days, invalidate
        if (cache_age > 30) {
          file.remove(cache_file)
          return(NULL)
        }
        tryCatch({
          cached_data <- readRDS(cache_file)
          return(cached_data)
        }, error = function(e) {
          warning("Error reading cache file ", cache_file, ": ", e$message)
          if(file.exists(cache_file)) file.remove(cache_file)
          return(NULL)
        })
      }
      return(NULL)
    },

    .validate_cache_dates = function(cached_data, requested_df) {
      # If we don't have dates in the structure, we can't validate
      if (!("YYYYMMDD" %in% colnames(cached_data))) {
        warning("Cache validation failed: YYYYMMDD column missing.")
        return(FALSE)
      }
      if (!("ENV" %in% colnames(cached_data))) {
        warning("Cache validation failed: ENV column missing.")
        return(FALSE)
      }

      # Check each unique combination of location and dates
      all_covered <- TRUE
      for (i in 1:nrow(requested_df)) {
        env_name <- requested_df$env[i]
        start_date <- as.Date(requested_df$start[i])
        end_date <- as.Date(requested_df$end[i])

        # Filter cache data by environment
        env_data <- cached_data[cached_data$ENV == env_name, ]
        if (nrow(env_data) == 0) {
          all_covered <- FALSE
          break # Environment not found in cache
        }

        # Convert dates for comparison
        env_dates <- tryCatch(as.Date(env_data$YYYYMMDD, format = "%Y%m%d"), error = function(e) NULL)
        if(is.null(env_dates) || all(is.na(env_dates))) {
            warning("Cache validation: Could not parse dates in YYYYMMDD for env ", env_name)
            all_covered <- FALSE
            break
        }
        env_dates <- na.omit(env_dates)

        # Check if all requested dates are in the cache
        requested_dates_seq <- seq.Date(start_date, end_date, by = "day")
        if (!all(requested_dates_seq %in% env_dates)) {
          all_covered <- FALSE
          break # Missing dates in cache
        }
      }
      return(all_covered)
    },

    .save_to_disk_cache = function(cache_key, data) {
       cache_file <- file.path(private$.cache_dir, paste0(cache_key, ".rds"))
       tryCatch({
         saveRDS(data, cache_file)
       }, error = function(e) {
         warning("Error saving to disk cache ", cache_file, ": ", e$message)
         if (file.exists(cache_file)) {
           file.remove(cache_file)
         }
       })
    },

    .add_to_memory_cache = function(cache_key, data) {
       # Limit in-memory cache size (simple FIFO)
       if (length(private$.memory_cache) >= 5) {
         try({
           oldest_key <- names(private$.memory_cache)[1]
           private$.memory_cache[[oldest_key]] <- NULL
         }, silent = TRUE)
       }
       private$.memory_cache[[cache_key]] <- data
    }
  ),

  public = list(
    initialize = function(cache_dir = file.path(rappdirs::user_cache_dir("plimanshiny"), "weather_cache")) {
      private$.cache_dir <- cache_dir
      if (!dir.exists(private$.cache_dir)) {
        dir.create(private$.cache_dir, recursive = TRUE)
      }
      return(self)
    },

    # The core method implementing the Cache Pattern + Chain of Responsibility logic
    getOrFetch = function(request_params, fetch_function, ...) {
      # 1. Generate Cache Key
      df <- request_params$coordinates
      cache_key <- private$.generate_cache_key(df, request_params$params, request_params$scale)

      # 2. Check Memory Cache (First level)
      in_memory_cache <- private$.memory_cache[[cache_key]]
      if (!is.null(in_memory_cache)) {
        if (private$.validate_cache_dates(in_memory_cache, df)) {
          return(list(
            data = in_memory_cache,
            source = "memory",
            cache_key = cache_key,
            needs_fetch = FALSE
          ))
        } else {
           private$.memory_cache[[cache_key]] <- NULL # Remove invalid entry
        }
      }

      # 3. Check Disk Cache (Second level)
      disk_cache <- private$.get_disk_cache(cache_key)
      if (!is.null(disk_cache)) {
        if (private$.validate_cache_dates(disk_cache, df)) {
          private$.add_to_memory_cache(cache_key, disk_cache) # Load into memory
          return(list(
            data = disk_cache,
            source = "disk",
            cache_key = cache_key,
            needs_fetch = FALSE
          ))
        } else {
           # Remove the invalid disk file
           file.remove(file.path(private$.cache_dir, paste0(cache_key, ".rds")))
        }
      }

      # 4. Cache Miss - Need to Fetch (Pass responsibility to fetch)
      return(list(
        data = NULL,
        source = "fetch",
        cache_key = cache_key,
        needs_fetch = TRUE
      ))
    },

    # Method to save data to cache (both memory and disk)
    save = function(cache_key, data) {
      if (is.null(data) || nrow(data) == 0) {
         warning("Attempted to save NULL or empty data to cache. Skipping.")
         return(invisible(self))
      }
      private$.add_to_memory_cache(cache_key, data)
      private$.save_to_disk_cache(cache_key, data)
      return(invisible(self)) # Allow chaining
    },

    # Method to clean cache
    cleanCache = function(age_days = NULL) {
      # Clear memory cache
      private$.memory_cache <- list()

      # Clear disk cache
      files_removed_count <- 0
      disk_cleared_status <- FALSE

      if (!dir.exists(private$.cache_dir)) {
        return(list(memory_cleared = TRUE, disk_cleared = FALSE, files_removed = 0, reason = "Cache directory does not exist"))
      }

      cache_files <- list.files(private$.cache_dir, pattern = "\\.rds$", full.names = TRUE)
      files_to_remove <- cache_files # Default to remove all

      if (!is.null(age_days) && is.numeric(age_days) && age_days > 0) {
         file_infos <- file.info(cache_files)
         files_to_remove <- cache_files[difftime(Sys.time(), file_infos$mtime, units = "days") > age_days]
      }

      if (length(files_to_remove) > 0) {
        removed_status <- unlink(files_to_remove)
        files_removed_count <- length(files_to_remove)
        disk_cleared_status <- all(removed_status == 0)
      } else {
         disk_cleared_status <- TRUE
      }

      list(
        memory_cleared = TRUE,
        disk_cleared = disk_cleared_status,
        files_removed = files_removed_count
      )
    },
    # Expose memory cache length for UI status
    getMemoryCacheStatus = function() {
      return(length(private$.memory_cache))
    },
    # Expose disk cache status for UI status
    getDiskCacheStatus = function() {
       return(dir.exists(private$.cache_dir) && length(list.files(private$.cache_dir, pattern = "\\.rds$")) > 0)
    }
  )
)


# --- Command Pattern (Refactored) ---
FetchWeatherCommand <- R6::R6Class("FetchWeatherCommand",
  private = list(
    .request_params = NULL,
    .session = NULL,
    .rv = NULL,
    .resclimate = NULL,
    .dfs = NULL,
    .cache_service = NULL, # Use CacheService instance
    .service_fun = NULL
  ),
  public = list(
    # Updated initialize to accept cache_service
    initialize = function(request_params, session, rv, resclimate, dfs, cache_service, service_fun) {
      private$.request_params <- request_params
      private$.session <- session
      private$.rv <- rv
      private$.resclimate <- resclimate
      private$.dfs <- dfs
      private$.cache_service <- cache_service # Store the service instance
      private$.service_fun <- service_fun
      return(self)
    },
    execute = function() {
      req_params <- private$.request_params
      df <- req_params$coordinates

      # 1. Check if API call is already in progress
      if (private$.rv$api_in_progress) {
        sendSweetAlert(
          session = private$.session,
          title = "Processing in progress",
          text = "There is already a data request in progress. Please wait.",
          type = "warning"
        )
        return()
      }

      # 2. Check Cache using CacheService
      cache_result <- private$.cache_service$getOrFetch(req_params)

      # 3. Handle Cache Hit
      if (!cache_result$needs_fetch) {
          cached_data <- cache_result$data
          private$.resclimate(cached_data)
          private$.dfs[["weather"]] <- create_reactval("weather", cached_data)
          cache_info <- paste0(
              "Source: ", cache_result$source, "\n",
              "Environments: ", length(unique(cached_data$ENV)), "\n",
              "Parameters: ", length(req_params$params), "\n",
              "Dates: ", min(as.Date(cached_data$YYYYMMDD, format = "%Y%m%d")), " to ",
              max(as.Date(cached_data$YYYYMMDD, format = "%Y%m%d"))
          )
          sendSweetAlert(
              session = private$.session,
              title = "Data retrieved from cache!",
              text = paste0("Climate information was retrieved from ", cache_result$source, " cache.\n\n", cache_info),
              type = "success"
          )
          return() # Stop execution, data found in cache
      }

      # 4. Cache Miss - Proceed to Fetch Data
      cache_key <- cache_result$cache_key # Get the key for saving later

      private$.rv$api_in_progress <- TRUE
      private$.rv$processing_error <- NULL
      all_weather_data <- NULL

        tryCatch({
          # Call the service function (get_climate)
          all_weather_data <- private$.service_fun(
            env = df$env,
            lat = df$lat,
            lon = df$lon,
            start = df$start,
            end = df$end,
            params = req_params$params,
            scale = req_params$scale,
            parallel = req_params$parallel,
            workers = req_params$workers,
            environment = "shiny" # Ensure shiny progress is used
          )

          if (is.null(all_weather_data) || nrow(all_weather_data) == 0) {
            stop("No weather data retrieved from the API.")
          }

          # --- Data Inspection and Cleaning (Added) ---

          # Clean temperature columns before GDD/CH calculations
          temp_cols_to_clean <- intersect(c("T2M", "T2M_MIN", "T2M_MAX"), names(all_weather_data))
          if (length(temp_cols_to_clean) > 0) {
            for (col in temp_cols_to_clean) {
              if (!is.numeric(all_weather_data[[col]])) {
                 # Use suppressWarnings to avoid flooding console if conversion creates NAs
                 all_weather_data[[col]] <- suppressWarnings(as.numeric(all_weather_data[[col]]))
              }
            }
          }
          # --- End Inspection and Cleaning ---

          # 5. Post-processing (GDD, Chilling Hours)
          if (req_params$compute_gdd || req_params$compute_ch$w || req_params$compute_ch$utah || req_params$compute_ch$nc) {

            # --- Ensure Date/Time columns are present ---
            if (!"YYYYMMDD" %in% names(all_weather_data)) {
               if ("DATE" %in% names(all_weather_data)) {
                  all_weather_data$YYYYMMDD <- format(as.Date(all_weather_data$DATE), "%Y%m%d")
               } else if (all(c("YEAR", "MO", "DY") %in% names(all_weather_data))) {
                  all_weather_data$YYYYMMDD <- paste0(
                     all_weather_data$YEAR,
                     formatC(all_weather_data$MO, width = 2, flag = "0"),
                     formatC(all_weather_data$DY, width = 2, flag = "0")
                  )
               } else {
                  stop("Cannot proceed: Missing required date columns (YYYYMMDD or DATE or YEAR/MO/DY).")
               }
            }
            if (!"DATE" %in% names(all_weather_data)) {
                all_weather_data$DATE <- as.Date(all_weather_data$YYYYMMDD, format = "%Y%m%d")
            }
            if (!"DOY" %in% names(all_weather_data)) {
               all_weather_data$DOY <- as.numeric(format(all_weather_data$DATE, "%j"))
            }
            # Ensure HR exists if scale is hourly (needed for CH accumulation order)
            if (req_params$scale == "hourly" && !"HR" %in% names(all_weather_data) && "HOUR" %in% names(all_weather_data)) {
                all_weather_data$HR <- all_weather_data$HOUR
            }
             if (req_params$scale == "hourly" && !"HR" %in% names(all_weather_data)) {
                 # Attempt to infer HR if missing, maybe from row number within a day? Risky.
                 warning("Hourly scale selected but HR column missing. CH accumulation might be incorrect if data isn't ordered chronologically.")
                 # Add a dummy HR if absolutely necessary, but this is not ideal
                 # all_weather_data <- all_weather_data |> dplyr::group_by(ENV, YYYYMMDD) |> dplyr::mutate(HR = dplyr::row_number() - 1) |> dplyr::ungroup()
             }
            # --- End Date/Time columns ---

            # Initialize placeholders for results
            ch_results_hourly <- NULL # Will hold HOURLY CH results
            gdd_results_daily <- NULL # Will hold DAILY GDD results

            # --- Chilling Hours Calculation (Operates on HOURLY data) ---
            if (req_params$scale == "hourly" && (req_params$compute_ch$w || req_params$compute_ch$utah || req_params$compute_ch$nc)) {
              # Assume calculate_*_ch functions return hourly data with cumulative columns
              # e.g., columns: ENV, YYYYMMDD, HR, DATE, T2M, ch_w, ch_w_accum, CH_Utah, CH_Utah_accum, CH_NC, CH_NC_accum
              all_weather_data_for_ch <- all_weather_data # Work on a copy

              if (req_params$compute_ch$w) {
                all_weather_data_for_ch <- calculate_weinberger_ch(all_weather_data_for_ch) # Assumes this adds ch_w and ch_w_accum
              }
              if (req_params$compute_ch$utah) {
                all_weather_data_for_ch <- calculate_utah_ch(all_weather_data_for_ch) # Assumes this adds CH_Utah and CH_Utah_accum
              }
              if (req_params$compute_ch$nc) {
                all_weather_data_for_ch <- calculate_nc_ch(all_weather_data_for_ch) # Assumes this adds CH_NC and CH_NC_accum
              }
              # Keep only the necessary ID columns and the new CH columns
              ch_cols_to_keep <- c("ENV", "YYYYMMDD", "HR", "DATE", # ID columns
                                   grep("^ch_w", names(all_weather_data_for_ch), value = TRUE), # Weinberger cols
                                   grep("^CH_Utah", names(all_weather_data_for_ch), value = TRUE), # Utah cols
                                   grep("^CH_NC", names(all_weather_data_for_ch), value = TRUE)) # NC cols
              ch_results_hourly <- all_weather_data_for_ch[, intersect(ch_cols_to_keep, names(all_weather_data_for_ch)), drop = FALSE]

            } else if (req_params$compute_ch$w || req_params$compute_ch$utah || req_params$compute_ch$nc) {
                 warning("Chilling hours requested but scale is not hourly. Calculation skipped.", call. = FALSE)
            }

            # --- GDD Calculation (Requires DAILY data) ---
            if (req_params$compute_gdd) {
                daily_data_for_gdd <- NULL
                if (req_params$scale == "hourly") {
                    # Aggregate hourly to daily to get Tmin/Tmax
                    daily_aggregated_data <- aggregate_hourly_data(all_weather_data)
                    # Ensure required columns exist
                    if(all(c("T2M_MIN", "T2M_MAX") %in% names(daily_aggregated_data))) {
                        daily_data_for_gdd <- daily_aggregated_data
                    } else {
                         warning("Could not aggregate T2M_MIN/T2M_MAX from hourly data. Skipping GDD.", call. = FALSE)
                    }
                } else {
                    # Use the data directly if already daily/monthly
                    daily_data_for_gdd <- all_weather_data
                    # Ensure Tmin/Tmax exist
                    if(!all(c("T2M_MIN", "T2M_MAX") %in% names(daily_data_for_gdd))){
                         warning("GDD requested for daily/monthly scale, but T2M_MIN/T2M_MAX are missing. Skipping GDD.", call. = FALSE)
                         daily_data_for_gdd <- NULL # Prevent calculation
                    }
                }

                # Calculate GDD if we have valid daily data
                if (!is.null(daily_data_for_gdd)) {
                    # Ensure DATE column exists for gdd_ometto_frue if it needs it
                     if (!"DATE" %in% names(daily_data_for_gdd) && "YYYYMMDD" %in% names(daily_data_for_gdd)) {
                        daily_data_for_gdd$DATE <- as.Date(daily_data_for_gdd$YYYYMMDD, format = "%Y%m%d")
                     }

                    gdd_results_daily_temp <- gdd_ometto_frue(
                        df = daily_data_for_gdd,
                        Tbase = req_params$gdd_params$basemin,
                        Tceil = req_params$gdd_params$baseupp,
                        Topt1 = req_params$gdd_params$optimallower,
                        Topt2 = req_params$gdd_params$optimalupper
                    )
                    # Select only relevant GDD columns to avoid duplication
                    gdd_cols_to_keep <- intersect(names(gdd_results_daily_temp), c("ENV", "YYYYMMDD", "DATE", "GDD", "FRUE", "GDD_CUMSUM", "RTA_CUMSUM")) # Add DATE if present
                    gdd_results_daily <- gdd_results_daily_temp[, gdd_cols_to_keep, drop = FALSE]
                }
            }


            # --- Combine Results ---
            combined_data <- all_weather_data # Start with the original fetched data (hourly or daily)

            # Join GDD results (Daily values broadcasted to hourly rows if scale is hourly)
            if (!is.null(gdd_results_daily)) {
                # Join by ENV and YYYYMMDD. DATE might also be useful if present in both.
                join_by_gdd <- c("ENV", "YYYYMMDD")
                if("DATE" %in% names(combined_data) && "DATE" %in% names(gdd_results_daily)) {
                    join_by_gdd <- c(join_by_gdd, "DATE")
                }
                # Select GDD columns to join, excluding the join keys themselves to avoid suffixing
                gdd_cols_to_join <- setdiff(names(gdd_results_daily), join_by_gdd)
                # Ensure no columns to join already exist in combined_data to avoid suffixes
                gdd_cols_to_join <- gdd_cols_to_join[!gdd_cols_to_join %in% names(combined_data)]

                if(length(gdd_cols_to_join) > 0) {
                  combined_data <- dplyr::left_join(combined_data,
                                                    gdd_results_daily[, c(join_by_gdd, gdd_cols_to_join), drop = FALSE],
                                                    by = join_by_gdd)
                } else {
                   warning("No new GDD columns to join or columns already exist.", call. = FALSE)
                }
            }

            # Join CH results (Hourly values joined to hourly rows) - Only if scale was hourly
            if (req_params$scale == "hourly" && !is.null(ch_results_hourly)) {
                # Join by ENV, YYYYMMDD, HR. DATE might also be useful.
                join_by_ch <- c("ENV", "YYYYMMDD", "HR")
                 if("DATE" %in% names(combined_data) && "DATE" %in% names(ch_results_hourly)) {
                    join_by_ch <- c(join_by_ch, "DATE")
                }
                # Select CH columns to join, excluding the join keys
                ch_cols_to_join <- setdiff(names(ch_results_hourly), join_by_ch)
                # Ensure no columns to join already exist in combined_data to avoid suffixes
                ch_cols_to_join <- ch_cols_to_join[!ch_cols_to_join %in% names(combined_data)]

                if(length(ch_cols_to_join) > 0) {
                  combined_data <- dplyr::left_join(combined_data,
                                                    ch_results_hourly[, c(join_by_ch, ch_cols_to_join), drop = FALSE],
                                                    by = join_by_ch)
                } else {
                   warning("No new CH columns to join or columns already exist.", call. = FALSE)
                }
            }

            # Assign the final combined data back
            all_weather_data <- combined_data
            # print(str(all_weather_data)) # Debug final combined data

          } else {
             # This 'else' corresponds to the main 'if' checking if GDD or CH calculation is needed.
             # No post-processing needed if neither GDD nor CH was requested.
             # all_weather_data remains as fetched.
          }

          # 6. Save to Cache using CacheService
          if (is.null(all_weather_data) || nrow(all_weather_data) == 0) {
          } else {
             private$.cache_service$save(cache_key, all_weather_data)
             # Check memory cache status immediately after saving
             mem_status_after_save <- private$.cache_service$getMemoryCacheStatus()
          }


          # 7. Update Reactives - Remains the same
          private$.resclimate(all_weather_data)
          private$.dfs[["weather"]] <- create_reactval("weather", all_weather_data)

          private$.rv$api_in_progress <- FALSE
          private$.rv$processing_error <- NULL

          # 8. Notify Success - Remains the same
          sendSweetAlert(
            session = private$.session,
            title = "Data successfully retrieved!",
            text = sprintf("Data processed for %d locations with %d climate parameters.", length(unique(all_weather_data$ENV)), length(req_params$params)),
            type = "success"
          )

        }, error = function(e) {
          # Error Handling - Remains the same
          private$.rv$api_in_progress <- FALSE
          private$.rv$processing_error <- e$message
          private$.resclimate(NULL)
          private$.dfs[["weather"]] <- NULL
          sendSweetAlert(
            session = private$.session,
            title = "Error during processing",
            text = paste("An error occurred:", e$message),
            type = "error"
        )
      }) # end tryCatch
    }) # end withProgress
  ) # end public list


#' weather Server Functions (Refactored)
#'
#' @noRd
mod_weather_server <- function(id, dfs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # index equation
    observeEvent(input$nasadict, {

      output$nasadictionary <- renderReactable({
        nasaparams <-
          system.file("app/www/nasaparams.csv", package = "plimanshiny", mustWork = FALSE) |>
          read.csv()
        nasaparams <- nasaparams[, c(2, 1, 4)]
        render_reactable(nasaparams,
                         defaultPageSize = 10,
                         columns = list(
                           abbreviation = colDef(maxWidth = 250),
                           name = colDef(maxWidth = 650),
                           level = colDef(maxWidth = 200)
                         ))

      })

      showModal(
        modalDialog(
          title = "NASAPOWER dictionary",
          reactable::reactableOutput(ns("nasadictionary")),
          footer = NULL,
          easyClose = TRUE,
          size = "xl"
        )
      )
    })


    # Instantiate Cache Service
    cache_service <- WeatherCacheService$new()

    # --- Reactive Values ---
    points <- reactiveValues(data = list())
    resclimate <- reactiveVal(NULL)
    rv <- reactiveValues(
      api_in_progress = FALSE,
      processing_error = NULL
    )
    # --- UI Logic and Observers ---
    coords <- reactive({
      if (length(points$data) == 0) return(NULL)
      # Ensure consistent column types before binding
      points_list <- lapply(points$data, function(p) {
         p$lat <- as.numeric(p$lat)
         p$lon <- as.numeric(p$lon)
         p$start <- as.Date(p$start)
         p$end <- as.Date(p$end)
         p
      })
      df <- tryCatch(dplyr::bind_rows(points_list), error = function(e) {
          warning("Error binding points: ", e$message)
          return(NULL) # Return NULL if binding fails
      })
      if(is.null(df) || nrow(df) == 0) return(NULL)
      as.data.frame(df)
    })
    output$map2 <- renderLeaflet({
      leaflet() |>
        addTiles(group = "OpenStreetMap") |>
        addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") |>
        setView(lng = 0, lat = 0, zoom = 2) |>
        addLayersControl(
          baseGroups = c("OpenStreetMap", "Esri World Imagery"),
          options = layersControlOptions(collapsed = TRUE)
        ) |>
        hideGroup("Esri World Imagery")
    })
    observeEvent(input$map2_click, {
      click <- input$map2_click
      req(click$lat, click$lng, input$envname, input$dates)
      new_lat <- round(click$lat, 4)
      new_lon <- round(click$lng, 4)

      is_dup <- is_duplicate_point(new_lat, new_lon, points$data)

      if (!is_dup) {
        new_point <- data.frame(
          env = input$envname,
          lat = new_lat,
          lon = new_lon,
          start = input$dates[[1]],
          end = input$dates[[2]],
          stringsAsFactors = FALSE
        )
        # Append data safely
        current_length <- length(points$data)
        points$data[[current_length + 1]] <- new_point
      } else {
         showNotification("Point already exists at these coordinates.", type = "warning")
      }
    })
    observeEvent(input$mun, {
      req(input$state, input$mun, input$dates)

      # Read municipality file

      mun_df_path <- system.file("app/www/coords_muni.csv", package = "plimanshiny", mustWork = FALSE)
      if(mun_df_path == "") {
          showNotification("Municipality coordinates file not found.", type = "error") # Translated
          return()
      }
      mun_df <- read.csv(file = mun_df_path, sep = ",")
      selected_mun <- dplyr::filter(mun_df,
                                   abbrev_state %in% input$state,
                                   name_muni %in% input$mun)

      # Check if municipalities were selected
      if(nrow(selected_mun) == 0) {
        showNotification("No municipalities found matching the selected criteria.", type = "warning") # Translated
        return()
      }

      # Get current date range
      start_date <- input$dates[[1]]
      end_date <- input$dates[[2]]

      # Validate dates
      if(is.null(start_date) || is.null(end_date) || end_date < start_date) {
        showNotification("Please select a valid date range.", type = "error") # Translated
        return()
      }

      # Temporary list for new points
      points_to_add <- list()
      skipped_invalid_count <- 0 # Counter for invalid coordinates
      skipped_duplicate_count <- 0 # Counter for duplicates (for logging only)
      current_point_count <- length(isolate(points$data)) # Initial isolated count

      # Process each selected municipality
      for(i in 1:nrow(selected_mun)) {
        mun_row <- selected_mun[i, ]
        # print(mun_row)

        # Explicit conversion to numeric with check
        new_lat <- suppressWarnings(as.numeric(as.character(mun_row$lat))) # Use lat
        new_lon <- suppressWarnings(as.numeric(as.character(mun_row$lon))) # Use lon

        # Check if coordinates are valid
        if(is.na(new_lat) || is.na(new_lon)) {
          skipped_invalid_count <- skipped_invalid_count + 1 # Increment invalid counter
          next
        }

        # Round coordinates for consistent comparison
        new_lat_rnd <- round(new_lat, 4)
        new_lon_rnd <- round(new_lon, 4)

        # Check for duplicate point (using refactored function)
        # Compare against existing points AND those being added in this batch
        existing_points_combined <- c(isolate(points$data), points_to_add) # Combine existing and those collected in this loop
        is_duplicate <- is_duplicate_point(new_lat_rnd, new_lon_rnd, existing_points_combined)

        if(!is_duplicate) {
          # Generate unique environment name, considering points to be added
          # Use municipality name as 'env' by default
          env_name <- mun_row$name_muni

          # Create new point data frame
          new_point <- data.frame(
            env = env_name,
            lat = new_lat_rnd, # Use rounded coordinates
            lon = new_lon_rnd, # Use rounded coordinates
            start = as.Date(start_date), # Ensure it's Date
            end = as.Date(end_date),     # Ensure it's Date
            stringsAsFactors = FALSE
          )
          # print(new_point)

          # Add to temporary list
          points_to_add[[length(points_to_add) + 1]] <- new_point

        } else {
          skipped_duplicate_count <- skipped_duplicate_count + 1 # Increment duplicate counter (for logging)
        }
      } # End of for loop

      # Add all new points at once
      if(length(points_to_add) > 0) {
        temp_list <- isolate(points$data)
        points$data <- c(temp_list, points_to_add)
      } else {
      }
      if(skipped_invalid_count > 0) {
        showNotification(paste("Skipped", skipped_invalid_count, "municipalities (invalid coordinates)."), type = "warning") # Translated
      }
    })
    observe({
      req(input$state)
      mun_df_path <- system.file("app/www/coords_muni.csv", package = "plimanshiny", mustWork = FALSE)
      if(mun_df_path == "") return() # Do nothing if file not found
      listmun <-
        read.csv(file = mun_df_path, sep = ",") |>
        dplyr::filter(abbrev_state %in% input$state) |>
        dplyr::arrange(name_muni) # Sort for better UX

      updatePickerInput(
        session,
        "mun",
        choices = unique(listmun$name_muni), # Ensure unique choices
        selected = NULL
      )
    })
    observeEvent(input$clear_points, {
      points$data <- list()
      # Clear markers on the map
      leafletProxy("map2") |> clearMarkers()
      showNotification("All points cleared.", type = "message")
    })
    observe({
       current_coords <- coords() # Use the reactive expression
       if(is.null(current_coords) || nrow(current_coords) == 0){
         updateTextInput(session, "envname", value = "ENV_1")
       } else{
         updateTextInput(session, "envname", value = paste0("ENV_", nrow(current_coords) + 1))
       }
    })

    # Display selected points table (latlondata) - Updated for delete button
    output$latlondata <- DT::renderDataTable({
      df_coords <- coords()
      req(df_coords, nrow(df_coords) > 0)

      # Add row IDs and delete buttons
      display_df <- df_coords |>
        dplyr::mutate(
          row_id = dplyr::row_number(), # Add a unique ID for each row
          delete = sprintf(
            '<button class="btn btn-danger btn-sm delete-point-btn" data-rowid="%s" type="button"><i class="fa fa-trash"></i></button>',
            row_id # Use the row ID
          )
        ) |>
        dplyr::select(row_id, env, lat, lon, start, end, delete) # Keep row_id for internal use if needed, or remove

      DT::datatable(
        display_df,
        escape = FALSE, # Allow HTML buttons
        selection = "none",
        rownames = FALSE,
        options = list(
          scrollY = "560px", scrollCollapse = TRUE, paging = FALSE, searching = FALSE, info = FALSE,
          # Ensure rowId is not treated as a data column by DT if selected
          columnDefs = list(list(visible=FALSE, targets=c('row_id'))) # Hide row_id column
        ),
        callback = DT::JS(sprintf("
          table.on('click', '.delete-point-btn', function() {
            var rowId = $(this).data('rowid'); // Get the row ID
            Shiny.setInputValue('%s', {id: rowId, timestamp: Date.now()}, {priority: 'event'});
          });
        ", ns("delete_point_click")))
      )
    })

    # Handle delete point button click (remains the same)
    observeEvent(input$delete_point_click, {
      req(input$delete_point_click$id)
      row_id_to_delete <- as.numeric(input$delete_point_click$id)

      isolate({
        if (row_id_to_delete > 0 && row_id_to_delete <= length(points$data)) {
          points$data <- points$data[-row_id_to_delete]
        } else {
           warning("Invalid row ID received for deletion: ", row_id_to_delete)
        }
      })
    })
    # Observe: Update Map Markers
    observe({
      df_coords <- coords() # Get the reactive data frame
      # Use req() to proceed only if df_coords is valid (not NULL, not empty)
      req(df_coords, nrow(df_coords) > 0)

      leafletProxy("map2", session) |>
        clearMarkers() |>
        addMarkers(
          data = df_coords,
          lng = ~lon,
          lat = ~lat,
          popup = ~paste("Env:", env, "<br>Lat:", lat, "<br>Lon:", lon, "<br>Start:", start, "<br>End:", end),
          label = ~env
        )
    })

    # Update parameter choices based on scale (remains the same)
    observe({
      nasaparams_path <- system.file("app/www/nasaparams.csv", package = "plimanshiny", mustWork = FALSE)
      if(nasaparams_path == "") return()
      nasaparams <- read.csv(nasaparams_path)

      scale_lowercase <- tolower(input$scale)
      suitableparams <- nasaparams[nasaparams$level == scale_lowercase, ]$abbreviation

      # Define default selections based on scale
      default_selection <- switch(scale_lowercase,
        "hourly" = c("T2M", "RH2M", "PRECTOTCORR", "PS", "WS2M"),
        "daily" = c("T2M", "T2M_MIN", "T2M_MAX", "T2M_RANGE", "RH2M", "PRECTOTCORR", "PS", "WS2M", "WD2M", "GWETTOP", "GWETROOT"),
        "monthly" = c("T2M", "T2M_MIN", "T2M_MAX", "T2M_RANGE", "RH2M", "PRECTOTCORR", "PS", "WS2M", "WD2M", "GWETTOP", "GWETROOT"),
        "climatology" = c("T2M", "T2M_MIN", "T2M_MAX", "T2M_RANGE", "RH2M", "PRECTOTCORR", "PS", "WS2M", "WD2M", "GWETTOP", "GWETROOT"),
        NULL # Default if scale doesn't match
      )
      # Ensure default selection only contains suitable params for the scale
      valid_selection <- intersect(default_selection, suitableparams)

      updatePickerInput(session, "params",
                        choices = suitableparams,
                        selected = valid_selection)
    })

    # Enable/disable parallel core selection (remains the same)
    observe({
      if(input$parallel) {
        shinyjs::enable("ncores")
        ncore_detected <- tryCatch(parallel::detectCores(), error = function(e) 2) # Default to 2 if detection fails
        # Limit default/max cores reasonably (e.g., max 8 or detected - 1)
        max_cores_allowed <- min(ncore_detected, 8)
        default_cores <- min(max(1, ncore_detected - 1), max_cores_allowed) # Default to n-1, capped
        updateNumericInput(session, "ncores",
                           value = default_cores,
                           max = max_cores_allowed,
                           min = 1)
      } else {
        shinyjs::disable("ncores")
        updateNumericInput(session, "ncores", value = 1) # Reset to 1 when disabled
      }
    })

    # --- Trigger API Fetch (Updated) ---
    observeEvent(input$get_weather, {
      current_coords <- coords()
      req(current_coords, nrow(current_coords) > 0)

      builder <- WeatherRequestBuilder$new()
      request_data <- tryCatch({
          builder$with_coordinates(current_coords)$
                  with_parameters(input$params)$
                  with_scale(input$scale)$
                  with_parallel(input$parallel, input$ncores)$
                  with_gdd(input$computegdd, input$basemin, input$baseupp, input$optimallower, input$optimalupper)$
                  with_chilling_hours(input$ch_w, input$ch_utah, input$ch_nc)$
                  build()
      }, error = function(e) {
          sendSweetAlert(session, title = "Input Error", text = paste("Failed to build request:", e$message), type = "error")
          return(NULL)
      })
      req(request_data)

      # Create and execute command, passing the cache_service instance
      command <- FetchWeatherCommand$new(
        request_params = request_data,
        session = session,
        rv = rv,
        resclimate = resclimate,
        dfs = dfs,
        cache_service = cache_service, # Pass the service instance
        service_fun = get_climate
      )
      command$execute()
    })


    # --- UI Updates (Weather table, Distribution, Envirotypes) ---
    output$weather_table <- reactable::renderReactable({
      data_to_display <- resclimate()
      req(data_to_display, nrow(data_to_display) > 0)

      # Use withProgress for loading feedback
      withProgress(message = 'Loading data table...', value = 0.5, {
        # Optimize data for display (round numeric columns)
        formatted_data <- data_to_display |> roundcols(digits = 3) # Assuming roundcols exists
        incProgress(0.5) # Increment progress

        # Render the table (assuming render_reactable exists)
        render_reactable( # Use the helper function
            formatted_data,
            filterable = TRUE,
            searchable = TRUE,
            sortable = TRUE,
            resizable = TRUE,
            max_width = NULL, # Adjust as needed
            defaultPageSize = 15,
            paginationType = "jump",
            highlight = TRUE,
            striped = TRUE,
            compact = TRUE,
            wrap = FALSE,
            showPageSizeOptions = TRUE,
            pageSizeOptions = c(10, 15, 25, 50, 100)
          )
      })
    })
    observe({
      data_res <- resclimate()
      if (!is.null(data_res) && nrow(data_res) > 0) {
         # Get numeric columns suitable for distribution plot
         numeric_cols <- names(data_res)[sapply(data_res, is.numeric)]
         # Exclude lat/lon/year/doy etc. if desired
         cols_to_exclude <- c("LAT", "LON", "YEAR", "DOY", "MO", "DY", "HR", "HOUR")
         valid_choices <- setdiff(numeric_cols, cols_to_exclude)

         selected_var <- isolate(input$variable) # Keep current selection if valid
         if(is.null(selected_var) || !selected_var %in% valid_choices) {
            selected_var <- valid_choices[1] # Default to first valid choice
         }

         updatePickerInput(session, "variable",
                           choices = valid_choices,
                           selected = selected_var)
      } else {
         updatePickerInput(session, "variable", choices = NULL, selected = NULL)
      }
    })
    observe({
      data_res <- resclimate()
      if (!is.null(data_res) && nrow(data_res) > 0) {
         choices <- c("none", colnames(data_res))
         selected_facet <- isolate(input$facet)
         if(is.null(selected_facet) || !selected_facet %in% choices) {
             selected_facet <- "none"
         }

         updatePickerInput(session, "facet",
                           choices = choices,
                           selected = selected_facet)
      } else {
         updatePickerInput(session, "facet", choices = "none", selected = "none")
      }
    })
    output$envirotypes_dist <- renderPlotly({
      req(input$variable, input$facet)
      plot_data <- resclimate()
      req(plot_data, nrow(plot_data) > 0)

      variable <- input$variable
      facet_var <- input$facet

      # Basic validation
      if (!variable %in% colnames(plot_data)) {
        return(plotly::plot_ly() |> plotly::layout(title = paste("Variable", variable, "not found")))
      }
      if (!is.numeric(plot_data[[variable]])) {
         return(plotly::plot_ly() |> plotly::layout(title = paste("Variable", variable, "is not numeric")))
      }
      if (all(is.na(plot_data[[variable]]))) {
        return(plotly::plot_ly() |> plotly::layout(title = paste("All values for", variable, "are NA")))
      }

      # Create ggplot object
      p <- ggplot(plot_data, aes(x = !!sym(variable))) +
        geom_density(fill = "steelblue", alpha = 0.6, na.rm = TRUE) +
        theme_minimal(base_size = 14) + # Slightly smaller base size
        labs(x = variable, y = "Density", title = paste("Distribution of", variable))

      # Add facet if selected and valid
      if (facet_var != "none") {
        if (facet_var %in% colnames(plot_data) && length(unique(na.omit(plot_data[[facet_var]]))) > 1) {
          p <- p + facet_wrap(as.formula(paste("~", facet_var)), ncol = 1, scales = "free_y") # Free y-scale often better for density facets
        } else {
          p <- p + labs(subtitle = paste("Facet variable", facet_var, "is invalid or has only one level"))
        }
      }

      # Convert to plotly
      plotly::ggplotly(p, tooltip = c("x")) |>
        plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = list('sendDataToCloud', 'select2d', 'lasso2d'))
    })
    output$envirotypes <- renderPlotly({
      # Basic requirements
      req(input$variable, resclimate(), nrow(resclimate()) > 0)
      # Require valid inputs for envirotyping
      req(input$quantiles, input$cropdates, input$cropdates_label)

      plot_data_orig <- resclimate()
      variable <- input$variable

      withProgress(message = 'Generating envirotypes...', value = 0, {
        tryCatch({
          # 1. Validate and parse inputs
          incProgress(0.1, detail = "Validating inputs...")
          quantiles_in <- tryCatch(as.numeric(unlist(strsplit(input$quantiles, ","))), warning = function(w) NULL)
          cropdates_in <- tryCatch(as.numeric(unlist(strsplit(input$cropdates, ","))), warning = function(w) NULL)
          cropdates_label_in <- trimws(unlist(strsplit(input$cropdates_label, ","))) # Trim whitespace

          if (is.null(quantiles_in) || any(is.na(quantiles_in)) || length(quantiles_in) == 0) stop("Invalid Quantiles input. Use comma-separated numbers.")
          if (is.null(cropdates_in) || any(is.na(cropdates_in)) || length(cropdates_in) == 0) stop("Invalid Crop Stages input. Use comma-separated numbers (e.g., DOY).")
          if (length(cropdates_in) != length(cropdates_label_in)) stop("Number of Crop Stages dates and labels must match.")
          if (!variable %in% names(plot_data_orig)) stop(paste("Selected variable", variable, "not found in data."))
          if (!"DATE" %in% names(plot_data_orig)) stop("DATE column is required for envirotyping.") # Assuming envirotype needs DATE

          # Ensure DATE is Date class
          plot_data <- plot_data_orig |> dplyr::mutate(DATE = as.Date(DATE))
          if(any(is.na(plot_data$DATE))) stop("Could not parse DATE column.")

          # Assuming 'datas' in envirotype refers to DOY
          if (!"DOY" %in% names(plot_data)) {
             plot_data <- plot_data |> dplyr::mutate(DOY = as.numeric(format(DATE, "%j")))
          }
          # Rename DOY to DFS if that's what envirotype expects
          if (!"DFS" %in% names(plot_data) && "DOY" %in% names(plot_data)) {
             plot_data <- plot_data |> dplyr::rename(DFS = DOY)
          }
          if (!"DFS" %in% names(plot_data)) stop("Column 'DFS' (Day From Sowing/Start or DOY) is required.")


          # 2. Optimize dataset (optional, keep if needed)
          incProgress(0.2, detail = "Preparing data...")
          plot_data <- plot_data |> dplyr::filter(!is.na(!!sym(variable)), !is.na(DFS))
          if(nrow(plot_data) == 0) stop("No valid data remaining after filtering NAs.")

          # 3. Calculate Envirotypes
          incProgress(0.4, detail = "Calculating envirotypes...")
          # Ensure envirotype function is available
          dfenviro <- envirotype(
              data = plot_data, # Pass the prepared data
              datas = cropdates_in, # Use parsed DOY/DFS values
              fases = cropdates_label_in, # Use parsed labels
              var = variable, # Use selected variable
              breaks = quantiles_in, # Use parsed quantiles
              labels = NULL # Let envirotype generate labels
            )

          if (is.null(dfenviro) || nrow(dfenviro) == 0) stop("Envirotyping resulted in no data.")

          # 4. Update Envirotype Table
          incProgress(0.7, detail = "Updating table...")
          output$dataenviro <- reactable::renderReactable({
            dfenviro |>
              dplyr::select(ENV, stage, xcut, Freq, fr) |>
              dplyr::rename(Environment = ENV, `Crop stage` = stage, Envirotype = xcut, Frequency = Freq, `Relative frequency` = fr) |>
              roundcols(digits = 3) |> # Assuming roundcols exists
              render_reactable( # Assuming render_reactable exists
                filterable = TRUE, searchable = TRUE, sortable = TRUE,
                compact = TRUE, highlight = TRUE
              )
          })

          # 5. Create Envirotype Plot
          incProgress(0.8, detail = "Creating plot...")
          # Use consistent colors
          num_colors <- length(unique(dfenviro$xcut))
          env_colors <- grDevices::hcl.colors(max(3, min(9, num_colors)), palette = "Blues 3")

          p <-
            ggplot(dfenviro, aes(x = fr, y = ENV, fill = xcut)) + # Use fr for relative frequency
            geom_col(position = "stack", width = 0.9, color = "white", linewidth = 0.2) + # Use geom_col for stacked bars
            facet_wrap(~stage, ncol = 1, scales = "free_y") + # Use renamed column
            scale_fill_manual(values = env_colors) + # Use defined colors
            scale_x_continuous(labels = scales::percent_format(), expand = c(0, 0)) + # Percentage axis
            scale_y_discrete(expand = c(0, 0.5)) + # Add some padding for y-axis
            theme_minimal(base_size = 12) +
            labs(x = 'Relative Frequency', y = "Environment", fill = 'Envirotype') +
            theme(legend.position = "bottom",
                  strip.text = element_text(face = "bold"),
                  strip.background = element_rect(fill = "gray90", color = NA),
                  panel.spacing.y = unit(1, "lines")) # Add space between facets

          incProgress(1.0, detail = "Completed!")

          # Convert to plotly
          plotly::ggplotly(p, tooltip = c("y", "fill", "x")) |>
             plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = list('sendDataToCloud')) |>
             plotly::layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2)) # Adjust legend


        }, error = function(e) {
          # Handle errors during envirotyping
          output$dataenviro <- reactable::renderReactable(NULL) # Clear table on error
          plotly::plot_ly() |>
            plotly::layout(title = paste("Error generating envirotypes:", e$message))
        })
      }) # End withProgress
    })


    # Display cache status and clear button (Updated)
    output$cache_status_ui <- renderUI({
      # Use cache_service methods to get status
      mem_items <- cache_service$getMemoryCacheStatus()
      disk_exists <- cache_service$getDiskCacheStatus()

      if (mem_items > 0 || disk_exists) {
        div(
          style = "margin-top: 15px; padding-top: 10px; border-top: 1px solid #eee;",
          dropdown(
            inputId = ns("cache_dropdown"),
            label = "Cache Manager",
            icon = icon("database"),
            status = "info",
            size = "sm", # Slightly larger button
            right = TRUE,
            tooltip = tooltipOptions(title = "Manage weather data cache"),
            # Content of the dropdown
            tags$div(style = "padding: 10px;",
              tags$b("Cache Status:"),
              tags$p(paste("Items in memory:", mem_items)),
              tags$p(paste("Disk cache:", if(disk_exists) "Exists" else "Empty/Not Found")),
              actionButton(
                inputId = ns("clear_cache"),
                label = "Clear All Cache",
                icon = icon("trash"),
                class = "btn-danger btn-sm", # Small red button
                width = "100%"
              )
            )
          )
        )
      } else {
        # Optionally show a message if cache is empty
        # tags$p(style="margin-top: 15px; color: grey;", "Cache is empty.")
        NULL
      }
    })

    # Handle clear cache button (Updated)
    observeEvent(input$clear_cache, {
      result <- cache_service$cleanCache() # Use the service method
      if(result$memory_cleared && result$disk_cleared) {
         text_msg <- paste0("Memory cache cleared. Disk cache: ", result$files_removed, " files deleted.")
         type_msg <- "success"
      } else {
         text_msg <- "Failed to clear cache completely."
         type_msg <- "warning"
      }
      sendSweetAlert(
        session = session,
        title = "Cache Cleared",
        text = text_msg,
        type = type_msg
      )
      # Force UI update for cache status (re-render the UI element)
      output$cache_status_ui <- renderUI({}) # Trigger re-render
      output$cache_status_ui <- renderUI({
         # Re-render the UI using the service status methods
         mem_items <- cache_service$getMemoryCacheStatus()
         disk_exists <- cache_service$getDiskCacheStatus()
         if (mem_items > 0 || disk_exists) {
           div(
             style = "margin-top: 15px; padding-top: 10px; border-top: 1px solid #eee;",
             dropdown(
               inputId = ns("cache_dropdown"), label = "Cache Manager", icon = icon("database"), status = "info", size = "sm", right = TRUE,
               tooltip = tooltipOptions(title = "Manage weather data cache"),
               tags$div(style = "padding: 10px;",
                 tags$b("Cache Status:"),
                 tags$p(paste("Items in memory:", mem_items)),
                 tags$p(paste("Disk cache:", if(disk_exists) "Exists" else "Empty/Not Found")),
                 actionButton(inputId = ns("clear_cache"), label = "Clear All Cache", icon = icon("trash"), class = "btn-danger btn-sm", width = "100%")
               )
             )
           )
         } else { NULL }
      })
    })

    # --- Observers for Input Dependencies (GDD/CH) ---

    observe({
      chilling_enabled <- isTRUE(input$ch_w) || isTRUE(input$ch_utah) || isTRUE(input$ch_nc)
      scale_input <- input$scale

      if (chilling_enabled) {
        # If CH is on, scale MUST be hourly
        if (!is.null(scale_input) && scale_input != "hourly") {
          updatePrettyRadioButtons(session, "scale", selected = "hourly")
          showNotification("Scale automatically set to 'hourly' for chilling hours calculation.", type = "warning", duration = 5)
        }
        # Disable scale selection when CH is active
        shinyjs::disable("scale")
      } else {
        # Re-enable scale selection if no CH method is active
        shinyjs::enable("scale")
      }
    })
    observeEvent(input$computegdd, {
      if (!isTRUE(input$computegdd)) {
        # If GDD is turned off, turn off CH switches too
        updatePrettySwitch(session, "ch_w", value = FALSE)
        updatePrettySwitch(session, "ch_utah", value = FALSE)
        updatePrettySwitch(session, "ch_nc", value = FALSE)
      }
      # Note: We don't automatically enable CH when GDD is turned on.
    })
    observe({
      thermal_enabled <- isTRUE(input$computegdd) || isTRUE(input$ch_w) || isTRUE(input$ch_utah) || isTRUE(input$ch_nc)
      req(thermal_enabled) # Only run if thermal computations are enabled

      current_params <- input$params
      scale_input <- input$scale
      req(scale_input) # Need scale to determine requirements

      required_temp_params <- character(0)
      if (scale_input == "hourly") {
        required_temp_params <- "T2M"
      } else if (scale_input %in% c("daily", "monthly")) {
        required_temp_params <- c("T2M_MIN", "T2M_MAX")
      }

      # Check if required params are missing
      missing_params <- required_temp_params[!required_temp_params %in% current_params]

      if (length(missing_params) > 0) {
        # Add missing parameters to the selection
        new_selection <- unique(c(current_params, required_temp_params))
        updatePickerInput(session, "params", selected = new_selection)

        # Notify user
        showNotification(
          paste("Parameter(s)", paste(missing_params, collapse=", "),
                "automatically added as required for thermal calculations."),
          type = "message", # Changed from "info" to "message"
          duration = 7
        )
      }
    })

    # GDD Calculator Observer
    observeEvent(input$calculate_gdd, {
      # Check if we have data to perform the calculation
      climate_data <- resclimate()
      if (is.null(climate_data) || nrow(climate_data) == 0) {
        sendSweetAlert(
          session = session,
          title = "No Data Available",
          text = "Please fetch climate data first before calculating GDD.",
          type = "warning"
        )
        return()
      }

      # Verify required parameters are present
      required_cols <- NULL
      if ("T2M_MIN" %in% names(climate_data) && "T2M_MAX" %in% names(climate_data)) {
        required_cols <- c("T2M_MIN", "T2M_MAX")
      } else if ("T2M" %in% names(climate_data)) {
        required_cols <- "T2M"
      }

      if (is.null(required_cols)) {
        sendSweetAlert(
          session = session,
          title = "Missing Required Parameters",
          text = paste("GDD calculation requires either T2M_MIN & T2M_MAX for daily data or T2M for hourly data.",
                       "Please ensure these parameters are included in your query."),
          type = "error"
        )
        return()
      }

      # Prepare dataset for GDD calculation
      withProgress(message = "Calculating GDD...", {
        # Step 1: Create daily data if we're working with hourly data
        daily_data <- climate_data
        if ("HR" %in% names(climate_data) || "HOUR" %in% names(climate_data)) {
          incProgress(0.2, detail = "Aggregating hourly data to daily...")
          daily_data <- aggregate_hourly_data(climate_data)

          # Ensure Date column
          if (!"DATE" %in% names(daily_data)) {
            daily_data$DATE <- as.Date(as.character(daily_data$YYYYMMDD), format = "%Y%m%d")
          }
        } else {
          # Ensure DATE exists for daily data
          if (!"DATE" %in% names(daily_data) && "YYYYMMDD" %in% names(daily_data)) {
            daily_data$DATE <- as.Date(as.character(daily_data$YYYYMMDD), format = "%Y%m%d")
          }
        }

        # Step 2: Calculate GDD using the Ometto method
        incProgress(0.5, detail = "Computing growing degree days...")
        gdd_result <- gdd_ometto_frue(
          df = daily_data,
          Tbase = input$basemin,
          Tceil = input$baseupp,
          Topt1 = input$optimallower,
          Topt2 = input$optimalupper
        )

        # Step 3: Merge results back into main dataset
        incProgress(0.8, detail = "Updating results...")

        # Keep only original columns plus GDD columns to avoid duplicating common columns
        original_cols <- setdiff(names(climate_data), c("GDD", "FRUE", "GDD_CUMSUM"))
        gdd_cols <- c("GDD", "FRUE", "GDD_CUMSUM")

        # For hourly data, we need to join back to original hourly data
        if ("HR" %in% names(climate_data) || "HOUR" %in% names(climate_data)) {
          # Merge GDD data back with original hourly data
          # First prepare join columns - ensure YYYYMMDD exists in both
          if (!"YYYYMMDD" %in% names(climate_data)) {
            climate_data$YYYYMMDD <- format(climate_data$DATE, "%Y%m%d")
          }

          # Join by ENV and YYYYMMDD (date)
          merged_data <- dplyr::left_join(
            climate_data,
            gdd_result[, c("ENV", "YYYYMMDD", gdd_cols)],
            by = c("ENV", "YYYYMMDD")
          )
        } else {
          # For daily data, just update with GDD columns
          merged_data <- gdd_result
        }

        # Step 4: Update the reactive values with new data
        resclimate(merged_data)
        dfs[["weather"]] <- create_reactval("weather", merged_data)

        # Step 5: Notify user
        incProgress(1.0, detail = "Complete!")
      })

      sendSweetAlert(
        session = session,
        title = "GDD Calculation Complete",
        text = paste("Growing Degree Days calculated successfully using the Ometto method with:",
                     paste("Base temp:", input$basemin, "°C"),
                     paste("Ceiling temp:", input$baseupp, "°C"),
                     paste("Optimal range:", input$optimallower, "-", input$optimalupper, "°C"),
                     sep = "\n"),
        type = "success"
      )
    })

  }) # End moduleServer
}


