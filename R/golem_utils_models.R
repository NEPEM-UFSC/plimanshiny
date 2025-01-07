gof <- function(model, y) {
  residuals <- residuals(model)
  fitted_values <- fitted(model)
  n <- length(y)
  p <- length(coef(model))
  ss_res <- sum(residuals^2)
  aic <- n * log(ss_res / n) + 2 * p
  rmse <- sqrt(mean(residuals^2))
  list(
    AIC = aic,
    RMSE = rmse,
    MAE = mean(abs(residuals))
  )
}

get_data_info <- function(dfpars, index, what = "model") {
  dfpars |>
    dplyr::pull(parms) |>
    purrr::flatten() |>
    purrr::pluck(index) |>
    purrr::pluck(what)
}




######## LOGISTIC MODEL 3P ##########
modfun_L3 <- function(x, b0, b1, b2) {
  b0 / (1 + exp((b1-x)/b2))
}
# first derivative
fdfun_L3 <- function(x, b0, b1, b2) {
  # D(expression(b0 / (1 + exp((b1-x)/b2))), "x")
  b0 * (exp((b1 - x)/b2) * (1/b2))/(1 + exp((b1 - x)/b2))^2
}
# second derivative
sdfun_L3 <- function(x, b0, b1, b2) {
  # D(expression(b0 * (exp((b1 - x)/b2) * (1/b2))/(1 + exp((b1 - x)/b2))^2), "x")
  -(b0 * (exp((b1 - x)/b2) * (1/b2) * (1/b2))/(1 + exp((b1 - x)/b2))^2 -
      b0 * (exp((b1 - x)/b2) * (1/b2)) * (2 * (exp((b1 - x)/b2) *
                                                 (1/b2) * (1 + exp((b1 - x)/b2))))/((1 + exp((b1 - x)/b2))^2)^2)
}

mod_L3 <-  function(data, flight_date = "date", predictor = "median.NDVI", sowing_date = NULL, parallel = FALSE){
  dftemp <-
    data |>
    dplyr::mutate(unique_plot = paste0(block, "_", plot_id)) |>
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) |>
    dplyr::group_by(unique_plot) |>
    tidyr::nest()

  # Apply the model
  if(parallel){
    availablecl <- parallel::detectCores()
    future::plan(future::multisession, workers = round(availablecl * .75))
  } else{
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine=rbind) %dofut% {
    df <- as.data.frame(dftemp$data[[i]])

    if(!is.null(sowing_date)){
      flights <- as.POSIXlt(df$date)$yday + 1 -  (as.POSIXlt(sowing_date)$yday + 1)
    } else {
      flights <- as.POSIXlt(df$date)$yday + 1
    }
    fflight <- min(flights)
    lflight <- max(flights) + 20
    flights_seq <- fflight:lflight
    y <- df |> dplyr::pull()

    # Logistic regression to predict median.NDVI as a function of flights
    model <- try(nls(y ~ SSlogis(flights, Asym, xmid, scal),
                     control = nls.control(maxiter = 1000)), silent = TRUE)

    if (inherits(model, "try-error")) return(data.frame())

    coefslog <- coef(model)
    b0 <- coefslog[1]
    b1 <- coefslog[2]
    b2 <- coefslog[3]

    # Critical points
    inflec <- optimise(fdfun_L3, interval = c(fflight, lflight), b0 = b0, b1 = b1, b2 = b2, maximum = FALSE)
    cp1 <- optimise(sdfun_L3, interval = c(fflight, lflight), b0 = b0, b1 = b1, b2 = b2, maximum = FALSE)
    cp2 <- optimise(sdfun_L3, interval = c(fflight, lflight), b0 = b0, b1 = b1, b2 = b2, maximum = TRUE)

    xfd <- seq(min(flights), ceiling(inflec$minimum), length.out = 500)
    yfd <- fdfun_L3(xfd, b0, b1, b2)

    dfreg <- data.frame(x = c(min(xfd), max(xfd)), y = c(max(yfd), min(yfd)))
    regmod <- lm(y ~ x, data = dfreg)
    predline <- predict(regmod, newdata = data.frame(x = xfd))
    distances <- abs(yfd - predline)
    head <- xfd[which.max(abs(yfd - predline))]

    maturation <- cp2$maximum
    int1 <- integrate(modfun_L3, lower = fflight, upper = lflight, b0 = b0, b1 = b1, b2 = b2)
    int2 <- integrate(modfun_L3, lower = head, upper = cp2$maximum, b0 = b0, b1 = b1, b2 = b2)
    int3 <- integrate(modfun_L3, lower = fflight, upper = head, b0 = b0, b1 = b1, b2 = b2)

    dplyr::tibble(unique_plot = dftemp$unique_plot[i],
                  b0 = b0,
                  b1 = b1,
                  b2 = b2,
                  heading = head,
                  inflection = inflec$minimum,
                  maturity = maturation,
                  repr_period = cp2$maximum - head,
                  auc = int1$value,
                  auc_vege_period = int3$value,
                  auc_repr_period = int2$value,
                  parms = list(model = modfun_L3,
                               modeladj = model,
                               fd = fdfun_L3,
                               sd = sdfun_L3,
                               coefs = list(
                                 b0 = b0,
                                 b1 = b1,
                                 b2 = b2
                               ),
                               xmin = fflight,
                               xmax = lflight))
  }
  results <-
    results_list |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)

  return(results)
}
help_mod_L3_eq <- function(){
  "$$y = \\frac{b_0}{1 + \\exp((b_1 - x)/b_2)}$$"
}
help_mod_L3_fd <- function(){
  "$$y'(x) = b_0 \\cdot \\frac{\\exp\\left(\\frac{b_1 - x}{b_2}\\right) \\cdot \\frac{1}{b_2}}{\\left(1 + \\exp\\left(\\frac{b_1 - x}{b_2}\\right)\\right)^2}$$"
}
help_mod_L3_sd <- function(){
  "$$
    y''(x) = -\\left(
b_0 \\cdot \\frac{\\exp\\left(\\frac{b_1 - x}{b_2}\\right) \\cdot \\frac{1}{b_2} \\cdot \\frac{1}{b_2}}{\\left(1 + \\exp\\left(\\frac{b_1 - x}{b_2}\\right)\\right)^2} -
b_0 \\cdot \\frac{\\exp\\left(\\frac{b_1 - x}{b_2}\\right) \\cdot \\frac{1}{b_2} \\cdot \\left(2 \\cdot \\exp\\left(\\frac{b_1 - x}{b_2}\\right) \\cdot \\frac{1}{b_2} \\cdot \\left(1 + \\exp\\left(\\frac{b_1 - x}{b_2}\\right)\\right)\\right)}{\\left(\\left(1 + \\exp\\left(\\frac{b_1 - x}{b_2}\\right)\\right)^2\\right)^2}
\\right)
    $$"
}
help_mod_L3 <- function(){
  div(
    style = "font-family: Arial, sans-serif; line-height: 1.5;",
    h2(style = "color: #2E86C1;", "Description of Returned Variables"),
    tags$ul(
      tags$li(tags$b("unique_plot:"), " A unique identifier for each individual plot."),
      tags$li(tags$b("b0:"), " The maximum value of the response variable (asymptote)."),
      tags$li(tags$b("b1:"), " The x-value at the inflection point of the logistic curve."),
      tags$li(tags$b("b2:"), " The slope factor of the logistic model, determining the steepness of the curve at the inflection point."),
      tags$li(tags$b("heading:"), " The time point at which the heading measurement was taken."),
      tags$li(tags$b("inflection:"), " The x-value at the inflection point of the logistic curve, where the growth rate is maximal."),
      tags$li(tags$b("maturity:"), " The time point at which the plant is considered mature."),
      tags$li(tags$b("repr_period:"), " The duration of the reproductive period."),
      tags$li(tags$b("auc:"), " The total area under the curve (AUC), representing the total response accumulated over the entire observation period."),
      tags$li(tags$b("auc_vege_period:"), " The AUC calculated specifically for the vegetative period."),
      tags$li(tags$b("auc_repr_period:"), " The AUC calculated specifically for the reproductive period."),
      tags$li(tags$b("parms:"), " A list containing model parameters and functions:"),
    ),
    p("These variables provide comprehensive information about the dynamics of the logistic growth model. For example:"),
    tags$ul(
      tags$li(tags$b("Asymptote:"), tags$i(" b0"), " defines the upper bound of the response variable."),
      tags$li(tags$b("Inflection Point:"), " Variables like ", tags$i("inflection"), ", ", tags$i("xinfp"), ", and ", tags$i("yinfp"), " mark the stage of maximum growth rate."),
      tags$li(tags$b("Curvature Metrics:"), " ", tags$i("xmace"), ", ", tags$i("ymace"), ", ", tags$i("xmdes"), ", and ", tags$i("ymdes"), " describe the bending behavior of the curve and changes in concavity.")
    ),
    p("This information is essential for analyzing growth patterns, comparing treatments, and understanding biological dynamics."),
    h2(style = "color: #2E86C1;", "Growth Curve"),
    # Description
    p("The three-parameter logistic model (3PL) is a widely used mathematical function to describe sigmoidal curves, often observed in biological growth processes. It is a simplified version of the four-parameter logistic model."),
    p("The equation is given by:"),
    p(help_mod_L3_eq()),
    p(strong("Where:")),
    withMathJax(
      tags$ul(
        tags$li("\\(x\\): The time or independent variable."),
        tags$li("\\(b_0\\): The maximum value of the response variable (asymptote)."),
        tags$li("\\(b_1\\): The x-value at the inflection point."),
        tags$li("\\(b_2\\): The slope factor, determining the steepness of the curve at the inflection point.")
      )
    ),
    p(strong("Model description Features:")),
    tags$ul(
      tags$li("Models sigmoidal growth curves that start slow, accelerate, and then plateau."),
      tags$li("Represents maximum size and growth rate clearly."),
    ),
    h2(style = "color: #2E86C1;", "First Derivative"),
    # Description
    p("The first derivative of the three-parameter logistic model (3PL) describes the rate of change of the response variable at any given point. It is particularly useful for identifying the inflection point, where the rate of growth is maximal."),
    p("The first derivative is given by:"),
    p(help_mod_L3_fd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Captures the growth rate at any point on the curve."),
      tags$li("The maximum value of the derivative corresponds to the inflection point."),
      tags$li("Helps in understanding the dynamics of growth over time."),
      tags$li("Useful for comparing growth rates across different conditions or treatments.")
    ),
    p("This derivative is particularly useful for studying dynamic biological processes, such as plant height growth, where understanding the rate of change is crucial."),
    h2(style = "color: #2E86C1;", "Second Derivative"),
    # Description
    p("The second derivative of the three-parameter logistic model (3PL) provides insight into the curvature of the growth curve, helping to identify points of acceleration or deceleration in growth. This is crucial for understanding how growth dynamics change over time."),
    p("The second derivative is given by:"),
    p(help_mod_L3_sd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Describes how the rate of growth changes over time (acceleration or deceleration)."),
      tags$li("Helps identify regions of maximum acceleration or deceleration in growth."),
      tags$li("Provides insights into the dynamics of curvature for growth modeling."),
      tags$li("Useful for fine-tuning growth predictions and analyzing biological phenomena.")
    ),
    p("The second derivative is a powerful tool for understanding not just the growth rate but also the changes in growth dynamics over time. It is particularly useful in identifying critical points in biological systems, such as transitions from rapid growth to slower growth phases.")
  )
}





mod_L3_thresh <-  function(data, flight_date = "date", predictor = "median.NDVI", sowing_date = NULL, threshold,
                           parallel = FALSE){
  modfun <- function(x, b0, b1, b2) {
    b0 / (1 + exp((b1-x)/b2))
  }
  # first derivative
  fdfun <- function(x, b0, b1, b2) {
    # D(expression(b0 / (1 + exp((b1-x)/b2))), "x")
    b0 * (exp((b1 - x)/b2) * (1/b2))/(1 + exp((b1 - x)/b2))^2
  }

  # second derivative
  sdfun <- function(x, b0, b1, b2) {
    # D(expression(b0 * (exp((b1 - x)/b2) * (1/b2))/(1 + exp((b1 - x)/b2))^2), "x")
    -(b0 * (exp((b1 - x)/b2) * (1/b2) * (1/b2))/(1 + exp((b1 - x)/b2))^2 -
        b0 * (exp((b1 - x)/b2) * (1/b2)) * (2 * (exp((b1 - x)/b2) *
                                                   (1/b2) * (1 + exp((b1 - x)/b2))))/((1 + exp((b1 - x)/b2))^2)^2)
  }
  dftemp <-
    data |>
    dplyr::mutate(unique_plot = paste0(block, "_", plot_id)) |>
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) |>
    dplyr::group_by(unique_plot) |>
    tidyr::nest()

  # Apply the model
  if(parallel){
    availablecl <- parallel::detectCores()
    future::plan(future::multisession, workers = round(availablecl * .75))
  } else{
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine=rbind) %dofut% {
    df <- as.data.frame(dftemp$data[[i]])

    if(!is.null(sowing_date)){
      flights <- as.POSIXlt(df$date)$yday + 1 -  (as.POSIXlt(sowing_date)$yday + 1)
    } else {
      flights <- as.POSIXlt(df$date)$yday + 1
    }
    fflight <- min(flights)
    lflight <- max(flights) + 20
    flights_seq <- fflight:lflight
    y <- df |> dplyr::pull()

    # Logistic regression to predict median.NDVI as a function of flights
    model <- try(nls(y ~ SSlogis(flights, Asym, xmid, scal),
                     control = nls.control(maxiter = 1000)), silent = TRUE)

    if (inherits(model, "try-error")) return(data.frame())

    coefslog <- coef(model)
    b0 <- coefslog[1]
    b1 <- coefslog[2]
    b2 <- coefslog[3]

    # Critical points
    inflec <- optimise(fdfun, interval = c(fflight, lflight), b0 = b0, b1 = b1, b2 = b2, maximum = FALSE)
    cp1 <- optimise(sdfun, interval = c(fflight, lflight), b0 = b0, b1 = b1, b2 = b2, maximum = FALSE)
    cp2 <- optimise(sdfun, interval = c(fflight, lflight), b0 = b0, b1 = b1, b2 = b2, maximum = TRUE)

    xfd <- seq(min(flights), ceiling(inflec$minimum), length.out = 500)
    yfd <- fdfun(xfd, b0, b1, b2)

    dfreg <- data.frame(x = c(min(xfd), max(xfd)), y = c(max(yfd), min(yfd)))
    regmod <- lm(y ~ x, data = dfreg)
    predline <- predict(regmod, newdata = data.frame(x = xfd))
    distances <- abs(yfd - predline)
    head <- xfd[which.max(abs(yfd - predline))]

    maturation <- cp2$maximum
    int1 <- integrate(modfun, lower = fflight, upper = lflight, b0 = b0, b1 = b1, b2 = b2)
    int2 <- integrate(modfun, lower = head, upper = cp2$maximum, b0 = b0, b1 = b1, b2 = b2)
    int3 <- integrate(modfun, lower = fflight, upper = head, b0 = b0, b1 = b1, b2 = b2)

    dplyr::tibble(unique_plot = dftemp$unique_plot[i],
                  b0 = b0,
                  b1 = b1,
                  b2 = b2,
                  heading = head,
                  inflection = inflec$minimum,
                  maturity = maturation,
                  repr_period = cp2$maximum - head,
                  auc = int1$value,
                  auc_vege_period = int3$value,
                  auc_repr_period = int2$value,
                  parms = list(model = modfun,
                               modeladj = model,
                               fd = fdfun,
                               sd = sdfun,
                               coefs = list(
                                 b0 = b0,
                                 b1 = b1,
                                 b2 = b2
                               ),
                               xmin = fflight,
                               xmax = lflight))
  }
  results <-
    results_list |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)

  return(results)
}

########### LOGISTIC MODEL 4 PARAMETERS #############
# Logistic model L.4()
modfun_L4 <- function(x, b0, b1, b2, b3) {
  b1 + (b2 - b1) / (1 + exp(b0 * (x - b3)))
}

# Derivatives
fdfun_L4 <- function(x, b0, b1, b2, b3) {
  -((b2 - b1) * (exp(b0 * (x - b3)) * b0) / (1 + exp(b0 * (x - b3)))^2)
}

sdfun_L4 <- function(x, b0, b1, b2, b3) {
  -((b2 - b1) * (exp(b0 * (x - b3)) * b0 * b0) / (1 + exp(b0 * (x - b3)))^2 -
      (b2 - b1) * (exp(b0 * (x - b3)) * b0) * (2 * (exp(b0 * (x - b3)) * b0 * (1 + exp(b0 * (x - b3))))) /
      ((1 + exp(b0 * (x - b3)))^2)^2)
}

mod_L4 <- function(data,
                   flight_date = "date",
                   predictor = "median.NDVI",
                   sowing_date = NULL,
                   parallel = FALSE) {
  # Prepare data
  dftemp <-
    data  |>
    dplyr::mutate(unique_plot = paste0(block, "_", plot_id)) |>
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) |>
    dplyr::group_by(unique_plot) |>
    tidyr::nest()

  results_list <- list()

  # For loop to process each group

  # Apply the model
  if(parallel){
    availablecl <- parallel::detectCores()
    future::plan(future::multisession, workers = round(availablecl * .75))
  } else{
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`

  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine=rbind) %dofut% {
    df <- as.data.frame(dftemp$data[[i]])
    if (!is.null(sowing_date)) {
      flights <- as.POSIXlt(df$date)$yday + 1 - (as.POSIXlt(sowing_date)$yday + 1)
    } else {
      flights <- as.POSIXlt(df$date)$yday + 1
    }

    fflight <- min(flights)
    lflight <- max(flights) + 20
    flights_seq <- fflight:lflight
    y <- df |> dplyr::pull()

    model <- drc::drm(y ~ flights, fct = drc::L.4(), data = df)
    coefslog <- coef(model)
    b0 <- coefslog[1]
    b1 <- coefslog[2]
    b2 <- coefslog[3]
    b3 <- coefslog[4]

    # CRITICAL POINTS
    inflec <- optimise(fdfun_L4, interval = c(fflight, lflight), b0 = b0, b1 = b1, b2 = b2, b3 = b3, maximum = FALSE)

    cp1 <- optimise(sdfun_L4, interval = c(fflight, lflight), b0 = b0, b1 = b1, b2 = b2, b3 = b3, maximum = FALSE)
    cp2 <- optimise(sdfun_L4, interval = c(fflight, lflight), b0 = b0, b1 = b1, b2 = b2, b3 = b3, maximum = TRUE)

    xfd <- seq(min(flights), ceiling(cp1$minimum), length.out = 500)
    yfd <- sdfun_L4(xfd, b0, b1, b2, b3)

    dfreg <- data.frame(x = c(min(xfd), max(xfd)), y = c(max(yfd), min(yfd)))
    regmod <- lm(y ~ x, data = dfreg)
    predline <- predict(regmod, newdata = data.frame(x = xfd))
    distances <- abs(yfd - predline)
    head <- xfd[which.max(abs(yfd - predline))]

    xfd2 <- seq(ceiling(b3), lflight, length.out = 500)
    yfd2 <- fdfun_L4(xfd2, b0, b1, b2, b3)

    dfreg2 <- data.frame(x = c(min(xfd2), max(xfd2)), y = c(min(yfd2), max(yfd2)))
    regmod2 <- lm(y ~ x, data = dfreg2)
    predline2 <- predict(regmod2, newdata = data.frame(x = xfd2))
    maturation <- xfd2[which.max(abs(yfd2 - predline2))]

    int1 <- integrate(modfun_L4, lower = fflight, upper = lflight, b0 = b0, b1 = b1, b2 = b2, b3 = b3)
    int2 <- integrate(modfun_L4, lower = head, upper = maturation, b0 = b0, b1 = b1, b2 = b2, b3 = b3)

    tibble::tibble(
      unique_plot = dftemp$unique_plot[i],
      b0 = b0,
      b1 = b1,
      b2 = b2,
      inflection = b3,
      heading = head,
      maturity = maturation,
      repr_period = maturation - head,
      auc = int1$value,
      auc_repr_period = int2$value,
      parms = list(model = modfun_L4, modeladj = model, fd = fdfun_L4, sd = sdfun_L4,
                   coefs = list(b0 = b0, b1 = b1, b2 = b2, b3 = b3), xmin = fflight, xmax = lflight)
    )
  }

  results <-
    results_list |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)

  return(results)
}

help_mod_L4_eq <- function(){
  "$$y = b_1 + \\frac{b_2 - b_1}{1 + \\exp\\left(b_0 \\cdot (x - b_3)\\right)}$$"
}
help_mod_L4_fd <- function(){
  "$$y'(x) = -\\frac{(b_2 - b_1) \\cdot \\left(\\exp\\left(b_0 \\cdot (x - b_3)\\right) \\cdot b_0\\right)}{\\left(1 + \\exp\\left(b_0 \\cdot (x - b_3)\\right)\\right)^2}$$"
}
help_mod_L4_sd <- function(){
  "$$
   y''(x) = -\\left(
\\frac{(b_2 - b_1) \\cdot \\left(\\exp\\left(b_0 \\cdot (x - b_3)\\right) \\cdot b_0^2\\right)}{\\left(1 + \\exp\\left(b_0 \\cdot (x - b_3)\\right)\\right)^2} -
\\frac{(b_2 - b_1) \\cdot \\left(\\exp\\left(b_0 \\cdot (x - b_3)\\right) \\cdot b_0\\right) \\cdot
\\left(2 \\cdot \\exp\\left(b_0 \\cdot (x - b_3)\\right) \\cdot b_0 \\cdot \\left(1 + \\exp\\left(b_0 \\cdot (x - b_3)\\right)\\right)\\right)}{\\left(\\left(1 + \\exp\\left(b_0 \\cdot (x - b_3)\\right)\\right)^2\\right)^2}
\\right)
  $$"
}
help_mod_L4 <- function(){
  div(
    style = "font-family: Arial, sans-serif; line-height: 1.5;",
    h2(style = "color: #2E86C1;", "Description of Returned Variables"),
    tags$ul(
      tags$li(tags$b("block:"), " The identifier for the experimental block, typically used to group plots spatially or temporally."),
      tags$li(tags$b("plot_id:"), " The unique identifier for individual plots within a block."),
      tags$li(tags$b("unique_plot:"), " A combined identifier that uniquely identifies each plot across all blocks (e.g., combining ", tags$i("block"), " and ", tags$i("plot_id"), ")."),
      tags$li(tags$b("b0:"), " The slope factor of the logistic model, determining the steepness of the curve at the inflection point."),
      tags$li(tags$b("b1:"), " The lower asymptote of the logistic curve, representing the minimum value of the response variable."),
      tags$li(tags$b("b2:"), " The upper asymptote of the logistic curve, representing the maximum value of the response variable."),
      tags$li(tags$b("inflection:"), " The x-value at the inflection point of the logistic curve, where the growth rate is maximal."),
      tags$li(tags$b("auc:"), " The area under the curve (AUC), representing the total response accumulated over the range of the independent variable."),
      tags$li(tags$b("xinfp:"), " The x-value at the inflection point, providing another representation of the inflection coordinate."),
      tags$li(tags$b("yinfp:"), " The y-value at the inflection point, corresponding to the response variable's value at the inflection."),
      tags$li(tags$b("xmace:"), " The x-value where the second derivative is maximum, reflecting the maximum acceleration of growth rate."),
      tags$li(tags$b("ymace:"), " The y-value showing the response at the maximum acceleration point."),
      tags$li(tags$b("xmdes:"), " The x-value where the second derivative is minimal, reflecting the maximum deceleration of growth rate."),
      tags$li(tags$b("ymdes:"), " The y-value showing the response at the maximum deceleration point.")
    ),
    h2(style = "color: #2E86C1;", "Growth Curve"),
    # Description
    style = "font-family: Arial, sans-serif; line-height: 1.5;",
    p("The four-parameter logistic model (4PL) is widely used to describe biological growth processes, such as plant height, weight, or population dynamics. It captures the characteristic sigmoidal (S-shaped) growth pattern often observed in nature."),
    p("The equation is given by:"),
    p("$$y = b_1 + \\frac{(b_2 - b_1)}{1 + \\exp(b_0 \\cdot (x - b_3))}$$"),
    p(strong("Where:")),
    withMathJax(
      tags$ul(
        tags$li("\\(x\\): The time in days after sowing."),
        tags$li("\\(b_0\\): The growth rate (steepness of the curve)."),
        tags$li("\\(b_1\\): The lower asymptote (e.g., NDVI at the maturity)."),
        tags$li("\\(b_2\\): The upper asymptote (e.g., NDVI at vegetative stage)."),
        tags$li("\\(b_3\\): The inflection point (e.g.,  time at which the rate of change in NDVI is maximal).")
      )
    ),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Models sigmoidal growth curves that start slow, accelerate, and then plateau."),
      tags$li("Represents initial size, growth rate, and maximum size clearly."),
      tags$li("Commonly applied to plant growth studies, animal development, and population dynamics.")
    ),
    h2(style = "color: #2E86C1;", "First Derivative"),
    p("The first derivative of the four-parameter logistic model (4PL) describes the rate of change of the response variable (e.g., growth rate) at any given point. It is particularly useful for identifying the inflection point, where the rate of growth is maximal."),
    p("The first derivative is given by:"),
    p("$$
    y'(x) = -\\frac{(b_2 - b_1) \\cdot \\exp(b_0 \\cdot (x - b_3)) \\cdot b_0}{(1 + \\exp(b_0 \\cdot (x - b_3)))^2}
    $$"),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Captures the rate of change in vegetation index values at any point on the curve."),
      tags$li("The maximum value of the derivative corresponds to the inflection point, indicating the peak rate of vegetation growth."),
      tags$li("Helps in understanding the temporal dynamics of vegetation growth and greening."),
      tags$li("Useful for comparing vegetation growth rates across different conditions, treatments, or geographical regions."),
      p("This derivative is particularly useful for studying dynamic vegetation processes, such as the rapid greening phase, where understanding the rate of change in vegetation index values like NDVI is crucial."),
      h2(style = "color: #2E86C1;", "Second Derivative"),
      # Description
      style = "font-family: Arial, sans-serif; line-height: 1.5;",
      p("The second derivative of the vegetation index curve provides insight into the curvature of the growth trajectory, helping to identify points of acceleration or deceleration in greening or vegetation growth. This is crucial for understanding how vegetation dynamics change over time."),
      p("The second derivative is given by:"),
      p("$$
y''(x) = -\\frac{(b_2 - b_1) \\cdot \\exp(b_0 \\cdot (x - b_3)) \\cdot b_0^2}{(1 + \\exp(b_0 \\cdot (x - b_3)))^2} -
\\frac{(b_2 - b_1) \\cdot \\exp(b_0 \\cdot (x - b_3)) \\cdot b_0 \\cdot
\\big(2 \\cdot \\exp(b_0 \\cdot (x - b_3)) \\cdot b_0 \\cdot (1 + \\exp(b_0 \\cdot (x - b_3)))\\big)}
{(1 + \\exp(b_0 \\cdot (x - b_3)))^4}
$$"),
      p(strong("Key Features:")),
      tags$ul(
        tags$li("Describes how the rate of change in the vegetation index varies over time (acceleration or deceleration)."),
        tags$li("Helps identify regions of rapid greening (acceleration) or slower growth phases (deceleration)."),
        tags$li("Provides insights into the dynamics of vegetation growth and senescence for temporal modeling."),
        tags$li("Useful for refining predictions of vegetation phenology and analyzing ecological phenomena.")
      ),
      p("The second derivative is a powerful tool for understanding not just the growth rate of vegetation indexes but also the changes in growth dynamics over time. It is particularly useful in identifying critical phases in vegetation systems, such as the transition from rapid growth to stabilization or senescence.")
    )
  )
}

D(expression(  -((b2 - b1) * ((1 + exp(b0 * (x - b3)))^((b4 - 1) - 1) * ((b4 -
                                                                            1) * (exp(b0 * (x - b3)) * b0)) * (b4 * (exp(b0 * (x - b3)) *
                                                                                                                       b0)) + (1 + exp(b0 * (x - b3)))^(b4 - 1) * (b4 * (exp(b0 *
                                                                                                                                                                               (x - b3)) * b0 * b0)))/((1 + exp(b0 * (x - b3)))^b4)^2 -
                   (b2 - b1) * ((1 + exp(b0 * (x - b3)))^(b4 - 1) * (b4 * (exp(b0 *
                                                                                 (x - b3)) * b0))) * (2 * ((1 + exp(b0 * (x - b3)))^(b4 -
                                                                                                                                       1) * (b4 * (exp(b0 * (x - b3)) * b0)) * ((1 + exp(b0 *
                                                                                                                                                                                           (x - b3)))^b4)))/(((1 + exp(b0 * (x - b3)))^b4)^2)^2)), "x")

############### Logistic model 5 PARAMETERS #########
modfun_L5 <- function(x, b0, b1, b2, b3, b4) {
  b1 + (b2 - b1) / (1 + exp(b0 * (x - b3)))^b4
}
fdfun_L5 <- function(x, b0, b1, b2, b3, b4){
  -((b2 - b1) * ((1 + exp(b0 * (x - b3)))^(b4 - 1) * (b4 * (exp(b0 *
                                                                  (x - b3)) * b0)))/((1 + exp(b0 * (x - b3)))^b4)^2)
}
sdfun_L5 <- function(x, b0, b1, b2, b3, b4){
  -((b2 - b1) * ((1 + exp(b0 * (x - b3)))^((b4 - 1) - 1) * ((b4 -
                                                               1) * (exp(b0 * (x - b3)) * b0)) * (b4 * (exp(b0 * (x - b3)) *
                                                                                                          b0)) + (1 + exp(b0 * (x - b3)))^(b4 - 1) * (b4 * (exp(b0 *
                                                                                                                                                                  (x - b3)) * b0 * b0)))/((1 + exp(b0 * (x - b3)))^b4)^2 -
      (b2 - b1) * ((1 + exp(b0 * (x - b3)))^(b4 - 1) * (b4 * (exp(b0 *
                                                                    (x - b3)) * b0))) * (2 * ((1 + exp(b0 * (x - b3)))^(b4 -
                                                                                                                          1) * (b4 * (exp(b0 * (x - b3)) * b0)) * ((1 + exp(b0 *
                                                                                                                                                                              (x - b3)))^b4)))/(((1 + exp(b0 * (x - b3)))^b4)^2)^2)
}
mod_L5 <-  function(data,
                    flight_date = "date",
                    predictor = "median.NDVI",
                    sowing_date = NULL,
                    parallel = FALSE){



  dftemp <-
    data |>
    dplyr::mutate(unique_plot = paste0(block, "_", plot_id)) |>
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) |>
    dplyr::group_by(unique_plot) |>
    tidyr::nest()


  # Apply the model
  if(parallel){
    availablecl <- parallel::detectCores()
    future::plan(future::multisession, workers = round(availablecl * .75))
  } else{
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine=rbind) %dofut% {
    df <- as.data.frame(dftemp$data[[i]])
    if(!is.null(sowing_date)){
      flights <- as.POSIXlt(df$date)$yday + 1 -  (as.POSIXlt(sowing_date)$yday + 1)
    } else {
      flights <- as.POSIXlt(df$date)$yday + 1
    }
    fflight <- min(flights)
    lflight <- max(flights) + 20
    flights_seq <- fflight:lflight
    y <- df |> dplyr::pull()

    model <- drc::drm(y ~ flights, fct = drc::L.5(), data = df)
    coefslog <- coef(model)
    b0 <- coefslog[1]
    b1 <- coefslog[2]
    b2 <- coefslog[3]
    b3 <- coefslog[4]
    b4 <- coefslog[5]

    # CRITICAL POINTS
    inflec <-
      optimise(fdfun_L5,
               interval = c(fflight, lflight),
               b0 = b0,
               b1 = b1,
               b2 = b2,
               b3 = b3,
               b4 = b4,
               maximum = FALSE)

    # # maturation, estimated as the maximum point of the second derivative
    cp1 <-
      optimise(sdfun_L5,
               interval = c(fflight, lflight),
               b0 = b0,
               b1 = b1,
               b2 = b2,
               b3 = b3,
               b4 = b4,
               maximum = FALSE)
    cp2 <-
      optimise(sdfun_L5,
               interval = c(fflight, lflight),
               b0 = b0,
               b1 = b1,
               b2 = b2,
               b3 = b3,
               b4 = b4,
               maximum = TRUE)
    # Heading
    xfd <- seq(min(flights), ceiling(cp1$minimum),  length.out = 500)
    yfd <- sdfun_L5(xfd, b0, b1, b2, b3, b4)
    # Heading, estimated by the maximum curvature of the second derivative
    # from the first flight to the inflection point

    # linear decreasing
    dfreg <- data.frame(x = c(min(xfd), max(xfd)),
                        y = c(max(yfd), min(yfd)))

    regmod <- lm(y ~ x, data = dfreg)
    coefsr <- coef(regmod)
    predline <- predict(regmod, newdata = data.frame(x = xfd))
    distances <- abs(yfd - predline)
    head <- xfd[which.max(abs(yfd - predline))]

    # Maturation
    xfd2 <- seq(inflec$minimum, lflight,  length.out = 500)
    yfd2 <- fdfun_L5(xfd2, b0, b1, b2, b3, b4)
    # Heading, estimated by the maximum curvature of the second derivative
    # from the first flight to the inflection point

    # linear decreasing
    dfreg2 <- data.frame(x = c(min(xfd2), max(xfd2)),
                         y = c(min(yfd2), max(yfd2)))

    regmod2 <- lm(y ~ x, data = dfreg2)
    # coefsr <- coef(regmod2)
    predline2 <- predict(regmod2, newdata = data.frame(x = xfd2))
    maturation <- xfd2[which.max(abs(yfd2 - predline2))]
    # integrate median.NDVI below the curve
    int1 <- integrate(modfun_L5, lower = fflight, upper = lflight, b0 = b0, b1 = b1, b2 = b2, b3 = b3, b4 = b4)
    int2 <- integrate(modfun_L5, lower = head, upper = maturation, b0 = b0, b1 = b1, b2 = b2, b3 = b3, b4 = b4)


    tibble::tibble(unique_plot = dftemp$unique_plot[i],
                   b0 = b0,
                   b1 = b1,
                   b2 = b2,
                   b3 = b3,
                   b4 = b4,
                   inflection = inflec$minimum,
                   heading = head,
                   maturity = maturation,
                   repr_period = maturation - head,
                   auc = int1$value,
                   auc_repr_period = int2$value,
                   parms = list(model = modfun_L5,
                                modeladj = model,
                                fd = fdfun_L5,
                                sd = sdfun_L5,
                                coefs = list(
                                  b0 = b0,
                                  b1 = b1,
                                  b2 = b2,
                                  b3 = b3,
                                  b4 = b4
                                ),
                                xmin = fflight,
                                xmax = lflight))
  }
  results <-
    results_list |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)
  return(results)
}


help_mod_L5_eq <- function(){
  "$$y = b_1 + \\frac{b_2 - b_1}{\\left(1 + \\exp\\left(b_0 \\cdot (x - b_3)\\right)\\right)^{b_4}}$$"
}
help_mod_L5_fd <- function(){
  "$$
  y'(x) = -\\frac{(b_2 - b_1) \\cdot \\left(\\left(1 + \\exp\\left(b_0 \\cdot (x - b_3)\\right)\\right)^{b_4 - 1} \\cdot \\left(b_4 \\cdot \\exp\\left(b_0 \\cdot (x - b_3)\\right) \\cdot b_0\\right)\\right)}{\\left(\\left(1 + \\exp\\left(b_0 \\cdot (x - b_3)\\right)\\right)^{b_4}\\right)^2}
  $$"
}
help_mod_L5_sd <- function(){
  "$$
  y''(x) = -\\left(
\\frac{
(b_2 - b_1) \\cdot
\\left[
\\left(1 + \\exp\\left(b_0 \\cdot (x - b_3)\\right)\\right)^{(b_4 - 1) - 1} \\cdot
\\left((b_4 - 1) \\cdot \\exp\\left(b_0 \\cdot (x - b_3)\\right) \\cdot b_0\\right) \\cdot
\\left(b_4 \\cdot \\exp\\left(b_0 \\cdot (x - b_3)\\right) \\cdot b_0\\right) +
\\left(1 + \\exp\\left(b_0 \\cdot (x - b_3)\\right)\\right)^{b_4 - 1} \\cdot
\\left(b_4 \\cdot \\exp\\left(b_0 \\cdot (x - b_3)\\right) \\cdot b_0^2\\right)
\\right]
}
{\\left(\\left(1 + \\exp\\left(b_0 \\cdot (x - b_3)\\right)\\right)^{b_4}\\right)^2} -
\\frac{
(b_2 - b_1) \\cdot
\\left[
\\left(1 + \\exp\\left(b_0 \\cdot (x - b_3)\\right)\\right)^{b_4 - 1} \\cdot
\\left(b_4 \\cdot \\exp\\left(b_0 \\cdot (x - b_3)\\right) \\cdot b_0\\right)
\\right] \\cdot
\\left[
2 \\cdot
\\left(1 + \\exp\\left(b_0 \\cdot (x - b_3)\\right)\\right)^{b_4 - 1} \\cdot
\\left(b_4 \\cdot \\exp\\left(b_0 \\cdot (x - b_3)\\right) \\cdot b_0\\right) \\cdot
\\left(1 + \\exp\\left(b_0 \\cdot (x - b_3)\\right)\\right)^{b_4}
\\right]
}
{\\left(\\left(\\left(1 + \\exp\\left(b_0 \\cdot (x - b_3)\\right)\\right)^{b_4}\\right)^2\\right)^2}
\\right)
  $$"
}

help_mod_L5 <- function(){
  div(
    style = "font-family: Arial, sans-serif; line-height: 1.5;",
    h2(style = "color: #2E86C1;", "Description of Returned Variables"),
    tags$ul(
      tags$li(tags$b("unique_plot:"), " A unique identifier for each individual plot."),
      tags$li(tags$b("b0:"), " The slope factor of the logistic model."),
      tags$li(tags$b("b1:"), " The lower asymptote of the logistic curve."),
      tags$li(tags$b("b2:"), " The upper asymptote of the logistic curve."),
      tags$li(tags$b("b3:"), " The x-value at the inflection point."),
      tags$li(tags$b("b4:"), " The Hill slope, influencing the steepness of the curve."),
      tags$li(tags$b("inflection:"), " The x-value at the inflection point of the logistic curve, where the growth rate is maximal."),
      tags$li(tags$b("auc:"), " The area under the curve (AUC), representing the total response accumulated over the range of the independent variable.")
    ),
    p("These variables provide comprehensive information about the dynamics of the logistic growth model. For example:"),
    tags$ul(
      tags$li(tags$b("Asymptotes:"), tags$i(" b1"), " and ", tags$i("b2"), " define the lower and upper bounds of the response variable."),
      tags$li(tags$b("Inflection Point:"), " Variables like ", tags$i("inflection"), " mark the stage of maximum growth rate."),
      tags$li(tags$b("Curve Steepness:"), " The ", tags$i("b4"), " parameter (Hill slope) controls the steepness of the curve around the inflection point.")
    ),
    p("This information is essential for analyzing growth patterns, comparing treatments, and understanding biological dynamics."),
    h2(style = "color: #2E86C1;", "Growth Curve"),
    # Description
    p("The five-parameter logistic model (5PL) is a flexible mathematical function used to describe sigmoidal curves, often encountered in biological and pharmacological contexts."),
    p("The equation is given by:"),
    p("$$y = b_1 + \\frac{(b_2 - b_1)}{(1 + \\exp(b_0 * (x - b_3)))^{b_4}}$$"),
    p(strong("Where:")),
    withMathJax(
      tags$ul(
        tags$li("\\(x\\): The time or independent variable."),
        tags$li("\\(b_0\\): The slope factor."),
        tags$li("\\(b_1\\): The lower asymptote."),
        tags$li("\\(b_2\\): The upper asymptote."),
        tags$li("\\(b_3\\): The x-value at the inflection point."),
        tags$li("\\(b_4\\): The Hill slope.")
      )
    ),
    p(strong("Model Description Features:")),
    tags$ul(
      tags$li("Highly flexible model that can capture a wide range of sigmoidal shapes."),
      tags$li("Allows for more precise fitting of data compared to simpler logistic models."),
      tags$li("Widely used in areas like drug discovery and pharmacokinetics."),
      tags$li("Requires careful parameter estimation due to the increased complexity.")
    ),
    h2(style = "color: #2E86C1;", "First Derivative"),
    # Description
    p("The first derivative of the five-parameter logistic model (5PL) describes the rate of change of the response variable at any given point. It is useful for identifying the inflection point and understanding the dynamics of the growth or response curve."),
    p("The first derivative is given by:"),
    p("$$
      y'(x) = -\\frac{(b_2 - b_1) * ((1 + \\exp(b_0 * (x - b_3)))^{(b_4 - 1)} * (b_4 * (\\exp(b_0 * (x - b_3))) * b_0))}{((1 + \\exp(b_0 * (x - b_3)))^{b_4})^2}
      $$"),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Captures the growth rate at any point on the curve."),
      tags$li("The maximum value of the derivative corresponds to the inflection point."),
      tags$li("Helps in understanding the dynamics of growth over time."),
      tags$li("Useful for comparing growth rates across different conditions or treatments.")
    ),
    h2(style = "color: #2E86C1;", "Second Derivative"),
    # Description
    p("The second derivative of the five-parameter logistic model (5PL) provides information about the curvature of the growth curve, indicating regions of acceleration or deceleration in the response."),
    p("The second derivative is given by:"),
    p("$$
      y''(x) = -\\frac{(b_2 - b_1) * ((1 + \\exp(b_0 * (x - b_3)))^((b_4 - 1) - 1) * ((b_4 - 1) * (\\exp(b_0 * (x - b_3))) * b_0) * (b_4 * (\\exp(b_0 * (x - b_3))) * b_0) + (1 + \\exp(b_0 * (x - b_3)))^{(b_4 - 1)} * (b_4 * (\\exp(b_0 * (x - b_3))) * b_0 * b_0))}{((1 + \\exp(b_0 * (x - b_3)))^{b_4})^2} -
      \\frac{(b_2 - b_1) * ((1 + \\exp(b_0 * (x - b_3)))^{(b_4 - 1)} * (b_4 * (\\exp(b_0 * (x - b_3))) * b_0)) * (2 * ((1 + \\exp(b_0 * (x - b_3)))^{(b_4 - 1)} * (b_4 * (\\exp(b_0 * (x - b_3))) * b_0) * ((1 + \\exp(b_0 * (x - b_3)))^{b_4})))}{(((1 + \\exp(b_0 * (x - b_3)))^{b_4})^2)^2}
      $$"),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Describes how the rate of growth changes over time (acceleration or deceleration)."),
      tags$li("Helps identify regions of maximum acceleration or deceleration in growth."),
      tags$li("Provides insights into the dynamics of curvature for growth modeling."),
      tags$li("Useful for fine-tuning growth predictions and analyzing biological phenomena.")
    )
  )
}





# Threshold-based methods
########## loess model ############

mod_loess <-  function(data,
                       flight_date = "date",
                       predictor = "median.NDVI",
                       sowing_date = NULL,
                       threshold,
                       parallel = FALSE,
                       span = 0.75){
  dftemp <-
    data |>
    dplyr::mutate(unique_plot = paste0(block, "_", plot_id)) |>
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) |>
    dplyr::group_by(unique_plot) |>
    tidyr::nest()

  # Apply the model
  if(parallel){
    availablecl <- parallel::detectCores()
    future::plan(future::multisession, workers = round(availablecl * .75))
  } else{
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine=rbind) %dofut% {
    df <- as.data.frame(dftemp$data[[i]])
    if(!is.null(sowing_date)){
      flights <- as.POSIXlt(df$date)$yday + 1 -  (as.POSIXlt(sowing_date)$yday + 1)
    } else {
      flights <- as.POSIXlt(df$date)$yday + 1
    }
    fflight <- min(flights)
    lflight <- max(flights) + 20
    flights_seq <- fflight:lflight
    y <- df |> dplyr::pull()
    # extract the date of maturity
    model <- loess(y ~ flights, span = span)

    # Maturation
    fitted.nge_loop <- predict(model, flights_seq)
    list_date_pred <- approx(fitted.nge_loop, flights_seq, xout = threshold)

    tibble::tibble(unique_plot = dftemp$unique_plot[i],
                   maturity = list_date_pred$y,
                   threshold = threshold,
                   parms = list(modeladj = model,
                                xmin = fflight,
                                xmax = lflight))
  }

  results <-
    results_list |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)
  return(results)
}


help_mod_loess <- function(){
  div(
    style = "font-family: Arial, sans-serif; line-height: 1.5;",
    h2(style = "color: #2E86C1;", "Description of Returned Variables"),
    tags$ul(
      tags$li(tags$b("unique_plot:"), " A unique identifier for each individual plot."),
      tags$li(tags$b("maturity:"), " The predicted maturity date based on the LOESS model."),
      tags$li(tags$b("threshold:"), " The threshold value used to determine the maturity date."),
      tags$li(tags$b("parms:"), " A list containing model parameters and adjustments, including the LOESS model and the range of observations (xmin, xmax)."),
      tags$li(tags$b("block:"), " The block identifier extracted from the unique plot identifier."),
      tags$li(tags$b("plot_id:"), " The plot identifier extracted from the unique plot identifier.")
    ),
    p("These variables provide comprehensive information about the dynamics of the LOESS-based growth model. For example:"),
    tags$ul(
      tags$li(tags$b("Maturity:"), " The predicted date of maturity calculated based on the threshold."),
      tags$li(tags$b("Threshold:"), " The NDVI value or other predictor used to identify the maturity date.")
    ),
    h2(style = "color: #2E86C1;", "LOESS Growth Curve"),
    p("The LOESS (Locally Estimated Scatterplot Smoothing) model is a non-parametric regression method used to model sigmoidal growth curves. It is particularly suitable for scenarios where the data does not follow a strict parametric form, allowing flexible fitting to complex patterns."),
    p("The model is applied to predict growth dynamics, such as plant development stages, using predictors like NDVI over time."),
    p(strong("Model description Features:")),
    tags$ul(
      tags$li("Adapts to data with flexible smoothing."),
      tags$li("Can handle varying growth trajectories."),
      tags$li("Provides robust predictions of key biological events, such as maturity.")
    ),
    h2(style = "color: #2E86C1;", "Threshold-based Analysis"),
    p("The LOESS-based model uses a threshold value to identify critical points in the growth trajectory, such as the date when a certain NDVI value is reached."),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Allows precise determination of biological events like maturity."),
      tags$li("Can incorporate sowing dates to align observations with specific phases of the growth cycle."),
      tags$li("Supports flexible modeling by adjusting parameters such as the LOESS span.")
    ),
    h2(style = "color: #2E86C1;", "References"),
    p("Volpato, L., A. Dobbels, A. Borem, and A.J. Lorenz. 2021. Optimization of temporal UAS-based imagery analysis to estimate plant maturity date for soybean breeding. The Plant Phenome Journal 4(1): e20018. doi: ",
      a(href = "https://onlinelibrary.wiley.com/doi/abs/10.1002/ppj2.20018",target = "_blank", "10.1002/ppj2.20018"))
  )
}



############## SEGMENTED MODEL ################
mod_segmented <-  function(data,
                           flight_date = "date",
                           predictor = "median.NDVI",
                           sowing_date = NULL,
                           threshold,
                           slope = "min",
                           parallel = FALSE){
  dftemp <-
    data |>
    dplyr::mutate(unique_plot = paste0(block, "_", plot_id)) |>
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) |>
    dplyr::group_by(unique_plot) |>
    tidyr::nest()

  # Apply the model
  if(parallel){
    availablecl <- parallel::detectCores()
    future::plan(future::multisession, workers = round(availablecl * .75))
  } else{
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine=rbind) %dofut% {
    df <- as.data.frame(dftemp$data[[i]])
    if(!is.null(sowing_date)){
      flights <- as.POSIXlt(df$date)$yday + 1 -  (as.POSIXlt(sowing_date)$yday + 1)
    } else {
      flights <- as.POSIXlt(df$date)$yday + 1
    }
    fflight <- min(flights)
    lflight <- max(flights) + 20
    flights_seq <- fflight:lflight
    y <- df |> dplyr::pull()

    # create linear model
    mod<-lm(y ~ flights)
    # set attempts to 0
    attempts = 0
    # set if.false to false
    if.false <- F
    # while if.false is false
    while(if.false == F){
      attempts <- attempts + 1
      #if the number of rows in the data is greater than 7 and the number of attempts is less than 100, then run the segmented function
      if(nrow(df) > 7 && attempts < 100){
        #run the segmented function with the following parameters
        seg_loop<-try(segmented::segmented(mod,
                                           seg.Z = ~ flights,
                                           npsi = 2,
                                           control = segmented::seg.control(n.boot = 50, random=T, tol=0.01)),
                      silent = T)

        #if the segmented function returns an error, then run the lm function
        if("try-error" %in% class(seg_loop)) {
          #run the lm function with the following parameters
          seg_loop<-lm(y ~ flights)
          #create a variable called slps that is equal to the second coefficient of the lm function
          slps <- (seg_loop$coefficients)[2]
          #create a variable called ncpt that is equal to the first coefficient of the lm function
          ncpt <- (seg_loop$coefficients)[1]
          #create a variable called DPM that is equal to the difference between the thresholdold and the intercept divided by the slope
          DPM <- (threshold - ncpt) / slps

          #if the segmented function does not return an error and the psi variable is not null, then run the following code
        } else if (!is.null(seg_loop$psi)) {

          #create a variable called slps that is equal to the slope of the segmented function
          slps <- segmented::slope(seg_loop)$flights
          #create a variable called ncpt that is equal to the intercept of the segmented function
          ncpt <- segmented::intercept(seg_loop)$flights

          #if the vegetation index is GLI or TGI, then set the slope variable equal to the minimum slope
          if (slope == "min") {
            slope <- min(slps[,1])
            #if the vegetation index is HI, then set the slope variable equal to the maximum slope
          } else {
            slope <- max(slps[,1])
            #if the vegetation index is not GLI, TGI, or HI, then print an error message
          }
          #create a variable called slope_interc that is equal to the index of the slope variable
          slope_interc <- which(slps[,1] == slope)
          #create a variable called B1_interc that is equal to the intercept at the slope_interc index
          B1_interc <- ncpt[slope_interc,1]

          #create a variable called DPM that is equal to the difference between the thresholdold and the intercept divided by the slope
          DPM <- (threshold - B1_interc) / slope
          #if the segmented function does not return an error and the psi variable is null, then run the following code
        } else {
          #run the lm function with the following parameters
          seg_loop<-lm(y ~ flights)
          #create a variable called slps that is equal to the second coefficient of the lm function
          slps <- (seg_loop$coefficients)[2]
          #create a variable called ncpt that is equal to the first coefficient of the lm function
          ncpt <- (seg_loop$coefficients)[1]
          #create a variable called DPM that is equal to the difference between the thresholdold and the intercept divided by the slope
          DPM <- (threshold - ncpt) / slps

        }

      } else {
        seg_loop<-try(segmented::segmented(mod,
                                           seg.Z = ~ flights,
                                           npsi = 1,
                                           control = segmented::seg.control(n.boot = 50, random=T, tol=0.01)),
                      silent = T) # try to run the segmented function

        if("try-error" %in% class(seg_loop)) { # if the segmented function fails, run a linear model
          seg_loop<-lm(y ~ flights) # run a linear model
          slps <- (seg_loop$coefficients)[2] # get the slope of the linear model
          ncpt <- (seg_loop$coefficients)[1] # get the intercept of the linear model
          DPM <- (threshold - ncpt) / slps # calculate the DPM

        } else if (!is.null(seg_loop$psi)) {

          slps <- segmented::slope(seg_loop)$flights
          ncpt <- segmented::intercept(seg_loop)$flights

          if (slope == "min") {
            slope <- min(slps[,1])
            #if the vegetation index is HI, then set the slope variable equal to the maximum slope
          } else {
            slope <- max(slps[,1])
            #if the vegetation index is not GLI, TGI, or HI, then print an error message
          }
          slope_interc <- which(slps[,1] == slope)
          B1_interc <- ncpt[slope_interc,1]

          DPM <- (threshold - B1_interc) / slope

        } else {
          seg_loop<-lm(y ~ flights)
          slps <- (seg_loop$coefficients)[2]
          ncpt <- (seg_loop$coefficients)[1]
          DPM <- (threshold - ncpt) / slps
        }
      }
      if.false <- T
    }


    tibble::tibble(unique_plot = dftemp$unique_plot[i],
                   maturity = as.numeric(DPM),
                   threshold = threshold,
                   parms = list(modeladj = seg_loop))
  }
  results <-
    results_list |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)
  return(results)
}



help_mod_segmented <- function(){
  div(
    style = "font-family: Arial, sans-serif; line-height: 1.5;",
    h2(style = "color: #2E86C1;", "Description of Returned Variables"),
    tags$ul(
      tags$li(tags$b("unique_plot:"), " A unique identifier for each individual plot."),
      tags$li(tags$b("maturity:"), " The predicted maturity date based on the segmented regression model."),
      tags$li(tags$b("threshold:"), " The threshold value used to determine the maturity date."),
      tags$li(tags$b("parms:"), " A list containing model parameters and adjustments, including segmented model objects and other relevant details."),
      tags$li(tags$b("block:"), " The block identifier extracted from the unique plot identifier."),
      tags$li(tags$b("plot_id:"), " The plot identifier extracted from the unique plot identifier.")
    ),
    p("These variables provide comprehensive information about the dynamics of the segmented regression growth model. For example:"),
    tags$ul(
      tags$li(tags$b("Maturity:"), " The predicted date of maturity calculated based on the threshold."),
      tags$li(tags$b("Threshold:"), " The NDVI value or other predictor used to identify the maturity date.")
    ),
    h2(style = "color: #2E86C1;", "Segmented Growth Curve"),
    p("The segmented regression model is a piecewise linear regression approach used to model changes in growth trajectories. It is particularly suitable for identifying inflection points and capturing complex growth patterns. The model is applied to predict growth dynamics, such as plant development stages, using predictors like NDVI over time."),
    p(strong("Model description Features:")),
    tags$ul(
      tags$li("Captures inflection points to provide insights into critical growth transitions."),
      tags$li("Handles varying sample sizes and complex growth trajectories."),
      tags$li("Provides robust predictions of key biological events, such as maturity.")
    ),
    h2(style = "color: #2E86C1;", "Threshold-based Analysis"),
    p("The segmented regression model uses a threshold value to identify critical points in the growth trajectory, such as the date when a certain NDVI value is reached."),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Allows precise determination of biological events like maturity."),
      tags$li("Can incorporate sowing dates to align observations with specific phases of the growth cycle."),
      tags$li("Supports flexible modeling by adjusting parameters such as the slope selection.")
    ),
    h2(style = "color: #2E86C1;", "Parallel Processing"),
    p("To optimize performance, the model supports parallel processing, enabling faster computation across multiple plots."),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Automatically detects available cores for parallel execution."),
      tags$li("Can be toggled on or off using the 'parallel' argument."),
      tags$li("Ensures sequential execution as a fallback or default option.")
    ),
    p("This implementation is ideal for large datasets, ensuring efficient and scalable analysis of growth patterns."),
    h2(style = "color: #2E86C1;", "References"),
    p("Volpato, L., A. Dobbels, A. Borem, and A.J. Lorenz. 2021. Optimization of temporal UAS-based imagery analysis to estimate plant maturity date for soybean breeding. The Plant Phenome Journal 4(1): e20018. doi: ",
      a(href = "https://onlinelibrary.wiley.com/doi/abs/10.1002/ppj2.20018",target = "_blank", "10.1002/ppj2.20018"))
  )
}






########### GROWTH MODELS ##########
# Define the Weibull Growth Model
modfun_weibull <- function(x, Asym, Drop, lrc, pwr) {
  Asym - Drop * exp(-exp(lrc) * x^pwr)
}

fdfun_weibull <- function(x, Asym, Drop, lrc, pwr) {
  Drop * (exp(-exp(lrc) * x^pwr) * (exp(lrc) * (x^(pwr - 1) * pwr)))
}

sdfun_weibull <- function(x, Asym, Drop, lrc, pwr) {
  Drop * (exp(-exp(lrc) * x^pwr) * (exp(lrc) * (x^((pwr - 1) -
                                                     1) * (pwr - 1) * pwr)) - exp(-exp(lrc) * x^pwr) * (exp(lrc) *
                                                                                                          (x^(pwr - 1) * pwr)) * (exp(lrc) * (x^(pwr - 1) * pwr)))
}

mod_weibull <- function(data,
                        flight_date = "date",
                        predictor = "median.NDVI",
                        sowing_date = NULL,
                        parallel = FALSE) {
  # Prepare data
  dftemp <-
    data |>
    dplyr::mutate(unique_plot = paste0(block, "_", plot_id)) |>
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) |>
    dplyr::group_by(unique_plot) |>
    tidyr::nest()

  # Parallel or Sequential Plan
  if (parallel) {
    availablecl <- parallel::detectCores()
    future::plan(future::multisession, workers = round(availablecl * 0.75))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`

  # Fit model for each group
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows) %dofut% {
    df <- as.data.frame(dftemp$data[[i]])
    if (!is.null(sowing_date)) {
      flights <- as.POSIXlt(df$date)$yday + 1 - (as.POSIXlt(sowing_date)$yday)
    } else {
      flights <- as.POSIXlt(df$date)$yday + 1
    }

    fflight <- min(flights)
    lflight <- max(flights) + 20
    flights_seq <- fflight:lflight
    y <- df |> dplyr::pull(!!rlang::sym(predictor))
    x <- flights + 1

    result <- tryCatch({
      # Fit Weibull model
      model <- nls(y ~ SSweibull(x, Asym, Drop, lrc, pwr),
                   control = nls.control(maxiter = 1000),
                   data = data.frame(x, y))

      coefs <- coef(model)
      gofval <- gof(model, y)

      # Area under curve
      int1 <- integrate(modfun_weibull,
                        lower = fflight,
                        upper = lflight,
                        Asym = coefs[[1]],
                        Drop = coefs[[2]],
                        lrc = coefs[[3]],
                        pwr = coefs[[4]])

      # critical points
      # inflection point
      fdopt_result <-
        optimize(fdfun_weibull,
                 Asym = coefs[[1]],
                 Drop = coefs[[2]],
                 lrc = coefs[[3]],
                 pwr = coefs[[4]],
      interval = c(fflight, lflight),
      maximum = TRUE)

    # maximum acceleration
    sdopt_result <-
      optimize(sdfun_weibull,
               Asym = coefs[[1]],
               Drop = coefs[[2]],
               lrc = coefs[[3]],
               pwr = coefs[[4]],
               interval = c(fflight, lflight),
               maximum = TRUE)

    # maximum deceleration
    sdopt_result2 <-
      optimize(sdfun_weibull,
               Asym = coefs[[1]],
               Drop = coefs[[2]],
               lrc = coefs[[3]],
               pwr = coefs[[4]],
               interval = c(fflight, lflight),
               maximum = FALSE)

    # Return results
    tibble::tibble(
      unique_plot = dftemp$unique_plot[i],
      model = "Weibull",
      asymptote = coefs[[1]],
      auc = int1$value,
      xinfp = fdopt_result$maximum,
      yinfp = fdopt_result$objective,
      xmace = sdopt_result$maximum,
      ymace = sdopt_result$objective,
      xmdes = sdopt_result2$minimum,
      ymdes = sdopt_result2$objective,
      aic = gofval[[1]],
      rmse = gofval[[2]],
      mae = gofval[[3]],
      parms = list(model = modfun_weibull,
                   modeladj = model,
                   fd = fdfun_weibull,
                   sd = sdfun_weibull,
                   coefs = list(Asym = coefs[[1]],
                                Drop = coefs[[2]],
                                lrc = coefs[[3]],
                                pwr = coefs[[4]]), xmin = fflight, xmax = lflight)
    )
    }, error = function(e) {
      # Return NA values if model fails
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Weibull",
        asymptote = NA_real_,
        auc = NA_real_,
        xinfp = NA_real_,
        yinfp = NA_real_,
        xmace = NA_real_,
        ymace = NA_real_,
        xmdes = NA_real_,
        ymdes = NA_real_,
        aic = NA_real_,
        rmse = NA_real_,
        mae = NA_real_,
        parms = NA
      )
    })

result
  }
  results <-
    results_list |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)

  return(results)
}



help_mod_weibull_eq <- function() {
  "$$y = \\text{Asym} - \\text{Drop} \\cdot \\exp\\left(-\\exp(\\text{lrc}) \\cdot x^{\\text{pwr}}\\right)$$"
}
help_mod_weibull_fd <- function() {
  "$$y'(x) = \\text{Drop} \\cdot \\exp\\left(-\\exp(\\text{lrc}) \\cdot x^{\\text{pwr}}\\right) \\cdot \\exp(\\text{lrc}) \\cdot x^{\\text{pwr} - 1} \\cdot \\text{pwr}$$"
}
help_mod_weibull_sd <- function() {
  "$$y''(x) = \\text{Drop} \\cdot \\left[\\exp\\left(-\\exp(\\text{lrc}) \\cdot x^{\\text{pwr}}\\right) \\cdot \\exp(\\text{lrc}) \\cdot x^{\\text{pwr} - 2} \\cdot \\text{pwr} \\cdot (\\text{pwr} - 1) - \\exp\\left(-\\exp(\\text{lrc}) \\cdot x^{\\text{pwr}}\\right) \\cdot \\left(\\exp(\\text{lrc}) \\cdot x^{\\text{pwr} - 1} \\cdot \\text{pwr}\\right)^2\\right]$$"
}


help_mod_weibull <- function() {
  div(
    style = "font-family: Arial, sans-serif; line-height: 1.5;",
    h2(style = "color: #2E86C1;", "Growth Curve"),
    p("The Weibull growth model is widely used to describe time-to-event data, failure rates, and biological growth processes. It captures flexible growth patterns, allowing for various shapes depending on the parameters."),
    p("The equation is given by:"),
    p(help_mod_weibull_eq()),
    p(strong("Where:")),
    withMathJax(
      tags$ul(
        tags$li("\\(x\\): The time or independent variable."),
        tags$li("\\(\\text{Asym}\\): The maximum potential size or upper asymptote."),
        tags$li("\\(\\text{Drop}\\): The range between the asymptote and the minimum value."),
        tags$li("\\(\\text{lrc}\\): A logarithmic growth rate parameter."),
        tags$li("\\(\\text{pwr}\\): A power parameter influencing the shape of the curve.")
      )
    ),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Models flexible growth curves that can be adjusted for various shapes."),
      tags$li("Represents growth rate, curve shape, and maximum size."),
      tags$li("Commonly applied to reliability analysis, biological growth, and survival data.")
    ),
    h2(style = "color: #2E86C1;", "First Derivative"),
    p("The first derivative of the Weibull model describes the rate of change of the response variable (e.g., growth rate) at any given point. It is particularly useful for identifying the point of maximum growth."),
    p("The first derivative is given by:"),
    p(help_mod_weibull_fd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Captures the growth rate at any point on the curve."),
      tags$li("Helps in understanding growth dynamics over time."),
      tags$li("Useful for comparing growth rates across different conditions or treatments.")
    ),
    h2(style = "color: #2E86C1;", "Second Derivative"),
    p("The second derivative of the Weibull model provides insight into the curvature of the growth curve, helping to identify points of acceleration or deceleration in growth. This is crucial for understanding how growth dynamics change over time."),
    p("The second derivative is given by:"),
    p(help_mod_weibull_sd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Describes how the rate of growth changes over time (acceleration or deceleration)."),
      tags$li("Helps identify regions of maximum acceleration or deceleration in growth."),
      tags$li("Useful for fine-tuning growth predictions and analyzing biological phenomena.")
    )
  )
}


############## GOMPERTZ MODEL #############

# Define Gompertz model
modfun_gompertz <- function(x, Asym, b2, b3) {
  Asym *exp(-b2 * b3 ^ x)
}

# First derivative of Gompertz model
fdfun_gompertz <- function(x, Asym, b2, b3) {
  -(Asym * (exp(-b2 * b3^x) * (b2 * (b3^x * log(b3)))))
}

# Second derivative of Gompertz model
sdfun_gompertz <- function(x, Asym, b2, b3) {
  -(Asym * (exp(-b2 * b3^x) * (b2 * (b3^x * log(b3) * log(b3))) -
              exp(-b2 * b3^x) * (b2 * (b3^x * log(b3))) * (b2 * (b3^x *
                                                                   log(b3)))))
}

mod_gompertz <- function(data,
                         flight_date = "date",
                         predictor = "median.NDVI",
                         sowing_date = NULL,
                         parallel = FALSE) {

  # Prepare data
  dftemp <-
    data |>
    dplyr::mutate(unique_plot = paste0(block, "_", plot_id)) |>
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) |>
    dplyr::group_by(unique_plot) |>
    tidyr::nest()

  # Parallel or Sequential Plan
  if (parallel) {
    availablecl <- parallel::detectCores()
    future::plan(future::multisession, workers = round(availablecl * 0.75))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`

  # Fit model for each group
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows) %dofut% {
    df <- as.data.frame(dftemp$data[[i]])
    if (!is.null(sowing_date)) {
      flights <- as.POSIXlt(df$date)$yday + 1 - (as.POSIXlt(sowing_date)$yday)
    } else {
      flights <- as.POSIXlt(df$date)$yday + 1
    }

    fflight <- min(flights)
    lflight <- max(flights) + 20
    flights_seq <- fflight:lflight
    y <- df |> dplyr::pull(!!rlang::sym(predictor))
    x <- flights

    result <- tryCatch({
      # Fit Gompertz model
      model <- nls(y ~ SSgompertz(x, Asym, b2, b3),
                   control = nls.control(maxiter = 1000),
                   data = data.frame(x, y))

      coefs <- coef(model)
      gofval <- gof(model, y)

      # Area under curve
      int1 <-
        integrate(modfun_gompertz,
                  lower = fflight,
                  upper = lflight,
                  Asym = coefs[[1]],
                  b2 = coefs[[2]],
                  b3 = coefs[[3]])

      # critical points
      fdopt_result <-
        optimize(fdfun_gompertz,
                 Asym = coefs[[1]],
                 b2 = coefs[[2]],
                 b3 = coefs[[3]],
                 interval = c(fflight, lflight),
                 maximum = TRUE)

      # maximum acceleration
      sdopt_result <-
        optimize(sdfun_gompertz,
                 Asym = coefs[[1]],
                 b2 = coefs[[2]],
                 b3 = coefs[[3]],
                 interval = c(fflight, lflight),
                 maximum = TRUE)

      # maximum deceleration
      sdopt_result2 <-
        optimize(sdfun_gompertz,
                 Asym = coefs[[1]],
                 b2 = coefs[[2]],
                 b3 = coefs[[3]],
                 interval = c(fflight, lflight),
                 maximum = FALSE)


      # Return results
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Gompertz",
        asymptote = coefs[[1]],
        auc = int1$value,
        xinfp = fdopt_result$maximum,
        yinfp = fdopt_result$objective,
        xmace = sdopt_result$maximum,
        ymace = sdopt_result$objective,
        xmdes = sdopt_result2$minimum,
        ymdes = sdopt_result2$objective,
        aic = gofval[[1]],
        rmse = gofval[[2]],
        mae = gofval[[3]],
        parms = list(model = modfun_gompertz,
                     modeladj = model,
                     fd = fdfun_gompertz,
                     sd = sdfun_gompertz,
                     coefs = list(Asym = coefs[[1]],
                                  b2 = coefs[[2]],
                                  b3 = coefs[[3]]), xmin = fflight, xmax = lflight)
      )
    }, error = function(e) {
      # Return NA values if model fails
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Gompertz",
        asymptote = NA_real_,
        auc = NA_real_,
        xinfp = NA_real_,
        yinfp = NA_real_,
        xmace = NA_real_,
        ymace = NA_real_,
        xmdes = NA_real_,
        ymdes = NA_real_,
        aic = NA_real_,
        rmse = NA_real_,
        mae = NA_real_,
        parms = NA
      )
    })

    result
  }

  results <-
    results_list |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)

  return(results)
}


# Gompertz model equation
help_mod_gompertz_eq <- function() {
  "$$y = \\text{Asym} \\cdot \\exp(-b_2 \\cdot b_3^x)$$"
}

# First derivative of the Gompertz model
help_mod_gompertz_fd <- function() {
  "$$y'(x) = -\\text{Asym} \\cdot \\exp(-b_2 \\cdot b_3^x) \\cdot b_2 \\cdot (b_3^x \\cdot \\ln(b_3))$$"
}

# Second derivative of the Gompertz model
help_mod_gompertz_sd <- function() {
  "$$y''(x) = -\\text{Asym} \\cdot \\left(\\exp(-b_2 \\cdot b_3^x) \\cdot b_2 \\cdot (b_3^x \\cdot \\ln(b_3) \\cdot \\ln(b_3)) - \\exp(-b_2 \\cdot b_3^x) \\cdot \\left(b_2 \\cdot (b_3^x \\cdot \\ln(b_3))\\right)^2\\right)$$"
}

help_mod_gompertz <- function() {
  div(
    style = "font-family: Arial, sans-serif; line-height: 1.5;",
    h2(style = "color: #2E86C1;", "Growth Curve"),
    # Description
    p("The Gompertz growth model is widely used to describe biological processes, including population growth, tumor growth, and plant development. It captures asymmetrical sigmoidal growth patterns, where growth starts rapidly, slows down, and eventually plateaus."),
    p("The equation is given by:"),
    p(help_mod_gompertz_eq()),
    p(strong("Where:")),
    withMathJax(
      tags$ul(
        tags$li("\\(x\\): The time or independent variable."),
        tags$li("\\(\\text{Asym}\\): The maximum potential size or upper asymptote."),
        tags$li("\\(b_2\\): The rate parameter, controlling how quickly the system approaches the asymptote."),
        tags$li("\\(b_3\\): A scaling parameter affecting the timing of maximal growth.")
      )
    ),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Models asymmetrical sigmoidal growth curves."),
      tags$li("Represents initial growth rate, maximal growth timing, and maximum size."),
      tags$li("Commonly applied to population dynamics, crop growth, and disease progression.")
    ),
    h2(style = "color: #2E86C1;", "First Derivative"),
    p("The first derivative of the Gompertz model describes the rate of change of the response variable (e.g., growth rate) at any given point. It is useful for identifying the point of maximal growth rate."),
    p("The first derivative is given by:"),
    p(help_mod_gompertz_fd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Captures the growth rate at any point on the curve."),
      tags$li("Helps in understanding growth dynamics over time."),
      tags$li("Useful for comparing growth rates across different conditions or treatments.")
    ),
    h2(style = "color: #2E86C1;", "Second Derivative"),
    # Description
    p("The second derivative of the Gompertz model provides insight into the curvature of the growth curve, helping to identify points of acceleration or deceleration in growth. This is crucial for understanding how growth dynamics change over time."),
    p("The second derivative is given by:"),
    p(help_mod_gompertz_sd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Describes how the rate of growth changes over time (acceleration or deceleration)."),
      tags$li("Helps identify regions of maximum acceleration or deceleration in growth."),
      tags$li("Useful for fine-tuning growth predictions and analyzing biological phenomena.")
    )
  )
}

############## LOGISTIC MODEL 3 PARAMETERS - GROWTH MODELS #############
mod_logistic_3P <- function(data,
                            flight_date = "date",
                            predictor = "median.NDVI",
                            sowing_date = NULL,
                            parallel = FALSE) {
  # Prepare data
  dftemp <-
    data |>
    dplyr::mutate(unique_plot = paste0(block, "_", plot_id)) |>
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) |>
    dplyr::group_by(unique_plot) |>
    tidyr::nest()

  # Parallel or Sequential Plan
  if (parallel) {
    availablecl <- parallel::detectCores()
    future::plan(future::multisession, workers = round(availablecl * 0.75))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`

  # Fit model for each group
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows) %dofut% {
    df <- as.data.frame(dftemp$data[[i]])
    if (!is.null(sowing_date)) {
      flights <- as.POSIXlt(df$date)$yday + 1 - (as.POSIXlt(sowing_date)$yday)
    } else {
      flights <- as.POSIXlt(df$date)$yday + 1
    }

    fflight <- min(flights)
    lflight <- max(flights) + 20
    y <- df |> dplyr::pull(!!rlang::sym(predictor))
    x <- flights

    result <- tryCatch({
      # Fit Gompertz model
      model <- nls(y ~ SSlogis(x, Asym, xmid, scal),
                   control = nls.control(maxiter = 1000),
                   data = data.frame(x, y))

      coefs <- coef(model)
      gofval <- gof(model, y)

      # Area under curve
      int1 <-
        integrate(modfun_L3,
                  lower = fflight,
                  upper = lflight,
                  b0 = coefs[[1]],
                  b1 = coefs[[2]],
                  b2 = coefs[[3]])

      # critical points
      fdopt_result <-
        optimize(fdfun_L3,
                 b0 = coefs[[1]],
                 b1 = coefs[[2]],
                 b2 = coefs[[3]],
                 interval = c(fflight, lflight),
                 maximum = TRUE)

      # maximum acceleration
      sdopt_result <-
        optimize(sdfun_L3,
                 b0 = coefs[[1]],
                 b1 = coefs[[2]],
                 b2 = coefs[[3]],
                 interval = c(fflight, lflight),
                 maximum = TRUE)

      # maximum deceleration
      sdopt_result2 <-
        optimize(sdfun_L3,
                 b0 = coefs[[1]],
                 b1 = coefs[[2]],
                 b2 = coefs[[3]],
                 interval = c(fflight, lflight),
                 maximum = FALSE)


      # Return results
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Logistic 3P",
        asymptote = coefs[[1]],
        auc = int1$value,
        xinfp = fdopt_result$maximum,
        yinfp = fdopt_result$objective,
        xmace = sdopt_result$maximum,
        ymace = sdopt_result$objective,
        xmdes = sdopt_result2$minimum,
        ymdes = sdopt_result2$objective,
        aic = gofval[[1]],
        rmse = gofval[[2]],
        mae = gofval[[3]],
        parms = list(model = modfun_L3,
                     modeladj = model,
                     fd = fdfun_L3,
                     sd = sdfun_L3,
                     coefs = list(
                       b0 = coefs[[1]],
                       b1 = coefs[[2]],
                       b2 = coefs[[3]]
                     ),
                     xmin = fflight, xmax = lflight)
      )
    }, error = function(e) {
      # Return NA values if model fails
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Logistic 3P",
        asymptote = NA_real_,
        auc = NA_real_,
        xinfp = NA_real_,
        yinfp = NA_real_,
        xmace = NA_real_,
        ymace = NA_real_,
        xmdes = NA_real_,
        ymdes = NA_real_,
        aic = NA_real_,
        rmse = NA_real_,
        mae = NA_real_,
        parms = NA
      )
    })

    result
  }
  results <-
    results_list |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)

  return(results)
}

help_mod_L3_gm <- function() {
  div(
    style = "font-family: Arial, sans-serif; line-height: 1.5;",
    h2(style = "color: #2E86C1;", "Growth Curve"),
    p("The 3-parameter logistic growth model is widely used to describe S-shaped growth curves. It characterizes processes with a maximum asymptotic value, an inflection point, and a steepness parameter."),
    p("The equation is given by:"),
    p(help_mod_L3_eq()),
    p(strong("Where:")),
    withMathJax(
      tags$ul(
        tags$li("\\(x\\): The time or independent variable."),
        tags$li("\\(b_0\\): The maximum asymptotic value."),
        tags$li("\\(b_1\\): The inflection point."),
        tags$li("\\(b_2\\): A scale parameter controlling the steepness of the curve.")
      )
    ),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Captures S-shaped growth dynamics."),
      tags$li("Identifies the point of maximum growth rate (inflection point)."),
      tags$li("Commonly applied to population growth, biological systems, and logistic processes.")
    ),
    h2(style = "color: #2E86C1;", "First Derivative"),
    p("The first derivative of the logistic growth model describes the rate of change of the response variable at any given point. It is particularly useful for identifying the maximum growth rate."),
    p("The first derivative is given by:"),
    p(help_mod_L3_fd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Captures the instantaneous growth rate."),
      tags$li("Helps identify the steepest part of the curve (inflection point)."),
      tags$li("Useful for analyzing growth dynamics over time.")
    ),
    h2(style = "color: #2E86C1;", "Second Derivative"),
    p("The second derivative of the logistic growth model provides insight into the curvature of the growth curve. It helps identify points where growth accelerates or decelerates the most."),
    p("The second derivative is given by:"),
    p(help_mod_L3_sd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Describes changes in the growth rate over time."),
      tags$li("Helps locate regions of maximum acceleration or deceleration."),
      tags$li("Useful for fine-tuning growth predictions and analyzing dynamic systems.")
    )
  )
}





############## LOGISTIC MODEL 4 PARAMETERS - GROWTH MODELS #############
mod_logistic_4P <- function(data,
                            flight_date = "date",
                            predictor = "median.NDVI",
                            sowing_date = NULL,
                            parallel = FALSE) {
  # Prepare data
  dftemp <-
    data |>
    dplyr::mutate(unique_plot = paste0(block, "_", plot_id)) |>
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) |>
    dplyr::group_by(unique_plot) |>
    tidyr::nest()

  results_list <- list()

  # Parallel or Sequential Plan
  if (parallel) {
    availablecl <- parallel::detectCores()
    future::plan(future::multisession, workers = round(availablecl * 0.75))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`

  # Fit model for each group
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows) %dofut% {
    df <- as.data.frame(dftemp$data[[i]])

    if (!is.null(sowing_date)) {
      flights <- as.POSIXlt(df$date)$yday + 1 - (as.POSIXlt(sowing_date)$yday)
    } else {
      flights <- as.POSIXlt(df$date)$yday + 1
    }

    fflight <- min(flights)
    lflight <- max(flights) + 20
    y <- df |> dplyr::pull(!!rlang::sym(predictor))

    # Use tryCatch to handle model fitting errors
    result <- tryCatch({
      # Fit Logistic model
      model <- drc::drm(y ~ flights, fct = drc::L.4(), data = data.frame(flights, y))
      coefslog <- coef(model)
      b0 <- coefslog[1]
      b1 <- coefslog[2]
      b2 <- coefslog[3]
      b3 <- coefslog[4]

      # Goodness of fit
      gofval <- gof(model, y)

      # Area under curve
      auc <- integrate(modfun_L4, lower = fflight, upper = lflight, b0 = b0, b1 = b1, b2 = b2, b3 = b3)

      # critical points
      fdopt_result <-
        optimize(fdfun_L4,
                 b0 = b0,
                 b1 = b1,
                 b2 = b2,
                 b3 = b3,
                 interval = c(fflight, lflight),
                 maximum = TRUE)

      # maximum acceleration
      sdopt_result <-
        optimize(sdfun_L4,
                 b0 = b0,
                 b1 = b1,
                 b2 = b2,
                 b3 = b3,
                 interval = c(fflight, lflight),
                 maximum = TRUE)

      # maximum deceleration
      sdopt_result2 <-
        optimize(sdfun_L4,
                 b0 = b0,
                 b1 = b1,
                 b2 = b2,
                 b3 = b3,
                 interval = c(fflight, lflight),
                 maximum = FALSE)

      # Return results
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Logistic 4P",
        asymptote = b2,
        auc = auc$value,
        xinfp = fdopt_result$maximum,
        yinfp = fdopt_result$objective,
        xmace = sdopt_result$maximum,
        ymace = sdopt_result$objective,
        xmdes = sdopt_result2$minimum,
        ymdes = sdopt_result2$objective,
        aic = gofval$AIC,
        rmse = gofval$RMSE,
        mae = gofval$MAE,
        parms = list(model = modfun_L4,
                     modeladj = model,
                     fd = fdfun_L4,
                     sd = sdfun_L4,
                     coefs = list(b0 = b0, b1 = b1, b2 = b2, b3 = b3),
                     xmin = fflight, xmax = lflight)
      )
    }, error = function(e) {
      # Return NA values if model fitting fails
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Logistic 4P",
        asymptote = NA_real_,
        auc = NA_real_,
        xinfp = NA_real_,
        yinfp = NA_real_,
        xmace = NA_real_,
        ymace = NA_real_,
        xmdes = NA_real_,
        ymdes = NA_real_,
        aic = NA_real_,
        rmse = NA_real_,
        mae = NA_real_,
        parms = NA
      )
    })

    result
  }

  results <-
    results_list |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)

  return(results)
}

# Logistic
help_mod_L4_gm <- function(){
  div(
    style = "font-family: Arial, sans-serif; line-height: 1.5;",
    p("This information is essential for analyzing growth patterns, comparing treatments, and understanding biological dynamics."),
    h2(style = "color: #2E86C1;", "Growth Curve"),
    # Description
    style = "font-family: Arial, sans-serif; line-height: 1.5;",
    p("The four-parameter logistic model (4PL) is widely used to describe biological growth processes, such as plant height, weight, or population dynamics. It captures the characteristic sigmoidal (S-shaped) growth pattern often observed in nature."),
    p("The equation is given by:"),
    p("$$y = b_1 + \\frac{(b_2 - b_1)}{1 + \\exp(b_0 \\cdot (x - b_3))}$$"),
    p(strong("Where:")),
    withMathJax(
      tags$ul(
        tags$li("\\(x\\): The time in days after sowing."),
        tags$li("\\(b_0\\): The growth rate (steepness of the curve)."),
        tags$li("\\(b_1\\): The initial size or lower asymptote (e.g., initial plant height)."),
        tags$li("\\(b_2\\): The maximum potential size or upper asymptote (e.g., maximum plant height)."),
        tags$li("\\(b_3\\): The inflection point (time or stage when growth rate is maximal).")
      )
    ),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Models sigmoidal growth curves that start slow, accelerate, and then plateau."),
      tags$li("Represents initial size, growth rate, and maximum size clearly."),
      tags$li("Commonly applied to plant growth studies, animal development, and population dynamics.")
    ),
    h2(style = "color: #2E86C1;", "First Derivative"),
    p("The first derivative of the four-parameter logistic model (4PL) describes the rate of change of the response variable (e.g., growth rate) at any given point. It is particularly useful for identifying the inflection point, where the rate of growth is maximal."),
    p("The first derivative is given by:"),
    p("$$
    y'(x) = -\\frac{(b_2 - b_1) \\cdot \\exp(b_0 \\cdot (x - b_3)) \\cdot b_0}{(1 + \\exp(b_0 \\cdot (x - b_3)))^2}
    $$"),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Captures the growth rate at any point on the curve."),
      tags$li("The maximum value of the derivative corresponds to the inflection point."),
      tags$li("Helps in understanding the dynamics of growth over time."),
      tags$li("Useful for comparing growth rates across different conditions or treatments."),
      p("This derivative is particularly useful for studying dynamic biological processes, such as plant height growth, where understanding the rate of change is crucial."),
      h2(style = "color: #2E86C1;", "Second Derivative"),
      # Description
      style = "font-family: Arial, sans-serif; line-height: 1.5;",
      p("The second derivative of the four-parameter logistic model (4PL) provides insight into the curvature of the growth curve, helping to identify points of acceleration or deceleration in growth. This is crucial for understanding how growth dynamics change over time."),
      p("The second derivative is given by:"),
      p("$$
    y''(x) = -\\frac{(b_2 - b_1) \\cdot \\exp(b_0 \\cdot (x - b_3)) \\cdot b_0^2}{(1 + \\exp(b_0 \\cdot (x - b_3)))^2} -
    \\frac{(b_2 - b_1) \\cdot \\exp(b_0 \\cdot (x - b_3)) \\cdot b_0 \\cdot
    \\big(2 \\cdot \\exp(b_0 \\cdot (x - b_3)) \\cdot b_0 \\cdot (1 + \\exp(b_0 \\cdot (x - b_3)))\\big)}
    {(1 + \\exp(b_0 \\cdot (x - b_3)))^4}
    $$"),
      p(strong("Key Features:")),
      tags$ul(
        tags$li("Describes how the rate of growth changes over time (acceleration or deceleration)."),
        tags$li("Helps identify regions of maximum acceleration or deceleration in growth."),
        tags$li("Provides insights into the dynamics of curvature for growth modeling."),
        tags$li("Useful for fine-tuning growth predictions and analyzing biological phenomena.")
      ),
      p("The second derivative is a powerful tool for understanding not just the growth rate but also the changes in growth dynamics over time. It is particularly useful in identifying critical points in biological systems, such as transitions from rapid growth to slower growth phases.")
    )
  )
}


######### VON bertalanffy MODEL ############

modfun_vonbert <- function(t, Linf, k, t0) {
  Linf * (1 - exp(-k * (t - t0)))
}

# First derivative of Von Bertalanffy model
fdfun_vonbert <- function(t, Linf, k, t0) {
  Linf * k * exp(-k * (t - t0))
}

# Second derivative of Von Bertalanffy model
sdfun_vonbert <- function(t, Linf, k, t0) {
  -(Linf * k * (exp(-k * (t - t0)) * k))
}
# self start functions
SSvonBertalanffy <- selfStart(
  model = function(t, Linf, k, t0) {
    Linf * (1 - exp(-k * (t - t0)))
  },
  initial = function(mCall, data, LHS, ...) {
    t <- eval(mCall[["t"]], data)
    y <- eval(LHS, data)

    # Estimate Linf: Maximum observed value
    Linf_init <- max(y)

    # Estimate k: Growth rate (inverse of time to reach 90% of Linf)
    k_init <- 0.1 # Default guess, refine based on data range

    # Estimate t0: Time of zero size (extrapolate back)
    t0_init <- min(t) - 1 # Slightly before the first time point

    c(Linf = Linf_init, k = k_init, t0 = t0_init)
  },
  parameters = c("Linf", "k", "t0")
)

mod_vonbert <- function(data,
                        flight_date = "date",
                        predictor = "median.NDVI",
                        sowing_date = NULL,
                        parallel = FALSE) {
  # Prepare data
  dftemp <-
    data |>
    dplyr::mutate(unique_plot = paste0(block, "_", plot_id)) |>
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) |>
    dplyr::group_by(unique_plot) |>
    tidyr::nest()

  # Parallel or Sequential Plan
  if (parallel) {
    availablecl <- parallel::detectCores()
    future::plan(future::multisession, workers = round(availablecl * 0.75))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`

  # Fit model for each group
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows) %dofut% {
    df <- as.data.frame(dftemp$data[[i]])
    if (!is.null(sowing_date)) {
      flights <- as.POSIXlt(df$date)$yday + 1 - (as.POSIXlt(sowing_date)$yday)
    } else {
      flights <- as.POSIXlt(df$date)$yday + 1
    }

    fflight <- min(flights)
    lflight <- max(flights) + 20
    y <- df |> dplyr::pull(!!rlang::sym(predictor))
    x <- flights

    result <- tryCatch({
      # Fit Gompertz model
      model <- nls(y ~ SSvonBertalanffy(x, Linf, k, t0),
                   control = nls.control(maxiter = 1000),
                   data = data.frame(x, y))

      coefs <- coef(model)
      gofval <- gof(model, y)

      # Area under curve
      int1 <-
        integrate(modfun_vonbert,
                  lower = fflight,
                  upper = lflight,
                  Linf = coefs[[1]],
                  k = coefs[[2]],
                  t0 = coefs[[3]])

      # critical points
      fdopt_result <-
        optimize(fdfun_vonbert,
                 Linf = coefs[[1]],
                 k = coefs[[2]],
                 t0 = coefs[[3]],
                 interval = c(fflight, lflight),
                 maximum = TRUE)

      # maximum acceleration
      sdopt_result <-
        optimize(sdfun_vonbert,
                 Linf = coefs[[1]],
                 k = coefs[[2]],
                 t0 = coefs[[3]],
                 interval = c(fflight, lflight),
                 maximum = TRUE)

      # maximum deceleration
      sdopt_result2 <-
        optimize(sdfun_vonbert,
                 Linf = coefs[[1]],
                 k = coefs[[2]],
                 t0 = coefs[[3]],
                 interval = c(fflight, lflight),
                 maximum = FALSE)


      # Return results
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Von Bertalanffy",
        asymptote = coefs[[1]],
        auc = int1$value,
        xinfp = fdopt_result$maximum,
        yinfp = fdopt_result$objective,
        xmace = sdopt_result$maximum,
        ymace = sdopt_result$objective,
        xmdes = sdopt_result2$minimum,
        ymdes = sdopt_result2$objective,
        aic = gofval[[1]],
        rmse = gofval[[2]],
        mae = gofval[[3]],
        parms = list(model = modfun_vonbert,
                     modeladj = model,
                     fd = fdfun_vonbert,
                     sd = sdfun_vonbert,
                     coefs = list(
                       Linf = coefs[[1]],
                       k = coefs[[2]],
                       t0 = coefs[[3]],
                     ),
                     xmin = fflight, xmax = lflight)
      )
    }, error = function(e) {
      # Return NA values if model fails
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Von Bertalanffy",
        asymptote = NA_real_,
        auc = NA_real_,
        xinfp = NA_real_,
        yinfp = NA_real_,
        xmace = NA_real_,
        ymace = NA_real_,
        xmdes = NA_real_,
        ymdes = NA_real_,
        aic = NA_real_,
        rmse = NA_real_,
        mae = NA_real_,
        parms = NA
      )
    })
    result
  }
  results <-
    results_list |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)

  return(results)
}

help_mod_vonbert_eq <- function() {
  "$$y(x) = L_\\infty \\cdot (1 - e^{-k \\cdot (x - t_0)})$$"
}

help_mod_vonbert_fd <- function() {
  "$$y'(x) = L_\\infty \\cdot k \\cdot e^{-k \\cdot (x - t_0)}$$"
}

help_mod_vonbert_sd <- function() {
  "$$y''(x) = -L_\\infty \\cdot k^2 \\cdot e^{-k \\cdot (x - t_0)}$$"
}

help_mod_vonbertalanffy <- function() {
  div(
    style = "font-family: Arial, sans-serif; line-height: 1.5;",
    h2(style = "color: #2E86C1;", "Growth Curve"),
    p("The Von Bertalanffy Growth Function (VBGF) is widely used in biology and ecology to model the growth of organisms over time. It describes growth as an asymptotic process, where the size approaches a maximum value \\(L_\\infty\\)."),
    p("The equation is given by:"),
    p(help_mod_vonbert_eq()),
    p(strong("Where:")),
    withMathJax(
      tags$ul(
        tags$li("\\(x\\): The time or independent variable."),
        tags$li("\\(L_\\infty\\): The asymptotic maximum size."),
        tags$li("\\(k\\): The growth rate coefficient."),
        tags$li("\\(t_0\\): The hypothetical time of zero size.")
      )
    ),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Models growth as an asymptotic process."),
      tags$li("Accounts for maximum size and growth rate dynamics."),
      tags$li("Commonly applied in fisheries, ecology, and biological growth studies.")
    ),
    h2(style = "color: #2E86C1;", "First Derivative"),
    p("The first derivative of the Von Bertalanffy model describes the rate of growth at any given time. It is useful for identifying periods of rapid growth."),
    p("The first derivative is given by:"),
    p(help_mod_vonbert_fd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Captures the instantaneous growth rate."),
      tags$li("Useful for analyzing growth dynamics over time."),
      tags$li("Helps identify the point of maximum growth.")
    ),
    h2(style = "color: #2E86C1;", "Second Derivative"),
    p("The second derivative of the Von Bertalanffy model provides insight into the curvature of the growth curve. It indicates whether growth is accelerating or decelerating."),
    p("The second derivative is given by:"),
    p(help_mod_vonbert_sd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Describes how the growth rate changes over time."),
      tags$li("Helps identify points of maximum acceleration or deceleration."),
      tags$li("Useful for understanding growth patterns and dynamics.")
    )
  )
}


############ EXPONENTIAL MODEL ############
modfun_exp <- function(t, a, b) {
  a * exp(b * t)
}

# First derivative of Exponential model
fdfun_exp <- function(t, a, b) {
  a * (exp(b * t) * b)
}
# Second derivative of Exponential model
sdfun_exp <- function(t, a, b) {
  a * b^2 * exp(b * t)
}
SSexponential <- selfStart(
  model = function(t, a, b) {
    a * exp(b * t)
  },
  initial = function(mCall, data, LHS, ...) {
    t <- eval(mCall[["t"]], data)
    y <- eval(LHS, data)

    # Estimate a: Initial value (y at t = 0 or smallest t)
    a_init <- y[which.min(t)]

    # Estimate b: Logarithmic growth rate
    b_init <- log(y[length(y)] / y[1]) / (t[length(t)] - t[1]) # (ln(y2/y1)) / (t2 - t1)

    c(a = a_init, b = b_init)
  },
  parameters = c("a", "b")
)
mod_exponential <- function(data,
                            flight_date = "date",
                            predictor = "median.NDVI",
                            sowing_date = NULL,
                            parallel = FALSE) {
  # Prepare data
  dftemp <-
    data |>
    dplyr::mutate(unique_plot = paste0(block, "_", plot_id)) |>
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) |>
    dplyr::group_by(unique_plot) |>
    tidyr::nest()

  # Parallel or Sequential Plan
  if (parallel) {
    availablecl <- parallel::detectCores()
    future::plan(future::multisession, workers = round(availablecl * 0.75))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`

  # Fit model for each group
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows) %dofut% {
    df <- as.data.frame(dftemp$data[[i]])
    if (!is.null(sowing_date)) {
      flights <- as.POSIXlt(df$date)$yday + 1 - (as.POSIXlt(sowing_date)$yday)
    } else {
      flights <- as.POSIXlt(df$date)$yday + 1
    }

    fflight <- min(flights)
    lflight <- max(flights) + 20
    y <- df |> dplyr::pull(!!rlang::sym(predictor))
    x <- flights

    result <- tryCatch({
      # Fit Exponential model
      model <- nls(y ~ SSexponential(x, a, b),
                   control = nls.control(maxiter = 1000),
                   data = data.frame(x, y))

      coefs <- coef(model)
      gofval <- gof(model, y)

      # Area under curve
      int1 <-
        integrate(modfun_exp,
                  lower = fflight,
                  upper = lflight,
                  a = coefs[[1]],
                  b = coefs[[2]])

      # Critical points
      fdopt_result <-
        optimize(fdfun_exp,
                 a = coefs[[1]],
                 b = coefs[[2]],
                 interval = c(fflight, lflight),
                 maximum = TRUE)

      # Maximum acceleration
      sdopt_result <-
        optimize(sdfun_exp,
                 a = coefs[[1]],
                 b = coefs[[2]],
                 interval = c(fflight, lflight),
                 maximum = TRUE)

      # Return results
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Exponential",
        asymptote = NA_real_, # Not applicable for exponential
        auc = int1$value,
        xinfp = fdopt_result$maximum,
        yinfp = fdopt_result$objective,
        xmace = sdopt_result$maximum,
        ymace = sdopt_result$objective,
        xmdes = NA_real_, # No deceleration in pure exponential
        ymdes = NA_real_,
        aic = gofval[[1]],
        rmse = gofval[[2]],
        mae = gofval[[3]],
        parms = list(model = modfun_exp,
                     modeladj = model,
                     fd = fdfun_exp,
                     sd = sdfun_exp,
                     coefs = list(
                       a = coefs[[1]],
                       b = coefs[[2]]
                     ),
                     xmin = fflight, xmax = lflight)
      )
    }, error = function(e) {
      # Return NA values if model fails
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Exponential",
        asymptote = NA_real_,
        auc = NA_real_,
        xinfp = NA_real_,
        yinfp = NA_real_,
        xmace = NA_real_,
        ymace = NA_real_,
        xmdes = NA_real_,
        ymdes = NA_real_,
        aic = NA_real_,
        rmse = NA_real_,
        mae = NA_real_,
        parms = NA
      )
    })
    result
  }
  results <-
    results_list |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)

  return(results)
}

help_mod_exp_eq <- function() {
  "$$y(x) = a \\cdot e^{b \\cdot x}$$"
}

help_mod_exp_fd <- function() {
  "$$y'(x) = a \\cdot b \\cdot e^{b \\cdot x}$$"
}

help_mod_exp_sd <- function() {
  "$$y''(x) = a \\cdot b^2 \\cdot e^{b \\cdot x}$$"
}

help_mod_exponential <- function() {
  div(
    style = "font-family: Arial, sans-serif; line-height: 1.5;",
    h2(style = "color: #2E86C1;", "Growth Curve"),
    p("The exponential growth model is widely used to describe processes that increase exponentially over time, such as population growth, financial returns, and biological systems. It assumes continuous, proportional growth."),
    p("The equation is given by:"),
    p(help_mod_exp_eq()),
    p(strong("Where:")),
    withMathJax(
      tags$ul(
        tags$li("\\(x\\): The time or independent variable."),
        tags$li("\\(a\\): The initial size or value at time zero."),
        tags$li("\\(b\\): The exponential growth rate parameter.")
      )
    ),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Models continuous growth processes that increase proportionally."),
      tags$li("Characterizes exponential acceleration or deceleration depending on the sign of \\(b\\)."),
      tags$li("Applicable to population growth, finance, and biological processes.")
    ),
    h2(style = "color: #2E86C1;", "First Derivative"),
    p("The first derivative of the exponential model describes the rate of change of the response variable. It indicates how quickly the system is growing or shrinking at any given time."),
    p("The first derivative is given by:"),
    p(help_mod_exp_fd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Captures the instantaneous growth rate."),
      tags$li("Useful for analyzing growth dynamics over time."),
      tags$li("Helps compare rates of growth across different systems or treatments.")
    ),
    h2(style = "color: #2E86C1;", "Second Derivative"),
    p("The second derivative of the exponential model provides insight into the acceleration of growth. It describes how the growth rate itself is changing over time."),
    p("The second derivative is given by:"),
    p(help_mod_exp_sd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Describes how the growth rate is accelerating or decelerating."),
      tags$li("Helps identify exponential acceleration patterns."),
      tags$li("Useful for analyzing rapid changes in systems with compounding effects.")
    )
  )
}



################ Janoschek model #################
modfun_janoschek <- function(x, Asym, y0, k, m) {
  Asym - (Asym - y0) * exp(-k * x^m)
}

# First derivative
fdfun_janoschek <- function(x, Asym, y0, k, m) {
  (Asym - y0) * (exp(-k * x^m) * (k * (x^(m - 1) * m)))
}

# Second derivative
sdfun_janoschek <- function(x, Asym, y0, k, m) {
  (Asym - y0) * (exp(-k * x^m) * (k * (x^((m - 1) - 1) * (m - 1) *
                                         m)) - exp(-k * x^m) * (k * (x^(m - 1) * m)) * (k * (x^(m -
                                                                                                  1) * m)))
}

SSjanoschek <- selfStart(
  model = function(x, Asym, y0, k, m) {
    Asym - (Asym - y0) * exp(-k * x^m)
  },
  initial = function(mCall, data, LHS, ...) {
    x <- eval(mCall[["x"]], data)
    y <- eval(LHS, data)

    # Estimate Asym and y0
    Asym_init <- max(y)
    y0_init <- min(y)

    # Estimate k (growth rate)
    y_mid <- (Asym_init + y0_init) / 2  # Midpoint of y
    x_mid <- x[which.min(abs(y - y_mid))]  # Find x where y is closest to midpoint
    k_init <- -log((y_mid - Asym_init) / (y0_init - Asym_init)) / x_mid^2 # Assuming m ~ 2 initially

    # Estimate m (curvature)
    # Use numerical slope changes to estimate curvature
    delta_y <- diff(y)
    delta_x <- diff(x)
    slopes <- delta_y / delta_x
    max_slope_index <- which.max(slopes)  # Where the curve is steepest
    x_steep <- x[max_slope_index]
    m_init <- log(log((Asym_init - y0_init) / (Asym_init - y[max_slope_index])) / k_init)
    m_init <- ifelse(is.infinite(m_init), 1, m_init)

    # Return estimates
    c(Asym = Asym_init, y0 = y0_init, k = k_init, m = m_init)
  },
  parameters = c("Asym", "y0", "k", "m")
)



mod_janoschek <- function(data,
                          flight_date = "date",
                          predictor = "median.NDVI",
                          sowing_date = NULL,
                          parallel = FALSE) {
  # Prepare data
  dftemp <-
    data |>
    dplyr::mutate(unique_plot = paste0(block, "_", plot_id)) |>
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) |>
    dplyr::group_by(unique_plot) |>
    tidyr::nest()

  # Parallel or Sequential Plan
  if (parallel) {
    availablecl <- parallel::detectCores()
    future::plan(future::multisession, workers = round(availablecl * 0.75))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`

  # Fit model for each group
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows) %dofut% {
    df <- as.data.frame(dftemp$data[[i]])
    if (!is.null(sowing_date)) {
      flights <- as.POSIXlt(df$date)$yday + 1 - (as.POSIXlt(sowing_date)$yday)
    } else {
      flights <- as.POSIXlt(df$date)$yday + 1
    }

    fflight <- min(flights)
    lflight <- max(flights) + 20
    y <- df |> dplyr::pull(!!rlang::sym(predictor))
    x <- flights

    result <- tryCatch({
      # Fit Janoschek model
      model <- nls(y ~ SSjanoschek(x, Asym, y0, k, m),
                   control = nls.control(maxiter = 1000),
                   data = data.frame(x, y))

      coefs <- coef(model)
      gofval <- gof(model, y)

      # Area under curve (AUC)
      auc_result <- integrate(
        modfun_janoschek,
        lower = fflight,
        upper = lflight,
        Asym = coefs["Asym"],
        y0 = coefs["y0"],
        k = coefs["k"],
        m = coefs["m"]
      )

      # Critical points
      fdopt_result <- optimize(
        fdfun_janoschek,
        interval = c(fflight, lflight),
        maximum = TRUE,
        Asym = coefs["Asym"],
        y0 = coefs["y0"],
        k = coefs["k"],
        m = coefs["m"]
      )

      sdopt_result <- optimize(
        sdfun_janoschek,
        interval = c(fflight, lflight),
        maximum = TRUE,
        Asym = coefs["Asym"],
        y0 = coefs["y0"],
        k = coefs["k"],
        m = coefs["m"]
      )

      # Deceleration point (minimum slope)
      deceleration_result <- optimize(
        fdfun_janoschek,
        interval = c(fflight, lflight),
        maximum = FALSE,
        Asym = coefs["Asym"],
        y0 = coefs["y0"],
        k = coefs["k"],
        m = coefs["m"]
      )

      # Return results
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Janoschek",
        asymptote = coefs["Asym"],
        auc = auc_result$value,
        xinfp = fdopt_result$maximum,
        yinfp = fdopt_result$objective,
        xmace = sdopt_result$maximum,
        ymace = sdopt_result$objective,
        xmdes = deceleration_result$minimum,
        ymdes = deceleration_result$objective,
        aic = gofval[[1]],
        rmse = gofval[[2]],
        mae = gofval[[3]],
        parms = list(
          model = modfun_janoschek,
          modeladj = model,
          fd = fdfun_janoschek,
          sd = sdfun_janoschek,
          coefs = list(
            Asym = coefs["Asym"],
            y0 = coefs["y0"],
            k = coefs["k"],
            m = coefs["m"]),
          xmin = fflight,
          xmax = lflight
        )
      )
    }, error = function(e) {
      # Return NA values if model fails
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Janoschek",
        asymptote = NA_real_,
        auc = NA_real_,
        xinfp = NA_real_,
        yinfp = NA_real_,
        xmace = NA_real_,
        ymace = NA_real_,
        xmdes = NA_real_,
        ymdes = NA_real_,
        aic = NA_real_,
        rmse = NA_real_,
        mae = NA_real_,
        parms = NA
      )
    })
    result
  }
  # Final results table
  results <- results_list |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)

  return(results)
}
# Helper for Janoschek model equation
help_mod_janoschek_eq <- function() {
  "$$y(x) = \\text{Asym} - (\\text{Asym} - y_0) \\cdot e^{-k \\cdot x^m}$$"
}

# Helper for the first derivative
help_mod_janoschek_fd <- function() {
  "$$y'(x) = (\\text{Asym} - y_0) \\cdot e^{-k \\cdot x^m} \\cdot \\left(k \\cdot x^{m - 1} \\cdot m\\right)$$"
}

# Helper for the second derivative
help_mod_janoschek_sd <- function() {
  "$$y''(x) = (\\text{Asym} - y_0) \\cdot \\left[e^{-k \\cdot x^m} \\cdot \\left(k \\cdot x^{m - 1} \\cdot m\\right)^2 - e^{-k \\cdot x^m} \\cdot k \\cdot x^{m - 2} \\cdot m \\cdot (m - 1)\\right]$$"
}

# Full helper with explanation
help_mod_janoschek <- function() {
  div(
    style = "font-family: Arial, sans-serif; line-height: 1.5;",
    h2(style = "color: #2E86C1;", "Janoschek Growth Curve"),
    p("The Janoschek model is a flexible growth model that describes systems approaching an asymptote with decelerating growth.
       It is commonly used in biological systems, ecological modeling, and other growth-constrained processes."),
    p("The equation is given by:"),
    p(help_mod_janoschek_eq()),
    p(strong("Where:")),
    withMathJax(
      tags$ul(
        tags$li("\\(x\\): The independent variable (e.g., time)."),
        tags$li("\\(\\text{Asym}\\): The upper asymptote or maximum growth value."),
        tags$li("\\(y_0\\): The starting value of the system."),
        tags$li("\\(k\\): The growth rate constant."),
        tags$li("\\(m\\): The curvature parameter controlling the steepness of the growth curve.")
      )
    ),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Models growth systems that approach an asymptote."),
      tags$li("Captures nonlinear, decelerating growth patterns."),
      tags$li("Applicable to systems with constrained growth or limited capacity.")
    ),
    h2(style = "color: #2E86C1;", "First Derivative"),
    p("The first derivative of the Janoschek model describes the rate of change of the response variable. It shows how quickly the system is growing at any given point."),
    p("The first derivative is given by:"),
    p(help_mod_janoschek_fd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Describes the instantaneous growth rate of the system."),
      tags$li("Helps analyze growth dynamics over time."),
      tags$li("Highlights periods of rapid growth or saturation.")
    ),
    h2(style = "color: #2E86C1;", "Second Derivative"),
    p("The second derivative of the Janoschek model provides insight into the acceleration or deceleration of growth. It describes how the growth rate itself is changing."),
    p("The second derivative is given by:"),
    p(help_mod_janoschek_sd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Describes how growth acceleration changes over time."),
      tags$li("Identifies inflection points where growth transitions from acceleration to deceleration."),
      tags$li("Useful for analyzing saturation dynamics in constrained systems.")
    )
  )
}

################# Trans-Gompertz Growth Function ###################
modfun_transgompertz <- function(t, A, b, c) {
  A * exp(-b * exp(-c * t))
}

# First derivative of Trans-Gompertz model
fdfun_transgompertz <- function(t, A, b, c) {
  A * (exp(-b * exp(-c * t)) * (b * (exp(-c * t) * c)))
}

# Second derivative of Trans-Gompertz model
sdfun_transgompertz <- function(t, A, b, c) {
  A * (exp(-b * exp(-c * t)) * (b * (exp(-c * t) * c)) * (b * (exp(-c *
                                                                     t) * c)) - exp(-b * exp(-c * t)) * (b * (exp(-c * t) * c *
                                                                                                                c)))
}

SStransGompertz <- selfStart(
  model = function(t, A, b, c) {
    A * exp(-b * exp(-c * t))
  },
  initial = function(mCall, data, LHS, ...) {
    t <- eval(mCall[["t"]], data)
    y <- eval(LHS, data)

    # Initial guesses for parameters
    A_init <- max(y) # Asymptotic maximum

    # Estimate c (growth rate)
    mid_y <- A_init / 2 # Midpoint of the asymptote
    t_mid <- t[which.min(abs(y - mid_y))] # Time to reach 50% of A
    c_init <- log(2) / t_mid # Growth rate estimation

    # Estimate b (scale parameter)
    y_near_start <- y[1] # Initial observed value
    b_init <- -log(y_near_start / A_init) / exp(-c_init * t[1]) # Scale estimation
    print(c(A = A_init, b = b_init, c = c_init))
    c(A = A_init, b = b_init, c = c_init)
  },
  parameters = c("A", "b", "c")
)

mod_transgompertz <- function(data,
                              flight_date = "date",
                              predictor = "median.NDVI",
                              sowing_date = NULL,
                              parallel = FALSE) {
  # Prepare data
  dftemp <- data |>
    dplyr::mutate(unique_plot = paste0(block, "_", plot_id)) |>
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) |>
    dplyr::group_by(unique_plot) |>
    tidyr::nest()

  # Parallel or Sequential Plan
  if (parallel) {
    availablecl <- parallel::detectCores()
    future::plan(future::multisession, workers = round(availablecl * 0.75))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`

  # Fit model for each group
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows) %dofut% {
    df <- as.data.frame(dftemp$data[[i]])
    if (!is.null(sowing_date)) {
      flights <- as.POSIXlt(df$date)$yday + 1 - (as.POSIXlt(sowing_date)$yday)
    } else {
      flights <- as.POSIXlt(df$date)$yday + 1
    }

    fflight <- min(flights)
    lflight <- max(flights) + 20
    y <- df |> dplyr::pull(!!rlang::sym(predictor))
    x <- flights

    result <- tryCatch({
      # Fit Trans-Gompertz model
      model <- nls(y ~ SStransGompertz(x, A, b, c),
                   control = nls.control(maxiter = 1000),
                   data = data.frame(x, y))

      coefs <- coef(model)
      gofval <- gof(model, y)

      # Area under curve
      int1 <- integrate(modfun_transgompertz,
                        lower = fflight,
                        upper = lflight,
                        A = coefs[[1]],
                        b = coefs[[2]],
                        c = coefs[[3]])

      # critical points
      fdopt_result <- optimize(fdfun_transgompertz,
                               A = coefs[[1]],
                               b = coefs[[2]],
                               c = coefs[[3]],
                               interval = c(fflight, lflight),
                               maximum = TRUE)

      # maximum acceleration
      sdopt_result <- optimize(sdfun_transgompertz,
                               A = coefs[[1]],
                               b = coefs[[2]],
                               c = coefs[[3]],
                               interval = c(fflight, lflight),
                               maximum = TRUE)

      # maximum deceleration
      sdopt_result2 <- optimize(sdfun_transgompertz,
                                A = coefs[[1]],
                                b = coefs[[2]],
                                c = coefs[[3]],
                                interval = c(fflight, lflight),
                                maximum = FALSE)

      # Return results
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Trans-Gompertz",
        asymptote = coefs[[1]],
        auc = int1$value,
        xinfp = fdopt_result$maximum,
        yinfp = fdopt_result$objective,
        xmace = sdopt_result$maximum,
        ymace = sdopt_result$objective,
        xmdes = sdopt_result2$minimum,
        ymdes = sdopt_result2$objective,
        aic = gofval[[1]],
        rmse = gofval[[2]],
        mae = gofval[[3]],
        parms = list(model = modfun_transgompertz,
                     modeladj = model,
                     fd = fdfun_transgompertz,
                     sd = sdfun_transgompertz,
                     coefs = list(
                       A = coefs[[1]],
                       b = coefs[[2]],
                       c = coefs[[3]]
                     ),
                     xmin = fflight, xmax = lflight)
      )
    }, error = function(e) {
      # Return NA values if model fails
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Trans-Gompertz",
        asymptote = NA_real_,
        auc = NA_real_,
        xinfp = NA_real_,
        yinfp = NA_real_,
        xmace = NA_real_,
        ymace = NA_real_,
        xmdes = NA_real_,
        ymdes = NA_real_,
        aic = NA_real_,
        rmse = NA_real_,
        mae = NA_real_,
        parms = NA
      )
    })
    result
  }
  results <- results_list |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)

  return(results)
}

# Trans-Gompertz model equation
help_mod_transgompertz_eq <- function() {
  "$$y(t) = A \\cdot e^{-b \\cdot e^{-c \\cdot t}}$$"
}

# First derivative of Trans-Gompertz
help_mod_transgompertz_fd <- function() {
  "$$y'(t) = A \\cdot e^{-b \\cdot e^{-c \\cdot t}} \\cdot b \\cdot e^{-c \\cdot t} \\cdot c$$"
}

# Second derivative of Trans-Gompertz
help_mod_transgompertz_sd <- function() {
  "$$y''(t) = A \\cdot e^{-b \\cdot e^{-c \\cdot t}} \\cdot \\left[b \\cdot e^{-c \\cdot t} \\cdot c \\cdot \\left(b \\cdot e^{-c \\cdot t} \\cdot c - 1\\right)\\right]$$"
}

# Description and comparison of Trans-Gompertz model
help_mod_transgompertz <- function() {
  div(
    style = "font-family: Arial, sans-serif; line-height: 1.5;",
    h2(style = "color: #2E86C1;", "Trans-Gompertz Growth Model"),
    p("The Trans-Gompertz growth model is widely used to describe growth processes, particularly those exhibiting asymmetry. It assumes growth is constrained by an upper limit \\(A\\), with a decelerating growth rate as time increases."),
    p("The model equation is:"),
    p(help_mod_transgompertz_eq()),
    p(strong("Where:")),
    withMathJax(
      tags$ul(
        tags$li("\\(t\\): The time or independent variable."),
        tags$li("\\(A\\): The asymptotic maximum size (carrying capacity)."),
        tags$li("\\(b\\): The scale parameter, controlling the curve's shape."),
        tags$li("\\(c\\): The growth rate parameter.")
      )
    ),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Accounts for asymmetric growth dynamics."),
      tags$li("Approaches an upper limit \\(A\\) over time."),
      tags$li("Useful for describing biological growth processes."),
      tags$li("Captures varying growth rates over time."),
    ),
    h2(style = "color: #2E86C1;", "First Derivative"),
    p("The first derivative describes the instantaneous growth rate, capturing how quickly the response variable is changing at any given time."),
    p("The first derivative is given by:"),
    p(help_mod_transgompertz_fd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Shows the peak growth rate over time."),
      tags$li("Helps identify when growth begins to decelerate."),
      tags$li("Useful for studying growth dynamics."),
    ),
    h2(style = "color: #2E86C1;", "Second Derivative"),
    p("The second derivative captures the curvature of the growth curve, indicating whether growth is accelerating or decelerating."),
    p("The second derivative is given by:"),
    p(help_mod_transgompertz_sd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Describes how the growth rate changes over time."),
      tags$li("Identifies points of maximum acceleration and deceleration."),
      tags$li("Useful for understanding growth patterns.")
    ),
    h2(style = "color: #2E86C1;", "Comparison to Logistic and Gompertz Models"),
    p("The Trans-Gompertz model is related to the logistic and Gompertz models but differs in flexibility and application:"),
    tags$ul(
      tags$li(
        strong("Logistic Model:"),
        " The logistic growth model assumes symmetric growth with a fixed inflection point at half the carrying capacity \\(A/2\\). Its equation is:",
        withMathJax("$$y(t) = \\frac{A}{1 + e^{-b \\cdot (t - t_0)}}$$")
      ),
      tags$li(
        strong("Gompertz Model:"),
        " The Gompertz model assumes an asymmetric growth process where the inflection point occurs earlier than in the logistic model. Its equation is:",
        withMathJax("$$y(t) = A \\cdot e^{-b \\cdot e^{-c \\cdot t}}$$")
      ),
      tags$li(
        strong("Trans-Gompertz Model:"),
        " Extends the Gompertz model by adding a scaling parameter \\(b\\), allowing more flexibility to capture diverse growth patterns."
      )
    ),
    p(strong("Key Differences:")),
    tags$ul(
      tags$li("The logistic model assumes symmetry, while the Gompertz and Trans-Gompertz models are asymmetric."),
      tags$li("The Trans-Gompertz model provides greater flexibility by incorporating the scaling parameter \\(b\\)."),
      tags$li("The Gompertz and Trans-Gompertz models are particularly suited for growth processes with early rapid growth and a slow approach to the asymptote.")
    )
  )
}


################# Sinusoidal Growth Model ###################
################# Sinusoidal Model ###################

# Model function
modfun_sinusoidal <- function(t, y0, a, b, c) {
  y0 + a * sin((2 * pi * t) / b + c)
}

# First derivative
fdfun_sinusoidal <- function(t, y0, a, b, c) {
  a * (cos((2 * pi * t)/b + c) * (2 * pi/b))
}

# Second derivative
sdfun_sinusoidal <- function(t, y0, a, b, c) {
  -(a * (sin((2 * pi * t)/b + c) * (2 * pi/b) * (2 * pi/b)))
}

# Self-start sinusoidal model
SSsinusoidal <- selfStart(
  model = function(t, y0, a, b, c) {
    y0 + a * sin((2 * pi * t) / b + c)
  },
  initial = function(mCall, data, LHS, ...) {
    t <- eval(mCall[["t"]], data)
    y <- eval(LHS, data)

    # Initial guesses for parameters
    y0_init <- mean(y) # Baseline
    a_init <- (max(y) - min(y)) / 2 # Amplitude
    b_init <- diff(range(t)) / 2 # Approximate period (half the time range)
    c_init <- 0 # Phase shift (default to 0)

    c(y0 = y0_init, a = a_init, b = b_init, c = c_init)
  },
  parameters = c("y0", "a", "b", "c")
)


mod_sinusoidal <- function(data,
                           flight_date = "date",
                           predictor = "median.NDVI",
                           sowing_date = NULL,
                           parallel = FALSE) {
  # Prepare data
  dftemp <- data |>
    dplyr::mutate(unique_plot = paste0(block, "_", plot_id)) |>
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) |>
    dplyr::group_by(unique_plot) |>
    tidyr::nest()

  # Parallel or Sequential Plan
  if (parallel) {
    availablecl <- parallel::detectCores()
    future::plan(future::multisession, workers = round(availablecl * 0.75))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`

  # Fit model for each group
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows) %dofut% {
    df <- as.data.frame(dftemp$data[[i]])
    if (!is.null(sowing_date)) {
      flights <- as.POSIXlt(df$date)$yday + 1 - (as.POSIXlt(sowing_date)$yday)
    } else {
      flights <- as.POSIXlt(df$date)$yday + 1
    }

    fflight <- min(flights)
    lflight <- max(flights) + 20
    y <- df |> dplyr::pull(!!rlang::sym(predictor))
    x <- flights

    result <- tryCatch({
      # Fit Sinusoidal model
      model <- nls(y ~ SSsinusoidal(x, y0, a, b, c),
                   control = nls.control(maxiter = 1000),
                   data = data.frame(x, y))

      coefs <- coef(model)
      gofval <- gof(model, y)

      # Critical points
      fdopt_result <- optimize(fdfun_sinusoidal,
                               y0 = coefs[[1]],
                               a = coefs[[2]],
                               b = coefs[[3]],
                               c = coefs[[4]],
                               interval = c(fflight, lflight),
                               maximum = TRUE)

      # Return results
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Sinusoidal",
        baseline = coefs[[1]],
        amplitude = coefs[[2]],
        period = coefs[[3]],
        phase_shift = coefs[[4]],
        xinfp = fdopt_result$maximum,
        yinfp = fdopt_result$objective,
        aic = gofval[[1]],
        rmse = gofval[[2]],
        mae = gofval[[3]],
        parms = list(model = modfun_sinusoidal,
                     modeladj = model,
                     fd = fdfun_sinusoidal,
                     sd = sdfun_sinusoidal,
                     coefs = list(
                       y0 = coefs[[1]],
                       a = coefs[[2]],
                       b = coefs[[3]],
                       c = coefs[[4]]
                     ),
                     xmin = fflight, xmax = lflight)
      )
    }, error = function(e) {
      # Return NA values if model fails
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Sinusoidal",
        baseline = NA_real_,
        amplitude = NA_real_,
        period = NA_real_,
        phase_shift = NA_real_,
        xinfp = NA_real_,
        yinfp = NA_real_,
        aic = NA_real_,
        rmse = NA_real_,
        mae = NA_real_,
        parms = NA
      )
    })
    result
  }
  results <- results_list |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)

  return(results)
}

# Sinusoidal model equation
help_mod_sinusoidal_eq <- function() {
  "$$y(t) = y_0 + a \\cdot \\sin\\left(\\frac{2\\pi t}{b} + c\\right)$$"
}

# First derivative of Sinusoidal model
help_mod_sinusoidal_fd <- function() {
  "$$y'(t) = a \\cdot \\cos\\left(\\frac{2\\pi t}{b} + c\\right) \\cdot \\frac{2\\pi}{b}$$"
}

# Second derivative of Sinusoidal model
help_mod_sinusoidal_sd <- function() {
  "$$y''(t) = -a \\cdot \\sin\\left(\\frac{2\\pi t}{b} + c\\right) \\cdot \\left(\\frac{2\\pi}{b}\\right)^2$$"
}

# Description and comparison of Sinusoidal model
help_mod_sinusoidal <- function() {
  div(
    style = "font-family: Arial, sans-serif; line-height: 1.5;",
    h2(style = "color: #2E86C1;", "Sinusoidal Growth Model"),
    p("The sinusoidal growth model is used to describe oscillatory or cyclic processes, such as seasonal variations in plant growth or periodic environmental changes."),
    p("The model equation is:"),
    p(help_mod_sinusoidal_eq()),
    p(strong("Where:")),
    withMathJax(
      tags$ul(
        tags$li("\\(t\\): The time or independent variable."),
        tags$li("\\(y_0\\): The baseline or mean value."),
        tags$li("\\(a\\): The amplitude (maximum deviation from baseline)."),
        tags$li("\\(b\\): The period (time for one complete oscillation)."),
        tags$li("\\(c\\): The phase shift (horizontal displacement of the curve).")
      )
    ),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Captures cyclic or oscillatory dynamics."),
      tags$li("Useful for modeling periodic phenomena in biology or ecology."),
      tags$li("Includes parameters for amplitude, baseline, period, and phase shift."),
    ),
    h2(style = "color: #2E86C1;", "First Derivative"),
    p("The first derivative describes the rate of change at any point in the cycle, providing insight into peak and trough dynamics."),
    p("The first derivative is given by:"),
    p(help_mod_sinusoidal_fd()),
    h2(style = "color: #2E86C1;", "Second Derivative"),
    p("The second derivative describes the curvature of the sinusoidal curve, highlighting points of maximum acceleration or deceleration."),
    p("The second derivative is given by:"),
    p(help_mod_sinusoidal_sd()),
    h2(style = "color: #2E86C1;", "Applications"),
    tags$ul(
      tags$li("Seasonal variations in crop growth."),
      tags$li("Cyclic environmental or biological processes."),
      tags$li("Modeling periodic plant responses to environmental factors.")
    )
  )
}
