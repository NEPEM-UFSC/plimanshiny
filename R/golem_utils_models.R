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
  b0 * (exp((b1 - x)/b2) * (1/b2))/(1 + exp((b1 - x)/b2))^2
}
# second derivative
sdfun_L3 <- function(x, b0, b1, b2) {
  -(b0 * (exp((b1 - x)/b2) * (1/b2) * (1/b2))/(1 + exp((b1 - x)/b2))^2 -
      b0 * (exp((b1 - x)/b2) * (1/b2)) * (2 * (exp((b1 - x)/b2) *
                                                 (1/b2) * (1 + exp((b1 - x)/b2))))/((1 + exp((b1 - x)/b2))^2)^2)
}

mod_L3 <- function(data,
                   flight_date = "date",
                   predictor = "median.NGRDI",
                   sowing_date = NULL,
                   parallel = FALSE) {
  dftemp <-
    data |>
    dplyr::mutate(unique_plot = paste0(block, "_", plot_id)) |>
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) |>
    dplyr::group_by(unique_plot) |>
    tidyr::nest()

  # Parallel or Sequential Plan
  if (parallel) {
    future::plan(future::multisession, workers = max(1, parallel::detectCores() %/% 4))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows, .options.future = list(seed = TRUE)) %dofut% {
    df <- as.data.frame(dftemp$data[[i]])

    tryCatch({
      if (!is.null(sowing_date)) {
        flights <- as.POSIXlt(df[[flight_date]])$yday + 1 - (as.POSIXlt(sowing_date)$yday + 1)
      } else {
        flights <- as.POSIXlt(df[[flight_date]])$yday + 1
      }

      fflight <- min(flights)
      lflight <- max(flights) + 20
      flights_seq <- fflight:lflight
      y <- df |> dplyr::pull(!!rlang::sym(predictor))

      # Logistic regression to predict median.NDVI as a function of flights
      model <- try(nls(y ~ SSlogis(flights, Asym, xmid, scal),
                       control = nls.control(maxiter = 1000),
                       data = data.frame(flights, y)),
                   silent = TRUE)

      if (inherits(model, "try-error")) {
        model <- suppressWarnings(
          minpack.lm::nlsLM(y ~ SSlogis(flights, Asym, xmid, scal),
                            data = data.frame(flights, y))
        )
      }

      coefslog <- coef(model)
      b0 <- coefslog["Asym"]
      b1 <- coefslog["xmid"]
      b2 <- coefslog["scal"]

      # Critical points
      xseq <- seq(fflight, lflight, length.out = 5000)
      cp1 <- xseq[which.min(sdfun_L3(xseq, b0, b1, b2))]
      cp2 <- xseq[which.max(sdfun_L3(xseq, b0, b1, b2))]

      xfd <- seq(min(flights), ceiling(cp1), length.out = 500)
      yfd <- sdfun_L3(xfd, b0, b1, b2)

      dfreg <- data.frame(x = c(min(xfd), max(xfd)), y = c(max(yfd), min(yfd)))
      regmod <- lm(y ~ x, data = dfreg)
      predline <- predict(regmod, newdata = data.frame(x = xfd))
      head <- xfd[which.max(abs(yfd - predline))]

      int1 <- integrate(modfun_L3, lower = fflight, upper = lflight, b0 = b0, b1 = b1, b2 = b2)
      int2 <- integrate(modfun_L3, lower = head, upper = cp2, b0 = b0, b1 = b1, b2 = b2)
      int3 <- integrate(modfun_L3, lower = fflight, upper = head, b0 = b0, b1 = b1, b2 = b2)

      dplyr::tibble(
        unique_plot = dftemp$unique_plot[i],
        b0 = b0,
        b1 = b1,
        b2 = b2,
        heading = head,
        inflection = b1,
        maturity = cp2,
        repr_period = cp2 - head,
        auc = int1$value,
        auc_vege_period = int3$value,
        auc_repr_period = int2$value,
        parms = list(
          model = modfun_L3,
          modeladj = model,
          fd = fdfun_L3,
          sd = sdfun_L3,
          coefs = list(b0 = b0, b1 = b1, b2 = b2),
          xmin = fflight,
          xmax = lflight
        )
      )
    }, error = function(e) {
      # Handle errors in model fitting or calculations
      dplyr::tibble(
        unique_plot = dftemp$unique_plot[i],
        b0 = NA_real_,
        b1 = NA_real_,
        b2 = NA_real_,
        heading = NA_real_,
        inflection = NA_real_,
        maturity = NA_real_,
        repr_period = NA_real_,
        auc = NA_real_,
        auc_vege_period = NA_real_,
        auc_repr_period = NA_real_,
        parms = NA
      )
    })
  }
  results <- results_list |>
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



########### LOGISTIC MODEL 4 PARAMETERS #############
modfun_L4 <- function(x, a, b, xmid, scal) {
  a + (b - a) / (1 + exp((xmid - x) / scal))
}

# Derivatives
fdfun_L4 <- function(x, a, b, xmid, scal) {

  (b - a) * (exp((xmid - x)/scal) * (1/scal))/(1 + exp((xmid -
                                                          x)/scal))^2
}

sdfun_L4 <- function(x, a, b, xmid, scal) {
  -((b - a) * (exp((xmid - x)/scal) * (1/scal) * (1/scal))/(1 +
                                                              exp((xmid - x)/scal))^2 - (b - a) * (exp((xmid - x)/scal) *
                                                                                                     (1/scal)) * (2 * (exp((xmid - x)/scal) * (1/scal) * (1 +
                                                                                                                                                            exp((xmid - x)/scal))))/((1 + exp((xmid - x)/scal))^2)^2)
}

mod_L4 <- function(data,
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
    future::plan(future::multisession, workers = max(1, parallel::detectCores() %/% 4))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`

  # Fit model for each group
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows, .options.future = list(seed = TRUE)) %dofut% {
    df <- as.data.frame(dftemp$data[[i]])
    tryCatch({
      # Compute flight days
      if (!is.null(sowing_date)) {
        flights <- as.POSIXlt(df[[flight_date]])$yday + 1 - (as.POSIXlt(sowing_date)$yday + 1)
      } else {
        flights <- as.POSIXlt(df[[flight_date]])$yday + 1
      }

      fflight <- min(flights)
      lflight <- max(flights) + 20
      y <- df |> dplyr::pull(!!rlang::sym(predictor))

      # Fit the L4 model
      model <- nls(
        y ~ SSfpl(flights, A, B, xmid, scal),
        data = data.frame(x = flights, y = y),
        control = minpack.lm::nls.lm.control(maxiter = 1000)
      )

      model <- try(
        nls( y ~ SSfpl(flights, A, B, xmid, scal),
            data = data.frame(flights, y),
            control = nls.control(maxiter = 1000)),
        silent = TRUE
      )

      if (inherits(model, "try-error")) {
        model <- suppressWarnings(
          minpack.lm::nlsLM(y ~ SSfpl(flights, A, B, xmid, scal),
                            data = data.frame(flights, y))
        )
      }

      coefslog <- coef(model)
      a <- coefslog[1]
      b <- coefslog[2]
      xmid <- coefslog[3]
      scal <- coefslog[4]

      # Critical points
      cp1 <- optimise(sdfun_L4, interval = c(fflight, lflight), a = a, b = b, xmid = xmid, scal = scal, maximum = FALSE)
      cp2 <- optimise(sdfun_L4, interval = c(fflight, lflight), a = a, b = b, xmid = xmid, scal = scal, maximum = TRUE)

      # Heading and maturity
      xfd <- seq(min(flights), ceiling(cp1$minimum), length.out = 500)
      yfd <- sdfun_L4(xfd, a, b, xmid, scal)
      dfreg <- data.frame(x = c(min(xfd), max(xfd)), y = c(max(yfd), min(yfd)))
      regmod <- lm(y ~ x, data = dfreg)
      predline <- predict(regmod, newdata = data.frame(x = xfd))
      head <- xfd[which.max(abs(yfd - predline))]

      xfd2 <- seq(ceiling(xmid), lflight, length.out = 500)
      yfd2 <- fdfun_L4(xfd2, a, b, xmid, scal)
      dfreg2 <- data.frame(x = c(min(xfd2), max(xfd2)), y = c(min(yfd2), max(yfd2)))
      regmod2 <- lm(y ~ x, data = dfreg2)
      predline2 <- predict(regmod2, newdata = data.frame(x = xfd2))
      maturation <- xfd2[which.max(abs(yfd2 - predline2))]

      # Area under curve
      int1 <- integrate(modfun_L4, lower = fflight, upper = lflight, a = a, b = b, xmid = xmid, scal = scal)
      int2 <- integrate(modfun_L4, lower = head, upper = maturation, a = a, b = b, xmid = xmid, scal = scal)

      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        a = a,
        b = b,
        inflection = xmid,
        scal = scal,
        heading = head,
        maturity = maturation,
        repr_period = maturation - head,
        auc = int1$value,
        auc_repr_period = int2$value,
        parms = list(
          model = modfun_L4,
          modeladj = model,
          fd = fdfun_L4,
          sd = sdfun_L4,
          coefs = list(a = a, b = b, xmid = xmid, scal = scal),
          xmin = fflight,
          xmax = lflight
        )
      )
    }, error = function(e) {
      # Return NA values if model fitting fails
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        a = NA_real_,
        b = NA_real_,
        inflection = NA_real_,
        scal = NA_real_,
        heading = NA_real_,
        maturity = NA_real_,
        repr_period = NA_real_,
        auc = NA_real_,
        auc_repr_period = NA_real_,
        parms = NA
      )
    })
  }

  # Final results table
  results <- results_list |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)

  return(results)
}


help_mod_L4_eq <- function() {
  "$$y(x) = a + \\frac{b - a}{1 + \\exp\\left(\\frac{x_{mid} - x}{scal}\\right)}$$"
}

help_mod_L4_fd <- function() {
  "$$y'(x) = \\frac{(b - a) \\cdot \\exp\\left(\\frac{x_{mid} - x}{scal}\\right) \\cdot \\frac{1}{scal}}{\\left(1 + \\exp\\left(\\frac{x_{mid} - x}{scal}\\right)\\right)^2}$$"
}

help_mod_L4_sd <- function() {
  "$$y''(x) = -\\left(\\frac{(b - a) \\cdot \\exp\\left(\\frac{x_{mid} - x}{scal}\\right) \\cdot \\frac{1}{scal}^2}{\\left(1 + \\exp\\left(\\frac{x_{mid} - x}{scal}\\right)\\right)^2} +
  \\frac{(b - a) \\cdot \\exp\\left(\\frac{x_{mid} - x}{scal}\\right) \\cdot \\frac{1}{scal} \\cdot \\left(2 \\cdot \\exp\\left(\\frac{x_{mid} - x}{scal}\\right) \\cdot \\frac{1}{scal} \\cdot \\left(1 + \\exp\\left(\\frac{x_{mid} - x}{scal}\\right)\\right)\\right)}{\\left(\\left(1 + \\exp\\left(\\frac{x_{mid} - x}{scal}\\right)\\right)^2\\right)^2}\\right)$$"
}

help_mod_L4 <- function() {
  div(
    style = "font-family: Arial, sans-serif; line-height: 1.5;",
    h2(style = "color: #2E86C1;", "Description of Returned Variables"),
    tags$ul(
      tags$li(tags$b("block:"), " The identifier for the experimental block."),
      tags$li(tags$b("plot_id:"), " The unique identifier for individual plots."),
      tags$li(tags$b("unique_plot:"), " A combined identifier uniquely identifying each plot."),
      tags$li(tags$b("a:"), " The lower asymptote of the curve, representing the minimum response value."),
      tags$li(tags$b("b:"), " The upper asymptote of the curve, representing the maximum response value."),
      tags$li(tags$b("xmid:"), " The inflection point, where the rate of change in the response variable is maximal."),
      tags$li(tags$b("scal:"), " A scale parameter controlling the steepness of the curve."),
      tags$li(tags$b("inflection:"), " The x-value at the inflection point."),
      tags$li(tags$b("auc:"), " The area under the curve (AUC), representing the accumulated response."),
      tags$li(tags$b("xinfp:"), " The x-value at the inflection point."),
      tags$li(tags$b("yinfp:"), " The y-value at the inflection point."),
      tags$li(tags$b("xmace:"), " The x-value where the second derivative is maximal, indicating maximum acceleration."),
      tags$li(tags$b("ymace:"), " The y-value at the maximum acceleration point."),
      tags$li(tags$b("xmdes:"), " The x-value where the second derivative is minimal, indicating maximum deceleration."),
      tags$li(tags$b("ymdes:"), " The y-value at the maximum deceleration point.")
    ),
    h2(style = "color: #2E86C1;", "Growth Curve"),
    p("The four-parameter logistic model (4PL) captures sigmoidal growth patterns, often observed in biological systems. It is characterized by lower and upper asymptotes, an inflection point, and a steepness parameter."),
    p("The equation is given by:"),
    p(help_mod_L4_eq()),
    p(strong("Where:")),
    withMathJax(
      tags$ul(
        tags$li("\\(x\\): The independent variable, typically time."),
        tags$li("\\(a\\): The lower asymptote, representing the minimal response."),
        tags$li("\\(b\\): The upper asymptote, representing the maximal response."),
        tags$li("\\(x_{mid}\\): The inflection point."),
        tags$li("\\(scal\\): The scale parameter controlling steepness.")
      )
    ),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Models growth curves that start slow, accelerate, and plateau."),
      tags$li("Represents initial, maximal, and inflection characteristics."),
      tags$li("Applicable to growth studies, population dynamics, and more.")
    ),
    h2(style = "color: #2E86C1;", "First Derivative"),
    p("The first derivative describes the rate of change of the response variable:"),
    p(help_mod_L4_fd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Captures the instantaneous growth rate."),
      tags$li("Useful for identifying inflection points."),
      tags$li("Helps in understanding temporal growth dynamics.")
    ),
    h2(style = "color: #2E86C1;", "Second Derivative"),
    p("The second derivative provides insight into the acceleration or deceleration of growth:"),
    p(help_mod_L4_sd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Identifies regions of rapid acceleration or deceleration."),
      tags$li("Highlights critical phases of growth or decline."),
      tags$li("Useful for studying dynamic growth processes.")
    )
  )
}




############### Logistic model 5 PARAMETERS #########
modfun_L5 <- function(x, asym1, asym2, xmid, iscal, theta) {
  asym2 + (asym1 - asym2)/(1 + exp(iscal * (log(x) - log(xmid))))^theta
}
fdfun_L5 <- function(x, asym1, asym2, xmid, iscal, theta){
  -((asym1 - asym2) * ((1 + exp(iscal * (log(x) - log(xmid))))^(theta -
                                                                  1) * (theta * (exp(iscal * (log(x) - log(xmid))) * (iscal *
                                                                                                                        (1/x)))))/((1 + exp(iscal * (log(x) - log(xmid))))^theta)^2)
}
sdfun_L5 <- function(x, asym1, asym2, xmid, iscal, theta){
  -((asym1 - asym2) * ((1 + exp(iscal * (log(x) - log(xmid))))^((theta -
                                                                   1) - 1) * ((theta - 1) * (exp(iscal * (log(x) - log(xmid))) *
                                                                                               (iscal * (1/x)))) * (theta * (exp(iscal * (log(x) - log(xmid))) *
                                                                                                                               (iscal * (1/x)))) + (1 + exp(iscal * (log(x) - log(xmid))))^(theta -
                                                                                                                                                                                              1) * (theta * (exp(iscal * (log(x) - log(xmid))) * (iscal *
                                                                                                                                                                                                                                                    (1/x)) * (iscal * (1/x)) - exp(iscal * (log(x) - log(xmid))) *
                                                                                                                                                                                                               (iscal * (1/x^2)))))/((1 + exp(iscal * (log(x) - log(xmid))))^theta)^2 -
      (asym1 - asym2) * ((1 + exp(iscal * (log(x) - log(xmid))))^(theta -
                                                                    1) * (theta * (exp(iscal * (log(x) - log(xmid))) * (iscal *
                                                                                                                          (1/x))))) * (2 * ((1 + exp(iscal * (log(x) - log(xmid))))^(theta -
                                                                                                                                                                                       1) * (theta * (exp(iscal * (log(x) - log(xmid))) * (iscal *
                                                                                                                                                                                                                                             (1/x)))) * ((1 + exp(iscal * (log(x) - log(xmid))))^theta)))/(((1 +
                                                                                                                                                                                                                                                                                                               exp(iscal * (log(x) - log(xmid))))^theta)^2)^2)
}

SSlogis5 <- selfStart(
  model = function(x, asym1, asym2, xmid, iscal, theta) {
    # Validate input
    if (any(x < 0)) {
      stop("Input (x) should be positive for this equation", call. = FALSE)
    }

    # Model equation
    .lxmid <- suppressWarnings(log(xmid))
    .expre1 <- 1 + exp(iscal * (log(x) - .lxmid))
    .expre2 <- .expre1^theta
    .expre3 <- (asym1 - asym2) / .expre2
    .value <- asym2 + .expre3
    .value <- ifelse(is.nan(.value), 0, .value)

    # Gradients
    .expr8 <- (1 + exp(iscal * (log(x) - .lxmid)))^theta
    .exp1 <- ifelse(is.nan(.expr8), 0, 1 / .expr8)
    .exp2 <- ifelse(is.nan(.expr8), 0, 1 - 1 / .expr8)

    .expr1 <- asym1 - asym2
    .expr6 <- exp(iscal * (log(x) - .lxmid))
    .expr7 <- 1 + .expr6
    .exp3 <- .expr1 * (.expr7^(theta - 1) * (theta * (.expr6 * (iscal * (1 / xmid))))) / .expr8^2
    .exp3 <- ifelse(is.nan(.exp3), 0, .exp3)

    .expr4 <- log(x) - .lxmid
    .exp4 <- -(.expr1 * (.expr7^(theta - 1) * (theta * (.expr6 * .expr4))) / .expr8^2)
    .exp4 <- ifelse(is.nan(.exp4), 0, .exp4)

    .exp5 <- -(.expr1 * (.expr8 * log(.expr7)) / .expr8^2)
    .exp5 <- ifelse(is.nan(.exp5), 0, .exp5)

    .actualArgs <- as.list(match.call()[c("asym1", "asym2", "xmid", "iscal", "theta")])

    # Gradient matrix
    if (all(unlist(lapply(.actualArgs, is.name)))) {
      .grad <- array(0, c(length(.value), 5L), list(NULL, c("asym1", "asym2", "xmid", "iscal", "theta")))
      .grad[, "asym1"] <- .exp1
      .grad[, "asym2"] <- .exp2
      .grad[, "xmid"] <- .exp3
      .grad[, "iscal"] <- .exp4
      .grad[, "theta"] <- .exp5
      dimnames(.grad) <- list(NULL, .actualArgs)
      attr(.value, "gradient") <- .grad
    }
    .value
  },
  initial = function(mCall, LHS, data, ...) {
    # Initialization logic
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    if (nrow(xy) < 5) {
      stop("Too few distinct input values to fit a logis5")
    }
    asym1 <- xy[1, "y"]
    asym2 <- xy[nrow(xy), "y"]
    xmid <- NLSstClosestX(xy, mean(c(asym1, asym2)))
    iscal <- 1 / (max(xy[,"x"], na.rm = TRUE) - xmid)
    theta <- 1

    objfun <- function(cfs) {
      pred <- SSlogis5(xy[,"x"], asym1 = cfs[1], asym2 = cfs[2], xmid = cfs[3], iscal = cfs[4], theta = cfs[5])
      ans <- sum((xy[,"y"] - pred)^2)
      ans
    }
    cfs <- c(asym1, asym2, xmid, iscal, theta)
    op <- try(stats::optim(cfs, objfun), silent = TRUE)

    if (!inherits(op, "try-error")) {
      asym1 <- op$par[1]
      asym2 <- op$par[2]
      xmid <- op$par[3]
      iscal <- op$par[4]
      theta <- op$par[5]
    }

    value <- c(asym1, asym2, xmid, iscal, theta)
    names(value) <- mCall[c("asym1", "asym2", "xmid", "iscal", "theta")]
    value
  },
  parameters = c("asym1", "asym2", "xmid", "iscal", "theta")
)

mod_L5 <- function(data,
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
    future::plan(future::multisession, workers = max(1, parallel::detectCores() %/% 4))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows, .options.future = list(seed = TRUE)) %dofut% {
    df <- as.data.frame(dftemp$data[[i]])

    tryCatch({
      if (!is.null(sowing_date)) {
        flights <- as.POSIXlt(df[[flight_date]])$yday + 1 - (as.POSIXlt(sowing_date)$yday + 1)
      } else {
        flights <- as.POSIXlt(df[[flight_date]])$yday + 1
      }

      fflight <- min(flights)
      lflight <- max(flights) + 20
      y <- df |> dplyr::pull(!!rlang::sym(predictor))

      # Fit the logistic model with 5 parameters
      model <- try(
        nls(y ~ SSlogis5(flights, asym1, asym2, xmid, iscal, theta),
            data = data.frame(flights, y),
            control = nls.control(maxiter = 1000)),
        silent = TRUE
      )

      if (inherits(model, "try-error")) {
        model <- suppressWarnings(
          minpack.lm::nlsLM(y ~ SSlogis5(flights, asym1, asym2, xmid, iscal, theta),
                            data = data.frame(flights, y))
        )
      }

      coefs <- coef(model)
      asym1 <- coefs["asym1"]
      asym2 <- coefs["asym2"]
      xmid <- coefs["xmid"]
      iscal <- coefs["iscal"]
      theta <- coefs["theta"]
      indexdecrease <- asym2 < asym1
      # Critical Points
      inflec <- suppressWarnings(
        optimise(
          fdfun_L5,
          interval = c(fflight, lflight),
          iscal = iscal,
          asym1 = asym1,
          asym2 = asym2,
          xmid = xmid,
          theta = theta,
          maximum = !indexdecrease
        )
      )

      cp1 <-
        suppressWarnings(
          optimise(
            sdfun_L5,
            interval = c(fflight, lflight),
            iscal = iscal,
            asym1 = asym1,
            asym2 = asym2,
            xmid = xmid,
            theta = theta,
            maximum = !indexdecrease
          )
        )

      cp2 <-
        suppressWarnings(
          optimise(
            sdfun_L5,
            interval = c(fflight, lflight),
            iscal = iscal,
            asym1 = asym1,
            asym2 = asym2,
            xmid = xmid,
            theta = theta,
            maximum = indexdecrease
          )
        )

      # Heading
      xfd <- seq(min(flights), ceiling(cp1$minimum), length.out = 500)
      yfd <- sdfun_L5(xfd, asym1, asym2, xmid, iscal,  theta)
      dfreg <- data.frame(x = c(min(xfd), max(xfd)), y = c(max(yfd), min(yfd)))
      regmod <- lm(y ~ x, data = dfreg)
      predline <- predict(regmod, newdata = data.frame(x = xfd))
      head <- xfd[which.max(abs(yfd - predline))]

      # Maturation
      xfd2 <- seq(inflec[[1]], lflight, length.out = 500)
      yfd2 <- fdfun_L5(xfd2, asym1, asym2, xmid, iscal, theta)
      dfreg2 <- data.frame(x = c(min(xfd2), max(xfd2)), y = c(min(yfd2), max(yfd2)))
      regmod2 <- lm(y ~ x, data = dfreg2)
      predline2 <- predict(regmod2, newdata = data.frame(x = xfd2))
      maturation <- xfd2[which.max(abs(yfd2 - predline2))]

      # Integrate AUC
      int1 <- integrate(
        modfun_L5,
        lower = fflight,
        upper = lflight,
        asym1 = asym1,
        asym2 = asym2,
        xmid = xmid,
        iscal = iscal,
        theta = theta
      )

      int2 <- integrate(
        modfun_L5,
        lower = head,
        upper = maturation,
        asym1 = asym1,
        asym2 = asym2,
        xmid = xmid,
        iscal = iscal,
        theta = theta
      )

      # Return results
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        asym1 = asym1,
        asym2 = asym2,
        xmid = xmid,
        iscal = iscal,
        theta = theta,
        inflection = inflec[[1]],
        heading = head,
        maturity = maturation,
        repr_period = maturation - head,
        auc = int1$value,
        auc_repr_period = int2$value,
        parms = list(
          model = modfun_L5,
          modeladj = model,
          fd = fdfun_L5,
          sd = sdfun_L5,
          coefs = list(
            asym1 = asym1,
            asym2 = asym2,
            xmid = xmid,
            iscal = iscal,
            theta = theta
          ),
          xmin = fflight,
          xmax = lflight
        )
      )
    }, error = function(e) {
      # Return NA values if the model fails
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        asym1 = NA_real_,
        asym2 = NA_real_,
        xmid = NA_real_,
        iscal = NA_real_,
        theta = NA_real_,
        inflection = NA_real_,
        heading = NA_real_,
        maturity = NA_real_,
        repr_period = NA_real_,
        auc = NA_real_,
        auc_repr_period = NA_real_,
        parms = NA
      )
    })
  }

  # Finalize results
  results <- results_list |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)

  return(results)
}



help_mod_L5_eq <- function() {
  "$$y(x) = asym2 + \\frac{asym1 - asym2}{\\left(1 + \\exp\\left(iscal \\cdot \\left(\\log(x) - \\log(xmid)\\right)\\right)\\right)^{theta}}$$"
}

help_mod_L5_fd <- function() {
  "$$
  y'(x) = -\\frac{
    (asym1 - asym2) \\cdot
    \\left[
      \\left(1 + \\exp\\left(iscal \\cdot \\left(\\log(x) - \\log(xmid)\\right)\\right)\\right)^{theta - 1} \\cdot
      \\left(theta \\cdot \\exp\\left(iscal \\cdot \\left(\\log(x) - \\log(xmid)\\right)\\right) \\cdot \\frac{iscal}{x}\\right)
    \\right]
  }{
    \\left(1 + \\exp\\left(iscal \\cdot \\left(\\log(x) - \\log(xmid)\\right)\\right)\\right)^{2 \\cdot theta}
  }
  $$"
}

help_mod_L5_sd <- function() {
  "$$
  y''(x) = -\\left(
    \\frac{
      (asym1 - asym2) \\cdot
      \\left[
        \\left(1 + \\exp\\left(iscal \\cdot \\left(\\log(x) - \\log(xmid)\\right)\\right)\\right)^{theta - 2} \\cdot
        \\left(
          (theta - 1) \\cdot \\exp\\left(iscal \\cdot \\left(\\log(x) - \\log(xmid)\\right)\\right) \\cdot \\frac{iscal}{x} \\cdot
          theta \\cdot \\exp\\left(iscal \\cdot \\left(\\log(x) - \\log(xmid)\\right)\\right) \\cdot \\frac{iscal}{x}
        \\right) +
        \\left(1 + \\exp\\left(iscal \\cdot \\left(\\log(x) - \\log(xmid)\\right)\\right)\\right)^{theta - 1} \\cdot
        \\left(theta \\cdot \\exp\\left(iscal \\cdot \\left(\\log(x) - \\log(xmid)\\right)\\right) \\cdot \\frac{iscal^2}{x^2}\\right)
      \\right]
    }{
      \\left(1 + \\exp\\left(iscal \\cdot \\left(\\log(x) - \\log(xmid)\\right)\\right)\\right)^{2 \\cdot theta}
    } -
    \\frac{
      (asym1 - asym2) \\cdot
      \\left[
        \\left(1 + \\exp\\left(iscal \\cdot \\left(\\log(x) - \\log(xmid)\\right)\\right)\\right)^{theta - 1} \\cdot
        \\left(theta \\cdot \\exp\\left(iscal \\cdot \\left(\\log(x) - \\log(xmid)\\right)\\right) \\cdot \\frac{iscal}{x}\\right)
      \\right] \\cdot
      2 \\cdot
      \\left[
        \\left(1 + \\exp\\left(iscal \\cdot \\left(\\log(x) - \\log(xmid)\\right)\\right)\\right)^{theta - 1} \\cdot
        \\left(theta \\cdot \\exp\\left(iscal \\cdot \\left(\\log(x) - \\log(xmid)\\right)\\right) \\cdot \\frac{iscal}{x}\\right)
      \\right]
    }{
      \\left(1 + \\exp\\left(iscal \\cdot \\left(\\log(x) - \\log(xmid)\\right)\\right)\\right)^{4 \\cdot theta}
    }
  \\right)
  $$"
}

help_mod_L5 <- function() {
  div(
    style = "font-family: Arial, sans-serif; line-height: 1.5;",
    h2(style = "color: #2E86C1;", "Description of Parameters and Outputs"),
    tags$ul(
      tags$li(tags$b("x:"), " Input variable, typically representing time or independent variable."),
      tags$li(tags$b("asym1:"), " Asymptotic value for low values of x."),
      tags$li(tags$b("asym2:"), " Asymptotic value for high values of x."),
      tags$li(tags$b("xmid:"), " The x-value at which y = (asym1 + asym2)/2 when theta = 1."),
      tags$li(tags$b("iscal:"), " Steepness of the transition from asym1 to asym2 (inverse of the scale)."),
      tags$li(tags$b("theta:"), " Asymmetry parameter; when theta = 1, this reduces to the 4-parameter logistic model."),
      tags$li(tags$b("inflection:"), " The x-value at the inflection point, where the growth rate is maximal."),
      tags$li(tags$b("auc:"), " The area under the curve (AUC), representing the total response over the range of x.")
    ),
    h2(style = "color: #2E86C1;", "Growth Curve"),
    p("The five-parameter logistic model (5PL) captures sigmoidal growth patterns with added flexibility for asymmetry."),
    p("The equation is given by:"),
    p(help_mod_L5_eq()),
    h2(style = "color: #2E86C1;", "First Derivative"),
    p("The first derivative describes the rate of change of the response variable:"),
    p(help_mod_L5_fd()),
    h2(style = "color: #2E86C1;", "Second Derivative"),
    p("The second derivative describes the curvature of the growth curve, indicating acceleration or deceleration:"),
    p(help_mod_L5_sd()),
    h2(style = "color: #2E86C1;", "Applications and Insights"),
    tags$ul(
      tags$li("The 5PL model provides more flexibility compared to simpler logistic models, allowing for asymmetry in growth."),
      tags$li("Useful for analyzing biological growth patterns, dose-response relationships, and more."),
      tags$li("Captures critical growth dynamics, including inflection points, asymptotes, and steepness.")
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
    future::plan(future::multisession, workers = max(1, parallel::detectCores() %/% 4))
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
mod_segmented <- function(data,
                          flight_date = "date",
                          predictor = "median.NDVI",
                          sowing_date = NULL,
                          threshold,
                          slope = "min",
                          parallel = FALSE) {

  # Prepare data
  dftemp <- data |>
    dplyr::mutate(unique_plot = paste0(block, "_", plot_id)) |>
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) |>
    dplyr::group_by(unique_plot) |>
    tidyr::nest()

  # Set parallel or sequential plan
  if (parallel) {
    future::plan(future::multisession, workers = max(1, parallel::detectCores() %/% 4))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential)) # Ensure plan reverts back

  # Helper function to fit model and calculate DPM
  calculate_dpm <- function(df, flights, threshold, slope) {
    mod <- stats::lm(df[[predictor]] ~ flights)
    attempts <- 0
    max_attempts <- 100
    dpm <- NA
    repeat {
      attempts <- attempts + 1
      seg_model <- try(segmented::segmented(mod, seg.Z = ~flights, npsi = ifelse(nrow(df) > 7, 2, 1),
                                            control = segmented::seg.control(n.boot = 50, random = TRUE, tol = 0.01)),
                       silent = TRUE)

      if (!inherits(seg_model, "try-error") && !is.null(seg_model$psi)) {
        slopes <- segmented::slope(seg_model)$flights
        intercepts <- segmented::intercept(seg_model)$flights

        target_slope <- if (slope == "min") min(slopes[, 1]) else max(slopes[, 1])
        idx <- which(slopes[, 1] == target_slope)
        dpm <- (threshold - intercepts[idx, 1]) / target_slope
        break
      } else if (attempts >= max_attempts || nrow(df) <= 7) {
        dpm <- (threshold - stats::coef(mod)[1]) / stats::coef(mod)[2]
        break
      }
    }
    list(dpm = dpm, model = ifelse(inherits(seg_model, "try-error"), mod, seg_model))
  }

  # Process each nested dataset in parallel
  `%dofut%` <- doFuture::`%dofuture%`
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = rbind) %dofut% {
    df <- as.data.frame(dftemp$data[[i]])
    flights <- as.POSIXlt(df[[flight_date]])$yday + 1
    if (!is.null(sowing_date)) {
      flights <- flights - (as.POSIXlt(sowing_date)$yday + 1)
    }
    model_result <- calculate_dpm(df, flights, threshold, slope)

    tibble::tibble(
      unique_plot = dftemp$unique_plot[i],
      maturity = as.numeric(model_result$dpm),
      threshold = threshold,
      parms = list(modeladj = model_result$model)
    )
  }

  # Post-process results
  results <- results_list |>
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


############## SEGMENTED MODEL 2 ################
mod_segmented2 <- function(data,
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

  # Set parallel or sequential plan
  if (parallel) {
    future::plan(future::multisession, workers = max(1, parallel::detectCores() %/% 4))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential)) # Ensure plan reverts back

  # Helper function to fit segmented regression model and calculate DPM
  calculate_dpm <- function(y, flights) {
    mod <- stats::lm(y ~ flights)  # Initial linear model
    seg_model <- try(segmented::segmented(
      mod,
      seg.Z = ~flights,
      npsi = 1,
      control = segmented::seg.control(n.boot = 50, random = TRUE, tol = 0.01)
    ), silent = TRUE)

    if (!inherits(seg_model, "try-error") && !is.null(seg_model$psi)) {
      # Extract breakpoint(s)
      breakpoints <- seg_model$psi[, "Est."]

      # Extract slopes and intercepts
      slopes <- segmented::slope(seg_model)$flights[,1]
      intercepts <- segmented::intercept(seg_model)$flights[, 1]

      # Return results
      list(dpm = breakpoints, model = seg_model, coefs = list(slopes = slopes, intercepts = intercepts))
    } else {
      # Fallback to linear regression if segmentation fails
      list(dpm = NA,  model = NA, coefs = NA)
    }
  }

  # Process each nested dataset in parallel
  `%dofut%` <- doFuture::`%dofuture%`
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = rbind) %dofut% {
    df <- as.data.frame(dftemp$data[[i]])
    flights <- as.POSIXlt(df[[flight_date]])$yday + 1
    if (!is.null(sowing_date)) {
      flights <- flights - (as.POSIXlt(sowing_date)$yday + 1)
    }
    model_result <- calculate_dpm(df[[predictor]], flights)

    tibble::tibble(
      unique_plot = dftemp$unique_plot[i],
      maturity = as.numeric(model_result$dpm),
      parms = list(modeladj = model_result$model,
                   coefs = model_result$coefs)
    )
  }

  # Post-process results
  results <-
    results_list |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)

  return(results)
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
    future::plan(future::multisession, workers = max(1, parallel::detectCores() %/% 4))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`

  # Fit model for each group
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows, .options.future = list(seed = TRUE)) %dofut% {
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
    future::plan(future::multisession, workers = max(1, parallel::detectCores() %/% 4))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`

  # Fit model for each group
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows, .options.future = list(seed = TRUE)) %dofut% {
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
    future::plan(future::multisession, workers = max(1, parallel::detectCores() %/% 4))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`

  # Fit model for each group
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows, .options.future = list(seed = TRUE)) %dofut% {
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
                            predictor = "q90",
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
    future::plan(future::multisession, workers = max(1, parallel::detectCores() %/% 4))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`

  # Fit model for each group
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows, .options.future = list(seed = TRUE)) %dofut% {
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

      model <- try(
        nls( y ~ SSfpl(flights, a, b, xmid, scal),
             data = data.frame(flights, y),
             control = nls.control(maxiter = 1000)),
        silent = TRUE
      )

      if (inherits(model, "try-error")) {
        model <- suppressWarnings(
          minpack.lm::nlsLM(y ~ SSfpl(flights, a, b, xmid, scal),
                            data = data.frame(flights, y))
        )
      }

      coefslog <- coef(model)
      a <- coefslog[1]
      b <- coefslog[2]
      xmid <- coefslog[3]
      scal <- coefslog[4]

      # Goodness of fit
      gofval <- gof(model, y)

      # Area under curve
      auc <- integrate(modfun_L4, lower = fflight, upper = lflight, a = a, b = b, xmid = xmid, scal = scal)
      # critical points
      fdopt_result <-
        optimize(fdfun_L4,
                 a = a,
                 b = b,
                 xmid = xmid,
                 scal = scal,
                 interval = c(fflight, lflight),
                 maximum = TRUE)

      # maximum acceleration
      sdopt_result <-
        optimize(sdfun_L4,
                 a = a,
                 b = b,
                 xmid = xmid,
                 scal = scal,
                 interval = c(fflight, lflight),
                 maximum = TRUE)

      # maximum deceleration
      sdopt_result2 <-
        optimize(sdfun_L4,
                 a = a,
                 b = b,
                 xmid = xmid,
                 scal = scal,
                 interval = c(fflight, lflight),
                 maximum = FALSE)

      # Return results
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Logistic 4P",
        asymptote = b,
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
                     coefs = list(a = a, b = b, xmid = xmid, scal = scal),
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
help_mod_L4_gm <- function() {
  div(
    style = "font-family: Arial, sans-serif; line-height: 1.5;",

    # Introduction
    p("This information is essential for analyzing growth patterns, comparing treatments, and understanding biological dynamics."),

    # Growth Curve Section
    h2(style = "color: #2E86C1;", "Growth Curve"),
    p("The four-parameter logistic model (4PL) is widely used to describe biological growth processes, such as plant height, weight, or population dynamics. It captures the characteristic sigmoidal (S-shaped) growth pattern often observed in nature."),
    p("The equation is given by:"),
    p(help_mod_L4_eq()),
    p(strong("Where:")),
    withMathJax(
      tags$ul(
        tags$li("\\(x\\): The independent variable, typically time or stage."),
        tags$li("\\(a\\): The lower asymptote, representing the minimum value of the response variable."),
        tags$li("\\(b\\): The upper asymptote, representing the maximum value of the response variable."),
        tags$li("\\(x_{mid}\\): The inflection point, where the rate of change in the response variable is maximal."),
        tags$li("\\(scal\\): A scale parameter that determines the steepness of the curve.")
      )
    ),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Models sigmoidal growth curves that start slow, accelerate, and then plateau."),
      tags$li("Represents initial size, growth rate, and maximum size clearly."),
      tags$li("Commonly applied to plant growth studies, animal development, and population dynamics.")
    ),

    # First Derivative Section
    h2(style = "color: #2E86C1;", "First Derivative"),
    p("The first derivative of the four-parameter logistic model (4PL) describes the rate of change of the response variable (e.g., growth rate) at any given point. It is particularly useful for identifying the inflection point, where the rate of growth is maximal."),
    p("The first derivative is given by:"),
    p(help_mod_L4_fd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Captures the growth rate at any point on the curve."),
      tags$li("The maximum value of the derivative corresponds to the inflection point."),
      tags$li("Helps in understanding the dynamics of growth over time."),
      tags$li("Useful for comparing growth rates across different conditions or treatments.")
    ),

    # Second Derivative Section
    h2(style = "color: #2E86C1;", "Second Derivative"),
    p("The second derivative of the four-parameter logistic model (4PL) provides insight into the curvature of the growth curve, helping to identify points of acceleration or deceleration in growth. This is crucial for understanding how growth dynamics change over time."),
    p("The second derivative is given by:"),
    p(help_mod_L4_sd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Describes how the rate of growth changes over time (acceleration or deceleration)."),
      tags$li("Helps identify regions of maximum acceleration or deceleration in growth."),
      tags$li("Provides insights into the dynamics of curvature for growth modeling."),
      tags$li("Useful for fine-tuning growth predictions and analyzing biological phenomena.")
    ),

    # Conclusion
    p("The second derivative is a powerful tool for understanding not just the growth rate but also the changes in growth dynamics over time. It is particularly useful in identifying critical points in biological systems, such as transitions from rapid growth to slower growth phases.")
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
    future::plan(future::multisession, workers = max(1, parallel::detectCores() %/% 4))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`

  # Fit model for each group
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows, .options.future = list(seed = TRUE)) %dofut% {
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
  if(parallel){
    future::plan(future::multisession, workers = max(1, parallel::detectCores() %/% 4))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`

  # Fit model for each group
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows, .options.future = list(seed = TRUE)) %dofut% {
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
    future::plan(future::multisession, workers = max(1, parallel::detectCores() %/% 4))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`

  # Fit model for each group
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows, .options.future = list(seed = TRUE)) %dofut% {
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
    future::plan(future::multisession, workers = max(1, parallel::detectCores() %/% 4))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`

  # Fit model for each group
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows, .options.future = list(seed = TRUE)) %dofut% {
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
    future::plan(future::multisession, workers = max(1, parallel::detectCores() %/% 4))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`

  # Fit model for each group
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows, .options.future = list(seed = TRUE)) %dofut% {
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

############ ASYMPTOTIC MODEL ############
modfun_asym <- function(x, Asym, R0, lrc) {
  Asym + (R0 - Asym) * exp(-exp(lrc) * x)
}

# First derivative of the asymptotic model
fdfun_asym <- function(x, Asym, R0, lrc) {
  -((R0 - Asym) * (exp(-exp(lrc) * x) * exp(lrc)))
}

# Second derivative of the asymptotic model
sdfun_asym <- function(x, Asym, R0, lrc) {
  (R0 - Asym) * (exp(-exp(lrc) * x) * exp(lrc) * exp(lrc))
}


mod_asymptotic <- function(data,
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
    future::plan(future::multisession, workers = max(1, parallel::detectCores() %/% 4))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`

  # Fit model for each group
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows) %dofut% {
    df <- as.data.frame(dftemp$data[[i]])
    tryCatch({
      if (!is.null(sowing_date)) {
        flights <- as.POSIXlt(df[[flight_date]])$yday + 1 - (as.POSIXlt(sowing_date)$yday)
      } else {
        flights <- as.POSIXlt(df[[flight_date]])$yday + 1
      }

      fflight <- min(flights)
      lflight <- max(flights) + 20
      y <- df |> dplyr::pull(!!rlang::sym(predictor))
      x <- flights

      # Fit Asymptotic model
      model <- nls(y ~ SSasymp(x, Asym, R0, lrc),
                   control = nls.control(maxiter = 1000),
                   data = data.frame(x, y))

      coefs <- coef(model)
      gofval <- gof(model, y)

      # Area under curve
      int1 <- integrate(modfun_asym,
                        lower = fflight,
                        upper = lflight,
                        Asym = coefs[["Asym"]],
                        R0 = coefs[["R0"]],
                        lrc = coefs[["lrc"]])


      # Return results
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Asymptotic",
        asymptote = coefs[["Asym"]],
        R0 = coefs[["R0"]],
        lrc = coefs[["lrc"]],
        auc = int1$value,
        aic = gofval[[1]],
        rmse = gofval[[2]],
        mae = gofval[[3]],
        parms = list(model = modfun_asym,
                     modeladj = model,
                     fd = fdfun_asym,
                     sd = sdfun_asym,
                     coefs = list(
                       Asym = coefs[["Asym"]],
                       R0 = coefs[["R0"]],
                       lrc = coefs[["lrc"]]
                     ),
                     xmin = fflight,
                     xmax = lflight)
      )
    }, error = function(e) {
      # Return NA values if model fails
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Asymptotic",
        asymptote = NA_real_,
        R0 = NA_real_,
        lrc = NA_real_,
        auc = NA_real_,
        aic = NA_real_,
        rmse = NA_real_,
        mae = NA_real_,
        parms = NA
      )
    })
  }

  results <- results_list |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)

  return(results)
}

help_mod_asym_eq <- function() {
  "$$y(x) = \\text{Asym} + (R_0 - \\text{Asym}) \\cdot e^{-e^{\\text{lrc}} \\cdot x}$$"
}

help_mod_asym_fd <- function() {
  "$$y'(x) = -(R_0 - \\text{Asym}) \\cdot e^{-e^{\\text{lrc}} \\cdot x} \\cdot e^{\\text{lrc}}$$"
}

help_mod_asym_sd <- function() {
  "$$y''(x) = (R_0 - \\text{Asym}) \\cdot e^{-e^{\\text{lrc}} \\cdot x} \\cdot e^{2 \\cdot \\text{lrc}}$$"
}

help_mod_asymptotic <- function() {
  div(
    style = "font-family: Arial, sans-serif; line-height: 1.5;",
    h2(style = "color: #2E86C1;", "Asymptotic Growth Model"),
    p("The asymptotic growth model describes processes that grow quickly at first and then level off asymptotically, such as biological growth or diffusion processes."),
    p("The equation is given by:"),
    p(help_mod_asym_eq()),
    p(strong("Where:")),
    withMathJax(
      tags$ul(
        tags$li("\\(x\\): The time or independent variable."),
        tags$li("\\(\\text{Asym}\\): The asymptotic value."),
        tags$li("\\(R_0\\): The initial response value."),
        tags$li("\\(\\text{lrc}\\): The natural log of the rate constant.")
      )
    ),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Models processes that plateau over time."),
      tags$li("Captures the transition from rapid growth to stabilization."),
      tags$li("Applicable to biological systems, chemical reactions, and logistic-type growth.")
    ),
    h2(style = "color: #2E86C1;", "First Derivative"),
    p("The first derivative of the asymptotic model represents the instantaneous rate of change in the response variable."),
    p("The first derivative is given by:"),
    p(help_mod_asym_fd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Describes how quickly the process is growing or stabilizing."),
      tags$li("Useful for analyzing the transition from rapid growth to leveling off."),
      tags$li("Helps identify points of maximal growth rate.")
    ),
    h2(style = "color: #2E86C1;", "Second Derivative"),
    p("The second derivative of the asymptotic model describes the acceleration or deceleration of growth."),
    p("The second derivative is given by:"),
    p(help_mod_asym_sd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Describes the rate at which growth acceleration changes."),
      tags$li("Helps identify inflection points and patterns in stabilization."),
      tags$li("Useful for understanding the dynamics of the leveling-off process.")
    )
  )
}


########### Asymmetric Gaussian bell-shaped model ###########
SSagauss <- selfStart(
  model = function(x, eta, beta, delta, sigma1, sigma2) {
    # Asymmetric Gaussian bell-shaped curve
    .expre0 <- (eta - beta)
    .expre1 <- exp(-((x - delta)^2) / (2 * sigma1^2))
    .expre2 <- exp(-((x - delta)^2) / (2 * sigma2^2))
    .value <- ifelse(
      x < delta,
      beta + .expre0 * .expre1,
      beta + .expre0 * .expre2
    )

    ## Gradient calculations
    .exp1 <- ifelse(x < delta,
                    1 - exp(-(x - delta)^2 / (2 * sigma1^2)),
                    1 - exp(-(x - delta)^2 / (2 * sigma2^2)))
    .exp2 <- ifelse(x < delta,
                    exp(-(x - delta)^2 / (2 * sigma1^2)),
                    exp(-(x - delta)^2 / (2 * sigma2^2)))

    .expr1 <- eta - beta
    .expr2 <- x - delta
    .expr4 <- 2 * sigma1^2
    .expr41 <- 2 * sigma2^2
    .expr6 <- exp(-.expr2^2 / .expr4)
    .expr61 <- exp(-.expr2^2 / .expr41)
    .expr7 <- -(.expr1 * (.expr6 * (2 * .expr2 / .expr4)))
    .expr8 <- -(.expr1 * (.expr61 * (2 * .expr2 / .expr41)))
    .exp3 <- ifelse(x < delta, .expr7, .expr8)

    .expr3 <- (x - delta)^2
    .expr8 <- exp(-.expr3 / .expr4)
    .exp4 <- ifelse(x < delta,
                    .expr1 * (.expr8 * (.expr3 * (2 * (2 * sigma1)) / .expr4^2)),
                    0)
    .exp5 <- ifelse(x < delta, 0,
                    .expr1 * (.expr8 * (.expr3 * (2 * (2 * sigma1)) / .expr4^2)))

    ## Combine gradients
    .grad <- cbind(.exp1, .exp2, .exp3, .exp4, .exp5)
    colnames(.grad) <- c("eta", "beta", "delta", "sigma1", "sigma2")
    attr(.value, "gradient") <- .grad

    .value
  },
  initial = function(mCall, LHS, data, ...) {
    # Initialization for self-start
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    if (nrow(xy) < 5) {
      stop("Too few distinct input values to fit an asymmetric Gaussian curve.")
    }
    eta <- max(xy[,"y"])
    beta <- min(xy[,"y"])
    delta <- NLSstClosestX(xy, eta)
    sigma1 <- abs((min(xy[,"x"]) - delta) / 2)
    sigma2 <- abs((max(xy[,"x"]) - delta) / 2)
    c(eta = eta, beta = beta, delta = delta, sigma1 = sigma1, sigma2 = sigma2)
  },
  parameters = c("eta", "beta", "delta", "sigma1", "sigma2")
)

# Asymmetric Gaussian function
modfun_agaus <- function(x, beta, eta, delta, sigma1, sigma2) {
  ifelse(
    x <= delta,
    beta + (eta - beta) * exp(-((x - delta)^2) / (2 * sigma1^2)),
    beta + (eta - beta) * exp(-((x - delta)^2) / (2 * sigma2^2))
  )
}
# First derivative of the asymmetric Gaussian function
fdfun_agaus <- function(x, beta, eta, delta, sigma1, sigma2) {
  ifelse(
    x <= delta,
    -((eta - beta) * (exp(-((x - delta)^2)/(2 * sigma1^2)) * (2 * (x - delta)/(2 * sigma1^2)))),
    -((eta - beta) * (exp(-((x - delta)^2)/(2 * sigma2^2)) * (2 * (x - delta)/(2 * sigma2^2))))
  )
}
# Second derivative of the asymmetric Gaussian function
sdfun_agaus <- function(x, beta, eta, delta, sigma1, sigma2) {
  ifelse(
    x <= delta,
    -((eta - beta) * (exp(-((x - delta)^2)/(2 * sigma1^2)) * (2/(2 *
                                                                   sigma1^2)) - exp(-((x - delta)^2)/(2 * sigma1^2)) * (2 *
                                                                                                                          (x - delta)/(2 * sigma1^2)) * (2 * (x - delta)/(2 * sigma1^2)))),
    -((eta - beta) * (exp(-((x - delta)^2)/(2 * sigma2^2)) * (2/(2 *
                                                                   sigma2^2)) - exp(-((x - delta)^2)/(2 * sigma2^2)) * (2 *
                                                                                                                          (x - delta)/(2 * sigma2^2)) * (2 * (x - delta)/(2 * sigma2^2))))
  )
}

mod_agauss <- function(data,
                       flight_date = "date",
                       predictor = "q90",
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
    future::plan(future::multisession, workers = max(1, parallel::detectCores() %/% 4))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`

  # Fit model for each group
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows) %dofut% {
    df <- as.data.frame(dftemp$data[[i]])
    tryCatch({
      if (!is.null(sowing_date)) {
        flights <- as.POSIXlt(df[[flight_date]])$yday + 1 - (as.POSIXlt(sowing_date)$yday)
      } else {
        flights <- as.POSIXlt(df[[flight_date]])$yday + 1
      }

      fflight <- min(flights)
      lflight <- max(flights) + 20
      y <- df |> dplyr::pull(!!rlang::sym(predictor))
      x <- flights

      # Fit Asymptotic model
      model <- minpack.lm::nlsLM(y ~ SSagauss(x, eta, beta, delta, sigma1, sigma2),
                                 data = data.frame(x, y))
      coefs <- coef(model)
      gofval <- gof(model, y)

      # Area under curve
      # parameters eta, beta, delta, sigma1, sigma2
      # integrate
      int1 <- integrate(modfun_agaus,
                        lower = fflight,
                        upper = lflight,
                        beta = coefs[["beta"]],
                        eta = coefs[["eta"]],
                        delta = coefs[["delta"]],
                        sigma1 = coefs[["sigma1"]],
                        sigma2 = coefs[["sigma2"]])


      # Return results
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Asymmetric Gaussian",
        beta = coefs[["beta"]],
        eta = coefs[["eta"]],
        delta = coefs[["delta"]],
        sigma1 = coefs[["sigma1"]],
        sigma2 = coefs[["sigma2"]],
        auc = int1$value,
        aic = gofval[[1]],
        rmse = gofval[[2]],
        mae = gofval[[3]],
        parms = list(model = modfun_agaus,
                     modeladj = model,
                     fd = fdfun_agaus,
                     sd = sdfun_agaus,
                     coefs = list(
                       beta = coefs[["beta"]],
                       eta = coefs[["eta"]],
                       delta = coefs[["delta"]],
                       sigma1 = coefs[["sigma1"]],
                       sigma2 = coefs[["sigma2"]]
                     ),
                     xmin = fflight,
                     xmax = lflight)
      )
    }, error = function(e) {
      # Return NA values if model fails
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Asymmetric Gaussian",
        beta = NA_real_,
        eta = NA_real_,
        delta = NA_real_,
        sigma1 = NA_real_,
        sigma2 = NA_real_,
        auc = NA_real_,
        aic = NA_real_,
        rmse = NA_real_,
        mae = NA_real_,
        parms = NA
      )
    })
  }

  results <- results_list |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)

  return(results)
}
# Equation of the asymmetric Gaussian function
help_mod_agaus_eq <- function() {
  "$$f(x) = \\begin{cases}
  \\beta + (\\eta - \\beta) \\cdot \\exp\\left(-\\frac{(x - \\delta)^2}{2 \\sigma_1^2}\\right) & \\text{if } x \\leq \\delta, \\\\
  \\beta + (\\eta - \\beta) \\cdot \\exp\\left(-\\frac{(x - \\delta)^2}{2 \\sigma_2^2}\\right) & \\text{if } x > \\delta.
  \\end{cases}$$"
}

# First derivative of the asymmetric Gaussian function
help_mod_agaus_fd <- function() {
  "$$f'(x) = \\begin{cases}
  -\\frac{(x - \\delta)}{\\sigma_1^2} \\cdot (\\eta - \\beta) \\cdot \\exp\\left(-\\frac{(x - \\delta)^2}{2 \\sigma_1^2}\\right) & \\text{if } x \\leq \\delta, \\\\
  -\\frac{(x - \\delta)}{\\sigma_2^2} \\cdot (\\eta - \\beta) \\cdot \\exp\\left(-\\frac{(x - \\delta)^2}{2 \\sigma_2^2}\\right) & \\text{if } x > \\delta.
  \\end{cases}$$"
}

# Second derivative of the asymmetric Gaussian function
help_mod_agaus_sd <- function() {
  "$$f''(x) = \\begin{cases}
  \\frac{(x - \\delta)^2 - \\sigma_1^2}{\\sigma_1^4} \\cdot (\\eta - \\beta) \\cdot \\exp\\left(-\\frac{(x - \\delta)^2}{2 \\sigma_1^2}\\right) & \\text{if } x \\leq \\delta, \\\\
  \\frac{(x - \\delta)^2 - \\sigma_2^2}{\\sigma_2^4} \\cdot (\\eta - \\beta) \\cdot \\exp\\left(-\\frac{(x - \\delta)^2}{2 \\sigma_2^2}\\right) & \\text{if } x > \\delta.
  \\end{cases}$$"
}

# Help function for the asymmetric Gaussian model
help_mod_asymmetric_gaussian <- function() {
  div(
    style = "font-family: Arial, sans-serif; line-height: 1.5;",
    h2(style = "color: #2E86C1;", "Asymmetric Gaussian Model"),
    p("The asymmetric Gaussian model describes processes with asymmetric growth and decline phases.
      It is commonly used for modeling biological, environmental, or temporal data where growth and senescence are not symmetric."),
    p("The equation is given by:"),
    p(help_mod_agaus_eq()),
    p(strong("Where:")),
    withMathJax(
      tags$ul(
        tags$li("\\(x\\): The independent variable, such as time."),
        tags$li("\\(\\beta\\): The baseline value."),
        tags$li("\\(\\eta\\): The maximum value."),
        tags$li("\\(\\delta\\): The point where the maximum value is reached."),
        tags$li("\\(\\sigma_1\\): Spread parameter for the left side (growth phase)."),
        tags$li("\\(\\sigma_2\\): Spread parameter for the right side (decline phase).")
      )
    ),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Captures asymmetric growth and senescence processes."),
      tags$li("Flexible for modeling nonlinear dynamics."),
      tags$li("Commonly used in plant phenology, water dynamics, and remote sensing.")
    ),
    h2(style = "color: #2E86C1;", "First Derivative"),
    p("The first derivative of the asymmetric Gaussian model represents the rate of change."),
    p("The first derivative is given by:"),
    p(help_mod_agaus_fd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Describes the rate of growth or decline."),
      tags$li("Helps identify critical transition points."),
      tags$li("Useful for analyzing growth acceleration and deceleration.")
    ),
    h2(style = "color: #2E86C1;", "Second Derivative"),
    p("The second derivative of the asymmetric Gaussian model describes the acceleration or deceleration of the rate of change."),
    p("The second derivative is given by:"),
    p(help_mod_agaus_sd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Captures inflection points in growth and senescence."),
      tags$li("Useful for analyzing transitions between phases."),
      tags$li("Describes the dynamics of stabilization or rapid change.")
    )
  )
}

############ BETA GROWTH FUNCTION ############
modfun_beta <- function(x, asym, xm, xe){
  asym * (1 + (xe - x) / (xe - xm)) * (x / xe)^(xe / (xe - xm))
}
# First Derivative of the Beta Growth Model
fdfun_beta <- function(x, asym, xm, xe) {
  asym * (1 + (xe - x)/(xe - xm)) * ((x/xe)^((xe/(xe - xm)) - 1) *
                                       ((xe/(xe - xm)) * (1/xe))) - asym * (1/(xe - xm)) * (x/xe)^(xe/(xe -
                                                                                                         xm))
}
# Second Derivative of the Beta Growth Model
sdfun_beta <- function(t, asym, xm, xe) {
  asym * (1 + (xe - x)/(xe - xm)) * ((x/xe)^(((xe/(xe - xm)) -
                                                1) - 1) * (((xe/(xe - xm)) - 1) * (1/xe)) * ((xe/(xe - xm)) *
                                                                                               (1/xe))) - asym * (1/(xe - xm)) * ((x/xe)^((xe/(xe - xm)) -
                                                                                                                                            1) * ((xe/(xe - xm)) * (1/xe))) - asym * (1/(xe - xm)) *
    ((x/xe)^((xe/(xe - xm)) - 1) * ((xe/(xe - xm)) * (1/xe)))
}

SSbetagf <- selfStart(
  model = function(time, w.max, t.e, t.m){
    # adapted from https://github.com/femiguez/nlraa/blob/master/R/SSbgf.R
    .expr1 <- t.e / (t.e - t.m)
    .expr2 <- (time/t.e)^.expr1
    .expr3 <- (1 + (t.e - time)/(t.e - t.m))
    .value <- w.max * .expr3 * .expr2

    ## Derivative with respect to w.max
    ## deriv(~ w.max * (1 + (t.e - time)/(t.e - t.m)) * (time/t.e)^(t.e / (t.e - t.m)),"w.max")
    .expr2 <- t.e - t.m
    .expr4 <- 1 + (t.e - time)/.expr2
    .expr8 <- (time/t.e)^(t.e/.expr2)
    .expi1 <- .expr4 * .expr8
    .expi1 <- ifelse(is.nan(.expi1),0,.expi1)

    ## Derivative with respect to t.e
    .expr1 <- t.e - time
    .expr5 <- w.max * (1 + .expr1/.expr2)
    .expr6 <- time/t.e
    .lexpr6 <- suppressWarnings(log(.expr6))
    .expr7 <- t.e/.expr2
    .expr8 <- .expr6^.expr7
    .expr10 <- 1/.expr2
    .expr11 <- .expr2^2
    .expi2 <- w.max * (.expr10 - .expr1/.expr11) * .expr8 + .expr5 * (.expr8 * (.lexpr6 * (.expr10 - t.e/.expr11)) - .expr6^(.expr7 - 1) * (.expr7 * (time/t.e^2)))
    .expi2 <- ifelse(is.nan(.expi2),0,.expi2)

    ## Derivative with respect to t.m
    ## deriv(~ w.max * (1 + (t.e - time)/(t.e - t.m)) * (time/t.e)^(t.e / (t.e - t.m)),"t.m")
    .expr10 <- .expr2^2
    .expi3 <- w.max * (.expr1/.expr10) * .expr8 + .expr5 * (.expr8 * (.lexpr6 * (t.e/.expr10)))
    .expi3 <- ifelse(is.nan(.expi3),0,.expi3)

    .actualArgs <- as.list(match.call()[c("w.max", "t.e", "t.m")])

    ##  Gradient
    if (all(unlist(lapply(.actualArgs, is.name)))) {
      .grad <- array(0, c(length(.value), 3L), list(NULL, c("w.max", "t.e", "t.m")))
      .grad[, "w.max"] <- .expi1
      .grad[, "t.e"] <- .expi2
      .grad[, "t.m"] <- .expi3
      dimnames(.grad) <- list(NULL, .actualArgs)
      attr(.value, "gradient") <- .grad
    }
    .value
  },
  initial = function(mCall, LHS, data, ...){

    xy <- sortedXyData(mCall[["time"]], LHS, data)
    if(nrow(xy) < 4){
      stop("Too few distinct input values to fit a bgf")
    }

    w.max <- max(xy[,"y"])
    t.e <- NLSstClosestX(xy, w.max)
    t.m <- t.e / 2
    value <- c(w.max, t.e, t.m)
    names(value) <- mCall[c("w.max","t.e","t.m")]
    value

  },
  parameters = c("w.max", "t.e", "t.m")
)


mod_beta <- function(data,
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
    future::plan(future::multisession, workers = max(1, parallel::detectCores() %/% 4))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`

  # Fit model for each group
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows) %dofut% {
    df <- as.data.frame(dftemp$data[[i]])
    tryCatch({
      if (!is.null(sowing_date)) {
        flights <- as.POSIXlt(df[[flight_date]])$yday + 1 - (as.POSIXlt(sowing_date)$yday)
      } else {
        flights <- as.POSIXlt(df[[flight_date]])$yday + 1
      }

      fflight <- min(flights)
      lflight <- max(flights) + 20
      y <- df |> dplyr::pull(!!rlang::sym(predictor))
      x <- flights

      # Fit Asymptotic model
      model <- minpack.lm::nlsLM(y ~ SSbetagf(x, asym, xe, xm),
                                 control = nls.control(maxiter = 1000),
                                 data = data.frame(x, y))

      coefs <- coef(model)
      gofval <- gof(model, y)

      # Area under curve
      int1 <- integrate(modfun_beta,
                        lower = fflight,
                        upper = lflight,
                        asym = coefs[["asym"]],
                        xe = coefs[["xe"]],
                        xm = coefs[["xm"]])


      # Return results
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Beta Growth",
        asymptote = coefs[["asym"]],
        xe = coefs[["xe"]],
        xm = coefs[["xm"]],
        auc = int1$value,
        aic = gofval[[1]],
        rmse = gofval[[2]],
        mae = gofval[[3]],
        parms = list(model = modfun_beta,
                     modeladj = model,
                     fd = fdfun_beta,
                     sd = sdfun_beta,
                     coefs = list(
                       asym = coefs[["asym"]],
                       xe = coefs[["xe"]],
                       xm = coefs[["xm"]]
                     ),
                     xmin = fflight,
                     xmax = lflight)
      )
    }, error = function(e) {
      # Return NA values if model fails
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Beta Growth",
        asymptote = NA_real_,
        xe = NA_real_,
        xm = NA_real_,
        auc = NA_real_,
        aic = NA_real_,
        rmse = NA_real_,
        mae = NA_real_,
        parms = NA
      )
    })
  }

  results <- results_list |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)

  return(results)
}

# Beta Growth Model Equation
help_mod_beta_eq <- function() {
  "$$f(x) = w_{\\text{max}} \\cdot \\left( 1 + \\frac{t_e - x}{t_e - t_m} \\right) \\cdot \\left( \\frac{x}{t_e} \\right)^{\\frac{t_e}{t_e - t_m}}$$"
}

# First Derivative of the Beta Growth Model
help_mod_beta_fd <- function() {
  "$$f'(x) = w_{\\text{max}} \\cdot \\left( \\frac{x}{t_e} \\right)^{\\frac{t_e}{t_e - t_m} - 1} \\cdot \\left[ \\frac{t_e}{t_e - t_m} \\cdot \\frac{1}{t_e} \\right] - w_{\\text{max}} \\cdot \\frac{1}{t_e - t_m} \\cdot \\left( \\frac{x}{t_e} \\right)^{\\frac{t_e}{t_e - t_m}}$$"
}

# Second Derivative of the Beta Growth Model
help_mod_beta_sd <- function() {
  "$$f''(x) = w_{\\text{max}} \\cdot \\left( \\frac{x}{t_e} \\right)^{\\frac{t_e}{t_e - t_m} - 2} \\cdot \\left( \\frac{t_e}{t_e - t_m} \\cdot \\frac{1}{t_e} \\right)^2 - 2 \\cdot w_{\\text{max}} \\cdot \\frac{1}{t_e - t_m} \\cdot \\left( \\frac{x}{t_e} \\right)^{\\frac{t_e}{t_e - t_m} - 1} \\cdot \\frac{t_e}{t_e - t_m} \\cdot \\frac{1}{t_e}$$"
}

# Help Documentation for Beta Growth Model
help_mod_beta <- function() {
  div(
    style = "font-family: Arial, sans-serif; line-height: 1.5;",
    h2(style = "color: #2E86C1;", "Beta Growth Model"),
    p("The Beta Growth Model is a flexible sigmoid function that describes determinate growth. This model accounts for an initial weight, a maximum weight, and dynamic growth patterns characterized by a rapid growth phase, a slowing phase, and a plateau."),
    p("The model was proposed in the publication:"),
    tags$blockquote(
      "Yin, X., J. Goudriaan, E.A. Lantinga, J. Vos, and H.J. Spiertz. 2003. ",
      em("A Flexible Sigmoid Function of Determinate Growth."),
      " Annals of Botany 91(3): 361–371. doi: 10.1093/aob/mcg029."
    ),
    p("The equation for the Beta Growth Model is:"),
    p(help_mod_beta_eq()),
    p(strong("Where:")),
    withMathJax(
      tags$ul(
        tags$li("\\(x\\): Time or independent variable."),
        tags$li("\\(w_{\\text{max}}\\): Maximum weight or biomass."),
        tags$li("\\(t_m\\): Time at which half of the maximum weight is reached."),
        tags$li("\\(t_e\\): Time at which the weight reaches its maximum.")
      )
    ),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Captures sigmoid-like growth patterns with flexible parameters."),
      tags$li("Includes a rapid growth phase, followed by deceleration and stabilization."),
      tags$li("Suitable for modeling biological growth, crop development, and other processes with determinate growth patterns.")
    ),
    h2(style = "color: #2E86C1;", "First Derivative"),
    p("The first derivative of the Beta Growth Model describes the instantaneous rate of growth."),
    p("The first derivative is given by:"),
    p(help_mod_beta_fd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Represents the rate of growth at any given time."),
      tags$li("Identifies the maximum growth rate during the rapid growth phase."),
      tags$li("Provides insight into the deceleration of growth as the process stabilizes.")
    ),
    h2(style = "color: #2E86C1;", "Second Derivative"),
    p("The second derivative of the Beta Growth Model describes the acceleration or deceleration of growth."),
    p("The second derivative is given by:"),
    p(help_mod_beta_sd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Represents the rate at which growth acceleration changes."),
      tags$li("Helps identify inflection points where growth transitions from acceleration to deceleration."),
      tags$li("Provides insights into the dynamics of growth stabilization.")
    ),
    h2(style = "color: #2E86C1;", "References"),
    p("YIN, X., J. GOUDRIAAN, E.A. LANTINGA, J. VOS, and H.J. SPIERTZ. 2003. A Flexible Sigmoid Function of Determinate Growth. Annals of Botany 91(3): 361–371. doi: ",
      a(href = "https://doi.org/10.1093/aob/mcg029",target = "_blank", "10.1093/aob/mcg029"))
  )
}


############ HILL GROWTH FUNCTION ############
modfun_hill <- function(x, a, Ka, n) {
  a / (1 + (Ka / x)^n)
}
# First Derivative of the Beta Growth Model
fdfun_hill <- function(x, a, Ka, n) {
  a * ((Ka/x)^(n - 1) * (n * (Ka/x^2)))/(1 + (Ka/x)^n)^2
}
# Second Derivative of the Beta Growth Model
sdfun_hill <- function(x, a, Ka, n) {
  -(a * ((Ka/x)^(n - 1) * (n * (Ka * (2 * x)/(x^2)^2)) + (Ka/x)^((n -
                                                                    1) - 1) * ((n - 1) * (Ka/x^2)) * (n * (Ka/x^2)))/(1 + (Ka/x)^n)^2 -
      a * ((Ka/x)^(n - 1) * (n * (Ka/x^2))) * (2 * ((Ka/x)^(n -
                                                              1) * (n * (Ka/x^2)) * (1 + (Ka/x)^n)))/((1 + (Ka/x)^n)^2)^2)
}

SShill <- selfStart(
  model = function(x, Ka, n, a){

    if(any(identical(x, 0))) stop("zero x is not allowed")

    .value <- a / (1 + (Ka/x)^n)

    ## Derivative with respect to Ka
    ## deriv(~a / (1 + (Ka/x)^n),"Ka")
    .expr1 <- Ka/x
    .expr3 <- 1 + .expr1^n
    .expi1 <- -(a * (.expr1^(n - 1) * (n * (1/x)))/.expr3^2)

    ## Derivative with respect to n
    ## deriv(~1 / (1 + (Ka/x)^n),"n")
    .expr2 <- .expr1^n
    .expi2 <- -(a * (.expr2 * log(.expr1))/.expr3^2)

    ## Derivative with respect to a
    ## deriv(~1 / (1 + (Ka/x)^n),"a")
    .expi3 <- 1/.expr3

    .actualArgs <- as.list(match.call()[c("Ka","n","a")])

    ##  Gradient
    if (all(unlist(lapply(.actualArgs, is.name)))) {
      .grad <- array(0, c(length(.value), 3L), list(NULL, c("Ka","n","a")))
      .grad[, "Ka"] <- .expi1
      .grad[, "n"] <- .expi2
      .grad[, "a"] <- .expi3
      dimnames(.grad) <- list(NULL, .actualArgs)
      attr(.value, "gradient") <- .grad
    }
    .value
  },
  initial = function(mCall, LHS, data, ...){

    xy <- sortedXyData(mCall[["x"]], LHS, data)
    if(nrow(xy) < 4){
      stop("Too few distinct input values to fit a hill3")
    }

    xy2 <- xy[,"x" > 0]
    y0 <- xy2[,"y"]/max(xy2[,"y"])
    y1 <- log(y0/(1 - y0))
    y2 <- ifelse(is.finite(y1), y1, NA)
    cfs <- try(coef(lm(y2 ~ log(xy2[,"x"]), na.action = "na.omit")), silent = TRUE)

    if(inherits(cfs, "try-error")){
      a <- max(xy[,"y"])
      n <- 1
      Ka <- mean(xy2[,"x"], na.rm = TRUE)
    }else{
      a <- max(xy[,"y"])
      n <- cfs[2]
      Ka <- exp(-cfs[1]/n)
    }

    value <- c(Ka, n, a)
    names(value) <- mCall[c("Ka","n","a")]
    value
  },
  parameters =  c("Ka","n","a")
)


mod_hill <- function(data,
                     flight_date = "date",
                     predictor = "q90",
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
    future::plan(future::multisession, workers = max(1, parallel::detectCores() %/% 4))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`

  # Fit model for each group
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows, .options.future = list(seed = TRUE)) %dofut% {
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

      model <- try(
        nls( y ~ SShill(flights, Ka, n, a),
             data = data.frame(flights, y),
             control = nls.control(maxiter = 1000)),
        silent = TRUE
      )

      if (inherits(model, "try-error")) {
        model <- suppressWarnings(
          minpack.lm::nlsLM(y ~ SShill(flights, Ka, n, a),
                            data = data.frame(flights, y))
        )
      }

      coefslog <- coef(model)

      # Goodness of fit
      gofval <- gof(model, y)

      # Area under curve
      auc <- integrate(modfun_hill,
                       lower = fflight,
                       upper = lflight,
                       Ka = coefslog[[1]],
                       n = coefslog[[2]],
                       a = coefslog[[3]])
      # critical points
      fdopt_result <-
        optimize(fdfun_hill,
                 Ka = coefslog[[1]],
                 n = coefslog[[2]],
                 a = coefslog[[3]],
                 interval = c(fflight, lflight),
                 maximum = TRUE)

      # maximum acceleration
      sdopt_result <-
        optimize(sdfun_hill,
                 Ka = coefslog[[1]],
                 n = coefslog[[2]],
                 a = coefslog[[3]],
                 interval = c(fflight, lflight),
                 maximum = TRUE)

      # maximum deceleration
      sdopt_result2 <-
        optimize(sdfun_hill,
                 Ka = coefslog[[1]],
                 n = coefslog[[2]],
                 a = coefslog[[3]],
                 interval = c(fflight, lflight),
                 maximum = FALSE)

      # Return results
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Hill",
        asymptote = coefslog[[3]],
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
        parms = list(model = modfun_hill,
                     modeladj = model,
                     fd = fdfun_hill,
                     sd = sdfun_hill,
                     coefs = list(
                       Ka = coefslog[[1]],
                       n = coefslog[[2]],
                       a = coefslog[[3]]
                     ),
                     xmin = fflight, xmax = lflight)
      )
    }, error = function(e) {
      # Return NA values if model fitting fails
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Hill",
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


help_mod_hill_eq <- function() {
  "$$f(x) = \\frac{a}{1 + \\left(\\frac{K_a}{x}\\right)^n}$$"
}

help_mod_hill_fd <- function() {
  "$$f'(x) = -a \\cdot \\frac{n \\cdot \\left(\\frac{K_a}{x}\\right)^n \\cdot \\frac{1}{x}}{\\left(1 + \\left(\\frac{K_a}{x}\\right)^n\\right)^2}$$"
}

help_mod_hill_sd <- function() {
  "$$f''(x) = -a \\cdot \\frac{n \\cdot \\left(\\frac{K_a}{x}\\right)^n \\cdot \\frac{1}{x^2} \\cdot \\left[(n+1) + 2 \\cdot \\left(\\frac{K_a}{x}\\right)^n\\right]}{\\left(1 + \\left(\\frac{K_a}{x}\\right)^n\\right)^3}$$"
}

help_mod_hill <- function() {
  div(
    style = "font-family: Arial, sans-serif; line-height: 1.5;",
    h2(style = "color: #2E86C1;", "Hill Function"),
    p("The Hill Function is widely used to describe cooperative binding processes, dose-response curves, and growth models. It provides a flexible sigmoid curve that captures steep or gradual transitions based on the Hill coefficient \\(n\\)."),
    p("The equation for the Hill Function is:"),
    p(help_mod_hill_eq()),
    p(strong("Where:")),
    withMathJax(
      tags$ul(
        tags$li("\\(x\\): Independent variable (e.g., substrate concentration)."),
        tags$li("\\(a\\): Maximum response or asymptote."),
        tags$li("\\(K_a\\): Half-saturation constant, where \\(f(x) = a/2\\)."),
        tags$li("\\(n\\): Hill coefficient, controlling the steepness of the curve.")
      )
    ),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Models sigmoid-like curves with flexible steepness."),
      tags$li("Used in biochemical and pharmacological studies."),
      tags$li("Provides insight into cooperative or inhibitory processes."),
      tags$li("Normalized to range between \\(0\\) and \\(a\\).")
    ),
    h2(style = "color: #2E86C1;", "First Derivative"),
    p("The first derivative of the Hill Function describes the instantaneous rate of change in the response variable."),
    p("The first derivative is given by:"),
    p(help_mod_hill_fd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Indicates how quickly the response is changing at a given \\(x\\)."),
      tags$li("Helps identify the point of maximal growth or steepest response."),
      tags$li("Useful in sensitivity analysis for cooperative processes.")
    ),
    h2(style = "color: #2E86C1;", "Second Derivative"),
    p("The second derivative of the Hill Function describes the acceleration or deceleration of growth."),
    p("The second derivative is given by:"),
    p(help_mod_hill_sd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Provides insight into the dynamics of sigmoid transitions."),
      tags$li("Helps identify inflection points where growth accelerates or decelerates."),
      tags$li("Useful for understanding the steepness and curvature of response.")
    )
  )
}

############ EXPONENTIAL-PLATEAU FUNCTION ############
# Main Function
modfun_exponential_plateau <- function(x, a, c, xs) {
  ifelse(x < xs,
         a * exp(c * x),                # Before the breakpoint
         a * exp(c * xs))               # At and after the breakpoint
}

# First Derivative of the Exponential-Plateau Function
fdfun_exponential_plateau <- function(x, a, c, xs) {
  ifelse(x < xs,
         a * c * exp(c * x),            # Derivative before the breakpoint
         0)                             # Derivative at and after the breakpoint
}

# Second Derivative of the Exponential-Plateau Function
sdfun_exponential_plateau <- function(x, a, c, xs) {
  ifelse(x < xs,
         a * c^2 * exp(c * x),          # Second derivative before the breakpoint
         0)                             # Second derivative at and after the breakpoint
}


SSexpplat <- selfStart(
  model = function(x, a, c, xs){

    .value <- (x < xs) * a * exp(c * x) + (x >= xs) * (a * exp(c * xs))

    ## Derivative with respect to a, c, xs
    ## deriv(~ a * exp(c * x), c("a"))
    .exp1 <- ifelse(x < xs, exp(c * x), exp(c * xs))

    ## Derivative with respect to c
    ## deriv(~ a * exp(c * x), c("c"))
    .exp2 <- ifelse(x < xs, a * (exp(c * x) * x), a * (exp(c * xs) * xs))

    ## Derivative with respect to xs
    ## deriv(~ a * exp(c * xs), c("xs"))
    .exp3 <- ifelse(x < xs, 0, a * (exp(c * xs) * c))

    .actualArgs <- as.list(match.call()[c("a","c","xs")])

    ##  Gradient
    if (all(unlist(lapply(.actualArgs, is.name)))) {
      .grad <- array(0, c(length(.value), 3L), list(NULL, c("a", "c", "xs")))
      .grad[, "a"] <- .exp1
      .grad[, "c"] <- .exp2
      .grad[, "xs"] <- .exp3
      dimnames(.grad) <- list(NULL, .actualArgs)
      attr(.value, "gradient") <- .grad
    }
    .value
  },
  initial = function(mCall, LHS, data, ...){

    xy <- sortedXyData(mCall[["x"]], LHS, data)
    if(nrow(xy) < 3){
      stop("Too few distinct input values to fit an exponential-plateau.")
    }

    if(any(xy[,"y"] < 0)) stop("negative values in y are not allowed.")
    ## On the log scale
    xy1 <- xy[1:floor(nrow(xy)/2),]
    ## Fit to half the data
    fit <- try(stats::lm(log(xy1[,"y"]) ~ xy1[,"x"]), silent = TRUE)

    if(inherits(fit, "try-error")){
      ## I don't see any reason why 'fit' should fail..., but in that case...
      a <- xy1[1, "y"] ## First observation in the sorted data
      c <- (xy1[nrow(xy1),"y"] - xy1[1,"y"])/(xy1[nrow(xy1),"x"] - xy1[1,"x"]) ## Average slope
    }else{
      a <- exp(coef(fit)[1])
      c <- coef(fit)[2]
    }

    objfun <- function(cfs){
      pred <- expfp(xy[,"x"], a=cfs[1], c=cfs[2], xs=cfs[3])
      ans <- sum((xy[,"y"] - pred)^2)
      ans
    }
    cfs <- c(a,c,mean(xy[,"x"]))
    op <- try(stats::optim(cfs, objfun, method = "L-BFGS-B",
                           upper = c(Inf, Inf, max(xy[,"x"])),
                           lower = c(-Inf, -Inf, min(xy[,"x"]))), silent = TRUE)

    if(!inherits(op, "try-error")){
      a <- op$par[1]
      c <- op$par[2]
      xs <- op$par[3]
    }else{
      ## If it fails I use the mean for the breakpoint
      xs <- mean(xy[,"x"])
    }

    value <- c(a, c, xs)
    names(value) <- mCall[c("a","c","xs")]
    value

  },
  parameters = c("a", "c", "xs")
)


mod_expplat <- function(data,
                        flight_date = "date",
                        predictor = "q90",
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
    future::plan(future::multisession, workers = max(1, parallel::detectCores() %/% 4))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`

  # Fit model for each group
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows, .options.future = list(seed = TRUE)) %dofut% {
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

      model <- try(
        nls( y ~ SSexpplat(flights, a, c, xs),
             data = data.frame(flights, y),
             control = nls.control(maxiter = 1000)),
        silent = TRUE
      )

      if (inherits(model, "try-error")) {
        model <- suppressWarnings(
          minpack.lm::nlsLM(y ~ SSexpplat(flights, a, c, xs),
                            data = data.frame(flights, y))
        )
      }
      asymp <- max(predict(model))
      coefslog <- coef(model)

      # Goodness of fit
      gofval <- gof(model, y)
      # Area under curve
      auc <- integrate(modfun_exponential_plateau,
                       lower = fflight,
                       upper = lflight,
                       a = coefslog[[1]],
                       c = coefslog[[2]],
                       xs = coefslog[[3]])

      # Return results
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Exponential-Plateau",
        asymptote = asymp,
        auc = auc$value,
        a = coefslog[[1]],
        c = coefslog[[2]],
        xs = coefslog[[3]],
        aic = gofval$AIC,
        rmse = gofval$RMSE,
        mae = gofval$MAE,
        parms = list(model = modfun_exponential_plateau,
                     modeladj = model,
                     fd = fdfun_exponential_plateau,
                     sd = sdfun_exponential_plateau,
                     coefs = list(
                       a = coefslog[[1]],
                       c = coefslog[[2]],
                       xs = coefslog[[3]]
                     ),
                     xmin = fflight, xmax = lflight)
      )
    }, error = function(e) {
      # Return NA values if model fitting fails
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Exponential-Plateau",
        asymptote = NA_real_,
        auc = NA_real_,
        a = NA_real_,
        c = NA_real_,
        xs = NA_real_,
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


help_mod_exponential_plateau_eq <- function() {
  "$$f(x) = \\begin{cases}
  a \\cdot e^{c \\cdot x}, & x < x_s \\\\
  a \\cdot e^{c \\cdot x_s}, & x \\geq x_s
  \\end{cases}$$"
}

help_mod_exponential_plateau_fd <- function() {
  "$$f'(x) = \\begin{cases}
  a \\cdot c \\cdot e^{c \\cdot x}, & x < x_s \\\\
  0, & x \\geq x_s
  \\end{cases}$$"
}

help_mod_exponential_plateau_sd <- function() {
  "$$f''(x) = \\begin{cases}
  a \\cdot c^2 \\cdot e^{c \\cdot x}, & x < x_s \\\\
  0, & x \\geq x_s
  \\end{cases}$$"
}

help_mod_exponential_plateau <- function() {
  div(
    style = "font-family: Arial, sans-serif; line-height: 1.5;",
    h2(style = "color: #2E86C1;", "Exponential-Plateau Function"),
    p("The Exponential-Plateau Function models growth processes where an exponential increase occurs up to a specified breakpoint \\(x_s\\), after which the response remains constant."),
    p("The equation for the Exponential-Plateau Function is:"),
    p(help_mod_exponential_plateau_eq()),
    p(strong("Where:")),
    withMathJax(
      tags$ul(
        tags$li("\\(x\\): Independent variable (e.g., time or concentration)."),
        tags$li("\\(a\\): Maximum response or scaling factor."),
        tags$li("\\(c\\): Growth rate constant."),
        tags$li("\\(x_s\\): Breakpoint where the plateau begins.")
      )
    ),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Models exponential growth followed by a plateau."),
      tags$li("Breakpoint \\(x_s\\) defines the transition point."),
      tags$li("Useful for modeling processes that saturate after initial growth."),
      tags$li("Allows clear distinction between growth and steady-state regions.")
    ),
    h2(style = "color: #2E86C1;", "First Derivative"),
    p("The first derivative of the Exponential-Plateau Function describes the rate of change in the response variable."),
    p("The first derivative is given by:"),
    p(help_mod_exponential_plateau_fd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Indicates the rate of growth before the plateau."),
      tags$li("Derivative is zero at and after the breakpoint."),
      tags$li("Highlights the transition from growth to steady state.")
    ),
    h2(style = "color: #2E86C1;", "Second Derivative"),
    p("The second derivative of the Exponential-Plateau Function describes the acceleration or deceleration of growth."),
    p("The second derivative is given by:"),
    p(help_mod_exponential_plateau_sd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Indicates the acceleration of growth before the plateau."),
      tags$li("Second derivative is zero at and after the breakpoint."),
      tags$li("Provides insights into the dynamics of exponential growth.")
    ),
    h2(style = "color: #2E86C1;", "References"),
    p("Archontoulis, S.V., and F.E. Miguez. 2015. Nonlinear Regression Models and Applications in Agricultural Research. Agronomy Journal 107(2): 786–798. doi: ",
      a(href = "https://acsess.onlinelibrary.wiley.com/doi/10.2134/agronj2012.0506",target = "_blank", "10.2134/agronj2012.0506"))
  )
}


############ EXPONENTIAL-LINEAR FUNCTION ############
# Main Function
modfun_exponential_linear <- function(t, cm, rm, tb) {
  (cm / rm) * log(1 + exp(rm * (t - tb)))
}

# First Derivative of the Exponential-Linear Function
fdfun_exponential_linear <- function(t, cm, rm, tb) {
  cm * exp(rm * (t - tb)) / (1 + exp(rm * (t - tb)))
}

# Second Derivative of the Exponential-Linear Function
sdfun_exponential_linear <- function(t, cm, rm, tb) {
  cm * rm * exp(rm * (t - tb)) / (1 + exp(rm * (t - tb)))^2
}


SSexplinear <- selfStart(
  model =  function(t, cm, rm, tb){

    .expre1 <- cm / rm
    .expre2 <- rm * (t - tb)
    .expre3 <- log(1 + exp(.expre2))
    .value <- .expre1 * .expre3

    ## Derivative with respect to cm
    ## deriv(~ (cm / rm) * log(1 + exp(rm * (t - tb))), "cm")
    .expr6 <- suppressWarnings(log(1 + exp(rm * (t - tb))))
    .exprr6 <- suppressWarnings(1/rm * .expr6)
    .exp1 <- ifelse(is.nan(.expr6),0,.exprr6)

    ## Derivative with respect to rm
    ## deriv(~ (cm / rm) * log(1 + exp(rm * (t - tb))), "rm")
    .expr1 <- cm/rm
    .expr2 <- t - tb
    .expr4 <- exp(rm * .expr2)
    .expr5 <- 1 + .expr4
    .expr6 <- suppressWarnings(log(1 + exp(rm * (t - tb))))
    .exprr6 <- suppressWarnings(1/rm * .expr6)
    .exprrr6 <- suppressWarnings(.expr1 * (.expr4 * .expr2/.expr5) - cm/rm^2 * .expr6)
    .exp2 <- ifelse(is.nan(.exprrr6), 0, .exprrr6)

    ## Derivative with respect to tb
    ## deriv(~ (cm / rm) * log(1 + exp(rm * (t - tb))), "tb")
    .exp3 <- -(.expr1 * (.expr4 * rm/.expr5))

    .actualArgs <- as.list(match.call()[c("cm", "rm", "tb")])

    ##  Gradient
    if (all(unlist(lapply(.actualArgs, is.name)))) {
      .grad <- array(0, c(length(.value), 3L), list(NULL, c("cm", "rm", "tb")))
      .grad[, "cm"] <- .exp1
      .grad[, "rm"] <- .exp2
      .grad[, "tb"] <- .exp3
      dimnames(.grad) <- list(NULL, .actualArgs)
      attr(.value, "gradient") <- .grad
    }
    .value
  },
  initial = function(mCall, LHS, data, ...){

    xy <- sortedXyData(mCall[["t"]], LHS, data)
    if(nrow(xy) < 4){
      stop("Too few distinct input values to fit a explin function.")
    }

    ## First phase is exponential and the second phase is linear
    cm <- coef(lm(y ~ x, data = xy))[2]
    y2 <- xy[,"y"]/max(xy[,"y"])
    rm <- coef(lm(y2 ~ xy[,"x"]))[2]
    tb <- floor(max(xy[,"x"])/2)

    value <- c(cm, rm, tb)
    names(value) <- mCall[c("cm","rm","tb")]
    value

  },
  parameters =  c("cm", "rm", "tb")
)


mod_explinear <- function(data,
                          flight_date = "date",
                          predictor = "q90",
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
    future::plan(future::multisession, workers = max(1, parallel::detectCores() %/% 4))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential))

  `%dofut%` <- doFuture::`%dofuture%`

  # Fit model for each group
  results_list <- foreach::foreach(i = seq_along(dftemp$data), .combine = dplyr::bind_rows, .options.future = list(seed = TRUE)) %dofut% {
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

      model <- try(
        nls( y ~ SSexplinear(flights, cm, rm, tb),
             data = data.frame(flights, y),
             control = nls.control(maxiter = 1000)),
        silent = TRUE
      )

      if (inherits(model, "try-error")) {
        model <- suppressWarnings(
          minpack.lm::nlsLM(y ~ SSexplinear(flights, cm, rm, tb),
                            data = data.frame(flights, y))
        )
      }
      coefslog <- coef(model)

      # Goodness of fit
      gofval <- gof(model, y)
      # Area under curve

      # Return results
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Exponential-Linear",
        cm = coefslog[[1]],
        rm = coefslog[[2]],
        tb = coefslog[[3]],
        aic = gofval$AIC,
        rmse = gofval$RMSE,
        mae = gofval$MAE,
        parms = list(model = modfun_exponential_linear,
                     modeladj = model,
                     fd = fdfun_exponential_linear,
                     sd = sdfun_exponential_linear,
                     coefs = list(
                       cm = coefslog[[1]],
                       rm = coefslog[[2]],
                       tb = coefslog[[3]]
                     ),
                     xmin = fflight, xmax = lflight)
      )
    }, error = function(e) {
      # Return NA values if model fitting fails
      tibble::tibble(
        unique_plot = dftemp$unique_plot[i],
        model = "Exponential-Linear",
        cm = NA_real_,
        rm = NA_real_,
        tb = NA_real_,
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


help_mod_exponential_linear_eq <- function() {
  "$$f(t) = \\frac{c_m}{r_m} \\cdot \\log(1 + \\exp(r_m \\cdot (t - t_b)))$$"
}

help_mod_exponential_linear_fd <- function() {
  "$$f'(t) = c_m \\cdot \\frac{\\exp(r_m \\cdot (t - t_b))}{1 + \\exp(r_m \\cdot (t - t_b))}$$"
}

help_mod_exponential_linear_sd <- function() {
  "$$f''(t) = c_m \\cdot r_m \\cdot \\frac{\\exp(r_m \\cdot (t - t_b))}{\\left(1 + \\exp(r_m \\cdot (t - t_b))\\right)^2}$$"
}

help_mod_exponential_linear <- function() {
  div(
    style = "font-family: Arial, sans-serif; line-height: 1.5;",
    h2(style = "color: #2E86C1;", "Exponential-Linear Growth Function"),
    p("The Exponential-Linear Growth Function models growth processes that start exponentially and transition smoothly into a linear phase at a specified time \\(t_b\\)."),
    p("The equation for the Exponential-Linear Growth Function is:"),
    p(help_mod_exponential_linear_eq()),
    p(strong("Where:")),
    withMathJax(
      tags$ul(
        tags$li("\\(t\\): Independent variable (e.g., time or age)."),
        tags$li("\\(c_m\\): Maximum growth rate during the linear phase."),
        tags$li("\\(r_m\\): Maximum growth rate during the exponential phase."),
        tags$li("\\(t_b\\): Time at which the transition occurs.")
      )
    ),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Combines exponential growth and linear growth in a single model."),
      tags$li("Transition from exponential to linear growth happens at \\(t_b\\)."),
      tags$li("Useful for modeling biological or physical processes with distinct growth phases.")
    ),
    h2(style = "color: #2E86C1;", "First Derivative"),
    p("The first derivative of the Exponential-Linear Growth Function describes the instantaneous rate of growth."),
    p("The first derivative is given by:"),
    p(help_mod_exponential_linear_fd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Indicates the growth rate at any time \\(t\\)."),
      tags$li("Shows a peak growth rate during the exponential phase."),
      tags$li("Useful for determining when the system transitions to a linear phase.")
    ),
    h2(style = "color: #2E86C1;", "Second Derivative"),
    p("The second derivative of the Exponential-Linear Growth Function describes the acceleration or deceleration of growth."),
    p("The second derivative is given by:"),
    p(help_mod_exponential_linear_sd()),
    p(strong("Key Features:")),
    tags$ul(
      tags$li("Indicates acceleration during the exponential phase."),
      tags$li("Becomes zero during the linear phase."),
      tags$li("Provides insights into the dynamics of the transition between growth phases.")
    ),
    h2(style = "color: #2E86C1;", "References"),
    p("Goudriaan, J., and J.L. Monteith. 1990. A Mathematical Function for Crop Growth Based on Light Interception and Leaf Area Expansion. Annals of Botany 66(6): 695–701. doi: ",
      a(href = "https://doi.org/10.1093/oxfordjournals.aob.a088084",target = "_blank", "10.1093/oxfordjournals.aob.a088084"))
  )
}



