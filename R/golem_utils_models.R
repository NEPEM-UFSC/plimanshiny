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
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) %>%
    tidyr::nest(parms = parms)

  return(results)
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
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) %>%
    tidyr::nest(parms = parms)

  return(results)
}


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
    dplyr::mutate(unique_plot = paste0(block, "_", plot_id)) %>%
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) %>%
    dplyr::group_by(unique_plot) %>%
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
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) %>%
    tidyr::nest(parms = parms)

  return(results)
}


# Logistic model L.5()
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



# Threshold-based methods
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
help_mod_L4_gm <- function(){
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
    p("These variables provide comprehensive information about the dynamics of the logistic growth model. For example:"),
    tags$ul(
      tags$li(tags$b("Asymptotes:"), tags$i(" b1"), " and ", tags$i("b2"), " define the lower and upper bounds of the response variable."),
      tags$li(tags$b("Inflection Point:"), " Variables like ", tags$i("inflection"), ", ", tags$i("xinfp"), ", and ", tags$i("yinfp"), " mark the stage of maximum growth rate."),
      tags$li(tags$b("Curvature Metrics:"), " ", tags$i("xmace"), ", ", tags$i("ymace"), ", ", tags$i("xmdes"), ", and ", tags$i("ymdes"), " describe the bending behavior of the curve and changes in concavity.")
    ),
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

