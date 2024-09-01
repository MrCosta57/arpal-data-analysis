check_missing_dates <- function(df, start_date, end_date, frequency) {
  complete_dates <- seq(
    from = as.Date(start_date),
    to = as.Date(end_date),
    by = frequency
  )
  missing_dates <- complete_dates[!complete_dates %in% df$Date]
  return(missing_dates)
}

# Function to find columns with some non-NA values
non_na_columns <- function(df) {
  df %>%
    summarise(across(everything(), ~ any(!is.na(.)))) %>%
    select(where(~.)) %>%
    colnames()
}

find_common_pollutants <- function(df1, df2, df3) {
  cols1 <- non_na_columns(df1)
  cols2 <- non_na_columns(df2)
  cols3 <- non_na_columns(df3)

  # Find common columns
  common_cols <- Reduce(intersect, list(cols1, cols2, cols3))
  return(common_cols)
}

col2rgb_custom <- function(input_col, alpha = 1) {
  rgb_col <- col2rgb(input_col)
  background_RGB <- col2rgb("white")
  new_col <- matrix(
    c(
      (1 - alpha) * background_RGB[1] + alpha * rgb_col[1],
      (1 - alpha) * background_RGB[2] + alpha * rgb_col[2],
      (1 - alpha) * background_RGB[3] + alpha * rgb_col[3]
    ),
    nrow = 3, ncol = 1, dimnames = list(c("red", "green", "blue"))
  )
  return(new_col)
}

rgb2hex_custom <- function(x) {
  return(rgb(x[1], x[2], x[3], maxColorValue = 255))
}


xts2ts <- function(xts_obj, frequency) {
  if (class(xts_obj)[1] == "ts") {
    return(xts_obj)
  }
  start_date <- c(as.integer(format(start(xts_obj), "%Y")), as.integer(format(start(xts_obj), "%m")))
  ts_obj <- ts(as.numeric(xts_obj), frequency = frequency, start = start_date)
  return(ts_obj)
}

ts2xts <- function(ts_obj, date_range) {
  if (class(ts_obj)[1] == "xts") {
    return(ts_obj)
  }
  xts_obj <- xts(as.numeric(ts_obj), order.by = date_range)
  return(xts_obj)
}

window_ts_xts <- function(ts_obj, date_range, start_date, end_date) {
  xts_obj <- ts2xts(ts_obj, date_range)
  xts_obj <- window(xts_obj, start = start_date, end = end_date)
  return(xts_obj)
}

date_to_float <- function(date, freq) {
  date <- as.Date(date)
  # Extract date components
  year <- as.numeric(format(date, "%Y"))
  day <- as.numeric(format(date, "%j"))

  # Calculate the floating-point representation
  date_float <- year + (day - 1) / freq

  return(date_float)
}

get_model_name <- function(model) {
  if (is.null(model$method)) {
    return(as.character(model))
  } else {
    return(as.character(model$method))
  }
}

test_arima_coefficients <- function(arima_model) {
  # thetahat_i/SE(thetahat_i)=t_i
  # can be used for significance test of the parameter theta_i

  # H0: theta_i=0
  # HA: theta_i!=0
  # if |t_i| is small -> H0
  # if |t_i| is large -> HA

  # SIMPLE RULE FOR CHECKING THE SIGNIFICANCE:
  # n large, if H0 is true t_i behaves like N(0,1)
  # So if |t_i|< 2 (2 is similar to 1.96, and 1.96 is the normal distribution quantile 0.975)
  #--> Accept H0 if |t_i|<=2
  #--> Reject H0 if |t_i|>2
  result <- abs(arima_model$coef / sqrt(diag(arima_model$var.coef))) > 2
  return(result)
}

prewhitening <- function(x, model, d = NULL) {
  # Arguments
  #
  # x	a univariate time series
  #
  # model	  a list with component ar and/or ma giving
  #         the AR and MA coefficients respectively.
  #         d in the list is the order of differencing

  if (!is.list(model)) {
    stop("'model' must be list")
  }
  p <- length(model$ar)
  q <- length(model$ma)
  d <- length(model$d)

  if (d > 0) {
    x <- diff(x, differences = d)
  }
  if (p > 0) {
    x <- filter(x, c(1, -model$ar), method = "convolution", sides = 1L)
    x[seq_along(model$ar)] <- 0 # we pad the missing values
  }
  if (q > 0) {
    x <- filter(x, -model$ma, method = "recursive")
  }
  as.ts(x)
}
