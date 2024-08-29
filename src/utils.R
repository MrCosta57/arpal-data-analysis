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

# library(xts)
# a<-xts(rnorm(1000), order.by = seq(as.Date("2021-01-01"), by = "day", length.out = 1000), frequency = 7)
# plot(a)

# plot(stl(ts(as.numeric(ts_m), frequency = 7), s.window = "periodic"))
