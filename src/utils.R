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

apply_filtering <- function(input_ts, weights) {
  # Apply the filter to the time series
  filtered_ts <- filter(input_ts, weights, sides = 2)
  return(filtered_ts)
}

simple_ma <- function(input_ts, p) {
  # Calculate the moving average weights
  ma_weights <- rep(1 / (2 * p + 1), 2 * p + 1)
  # Apply the moving average filter
  ma_filter <- apply_filtering(input_ts, ma_weights)
  return(ma_filter)
}

spencer_filtering <- function(input_ts) {
  w <- c(74, 67, 46, 21, 3, -5, -6, -3)
  w <- c(rev(w[-1]), w)
  # After these steps w is: (-3 -6 -5  3 21 46 67 74 67 46 21  3 -5 -6 -3)
  weights_spencer <- w / sum(w) # sum(w) is 320
  spencer_ts <- apply_filtering(input_ts, weights_spencer)
  return(spencer_ts)
}
