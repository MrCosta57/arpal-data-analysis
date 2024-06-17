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
