print_table_custom <- function(
    df, title = "", is_summary = FALSE, full_width = FALSE,
    style = c("striped", "hover", "condensed", "responsive"), highlight_rows = NULL) {
  if (is_summary) {
    df <- t(do.call(cbind, lapply(df, summary)))
  }

  # Check if row highlighting is enabled
  if (!is.null(highlight_rows)) {
    # Convert row names to indices if they are provided as names
    if (is.character(highlight_rows)) {
      highlight_rows <- which(rownames(df) %in% highlight_rows, arr.ind = TRUE)
    }
    return(kable(df, caption = title) %>%
      kable_styling(full_width = full_width, bootstrap_options = style) %>%
      row_spec(highlight_rows, extra_css = "background-color: #68BBE3;"))
  } else {
    return(kable(df, caption = title) %>%
      kable_styling(full_width = full_width, bootstrap_options = style))
  }
}


check_missing_dates <- function(df, start_date, end_date, frequency) {
  complete_dates <- seq(
    from = as.Date(start_date),
    to = as.Date(end_date),
    by = frequency
  )
  missing_dates <- complete_dates[!complete_dates %in% df$Date]
  return(missing_dates)
}
