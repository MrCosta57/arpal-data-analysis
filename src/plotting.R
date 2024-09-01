print_table_custom <- function(
    df, title = "", is_summary = FALSE, full_width = FALSE,
    style = c("striped", "hover", "condensed", "responsive"),
    highlight_rows = NULL) {
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
      row_spec(highlight_rows, extra_css = "background-color: rgba(104, 187, 227, 0.25);"))
  } else {
    return(kable(df, caption = title) %>%
      kable_styling(full_width = full_width, bootstrap_options = style))
  }
}

plot_single_ts <- function(ts, ts_name, ts_color, ylab) {
  if (class(ts)[1] != "xts") {
    stop("The time series must be of class 'xts'.")
  }
  print(plot(
    ts,
    type = "l",
    col = ts_color,
    lwd = 1,
    main = ts_name,
    ylab = ylab,
    xlab = "Time",
    grid.col = "lightgray",
    grid.ticks.lty = 2,
    yaxis.right = FALSE
  ))
}

plot_ts_grid <- function(ts_list, ts_names, ts_colors, ylab, n_row = 1) {
  # Check if the length of colors matches the number of time series
  if (length(ts_colors) != length(ts_list)) {
    stop("Length of 'colors' must be equal to the length of 'ts_list'.")
  }
  # Check if the length of ts_names matches the number of time series
  if (length(ts_names) != length(ts_list)) {
    stop("Length of 'ts_names' must be equal to the length of 'ts_list'.")
  }
  # Determine the number of columns based on n_row
  n_col <- ceiling(length(ts_list) / n_row)
  # Set up the plotting area
  par(mfrow = c(n_row, n_col), mar = c(4, 4, 2, 1))
  # Loop over each time series and plot it
  for (i in seq_along(ts_list)) {
    plot_single_ts(
      ts = ts_list[[i]],
      ts_name = ts_names[i],
      ts_color = ts_colors[i],
      ylab = ylab
    )
  }
  # Reset plotting parameters to default
  par(mfrow = c(1, 1))
}

plot_3_ts <- function(ts1, ts2, ts3, ts_colors, main, ylab, legend_names) {
  # Find the range for the y-axis
  if (class(ts1)[1] != "xts" | class(ts2)[1] != "xts" | class(ts3)[1] != "xts") {
    stop("All time series must be of class 'xts'.")
  }
  y_range <- range(sapply(list(ts1, ts2, ts3), range, na.rm = TRUE))

  plot(
    ts1,
    lty = 1,
    col = ts_colors[1], lwd = 1, main = main,
    ylab = ylab, ylim = y_range, xlab = "Time",
    grid.col = "lightgray",
    grid.ticks.lty = 2, yaxis.right = FALSE
  )
  lines(ts2, col = ts_colors[2], lwd = 1, lty = 1)
  lines(ts3, col = ts_colors[3], lwd = 1, lty = 1)
  # # Add a legend
  addLegend("topleft",
    legend.names = legend_names,
    lty = c(1, 1, 1), lwd = c(1, 1, 1),
    col = ts_colors
  )
}

plot_filtered_ts <- function(original_ts, filtered_ts_list, line_colors, legend_names, main, ylab) {
  if (class(original_ts) != "ts" | class(filtered_ts_list[[1]]) != "ts") {
    stop("All time series must be of class 'ts'.")
  }
  # Plot the original time series
  plot(
    original_ts,
    main = main,
    ylab = ylab,
    col = "dimgray"
  )

  for (i in 1:length(filtered_ts_list)) {
    lines(
      filtered_ts_list[[i]],
      lwd = 2, lty = "dashed",
      col = line_colors[i]
    )
  }

  legend("topleft",
    legend = legend_names,
    lty = "dashed", lwd = 2,
    col = line_colors
  )
}

plot_train_test_ts <- function(train_ts, test_ts, line_colors, legend_names, main, ylab, train_date_range, test_date_range) {
  train_ts <- ts2xts(train_ts, train_date_range)
  test_ts <- ts2xts(test_ts, test_date_range)
  plot(index(train_ts), coredata(train_ts),
    main = main, ylab = ylab, col = line_colors[1], type = "l", xlab = "Time"
  )
  lines(index(test_ts), coredata(test_ts), col = line_colors[2], type = "l")

  legend("topleft",
    legend = c("Train", "Test"),
    lty = 1, lwd = 1, col = line_colors
  )
}


plot_acf_pacf <- function(ts, main_text, lag_max = NULL) {
  if (class(ts) != "ts") {
    stop("The time series must be of class 'ts'.")
  }
  par(mfrow = c(1, 2))
  # Set outer margins to make room for the title
  par(oma = c(0, 0, 2, 0))
  # Set inner margins for the plots
  par(mar = c(5, 4, 1, 2))

  if (is.null(lag_max)) {
    lag_max <- as.integer(length(ts) / 4)
  }
  Acf(ts, lag.max = lag_max, main = "")
  Pacf(ts, lag.max = lag_max, main = "")
  mtext(main_text, outer = TRUE, cex = 1.5)
  par(oma = c(0, 0, 0, 0))
  par(mar = c(0, 0, 0, 0))
  par(mfrow = c(1, 1))
}

plot_ccf <- function(ts1, ts2, main_text, lag_max = NULL) {
  if (class(ts1) != "ts" | class(ts2) != "ts") {
    stop("Both time series must be of class 'ts'.")
  }
  if (is.null(lag_max)) {
    lag_max <- as.integer(length(ts1) / 4)
  }
  plot(
    Ccf(ts1, ts2, lag.max = lag_max, plot = FALSE),
    ylab = "CCF",
    main = main_text
  )
  grid()
}

log_my <- function(ts) {
  log_ts <- ts(ts)
  log_ts[log_ts == 0] <- 1
  log_ts <- log(log_ts)
  return(log_ts)
}

plot_pollutant_XY_lin <- function(x, y, xlab, ylab, station_name, unit_measure_x, unit_measure_y) {
  plot(y ~ x,
    xlab = paste0(xlab, " (", unit_measure_x, ")"),
    ylab = paste0(ylab, " (", unit_measure_y, ")"),
    main = paste(station_name, "-", ylab, "vs", xlab),
    col = "steelblue"
  )
  fit <- lm(y ~ x)
  abline(fit, col = "darkorange", lwd = 2)
  print(summary(fit))
  checkresiduals(resid(fit))
}

plot_nn_residuals <- function(nn_model, model_name, ts, lag_max = NULL) {
  if (is.null(lag_max)) {
    lag_max <- as.integer(length(ts) / 4)
  }
  par(mfrow = c(1, 2))
  r <- resid(nn_model)
  r_len_div5 <- as.integer(length(r) / 5)
  Acf(r, main = paste(model_name, "residuals"), lag.max = lag_max)
  Acf(r^2, main = paste(model_name, "residuals^2"), lag.max = lag_max)
  par(mfrow = c(1, 1))
  print(Box.test(r, lag = min(10, r_len_div5), type = "Ljung-Box"))
  print(Box.test(r^2, lag = min(10, r_len_div5), type = "Ljung-Box"))

  layout_matrix <- matrix(c(1, 2, 3, 3), nrow = 2, ncol = 2, byrow = TRUE)
  layout(layout_matrix)
  Ccf(ts, r, main = paste(model_name, "residuals - time series"), lag.max = lag_max)
  Ccf(ts^2, r, main = paste(model_name, "residuals - time series^2"), lag.max = lag_max)
  Ccf(r * ts^2, r, main = paste(model_name, "residuals - residuals*time series^2"), lag.max = lag_max)
  par(mfrow = c(1, 1))
}


plot_AQ_stations <- function(data_aq,
                             title = "Map of ARPA stations in Lombardy",
                             prov_line_type = 1,
                             prov_line_size = 1,
                             col_points = c("red", "blue", "green"),
                             xlab = "Longitude",
                             ylab = "Latitude") {
  if (!curl::has_internet()) {
    message(
      "Internet connection not available at the moment.\nPlease check your internet connection. If the problem persists, please contact the package maintainer."
    )
    return(invisible(NULL))
  }

  Lombardia <- get_Lombardia_geospatial(NUTS_level = "NUTS3")
  if (is.null(Lombardia)) {
    message(
      "The map will not include the ground layer with Lombardy's shapefile. Only points/coordinates will be plot."
    )
  }

  Stats_aq <- get_ARPA_Lombardia_AQ_registry()

  data_aq <- data.frame(IDStation = unique(data_aq$IDStation))
  d_aq <- dplyr::left_join(data_aq, Stats_aq, by = "IDStation")
  d_aq <- d_aq %>%
    dplyr::select(IDStation, Longitude, Latitude) %>%
    dplyr::distinct() %>%
    dplyr::arrange(IDStation) %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

  # Add a column for colors to d_aq
  d_aq <- d_aq %>%
    dplyr::mutate(color = col_points)

  geo_plot <- Lombardia %>% ggplot2::ggplot() +
    ggplot2::geom_sf(linetype = prov_line_type, size = prov_line_size) +
    ggplot2::geom_sf(data = d_aq, aes(color = color), size = 3) +
    ggplot2::geom_sf_text(data = d_aq, aes(label = IDStation), nudge_y = 0.01) +
    ggplot2::scale_color_identity() +
    ggplot2::labs(title = title) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(
      labels = function(x) {
        paste0(x, "°", "E")
      }
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) {
        paste0(x, "°", "N")
      }
    )

  print(geo_plot)
}


plot_zoning_map <- function(title = "ARPA Lombardia zoning",
                            line_type = 1, line_size = 1, xlab = "Longitude", ylab = "Latitude") {
  if (!curl::has_internet()) {
    message("Internet connection not available at the moment.\nPlease check your internet connection. If the problem persists, please contact the package maintainer.")
    return(invisible(NULL))
  }
  temp <- tempfile()
  res <- suppressWarnings(try(curl::curl_fetch_disk(
    "https://github.com/PaoloMaranzano/ARPALData/raw/main/ARPA_zoning_shape.zip",
    temp
  ), silent = TRUE))
  if (res$status_code != 200) {
    message(paste0(
      "The internet resource for ARPA Lombardia zoninig (from GitHub) is not available at the moment. Status code: ",
      res$status_code, ".\nPlease, try later. If the problem persists, please contact the package maintainer."
    ))
    return(invisible(NULL))
  }
  temp1 <- tempfile()
  temp2 <- tempfile()
  download.file(
    url = "https://github.com/PaoloMaranzano/ARPALData/raw/main/ARPA_zoning_shape.zip",
    destfile = temp1
  )
  unzip(zipfile = temp1, exdir = temp2)
  your_SHP_file <- list.files(temp2, pattern = ".shp$", full.names = TRUE)
  Zoning <- sf::read_sf(your_SHP_file) %>%
    sf::st_as_sf(crs = 4326) %>%
    dplyr::mutate(Zone = case_when(
      COD_ZONA == "A" ~
        "Urbanized Plain", COD_ZONA == "Agg_BG" ~
        "Metropolitan area of Bergamo", COD_ZONA ==
        "Agg_BS" ~ "Metropolitan area of Brescia", COD_ZONA ==
        "Agg_MI" ~ "Metropolitan area of Milano", COD_ZONA ==
        "B" ~ "Rural Plain", COD_ZONA == "C" ~ "Mountain",
      COD_ZONA == "D" ~ "Valley floor"
    )) %>%
    dplyr::select(
      Cod_Zone = COD_ZONA,
      Zone, geometry
    )
  geo_plot <- Zoning %>% ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = Zone),
      linetype = line_type, size = line_size
    ) +
    ggplot2::labs(
      title = title,
      x = xlab, y = ylab
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
    ggplot2::scale_x_continuous(labels = function(x) {
      paste0(
        x,
        "°", "E"
      )
    }) +
    ggplot2::scale_y_continuous(labels = function(x) {
      paste0(
        x,
        "°", "N"
      )
    })
  print(geo_plot)

  file.remove(your_SHP_file)
}
