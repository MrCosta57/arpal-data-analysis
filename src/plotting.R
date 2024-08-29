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
  print(plot.xts(
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
  y_range <- range(sapply(list(ts1, ts2, ts3), range, na.rm = TRUE))

  plot.xts(
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

plot_periodogram <- function(ts) {
  # Compute the periodogram
  pgram <- TSA::periodogram(ts, plot = FALSE)
  # Frequencies and spectrum
  frequencies <- pgram$freq * length(ts)
  spectrum <- pgram$spec / length(ts)

  # Define labels for the x-axis
  x_labels <- c(1, 2, 4, 6, 12, 26, 52, 104)
  x_labels_text <- c(
    "Annual (1)", "Semiannual (2)", "Quarterly (4)",
    "Bimonthly (6)", "Monthly (12)", "Biweekly (26)",
    "Weekly (52)", "Semiweekly (104)"
  )

  # Create a data frame for plotting
  df <- data.frame(frequencies = frequencies, spectrum = spectrum)

  # Plot using ggplot2
  p <- ggplot(df, aes(x = frequencies, y = spectrum)) +
    geom_step(color = "purple") +
    scale_x_log10(breaks = x_labels, labels = x_labels_text) +
    scale_y_continuous(labels = scientific) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    labs(x = "Frequency", y = "Variance", title = "Periodogram")

  return(p)
}

plot_acf_pacf <- function(ts, main_text, do_acf = TRUE, do_pacf = TRUE, lag_max = NULL) {
  if (do_acf & do_pacf) {
    par(mfrow = c(1, 2))
  }
  if (is.null(lag_max)) {
    lag_max <- as.integer(length(ts) / 4)
  }
  if (do_acf) {
    plot(
      Acf(ts, lag.max = lag_max, plot = FALSE),
      main = main_text
    )
  }
  if (do_pacf) {
    plot(
      Pacf(ts, lag.max = lag_max, plot = FALSE),
      main = main_text
    )
  }
  if (do_acf & do_pacf) {
    par(mfrow = c(1, 1))
  }
}

plot_ccf <- function(ts1, ts2, main_text, lag_max = NULL) {
  if (is.null(lag_max)) {
    lag_max <- as.integer(length(ts1) / 4)
  }
  plot(
    Ccf(ts1, ts2, lag.max = lag_max, plot = FALSE),
    main = main_text
  )
  grid()
}

seasonalaxis <- function(frequency, nlags, type, plot = TRUE) {
  # List of unlabelled tick points
  out2 <- NULL
  # Check for non-seasonal data
  if (length(frequency) == 1) {
    # Compute number of seasonal periods
    np <- trunc(nlags / frequency)
    evenfreq <- (frequency %% 2L) == 0L

    # Defaults for labelled tick points
    if (type == "acf") {
      out <- pretty(1:nlags)
    } else {
      out <- pretty(-nlags:nlags)
    }

    if (frequency == 1) {
      if (type == "acf" && nlags <= 16) {
        out <- 1:nlags
      } else if (type == "ccf" && nlags <= 8) {
        out <- (-nlags:nlags)
      } else {
        if (nlags <= 30 && type == "acf") {
          out2 <- 1:nlags
        } else if (nlags <= 15 && type == "ccf") {
          out2 <- (-nlags:nlags)
        }
        if (!is.null(out2)) {
          out <- pretty(out2)
        }
      }
    } else if (frequency > 1 &&
      ((type == "acf" && np >= 2L) || (type == "ccf" && np >= 1L))) {
      if (type == "acf" && nlags <= 40) {
        out <- frequency * (1:np)
        out2 <- 1:nlags
        # Add half-years
        if (nlags <= 30 && evenfreq && np <= 3) {
          out <- c(out, frequency * ((1:np) - 0.5))
        }
      } else if (type == "ccf" && nlags <= 20) {
        out <- frequency * (-np:np)
        out2 <- (-nlags:nlags)
        # Add half-years
        if (nlags <= 15 && evenfreq && np <= 3) {
          out <- c(out, frequency * ((-np:np) + 0.5))
        }
      } else if (np < (12 - 4 * (type == "ccf"))) {
        out <- frequency * (-np:np)
      }
    }
  } else {
    # Determine which frequency to show
    np <- trunc(nlags / frequency)
    frequency <- frequency[which(np <= 16)]
    if (length(frequency) > 0L) {
      frequency <- min(frequency)
    } else {
      frequency <- 1
    }
    out <- seasonalaxis(frequency, nlags, type, plot = FALSE)
  }
  if (plot) {
    axis(1, at = out)
    if (!is.null(out2)) {
      axis(1, at = out2, tcl = -0.2, labels = FALSE)
    }
  } else {
    return(out)
  }
}

plot_ccf_prewhiten <- function(ccf.out, frequency) {
  nlags <- (dim(ccf.out$lag)[1] - 1) / 2
  ccf.out$lag[, 1, 1] <- -nlags:nlags
  vnames <- c(deparse(substitute(x))[1L], deparse(substitute(y))[1L])
  ccf.out$snames <- paste(vnames, collapse = " & ")
  plot(ccf.out, ylab = "CCF", xaxt = "n")
  seasonalaxis(frequency, nlags, type = "ccf")
  return(invisible(ccf.out))
}

plot_different_acf_pacf <- function(ts, lag_max, station_name) {
  diff_ts <- diff(ts)
  diff_7_ts <- diff(ts, lag = 7)

  log_ts <- ts(ts)
  log_ts[log_ts == 0] <- 1
  log_ts <- log(log_ts)

  plot_acf_pacf(ts, station_name, lag_max = lag_max)
  plot_acf_pacf(log_ts, paste(station_name, "-", "log"), lag_max = lag_max)
  plot_acf_pacf(diff_ts, paste(station_name, "-", "differenced"), lag_max = lag_max)
  plot_acf_pacf(diff(diff_ts), paste(station_name, "-", "2 times differenced"), lag_max = lag_max)
  plot_acf_pacf(diff_7_ts, paste(station_name, "-", "differenced 7"), lag_max = lag_max)
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
