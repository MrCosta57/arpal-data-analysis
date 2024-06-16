plot_ts_grid <- function(ts_list, n_row = 1, names, colors, ylab) {
    # Set output parameters
    par(
        mfrow = c(n_row, ceiling(length(ts_list) / n_row))
    )

    # Plot each single time series
    for (name in names) {
        plot(
            ts_list[[name]],
            main = name,
            col = colors[[name]],
            ylab = ylab
        )
        grid()
    }

    # Reset the plotting parameters
    par(mfrow = c(1, 1))
}

plot_multiple_ts <- function(ts_list, names, colors, main, ylab, lwd = 1, lty = NULL) {
    # Plot the first time series
    ?plot
    if (is.null(lty)) {
        lty_ <- "solid"
    } else {
        lty_ <- lty[1]
    }
    plot(
        ts_list[[names[1]]],
        xlim = range(do.call(c, lapply(ts_list, time)), na.rm = TRUE),
        ylim = range(unlist(ts_list), na.rm = TRUE),
        type = "l", col = colors[[names[1]]],
        main = main, xlab = "Year", ylab = ylab, lwd = lwd, lty = lty_
    )
    grid()

    # Add the remaining time series to the plot
    for (i in 2:length(names)) {
        if (is.null(lty)) {
            lty_ <- "solid"
        } else {
            (lty_ <- lty[i])
        }
        i_name <- names[i]
        lines(ts_list[[i_name]], type = "l", lwd = lwd, lty = lty_, col = colors[[i_name]])
    }

    # Add legend
    legend(
        "topleft",
        legend = names, cex = 0.6,
        lty = lty, lwd = 2, col = unlist(colors)
    )
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
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  
  # Add a column for colors to d_aq
  d_aq <- d_aq %>%
    dplyr::mutate(color = col_points[1:nrow(d_aq)])
  
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
