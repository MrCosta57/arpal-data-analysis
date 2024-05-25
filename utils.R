check_missing_dates <- function(df, start_date, end_date, frequency) {
  complete_dates <- seq(from = as.Date(start_date),
                        to = as.Date(end_date),
                        by = frequency)
  missing_dates <- complete_dates[!complete_dates %in% df$Date]
  return(missing_dates)
}


plot_AQ_W_stations <- function (data_aq,
                                data_w,
                               title = "Map of ARPA stations in Lombardy",
                               prov_line_type = 1,
                               prov_line_size = 1,
                               col_points=c("steelblue", "darkorange"),
                               xlab = "Longitude",
                               ylab = "Latitude")
{
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
  Stats_w <- get_ARPA_Lombardia_W_registry()
  
  data_aq <- data.frame(IDStation = unique(data_aq$IDStation))
  d_aq <- dplyr::left_join(data_aq, Stats_aq, by = "IDStation")
  d_aq <- d_aq %>% sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  
  data_w <- data.frame(IDStation = unique(data_w$IDStation))
  d_w <- dplyr::left_join(data_w, Stats_w, by = "IDStation")
  d_w <- d_w %>% sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  
  
  geo_plot <- Lombardia %>% ggplot2::ggplot() + 
    ggplot2::geom_sf(linetype = prov_line_type, size = prov_line_size) + 
    ggplot2::geom_sf(data = d_aq, col = col_points[1]) + 
    ggplot2::geom_sf(data = d_w, col = col_points[2]) +
    ggplot2::labs(title = title) + 
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(
      labels = function(x)
        paste0(x, "°", "E")
    ) + ggplot2::scale_y_continuous(
      labels = function(x)
        paste0(x, "°", "N")
    )+ ggplot2::scale_color_manual(
      values = col_points,
      labels = c("AQ stations", "W stations")) 

  print(geo_plot)
}
