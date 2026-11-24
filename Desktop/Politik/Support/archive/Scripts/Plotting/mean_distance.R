plot_mean_distance <- function(data, path) {
  
  data[!is.na(ext_name), haversine := get_haversine(dyad_country, ext_name)]
  
  data_mean_distance <- data[, .(mean_distance = mean(haversine, na.rm = TRUE)),
                             by = "year"]
  
  p <- ggplot(data_mean_distance) +
    geom_point(aes(x = year, y = mean_distance),
               size = 4, col = "darkblue") +
     geom_line(aes(x = year, y = mean_distance),
               linewidth = 1, col = "darkblue") +
    geom_smooth(aes(x = year, y = mean_distance), se = F,
                linewidth = 2, col = "darkblue") +
    line1 + line2 +
    labs(y = "Mean Interventions Distance (KM)",
         x = "Year") +
    theme_minimal(base_size = 28)
  
  save_interactive(p, path)
}
