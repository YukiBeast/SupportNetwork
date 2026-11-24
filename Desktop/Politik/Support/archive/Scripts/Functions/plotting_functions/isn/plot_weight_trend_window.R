plot_weight_trend_window <- function(isn, path) {
  window_list <- list()
  for (i in 1975:2013) {
    isn_window <- isn[year %in% i:(i+4), ]
    san_i <- load_san_window(isn_window)
    window_list[[i - 1974]] <- cbind(data.table(window = rep(paste0(i, "-", i+4), nrow(san_i))),
                                     san_i
                                     )
  }
  
  san_window <- rbindlist(window_list)
  trend <- san_window[, .(positive_weights = sum(Weight[Weight >= 0]),
                          negative_weights = abs(sum(Weight[Weight < 0]))),
                      by = "window"]
  
  p <- ggplot(trend,
              aes(x = window, group = 1)) +
    geom_line(aes(y = positive_weights), col = "green4", linewidth = 1.2) +
    geom_line(aes(y = negative_weights), col = "coral4", linewidth = 1.2) +
    
    geom_vline(xintercept = "1987-1991", linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = "1997-2001", linetype = "dashed", color = "gray50") +
    
    # Estetica e leggibilità
    theme_minimal(base_size = 18) +
    labs(y =" Weights Sum",
         x = "Window") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1), # Ruota le etichette temporali
    )
  
  save_interactive(p, path)
}
# Example Usage:
# plot_weight_trend_window(isn_nocoalitions, path = "Results/Pics/san_window_weights_trends.pdf")
