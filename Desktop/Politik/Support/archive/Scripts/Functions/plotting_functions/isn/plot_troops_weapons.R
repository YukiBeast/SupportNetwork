plot_troops_weapons <- function(isn, share, path) {
    library(data.table)
    library(ggplot2)
    dt <- copy(isn)
    
    dt_plot <- dt[, .(provided = sum(troups_or_weapons == 0, na.rm = TRUE),
                      not_provided = sum(troups_or_weapons == 1, na.rm = TRUE),
                      total_count = sum(!is.na(ext_nonstate))),
                  by = "year"]
    y_cap <- NULL
    
    if(share) {
      dt_plot <- dt_plot[, .(year,
                             provided = provided / total_count,
                             not_provided = not_provided / total_count)]
      y_cap <- scale_y_continuous(limits = c(0.2, 0.8))
    }
    p <- dt_plot |>

    ggplot(aes(x = year)) +
    
    # Layers for "Provided"
    geom_point(aes(y = provided, col = "provided")) +
    geom_line(aes(y = provided, col = "provided")) +
    geom_smooth(aes(y = provided, col = "provided"), se = FALSE) +
    
    # Layers for "Not Provided"
    geom_point(aes(y = not_provided, col = "not_provided")) +
    geom_line(aes(y = not_provided, col = "not_provided")) +
    geom_smooth(aes(y = not_provided, col = "not_provided"), se = FALSE) +
    
    # Custom colors and legend
    scale_color_manual(
      values = c("provided" = "purple3",      # Change color as you like
                 "not_provided" = "darkgray"), # Change color as you like
      labels = c("provided" = "Troops/Weapons Provided",
                 "not_provided" = "No Troops/Weapons"),
      name = "Support Type"
    ) + 
    line1 + line2 + y_cap +
    labs(x = "Year", y = ifelse(share, "Share (%)", "Total Count")) +
    theme_minimal(base_size = 24)
  
  save_interactive(p, path)
}

# plot_troops_weapons(isn_nocoalitions, FALSE, "Results/lethal_support.pdf")  
# plot_troops_weapons(isn_nocoalitions, TRUE, "Results/lethal_support_share.pdf")  
  