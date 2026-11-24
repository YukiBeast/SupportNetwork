count_total_dyads_supporters <- function(data, path, upper_limit = NULL,
                                         caption = "No caption provided",
                                         log_file = "plot_tracker.Rmd") {
  
  # 1. Create and store the plot in a variable 'p'
  p <- data[, .(total_dyads = length(unique(dyad_id)),
                total_ext_supporters = uniqueN(ext_name, na.rm = TRUE)),
            by = "year"] |>
    ggplot() +
    geom_point(aes(x = year, y = total_dyads,
                   col = "Dyads")) +
    geom_line(aes(x = year, y = total_dyads,
                  col = "Dyads"),
              linewidth = 0.8) +
    geom_smooth(aes(x = year, y = total_dyads,
                    col = "Dyads"),
                linewidth = 0.6,
                se = FALSE) +
    geom_point(aes(x = year, y = total_ext_supporters,
                   col = "External Supporters")) +
    geom_line(aes(x = year, y = total_ext_supporters,
                  col = "External Supporters"),
              linewidth = 0.8) +
    geom_smooth(aes(x = year, y = total_ext_supporters,
                    col = "External Supporters"),
                linewidth = 0.6,
                se = FALSE) +
    scale_color_manual(values = c("Dyads" = "salmon",
                                  "External Supporters" = "cyan3"),
                       name = NULL) + line1 + line2 +
    { if (is.null(upper_limit)) expand_limits(y = 0)
      else coord_cartesian(ylim = c(0, upper_limit)) } +
    labs(x = "Year",
         y = "Count") +
    theme_minimal(base_size = 28) +
    theme(legend.position = "none")
  
  save_interactive(p, path, caption, log_file)
}
