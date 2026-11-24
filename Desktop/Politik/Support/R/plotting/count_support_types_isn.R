count_support_types_isn <- function(data, path, upper_limit = NULL,
                                    caption = "No caption provided",
                                    log_file = "plot_tracker.Rmd") {
  
  library(data.table, quietly = T)
  library(ggplot2, quietly = T)
  
  p <- data[!is.na(ext_name), .(total_ties = sum(actor_nonstate == 0, na.rm = TRUE) + sum(actor_nonstate == 1, na.rm = TRUE),
                gov_ties = sum(actor_nonstate == 0, na.rm = TRUE),
                non_gov_ties = sum(actor_nonstate == 1, na.rm = TRUE)),
            by = "year"] |>
    ggplot() +
    ## all ties ##
    geom_point(aes(x = year,
                   y = total_ties,
                   col = "Total Support Ties")) +
    geom_line(aes(x = year,
                  y = total_ties,
                  col = "Total Support Ties"),
              linewidth = 0.8) +
    geom_smooth(aes(x = year,
                    y = total_ties,
                    col = "Total Support Ties"),
                linewidth = 0.8,
                se = FALSE) +
    ## only gov ties ##
    geom_point(aes(x = year,
                   y = gov_ties,
                   col = "Government Support Ties")) +
    geom_line(aes(x = year,
                  y = gov_ties,
                  col = "Government Support Ties"),
              linewidth = 0.8) +
    geom_smooth(aes(x = year,
                    y = gov_ties,
                    col = "Government Support Ties"),
                linewidth = 0.8,
                se = FALSE) +
    ## only rebel ties ##
    geom_point(aes(x = year,
                   y = non_gov_ties,
                   col = "Rebels Support Ties")) +
    geom_line(aes(x = year,
                  y = non_gov_ties,
                  col = "Rebels Support Ties"),
              linewidth = 0.8) +
    geom_smooth(aes(x = year,
                    y = non_gov_ties,
                    col = "Rebels Support Ties"),
                linewidth = 0.6,
                se = FALSE) +
    scale_color_manual(values = c("Total Support Ties" = "black",
                                  "Government Support Ties" = "#44AA99",
                                  "Rebels Support Ties" = "#999933"),
                       name = NULL) + line1 + line2 +
    { if (is.null(upper_limit)) expand_limits(y = 0)
      else coord_cartesian(ylim = c(0, upper_limit)) } +
    labs(x = "Year",
         y = "Count") +
    theme_minimal(base_size = 24) +
    theme(legend.position = "none")
  
  save_interactive(p, path, caption, log_file)
}
