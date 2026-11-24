avg_conflicts <- function(data, path, upper_limit = 7.5,
                          caption = "no caption provided",
                          log_file = "plot_tracker.Rmd") {
  
  p <- data[, .(tot_conflicts_per_actor = length(unique(.SD$dyad_id))),
            by = c("year", "ext_name")][, .(avg_conflicts_per_actor = mean(tot_conflicts_per_actor)),
                                        by = "year"][
                                          data[, .(tot_actors_per_conflict = length(unique(.SD$ext_name))),
                                               by = c("year", "dyad_id")][, .(avg_actors_per_conflict= mean(tot_actors_per_conflict)),
                                                                          by = "year"],
                                          on = "year"
                                        ] |>
    ggplot() +
    
    geom_point(aes(x = year, y = avg_conflicts_per_actor,
                   col = "Avg. Conflicts per Actor")) +
    geom_line(aes(x = year,
                  y = avg_conflicts_per_actor,
                  col = "Avg. Conflicts per Actor"),
              linewidth = 0.8) +
    geom_smooth(aes(x = year, y = avg_conflicts_per_actor,
                    col = "Avg. Conflicts per Actor"),
                linewidth = 0.6,
                se = FALSE) +
    
    geom_point(aes(x = year, y = avg_actors_per_conflict,
                   col = "Avg. Actors per Conflict")) +
    geom_line(aes(x = year, y = avg_actors_per_conflict,
                  col = "Avg. Actors per Conflict"),
              linewidth = 0.8) +
    geom_smooth(aes(x = year, y = avg_actors_per_conflict,
                    col = "Avg. Actors per Conflict"),
                linewidth = 0.6,
                se = FALSE) +
    
    scale_color_manual(values = c("Avg. Conflicts per Actor" = "orange3",
                                  "Avg. Actors per Conflict" = "cyan3"),
                       name = NULL) + line1 + line2 +
    labs(x = "Year",
         y = "Average") +
    scale_y_continuous(limits = c(0, upper_limit)) +
    theme_minimal(base_size = 24) +
    theme(legend.position = "none")
    
    save_interactive(p, path, caption, log_file)
}
