plot_count_ties_san <- function(disaggregated_san, path,
                                caption = "no caption provided",
                                log_file = "plot_tracker.Rmd") {
  
  p <- disaggregated_san[, {g <- graph_from_data_frame(.SD[, -"dyad_id"])
  .(total_nodes = length(V(g)),
    total_ties = length(E(g)),
    negative_ties = sum(E(g)$sign == -1),
    positive_ties = sum(E(g)$sign == 1))}, by = "year"] |>
    ggplot(aes(x = year)) +
    geom_line(aes(y = total_ties, col = "Total Ties")) +
    geom_point(aes(y = total_ties, col = "Total Ties")) +
    geom_line(aes(y = negative_ties, col = "Negative Ties")) +
    geom_point(aes(y = negative_ties, col = "Negative Ties")) +
    geom_line(aes(y = positive_ties, col = "Positive Ties")) +
    geom_point(aes(y = positive_ties, col = "Positive Ties")) +
    labs(x = "Year", y = "# of Ties", color = "") +
    scale_color_manual(values = c("Total Ties" = "black",
                                  "Negative Ties" = "darkred",
                                  "Positive Ties" = "darkgreen")) +
    line1 + line2 +
    theme_minimal(base_size = 24) +
    theme(legend.position = "none")
  
  save_interactive(p, path, caption, log_file)
}
