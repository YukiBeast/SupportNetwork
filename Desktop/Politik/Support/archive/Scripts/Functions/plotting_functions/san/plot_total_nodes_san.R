plot_total_nodes_san <- function(disaggregated_san, path) {
  p <- disaggregated_san[, {g <- graph_from_data_frame(.SD[, -"dyad_id"])
  .(total_nodes = length(V(g)),
    total_ties = length(E(g)),
    negative_ties = sum(E(g)$sign == -1),
    positive_ties = sum(E(g)$sign == 1))}, by = "year"] |>
    ggplot(aes(x = year)) +
    geom_line(aes(y = total_nodes), col = "cyan3", linewidth = 1) +
    geom_point(aes(y = total_nodes), col = "cyan3", size = 2) +

    line1 + line2 +

    labs(x = "Year", y = "Total Nodes") +
    theme_minimal(base_size = 18)
  
  save_interactive(p, path)
}
