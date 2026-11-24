plot_ggraph_san <- function(san_filtered, show_labels = TRUE, show_legend = FALSE,
                            keep_largest_component = TRUE,
                            path,
                            caption = "No caption provided",
                            log_file = "plot_tracker.Rmd",
                            stretch = 0.6) {   # <1 pulls periphery in, stretches core
  
  library(ggraph)
  library(graphlayouts)   # for layout_with_stress
  
  set.seed(123)
  
  if (!("sign" %in% names(san_filtered))) {
    san_filtered$sign <- sign(san_filtered$balance)
  }
  
  g <- graph_from_data_frame(san_filtered, directed = FALSE)
  
  if (keep_largest_component) {
    comp <- components(g)
    g <- induced_subgraph(g, V(g)[comp$membership == which.max(comp$csize)])
  }
  
  V(g)$degree <- degree(g) * 4
  V(g)$label <- get_iso3(V(g)$name)
  E(g)$distance <- 14 - E(g)$balance
  
  # --- custom layout: stress, then radial compression ---
  lay <- layout_with_stress(g, weights = E(g)$distance)
  lay <- scale(lay, center = TRUE, scale = FALSE)   # center on origin
  r <- pmax(sqrt(rowSums(lay^2)), 1e-6)             # radial distance per node
  lay <- lay * r^(stretch - 1)                      # stretch core, shrink periphery
  
  p <- ggraph(g, layout = lay) +
    geom_edge_arc(aes(color = as.factor(sign)), strength = 0, alpha = 0.3, width = 0.3) +
    scale_edge_color_manual(values = c("1" = "#117733", "-1" = "#661100", "0" = "#888888"), guide = "none") +
    geom_node_point(aes(size = degree), shape = 21, fill = "white", color = "white", stroke = 1) +
    geom_node_point(aes(size = degree), fill = "cyan3", alpha = 0.5,
                    shape = 21, color = "black", stroke = 1) +
    scale_size_continuous(range = c(1, 26), guide = "none") +
    theme_void()
  
  if (show_legend) {
    p <- p + theme(
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 9)
    )
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  if (show_labels) {
    p <- p + geom_node_text(aes(label = label), repel = TRUE, size = 6, fontface = "bold")
  }
  
  save_interactive(p, path, caption, log_file)
  return(invisible(lay))
}
plot_ggraph_san(san_7591)
