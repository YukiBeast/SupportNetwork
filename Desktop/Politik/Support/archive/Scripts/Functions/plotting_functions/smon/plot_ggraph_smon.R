
library(igraph)
library(ggraph)
library(tidygraph)
library(ggplot2)
library(graphlayouts)

plot_smon_ggraph <- function(san_filtered, communities, path = NULL,
                             top_n = NULL, show_labels = TRUE,
                             mind_comm_size = 0, priority_nodes = NULL,
                             show_legend = TRUE,
                             keep_largest_component = TRUE,
                             stretch = 0.6,
                             caption = "No caption provided",
                             log_file = "plot_tracker.Rmd") {
  
  set.seed(123)
  
  if (!("sign" %in% names(san_filtered))) {
    san_filtered$sign <- sign(san_filtered$balance)
  }
  
  g <- graph_from_data_frame(
    san_filtered, directed = FALSE, vertices = communities
  )
  
  # Remove nodes without an assigned community
  g <- delete_vertices(g, V(g)[is.na(community)])

  # --- 1. keep only the largest connected component ---
  if (keep_largest_component) {
    comp <- components(g)
    g <- induced_subgraph(
      g, V(g)[comp$membership == which.max(comp$csize)]
    )
  }
  
  # --- 2. compute layout on exactly this graph ---
  E(g)$distance <- 14 - E(g)$balance
  lay <- layout_with_stress(g, weights = E(g)$distance)
  lay <- scale(lay, center = TRUE, scale = FALSE)
  
  r <- pmax(sqrt(rowSums(lay^2)), 1e-6)
  lay <- lay * r^(stretch - 1)
  rownames(lay) <- V(g)$name
  
  # --- 3. community size filtering with priority protection ---
  if (mind_comm_size > 0) {
    
    comm_counts <- table(V(g)$community)
    valid_comms <- names(comm_counts[comm_counts >= mind_comm_size])
    
    if (!is.null(priority_nodes)) {
      priority_comms <- unique(
        as.character(V(g)$community[V(g)$name %in% priority_nodes])
      )
      valid_comms <- union(valid_comms, priority_comms)
    }
    
    valid_nodes <- V(g)[as.character(V(g)$community) %in% valid_comms]
    g <- induced_subgraph(g, valid_nodes)
  }
  
  # --- 4. subset layout to surviving nodes ---
  idx <- match(V(g)$name, rownames(lay))
  
  if (anyNA(idx)) {
    g <- delete_vertices(g, V(g)$name[is.na(idx)])
    idx <- match(V(g)$name, rownames(lay))
  }
  
  lay_aligned <- lay[idx, , drop = FALSE]
  
  # --- 5. calculate node attributes ---
  V(g)$degree <- degree(g) * 4
  V(g)$label <- get_iso3(V(g)$name)
  
  # Clean up probabilities for alpha scaling
  node_alphas <- V(g)$prob
  if (is.null(node_alphas)) node_alphas <- rep(1, vcount(g))
  
  V(g)$alpha_val <- ifelse(is.na(node_alphas), 1, node_alphas)
  
  # --- 6. determine community coloring strategy ---
  
  comm_counts <- sort(table(V(g)$community), decreasing = TRUE)
  all_comms <- names(comm_counts)
  
  if (!is.null(top_n)) {
    
    # Identify priority communities
    priority_comms <- character(0)
    
    if (!is.null(priority_nodes)) {
      priority_comms <- unique(
        as.character(V(g)$community[V(g)$name %in% priority_nodes])
      )
    }
    
    # Select top communities, protecting priority communities
    remaining_comms <- setdiff(all_comms, priority_comms)
    top_comms_ordered <- c(priority_comms, remaining_comms)
    
    top_comms <- head(
      top_comms_ordered,
      max(top_n, length(priority_comms))
    )
    
    # Prepare legend labels
    comm_labels <- sapply(top_comms, function(c_id) {
      member_names <- V(g)$name[
        as.character(V(g)$community) == c_id
      ]
      
      member_isos <- get_iso3(member_names)
      
      paste0(
        "Comm. ", c_id, " (",
        paste(sort(member_isos), collapse = ", "), ")"
      )
    })
    
    names(comm_labels) <- top_comms
    
    # Assign remaining communities to the grey category
    V(g)$legend_group <- ifelse(
      as.character(V(g)$community) %in% top_comms,
      comm_labels[as.character(V(g)$community)],
      "Other (Periphery/Singletons)"
    )
    
    V(g)$legend_group <- factor(
      V(g)$legend_group,
      levels = c(comm_labels, "Other (Periphery/Singletons)")
    )
    
    # Community palette
    base_palette <- c(
      "dodgerblue3", "firebrick3", "darkorchid3", "gold2",
      "darkorange2", "forestgreen", "deeppink3", "cyan4"
    )
    
    final_palette <- setNames(
      c(
        rep(base_palette, length.out = length(top_comms)),
        "gray80"
      ),
      c(comm_labels, "Other (Periphery/Singletons)")
    )
    
  } else {
    
    # Color every community individually
    V(g)$legend_group <- as.factor(V(g)$community)
  }
  
  # --- 7. plot with ggraph ---
  
  p <- ggraph(g, layout = lay_aligned) +
    geom_edge_arc(
      aes(color = as.factor(sign)),
      strength = 0, alpha = 0.3, width = 0.3
    ) +
    scale_edge_color_manual(
      values = c(
        "1" = "#117733",
        "-1" = "#661100",
        "0" = "#888888"
      ),
      guide = "none"
    ) +
    
    geom_node_point(
      aes(size = degree),
      shape = 21, fill = "white",
      color = "white", stroke = 1
    ) +
    
    geom_node_point(
      aes(
        fill = legend_group,
        size = degree,
        alpha = alpha_val
      ),
      shape = 21, color = "black", stroke = 1
    ) +
    
    scale_size_continuous(
      range = c(1, 26), guide = "none"
    ) +
    scale_alpha_continuous(
      range = c(0.2, 1), guide = "none"
    ) +
    theme_void()
  
  # Apply appropriate fill scale
  if (!is.null(top_n)) {
    
    p <- p + scale_fill_manual(
      values = final_palette,
      name = "Top Geopolitical Communities & Members"
    )
    
  } else {
    
    p <- p + scale_fill_discrete(
      name = "Geopolitical Communities"
    )
  }
  
  # --- 8. legend formatting ---
  
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
  
  # --- 9. node labels ---
  
  if (show_labels) {
    p <- p + geom_node_text(
      aes(label = label),
      repel = TRUE, size = 6, fontface = "bold"
    )
  }
  
  # --- 10. execution and saving ---
  
  save_interactive(p, path, caption, log_file)
}



