library(igraph)
library(data.table)

plot_smon_network <- function(san, year.f, smon_choice,
                              path = NULL, top_n = 5, show_labels = TRUE,
                              show_legend = FALSE,
                              mind_comm_size = 0, priority_nodes = NULL,
                              caption = "no caption provided",
                              log_file = "plot_tracker.Rmd"
                              ) {
  
  set.seed(123)
  
  # --- 1. Filter Edge Data by Year(s) ---
  dt <- as.data.table(copy(san))
  san_filtered <- dt[year %in% year.f, .(state1, state2, balance)]
  
  if (!("sign" %in% names(san_filtered))) {
    san_filtered$sign <- sign(san_filtered$balance)
  }
  
  # --- 2. Extract Communities from Chosen SMON Object ---
  # Supports either choose_k output ($z_norm) or older full smon_results ($best_z_norm)
  target_z <- if (!is.null(smon_choice$z_norm)) smon_choice$z_norm else smon_choice$best_z_norm
  communities <- assign_communities(target_z)
  
  # Only retain metadata for nodes active in the filtered period
  active_nodes <- unique(c(san_filtered$state1, san_filtered$state2))
  comm_filtered <- communities[communities$actors %in% active_nodes, ]
  
  # Note: vertices dataframe expects the first column to be node names
  g <- graph_from_data_frame(san_filtered, directed = FALSE, vertices = comm_filtered)
  
  # --- 3. Community Size Filtering with Priority Protection ---
  if (mind_comm_size > 0) {
    comm_counts <- table(V(g)$community)
    valid_comms <- names(comm_counts[comm_counts >= mind_comm_size])
    
    # Ensure priority nodes' communities are never deleted
    if (!is.null(priority_nodes)) {
      priority_comms <- unique(as.character(V(g)$community[V(g)$name %in% priority_nodes]))
      valid_comms <- union(valid_comms, priority_comms)
    }
    
    valid_nodes <- V(g)[as.character(V(g)$community) %in% valid_comms]
    g <- induced_subgraph(g, valid_nodes)
  }
  
  # Strip disconnected isolates
  g <- delete_vertices(g, V(g)[degree(g) == 0])
  
  # --- 4. Edge & Node Sizing ---
  E(g)$color <- ifelse(E(g)$sign > 0, "chartreuse4", "brown3")
  E(g)$color <- ifelse(E(g)$sign == 0, "gray34", E(g)$color)
  
  deg <- degree(g)
  v_sizes <- 6 + (deg / max(pmax(deg, 1))) * 19
  
  V(g)$label <- get_iso3(V(g)$name)
  if (!show_labels) V(g)$label <- NA
  
  # --- 5. Dynamic Top Communities & Palette Assignment ---
  comm_counts <- sort(table(V(g)$community), decreasing = TRUE)
  all_comms <- names(comm_counts)
  
  priority_comms <- character(0)
  if (!is.null(priority_nodes)) {
    priority_comms <- unique(as.character(V(g)$community[V(g)$name %in% priority_nodes]))
  }
  
  remaining_comms <- setdiff(all_comms, priority_comms)
  top_comms_ordered <- c(priority_comms, remaining_comms)
  top_comms <- head(top_comms_ordered, max(top_n, length(priority_comms)))
  
  base_palette <- c("dodgerblue3", "firebrick3", "darkorchid3", "gold2", 
                    "darkorange2", "forestgreen", "deeppink3", "cyan4")
  palette <- base_palette[1:length(top_comms)]
  color_mapping <- setNames(palette, top_comms)
  
  # Match by character name to prevent positional indexing bugs
  V(g)$color <- ifelse(as.character(V(g)$community) %in% top_comms, 
                       color_mapping[as.character(V(g)$community)], 
                       "gray80")
  
  # --- 6. Legend Label Generation ---
  community_member_labels <- sapply(top_comms, function(c_id) {
    member_names <- V(g)$name[as.character(V(g)$community) == c_id]
    member_isos <- get_iso3(member_names)
    paste0("Comm. ", c_id, " (", paste(sort(member_isos), collapse = ", "), ")")
  })
  
  # --- 7. Layout & Drawing Wrapper ---
  l <- layout_components(g, layout = layout_with_kk, weights = NA)
  
  draw_plot <- function() {
    par(mar = c(8, 1, 3, 1)) 
    
    # Handle alpha probabilities safely (clipped to [0.15, 1.0])
    node_alphas <- V(g)$prob
    if (is.null(node_alphas)) node_alphas <- rep(1, vcount(g))
    node_alphas <- ifelse(is.na(node_alphas), 1, pmin(pmax(node_alphas, 0.15), 1))
    
    # Helper function to mix a color with white to simulate transparency opaquely
    mix_with_white <- function(col, weight) {
      rgb_val <- col2rgb(col)
      # Mix the color with white (255, 255, 255) based on the weight (probability)
      mixed_rgb <- (rgb_val * weight) + (255 * (1 - weight))
      rgb(mixed_rgb[1,], mixed_rgb[2,], mixed_rgb[3,], maxColorValue = 255)
    }
    
    # Apply the mix to all vertex colors using their alpha (probability) weights
    v_colors_opaque <- unlist(Map(mix_with_white, V(g)$color, node_alphas))    
    plot(g,
         layout             = l,
         margin             = 0,
         
         vertex.size        = v_sizes / 1.5, 
         vertex.color       = v_colors_opaque,         
         vertex.frame.color = "gray30",
         
         edge.color         = adjustcolor(E(g)$color, alpha.f = 0.5), 
         edge.width         = 1.2,                
         edge.curved        = 0.15,                
         
         vertex.label       = V(g)$label,
         vertex.label.color = "black",
         vertex.label.cex   = 0.75,
         vertex.label.family= "sans",
         vertex.label.font  = 2,
         vertex.label.dist  = 0.8
    )
    
    # Bottom Legend
    par(xpd = TRUE)
    if (show_legend) {
      legend(x = 0, y = -1.15, 
             legend = c(community_member_labels, "Other (Periphery/Singletons)"),
             pt.bg  = c(palette, "gray80"),
             pch    = 21, 
             pt.cex = 1.2,
             col    = "gray30", 
             title  = "Top Geopolitical Communities & Members",
             bty    = "n", 
             cex    = 0.7,
             horiz  = FALSE)
    }

  }
  
  draw_plot()
  
  # --- 8. Interactive PDF Export ---
  if (!is.null(path) && nzchar(trimws(path))) {
    ans <- readline(prompt = "Do you want to save the plot? (yes/no): ")
    if (tolower(trimws(ans)) %in% c("ja", "j", "yes", "y")) {
      pdf(file = path, width = 12, height = 10) 
      draw_plot()
      dev.off()
      cat("Success! Plot saved to:", path, "\n")
      update_tracker(path, caption, log_file)
    } else {
      cat("Okay, plot was not saved.\n")
    }
  }
}

# plot_smon_network(san, year.f = 1985:1991, choice_8591, top_n = 8)
