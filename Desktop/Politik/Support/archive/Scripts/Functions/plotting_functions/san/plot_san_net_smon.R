plot_san_net_smon <- function(san, year.f, path = NULL,
                              caption = "No caption provided",
                              log_file = "plot_tracker.Rmd",
                              legend.position = "bottomleft",
                              smon_choice,
                              top_n = 8,
                              mind_comm_size = 2,
                              priority_nodes = c("United States of America", "Russia (Soviet Union)"),
                              layout = NULL) {
  set.seed(1)
  library(igraph, quietly = TRUE)
  library(data.table, quietly = TRUE)
  library(countrycode, quietly = TRUE)
  
  if (!("sign" %in% names(san))) {
    san$sign <- sign(san$balance)
  }
  
  # --- 1. Filter by Year & Match Vertex Order ---
  target_z <- if (!is.null(smon_choice$z_norm)) smon_choice$z_norm else smon_choice$best_z_norm
  communities <- assign_communities(target_z)
  
  comm_map <- setNames(as.character(communities$community), communities$actors)
  prob_map <- setNames(communities$prob, communities$actors)
  
  dt <- as.data.table(copy(san))
  dt <- dt[year %in% year.f, .(state1, state2, balance, sign)]
  
  g_full <- graph_from_data_frame(dt, directed = FALSE)
  
  V(g_full)$community <- comm_map[V(g_full)$name]
  V(g_full)$prob <- prob_map[V(g_full)$name]
  
  # --- 2. Master Layout Coordinates (Normalized [-1, 1]) ---
  if (!is.null(layout)) {
    raw_coords <- layout
  } else {
    raw_coords <- layout_components(g_full, layout = layout_with_kk)
  }
  
  master_coords <- norm_coords(raw_coords, ymin = -1, ymax = 1, xmin = -1, xmax = 1)
  rownames(master_coords) <- V(g_full)$name
  
  # --- 3. Filter Nodes with Priority Protection ---
  comm_counts <- table(V(g_full)$community)
  valid_comms <- names(comm_counts[comm_counts >= mind_comm_size])
  
  if (!is.null(priority_nodes)) {
    priority_comms <- unique(as.character(V(g_full)$community[V(g_full)$name %in% priority_nodes]))
    valid_comms <- union(valid_comms, priority_comms)
  }
  
  g_sub <- induced_subgraph(g_full, V(g_full)[as.character(V(g_full)$community) %in% valid_comms])
  g_sub <- delete_vertices(g_sub, V(g_sub)[degree(g_sub) == 0])
  
  layout_sub <- master_coords[V(g_sub)$name, , drop = FALSE]
  
  # --- 4. Color Palette & Community Mapping ---
  comm_sub_counts <- sort(table(V(g_sub)$community), decreasing = TRUE)
  all_comms <- names(comm_sub_counts)
  
  priority_sub_comms <- character(0)
  if (!is.null(priority_nodes)) {
    priority_sub_comms <- unique(as.character(V(g_sub)$community[V(g_sub)$name %in% priority_nodes]))
  }
  
  remaining_comms <- setdiff(all_comms, priority_sub_comms)
  top_comms <- head(c(priority_sub_comms, remaining_comms), max(top_n, length(priority_sub_comms)))
  
  base_palette <- c("dodgerblue3", "firebrick3", "darkorchid3", "gold2", 
                    "darkorange2", "forestgreen", "deeppink3", "cyan4",
                    "sienna3", "mediumpurple3", "springgreen4", "palevioletred3")
  
  palette <- base_palette[1:length(top_comms)]
  color_mapping <- setNames(palette, top_comms)
  
  base_colors <- ifelse(as.character(V(g_sub)$community) %in% top_comms,
                        color_mapping[as.character(V(g_sub)$community)],
                        "gray80")
  
  # Extract probabilities, bound them to [0.15, 1] so they don't vanish entirely
  node_alphas <- V(g_sub)$prob
  if (is.null(node_alphas)) node_alphas <- rep(1, vcount(g_sub))
  node_alphas <- ifelse(is.na(node_alphas), 1, pmin(pmax(node_alphas, 0.15), 1))
  
  # --- CUSTOM SOLID COLOR MIXING (NO ALPHA TRANSPARENCY) ---
  mix_with_white <- function(col, weight) {
    rgb_val <- col2rgb(col)
    mixed_rgb <- (rgb_val * weight) + (255 * (1 - weight))
    rgb(mixed_rgb[1,], mixed_rgb[2,], mixed_rgb[3,], maxColorValue = 255)
  }
  v_colors_opaque <- unlist(Map(mix_with_white, base_colors, node_alphas))
  
  # --- 5. Sizing & Labels ---
  deg <- degree(g_sub)
  v_sizes <- 6 + (deg / max(pmax(deg, 1))) * 19
  V(g_sub)$label <- get_iso3(V(g_sub)$name)
  
  # --- 6. Plotting Wrapper ---
  draw_plot <- function() {
    par(mar = c(5.1, 4.1, 4.1, 2.1))
    
    plot(g_sub,
         layout             = layout_sub,
         rescale            = FALSE,
         xlim               = c(-1, 1),
         ylim               = c(-1, 1),
         margin             = -0.1,
         asp                = 1,
         
         vertex.size        = v_sizes / 3,
         vertex.color       = v_colors_opaque,     # <--- Updated to the solid mixed colors
         vertex.frame.color = "gray30",
         
         edge.color         = adjustcolor("grey", alpha.f = 0.5),
         edge.width         = 1.5,
         edge.curved        = 0.1,
         
         vertex.label       = V(g_sub)$label,
         vertex.label.color = "black",
         vertex.label.cex   = 0.75,
         vertex.label.family= "sans",
         vertex.label.font  = 2,
         vertex.label.dist  = 1
    )
    
    if (legend.position %in% c("bottomright", "bottom", "bottomleft", "left",
                               "topleft", "top", "topright", "right", "center")) {
      legend(legend.position, 
             legend = c(paste0("Comm. ", top_comms), "Other"),
             pt.bg  = c(palette, "gray80"),
             pch    = 21,
             pt.cex = 1.3,
             col    = "gray30", 
             bty    = "n",
             cex    = 0.75,
             title  = "Communities")
    }
  }
  
  draw_plot()
  
  if (!is.null(path)) {
    antwort <- readline(prompt = "Möchtest du den Plot speichern? (ja/nein): ")
    if (tolower(trimws(antwort)) %in% c("ja", "j", "yes", "y")) {
      pdf(file = path, width = 10, height = 8)
      draw_plot()
      dev.off()
      cat("Erfolg! Plot wurde gespeichert unter:", path, "\n")
      if (exists("update_tracker", mode = "function")) {
        update_tracker(path, caption, log_file)
      }
    } else {
      cat("Okay, Plot wurde nicht gespeichert.\n")
    }
  }
}
