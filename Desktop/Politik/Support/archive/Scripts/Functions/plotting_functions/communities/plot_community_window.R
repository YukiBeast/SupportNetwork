plot_community_window <- function(graphs, window_name, path = NULL, top_n = 5, show_labels = TRUE,
                                  mind_comm_size = 0) {
  set.seed(1)
  library(igraph, quietly = TRUE)
  library(countrycode, quietly = TRUE)
  
  g <- graphs[[window_name]]
  
  if (mind_comm_size > 0) {

    # 1. Count the frequency of each community
    comm_counts <- table(V(g)$community)
    
    # 2. Identify the valid community IDs
    valid_comms <- names(comm_counts[comm_counts >= mind_comm_size])
    
    # 3. Filter the vertices and extract the subgraph
    valid_nodes <- V(g)[V(g)$community %in% valid_comms]
    g <- induced_subgraph(g, valid_nodes)
  }
  
  # 1. Edge Colors: Green for Positive, Red for Negative, Gray for Balanced
  E(g)$color <- ifelse(E(g)$sign > 0, "chartreuse4", "brown3")
  E(g)$color <- ifelse(E(g)$sign == 0, "gray34", E(g)$color)
  
  # 2. Node Sizes: Scaled by degree
  deg <- degree(g)
  v_sizes <- 6 + (deg / max(deg)) * 19
  
  V(g)$label <- get_iso3(V(g)$name)
  
  if (!show_labels) V(g)$label <- NA
  
  # 4. Node Colors: Top Communities mapped to a vibrant palette
  comm_counts <- sort(table(V(g)$community), decreasing = TRUE)
  top_comms <- names(comm_counts)[1:min(top_n, length(comm_counts))]
  

  
  palette <- c("dodgerblue3", "darkorange2", "darkorchid3", "gold2", "firebrick3")[1:length(top_comms)]
  color_mapping <- setNames(palette, top_comms)
  
  V(g)$color <- ifelse(V(g)$community %in% top_comms, 
                       color_mapping[V(g)$community], 
                       "gray80")
  
  # Generate text labels containing the actual members (ISO3 codes) for each top community
  community_member_labels <- sapply(top_comms, function(c_id) {
    member_names <- V(g)$name[V(g)$community == c_id]
    member_isos <- get_iso3(member_names)
    paste0("Comm. ", c_id, " (", paste(sort(member_isos), collapse = ", "), ")")
  })
  
  # 5. Layout Calculation (Leave extra bottom room for the text legend)
  l <- layout_components(g, layout = layout_with_kk, weights = NA)
  
  # 6. Plotting Wrapper Function
  draw_plot <- function() {
    # Adjust outer margins to provide ample space underneath for the member list
    par(mar = c(8, 1, 3, 1)) 
    
    plot(g,
         main = paste0("Support Actor Network: ", window_name),
         layout = l,
         margin = 0,
         
         vertex.size = v_sizes / 1.5, 
         vertex.color = adjustcolor(V(g)$color, alpha.f = 0.85),
         vertex.frame.color = "gray30",
         
         edge.color = adjustcolor(E(g)$color, alpha.f = 0.5),
         edge.width = 1.2,                
         edge.curved = 0.15,               
         
         vertex.label.color = "black",
         vertex.label.cex = 0.75,
         vertex.label.family = "sans",
         vertex.label.font = 2,
         vertex.label.dist = 0.8
    )
    
    # Legend placed cleanly underneath the plot mapping colors to member lists
    par(xpd = TRUE) # Allow plotting outside the main bounding box
    legend(x = 0, y = -1.15, 
           legend = c(community_member_labels, "Other (Periphery/Singletons)"),
           pt.bg = c(palette, "gray80"),
           pch = 21, 
           pt.cex = 1.2,
           col = "gray30",
           title = "Top Geopolitical Communities & Members",
           bty = "n", 
           cex = 0.7,
           horiz = FALSE)
  }
  
  # 7. Render Plot
  draw_plot()
  
  # 8. Interactive Prompt for PDF Saving
  if (!is.null(path)) {
    ans <- readline(prompt = paste0("Do you want to save the plot for ", window_name, "? (yes/no): "))
    
    if (tolower(trimws(ans)) %in% c("ja", "j", "yes", "y")) {
      pdf(file = path, width = 12, height = 10) # Expanded canvas height for the bottom legend
      draw_plot()
      dev.off()
      cat("Success! Plot saved to:", path, "\n")
    } else {
      cat("Okay, plot was not saved.\n")
    }
  }
}
for (window in unique(epoch_metrics$Window)) {
  plot_community_window(epoch_graphs,
                        window,
                        paste0("Results/Pics/communities/net_",
                               window,
                               "_min4_lab.pdf"),
                        5,
                        TRUE,
                        4)
  
  plot_community_window(epoch_graphs,
                        window,
                        paste0("Results/Pics/communities/net_",  window,
                               "_min4_nolab.pdf"),
                        5,
                        FALSE,
                        4)
}


# Run export scripts:
# for (i in 1975:2013) {
#   plot_community_window(results$graphs,
#                         window_name = paste0(i, "-", i+4),
#                         path = paste0("Results/Pics/communities/all_nets/community_window_",
#                                       gsub("^([0-9]{2})", "", i),
#                                       gsub("^([0-9]{2})", "", i + 4),
#                                       ".pdf"))
  
#}
# plot_community_window(results$graphs, "1975-1979", "Results/Pics/communities/community_window_7579.pdf")
# plot_community_window(results$graphs, "1984-1988", "Results/Pics/communities/community_window_8488.pdf")
# plot_community_window(results$graphs, "1991-1995", "Results/Pics/communities/community_window_9195.pdf")
# plot_community_window(results$graphs, "2005-2009", "Results/Pics/communities/community_window_0509.pdf")
