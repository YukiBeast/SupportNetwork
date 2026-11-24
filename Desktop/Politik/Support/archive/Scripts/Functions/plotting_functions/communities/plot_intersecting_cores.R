plot_intersecting_cores <- function(core_df, target_window, min_ratio = 0.0) {
  library(igraph)
  
  core_df <- core_df[actor != core, ]
  # 1. Filter data for the chosen window and anchors
  dt <- core_df[window == target_window & 
                  actor %in% c("United States of America", "Russia (Soviet Union)") &
                  ratio >= min_ratio] 
  
  if (nrow(dt) == 0) stop("No data found for this window.")
  
  # 2. Build the graph
  edges <- dt[, .(from = actor, to = core, weight = ratio)]
  nodes <- data.frame(name = unique(c(edges$from, edges$to)))
  g <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
  
  # 3. Default node styling (Peripheral members)
  V(g)$color <- rep("grey85", length(V(g)))
  V(g)$size <- rep(8, length(V(g)))
  V(g)$label.cex <- rep(0.8, length(V(g)))
  V(g)$label.color <- rep("black", length(V(g)))
  V(g)$frame.color <- rep("white", length(V(g)))
  
  # Style USA (Anchor)
  if ("United States of America" %in% V(g)$name) {
    idx_usa <- which(V(g)$name == "United States of America")
    V(g)$color[idx_usa] <- "steelblue"
    V(g)$size[idx_usa] <- 20
#    V(g)$label.font[idx_usa] <- 2 # Bold
  }
  
  # Style Russia (Anchor)
  if ("Russia (Soviet Union)" %in% V(g)$name) {
    idx_rus <- which(V(g)$name == "Russia (Soviet Union)")
    V(g)$color[idx_rus] <- "firebrick"
    V(g)$size[idx_rus] <- 20
#    V(g)$label.font[idx_rus] <- 2 # Bold
  }
  
  # 4. Generate Force-Directed Layout
  l <- layout_with_fr(g, weights = E(g)$weight)
  
  # 5. Rotate the layout so USA and Russia are at the same height
  if (("United States of America" %in% V(g)$name) && 
      ("Russia (Soviet Union)" %in% V(g)$name)) {
    
    # Calculate the difference in coordinates
    dx <- l[idx_rus, 1] - l[idx_usa, 1]
    dy <- l[idx_rus, 2] - l[idx_usa, 2]
    
    # Find the angle of the line connecting them
    angle <- atan2(dy, dx)
    
    # Rotate the entire layout by the negative angle to make it horizontal
    cos_a <- cos(-angle)
    sin_a <- sin(-angle)
    
    new_x <- l[, 1] * cos_a - l[, 2] * sin_a
    new_y <- l[, 1] * sin_a + l[, 2] * cos_a
    
    l[, 1] <- new_x
    l[, 2] <- new_y
  }
  
  # 6. Plot the graph
  plot(g, layout = l,
       edge.width = E(g)$weight * 3, # Thicker edges for higher ratios
       edge.color = rgb(0.5, 0.5, 0.5, alpha = E(g)$weight), # Edges fade based on ratio
       main = paste0("", target_window))
  # Core-Periphery Structure: 
}

# How to use it:
plot_intersecting_cores(core_df, "1975-1991")
plot_intersecting_cores(core_df, "1992-2001")
plot_intersecting_cores(core_df, "2002-2017")

par(mfrow = c(3, 1), mar = c(1, 1, 2, 1))

# 2. Call your function for each of the three windows
plot_intersecting_cores(core_df, "1975-1991")
plot_intersecting_cores(core_df, "1992-2001")
plot_intersecting_cores(core_df, "2002-2017")

# 3. Reset the plotting area back to default (1 plot per layout) when you are done
par(mfrow = c(1, 1))
