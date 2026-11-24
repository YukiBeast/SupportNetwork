library(ggplot2)

plot_mod_smon <- function(smon_results, 
                          chosen_k = NULL, 
                          path = NULL, 
                          base_size = 18) {
  
  df <- as.data.frame(smon_results$results_summary)
  
  # Identify individual maxima
  min_mod_disc <- min(df$mod_discrete)
  max_mod_disc <- max(df$mod_discrete)
  max_disc <- df$k[which.max(df$mod_discrete)]
  max_cont <- df$k[which.max(df$mod_continuous)]
  
  # 1. Dual-axis transformation scaling factors
  range_disc <- range(df$mod_discrete, na.rm = TRUE)
  range_cont <- range(df$mod_continuous, na.rm = TRUE)
  
  # Safeguard against flat values
  diff_disc <- if (diff(range_disc) == 0) 1 else diff(range_disc)
  diff_cont <- if (diff(range_cont) == 0) 1 else diff(range_cont)
  
  a_scale  <- diff_cont / diff_disc
  b_offset <- range_cont[1] - a_scale * range_disc[1]
  
  # Dynamic vertical positions for text annotations
  y_top <- max(df$mod_discrete, na.rm = TRUE)
  y_pos_disc <- y_top * 0.95
  y_pos_cont <- y_top * 0.85
  y_pos_opt  <- y_top * 0.75
  
  # 2. Build the Plot
  p <- ggplot(df, aes(x = k)) +
    # Modularity curves
    geom_line(aes(y = mod_discrete, col = "Discrete"), linewidth = 1.2) +
    geom_line(aes(y = (mod_continuous - b_offset) / a_scale, col = "Continuous"), linewidth = 1.2) +
    
    # Reference markers for maxima
    geom_vline(xintercept = max_disc, col = "#888888", linetype = "dashed", linewidth = 0.8) +
    geom_vline(xintercept = max_cont, col = "black", linetype = "dashed", linewidth = 0.8) +
    
    # Labels for maxima
    annotate("text", x = max_disc + 0.35, y = min_mod_disc, 
             label = paste0("max(disc) = ", max_disc), col = "#888888", 
             size = base_size / 3.2, fontface = "bold", hjust = 0) +
    annotate("text", x = max_cont + 0.35, y = min_mod_disc, 
             label = paste0("max(cont) = ", max_cont), col = "black", 
             size = base_size / 3.2, fontface = "bold", hjust = 0)
  
  # 3. Optional: Highlight the final chosen k (e.g. from choose_k)
  if (!is.null(chosen_k)) {
    # If the user passed the choice object directly (e.g. choice_8591$k), extract it
    k_val <- if (is.list(chosen_k)) chosen_k$k else chosen_k
    
    p <- p +
      geom_vline(xintercept = k_val, col = "darkgoldenrod", linetype = "solid", linewidth = 1.1) +
      annotate("text", x = k_val + 0.35, y = mean(c(min_mod_disc, max_mod_disc)), 
               label = paste0("Optimal k* = ", k_val), col = "darkgoldenrod", 
               size = base_size / 2.8, fontface = "bold", hjust = 0)
  }
  
  # 4. Axis and Theme Configuration
  p <- p +
    scale_y_continuous(
      name = "Discrete Modularity",
      sec.axis = sec_axis(~ . * a_scale + b_offset, name = "Continuous Modularity")
    ) +
    scale_color_manual(
      name = "Modularity Type",
      values = c("Discrete" = "#888888", "Continuous" = "black")
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
    theme_minimal(base_size = base_size) +
    theme(
      axis.title.y.left  = element_text(color = "#888888", face = "bold", margin = margin(r = 10)),
      axis.text.y.left   = element_text(color = "#888888"),
      axis.title.y.right = element_text(color = "black", face = "bold", margin = margin(l = 10)),
      axis.text.y.right  = element_text(color = "black"),
      legend.position    = "bottom"
    )
  
  save_interactive(p, path)
}
