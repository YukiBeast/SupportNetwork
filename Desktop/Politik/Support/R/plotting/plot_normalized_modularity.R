
library(data.table)
library(ggplot2)

plot_normalized_modularity <- function(avg_epochs,
                                       facet_ncol = 2,
                                       path,
                                       caption = "No caption provided",
                                       log_file = "plot_tracker.Rmd") {
  
  dt <- copy(avg_epochs)
  setorder(dt, epoch, k)
  
  # --- 1. Calculate normalization parameters within each epoch ---
  
  ranges <- dt[, .(
    min_cont = min(avg_mod_cont, na.rm = TRUE),
    max_cont = max(avg_mod_cont, na.rm = TRUE),
    min_disc = min(avg_mod_disc, na.rm = TRUE),
    max_disc = max(avg_mod_disc, na.rm = TRUE)
  ), by = epoch]
  
  dt <- merge(ranges, dt, by = "epoch", sort = FALSE)
  setorder(dt, epoch, k)
  
  dt[, `:=`(
    range_cont = max_cont - min_cont,
    range_disc = max_disc - min_disc
  )]
  
  dt[range_cont == 0, range_cont := NA_real_]
  dt[range_disc == 0, range_disc := NA_real_]
  
  # --- 2. Normalize means and standard deviations ---
  
  dt[, `:=`(
    mod_cont = (avg_mod_cont - min_cont) / range_cont,
    mod_disc = (avg_mod_disc - min_disc) / range_disc,
    
    sd_cont = sd_mod_cont / range_cont,
    sd_disc = sd_mod_disc / range_disc
  )]
  
  # --- 3. Determine k* using the original sign-change mechanism ---
  
  k_star_dt <- dt[, {
    
    norm_cont <- mod_cont
    norm_disc <- mod_disc
    
    delta <- norm_cont - norm_disc
    
    sign_change <- which(
      delta[-1] * delta[-length(delta)] < 0
    )
    
    k_star <- if (length(sign_change) == 0) {
      NA_integer_
    } else {
      as.integer(k[sign_change[1] + 1])
    }
    
    .(k_star = k_star)
    
  }, by = epoch]
  
  # --- 4. Reshape into long format ---
  
  plot_long <- rbindlist(list(
    
    dt[, .(
      k, epoch,
      type = "Continuous",
      modularity = mod_cont,
      sd = sd_cont
    )],
    
    dt[, .(
      k, epoch,
      type = "Discrete",
      modularity = mod_disc,
      sd = sd_disc
    )]
    
  ))
  
  # --- 5. Calculate mean +/- 1 SD ---
  
  plot_long[, `:=`(
    lower = pmax(0, modularity - sd),
    upper = pmin(1, modularity + sd)
  )]
  
  plot_long[, type := factor(
    type,
    levels = c("Continuous", "Discrete")
  )]
  
  # --- 6. Print selected k* values ---
  
  cat("\nSelected k* values:\n")
  print(k_star_dt)
  
  # --- 7. Define colors ---
  
  colors <- c(
    "Continuous" = "#556B2F",
    "Discrete" = "#C6A15B"
  )
  
  # --- 8. Prepare annotation positions ---
  
  annotations <- k_star_dt[!is.na(k_star)]
  
  annotations[, `:=`(
    label = paste0("k* = ", k_star),
    y = 0.025
  )]
  
  # --- 9. Plot ---
  
  p <- ggplot(
    plot_long,
    aes(
      x = k,
      y = modularity,
      color = type,
      fill = type
    )
  ) +
    
    geom_ribbon(
      aes(ymin = lower, ymax = upper),
      alpha = 0.2,
      color = NA
    ) +
    
    geom_line(linewidth = 1.2) +
    
    # Vertical lines indicating k*
    geom_vline(
      data = annotations,
      aes(xintercept = k_star),
      inherit.aes = FALSE,
      color = "black",
      linetype = "dashed",
      linewidth = 0.9
    ) +
    
    # k* annotations
    geom_text(
      data = annotations,
      aes(
        x = k_star,
        y = y,
        label = label
      ),
      inherit.aes = FALSE,
      hjust = -0.15,
      vjust = 0.5,
      color = "black",
      size = 6
    ) +
    
    facet_wrap(
      ~ epoch,
      ncol = facet_ncol,
      scales = "free_x"
    ) +
    
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    
    scale_x_continuous(
      breaks = scales::breaks_width(5)
    ) +
    
    scale_y_continuous(
      limits = c(0, 1)
    ) +
    
    labs(
      x = "Number of communities (k)",
      y = "Min-max normalized modularity"
    ) +
    
    theme_minimal(base_size = 24) +
    
    theme(
      legend.position = "none",
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(
        face = "bold")
    )
  
  # Store selected values as an attribute
  attr(p, "k_star") <- k_star_dt
  
  save_interactive(p, path, caption, log_file)
}
