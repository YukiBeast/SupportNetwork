library(ggplot2)
library(data.table)

plot_mod_smon_faceted <- function(mod_data,   # combined dataset with epoch column
                                  title = NULL,
                                  path = NULL,
                                  base_size = 18) {
  
  # 1. Per-epoch min-max normalization of both modularities to [0, 1]
  df <- as.data.table(mod_data)
  
  df[, `:=`(
    disc_min = min(mod_discrete, na.rm = TRUE),
    disc_max = max(mod_discrete, na.rm = TRUE),
    cont_min = min(mod_continuous, na.rm = TRUE),
    cont_max = max(mod_continuous, na.rm = TRUE),
    max_disc = k[which.max(mod_discrete)],
    max_cont = k[which.max(mod_continuous)]
  ), by = epoch]
  
  # safeguard against flat values
  df[, disc_rng := ifelse(disc_max - disc_min == 0, 1, disc_max - disc_min)]
  df[, cont_rng := ifelse(cont_max - cont_min == 0, 1, cont_max - cont_min)]
  
  df[, `:=`(
    disc_norm = (mod_discrete - disc_min) / disc_rng,
    cont_norm = (mod_continuous - cont_min) / cont_rng
  )]
  
  # 2. Per-epoch crossing point (floor of k where the normalized curves cross)
  #    computed via linear interpolation of the difference curve
  cross_pts <- df[, {
    # unique k, sorted
    ks <- sort(unique(k))
    # interpolate both normalized curves onto a fine grid
    fine <- seq(min(ks), max(ks), by = 0.01)
    cont_fine <- approx(x = ks, y = cont_norm[match(ks, k)], xout = fine)$y
    disc_fine <- approx(x = ks, y = disc_norm[match(ks, k)], xout = fine)$y
    diff_fine <- cont_fine - disc_fine
    
    # find where the sign changes (first crossing)
    idx <- which(diff_fine[-1] * diff_fine[-length(diff_fine)] < 0)
    if (length(idx) > 0) {
      k_star <- floor(fine[idx[1]])  # floor of the crossing k
    } else {
      k_star <- NA_integer_  # no crossing within range
    }
    .(k_star = k_star)
  }, by = epoch]
  
  # merge crossing points back onto the plotting data
  df <- merge(df, cross_pts, by = "epoch")
  
  # 3. Build the plot
  # 3. Build the plot
  p <- ggplot(df, aes(x = k)) +
    geom_line(aes(y = disc_norm, col = "Discrete"), linewidth = 1.2) +
    geom_line(aes(y = cont_norm, col = "Continuous"), linewidth = 1.2) +
    
    # reference markers for per-epoch maxima
    geom_vline(aes(xintercept = max_disc), col = "#888888", linetype = "dashed", linewidth = 0.8) +
    geom_vline(aes(xintercept = max_cont), col = "black", linetype = "dashed", linewidth = 0.8) +
    
    # maxima labels
    geom_text(aes(x = max_disc, y = 0.00,
                  label = paste0("max(disc) = ", max_disc)),
              col = "#888888", size = base_size / 3, fontface = "bold", hjust = 0) +
    geom_text(aes(x = max_cont, y = 0.00,
                  label = paste0("max(cont) = ", max_cont)),
              col = "black", size = base_size / 3, fontface = "bold", hjust = 0.5) +
    
    # crossing point marker in gold
    geom_vline(aes(xintercept = k_star), col = "darkgoldenrod", linetype = "dashed", linewidth = 1.1) +
    geom_text(aes(x = k_star + 0.35, y = 0.3,
                  label = paste0("k* = ", k_star)),
              col = "darkgoldenrod", size = base_size / 3, fontface = "bold", hjust = 0) +
    
    # faceting: label each panel with the time range, boxed
    facet_wrap(~ years, scales = "free_x") +
    
    scale_y_continuous(name = "Normalized Modularity", limits = c(0, 1)) +
    scale_color_manual(
      name = "Modularity Type",
      values = c("Discrete" = "#888888", "Continuous" = "black")
    ) +
    labs(title = title) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
    theme_minimal(base_size = base_size) +
    theme(
      legend.position = "bottom",
      strip.background = element_rect(fill = "grey90", colour = "black", linewidth = 0.6),
      strip.text = element_text(face = "bold", size = base_size * 0.8)
    )
  
  save_interactive(p, path)
  
}

