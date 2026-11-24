plot_nonstate_interventions <- function(isn, share = FALSE, path,
                                        caption = "no caption provided",
                                        log_file = "plot_tracker.Rmd") {
  library(data.table, quietly = T)
  library(ggplot2, quietly = T)
  dt <- copy(isn)
  
    dt_plot <- dt[, .(state_count = sum(ext_nonstate == 0, na.rm = TRUE),
                                      non_state_count = sum(ext_nonstate == 1, na.rm = TRUE),
                      total_count = sum(!is.na(ext_nonstate))),
                                  by = "year"]

    if(share) dt_plot <- dt_plot[, .(year,
                              state_count = state_count / total_count,
                              non_state_count = non_state_count / total_count)]
  p <-  dt_plot |>
    ggplot(aes(x = year)) +
    
    # Couches pour les "State Supporters"
    geom_point(aes(y = state_count, col = "state")) +
    geom_line(aes(y = state_count, col = "state")) +
    geom_smooth(aes(y = state_count, col = "state"), se = FALSE) +
    
    # Couches pour les "Non-State Supporters"
    geom_point(aes(y = non_state_count, col = "non_state")) +
    geom_line(aes(y = non_state_count, col = "non_state")) +
    geom_smooth(aes(y = non_state_count, col = "non_state"), se = FALSE) +
    line1 + line2 +
    
    # Personnalisation des couleurs et de la légende
    scale_color_manual(
      values = c("state" = "darkgrey",         # Vert (tu peux changer la couleur)
                 "non_state" = "purple3"),    # Rouge (tu peux changer la couleur)
      labels = c("state" = "State Supporters",
                 "non_state" = "Non-State Supporters"),
      name = "Supporter Type"
    ) + 
    # line1 + line2 +  <-- Décommente ceci si tu as toujours ces variables globales
    labs(x = "Year", y = "Total Count") +
    theme_minimal(base_size = 24) +
    theme(legend.position = "none")
  
  save_interactive(p, path, caption, log_file)
}

# plot_nonstate_interventions(nocoalitions, path = "results/nonstate_vs_state.pdf",
#                             caption = "count how many state (darkgrey)
#                             and not-state (purple) intervened in a given year")
# # 
# plot_nonstate_interventions(isn_nocoalitions,
#                              share = TRUE,
#                              path = "results/nonstate_vs_state_share.pdf",
#                             caption = "same but share among all support provides")
