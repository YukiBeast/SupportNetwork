dyadic_relation <- function(san, state1_f, state2_f, path,
                            caption = "no caption provided",
                            log_file = "plot_tracker.Rmd"
                            ) {
  
  library(pammtools, quietly = TRUE)
  
  # 1. Base data preparation
  dt_plot <- san[(state1 == state1_f & state2 == state2_f) |
                   (state1 == state2_f & state2 == state1_f), 
                 .(year, same_side, opposite_side, balance, type)]
  
  # Lückenlose Jahre generieren (Der Merge passiert VOR der Berechnung!)
  merge_dt <- data.table(year = 1975:2017)
  dt_plot <- dt_plot[merge_dt, on = "year"]
  
  # NAs mit 0 auffüllen für die Linien
  setnafill(dt_plot, fill = 0, cols = c("same_side", "opposite_side", "balance"))
  # NAs in type als "null" deklarieren
  dt_plot[is.na(type), type := "neg"]
  
  # Sort by year and create groups
  dt_plot <- dt_plot[order(year)]
  dt_plot[, type_group := rleid(type)]
  
  # 2. Identify where transitions happen
  dt_plot[, next_group := shift(type_group, type = "lead")]
  transition_idx <- which(dt_plot$type_group != dt_plot$next_group)
  
  if (length(transition_idx) > 0) {
    
    # 3. Für Step-Plots kopieren wir einfach die Zeile des NÄCHSTEN Jahres...
    next_rows <- dt_plot[transition_idx + 1]
    
    extended_ends <- copy(next_rows)
    # ... und weisen sie der ALTEN Gruppe zu. 
    # So zieht sich die Treppen-Fläche exakt bis zur Kante des neuen Jahres.
    extended_ends[, `:=`(
      type_group = dt_plot$type_group[transition_idx],
      type = dt_plot$type[transition_idx],
      next_group = NULL
    )]
    
    # 4. Bind the extended points to the data and re-sort
    dt_plot[, next_group := NULL]
    dt_expanded <- rbind(dt_plot, extended_ends)
    dt_expanded <- dt_expanded[order(year, type_group)]
    
  } else {
    dt_plot[, next_group := NULL]
    dt_expanded <- dt_plot
  }
  
  # 5. Plot
  p <- dt_expanded |>
    ggplot(aes(x = year)) +
    geom_stepribbon(aes(ymin = -opposite_side, 
                        ymax = same_side, 
                        fill = type, 
                        group = type_group), 
                    alpha = 0.3,
                    direction = "hv") +
    geom_step(aes(y = same_side, linetype = "Co - interventions"), direction = "hv") +
    geom_point(aes(y = balance)) +
    geom_step(aes(y = -opposite_side, linetype = "Opposite interventions"), direction = "hv") +
    geom_step(aes(y = balance, linetype = "Actor Relation"), linewidth = 1.5, direction = "hv") +
    scale_linetype_manual(values = c("Actor Relation" = "solid",
                                     "Co - interventions" = "twodash",
                                     "Opposite interventions" = "twodash"),
                          name = "Line Type") +
    scale_fill_manual(values = c("ambi" = "#888888",
                                 "neg" = "#661100",
                                 "pos" = "#117733"
                                 ),
                      labels = c("ambi" = "Ambivalent",
                                "neg" = "Strictly Negative",
                                "pos" = "Strictly Positive"),
                      name = "Relation Type"
                      ) + 
    line1 + line2 +
    scale_y_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 2), limits = c(-12, 12)) +
    labs(x = "Year", y = "Count") +
    theme_minimal(base_size = 24) +
    theme(legend.position = "none")
  
  save_interactive(p, path, caption, log_file)
}
