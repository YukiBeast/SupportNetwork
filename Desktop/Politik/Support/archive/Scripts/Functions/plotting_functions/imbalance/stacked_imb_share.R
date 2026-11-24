stacked_imb_share <- function(imb, path){
  library(data.table, quietly = TRUE)
  library(ggplot2, quietly = TRUE)
  
  line1$aes_params$colour = "black"
  line2$aes_params$colour = "black"
  
  line1$aes_params$linewidth = 1
  line2$aes_params$linewidth = 1
  
  dt <- copy(imb)
  
  interesting <- c("United States of America", "Iraq",
                   "Russia (Soviet Union)",
                   "China", "Saudi Arabia", "United Kingdom")
  
  dt[, state_cat := ifelse(state %in% interesting,
                         state,
                         "Others")][, imb_per_state_cat := sum(imbalanced_per_state),
                                    by = c("year", "state_cat")]
  
  dt_cat <- unique(dt[, .(year, state_cat, imb_per_state_cat, count_imb)])
  
  dt_cat[, .(state_cat,
             sum(imb_per_state_cat, na.rm = TRUE) / (count_imb * 3)), by = "year"][, all(V2 == 1)]
  
  
  dt_cat <- dt_cat[, .(state_cat,
                       imb_per_state_share = ifelse(count_imb != 0,
                                                    imb_per_state_cat / (3 * count_imb),
                                                    0),
                       count_imb),
                   by = "year"]
  
  # --- DER FIX: Fehlende Kombinationen auffüllen ---
  # a) Erstelle ein Grid mit ALLEN Jahren und ALLEN Kategorien
  full_grid <- CJ(year = unique(dt_cat$year), state_cat = unique(dt_cat$state_cat))
  
  # b) Merge das Grid mit den echten Daten (fehlende Jahre/Länder werden NA)
  dt_cat <- dt_cat[full_grid, on = .(year, state_cat)]
  
  # c) Setze alle dadurch entstandenen NAs (die Lücken) auf 0
  dt_cat[is.na(imb_per_state_share), imb_per_state_share := 0]

  p <- ggplot(dt_cat, aes(x = year, fill = state_cat)) +
    geom_area(aes(y = imb_per_state_share), col = "black") +
    line1 + line2 +
    labs(y = "Imbalance per State (%)",
         x = "Year",
         fill = "State") +
    theme_minimal(base_size = 24)
  
  save_interactive(p, path)
}
