landscape <- function(san, anchor, min_cointerv = NULL, top = NULL, path,
                      base_size = 24, font_size = 16) {
  library(data.table, quietly = TRUE)
  library(ggplot2, quietly = TRUE)
  
  line1$aes_params$colour <- "black"
  line2$aes_params$colour <- "black"
  
  san <- as.data.table(san)
  
  dt <- san[state1 == anchor | state2 == anchor, ]
  dt <- dt[, .(year,  # for x-achse
               actor = ifelse(state1 == anchor, state2, state1),  # y
               cointerv = same_side + opposite_side,  # to only keep the ones with more than min_cointerv (or top ones)
               balance)] # color
  
  dt[, balance := ifelse(cointerv == 0, NA, balance)]
  
  if (!is.null(min_cointerv)) {
    actors_to_keep <- dt[, .(total_cointerv = sum(cointerv, na.rm = TRUE)),
                         by = "actor"][total_cointerv >= min_cointerv, actor]
    
    dt <- dt[actor %in% actors_to_keep, ]
  }
  
  if (!is.null(top)) {
    actors_to_keep <- dt[, .(total_cointerv = -sum(cointerv, na.rm = TRUE)),
                         by = "actor"]
    
    setkeyv(actors_to_keep, "total_cointerv")
    actors_to_keep <- head(actors_to_keep, n = top)[, actor]
    
    dt <- dt[actor %in% actors_to_keep, ]
    
  }
  
  # 1. Calculate the overall net balance for each remaining actor
  actor_balances <- dt[, .(net_balance = sum(balance, na.rm = TRUE)), by = actor]
  
  # 2. Sort the actors. 
  # We sort descending (-net_balance) so the most positive (blue) is first.
  # In ggplot, the first factor level is plotted at the bottom of the y-axis.
  setorder(actor_balances, -net_balance)
  
  # 3. Convert the 'actor' column in your main dataset to a factor using this specific order
  dt[, actor := factor(actor, levels = actor_balances$actor)]
  
  p <- ggplot(dt, aes(x = year, y = actor, fill = balance)) +
    geom_tile() +
    scale_fill_gradient2(
      low = "firebrick",     # Color for negative values
      mid = "grey90",         # Color for exactly 0
      high = "steelblue",    # Color for positive values
      midpoint = 0,          # Center the divergent scale at 0
      limits = c(-8, 8),            # Squeezes the gradient to saturate faster
      oob = scales::squish,         # Tells ggplot to give values outside the limits the maximum color, rather than dropping them
      na.value = "white",   # white for NA (years with 0 interventions)
      name = "Balance"
    ) + line1 + line2 +
    labs(x = "Year",
         y = "",
         title = anchor) +
      theme_bw(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      legend.position = "none"
      # axis.text.y = element_text(size = 12),
      # axis.text.x = element_text(size = 12)
      )
  
  save_interactive(p, path)
  
}

landscape(san, "United States of America", top = 15,
            path = "Results/redo/SAN/landscape_usa.pdf",
          base_size = 20)
landscape(san, "United States of America", min_cointerv = 5,
                      path = "Results/redo/SAN/landscape_usa_min5.pdf",
          base_size = 12)
landscape(san, "Russia (Soviet Union)", top = 15,
            path = "Results/redo/SAN/landscape_russia.pdf",
          base_size = 20)
landscape(san, "Russia (Soviet Union)", min_cointerv = 5,
           path = "Results/redo/SAN/landscape_russia_min5.pdf",
          base_size = 12)
