# finalize results once the smon_consensus has run and comm_epochs store relevant information

# store network plots for each epoch
epochs <- list(1975:1991, 1992:2001, 2002:2008, 2009:2017)

for (i in seq_along(epochs)) {
  
  epoch_label <- paste0(min(epochs[[i]]), "-", max(epochs[[i]]))
  
  san_filtered <- san[year %in% epochs[[i]], .(state1, state2, balance)]
  
  communities_filtered <- comm_epochs[epoch == epoch_label]
  
  plot_smon_ggraph(san_filtered, communities_filtered, mind_comm_size = 2,
                   # top_n = 8,
                   show_legend = FALSE,
                   priority_nodes = c("United States of America", "Russia (Soviet Union)"),
                   path = paste0("Results/SMON/smon_community_net",
                                 epoch_label,
                                 ".pdf"))
}

# plot (normalized) discrete (gold) and continuous (green) modularity and their crossing point, chosen as the first k for which the sign of C - D changes.
plot_normalized_modularity(avg_epochs,
                           path = "Results/SMON/Q_comparison.pdf",
                           caption = "plot (normalized) discrete (gold) and continuous (green) modularity and their crossing point, chosen as the first k for which the sign of C - D changes.
")



