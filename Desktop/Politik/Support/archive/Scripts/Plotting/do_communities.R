# create_communities

# Execution:

### First draft: sliding 5 - years-windows (results using CPM modularity)

results <- calculate_communities_signed_mod(isn_nocoalitions, 5, resolution = 0.75)

macro_table <- results$metrics

graph_list <- results$graphs



for(res in seq(0, 1, length.out = 9)) {
  
  results <- calculate_communities_signed_mod(isn_nocoalitions, 5, resolution = res)
  
  plot_num_comm_vs_mod(results$metrics, "Results/#comm_vs_mod.pdf")
  
}

### First draft: sliding 5 - years-windows (results using classical modularity)

wrong_results <- calculate_communities_signed_mod(isn_nocoalitions, 5, method = "modularity")

plot_num_comm_vs_mod(wrong_results$metrics, coeff = 2,
                     
                     path = "Results/Pics/communities/comm_vs_mod_classic.pdf")

plot_mod(wrong_results$metrics, 1, "Results/Pics/communities/classic_mod.pdf")

plot_weight_trend_window(isn_nocoalitions, "Results/Pics/communities/weight_trend_window.pdf")





# Define historical epochs
epochs <- list(
  c(1975, 1991), # Cold War
  c(1992, 2001), # Post-Cold War / Unipolar Moment
  c(2002, 2017)  # Post-9/11 / War on Terror
)

# Run the generalized function
results_epochs <- calculate_communities_signed_mod(isn_nocoalitions, 
                                                   custom_periods = epochs, 
                                                   resolution = 0.75,
                                                   method = "CPM")

epoch_metrics <- results_epochs$metrics
epoch_metrics

epoch_graphs <- results_epochs$graphs
plot_community_window(epoch_graphs, "1975-1991", "Results/Pics/communities/net_7591.pdf")
plot_community_window(epoch_graphs, "1992-2001", "Results/Pics/communities/net9201_.pdf")
plot_community_window(epoch_graphs, "2002-2017", "Results/Pics/communities/net_0217.pdf")

plot_community_window(epoch_graphs, "1975-1991", "Results/Pics/communities/nolab_net_7591.pdf",
                      show_labels = FALSE)
plot_community_window(epoch_graphs, "1992-2001", "Results/Pics/communities/nolab_net9201_.pdf",
                      show_labels = FALSE)
plot_community_window(epoch_graphs, "2002-2017", "Results/Pics/communities/nolab_net_0217.pdf",
                      show_labels = FALSE)

length(V(epoch_graphs$`1975-1991`))
length(V(epoch_graphs$`1992-2001`))
length(V(epoch_graphs$`2002-2017`))


results_classic <- calculate_communities_signed_mod(isn_nocoalitions,
                                                    custom_periods = epochs,
                                                    method = "modularity")
classic_metrics <- results_classic$metrics
classic_graphs <- results_classic$graphs
plot_community_window(classic_graphs, "1975-1991", "Results/Pics/communities/net_7591.pdf")
length(V(classic_graphs$`1975-1991`))

# ===========
# create dataset with all communities
# ===========
generate_epochs_communities(isn_nocoalitions = isn_nocoalitions)
