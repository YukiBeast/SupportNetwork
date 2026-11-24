library(data.table)
library(igraph)
library(ggplot2)
source("Scripts/SMON/run_smon.R")
source("Scripts/SMON/plot_mod_smon.R")
source("Scripts/SMON/plot_smon_network.R")
source("utils/assign_communities.R")
source("utils/update_elementwise.R")

source("utils/get_iso3.R")

# For year 1985 (for paper):
# - plot network for 1985 and save it as pdf. Update plot_tracker: only legend indicating what colors and node size represent are needed)
# - plot smon communities net. for 1985 and save it as pdf. Update plot_tracker: legend
# - extract dataset containing all communities and probs for this year and save it as smon_communities_1985.Rdata


# Step 1: Filter the edge list for the desired period
san_1985 <- san[year == 1985, .(state1, state2, balance)]

# Step 2: Run SMON across candidate community counts (e.g., k = 2 to 20)
smon_1985 <- run_smon(san_1985, num_comm = 2:30, max_iter = 500)

# Step 3: Automatically choose optimal k using the elbow heuristic
choice_1985 <- choose_k(smon_1985, method = "elbow")
choice_1985_frob <- choose_k(smon_1985, method = "frobenius")
cat("Optimal k chosen:", choice_1985$k, "with discrete modularity:", choice_1985$mod_discrete, "\n")

cat("Optimal k chosen:", choice_1985_frob$k, "with frobenius criterion:", choice_1985_frob$mod_discrete, "\n")

as.data.frame(round(choice_1985_frob$z_norm, 3)) |> View()

comm_1985 <- assign_communities(choice_1985_frob$z_norm)
saveRDS(comm_1985, "Results/SMON/smon_communities_1985.Rds")

# Step 4: Render the standard SAN plot and capture its layout coordinates
par(mfrow = c(1, 2))
layout_1985 <- plot_san_net(san, 1985, legend.position = "none", path = "Results/SMON/san_1985.pdf",
                            caption = "darkred: negative tie,
                            darkgreen: positive tie,
                            grey: balanced tie,
                            
                            node_size: actor's degree,
                            
                            edge thickness: net tie weight (balance)",
                            )

# Step 5: Render the matching SMON community plot using the exact same coordinates
plot_san_net_smon(
  san            = san,
  year.f         = 1985,
  smon_choice   = choice_1985_frob,
  layout         = layout_1985,
  mind_comm_size = 2,
  top_n          = 8,
  priority_nodes = c("United States of America", "Russia (Soviet Union)"),
  legend.position = "none",
  path           = "Results/SMON/smon_net_1985.pdf",
  caption = "added some more colors"
)

plot_mod_smon(smon_1985, chosen_k = 11)

# previous caption:
# only plotted communities with more than 1 member (isolates removed, here there were none),
# 
# node's color: community with highest prob.
#   node's saturation: detected prob. for that node to belong to that community,
# node's size: node's degree





# Define the periods/epochs
epochs <- list(
  "Cold War"           = 1975:1991,
  "Post-Soviet"        = 1992:2001,
  "War on Terror"      = 2002:2008,
  "Multipolarity"      = 2009:2017
)

# Initialize an empty list to store the community data frames
all_communities_list <- list()
all_modularity_list <- list()
combined_mod_data <- list()
par(mfrow = c(2, 2))

for (epoch_name in names(epochs)) {
  epoch_years <- epochs[[epoch_name]]
  cat("\n======================================================\n")
  cat("Running analysis for epoch:", epoch_name, "(", min(epoch_years), "-", max(epoch_years), ")\n")
  
  # Step 1: Filter the edge list for the current epoch
  san_epoch <- san[year %in% epoch_years, .(state1, state2, balance)]
  
  # Step 2: Run SMON across candidate community counts
  smon_epoch <- run_smon(san_epoch, num_comm = 2:50, max_iter = 500)
  
  # Step 3: Automatically choose optimal k using the "crossing point" method
  choice_epoch <- choose_k(smon_epoch, method = "cross")
  best_k <- choice_epoch$k
  
  cat("Optimal k chosen (cross):", best_k, 
      "| Discrete Modularity:", round(choice_epoch$mod_discrete, 3), "\n")
  
  # Step 4: Extract community assignments and append epoch metadata
  comm_df <- assign_communities(choice_epoch$z_norm)
  comm_df$epoch <- epoch_name
  comm_df$years <- paste(min(epoch_years), max(epoch_years), sep = "-")
  
  # Store in our master list
  all_communities_list[[epoch_name]] <- comm_df
  
  # Step 5: Save the full modularity data for this epoch
  mod_epoch <- smon_epoch$results_summary
  mod_epoch$epoch <- epoch_name
  mod_epoch$years <- paste(min(epoch_years), max(epoch_years), sep = "-")
  mod_epoch$best_k <- best_k
  
  all_modularity_list[[epoch_name]] <- mod_epoch
  combined_mod_data[[epoch_name]] <- mod_epoch
  
  # Step 6: Render the Modularity Landscape plot
  # This shows the Modularity curves and highlights our chosen cross k
  # plot_mod_smon(
  #   smon_results = smon_epoch,
  #   chosen_k     = best_k,
  #   title = paste0(epoch_name, " (", min(epoch_years), "-", max(epoch_years), ")")
  # )
  
  # Step 7: Render the SMON Community Network Plot
  # Using plot_smon_network (which computes its own layout internally)
  # plot_smon_network(
  #   san            = san, 
  #   year.f         = epoch_years, 
  #   smon_choice    = choice_epoch, 
  #   mind_comm_size = 4, 
  #   top_n          = 8, 
  #   priority_nodes = c("United States of America", "Russia (Soviet Union)"),
  #   # Add a path if you want it automatically saved, e.g., 
  #   path = paste0("Results/SMON/smon_net_", min(epoch_years), "-", max(epoch_years), ".pdf"),
  #   caption = paste0("update smon network for epoch: ", min(epoch_years), "-", max(epoch_years),
  #                    "min community members: 4")
  # )
}
all_modularity <- rbindlist(combined_mod_data, use.names = TRUE, fill = TRUE)

plot_mod_smon_faceted(all_modularity,
                      path = "Results/SMON/Q_comparison.pdf")


# Combine all epochs into one master dataset

final_communities_df <- rbindlist(all_communities_list)

saveRDS(final_communities_df, "Results/SMON/smon_communities.Rds")

cat("\nAnalysis complete! Combined dataset 'final_communities_df' is ready.\n")






