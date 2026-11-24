generate_epochs_communities <- function(
    grid = seq(0.25, 1.25, by = 0.05),
    isn_nocoalitions,
    epochs = list(
      c(1975, 1991), # Cold War
      c(1992, 2001), # Post-Cold War / Unipolar Moment
      c(2002, 2017)  # Post-9/11 / War on Terror
    )) {
  
  # First delete ALL files in the target folder
  folder_path <- "Data/community_data"
  
  files_to_delete <- list.files(folder_path, full.names = TRUE) 
  
  file.remove(files_to_delete)
  
  for (res in seq(0.25, 1.25, by = 0.05)) {
    results_epochs <- calculate_communities_signed_mod(isn_nocoalitions, 
                                                       custom_periods = epochs, 
                                                       resolution = res,
                                                       method = "CPM")
    
    epoch_metrics <- results_epochs$metrics
    epoch_graphs <- results_epochs$graphs
    
    list_dt <- list()
    for (i in 1:3) {
      list_dt[[i]] <- data.table(window = paste0(epochs[[i]][[1]], "-", epochs[[i]][[2]]),
                                 community_num = V(epoch_graphs[[i]])$community,
                                 actor = V(epoch_graphs[[i]])$name)
      
    }
    
    community_dt <- rbindlist(list_dt)
    saveRDS(community_dt, paste0("Data/community_data/epochs_communities_", res, ".rds"))
  }
  
}
