#' Extract Core Community Members Across Resolution Parameters
#'
#' @description 
#' Identifies the "core" and "peripheral" members of an actor's community. For a given set of anchor actors, 
#' this function inspects their community assignment across multiple datasets (e.g., resulting 
#' from different Leiden resolution parameters) and calculates the ratio of how often each actor 
#' appears in the same community as the anchor.
#'
#' @param folder A character string specifying the path to a directory containing the 
#'   community datasets (saved as .RDS files). 
#' @param actors A character vector containing the names of the anchor actors whose 
#'   communities should be inspected. Defaults to USA and Russia.
#'
#' @details 
#' All `.RDS` datasets contained in the `folder` must include at least the following columns:
#' \itemize{
#'   \item \code{window} (character): The time window of the network.
#'   \item \code{community_num} (integer/numeric): The community ID assigned to the node.
#'   \item \code{actor} (character): The name of the node/state.
#' }
#'
#' @return A \code{data.table} with four columns:
#' \itemize{
#'   \item \code{window}: The time window inspected.
#'   \item \code{actor}: The anchor actor being analyzed.
#'   \item \code{core}: The name of a co-member found in the anchor's community.
#'   \item \code{ratio}: A numeric value between 0 and 1 indicating the proportion of datasets 
#'         in which this co-member shared a community with the anchor.
#' }
#' 
#' @import data.table
#' @export
extract_core <- function(folder,
                         actors = c("United States of America", "Russia (Soviet Union)")) {
  
  library(data.table, quietly = TRUE)
  
  # 1. Read files directly into a list of data.tables
  files <- list.files(folder, full.names = TRUE)
  df_list <- lapply(files, function(fl) setDT(readRDS(fl)))
  
  windows <- unique(df_list[[1]]$window)
  core_list <- list() 
  list_idx <- 1
  
  for (act in actors) {
    for (wind in windows) {
      members_list <- list()
      
      for (i in seq_along(df_list)) {
        dt <- df_list[[i]]
        
        # Find the community of the anchor actor in this specific dataset
        num_comm <- dt[window == wind & actor == act]$community_num
        
        # Safety check: if the anchor didn't intervene in this window, skip to avoid breaking
        if (length(num_comm) == 0) next
        
        # Extract all members of that community and store them
        members <- dt[window == wind & community_num == num_comm]$actor
        members_list[[i]] <- members
      }
      
      # Remove any empty elements (in case the actor was missing in some datasets)
      members_list <- Filter(length, members_list)
      
      # 2. Calculate the appearance ratio for all co-members
      if (length(members_list) > 0) {
        total_runs <- length(members_list)
        all_members <- unlist(members_list)
        
        # Count how many times each actor appeared in the anchor's community
        member_counts <- table(all_members)
        
        # Create a data.table with the calculated ratio
        core_list[[list_idx]] <- data.table(
          window = wind,
          actor = act,
          core = names(member_counts),
          ratio = as.numeric(member_counts) / total_runs
        )
        
        list_idx <- list_idx + 1 # Increment safely
      }
    }
  }
  
  # 3. Bind the clean list of data.tables together
  core_df <- rbindlist(core_list)
  
  # Optional: Sort descending by window, anchor, and then ratio to make it readable
  setorder(core_df, window, actor, -ratio)
  
  return(core_df)
}


core_df <- extract_core("Data/community_data")
