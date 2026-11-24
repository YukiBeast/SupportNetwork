create_imbalance_dt <- function(disaggregated_san) {
  library(data.table)
  cat("Starting step 1...\n")
  
  # A. Get a unique list of EVERY state that appears in a given year
  all_actors <- unique(rbind(
    disaggregated_san[, .(year, state = state1)],
    disaggregated_san[, .(year, state = state2)]
  ))
  
  # B. Calculate per state (using the safe list of actors)
  imbalance_dt <- all_actors[, .(
    imbalanced_per_state = calc_imbalance_actor(disaggregated_san, 
                                                actor = .BY$state, 
                                                year.f = .BY$year, ratio = 1),
    balanced_per_state = calc_imbalance_actor(disaggregated_san, 
                                              actor = .BY$state, 
                                              year.f = .BY$year, ratio = 2)
  ), by = c("state", "year")]
  
  cat("Step 1 done. \nStarting step 2: Calculating global totals...\n")
  Sys.sleep(1)
  # 3. The Global Math Trick
  # Since every triangle has 3 nodes, the global count is the sum of local counts / 3
  yearly_imb <- imbalance_dt[, .(
    count_imb = sum(imbalanced_per_state) / 3,
    count_bal = sum(balanced_per_state) / 3
  ), by = "year"]
  
  # Get the total overall configurations
  yearly_imb[, count_all := count_imb + count_bal]
  
  cat("Step 2 done. Proceeding with merging now.\n")
  
  # 4. Merge the datasets
  joined_imb <- imbalance_dt[yearly_imb, on = "year"]
  
  Sys.sleep(1)
  cat("Finally! The complete imb dataset has been invisibly returned.\n")
  return(invisible(joined_imb))
}
