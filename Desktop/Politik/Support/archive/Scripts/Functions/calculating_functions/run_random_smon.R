library(data.table)

run_random_smon <- function(san_filtered, k, runs = 20) {
  
  list_z <- list()
  list_mod_history <- list()
  for (i in seq_len(runs)) {
    
    smon_i <- run_smon(san_filtered, k, max_iter = 500, seed = FALSE)
    

    k_results <- smon_i$by_k[[as.character(k)]]
    z_norm <- k_results$z_norm
    list_z[[i]] <- z_norm
    
    mod_history <- data.frame(run = i,
                              iteration = 1:500,
                              mod_cont = k_results$mod_history)
    
    list_mod_history[[length(list_mod_history) + 1]] <- mod_history
  }
  
  mod_history <- rbindlist(list_mod_history)
  
  return(list(all_z = list_z,
              mod_history = mod_history))
}
random_smon <- run_random_smon(san_7591, 8)
