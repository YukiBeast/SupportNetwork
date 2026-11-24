# ------------------------------------------------------------------
# 1. average continuous and discrete modularity over runs, per k
# ------------------------------------------------------------------
avg_mod_smon <- function(san_filtered, runs, k_range = 2:50) {
  mod_list <- vector("list", runs)
  times    <- numeric(runs)                      # seconds per run
  
  for (r in seq_len(runs)) {
    t0 <- Sys.time()
    smon <- run_smon(san_filtered, num_comm = k_range, seed = FALSE)
    times[r] <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    mod_list[[r]] <- smon$results_summary[, .(k, mod_continuous, mod_discrete)]
  }
  
  mod_all <- rbindlist(mod_list)
  avg <- mod_all[, .(
    avg_mod_cont = mean(mod_continuous),
    avg_mod_disc = mean(mod_discrete),
    sd_mod_cont  = sd(mod_continuous),
    sd_mod_disc  = sd(mod_discrete)
  ), by = k]
  
  list(
    avg          = avg[order(k)],
    time_per_run = times,
    total_time   = sum(times)
  )
}