# ------------------------------------------------------------------
# Average continuous and discrete modularity over independent SMON runs,
# for every k in k_range.
#
# cl: optional parallel cluster (parallel::makeCluster). Without it,
#     the runs are computed one after another.
# ------------------------------------------------------------------
avg_mod_smon <- function(san_filtered, runs, k_range = 2:50, cl = NULL) {

  one_run <- function(r, san, ks) {
    smon <- run_smon(san, num_comm = ks, seed = FALSE)
    smon$results_summary[, .(k, mod_continuous, mod_discrete)]
  }
  environment(one_run) <- globalenv()     # do not ship this function's frame to the workers

  t0 <- Sys.time()
  mod_list <- if (is.null(cl)) {
    lapply(seq_len(runs), one_run, san = san_filtered, ks = k_range)
  } else {
    parallel::parLapply(cl, seq_len(runs), one_run, san = san_filtered, ks = k_range)
  }
  total_time <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  mod_all <- rbindlist(mod_list)
  avg <- mod_all[, .(
    avg_mod_cont = mean(mod_continuous),
    avg_mod_disc = mean(mod_discrete),
    sd_mod_cont  = sd(mod_continuous),
    sd_mod_disc  = sd(mod_discrete)
  ), by = k]

  list(
    avg        = avg[order(k)],
    total_time = total_time
  )
}
