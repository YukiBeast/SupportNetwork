# ==================================================================
# 02_smon.R
# SMON community detection on the four period SANs:
#   step 1: select k* per period from ensemble-averaged modularity curves
#   step 2: consensus membership matrix Z at k*
# Slow: run once, results are saved to Data/ and used by 03_community_plots.R.
# ==================================================================

source("scripts/00_setup.R")      # packages, functions, isn, epochs, san_periods


# ------------------------------------------------------------------
# Settings
# ------------------------------------------------------------------
k_range   <- 2:50     # candidate numbers of communities
runs_k    <- 50       # SMON runs per k for selecting k*
runs_cons <- 150      # SMON runs for the consensus Z at k*

# For the small run (meaningless results):
# k_range   <- 2:15
# runs_k    <- 2
# runs_cons <- 3

set.seed(2026)        # once: runs differ from each other, the script is reproducible


# ==================================================================
# STEP 1: SELECT k* PER PERIOD (slow)
# ==================================================================

avg_all <- list()
k_star  <- integer(0)

for (e in names(epochs)) {
  avg_df <- avg_mod_smon(san_periods[[e]], runs = runs_k, k_range = k_range)

  k_star[e] <- choose_k_cross(avg_df$avg)

  avg_df$avg[, epoch := e]
  avg_all[[e]] <- avg_df$avg

  cat("epoch", e, ": k* =", k_star[e],
      "(", round(avg_df$total_time / 60, 1), "min )\n")
}

avg_epochs <- rbindlist(avg_all)   # k, avg_mod_cont, avg_mod_disc, sd_*, epoch

# sd of discrete modularity at its maximum vs. at k* ([X] in Section 5.1)
sd_comparison <- avg_epochs[, .(
  k_max_disc  = k[which.max(avg_mod_disc)],
  sd_at_max   = sd_mod_disc[which.max(avg_mod_disc)],
  k_star      = k_star[.BY$epoch],
  sd_at_kstar = sd_mod_disc[k == k_star[.BY$epoch]]
), by = epoch]
sd_comparison[, ratio := sd_at_max / sd_at_kstar]
print(sd_comparison)


# ==================================================================
# STEP 2: CONSENSUS PARTITION AT k*
# ==================================================================

comm_all <- list()
z_all    <- list()

for (e in names(epochs)) {
  cons <- smon_consensus(san_periods[[e]], k = k_star[[e]], runs = runs_cons)

  comm_all[[e]] <- as.data.table(cons$communities)[, epoch := e]
  z_all[[e]]    <- cons$avg_z

  cat("epoch", e, ": consensus done (k* =", k_star[[e]], ")\n")
}

comm_epochs <- rbindlist(comm_all)  # actors, community, prob, epoch

# add state / non-state information
actor_type <- unique(isn[!is.na(ext_name), .(actors = ext_name, ext_nonstate)])
stopifnot(!anyDuplicated(actor_type$actors))          # one type per actor
epochs_communities_avg <- actor_type[comm_epochs, on = "actors"]


# ==================================================================
# SAVE
# ==================================================================

saveRDS(san_periods,            "Data/epochs_san.rds")              # networks SMON ran on
saveRDS(avg_epochs,             "Data/epochs_modularity_avg.rds")   # curves for Q_comparison
saveRDS(k_star,                 "Data/epochs_k_star.rds")
saveRDS(sd_comparison,          "Data/epochs_sd_comparison.rds")
saveRDS(z_all,                  "Data/epochs_avg_z.rds")            # full consensus Z per period
saveRDS(epochs_communities_avg, "Data/epochs_communities_avg.rds")
