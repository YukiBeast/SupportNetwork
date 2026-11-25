# ==================================================================
# 02_smon.R
# SMON community detection on the four period SANs:
#   step 1: select k* per period from ensemble-averaged modularity curves
#   step 2: consensus membership matrix Z at k*
# Runs in parallel and saves every finished step to Data/checkpoints/,
# so an interrupted run resumes where it stopped.
# ==================================================================

source("scripts/00_setup.R")      # packages, functions, isn, epochs, san_periods


# ------------------------------------------------------------------
# Settings
# ------------------------------------------------------------------
k_range <- NULL   # set per period below: 2 to floor(n / 4)

# candidate k per period: at most n/4 communities of at least 4 actors
n_nodes <- vapply(san_periods, function(s) uniqueN(c(s$state1, s$state2)), integer(1))
k_range <- lapply(n_nodes, function(n) 2:ceiling(n / 4))
print(data.table(epoch = names(n_nodes), n = n_nodes,
                 k_max = vapply(k_range, max, integer(1))))

runs_k    <- 250       # SMON runs per k for selecting k*
runs_cons <- 500      # SMON runs for the consensus Z at k*
# n_cores   <- max(1, parallel::detectCores() - 1)
n_cores <- 15

ckpt <- "Data/checkpoints"
dir.create(ckpt, recursive = TRUE, showWarnings = FALSE)
# NOTE: delete Data/checkpoints/ whenever the settings or the networks change


# ------------------------------------------------------------------
# Excluded actors per period (footnote in Section 5.1)
# ------------------------------------------------------------------
excluded <- rbindlist(lapply(names(epochs), function(e) {
  isn_e    <- isn[year %in% epochs[[e]] & !is.na(ext_name)]
  all_act  <- unique(isn_e$ext_name)
  sw       <- load_san_window(isn_e)                      # before dropping zero weights
  with_tie <- unique(c(sw$state1, sw$state2))
  in_san   <- unique(c(san_periods[[e]]$state1, san_periods[[e]]$state2))
  data.table(
    epoch          = e,
    actors_isn     = length(all_act),
    no_tie         = length(setdiff(all_act, with_tie)),  # intervened without co-/counter-intervener
    only_nullified = length(setdiff(with_tie, in_san)),   # all ties cancel out
    in_san         = length(in_san)
  )
}))
print(excluded)


# ------------------------------------------------------------------
# Parallel cluster (works on Windows, macOS and Linux)
# ------------------------------------------------------------------
closeAllConnections()
cl <- parallel::makeCluster(n_cores)
parallel::clusterEvalQ(cl, { library(data.table); library(igraph) })
parallel::clusterExport(cl, c("run_smon", "update_elementwise"))
parallel::clusterSetRNGStream(cl, 2026)   # reproducible for a given n_cores
cat("Running on", n_cores, "cores\n")


# ==================================================================
# STEP 1: SELECT k* PER PERIOD (slow)
# ==================================================================

avg_all <- list()
k_star  <- integer(0)

for (e in names(epochs)) {
  f <- file.path(ckpt, paste0("avg_mod_", e, ".rds"))

  if (file.exists(f)) {
    avg <- readRDS(f)
    cat("epoch", e, ": loaded from checkpoint\n")
  } else {
    parallel::clusterSetRNGStream(cl, 2026 + match(e, names(epochs)))
    res <- avg_mod_smon(san_periods[[e]], runs = runs_k, k_range = k_range[[e]], cl = cl)
    avg <- res$avg[, epoch := e]
    saveRDS(avg, f)
    cat("epoch", e, ": done in", round(res$total_time / 60, 1), "min\n")
  }

  avg_all[[e]] <- avg
  k_star[e]    <- choose_k_cross(avg)
  cat("epoch", e, ": k* =", k_star[e], "\n")
}

if (anyNA(k_star)) {
  stop("No k* found for: ", paste(names(k_star)[is.na(k_star)], collapse = ", "),
       ". Extend k_range or increase runs_k.")
}

avg_epochs <- rbindlist(avg_all)   # k, avg_mod_cont, avg_mod_disc, sd_*, epoch

# sd of discrete modularity at its maximum vs. at k* ([X] in Section 5.1)
sd_comparison <- avg_epochs[, {
  ks    <- k_star[[.BY$epoch]]
  i_max <- which.max(avg_mod_disc)
  i_ks  <- which(k == ks)
  .(k_max_disc  = k[i_max],
    sd_at_max   = sd_mod_disc[i_max],
    k_star      = ks,
    sd_at_kstar = sd_mod_disc[i_ks])
}, by = epoch]
sd_comparison[, ratio := sd_at_max / sd_at_kstar]
print(sd_comparison)

# k* when the curves are normalized over shorter k ranges (appendix)
k_sens <- rbindlist(lapply(names(epochs), function(e) {
  bounds <- c("20" = 20, "30" = 30, "n/4" = max(k_range[[e]]))
  rbindlist(lapply(names(bounds), function(b) {
    avg_epochs[epoch == e & k <= bounds[[b]],
               .(epoch = e, bound = b, k_star = choose_k_cross(.SD))]
  }))
}))
print(dcast(k_sens, epoch ~ bound, value.var = "k_star"))
saveRDS(k_sens, "Data/results/epochs_k_sensitivity.rds")

# ==================================================================
# STEP 2: CONSENSUS PARTITION AT k*
# ==================================================================

comm_all <- list()
z_all    <- list()

for (e in names(epochs)) {
  f <- file.path(ckpt, paste0("consensus_", e, "_k", k_star[[e]], ".rds"))

  if (file.exists(f) && "zero_share" %in% names(readRDS(f)$communities)) {
    cons <- readRDS(f)
    cat("epoch", e, ": consensus loaded from checkpoint\n")
  } else {
    parallel::clusterSetRNGStream(cl, 3026 + match(e, names(epochs)))
    cons <- smon_consensus(san_periods[[e]], k = k_star[[e]], runs = runs_cons, cl = cl)
    saveRDS(cons, f)
    cat("epoch", e, ": consensus done (k* =", k_star[[e]], ")\n")
  }

  comm_all[[e]] <- as.data.table(cons$communities)[, epoch := e]
  z_all[[e]]    <- cons$avg_z
}

parallel::stopCluster(cl)

for (e in names(epochs)) {
  f <- file.path(ckpt, paste0("consensus_", e, "_k", k_star[[e]], ".rds"))
  cons <- readRDS(f)
  cat(e, ": iterations =", cons$align_iter, "| unstable runs =", cons$n_unstable, "\n")
  if (cons$n_unstable > 0) {
    z <- cons$avg_z
    top2 <- t(apply(z, 1, function(r) sort(r, decreasing = TRUE)[1:2]))
    margin <- top2[, 1] - top2[, 2]
    bound <- cons$n_unstable / length(cons$zero_share) * 0 + cons$n_unstable / 500
    print(sort(margin[margin < bound]))
  }
}

comm_epochs <- rbindlist(comm_all)  # actors, community, prob, epoch

# add state / non-state information
actor_type <- unique(isn[!is.na(ext_name), .(actors = ext_name, ext_nonstate)])
stopifnot(!anyDuplicated(actor_type$actors))
epochs_communities_avg <- actor_type[comm_epochs, on = "actors"]


# ==================================================================
# SAVE
# ==================================================================

saveRDS(san_periods,            "Data/results/epochs_san.rds")              # networks SMON ran on
saveRDS(avg_epochs,             "Data/results/epochs_modularity_avg.rds")   # curves for Q_comparison
saveRDS(k_star,                 "Data/results/epochs_k_star.rds")
saveRDS(sd_comparison,          "Data/results/epochs_sd_comparison.rds")
saveRDS(excluded,               "Data/results/epochs_excluded_actors.rds")  # footnote numbers
saveRDS(z_all,                  "Data/results/epochs_avg_z.rds")            # full consensus Z per period
saveRDS(epochs_communities_avg, "Data/results/epochs_communities_avg.rds")
