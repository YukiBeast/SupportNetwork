library(data.table)
library(clue)   # for solve_LSAP (Hungarian alignment)
library(igraph)

# ------------------------------------------------------------------
# 1. average continuous and discrete modularity over runs, per k
# ------------------------------------------------------------------
avg_mod_smon <- function(san_filtered, runs, k_range = 2:50) {
  mod_list <- vector("list", runs)
  times    <- numeric(runs)          # seconds per run
  
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
    avg      = avg[order(k)],
    time_per_run = times,
    total_time   = sum(times)
  )
}


# ------------------------------------------------------------------
# 2. choose k at the crossing point of the two normalized curves
# ------------------------------------------------------------------
choose_k_cross <- function(avg_df) {
  # normalize each curve to [0,1] so they share a scale
  norm_cont <- (avg_df$avg_mod_cont - min(avg_df$avg_mod_cont)) /
    (max(avg_df$avg_mod_cont) - min(avg_df$avg_mod_cont))
  norm_disc <- (avg_df$avg_mod_disc - min(avg_df$avg_mod_disc)) /
    (max(avg_df$avg_mod_disc) - min(avg_df$avg_mod_disc))
  
  # continuous rises, discrete falls -> they cross once; find the sign change
  diff <- norm_cont - norm_disc
  sign_change <- which(diff[-1] * diff[-length(diff)] < 0)
  if (length(sign_change) == 0) return(NA_integer_)
  avg_df$k[sign_change[1] + 1]   # first crossing
}

# ------------------------------------------------------------------
# 3. consensus soft partition at a fixed k
# ------------------------------------------------------------------
smon_consensus <- function(san_filtered, k, runs) {
  # run smon runs times with the chosen k
  z_list <- vector("list", runs)
  for (r in seq_len(runs)) {
    smon <- run_smon(san_filtered, num_comm = k)
    z_list[[r]] <- as.matrix(smon$by_k[[as.character(k)]]$z_norm)
  }
  
  n <- nrow(z_list[[1]])
  stopifnot(all(sapply(z_list, nrow) == n))   # same node set in every run
  
  # align each run's columns to the first one via Hungarian on 1 - cor
  ref <- z_list[[1]]
  for (r in 2:runs) {
    sim <- cor(ref, z_list[[r]])
    sim[is.na(sim)] <- 0                      # guard against constant columns
    perm <- solve_LSAP(1 - sim)
    z_list[[r]] <- z_list[[r]][, as.integer(perm)]
  }
  
  # elementwise average
  avg_z <- Reduce(`+`, z_list) / runs
  
  # hard partition + confidence, keeping every node (isolates -> NA / 0)
  partition <- apply(avg_z, 1, function(row) {
    if (sum(row) == 0) return(NA_integer_)
    which.max(row)
  })
  prob <- apply(avg_z, 1, max)                # confidence for alpha scaling
  prob[is.na(partition)] <- 0                 # isolates: no affiliation, alpha 0
  
  # data.frame in the same format assign_communities returns
  communities <- data.frame(
    actors    = rownames(avg_z),
    community = unname(partition),
    prob      = round(unname(prob), 3),
    stringsAsFactors = FALSE
  )
  
  list(
    avg_z       = avg_z,
    partition   = partition,      # named vector, NA for isolates
    prob        = prob,           # named vector, 0 for isolates
    communities = communities     # ready for plot_smon_ggraph
  )
}


epochs <- list(1975:1991, 1992:2001, 2002:2008, 2009:2017)

avg_all   <- list()
comm_all  <- list()

for (period in epochs) {
  epoch_label <- paste0(min(period), "-", max(period))
  
  san_filtered <- san[year %in% period, .(state1, state2, balance)]
  
  # step 1: average modularity over runs, per k
  avg_df <- avg_mod_smon(san_filtered, runs = 50, k_range = 2:50)
  
  # step 2: pick k* at the crossing point
  k_star <- choose_k_cross(avg_df$avg)
  
  # step 3: consensus partition at that k
  cons <- smon_consensus(san_filtered, k = k_star, runs = 50)
  
  # tag with epoch and collect
  avg_df$avg$epoch  <- epoch_label
  comm_all[[epoch_label]] <- cons$communities
  comm_all[[epoch_label]]$epoch <- epoch_label
  avg_all[[epoch_label]]  <- avg_df$avg
  
  cat("epoch", epoch_label, ": k* =", k_star, "\n")
}

# bind everything into two data.frames
avg_epochs  <- rbindlist(avg_all)   # k, avg_mod_cont, avg_mod_disc, sd_*, epoch
comm_epochs <- rbindlist(comm_all)  # actors, community, prob, epoch
epochs

# comm_epochs[comm_epochs$prob == 0]$community <- NA

isn_nocoalitions <- load_support_nocoalitions()
epochs_communities_avg <- comm_epochs[unique(isn_nocoalitions[, .(ext_name, ext_nonstate)]),
                                      on = c("actors" = "ext_name")]

epochs_communities_avg <- epochs_communities_avg[!is.na(epoch)]
saveRDS(epochs_communities_avg, "Data/epochs_communities_avg.rds")
