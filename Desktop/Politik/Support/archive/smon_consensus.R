# ------------------------------------------------------------------
# 3. consensus soft partition at a fixed k
# ------------------------------------------------------------------
smon_consensus <- function(san_filtered, k, runs) {
  z_list <- vector("list", runs)
  for (r in seq_len(runs)) {
    smon <- run_smon(san_filtered, num_comm = k, seed = FALSE)   # independent start every run
    z_list[[r]] <- as.matrix(smon$by_k[[as.character(k)]]$z_norm)
  }
  
  n <- nrow(z_list[[1]])
  stopifnot(all(sapply(z_list, nrow) == n))      # same node set in every run
  
  # align each run's columns to the first run (Hungarian on 1 - cor)
  ref <- z_list[[1]]
  for (r in 2:runs) {
    sim <- cor(ref, z_list[[r]])
    sim[is.na(sim)] <- 0                         # guard against constant columns
    perm <- solve_LSAP(1 - sim)
    z_list[[r]] <- z_list[[r]][, as.integer(perm)]
  }
  
  # elementwise average
  avg_z <- Reduce(`+`, z_list) / runs
  
  # hard partition + membership weight (isolates -> NA / 0)
  partition <- apply(avg_z, 1, function(row) {
    if (sum(row) == 0) return(NA_integer_)
    which.max(row)
  })
  prob <- apply(avg_z, 1, max)
  prob[is.na(partition)] <- 0
  
  communities <- data.frame(
    actors    = rownames(avg_z),
    community = unname(partition),
    prob      = round(unname(prob), 3),
    stringsAsFactors = FALSE
  )
  
  list(
    avg_z       = avg_z,
    partition   = partition,
    prob        = prob,
    communities = communities
  )
}