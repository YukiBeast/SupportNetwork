# ------------------------------------------------------------------
# SMON: signed modularity maximization with orthogonal and nonnegative
# constraints (Zhang et al. 2020), for one or several numbers of
# communities k.
#
# san_filtered: one row per actor pair (state1, state2, balance),
#               e.g. san_periods[["1975-1991"]]
# P and N are the positive and negative parts of the net weight W.
#
# seed = TRUE fixes the start (set.seed(123)); keep FALSE for ensembles.
# track_history = TRUE stores continuous modularity at every iteration
# (only needed for convergence diagnostics; costs one extra product).
# ------------------------------------------------------------------
run_smon <- function(san_filtered, num_comm, max_iter = 500,
                     seed = FALSE, track_history = FALSE) {
  if (seed) set.seed(123)
  num_comm <- sort(num_comm)

  # --- 1. signed adjacency and modularity matrix ---
  g <- graph_from_data_frame(san_filtered, directed = FALSE)
  if (any_multiple(g)) stop("More than one edge per actor pair: use the period SAN.")
  A <- as_adjacency_matrix(g, attr = "balance", sparse = FALSE)

  A_plus  <- pmax(A, 0)
  A_minus <- pmax(-A, 0)
  k_plus  <- rowSums(A_plus)
  k_minus <- rowSums(A_minus)
  m_plus  <- sum(k_plus)                      # = 2m+
  m_minus <- sum(k_minus)                     # = 2m-
  if (m_plus == 0 || m_minus == 0) stop("Network has no positive or no negative ties.")
  total_2m <- m_plus + m_minus

  B <- A - tcrossprod(k_plus) / m_plus + tcrossprod(k_minus) / m_minus
  n <- nrow(B)

  # --- 2. shift to positive semidefinite, split once ---
  lambda_min <- min(eigen(B, symmetric = TRUE, only.values = TRUE)$values)
  q <- abs(lambda_min) + 1e-5
  B_tilde <- B + q * diag(n)
  B_plus  <- pmax(B_tilde, 0)
  B_minus <- pmax(-B_tilde, 0)

  # --- 3. optimize for each k ---
  k_details_list <- list()
  k_summary_list <- vector("list", length(num_comm))

  for (idx in seq_along(num_comm)) {
    k <- num_comm[idx]

    z <- matrix(runif(n * k), nrow = n, ncol = k, dimnames = list(rownames(A), NULL))
    mod_history <- if (track_history) numeric(max_iter) else NULL

    for (iter in seq_len(max_iter)) {
      z <- update_elementwise(z, B_plus, B_minus)
      if (track_history) mod_history[iter] <- sum(z * (B %*% z)) / total_2m
    }

    final_cont_mod <- sum(z * (B %*% z)) / total_2m

    # row-normalized weights, hard assignment, discrete modularity
    z_norm <- z / pmax(rowSums(z), 1e-10)
    cluster_assign_k <- max.col(z_norm, ties.method = "first")
    z_discrete_k <- matrix(0, nrow = n, ncol = k)
    z_discrete_k[cbind(seq_len(n), cluster_assign_k)] <- 1
    final_disc_mod <- sum(z_discrete_k * (B %*% z_discrete_k)) / total_2m

    k_details_list[[as.character(k)]] <- list(
      k                   = k,
      z                   = z,
      z_norm              = z_norm,
      z_discrete          = z_discrete_k,
      cluster_assignments = cluster_assign_k,
      mod_continuous      = final_cont_mod,
      mod_discrete        = final_disc_mod,
      mod_history         = mod_history
    )
    k_summary_list[[idx]] <- data.table(
      k              = k,
      mod_continuous = final_cont_mod,
      mod_discrete   = final_disc_mod
    )
  }

  list(
    results_summary = rbindlist(k_summary_list),
    by_k            = k_details_list,
    graph           = g,
    B               = B,
    total_2m        = total_2m
  )
}
