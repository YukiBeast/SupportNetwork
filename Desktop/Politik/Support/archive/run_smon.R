library(data.table)
library(igraph)

run_smon <- function(san_filtered, num_comm, max_iter = 500, seed = TRUE) {
  if (seed) {
    set.seed(123)
  }
  
  num_comm <- sort(num_comm)
  
  g <- graph_from_data_frame(san_filtered, directed = FALSE)
  A <- as_adjacency_matrix(g, attr = "balance", sparse = FALSE)
  
  A_plus <- (abs(A) + A) / 2
  A_minus <- (abs(A) - A) / 2
  
  k_plus <- rowSums(A_plus)
  k_minus <- rowSums(A_minus)
  
  # For undirected graphs, sum(rowSums) is exactly 2m
  m_plus <- sum(k_plus)
  m_minus <- sum(k_minus)
  
  # Global normalization factor (2m^+ + 2m^-)
  total_2m <- m_plus + m_minus 
  
  B <- A - (1 / m_plus) * (k_plus %*% t(k_plus)) + (1 / m_minus) * (k_minus %*% t(k_minus))
  
  n <- nrow(B)
  eigenvalues <- eigen(B, symmetric = TRUE, only.values = TRUE)$values
  lambda_min <- min(eigenvalues)
  
  q <- abs(lambda_min) + 1e-5
  B_tilde <- q * diag(nrow = n) + B
  
  # Storage for all k
  k_details_list <- list()
  k_summary_list <- vector("list", length(num_comm))
  
  for (idx in seq_along(num_comm)) {
    k <- num_comm[idx]
    
    z_0 <- matrix(runif(n * k), nrow = n, ncol = k)
    z_old <- z_0
    
    mod_history <- numeric(max_iter)
    
    for (iter in seq_len(max_iter)) {
      Lambda <- t(z_old) %*% (B_tilde %*% z_old)
      z_new <- update_elementwise(z_old, B_tilde, Lambda)
      z_old <- z_new
      
      current_mod <- sum(z_old * (B %*% z_old)) / total_2m
      mod_history[iter] <- current_mod
    }
    
    final_cont_mod <- mod_history[max_iter]
    
    # 1. Normalize rows to sum to 1 (Probabilities)
    z_norm <- z_old / pmax(rowSums(z_old), 1e-10)
    
    # 2. Strict discrete assignments
    cluster_assign_k <- max.col(z_norm, ties.method = "first")
    z_discrete_k <- matrix(0, nrow = n, ncol = k)
    z_discrete_k[cbind(seq_len(n), cluster_assign_k)] <- 1
    
    # 3. Strict discrete modularity score
    final_disc_mod <- sum(z_discrete_k * (B %*% z_discrete_k)) / total_2m
    
    # Store converged objects for this specific k
    k_details_list[[as.character(k)]] <- list(
      k                   = k,
      z                   = z_old,
      z_norm              = z_norm,
      z_discrete          = z_discrete_k,
      cluster_assignments = cluster_assign_k,
      mod_continuous      = final_cont_mod,
      mod_discrete        = final_disc_mod,
      mod_history         = mod_history
    )
    
    # Metric summary row
    k_summary_list[[idx]] <- data.frame(
      k              = k,
      mod_continuous = final_cont_mod,
      mod_discrete   = final_disc_mod
    )
  }
  
  summary_df <- rbindlist(k_summary_list)
  
  return(list(
    results_summary = summary_df,      # data.table with k, mod_continuous, mod_discrete
    by_k            = k_details_list,  # Named list (key = as.character(k)) with all converged matrices
    graph           = g,               # Full underlying igraph object
    B               = B,               # Modularity matrix
    total_2m        = total_2m         # Normalization denominator
  ))
}
