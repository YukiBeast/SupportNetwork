choose_k <- function(smon_results, 
                     method = "elbow",  # elbow (C) or frobenius (B)
                     tau_range = c(0.70, 0.90),
                     n_range = c(40, 200),
                     min_comm_size = 2) {
  
  # 1. Determine network size (n) from the converged matrices
  first_k <- names(smon_results$by_k)[1]
  n <- nrow(smon_results$by_k[[first_k]]$z_norm)
  
  # 2. Extract cluster distributions, modularities, and Frobenius distances for each k
  k_data <- rbindlist(lapply(smon_results$results_summary$k, function(k_val) {
    k_info <- smon_results$by_k[[as.character(k_val)]]
    comm_sizes <- table(k_info$cluster_assignments)
    
    # Count degenerate communities (e.g., singletons or dyads)
    num_trivial <- sum(comm_sizes < min_comm_size)
    
    # Calculate Frobenius Distance (Distortion)
    D <- k_info$z_norm - k_info$z_discrete
    frob_dist <- sqrt(sum(D^2))
    
    data.table(
      k            = k_val,
      num_trivial  = num_trivial,
      mod_discrete = k_info$mod_discrete,
      frob_dist    = frob_dist
    )
  }))
  
  # 3. Filter out candidate k values that produce trivial/singleton communities
  k_valid <- k_data[num_trivial == 0]
  
  if (nrow(k_valid) == 0) {
    warning("All k produced degenerate communities. Falling back to all k values.")
    k_valid <- k_data
  }
  
  # 4. Selection Logic based on chosen method
  if (method == "frobenius") {
    
    # The best k minimizes the Frobenius distance (information loss)
    best_idx <- which.min(k_valid$frob_dist)
    best_k <- k_valid$k[best_idx]
    
    diagnostics <- list(
      method        = "frobenius",
      n_nodes       = n,
      min_frob_dist = round(k_valid$frob_dist[best_idx], 4)
    )
    
  } else {
    
    # Default / Elbow Method
    max_mod <- max(k_valid$mod_discrete, na.rm = TRUE)
    
    if (method == "elbow") {
      # Linear interpolation of tau based on n, clamped between tau_range
      frac <- (n - n_range[1]) / (n_range[2] - n_range[1])
      frac_clamped <- pmin(pmax(frac, 0), 1)
      tau <- tau_range[2] - frac_clamped * (tau_range[2] - tau_range[1])
    } else {
      # Fallback to a fixed 90% threshold if an unknown method is passed
      tau <- 0.90 
    }
    
    min_mod_threshold <- max_mod * tau
    
    # Select the largest valid k within the allowed tolerance
    candidates <- k_valid[mod_discrete >= min_mod_threshold]
    best_k <- max(candidates$k)
    
    diagnostics <- list(
      method    = method,
      n_nodes   = n,
      tau_used  = round(tau, 3),
      threshold = round(min_mod_threshold, 4),
      max_mod   = round(max_mod, 4)
    )
  }
  
  # 5. Extract and return the chosen result with diagnostics attached
  chosen_result <- smon_results$by_k[[as.character(best_k)]]
  chosen_result$selection_diagnostics <- diagnostics
  
  return(chosen_result)
}
