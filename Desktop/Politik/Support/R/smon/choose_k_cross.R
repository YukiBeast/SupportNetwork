# ------------------------------------------------------------------
# choose k*: first k at which normalized continuous modularity
# exceeds normalized discrete modularity
#   K* = min{ K : Q_d~(K) < Q_c~(K) }   (Equation k star in the paper)
# ------------------------------------------------------------------
choose_k_cross <- function(avg_df) {
  norm_cont <- (avg_df$avg_mod_cont - min(avg_df$avg_mod_cont)) /
    (max(avg_df$avg_mod_cont) - min(avg_df$avg_mod_cont))
  norm_disc <- (avg_df$avg_mod_disc - min(avg_df$avg_mod_disc)) /
    (max(avg_df$avg_mod_disc) - min(avg_df$avg_mod_disc))
  
  idx <- which(norm_disc < norm_cont)
  if (length(idx) == 0) {
    warning("Continuous modularity never exceeds discrete modularity in this k range.")
    return(NA_integer_)
  }
  avg_df$k[idx[1]]
}
