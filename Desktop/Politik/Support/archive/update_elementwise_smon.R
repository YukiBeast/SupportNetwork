update_elementwise <- function(z_old, B_tilde, Lambda) {
  # Split B_tilde directly
  B_plus <- (abs(B_tilde) + B_tilde) / 2
  B_minus <- (abs(B_tilde) - B_tilde) / 2
  
  n <- nrow(B_tilde)
  
  # Split Lambda
  Lambda_plus <- (abs(Lambda) + Lambda) / 2
  Lambda_minus <- (abs(Lambda) - Lambda) / 2
  
  # Calculate matrices directly (no need to flatten/unflatten with as.vector)
  num <- (B_plus %*% z_old) + (z_old %*% Lambda_minus)
  denom <- (B_minus %*% z_old) + (z_old %*% Lambda_plus)
  
  # Apply floating point protection and division-by-zero safeguard
  ratio <- pmax(num / (denom + 1e-10), 0)
  
  z_new <- z_old * sqrt(ratio)
  
  return(z_new)
}
