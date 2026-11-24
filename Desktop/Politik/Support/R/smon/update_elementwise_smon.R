# ------------------------------------------------------------------
# One multiplicative SMON update (Zhang et al. 2020).
#
# B_plus, B_minus: positive and negative parts of the shifted modularity
# matrix B_tilde = B_plus - B_minus. They are constant during a run, so
# run_smon() computes them once and passes them in.
#
# Lambda = Z' B_tilde Z is computed from the same two products that the
# update needs, so each iteration costs two n x n x k products instead
# of three, and no n x n matrix is split anew.
# ------------------------------------------------------------------
update_elementwise <- function(z, B_plus, B_minus) {
  Pz <- B_plus  %*% z
  Mz <- B_minus %*% z

  Lambda  <- crossprod(z, Pz - Mz)          # = t(z) %*% B_tilde %*% z
  Lambda_plus  <- pmax(Lambda, 0)
  Lambda_minus <- pmax(-Lambda, 0)

  num   <- Pz + z %*% Lambda_minus
  denom <- Mz + z %*% Lambda_plus

  z * sqrt(pmax(num / (denom + 1e-10), 0))
}
