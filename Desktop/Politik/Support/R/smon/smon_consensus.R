# ------------------------------------------------------------------
# Consensus soft partition at a fixed k: run SMON `runs` times from
# independent random starts, align the columns of all runs to a common
# reference (Hungarian algorithm on 1 - correlation), and average.
#
# Alignment is iterative: all runs are first aligned to run 1 and
# averaged; then all runs are re-aligned to that average, and so on,
# until no run changes its column order any more (at most
# max_align_iter times).
# Aligning to a single run makes the result depend on that run: if it
# is atypical, communities that are separate in almost every run can be
# merged in the average.
#
# SMON can drive an actor's whole row of Z to zero (no membership in
# any community). Such rows are counted per run: zero_share is the
# share of runs in which the actor had no membership. Rows of avg_z
# sum to 1 - zero_share, not to one.
#
# cl: optional parallel cluster (parallel::makeCluster).
# ------------------------------------------------------------------
smon_consensus <- function(san_filtered, k, runs, cl = NULL, max_align_iter = 20) {

  one_run <- function(r, san, k) {
    smon <- run_smon(san, num_comm = k, seed = FALSE)   # independent start every run
    as.matrix(smon$by_k[[as.character(k)]]$z_norm)
  }
  environment(one_run) <- globalenv()

  z_list <- if (is.null(cl)) {
    lapply(seq_len(runs), one_run, san = san_filtered, k = k)
  } else {
    parallel::parLapply(cl, seq_len(runs), one_run, san = san_filtered, k = k)
  }

  n <- nrow(z_list[[1]])
  stopifnot(all(vapply(z_list, nrow, numeric(1)) == n))   # same node set in every run

  # actors with an all-zero membership row, per run
  zero_rows  <- vapply(z_list, function(z) rowSums(z) < 1e-8, logical(n))  # n x runs
  zero_share <- rowMeans(zero_rows)

  # column order that best matches a reference matrix, for every run
  perms_to <- function(ref) {
    lapply(z_list, function(z) {
      sim <- suppressWarnings(cor(ref, z))                # constant columns give NA
      sim[is.na(sim)] <- 0
      as.integer(clue::solve_LSAP(1 - sim))
    })
  }

  # iterative alignment: reference = run 1, then the current average;
  # converged when no run changes its column order any more
  ref <- z_list[[1]]
  perms_old <- NULL
  for (it in seq_len(max_align_iter)) {
    perms <- perms_to(ref)
    avg_z <- Reduce(`+`, Map(function(z, p) z[, p], z_list, perms)) / runs
    n_changed <- if (is.null(perms_old)) runs else sum(!mapply(identical, perms, perms_old))
    if (n_changed == 0) break
    perms_old <- perms
    ref <- avg_z
  }
  if (n_changed > 0) {
    warning("Alignment not fully converged after ", max_align_iter, " iterations: ",
            n_changed, " of ", runs, " runs still switch column order.")
  }
  rownames(avg_z) <- rownames(z_list[[1]])

  partition <- max.col(avg_z, ties.method = "first")
  prob      <- apply(avg_z, 1, max)

  communities <- data.frame(
    actors     = rownames(avg_z),
    community  = unname(partition),
    prob       = round(unname(prob), 3),
    zero_share = round(unname(zero_share), 3),
    stringsAsFactors = FALSE
  )

  list(
    avg_z       = avg_z,
    partition   = setNames(partition, rownames(avg_z)),
    prob        = prob,
    zero_share  = setNames(zero_share, rownames(avg_z)),
    align_iter  = it,
    n_unstable  = n_changed,             # runs still switching alignment (0 = converged)
    communities = communities
  )
}
