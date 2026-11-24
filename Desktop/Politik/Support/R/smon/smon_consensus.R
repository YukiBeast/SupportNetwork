# ------------------------------------------------------------------
# Consensus soft partition at a fixed k: run SMON `runs` times from
# independent random starts, align the columns of all runs to a common
# reference (Hungarian algorithm on 1 - correlation), and average.
#
# Alignment is iterative: all runs are first aligned to run 1 and
# averaged; then all runs are re-aligned to that average, and so on,
# until the average no longer changes (at most max_align_iter times).
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

  # align every run's columns to a reference matrix
  align_to <- function(ref) {
    lapply(z_list, function(z) {
      sim <- suppressWarnings(cor(ref, z))                # constant columns give NA
      sim[is.na(sim)] <- 0
      z[, as.integer(clue::solve_LSAP(1 - sim))]
    })
  }

  # iterative alignment: reference = run 1, then the current average
  ref <- z_list[[1]]
  for (it in seq_len(max_align_iter)) {
    avg_z <- Reduce(`+`, align_to(ref)) / runs
    if (isTRUE(all.equal(avg_z, ref, check.attributes = FALSE))) break
    ref <- avg_z
  }
  if (it == max_align_iter) warning("Alignment did not converge in ", max_align_iter, " iterations.")
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
    communities = communities
  )
}
