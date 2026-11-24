# ------------------------------------------------------------------
# Shared layout for all network plots (ISN, SAN, SMON).
#
# 1. The graph is split into its connected components.
# 2. Each component gets its own stress layout (graphlayouts), centered,
#    with an optional radial transformation: stretch < 1 pulls the
#    periphery in and spreads the core, stretch = 1 leaves it unchanged.
# 3. The largest component is kept as it is. Every smaller component is
#    rescaled to the same typical edge length and placed into the largest
#    empty gap inside the main network's bounding box, so it fills unused
#    space instead of widening the figure. Only if no gap is big enough
#    is it placed just outside, as close to the centre as possible.
#
# distance: optional positive edge lengths (larger = further apart),
#           in the order of E(g).
# margin:   free space around a placed component, in typical edge lengths.
# Returns a 2-column matrix with one row per vertex, rownames = V(g)$name.
# ------------------------------------------------------------------
layout_stress_radial <- function(g, distance = NULL, stretch = 1, seed = 123,
                                 margin = 1, grid_n = 80) {

  if (!is.null(distance)) {
    stopifnot(length(distance) == igraph::ecount(g), all(distance > 0))
    igraph::E(g)$layout_dist <- distance        # carried into the components
  }
  set.seed(seed)

  # --- helpers ---
  median_edge_length <- function(cg, l) {
    el <- igraph::as_edgelist(cg, names = FALSE)
    if (nrow(el) == 0) return(NA_real_)
    stats::median(sqrt(rowSums((l[el[, 1], , drop = FALSE] - l[el[, 2], , drop = FALSE])^2)))
  }

  layout_component <- function(cg) {
    n <- igraph::vcount(cg)
    if (n == 1) return(matrix(0, nrow = 1, ncol = 2))
    if (n == 2) return(matrix(c(-0.5, 0, 0.5, 0), nrow = 2, byrow = TRUE))
    w <- if (is.null(distance)) NA else igraph::E(cg)$layout_dist
    l <- graphlayouts::layout_with_stress(cg, weights = w)
    l <- sweep(l, 2, colMeans(l))
    if (stretch != 1) {
      r <- pmax(sqrt(rowSums(l^2)), 1e-6)
      l <- l * r^(stretch - 1)
    }
    l
  }

  min_dist_to <- function(cand, pts) {
    d2 <- outer(cand[, 1], pts[, 1], "-")^2 + outer(cand[, 2], pts[, 2], "-")^2
    sqrt(apply(d2, 1, min))
  }

  grid_in <- function(lo, hi) {
    as.matrix(expand.grid(seq(lo[1], hi[1], length.out = grid_n),
                          seq(lo[2], hi[2], length.out = grid_n)))
  }

  # --- 1. components, largest first ---
  comps <- igraph::decompose(g)
  comps <- comps[order(vapply(comps, igraph::vcount, numeric(1)), decreasing = TRUE)]
  lays  <- lapply(comps, layout_component)

  # --- 2. place the smaller components ---
  unit <- median_edge_length(comps[[1]], lays[[1]])
  if (!is.finite(unit) || unit <= 0) unit <- 1
  placed <- lays[[1]]

  if (length(comps) > 1) {
    for (i in 2:length(comps)) {
      l <- lays[[i]]
      len <- median_edge_length(comps[[i]], l)
      if (is.finite(len) && len > 0) l <- l * unit / len    # same scale as main network
      l <- sweep(l, 2, colMeans(l))

      need <- max(sqrt(rowSums(l^2))) + margin * unit       # radius incl. free space
      lo <- apply(placed, 2, min)
      hi <- apply(placed, 2, max)

      # a) largest gap inside the bounding box (component stays inside it)
      best <- NULL
      if (all(hi - lo > 2 * need)) {
        cand <- grid_in(lo + need, hi - need)
        dmin <- min_dist_to(cand, placed)
        if (max(dmin) >= need) best <- cand[which.max(dmin), ]
      }

      # b) otherwise: just outside, as close to the centre as possible
      if (is.null(best)) {
        cand <- grid_in(lo - need, hi + need)
        dmin <- min_dist_to(cand, placed)
        ok   <- dmin >= need
        ctr  <- colMeans(placed)
        dctr <- sqrt((cand[, 1] - ctr[1])^2 + (cand[, 2] - ctr[2])^2)
        best <- cand[ok, , drop = FALSE][which.min(dctr[ok]), ]
      }

      lays[[i]] <- sweep(l, 2, best, "+")
      placed <- rbind(placed, lays[[i]])
    }
  }

  # --- 3. back to the vertex order of g ---
  lay <- do.call(rbind, lays)
  merged_names <- unlist(lapply(comps, function(cg) igraph::V(cg)$name))
  lay <- lay[match(igraph::V(g)$name, merged_names), , drop = FALSE]
  lay <- sweep(lay, 2, colMeans(lay))
  rownames(lay) <- igraph::V(g)$name
  lay
}


# ------------------------------------------------------------------
# Edge lengths for signed weighted networks (SAN, SMON).
#
# Maps balance linearly onto [dist_range[1], dist_range[2]]: the most
# positive tie gets the shortest length, the most negative the longest.
# Bounding the ratio (default 1:3) keeps strongly positive pairs from
# collapsing onto each other while preserving the ordering of ties.
# ------------------------------------------------------------------
balance_to_distance <- function(balance, dist_range = c(1, 3)) {
  stopifnot(length(dist_range) == 2, dist_range[1] > 0, dist_range[2] >= dist_range[1])
  span <- max(balance) - min(balance)
  if (span == 0) return(rep(mean(dist_range), length(balance)))
  dist_range[2] - (balance - min(balance)) / span * diff(dist_range)
}
