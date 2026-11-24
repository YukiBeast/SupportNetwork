# ------------------------------------------------------------------
# Period SAN: signed weighted one-mode projection of the period ISN.
#
# - one row per actor pair
# - each shared conflict (dyad) counted once per period, regardless
#   of how many years the interventions lasted
# - balance = same-side conflicts - opposite-side conflicts
# - pairs with balance 0 (ambivalent balanced) are dropped: they carry
#   no information for signed modularity, and actors whose ties are
#   all zero would otherwise receive a degenerate, near-zero row in Z
#
# requires: load_san_window(), compute_dyadic_relationships_window()
# ------------------------------------------------------------------
build_period_san <- function(isn, period) {
  sw <- load_san_window(isn[year %in% period])
  sw <- sw[Weight != 0, .(state1, state2, balance = Weight)]
  stopifnot(!anyDuplicated(sw[, .(state1, state2)]))   # exactly one edge per pair
  sw
}
