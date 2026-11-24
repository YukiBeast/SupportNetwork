# ------------------------------------------------------------------
# Pairwise relations of support providers over a whole period
# (one entry per actor pair and shared conflict).
#
# Two actors are related through a conflict only if they intervened in
# it in the same year at least once during the period. The conflict
# counts once, with the relation that held in the majority of those
# shared years:
#   more same-side years     -> one entry +1
#   more opposite-side years -> one entry -1
#   equal                    -> one entry +1 and one entry -1 (net 0)
# Sides are compared year by year, so an actor that switched side
# (e.g. after the faction it backed took power) is handled correctly.
#
# isn_window: ISN rows of the period with a supporter (needs year,
#             dyad_id, ext_name, signed_support).
# ------------------------------------------------------------------
compute_dyadic_relationships_window <- function(isn_window) {

  df <- unique(isn_window[, .(year, dyad_id, ext_name, signed_support)])
  stopifnot(!anyDuplicated(df[, .(year, dyad_id, ext_name)]))   # one side per actor, conflict and year

  # all pairs active in the same conflict in the same year
  yearly <- df[df, on = .(year, dyad_id), allow.cartesian = TRUE][ext_name < i.ext_name]
  yearly[, s := signed_support * i.signed_support]
  setnames(yearly, c("ext_name", "i.ext_name"), c("state1", "state2"))

  # one relation per pair and conflict: majority over the shared years
  co <- yearly[, .(net = sum(s)), by = .(dyad_id, state1, state2)]
  co[, .(sign = if (net > 0) 1 else if (net < 0) -1 else c(1, -1)),
     by = .(dyad_id, state1, state2)]
}
