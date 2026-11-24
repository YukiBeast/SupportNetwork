# ==================================================================
# 01_descriptives.R
# All figures of Section 4 (ISN and SAN descriptives).
# Output paths mirror the Overleaf folders Figures/ISN and Figures/SAN.
# ==================================================================

source("scripts/00_setup.R")


# ------------------------------------------------------------------
# 0. Sanity check: one row per actor, dyad, year and supported side?
#    (count_support_types_isn and most_interventionist count rows)
# ------------------------------------------------------------------
dupl <- isn[!is.na(ext_name), .N, by = .(year, dyad_id, ext_name, actor_nonstate)][N > 1]
if (nrow(dupl) > 0) {
  warning(nrow(dupl), " duplicated actor-dyad-year rows: row-based counts are inflated.")
}


# ==================================================================
# SECTION 4.1: ISN
# ==================================================================

# --- fig:ISN_1985 -------------------------------------------------
plot_ggraph_isn(
  isn[year == 1985],
  width = 12,
  height = 8,
  path    = "Figures/ISN/isn_1985.pdf",
  caption = "ISN 1985. Cyan circles: intervening actors (size = degree);
             orange squares: conflict dyads. Teal ties: government support,
             olive ties: rebel support."
)

# --- fig:ISN_nodes_ties (left) ------------------------------------
count_total_dyads_supporters(
  isn,
  path    = "Figures/ISN/total_dyads_supporters.pdf",
  caption = "Conflict dyads (salmon) and external supporters (cyan) per year."
)

# --- fig:ISN_nodes_ties (right) -----------------------------------
count_support_types_isn(
  isn,
  path    = "Figures/ISN/support_types_isn.pdf",
  caption = "Support ties per year: government (teal), rebel (olive), total (black)."
)

# --- fig:supportedside --------------------------------------------
support_abs <- count_support(isn, absolute = TRUE, coalitions = FALSE)

plot_support_configuration(
  support_abs,
  path    = "Figures/ISN/support_count_abs.pdf",
  share   = FALSE,
  caption = "Support configuration per conflict dyad: both sides (black),
             government only (teal triangles), rebels only (olive squares),
             no support (gray diamonds)."
)

# --- fig:nsa_support (left and right) -----------------------------
plot_nonstate_interventions(
  isn, share = FALSE,
  path    = "Figures/ISN/nonstate_vs_state.pdf",
  caption = "State (gray) and non-state (purple) support suppliers, count."
)

plot_nonstate_interventions(
  isn, share = TRUE,
  path    = "Figures/ISN/nonstate_vs_state_share.pdf",
  caption = "State (gray) and non-state (purple) support suppliers, share."
)

# --- fig:top3 -----------------------------------------------------
most_interventionist(
  isn,
  path = "Figures/ISN/most_interventionist.pdf"
)


# ==================================================================
# SECTION 4.2: SAN
# ==================================================================

# --- fig:SAN_1985 -------------------------------------------------
plot_ggraph_san(
  san[year == 1985],
  dist_range = c(1, 2.5),
  width = 12,
  height = 8,
  path    = "Figures/SAN/network_1985_san.pdf",
  caption = "Weighted SAN 1985. Green: positive, red: negative, gray: nullified
             relations; width = |balance|; node size = degree."
)

# --- fig:SAN_ties_relations (left) --------------------------------
plot_count_ties_san(
  disaggregated_san,
  path    = "Figures/SAN/count_ties_san.pdf",
  caption = "Multiplex SAN: joint (green), opposed (red) and total (black) ties per year."
)

# --- fig:SAN_ties_relations (right) -------------------------------
plot_relation_types_san(
  san,
  path    = "Figures/SAN/relation_types.pdf",
  caption = "Weighted SAN: positive (green), negative (red) and ambivalent (gray)
             relations per year."
)
