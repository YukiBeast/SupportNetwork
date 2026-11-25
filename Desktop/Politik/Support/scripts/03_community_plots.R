# ==================================================================
# 03_community_plots.R
# Section 5 and appendix: everything based on the SMON results.
# Requires the output of 02_smon.R in Data/.
# ==================================================================

source("scripts/00_setup.R")


# ------------------------------------------------------------------
# 0. Load SMON results
# ------------------------------------------------------------------
san_periods_smon <- readRDS("Data/results/epochs_san.rds")              # networks SMON ran on
avg_epochs       <- readRDS("Data/results/epochs_modularity_avg.rds")   # modularity curves
k_star           <- readRDS("Data/results/epochs_k_star.rds")
sd_comparison    <- readRDS("Data/results/epochs_sd_comparison.rds")
comm             <- as.data.table(readRDS("Data/results/epochs_communities_avg.rds"))
# actors with no membership in most runs are unassigned
zero_cut <- 0.75
comm[zero_share > zero_cut, community := NA_integer_]

# renumber communities by size within each period (1 = largest), so plots,
# overview figure and appendix list all use the same numbers
comm[!is.na(community), size := .N, by = .(epoch, community)]
comm[!is.na(community),
     community := match(community, unique(community[order(-size, community)])),
     by = epoch]
comm[, size := NULL]

# the period networks must be the ones SMON was estimated on
if (!isTRUE(all.equal(san_periods_smon, san_periods))) {
  warning("Period SANs differ from those used in 02_smon.R: re-run 02_smon.R.")
}

out <- "Figures/Community_Detection/"


# ------------------------------------------------------------------
# 1. Numbers for the text
# ------------------------------------------------------------------
cat("\nk* per period:\n");                       print(k_star)
cat("\nSD of Q_d at its maximum vs. at k* ([X] in Section 5.1):\n"); print(sd_comparison)

epoch_summary <- comm[!is.na(community), .(
  n_actors     = .N,
  K            = uniqueN(community),
  ANW          = round(mean(prob), 3),
  largest_comm = max(table(community))
), by = epoch]
epoch_summary[, unassigned := comm[is.na(community), .N, by = epoch][
  epoch_summary, on = "epoch", fcoalesce(N, 0L)]]
cat("\nUnassigned actors:\n")
print(comm[is.na(community), .(epoch, actors, prob, zero_share)][order(epoch, actors)])
cat("\nActors with zero membership in at least one run:\n")
print(comm[zero_share > 0, .N, by = epoch])


# ------------------------------------------------------------------
# 2. fig:Number Communities (appendix): modularity curves and k*
# ------------------------------------------------------------------
plot_normalized_modularity(
  avg_epochs,
  path    = paste0(out, "Q_comparison.pdf"),
  caption = "Min-max normalized continuous (green) and discrete (gold) modularity,
             mean +/- 1 SD across runs; dashed line: k*."
)


# ------------------------------------------------------------------
# 3. fig:SMON_1975_1991 and appendix network plots
# ------------------------------------------------------------------
for (e in names(epochs)) {
  plot_ggraph_smon(
    san_periods_smon[[e]],
    comm[epoch == e],
    min_comm_size  = 2,
    priority_nodes = c("United States of America", "Russia (Soviet Union)"),
    path    = paste0(out, "smon_community_net", e, ".pdf"),
    width   = 12, height = 8,
    edge_alpha = c(within = 0.1, between = 0.1),
    min_alpha = 0.3,
    top_n = 7,
    caption = paste0("SMON communities ", e, ". Fill = community (the largest 5 + the two containing USA and Russia),
    other actors (gray) have community ranking (by size) attached.
    opacity = membership, 
                     weight (floored at 0.15), size = degree. Communities with < 2
                     members and nodes outside the largest component omitted.")
  )
}


# ------------------------------------------------------------------
# 4. fig:comparison communities: number, size and cohesion
# ------------------------------------------------------------------
comm_stats <- plot_communities_epochs(
  comm,
  epoch_order = names(epochs),
  path    = paste0(out, "communities_all_epochs.pdf"),
  caption = "Communities per period ordered by size; light gray = state,
             dark gray = non-state members; number above bar = average
             membership weight. n = actors, K = communities, ANW = average
             node weight."
)


# ------------------------------------------------------------------
# 5. Appendix: list of community members as LaTeX
# ------------------------------------------------------------------
write_community_members_tex(
  comm,
  path        = paste0(out, "community_members.tex"),
  epoch_order = names(epochs)
)
