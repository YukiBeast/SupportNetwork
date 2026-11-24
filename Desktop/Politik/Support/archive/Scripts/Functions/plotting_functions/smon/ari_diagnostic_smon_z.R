library(data.table)

ari_diagnostic_smon_z <- function(random_smon,
                                   path,
                                   caption = "No caption provided",
                                   log_file = "plot_tracker.Rmd") {
  
  # hard-assign each final z
  clusters <- lapply(random_smon$all_z, function(z) {
    zn <- z / pmax(rowSums(z), 1e-10)
    max.col(zn, ties.method = "first")
  })
  
  # pairwise Adjusted Rand Index
  n <- length(clusters)
  ari <- matrix(NA, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      ari[i, j] <- igraph::compare(clusters[[i]], clusters[[j]],
                                   method = "adjusted.rand")
    }
  }
  
  ari_df <- as.data.table(ari)
  
  ari_long <- melt(ari_df, measure.vars = names(ari_df),
                   variable.name = "run_j", value.name = "ari")
  ari_long[, run_i := rep(seq_len(nrow(ari_df)), times = ncol(ari_df))]
  ari_long[, run_j := as.numeric(gsub("V", "", run_j))]
  
  p <- ggplot(ari_long, aes(x = run_i, y = run_j, fill = ari)) +
    geom_tile() +
    scale_fill_gradient2(low = "white", mid = "steelblue", high = "darkblue",
                         midpoint = 0.5, limits = c(0, 1),
                         name = "ARI") +
    coord_equal() +
    theme_minimal() +
    labs(x = "Run i", y = "Run j",
         title = "pairwise ARI across runs")
  
  save_interactive(p, path, caption, log_file)
}
ari_diagnostic_smon_z(random_smon)
