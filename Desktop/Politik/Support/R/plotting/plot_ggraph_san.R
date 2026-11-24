# ------------------------------------------------------------------
# SAN: signed weighted one-mode network of support providers.
#
# san_edges: one row per actor pair, already filtered, e.g.
#            san[year == 1985] (yearly) or san_periods[["1975-1991"]].
#            Needs state1, state2, balance.
#
# Ties: green = positive, red = negative, gray = nullified (balance 0);
#       width = absolute balance. Nodes sized by degree.
# Layout: positively related actors are pulled together, negatively
#         related actors pushed apart.
#
# requires: layout_stress_radial(), get_iso3(), save_interactive()
# ------------------------------------------------------------------
plot_ggraph_san <- function(san_edges, path = NULL,
                            show_labels = TRUE, label_size = 4,
                            keep_largest_component = FALSE,
                            stretch = 1,
                            dist_range = c(1, 3),              # edge length: most positive : most negative
                            width = 10, height = 8,           # pdf size in inches
                            caption = "No caption provided",
                            log_file = "plot_tracker.Rmd") {

  # --- 1. edges ---
  edges <- as.data.table(san_edges)[, .(state1, state2, balance,
                                        sign = sign(balance))]

  g <- graph_from_data_frame(edges, directed = FALSE)
  if (any_multiple(g)) stop("More than one edge per actor pair: aggregate san_edges first.")

  if (keep_largest_component) {
    comp <- components(g)
    g <- induced_subgraph(g, V(g)[comp$membership == which.max(comp$csize)])
  }

  # --- 2. node attributes ---
  V(g)$degree <- degree(g)
  V(g)$label  <- get_iso3(V(g)$name)

  # --- 3. layout: edge length shrinks with balance, bounded ratio ---
  dist <- balance_to_distance(E(g)$balance, dist_range)
  lay  <- layout_stress_radial(g, distance = dist, stretch = stretch)

  # --- 4. plot ---
  p <- ggraph(g, layout = lay) +
    geom_edge_link(
      aes(color = factor(sign, levels = c(1, -1, 0)), width = abs(balance)),
      alpha = 0.5
    ) +
    scale_edge_color_manual(
      values = c("1" = "#117733", "-1" = "#661100", "0" = "#888888"),
      guide = "none"
    ) +
    scale_edge_width_continuous(range = c(0.3, 2.5), guide = "none") +
    geom_node_point(
      aes(size = degree),
      shape = 21, fill = "cyan3", color = "black", stroke = 0.6, alpha = 0.85
    ) +
    scale_size_continuous(range = c(2, 14), guide = "none") +
    theme_void()

  if (show_labels) {
    p <- p + geom_node_text(aes(label = label), repel = TRUE,
                            size = label_size, fontface = "bold")
  }

  # --- 5. show / save ---
  # the plot is always displayed: directly if no path is given,
  # otherwise save_interactive() prints it first and then asks whether to save
  if (is.null(path)) print(p) else save_interactive(p, path, caption, log_file,
                                                     width = width, height = height)
  invisible(p)
}
