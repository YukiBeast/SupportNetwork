# ------------------------------------------------------------------
# SMON communities on the period SAN.
#
# san_period:  one row per actor pair, e.g. san_periods[["1975-1991"]]
#              (state1, state2, balance)
# communities: consensus result for the same period, with columns
#              actors, community, prob (membership weight w_i)
#
# Node fill = community, node opacity = membership weight (absolute
# scale, comparable across periods), node size = degree.
# Ties: green = positive, red = negative; ties between communities are
# drawn fainter than ties within communities.
#
# community_pull (0 to <1): shortens edge lengths within communities and
# lengthens them between communities in the layout, so communities form
# visibly separate groups. 0 = layout ignores the communities.
# top_n: colour only the top_n communities (priority communities first,
# then by size); all others are gray. With number_other = TRUE the labels
# of gray actors carry their community number, e.g. "TUR (7)", so gray
# never suggests shared membership.
# show_hulls: draws a shaded hull around every community with at least
# hull_min_size members (requires the ggforce package).
#
# requires: layout_stress_radial(), get_iso3(), save_interactive()
# ------------------------------------------------------------------
plot_ggraph_smon <- function(san_period, communities, path = NULL,
                             top_n = NULL, min_comm_size = 0,
                             priority_nodes = NULL,
                             show_labels = TRUE, label_size = 4,
                             show_legend = FALSE,
                             keep_largest_component = TRUE,
                             stretch = 0.6,
                             dist_range = c(1, 3),              # edge length: most positive : most negative
                             community_pull = 0.3,              # 0 = ignore communities in layout
                             number_other = TRUE,
                             show_hulls = FALSE, hull_min_size = 3,
                             edge_alpha = c(within = 0.35, between = 0.08),
                             width = 10, height = 8,           # pdf size in inches
                             caption = "No caption provided",
                             log_file = "plot_tracker.Rmd") {

  # --- 1. nodes with a community, and the edges among them ---
  comm <- as.data.table(communities)[
    !is.na(community),
    .(name = actors, community = as.character(community), memb_weight = prob)
  ]
  edges <- as.data.table(san_period)[
    state1 %in% comm$name & state2 %in% comm$name,
    .(state1, state2, balance, sign = sign(balance))
  ]
  comm <- comm[name %in% c(edges$state1, edges$state2)]

  g <- graph_from_data_frame(edges, directed = FALSE, vertices = comm)
  if (any_multiple(g)) stop("More than one edge per actor pair: use the period SAN.")

  # --- 2. largest connected component ---
  if (keep_largest_component) {
    comp <- components(g)
    g <- induced_subgraph(g, V(g)[comp$membership == which.max(comp$csize)])
  }

  # --- 3. layout on the full graph (before any community filtering) ---
  stopifnot(community_pull >= 0, community_pull < 1)
  same_comm <- function(g) {
    ends_idx <- ends(g, E(g), names = FALSE)
    V(g)$community[ends_idx[, 1]] == V(g)$community[ends_idx[, 2]]
  }
  dist <- balance_to_distance(E(g)$balance, dist_range)
  dist <- dist * ifelse(same_comm(g), 1 - community_pull, 1 + community_pull)
  lay  <- layout_stress_radial(g, distance = dist, stretch = stretch)

  # --- 4. drop small communities, protecting priority actors ---
  if (min_comm_size > 0) {
    sizes <- table(V(g)$community)
    keep  <- names(sizes)[sizes >= min_comm_size]
    if (!is.null(priority_nodes)) {
      keep <- union(keep, V(g)$community[V(g)$name %in% priority_nodes])
    }
    g <- induced_subgraph(g, V(g)[community %in% keep])
  }
  lay <- lay[V(g)$name, , drop = FALSE]

  # --- 5. node attributes ---
  V(g)$degree    <- degree(g)
  V(g)$label     <- get_iso3(V(g)$name)
  V(g)$alpha_val <- pmin(pmax(V(g)$memb_weight, 0.15), 1)   # absolute, not rescaled per plot
  V(g)$comm_size <- as.numeric(table(V(g)$community)[V(g)$community])
  E(g)$edge_a    <- ifelse(same_comm(g), edge_alpha[["within"]], edge_alpha[["between"]])

  # --- 6. community colours ---
  sizes     <- sort(table(V(g)$community), decreasing = TRUE)
  all_comms <- names(sizes)

  if (!is.null(top_n)) {
    priority_comms <- unique(V(g)$community[V(g)$name %in% priority_nodes])
    ordered   <- c(priority_comms, setdiff(all_comms, priority_comms))
    top_comms <- head(ordered, max(top_n, length(priority_comms)))
  } else {
    top_comms <- all_comms
  }

  legend_labels <- vapply(top_comms, function(c_id) {
    members <- sort(get_iso3(V(g)$name[V(g)$community == c_id]))
    paste0("Comm. ", c_id, " (", paste(members, collapse = ", "), ")")
  }, character(1))

  if (number_other) {
    is_other <- !(V(g)$community %in% top_comms)
    V(g)$label[is_other] <- paste0(V(g)$label[is_other], " (", V(g)$community[is_other], ")")
  }
  
  other <- "Other"
  V(g)$legend_group <- ifelse(V(g)$community %in% top_comms,
                              legend_labels[V(g)$community], other)
  V(g)$legend_group <- factor(V(g)$legend_group, levels = c(legend_labels, other))

  if (!is.null(top_n)) {
    # Okabe-Ito palette (colourblind-safe), without black
    base_palette <- c("#0072B2", "#D55E00", "#CC79A7", "#E69F00",
                      "#009E73", "#56B4E9", "#F0E442")
    if (length(top_comms) > length(base_palette)) {
      warning("More than 7 highlighted communities: colours are recycled.")
    }
    comm_cols <- rep(base_palette, length.out = length(top_comms))
  } else {
    comm_cols <- scales::hue_pal()(length(top_comms))   # ggplot default, as before
  }
  palette <- setNames(c(comm_cols, "gray80"), c(legend_labels, other))

  # --- 7. plot ---
  p <- ggraph(g, layout = lay)
  
  if (show_hulls) {
    if (!requireNamespace("ggforce", quietly = TRUE)) stop("show_hulls = TRUE needs the ggforce package.")
    p <- p + ggforce::geom_mark_hull(
      data = function(d) d[d$comm_size >= hull_min_size, ],
      aes(x = x, y = y, group = community, fill = legend_group),
      alpha = 0.10, colour = NA, expand = grid::unit(4, "mm"), concavity = 3
    )
  }
  
  p <- p +
    geom_edge_link(aes(color = factor(sign, levels = c(1, -1, 0)), edge_alpha = edge_a),
                   width = 0.3) +
    scale_edge_alpha_identity() +
    scale_edge_color_manual(
      values = c("1" = "#117733", "-1" = "#661100", "0" = "#888888"),
      guide = "none"
    ) +
    # white underlay so transparent nodes do not show edges through them
    geom_node_point(aes(size = degree), shape = 21, fill = "white",
                    color = "white", stroke = 0.6) +
    geom_node_point(aes(size = degree, fill = legend_group, alpha = alpha_val),
                    shape = 21, color = "black", stroke = 0.6) +
    scale_fill_manual(values = palette, drop = TRUE,
                      name = "Communities & Members") +
    scale_alpha_identity() +
    scale_size_continuous(range = c(2, 14), guide = "none") +
    theme_void()

  if (show_legend) {
    p <- p + theme(legend.position = "bottom",
                   legend.direction = "vertical",
                   legend.title = element_text(face = "bold", size = 10),
                   legend.text  = element_text(size = 9)) +
      guides(fill = guide_legend(override.aes = list(size = 4)))
  } else {
    p <- p + theme(legend.position = "none")
  }

  if (show_labels) {
    p <- p + geom_node_text(aes(label = label), repel = TRUE,
                            size = label_size, fontface = "bold")
  }

  # --- 8. show / save ---
  # the plot is always displayed: directly if no path is given,
  # otherwise save_interactive() prints it first and then asks whether to save
  if (is.null(path)) print(p) else save_interactive(p, path, caption, log_file,
                                                     width = width, height = height)
  invisible(p)
}
