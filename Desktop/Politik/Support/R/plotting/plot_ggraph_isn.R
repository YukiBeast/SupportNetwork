# ------------------------------------------------------------------
# ISN: signed two-mode network of support providers and conflict dyads.
#
# isn_edges: rows of the ISN, already filtered to the year(s) to plot,
#            e.g. nocoalitions[year == 1985]. Needs ext_name, dyad_id,
#            actor_nonstate (0 = government supported, 1 = rebels supported).
#
# Actors: cyan circles, sized by degree. Conflict dyads: orange squares.
# Ties: teal (#44AA99) = government support, olive (#999933) = rebel support.
#
# requires: layout_stress_radial(), get_iso3(), save_interactive()
# ------------------------------------------------------------------
plot_ggraph_isn <- function(isn_edges, path = NULL,
                            show_labels = TRUE, label_size = 4,
                            stretch = 1,
                            width = 10, height = 8,           # pdf size in inches
                            caption = "No caption provided",
                            log_file = "plot_tracker.Rmd") {

  # --- 1. edges: one tie per actor, dyad and supported side ---
  edges <- unique(as.data.table(isn_edges)[
    !is.na(ext_name),
    .(from = ext_name,
      to   = paste0("dyad_", dyad_id),
      side = fifelse(actor_nonstate == 0, "government", "rebel"))
  ])

  # --- 2. nodes: the two modes ---
  actors    <- unique(edges$from)
  conflicts <- unique(edges$to)
  nodes <- data.table(
    name = c(actors, conflicts),
    type = c(rep("actor", length(actors)), rep("conflict", length(conflicts)))
  )

  g <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)

  # --- 3. node attributes ---
  deg <- degree(g)
  V(g)$size_val <- ifelse(V(g)$type == "actor", deg, min(deg[V(g)$type == "actor"]))
  V(g)$label    <- ifelse(V(g)$type == "actor", get_iso3(V(g)$name), "")

  # --- 4. layout ---
  lay <- layout_stress_radial(g, stretch = stretch)

  # --- 5. plot ---
  p <- ggraph(g, layout = lay) +
    geom_edge_link(aes(color = side), alpha = 0.6, width = 0.5) +
    scale_edge_color_manual(
      values = c(government = "#44AA99", rebel = "#999933"),
      guide = "none"
    ) +
    geom_node_point(
      aes(size = size_val, fill = type, shape = type),
      color = "black", stroke = 0.6, alpha = 0.85
    ) +
    scale_shape_manual(values = c(actor = 21, conflict = 22), guide = "none") +
    scale_fill_manual(values = c(actor = "cyan3", conflict = "orange3"), guide = "none") +
    scale_size_continuous(range = c(2, 12), guide = "none") +
    theme_void()

  if (show_labels) {
    p <- p + geom_node_text(aes(label = label), repel = TRUE,
                            size = label_size, fontface = "bold")
  }

  # --- 6. show / save ---
  # the plot is always displayed: directly if no path is given,
  # otherwise save_interactive() prints it first and then asks whether to save
  if (is.null(path)) print(p) else save_interactive(p, path, caption, log_file,
                                                     width = width, height = height)
  invisible(p)
}
