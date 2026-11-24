# ------------------------------------------------------------------
# Community overview across periods (fig:comparison communities).
#
# comm: consensus communities of all periods, with columns
#       actors, community, prob (membership weight), epoch, ext_nonstate
#
# One panel per period: bars = communities ordered by size, split into
# state (light gray) and non-state (dark gray) members; the number above
# each bar is the community's average membership weight. The panel title
# gives n (actors), K (communities) and ANW (average node weight).
#
# requires: save_interactive()
# ------------------------------------------------------------------
plot_communities_epochs <- function(comm, path = NULL,
                                    epoch_order = NULL,
                                    width = 14, height = 9,
                                    caption = "No caption provided",
                                    log_file = "plot_tracker.Rmd") {

  dt <- as.data.table(comm)[!is.na(community)]
  dt[, community := as.character(community)]

  # --- 1. statistics per community and per period ---
  comm_stats <- dt[, .(
    size           = .N,
    mean_w         = mean(prob),
    share_nonstate = if (all(is.na(ext_nonstate))) 0 else mean(ext_nonstate == 1, na.rm = TRUE)
  ), by = .(epoch, community)]

  epoch_stats <- dt[, .(n = .N, K = uniqueN(community), anw = mean(prob)), by = epoch]
  epoch_stats[, facet_label := sprintf("%s\nn = %d, K = %d, ANW = %.3f", epoch, n, K, anw)]

  if (is.null(epoch_order)) epoch_order <- sort(unique(dt$epoch))
  epoch_stats[, facet_label := factor(facet_label,
                                      levels = facet_label[match(epoch_order, epoch)])]

  comm_stats <- epoch_stats[comm_stats, on = "epoch"]
  setorder(comm_stats, epoch, -size, -mean_w)
  comm_stats[, rank := as.integer(community)]          # communities are numbered by size

  # --- 2. bars split into state / non-state members ---
  bars <- melt(
    comm_stats[, .(facet_label, rank,
                   state    = size * (1 - share_nonstate),
                   nonstate = size * share_nonstate)],
    id.vars = c("facet_label", "rank"),
    variable.name = "type", value.name = "count"
  )

  # --- 3. plot ---
  p <- ggplot(bars, aes(x = factor(rank), y = count, fill = type)) +
    geom_col(width = 0.8) +
    geom_text(data = comm_stats,
              aes(x = factor(rank), y = size, label = sprintf("%.2f", mean_w)),
              inherit.aes = FALSE, vjust = -0.4, size = 4) +
    facet_wrap(~ facet_label, ncol = 2, scales = "free_x") +
    scale_fill_manual(values = c(state = "gray80", nonstate = "gray35"),
                      labels = c(state = "State", nonstate = "Non-state"),
                      name = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(x = "Community (numbered by size)", y = "Number of actors") +
    theme_minimal(base_size = 20) +
    theme(legend.position    = "bottom",
          panel.grid.minor   = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.text         = element_text(face = "bold"))

  if (is.null(path)) print(p) else save_interactive(p, path, caption, log_file,
                                                     width = width, height = height)
  invisible(comm_stats)
}


# ------------------------------------------------------------------
# Appendix list of community members (app: community members) as LaTeX.
#
# Writes one block per period: communities grouped by size, members in
# alphabetical order with their membership weight. Include it in the
# paper with \input{...}.
# ------------------------------------------------------------------
write_community_members_tex <- function(comm, path, epoch_order = NULL) {

  dt <- as.data.table(comm)[!is.na(community)]
  dt[, community := as.character(community)]
  if (is.null(epoch_order)) epoch_order <- sort(unique(dt$epoch))

  esc <- function(x) gsub("([&%$#_{}])", "\\\\\\1", x)   # escape LaTeX specials

  lines <- c("Community members in each period. Parentheses show the membership weight $w_i$.\\\\")

  for (e in epoch_order) {
    de    <- dt[epoch == e]
    sizes <- de[, .N, by = community][order(-N)]
    lines <- c(lines, sprintf("\\textbf{Communities %s:}\\\\", gsub("-", "--", e)))

    for (sz in unique(sizes$N)) {
      lines <- c(lines,
                 sprintf("%d %s:", sz, ifelse(sz == 1, "member", "members")),
                 "\\begin{itemize}[noitemsep, topsep=0pt]")
      for (c_id in sort(as.numeric(sizes[N == sz, community]))) {
        m <- de[community == as.character(c_id)][order(actors)]
        lines <- c(lines, paste0("    \\item \\textit{C", c_id, ":} ",
                                 paste(sprintf("%s (%.3f)", esc(m$actors), m$prob),
                                       collapse = ", ")))
      }
      lines <- c(lines, "\\end{itemize}")
    }
    
    un <- as.data.table(comm)[epoch == e & is.na(community)][order(actors)]
    if (nrow(un) > 0) {
      lines <- c(lines, paste0("Unassigned: ", paste(esc(un$actors), collapse = ", "), "\\\\"))
    }
    
    lines <- c(lines, "")
  }

  con <- file(path, open = "w", encoding = "UTF-8")
  writeLines(lines, con)
  close(con)
  message("Community member list written to: ", path)
}
