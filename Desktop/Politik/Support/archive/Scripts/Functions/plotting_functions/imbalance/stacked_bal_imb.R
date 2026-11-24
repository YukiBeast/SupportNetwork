stacked_bal_imb <- function(imb, path) {
  p <- unique(imb[, .(year, count_bal, count_imb)]) |> melt(id.vars = "year") |>
    ggplot(aes(x = year, y = value, fill = variable)) +
    geom_area(col = "black") +
    scale_fill_manual(values = c("count_bal" = "#F8766D", 
                                 "count_imb" = "#00BFCA"),
                      labels = c("count_bal" = "Balanced Triangles",
                                 "count_imb" = "Imbalanced Triangles")) +
    labs(x = "Year", y = "Total Triangles (Log Scale)", fill = "") +
    scale_y_log10() +
    theme_minimal(base_size = 18)
  
  save_interactive(p, path)
}
