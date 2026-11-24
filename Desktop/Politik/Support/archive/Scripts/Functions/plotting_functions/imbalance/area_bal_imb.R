area_bal_imb <- function(imb, path) {
  
  line1$aes_params[["colour"]] <- "black"
  line2$aes_params[["colour"]] <- "black"
  
  p <- unique(imb[, .(count_bal = count_bal / count_all,
                      count_imb = count_imb / count_all,
                      year)]) |> melt(id.vars = "year") |>
    ggplot(aes(x = year, y = value, fill = variable)) +
    geom_area(position = "fill", col = "black") +
    scale_fill_manual(values = c("count_bal" = "#F8766D", 
                                 "count_imb" = "#00BFCA"),
                      labels = c("count_bal" = "Balanced Triangles",
                                 "count_imb" = "Imbalanced Triangles")) +
    #  scale_y_log10() +
    line1 + line2 +
    labs(x = "Year", y = "% of Total Triangles", fill = "") +
    theme_minimal(base_size = 18)
  
  save_interactive(p, path)
}
