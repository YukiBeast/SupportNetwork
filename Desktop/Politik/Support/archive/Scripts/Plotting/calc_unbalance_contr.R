joined_imb[, clean_imb := (imbalance_total / imbalance_global)]
joined_imb[, clean_bal := (balance_total / balance_global) * count_bal]

ggplot(joined_imb, aes(x = year, y = clean_imb, fill = state)) +
  geom_area(position = "fill", col = "black") +
  labs(x = "Year", y = "Cumulative State-Share of Imbalanced Triangles",
        fill = "") +
  theme_minimal(base_size = 18)








  
