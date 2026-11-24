source("Setup.R")

# -------- figure 2
# -------- no coalitions ---------

support[ext_coalition != 1 & !is.na(ext_name), .(total_dyads = length(unique(dyad_id)),
                                                 total_ext_supporters = length(unique(ext_name))),
        by = "year"] |>
  ggplot() +
  geom_point(aes(x = year, y = total_dyads,
                 col = "Dyads")) +
  geom_line(aes(x = year, y = total_dyads,
                col = "Dyads"),
            linewidth = 0.8) +
  geom_smooth(aes(x = year, y = total_dyads,
                  col = "Dyads"),
              linewidth = 0.6,
              se = FALSE) +
  geom_point(aes(x = year, y = total_ext_supporters,
                 col = "External Supporters")) +
  geom_line(aes(x = year, y = total_ext_supporters,
                col = "External Supporters"),
            linewidth = 0.8) +
  geom_smooth(aes(x = year, y = total_ext_supporters,
                  col = "External Supporters"),
              linewidth = 0.6,
              se = FALSE) +
  scale_color_manual(values = c("Dyads" = "salmon",
                                "External Supporters" = "cyan3"),
                     name = NULL) +
  geom_vline(xintercept = 1991,
             col = "darkgray",
             linetype = "dashed",
             linewidth = 0.6) +
  geom_vline(xintercept = 2001,
             col = "darkgray",
             linetype = "dashed",
             linewidth = 0.6) +
  labs(x = "Year",
       y = "Count") +
  scale_y_continuous(limits = c(0, 160)) +
  theme_minimal(base_size = 28)

ggsave("Results/Figures/total_dyads_supporters_nc.png", width = 20, height = 16)

# ---------- with coalitions --------------

support[!is.na(ext_name), .(total_dyads = length(unique(dyad_id)),
                                                    total_ext_supporters = length(unique(ext_name))),
        by = "year"] |>
  ggplot() +
  geom_point(aes(x = year, y = total_dyads,
                 col = "Dyads")) +
  geom_line(aes(x = year, y = total_dyads,
                col = "Dyads"),
            linewidth = 0.8) +
  geom_smooth(aes(x = year, y = total_dyads,
                  col = "Dyads"),
              linewidth = 0.6,
              se = FALSE) +
  geom_point(aes(x = year, y = total_ext_supporters,
                 col = "External Supporters")) +
  geom_line(aes(x = year, y = total_ext_supporters,
                col = "External Supporters"),
            linewidth = 0.8) +
  geom_smooth(aes(x = year, y = total_ext_supporters,
                  col = "External Supporters"),
              linewidth = 0.6,
              se = FALSE) +
  scale_color_manual(values = c("Dyads" = "salmon",
                                "External Supporters" = "cyan3"),
                     name = NULL) +
  geom_vline(xintercept = 1991,
             col = "darkgray",
             linetype = "dashed",
             linewidth = 0.6) +
  geom_vline(xintercept = 2001,
             col = "darkgray",
             linetype = "dashed",
             linewidth = 0.6) +
  scale_y_continuous(limits = c(0, 160)) +
  labs(x = "Year",
       y = "Count") +
  theme_minimal(base_size = 28)

ggsave("Results/Figures/total_dyads_supporters_wc.png", width = 20, height = 16)



