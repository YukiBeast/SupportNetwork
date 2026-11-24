source("Setup.R")

# -------- figure 3
# -------- no coalitions ---------

support[ext_coalition != 1, .(total_ties = sum(actor_nonstate == 0, na.rm = TRUE) + sum(actor_nonstate == 1, na.rm = TRUE),
                                 gov_ties = sum(actor_nonstate == 0, na.rm = TRUE),
                                 non_gov_ties = sum(actor_nonstate == 1, na.rm = TRUE)),
        by = "year"] |>
  ggplot() +
  ## all ties ##
  geom_point(aes(x = year,
                 y = total_ties,
                 col = "Total Support Ties")) +
  geom_line(aes(x = year,
                y = total_ties,
                col = "Total Support Ties"),
            linewidth = 0.8) +
  geom_smooth(aes(x = year,
                  y = total_ties,
                  col = "Total Support Ties"),
              linewidth = 0.6,
              se = FALSE) +
  ## only gov ties ##
  geom_point(aes(x = year,
                 y = gov_ties,
                 col = "Government Support Ties")) +
  geom_line(aes(x = year,
                y = gov_ties,
                col = "Government Support Ties"),
            linewidth = 0.8) +
  geom_smooth(aes(x = year,
                  y = gov_ties,
                  col = "Government Support Ties"),
              linewidth = 0.6,
              se = FALSE) +
  ## only rebel ties ##
  geom_point(aes(x = year,
                 y = non_gov_ties,
                 col = "Rebels Support Ties")) +
  geom_line(aes(x = year,
                y = non_gov_ties,
                col = "Rebels Support Ties"),
            linewidth = 0.8) +
  geom_smooth(aes(x = year,
                  y = non_gov_ties,
                  col = "Rebels Support Ties"),
              linewidth = 0.6,
              se = FALSE) +
  scale_color_manual(values = c("Total Support Ties" = "black",
                                "Government Support Ties" = "aquamarine4",
                                "Rebels Support Ties" = "red4"),
                     name = NULL) +
  geom_vline(xintercept = 1991,
             col = "darkgray",
             linetype = "dashed",
             linewidth = 0.6) +
  geom_vline(xintercept = 2001,
             col = "darkgray",
             linetype = "dashed",
             linewidth = 0.6) +
  scale_y_continuous(limits = c(0, 600)) +
  labs(x = "Year",
       y = "Count") +
  theme_minimal(base_size = 28)

ggsave("Results/Figures/support_ties_nc.pdf", width = 20, height = 16)


# ---------- with coalitions --------------

support[, .(total_ties = sum(actor_nonstate == 0, na.rm = TRUE) + sum(actor_nonstate == 1, na.rm = TRUE),
                                 gov_ties = sum(actor_nonstate == 0, na.rm = TRUE),
                                 non_gov_ties = sum(actor_nonstate == 1, na.rm = TRUE)),
        by = "year"] |>
  ggplot() +
  ## all ties ##
  geom_point(aes(x = year,
                 y = total_ties,
                 col = "Total Support Ties")) +
  geom_line(aes(x = year,
                y = total_ties,
                col = "Total Support Ties"),
            linewidth = 0.8) +
  geom_smooth(aes(x = year,
                  y = total_ties,
                  col = "Total Support Ties"),
              linewidth = 0.6,
              se = FALSE) +
  ## only gov ties ##
  geom_point(aes(x = year,
                 y = gov_ties,
                 col = "Government Support Ties")) +
  geom_line(aes(x = year,
                y = gov_ties,
                col = "Government Support Ties"),
            linewidth = 0.8) +
  geom_smooth(aes(x = year,
                  y = gov_ties,
                  col = "Government Support Ties"),
              linewidth = 0.6,
              se = FALSE) +
  ## only rebel ties ##
  geom_point(aes(x = year,
                 y = non_gov_ties,
                 col = "Rebels Support Ties")) +
  geom_line(aes(x = year,
                y = non_gov_ties,
                col = "Rebels Support Ties"),
            linewidth = 0.8) +
  geom_smooth(aes(x = year,
                  y = non_gov_ties,
                  col = "Rebels Support Ties"),
              linewidth = 0.6,
              se = FALSE) +
  scale_color_manual(values = c("Total Support Ties" = "black",
                                "Government Support Ties" = "aquamarine4",
                                "Rebels Support Ties" = "red4"),
                     name = NULL) +
  geom_vline(xintercept = 1991,
             col = "darkgray",
             linetype = "dashed",
             linewidth = 0.6) +
  geom_vline(xintercept = 2001,
             col = "darkgray",
             linetype = "dashed",
             linewidth = 0.6) +
  scale_y_continuous(limits = c(0, 600)) +
  labs(x = "Year",
       y = "Count") +
  theme_minimal(base_size = 28)

ggsave("Results/Figures/support_ties_wc.pdf", width = 20, height = 16)
