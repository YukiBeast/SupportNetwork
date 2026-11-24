source("Setup.R")

# -------- figure 4
# -------- ABSOLUTE ---------
# --------- with coalitions --------

library(data.table)

support_abs_wc <- count_support(support,
                                absolute = TRUE,
                                coalitions = TRUE)

figure4(support_abs_wc)

ggsave("Results/Figures/support_count_abs_wc.png", width = 20, height = 16)

# ------- without coalition --------
support_abs_nc <- count_support(support,
                                absolute = TRUE,
                                coalitions = FALSE)

figure4(support_abs_nc)

ggsave("Results/Figures/support_count_abs_nc.png", width = 20, height = 16)

# -------- PERCENT ---------
# --------- with coalitions --------

support_per_wc <- count_support(support,
                                absolute = FALSE,
                                coalitions = TRUE)

figure4(support_per_wc)

ggsave("Results/Figures/support_count_per_wc.png", width = 20, height = 16)

# --------- without coalitions --------

support_per_nc <- count_support(support,
                                absolute = FALSE,
                                coalitions = FALSE)

figure4(support_per_nc)

ggsave("Results/Figures/support_count_per_nc.png", width = 20, height = 16)


# ------ interventions per conflict ------------

conflict_support <- support[, .(no_support = all(ext_sup == 0),
                                gov_support = all(ext_sup == 1 & actor_nonstate == 0),
                                rebel_support = all(ext_sup == 1 & actor_nonstate == 1)),
                            by = c("year", "dyad_id")][,
                                                       .(year,
                                                         dyad_id,
                                                         no_support,
                                                         gov_support,
                                                         rebel_support,
                                                         mixed_support = as.numeric(no_support == 0 &
                                                                                      gov_support == 0 &
                                                                                      rebel_support == 0))][,
                                                                                                            .(no_support = sum(no_support),
                                                                                                              gov_support = sum(gov_support),
                                                                                                              rebel_support = sum(rebel_support),
                                                                                                              mixed_support = sum(mixed_support)),
                                                                                                            by = "year"
                                                                                      ]
ggplot(data = conflict_support, aes(x = year)) +

  geom_point(aes(y = no_support,
                 shape = "No Support"),
             col = "azure3") +
  geom_smooth(aes(y = no_support),
                  col = "azure3",
              se = FALSE) +

  geom_point(aes(y = gov_support,
                 shape = "Government Support"),
             col = "bisque4") +
  geom_smooth(aes(y = gov_support),
                  col = "bisque4",
              se = FALSE) +

  geom_point(aes(y = mixed_support,
                 shape = "Competitive Intervention"),
             col = "black") +
  geom_smooth(aes(y = mixed_support),
                  col = "black",
              se = FALSE) +

  geom_point(aes(y = rebel_support,
                 shape = "Rebel Support"),
             col = "darkgoldenrod4") +
  geom_smooth(aes(y = rebel_support),
                  col = "darkgoldenrod4",
              se = FALSE) +
  scale_shape_manual(values = c("Competitive Intervention" = 16,
                              "Government Support" = 17,
                              "Rebel Support" = 15,
                              "No Support" = 18),
                   name = NULL) +
  geom_vline(xintercept = 1991,
             col = "darkgray",
             linetype = "dashed",
             linewidth = 0.6) +
  geom_vline(xintercept = 2001,
             col = "darkgray",
             linetype = "dashed",
             linewidth = 0.6) +
  labs(y = "# of Conflicts",
       x = "Year") +
  theme_minimal(base_size = 28) +
  theme(legend.position = "bottom",
        legend.byrow = TRUE)


ggsave("Results/Figures/conflict_support_abs.png", width = 20, height = 16)

conflict_support[, .({total = sum(no_support,
                                  gov_support,
                                  mixed_support,
                                  rebel_support)},
                     no_support = no_support / total,
                     gov_support = gov_support / total,
                     mixed_support = mixed_support / total,
                     rebel_support = rebel_support / total
                     ), by = "year"] |>
  ggplot(aes(x = year)) +
  geom_point(aes(y = no_support,
                 shape = "No Support"),
             col = "azure3") +
  geom_smooth(aes(y = no_support),
              col = "azure3",
              se = FALSE) +
  
  geom_point(aes(y = gov_support,
                 shape = "Government Support"),
             col = "bisque4") +
  geom_smooth(aes(y = gov_support),
              col = "bisque4",
              se = FALSE) +
  
  geom_point(aes(y = mixed_support,
                 shape = "Competitive Intervention"),
             col = "black") +
  geom_smooth(aes(y = mixed_support),
              col = "black",
              se = FALSE) +
  
  geom_point(aes(y = rebel_support,
                 shape = "Rebel Support"),
             col = "darkgoldenrod4") +
  geom_smooth(aes(y = rebel_support),
              col = "darkgoldenrod4",
              se = FALSE) +
  scale_shape_manual(values = c("Competitive Intervention" = 16,
                                "Government Support" = 17,
                                "Rebel Support" = 15,
                                "No Support" = 18),
                     name = NULL) +
  geom_vline(xintercept = 1991,
             col = "darkgray",
             linetype = "dashed",
             linewidth = 0.6) +
  geom_vline(xintercept = 2001,
             col = "darkgray",
             linetype = "dashed",
             linewidth = 0.6) +
  labs(y = "% of Conflicts",
       x = "Year") +
  theme_minimal(base_size = 28) +
  theme(legend.position = "bottom",
        legend.byrow = TRUE)

ggsave("Results/Figures/conflict_support_percent.png", width = 20, height = 16)
