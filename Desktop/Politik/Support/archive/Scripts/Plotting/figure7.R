source("Setup.R")

# -------- figure 7
# -------- competitive interventions ---------



# 4. Plot Relationship
# first impression
network_support[rn == "Government of Russia (Soviet Union)", 
                .(year, `Government of United States of America`)] |>
  ggplot() +
  geom_line(aes(x = year, y = `Government of United States of America`)) +
  labs(title = "USA - USSR/Russia Intervention Relationship",
       y = "Relationship Score (A * A^T)", 
       x = "Year") +
  theme_minimal()

# refined pictures
network_support |>
  plot_relationship("Government of United States of America",
                                "Government of Russia (Soviet Union)")
ggsave("Results/Figures/usa_rus_relation.png", width = 24, height = 16)

network_support |>
  plot_relationship("Government of Cuba",
                    "Government of Russia (Soviet Union)")
ggsave("Results/Figures/cub_rus_relation.png", width = 24, height = 16)

network_support |>
  plot_relationship("Government of United States of America",
                    "Government of China")

ggsave("Results/Figures/usa_chn_relation.png", width = 24, height = 16)

network_support |>
  plot_relationship("Government of Russia (Soviet Union)",
                    "Government of China")

ggsave("Results/Figures/rus_chn_relation.png", width = 24, height = 16)

network_support |>
  plot_relationship("Government of United States of America",
                    "Government of Iran")

ggsave("Results/Figures/usa_irn_relation.png", width = 24, height = 16)
