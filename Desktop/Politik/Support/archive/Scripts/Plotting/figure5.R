source("Setup.R")

# -------- figure 5
# -------- with coalitions ---------

support[, .(tot_conflicts_per_actor = length(unique(.SD$dyad_id))),
        by = c("year", "ext_name")][, .(avg_conflicts_per_actor = mean(tot_conflicts_per_actor)),
                                    by = "year"][
                                      support[!is.na(ext_name), .(tot_actors_per_conflict = length(unique(.SD$ext_name))),
                                              by = c("year", "dyad_id")][, .(avg_actors_per_conflict= mean(tot_actors_per_conflict)),
                                                                          by = "year"],
                                      on = "year"
                                    ] |>
  ggplot() +

  geom_point(aes(x = year, y = avg_conflicts_per_actor,
                 col = "Avg. Conflicts per Actor")) +
  geom_line(aes(x = year,
                y = avg_conflicts_per_actor,
                col = "Avg. Conflicts per Actor"),
            linewidth = 0.8) +
  geom_smooth(aes(x = year, y = avg_conflicts_per_actor,
                  col = "Avg. Conflicts per Actor"),
              linewidth = 0.6,
              se = FALSE) +

  geom_point(aes(x = year, y = avg_actors_per_conflict,
                 col = "Avg. Actors per Conflict")) +
  geom_line(aes(x = year, y = avg_actors_per_conflict,
                col = "Avg. Actors per Conflict"),
            linewidth = 0.8) +
  geom_smooth(aes(x = year, y = avg_actors_per_conflict,
                  col = "Avg. Actors per Conflict"),
              linewidth = 0.6,
              se = FALSE) +

  scale_color_manual(values = c("Avg. Conflicts per Actor" = "orange3",
                                "Avg. Actors per Conflict" = "cyan3"),
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
  scale_y_continuous(limits = c(0, 9)) +
  theme_minimal(base_size = 28)

ggsave("Results/Figures/avg_conflicts_supporters_wc.png", width = 20, height = 16)

# -------- no coalitions ---------

support[ext_coalition != 1, .(tot_conflicts_per_actor = length(unique(.SD$dyad_id))),
        by = c("year", "ext_name")][, .(avg_conflicts_per_actor = mean(tot_conflicts_per_actor)),
                                    by = "year"][
                                      support[ext_coalition != 1 & !is.na(ext_name), .(tot_actors_per_conflict = length(unique(.SD$ext_name))),
                                              by = c("year", "dyad_id")][, .(avg_actors_per_conflict= mean(tot_actors_per_conflict)),
                                                                         by = "year"],
                                      on = "year"
                                    ] |>
  ggplot() +
  
  geom_point(aes(x = year, y = avg_conflicts_per_actor,
                 col = "Avg. Conflicts per Actor")) +
  geom_line(aes(x = year,
                y = avg_conflicts_per_actor,
                col = "Avg. Conflicts per Actor"),
            linewidth = 0.8) +
  geom_smooth(aes(x = year, y = avg_conflicts_per_actor,
                  col = "Avg. Conflicts per Actor"),
              linewidth = 0.6,
              se = FALSE) +
  
  geom_point(aes(x = year, y = avg_actors_per_conflict,
                 col = "Avg. Actors per Conflict")) +
  geom_line(aes(x = year, y = avg_actors_per_conflict,
                col = "Avg. Actors per Conflict"),
            linewidth = 0.8) +
  geom_smooth(aes(x = year, y = avg_actors_per_conflict,
                  col = "Avg. Actors per Conflict"),
              linewidth = 0.6,
              se = FALSE) +
  
  scale_color_manual(values = c("Avg. Conflicts per Actor" = "orange3",
                                "Avg. Actors per Conflict" = "cyan3"),
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
  scale_y_continuous(limits = c(0, 9)) +
  theme_minimal(base_size = 28)

ggsave("Results/Figures/avg_conflicts_supporters_nc.png", width = 20, height = 16)
