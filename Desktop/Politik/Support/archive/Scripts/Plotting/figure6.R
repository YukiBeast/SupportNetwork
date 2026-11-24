source("Setup.R")

# -------- figure 6 ---------

tot_interv <- support[!is.na(ext_name), .(tot_interventions = .N),
                      by = c("year", "ext_name")]
setorderv(tot_interv, cols = c("year", "tot_interventions"),
         order = -1)

tot_interv <- tot_interv[, .(tot_interventions,
                             ext_name,
                             overall = sum(tot_interventions)),
                         by = "year"][, head(.SD, 3), by = "year"][,
                           .(share = 100 * sum(tot_interventions) / overall,
                             tot_interventions,
                             ext_name = gsub("Government of", "", ext_name)),
                           by = "year"
                         ]

relevant_states <- tot_interv[, .N, by = "ext_name"][N > 2, ext_name]
shapes <- 0:10
names(shapes) <- relevant_states

complete_years <- as.data.table(cross_join(data.frame(ext_name = relevant_states), data.frame(year = 1975:2014)))
tot_interv[ext_name %in% relevant_states, ][complete_years, on = c("ext_name", "year"), allow.cartesian = TRUE] |>
ggplot(aes(x = year)) +
  geom_line(data = tot_interv[ext_name %in% relevant_states & !is.na(share), ],
            aes(x = year,
                y = share),
            linewidth = 1.5) +
  geom_point(aes(y = share),
             size = 5) +
  geom_point(aes(y = tot_interventions,
                 shape = ext_name),
             size = 5) +
  geom_line(aes(y = tot_interventions,
                group = ext_name)) +

  scale_y_continuous(

    # Features of the first axis
    name = "# of Interventions",

    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*1, name = "Share of Interventions (%)"),
    limits = c(0, 40)
  ) +
  scale_shape_manual(name  = "Actor",
                     values = shapes) +
  geom_vline(xintercept = 1991,
             col = "darkgray",
             linetype = "dashed",
             linewidth = 0.6) +
  geom_vline(xintercept = 2001,
             col = "darkgray",
             linetype = "dashed",
             linewidth = 0.6) +
  theme_minimal(base_size = 28) +
  labs(x = "Year") +
  theme(legend.position = "bottom",
        legend.byrow = TRUE)

ggsave("Results/Figures/most_intervenist.png", width = 24, height = 16)
