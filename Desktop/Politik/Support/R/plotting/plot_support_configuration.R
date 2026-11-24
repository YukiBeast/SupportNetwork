plot_support_configuration <- function(data, path, share = FALSE,
                    caption = "No caption provided",
                    log_file = "plot_tracker.Rmd") {
  y_upper <- max(data$mixed_support, data$gov_support, data$no_support, data$rebel_support,
                 na.rm = TRUE) * 2
  p <- ggplot(data) +
    geom_point(aes(x = year,
                   y = mixed_support,
                   shape = "Competitive Intervention"),
               col = "black") +
    geom_line(aes(x = year,
                  y = mixed_support),
              col = "black",
              alpha = 0.4) +
    geom_smooth(aes(x = year,
                    y = mixed_support),
                se = FALSE,
                col = "black") +
    geom_point(aes(x = year,
                   y = gov_support,
                   shape = "Government Support"),
               col = "#44AA99") +
    geom_line(aes(x = year,
                  y = gov_support),
              col = "#44AA99",
              alpha = 0.4) +    
    geom_smooth(aes(x = year,
                    y = gov_support),
                se = FALSE,
                col = "#44AA99") +
    geom_point(aes(x = year,
                   y = rebel_support,
                   shape = "Rebel Support"),
               col = "#999933") +
    geom_line(aes(x = year,
                  y = rebel_support),
              col = "#999933",
              alpha = 0.4) +
    geom_smooth(aes(x = year,
                    y = rebel_support),
                se = FALSE,
                col = "#999933") +
    geom_point(aes(x = year,
                   y = no_support,
                   shape = "No Support"),
               col = "#888888") +
    geom_line(aes(x = year,
                  y = no_support),
              col = "#888888",
              alpha = 0.4) +
    geom_smooth(aes(x = year,
                    y = no_support),
                se = FALSE,
                col = "#888888") +
    scale_shape_manual(values = c("Competitive Intervention" = 16,
                                  "Government Support" = 17,
                                  "Rebel Support" = 15,
                                  "No Support" = 18),
                       name = NULL) + line1 + line2 +
    #    scale_y_continuous(limits = c(0, y_upper)) +
    labs(x = "Year",
         y = ifelse(share, "Share", "Count")) +
    theme_minimal(base_size = 28) +
    theme(legend.position = "none")
  save_interactive(p, path, caption, log_file)
}

count_support <- function(data,
                          absolute = TRUE,
                          coalitions = TRUE) {
  
  unique_interventions <- unique(data[, 
                                      .(year, dyad_id, ext_name, actor_nonstate)])
  if (coalitions == FALSE) {
    unique_interventions <- unique(data[ext_coalition == 0 | is.na(ext_coalition), 
                                        .(year, dyad_id, ext_name, actor_nonstate)])}
  
  count_interventions <- unique_interventions[, .(
    # 1. Evaluate the status for each dyad (returns TRUE or FALSE per dyad)
    no_support = all(is.na(ext_name)),
    
    # For gov/rebel, we ensure there IS an external supporter, 
    # and that ALL non-NA supporters match the target actor type
    only_gov   = any(!is.na(ext_name)) & all(actor_nonstate[!is.na(ext_name)] == 0),
    only_rebel = any(!is.na(ext_name)) & all(actor_nonstate[!is.na(ext_name)] == 1)
    
  ), by = .(year, dyad_id)  # <-- First group by year AND dyad_id
  ][, .(
    # 2. Now chain a second operation to aggregate up to the year level
    all_dyads     = .N,                 # .N counts the rows (which are now unique dyads)
    no_support    = sum(no_support),    # TRUE counts as 1, FALSE as 0
    gov_support   = sum(only_gov),
    rebel_support = sum(only_rebel)
  ), by = "year"]                       # <-- Finally group by year
  
  count_interventions[, mixed_support := all_dyads - no_support - gov_support - rebel_support, by = .I]
  
  if (absolute == FALSE) {
    count_interventions <- count_interventions[, .(year,
                                                   no_support = no_support / all_dyads,
                                                   gov_support = gov_support / all_dyads,
                                                   rebel_support = rebel_support / all_dyads,
                                                   mixed_support = mixed_support / all_dyads)]
  }
  
  return(count_interventions)
}
