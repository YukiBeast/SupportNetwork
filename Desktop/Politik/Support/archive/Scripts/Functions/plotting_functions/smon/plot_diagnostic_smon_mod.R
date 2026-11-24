plot_diagnostic_smon_mod <- function(random_smon,
                                     path,
                                     caption = "No caption provided",
                                     log_file = "plot_tracker.Rmd") {
  # random_smon$mod_history$run <- as.character(random_smon$mod_history$run)
  p <- ggplot(random_smon$mod_history, aes(x = iteration, y = mod_cont, group = run, color = run)) + 
    geom_line() +
    coord_cartesian(ylim = c(0.06, 0.075)) +
    labs(x = "Run", y = "Continuous Modularity") +
    theme_minimal()

  save_interactive(p, path, caption, log_file)
    
    
}

plot_diagnostic_smon_mod(random_smon)
