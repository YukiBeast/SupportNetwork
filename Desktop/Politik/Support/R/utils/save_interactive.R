save_interactive <- function(p, path,
                             caption = "No caption provided",
                             log_file = "plot_tracker.Rmd",
                             width = 20, height = 12) {
  # 1. Display the plot in the plotting pane
  print(p)
  
  # 2. Prompt the user in the console
  answer <- readline(prompt = "Do you want to save the plot? (y/n): ")
  
  # 3. Check if the answer is one of the accepted "yes" variants
  if (tolower(trimws(answer)) %in% c("y", "yes", "1")) {
    # Explicitly pass the plot 'p' to ggsave
    ggsave(filename = path, plot = p, width = width, height = height)
    
    # 4. Generate timestamp and format the log entry
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    log_entry <- paste0("- [ ] **File:** `", path, "`\n    * **Caption:** ", caption, "\n    * **Saved:** ", timestamp, "\n\n")
    
    # 5. Append the entry to your tracking document
    cat(log_entry, file = log_file, append = TRUE)
    
    message("Plot successfully saved to: ", path)
    message("Log successfully updated in: ", log_file)
  } else {
    message("Plot was not saved.")
  }
}
