update_tracker <- function(path, caption = "No caption provided", log_file = "plot_tracker.Rmd") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- paste0("- [ ] **File:** `", path, "`\n    * **Caption:** ", caption, "\n    * **Saved:** ", timestamp, "\n\n")
  
  # Append the entry to your tracking document
  cat(log_entry, file = log_file, append = TRUE)
  
  message("Log successfully updated in: ", log_file)
}
