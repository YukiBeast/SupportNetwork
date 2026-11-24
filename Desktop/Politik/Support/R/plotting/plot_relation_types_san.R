plot_relation_types_san <- function(san, path,
                                    caption = "no caption provided",
                                    log_file = "plot_tracker.Rmd") {
  library(data.table)
  library(ggplot2)
  p <- san[, .(ambi = sum(type == "ambi"),
              neg = sum(type == "neg"),
              pos = sum(type == "pos")),
          by = "year"] |>
    ggplot(aes(x = year)) +
    
    geom_point(aes(y = ambi, col = "ambi")) +
    geom_line(aes(y = ambi, col = "ambi")) +
    geom_smooth(aes(y = ambi, col = "ambi"), se = F) +
    
    geom_point(aes(y = pos, col = "pos")) +
    geom_line(aes(y = pos, col = "pos")) +
    geom_smooth(aes(y = pos, col = "pos"), se = F) +
    
    geom_point(aes(y = neg, col = "neg")) +
    geom_line(aes(y = neg, col = "neg")) +
    geom_smooth(aes(y = neg, col = "neg"), se = F) +
    
    scale_color_manual(values = c("ambi" = "azure3",
                                  "neg" = "#661100",
                                  "pos" = "#117733"),
                       labels = c("ambi" = "Ambivalent",
                                  "neg" = "Strictly Negative",
                                  "pos" = "Strictly Positive"),
                       name = "Relation Type"
    ) + 
    line1 + line2 +
    labs(x = "Year", y = "Count") +
    theme_minimal(base_size = 24) +
    theme(legend.position = "none")

  save_interactive(p, path, caption, log_file)
}
