plot_san_net <- function(san, year.f, path,
                         caption = "No caption provided",
                         log_file = "plot_tracker.Rmd",
                         legend.position = "bottomleft") {
  set.seed(1)
  library(igraph, quietly = T)
  library(data.table, quietly = T)
  library(countrycode, quietly = T)
  
  if (!("sign" %in% names(san))) {
    san$sign <- sign(san$balance)
  }
  
  dt <- as.data.table(copy(san))
  dt <- dt[year %in% year.f, .(state1, state2, balance, sign)]
  
  g <- graph_from_data_frame(dt, directed = FALSE)
  
  E(g)$balance_abs <- abs(E(g)$balance) / 2
  E(g)$color <- ifelse(E(g)$sign > 0, "#117733",
                       "#661100")
  E(g)$color <- ifelse(E(g)$sign == 0, "#888888",
                       E(g)$color)
  
  
  deg <- degree(g)
  v_sizes <- 6 + (deg / max(deg)) * 19
  
  # Falls countrycode ein Land gar nicht erkennt (NA), behalten wir den alten Namen
  V(g)$label <- get_iso3(V(g)$name)
  
  # Zieht Hubs oft sehr sauber auseinander
  l <- layout_components(g, layout = layout_with_kk)
  
  draw_plot <- function() {
    plot(g,
#         main = paste0("SAN in ", year.f),
         layout = l,
         
         margin = -0.1,
         
         vertex.size = v_sizes / 3,
         vertex.color = adjustcolor("cyan3", alpha.f = 0.5),
         
         edge.color = E(g)$color,
         edge.width = E(g)$balance_abs,                # Kanten leicht dicker machen
         edge.curved = 0.1,               # Leichte Kurven entzerren parallele Linien
         
         vertex.label.color = "black",
         vertex.label.cex = 0.75,
         vertex.label.family = "sans",
         vertex.label.font = 2,
         vertex.label.dist = 1,
         
    )
    if (legend.position %in% c("bottomright",
                                 "bottom", "bottomleft", "left",
                                 "topleft", "top", "topright",
                                 "right", "center")) {
      legend(legend.position, 
             legend = c("Positive", "Negative", "Balanced"),
             col = c("#117733", "#661100", "#888888"), 
             lty = 1,       # Sagt R: Male durchgezogene Linien
             lwd = 2,       # Macht die Linien in der Legende etwas dicker
             title = "Relation")
    }

    
  }
  
  # 1. Plot direkt im RStudio Viewer anzeigen
  draw_plot()
  
  # 2. Interaktive Abfrage (nur wenn ein Pfad angegeben wurde)
  if (!is.null(path)) {
    # Holt die Antwort des Users aus der Konsole
    antwort <- readline(prompt = "Möchtest du den Plot speichern? (ja/nein): ")
    
    # Checkt ob die Antwort "ja", "j", "yes" oder "y" ist (egal ob groß/klein)
    if (tolower(trimws(antwort)) %in% c("ja", "j", "yes", "y")) {
      
      # Öffnet das Gerät, zeichnet den Plot neu und schließt es
      pdf(file = path, width = 10, height = 8)
      draw_plot()
      dev.off()
      
      cat("Erfolg! Plot wurde gespeichert unter:", path, "\n")
      update_tracker(path, caption, log_file)
      
    } else {
      cat("Okay, Plot wurde nicht gespeichert.\n")
    }
  }
  return(invisible(l))
}

# l <- plot_san_net(san, 1985, legend.position = "none", path = "")
