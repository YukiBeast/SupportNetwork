plot_isn_net <- function(isn, year.f, path, legend.position = "bottomleft",
                         caption = "No caption provided",
                         log_file = "plot_tracker.Rmd") {
  
  set.seed(1)
  library(igraph, quietly = T)
  library(data.table, quietly = T)
  library(countrycode, quietly = T)
  
  dt <- as.data.table(copy(isn))
  dt <- dt[year == year.f & !is.na(ext_name), .(dyad_id, ext_name, actor_nonstate)]
  
  g <- graph_from_data_frame(dt, directed = FALSE)
  
  E(g)$color <-   ifelse(E(g)$actor_nonstate == 0, "#44AA99",
                         "#999933")
  
  states <- unique(dt$ext_name)
  
  V(g)$color <- ifelse(V(g)$name %in% states, adjustcolor("cyan3", alpha.f = 0.8), adjustcolor("orange3", alpha.f = 0.7))
  V(g)$shape <- ifelse(V(g)$name %in% states, "circle", "square")
  
  V(g)$label <- ifelse(V(g)$name %in% states,
                       countrycode(sourcevar = V(g)$name, 
                                   origin = "country.name", 
                                   destination = "iso3c",
                                   # Hier fangen wir deine speziellen Namen ab:
                                   custom_match = c(
                                     "Russia (Soviet Union)" = "RUS", # oder RUS
                                     "DR Congo (Zaire)" = "COD",      # oder COD
                                     "Vietnam (North Vietnam)" = "VNM",
                                     "Malaysian elements" = "MYS",
                                     "South Yemen" = "S-YMD",
                                     "Yemen (North Yemen)" = "N-YMD",
                                     "East Germany" = "DDR",
                                     "Serbian Republic of Bosnia-Herzegovina" = "BIH",
                                     "Interahamwe, ex-FAR" = "INT/FAR",
                                     "Jemaah Islamiya" = "JI",
                                     "Lashkar-e-Taiba" = "LeT"
                                   ), warn = FALSE),
                       "")
  
  V(g)$label <- ifelse(is.na(V(g)$label), V(g)$name,  V(g)$label)
  
  V(g)$size <- ifelse(V(g)$name %in% states, degree(g) / max(degree(g)) * 8,
                      4)
  
  V(g)$size[V(g)$size < 2.5] <- 2.5
  
  # Zieht Hubs oft sehr sauber auseinander
  l <- layout_components(g, layout = layout_with_kk)
  
  draw_plot <- function() {
    plot(g,
#         main = paste0("ISN in ", year.f),
         layout = l,
         
         margin = c(-0.07, -0.4, -0.07, -0.4),
         
         vertex.color = V(g)$color,
         vertex.shape = V(g)$shape,
         vertex.size = V(g)$size,
         
         # Beim Plot-Befehl:
         edge.color = adjustcolor(E(g)$color, alpha.f = 0.75), # 75% Deckkraft
         edge.width = 1.2,                # Kanten leicht dicker machen
         edge.curved = 0.1,               # Leichte Kurven entzerren parallele Linien
         
         vertex.label.color = "black",
         vertex.label.cex = 0.75,
         vertex.label.family = "sans",
         vertex.label.font = 2,
         vertex.label.dist = 0.7,
         
    )
    
    if (legend.position %in% c("bottomright",
                               "bottom", "bottomleft", "left",
                               "topleft", "top", "topright",
                               "right", "center")) {
      legend(legend.position, 
             legend = c("Government", "Rebel"),
             col = c("#44AA99", "#999933"), 
             lty = 1,       # Sagt R: Male durchgezogene Linien
             lwd = 2,       # Macht die Linien in der Legende etwas dicker
             title = "Support Type")
      legend("topleft",
             legend = c("Dyad", "Intervening Actor"),
             col = c(adjustcolor("orange3", alpha.f = 0.7), adjustcolor("cyan3", alpha.f = 0.8)),
             pch = c(15, 16),
             title = "Node Type")
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
  
}
