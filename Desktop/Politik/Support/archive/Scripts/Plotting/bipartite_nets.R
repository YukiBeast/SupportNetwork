library(data.table)
library(stringr)

# Convert your unique list to a data.table
locs <- data.table(raw_location = unique(support$location))

# Split into Country and Region
locs[, c("country", "region") := tstrsplit(raw_location, ": ", fixed = TRUE)]

# Clean up common naming inconsistencies
locs[, country := str_remove(country, " \\(.*\\)")] # Removes (Burma), (Zaire), etc.
locs[country == "Russia", country := "Russia"]

# Now you have a clean table:
# raw_location                 country        region
# 1: Iran: Kurdistan            Iran           Kurdistan
# 2: Philippines: Government    Philippines    Government

library(maps)
library(dplyr)
library(stringr)

# 1. Hauptstädte aus 'world.cities' laden und filtern
# 'capital == 1' sind die Hauptstädte
data("world.cities")
capitals <- world.cities %>% 
  filter(capital == 1) %>% 
  select(country.etc, name, lat, long) %>%
  rename(country = country.etc, capital_name = name) %>% as.data.table()

# 2. Deine Locations vorbereiten (wie im vorigen Schritt)
# Wir gehen davon aus, dass 'locs' dein Data.table mit 'country' und 'region' ist
locs[, country_clean := str_trim(country)]

# 3. Merge: Regierungssitze mit Hauptstädten matchen
# Hinweis: Manche Ländernamen in 'world.cities' weichen leicht von deinem Dataset ab
# Ein einfaches Join sollte bei den meisten klappen:
setkey(locs, country_clean)
setkey(capitals, country)

final_coords <- capitals[locs, on = .(country = country_clean)]

# 4. Ergebnisprüfung
# Jetzt haben alle Zeilen, bei denen region == "Government" war, 
# automatisch die Koordinaten der Hauptstadt
final_coords <- final_coords[, .(raw_location,capital_name, country, region, lat, long)]
final_coords[region != "Government", c("lat", "long") := .(NA, NA)]

final_coords[region != "Government", raw_location]





g <- support[!is.na(ext_name), .(year, dyad_name, ext_name, actor_nonstate, ext_nonstate, location)]
g[, ext_name := gsub("Government of ", "", ext_name)]
g[, ext_iso := countrycode::countrycode(ext_name, origin = "country.name",
                                        destination = "iso3c")]
g[, ext_name := ifelse(is.na(ext_iso), ext_name, ext_iso)]

plot_bipartite <- function(g, year.f) {
# 1. Divide the plot device into two rows
  layout(matrix(c(1, 2), nrow = 2, byrow = TRUE), heights = c(4, 1))
  
  # 2. Plot the graph in the first section
  par(mar = c(1, 1, 3, 1))
  g_filt <- g[year == year.f, -c("year")]
  graph <- graph_from_data_frame(g_filt, directed = FALSE)
  
  V(graph)$type <- V(graph)$name %in% g$ext_name
  
  # Styling
  V(graph)$color <- ifelse(!V(graph)$type, "lightblue", "salmon")
  V(graph)$shape <- ifelse(!V(graph)$type, "square", "circle")
  
  # Fix: Use edge.lty for line types
  E(graph)$lty <- ifelse(E(graph)$actor_nonstate == 1, 2, 1) # 2=dashed, 1=solid
  
  # Labels
  nodes_to_label <- c("RUS", "CHN", "IRQ", "USA", "CUB", "ISR", "IRN", "GBR", "al-Qaida")
  V(graph)$label <- ifelse(V(graph)$name %in% nodes_to_label, V(graph)$name, "")
  
  # Plot
  plot(graph, 
       vertex.size = 8,
       vertex.label = V(graph)$label,
       vertex.label.cex = 0.65,
       vertex.label.color = "black",
       vertex.label.font = 2,
       edge.lty = E(graph)$lty, # Corrected argument
       main = paste("Network in", year.f))
  
# 3. Legend Area
  par(mar = c(0, 0, 0, 0))
  plot.new() # Create a blank canvas for the legends
  
  # Place first legend on the left half (x = 0.25)
  legend(x = 0.25, y = 1, 
         legend = c("Intervening Actor", "Dyad"), 
         col = c("salmon", "lightblue"), 
         title = "Node Type",
         pch = c(19, 15), bty = "n", cex = 1)
  
  # Place second legend on the right half (x = 0.75)
  legend(x = 0.5, y = 1, 
         legend = c("Rebel Support", "Government Support"), 
         title = "Line Type",
         lty = c(2, 1), bty = "n", cex = 1)
}

# Run the function
dev.off()
plot_bipartite(g, 1985)
plot_bipartite(g, 1995)

plot_bipartite(g, 2005)
