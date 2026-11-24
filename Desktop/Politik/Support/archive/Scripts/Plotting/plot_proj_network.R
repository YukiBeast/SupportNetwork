# Idea of this function is to take network_proj and a selected year as input and
# create a network object for that year ready to be plotteed
source("setup.R", echo = TRUE)
source("scripts/functions/get_haversine.R")
library(igraph)

network_proj[state1 == "Yemen (North Yemen)", state1 := "Yemen"]
network_proj[state2 == "Yemen (North Yemen)", state2 := "Yemen"]

network_proj[state1 == "Serbian Republic of Bosnia-Herzegovina", state1 := "Bosnia-Herzegovina"]
network_proj[state2 == "Serbian Republic of Bosnia-Herzegovina", state2 := "Bosnia-Herzegovina"]

network_proj[, state1 := gsub("Government of ", "", state1)]
network_proj[, state1_iso := countrycode::countrycode(state1, origin = "country.name",
                                        destination = "iso3c")]
network_proj[, state1_iso := ifelse(is.na(state1_iso), state1, state1_iso)]

network_proj[, state2 := gsub("Government of ", "", state2)]
network_proj[, state2_iso := countrycode::countrycode(state2, origin = "country.name",
                                        destination = "iso3c")]

network_proj[, state2_iso := ifelse(is.na(state2_iso), state2, state2_iso)]

network_proj[, dist := get_haversine(state1, state2)]
top_states <- c(
  # Westliche Mächte & NATO
  "United States of America", "United Kingdom", "France", "Germany", "Canada", 
  "Italy", "Netherlands", "Belgium", "Spain", "Turkey", "Greece", "Portugal",
  
  # Osteuropa & Eurasien
  "Russia (Soviet Union)", "Ukraine", "Belarus", "Serbia", "Poland", "Kazakhstan", "Uzbekistan",
  
  # Asien & Pazifik
  "China", "India", "Japan", "Pakistan", "North Korea", "South Korea", 
  "Vietnam", "Indonesia", "Australia", "Thailand", "Myanmar",
  
  # Naher Osten & Nordafrika
  "Iran", "Saudi Arabia", "Israel", "Syria", "Iraq", "Egypt", "Libya", 
  "Algeria", "Morocco", "Jordan",
  
  # Subsahara-Afrika
  "South Africa", "Nigeria", "Ethiopia", "Sudan", "Angola", "DR Congo", 
  "Kenya", "Tanzania", "Somalia",
  
  # Lateinamerika
  "Cuba", "Brazil", "Argentina", "Chile", "Colombia", "Nicaragua", "Venezuela"
)

all_states <- unique(network_proj$state1, network_proj$state2)

plot_proj_network <- function(edges, vertices = NULL, year.f, states) {

set.seed(1)
# print(unique(edges[year == year.f, state1_iso]))
  
  if(length(states) == 1) {
  edges_filt <- edges[year == year.f & state1 != state2
    & (state1 %in% states | state2 %in% states)
    ]
  } else   {edges_filt <- edges[year == year.f & state1 != state2
    & (state1 %in% states & state2 %in% states)
    ]}
  edges_filt[, year := NULL]

  if (!is.null(vertices)) {
    vertices_filt <- vertices[year == year.f, year := NULL]
  } else vertices_filt <- NULL

  edges_filt$dist[is.na(edges_filt$dist)] <- mean(edges_filt$dist, na.rm = TRUE)
  # print(unique(edges_filt$state1_iso))
  g <- graph_from_data_frame(d = edges_filt[, .(state1_iso, state2_iso, score, dist)],
    vertices = vertices_filt,
    directed = FALSE)
  g <- simplify(g, remove.multiple = TRUE,
                remove.loops = TRUE,
                edge.attr.comb = list(dist = "first", 
                                      score = "first"))
  
  transparent_colors <- ifelse(get.edge.attribute(g, "score") > 0, 
                                adjustcolor("#57c0f5", alpha.f = 0.5), 
                                adjustcolor("#ae3636", alpha.f = 0.5))

  E(g)$weight <- (E(g)$score + abs(min(E(g)$score)) + 0.01) * (5000 / (E(g)$dist + 0.1))
#  lay <- layout_with_fr(g, weights = E(g)$dist_inv)
  lay <- layout_with_fr(g, weights = E(g)$weight)
  
#  lay <- layout_with_graphopt(g, charge = 0.3)

  d <- degree(g)
  # print(unique(V(g)))
  plot.igraph(g,
 #             layout = lay,
              edge.curved = 0,
              edge.color = transparent_colors,
              vertex.size = exp(d / max(d) * 2) + 3,
              vertex.label.cex = 0.7,
              vertex.label.color = "black",
              vertex.label.font = 2,
              vertex.color = "salmon",
              main = paste0("SAN in year ", year.f))
              
}

dev.off()
plot_proj_network(edges = network_proj, year = 1985, states = all_states)
dev.off()

plot_proj_network(edges = network_proj, year = 1995, states = all_states)

dev.off()
plot_proj_network(edges = network_proj, year = 2005, states = all_states)
