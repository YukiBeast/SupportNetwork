# Idea of this function is to take network_proj and a selected year as input and
# create a network object for that year ready to be plotteed
source("setup.R", echo = TRUE)
source("scripts/functions/get_haversine.R")
library(igraph)

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


plot_proj_network(edges = network_proj, year = 1980, states = top_states)

plot_proj_network(edges = network_proj, year = 1995, states = top_states)

plot_proj_network(edges = network_proj, year = 2010, states = top_states)
