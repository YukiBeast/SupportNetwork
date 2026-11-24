source("Setup.R", echo = TRUE)
file
source("Scripts/Functions/get_haversine.R")
source("Scripts/Plotting/plot_proj_network.R")

library(igraph)

plot_ego_proj(network_proj, "United States of America", 1980)
plot_ego_proj(network_proj, "Germany", 2005)

plot_ego_proj(network_proj, "United States of America", 1995)

plot_ego_proj(network_proj, "United States of America", 2010)



