get_haversine <- function(state1, state2) {

  library(data.table, quietly = TRUE)
  library(igraph, quietly = TRUE)
  library(countrycode, quietly = TRUE)
  library(geosphere, quietly = TRUE)
  library(maps, quietly = TRUE)

  iso_state1 <- countrycode(gsub("Government of ", "", state1), 
                            "country.name", "iso3c", warn = FALSE)
  iso_state2 <- countrycode(gsub("Government of ", "", state2), 
                            "country.name", "iso3c", warn = FALSE)

  data(world.cities, envir = environment())
  capitals <- as.data.table(world.cities)[capital == 1,
                                          .(lat, long,
                                            ISO = countrycode(gsub("Government of ", "", country.etc),
                                                              "country.name", "iso3c", warn = FALSE))]

  capitals <- unique(capitals[!is.na(ISO)], by = "ISO") # avoid South africa-Crash
  
    #
    idx1 <- match(iso_state1, capitals$ISO)
    idx2 <- match(iso_state2, capitals$ISO)
    
    lon1 <- capitals$long[idx1]
    lat1 <- capitals$lat[idx1]
    
    lon2 <- capitals$long[idx2]
    lat2 <- capitals$lat[idx2]
    
    distance_km <- distHaversine(
      p1 = cbind(lon1, lat1),
      p2 = cbind(lon2, lat2)
    ) / 1000
    
    return(distance_km)
  
}



