find_neighbours <- function(g, state) {
  
  library(data.table, quietly = TRUE)
  neighbours <- unique(c(g[v1 == state, v2], g[v2 == state, v1]))
  neighbours <- neighbours[neighbours != state]
  return(neighbours)
  
}
