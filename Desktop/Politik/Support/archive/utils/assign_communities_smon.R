assign_communities <- function(z) {
  z <- round(z, 2)
  actors <- rownames(z)
  community <- numeric(length(actors))
  prob <- numeric(length(actors))
  
  for (i in seq_along(actors)) {
    community[[i]] <- which.max(z[i, ])
    prob[[i]] <- max(z[i, ])
  }
  
  return(data.frame(actors = actors,
                    community = community,
                    prob = prob))
  
}
