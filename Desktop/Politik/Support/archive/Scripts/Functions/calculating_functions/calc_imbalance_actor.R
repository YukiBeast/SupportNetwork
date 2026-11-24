# This function takes in the disaggregated_san and returns for every year-state pair:

# ratio = 0 --> measure 1: imbalanced triangles per state divided by total triangles by that state
# ratio = 1 --> total imbalanced triangles per state
# ratio = 2 --> total balanced triangles per state
calc_imbalance_actor <- function(disaggregated_san, actor, year.f, ratio = 0) {
  library(data.table)
  
  # 1. Setup & Ordering
  dt <- copy(disaggregated_san[year == year.f])
  dt[, `:=`(v1 = pmin(state1, state2), v2 = pmax(state1, state2))]
  dt <- dt[, .(v1, v2, sign, dyad_id)]
  
  # 2. Get ego links (All nodes connected to the 'actor')
  # We extract the neighbor's ID and the sign of the edge connecting them to 'actor'
  ego <- dt[v1 == actor | v2 == actor, 
            .(neighbor = ifelse(v1 == actor, v2, v1), 
              sign_actor = sign)]
  
  if (nrow(ego) < 2) return(0) # Need at least 2 neighbors to form a triangle
  
  # 3. Find Triangles (The Magic Step)
  # A triangle exists if two of the actor's neighbors share an edge in dt.
  # Since dt maintains v1 < v2, we NEVER double-count!
  triads <- dt[v1 %in% ego$neighbor & v2 %in% ego$neighbor]
  
  if (nrow(triads) == 0) return(0)
  
  # 4. Bring in the signs from the ego connections
  # Join the sign for the Actor-v1 edge
  triads <- merge(triads, ego[, .(v1 = neighbor, sign_v1 = sign_actor)], by = "v1",
                  allow.cartesian = TRUE)
  # Join the sign for the Actor-v2 edge
  triads <- merge(triads, ego[, .(v2 = neighbor, sign_v2 = sign_actor)], by = "v2",
                  allow.cartesian = TRUE)
  
  # 5. Calculate Imbalance
  triads[, product := sign * sign_v1 * sign_v2]
  
  if(ratio == 1) return(triads[product < 0, as.double(.N)])
  if(ratio == 2) return(triads[product > 0, as.double(.N)])
  
  # Default: ratio = 0 (proportion of imbalanced triangles)
  return(triads[product < 0, .N] / triads[, .N])
}
