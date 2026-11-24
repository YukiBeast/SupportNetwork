load_san_window <- function(isn_window) {
  # Filter out NAs
  dt_valid <- isn_window[!is.na(ext_name)]
  
  # 1. Get disaggregated dyadic relationships for the whole window
  disaggregated_san <- compute_dyadic_relationships_window(dt_valid)
  
  # 2. Calculate degrees for the window (unique dyads per state)
  degree_table <- unique(dt_valid[, .(dyad_id, ext_name)])[, 
                                                           .(degree = .N), by = .(state = ext_name)]
  
  # 3. Aggregate ties by state pairs (no year grouping)
  aggregate_san <- disaggregated_san[, .(opposite_side = sum(sign < 0),
                                         same_side = sum(sign > 0)), 
                                     by = .(state1, state2)]
  
  # 4. Merge degrees
  aggregate_san[degree_table, degree1 := i.degree, on = .(state1 = state)]
  aggregate_san[degree_table, degree2 := i.degree, on = .(state2 = state)]
  
  # 5. Calculate final weights and types
  san <- aggregate_san[, .(
    state1, degree1, state2, degree2, same_side, opposite_side, 
    Weight = same_side - opposite_side,
    type = fcase(
      same_side > 0  & opposite_side > 0,  "ambi",
      same_side > 0  & opposite_side == 0, "pos",
      same_side == 0 & opposite_side > 0,  "neg",
      same_side == 0 & opposite_side == 0, "null",
      default = NA_character_)
  )]
  
  # Filter out null relationships if any exist
  return(san[Weight != 0 | same_side > 0 | opposite_side > 0])  
}
