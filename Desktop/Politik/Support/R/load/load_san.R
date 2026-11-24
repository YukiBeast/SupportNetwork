load_san <- function(isn) {

  disaggregated_san <- compute_dyadic_relationships(isn[!is.na(ext_name), ])
  
  degree_table <- isn[!is.na(ext_name), 
                               .(degree = uniqueN(dyad_id)), 
                               by = .(year, state = ext_name)]
  aggregate_san <- disaggregated_san[, .(opposite_side = sum(sign < 0),
                                        same_side = sum(sign > 0)), by = c("year", "state1", "state2")]
  
  aggregate_san[degree_table, degree1 := i.degree, on = c("year", state1 = "state")]
  aggregate_san[degree_table, degree2 := i.degree, on = c("year", state2 = "state")]
  

  san <- aggregate_san[, .(
    year, state1, degree1, state2, degree2, same_side, opposite_side, 
    balance = same_side - opposite_side,
    type = fcase(
      same_side > 0  & opposite_side > 0,  "ambi",
      same_side > 0  & opposite_side == 0, "pos",
      same_side == 0 & opposite_side > 0,  "neg",
      same_side == 0 & opposite_side == 0, "null",
      default = NA_character_))][]

  return(san)  
}