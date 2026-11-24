compute_dyadic_relationships <- function(isn) {
  # 1. Ensure we only have the columns we need
  # We assume 'isn' has: year, dyad_id, ext_name, signed_support
  df <- isn[, .(year, dyad_id, ext_name, signed_support)]
  
  # 2. Self-join on year and dyad_id
  # This creates all possible pairs of states within the same dyad
  setkey(df, year, dyad_id)
  
  # Perform the join
  res <- df[df, on = .(year, dyad_id), allow.cartesian = TRUE]
  
  # 3. Calculate relationship sign
  # If both support the same side (1*1 or -1*-1), result is 1 (Positive)
  # If they support opposite sides (1*-1), result is -1 (Negative)
  res[, sign := signed_support * i.signed_support]
  
  # 4. Clean up:
  # Remove self-loops (a state interacting with itself)
  # Ensure state1 < state2 to avoid duplicate mirrored rows (A-B and B-A)
  res <- res[ext_name < i.ext_name]
  
  # Rename columns for clarity
  setnames(res, c("ext_name", "i.ext_name"), c("state1", "state2"))
  
  return(res[, .(year, dyad_id, state1, state2, sign)])
}
