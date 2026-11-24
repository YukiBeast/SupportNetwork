load_functions <- function() {
  path <- "Scripts/Functions"
  funcs <- list.files(path = path)
  
  for (i in seq_len(length(funcs))) {
    source(paste0(path, "/", funcs[[i]]))
  }
}
load_functions()
