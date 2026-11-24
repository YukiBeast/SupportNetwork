calculate_communities_signed_mod <- function(isn, sliding_window = NULL, custom_periods = NULL, resolution = 0.75, method = "CPM") {
  library(data.table, quietly = T)
  library(igraph)
  
  setDT(isn)
  metrics_list <- list()
  graphs_list <- list() 
  window_list <- list()
  
  # Logic to handle either sliding windows OR custom periods
  if (!is.null(custom_periods)) {
    for (w in custom_periods) {
      window_list[[paste0(w[1], "-", w[2])]] <- c(w[1], w[2])
    }
  } else if (!is.null(sliding_window)) {
    slide <- sliding_window - 1
    start_years <- 1975:(2017 - slide)
    for (y in start_years) {
      window_list[[paste0(y, "-", y + slide)]] <- c(y, y + slide)
    }
  } else {
    stop("You must provide either 'sliding_window' or 'custom_periods'.")
  }
  
  # Iterate over the dynamically generated window_list
  for (w_name in names(window_list)) {
    y_start <- window_list[[w_name]][1]
    y_end <- window_list[[w_name]][2]
    
    dt_window <- isn[year >= y_start & year <= y_end]
    edges_san <- load_san_window(dt_window) 
    
    if(nrow(edges_san) == 0) next 
    
    g <- graph_from_data_frame(edges_san[, .(state1, state2, Weight)], directed = FALSE)
    E(g)$sign <- sign(E(g)$Weight)
    E(g)$abs_weight <- abs(E(g)$Weight)
    
    set.seed(123)
    leiden_res <- cluster_leiden(g, 
                                 objective_function = method, 
                                 weights = edges_san$Weight, 
                                 resolution = resolution)
    
    V(g)$community <- as.character(membership(leiden_res))
    communities <- membership(leiden_res)
    
    usa_comm <- if ("United States of America" %in% names(communities)) communities[["United States of America"]] else NA
    rus_comm <- if ("Russia (Soviet Union)" %in% names(communities)) communities[["Russia (Soviet Union)"]] else NA
    
    metrics_list[[w_name]] <- data.table(
      Window = w_name,
      Num_Communities = length(unique(communities)),
      Modularity_Score = leiden_res$quality,
      USA_Community = usa_comm,
      RUS_Community = rus_comm,
      Same_Bloc = (usa_comm == rus_comm & !is.na(usa_comm) & !is.na(rus_comm))
    )
    
    graphs_list[[w_name]] <- g
  }
  
  return(list(
    metrics = rbindlist(metrics_list), 
    graphs = graphs_list
  ))
}
