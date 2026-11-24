library(igraph)
library(data.table)

graph_1975 <- graph_from_data_frame(dyadic_network[year == 1975, -c("year", "dyad_id")])

summary_bipartite <- function(data) {
summary_dt <- 
  data[, {g <- .SD[!is.na(ext_name), .(dyad_name, ext_name, actor_nonstate, ext_nonstate, location)]
          g[, ext_name := gsub("Government of ", "", ext_name)]
          graph <- graph_from_data_frame(g, directed = FALSE)
          V(graph)$type <- V(graph)$name %in% g$ext_name
          .(total_intervening_actors = sum(V(graph)$type == TRUE),
            total_dyads = sum(V(graph)$type == FALSE),
            nonstate_actors = sum(E(graph)$ext_nonstate == 1),
            total_interventions = length(E(graph)),
            total_rebel_support = sum(E(graph)$actor_nonstate == 1),
            total_gov_support = sum(E(graph)$actor_nonstate == 0),
            graph_density = length(E(graph)) / (sum(V(graph)$type == TRUE) * sum(V(graph)$type == FALSE)))
        }
        ,
      by = "year"]

  get_max_year <- function(summary_dt, var) {
    var_max <- max(summary_dt[, get(var)], na.rm = TRUE)
    paste0(round(var_max, 3),
            " (", summary_dt[get(var) == var_max, year], ")")
  }
  data.table("Max Density" = get_max_year(summary_dt, "graph_density"),
             "Max Interventions" = get_max_year(summary_dt, "total_interventions"),
             "Max Rebel Support" = get_max_year(summary_dt, "total_rebel_support"),
             "Max Gov. Support" = get_max_year(summary_dt, "total_gov_support"))
}

data <- copy(support)
View(summary_bipartite(data))

summary_projected <- function(data) {
  data <- data[, -"dyad_id"]
  summary_dt <-
    data[, {
      graph <- graph_from_data_frame(.SD, directed = FALSE)
      .(graph_density = graph.density(simplify(graph, remove.multiple = TRUE)),
        total_ties = length(E(graph)),
        total_positive_ties = sum(E(graph)$sign == 1),
        total_negative_ties = sum(E(graph)$sign == -1),
        total_triangles = length(triangles(graph)) / 3)
    },
    by = "year"]
  
    get_max_year <- function(summary_dt, var) {
    var_max <- max(summary_dt[, get(var)], na.rm = TRUE)
    paste0(round(var_max, 3),
            " (", summary_dt[get(var) == var_max, year], ")")
  }

  get_min_year <- function(summary_dt, var) {
    var_min <- min(summary_dt[, get(var)], na.rm = TRUE)
    paste0(round(var_min, 3),
            " (", summary_dt[get(var) == var_min, year], ")")
  }
  data.table("Max Density" = get_max_year(summary_dt, "graph_density"),
             "Max Ties" = get_max_year(summary_dt, "total_ties"),
             "Max Ties (+)" = get_max_year(summary_dt, "total_positive_ties"),
             "Max Ties (-)" = get_max_year(summary_dt, "total_negative_ties"),
             "Min Triangles" = get_min_year(summary_dt, "total_triangles"),
             "Max Triangles" = get_max_year(summary_dt, "total_triangles")
             )
  }

data <- copy(dyadic_network)
View(summary_projected(dyadic_network))
