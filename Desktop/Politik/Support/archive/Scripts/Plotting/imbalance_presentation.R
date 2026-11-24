library(igraph)

# Netzwerk erstellen
edges <- data.frame(state1 = c("A", "A", "B", "B", "B", "B"),
                    state2 = c("B", "C", "C", "C", "C", "C"),
                    sign = c(1, -1, 1, -1, -1, -1))
g <- graph_from_data_frame(edges, directed = FALSE)


set.seed(3)
my_layout <- layout_with_kk(g)

plot_remove_edge <- function(g, edge, main = NULL) {
  
E(g)$color <- ifelse(E(g)$sign == 1, "blue", "red")
  
if(!any(is.na(edge))) E(g)$color[edge] <- "white"
plot(g, 
      vertex.color = "white", 
      layout = my_layout,
      vertex.size = 30,
      edge.color = E(g)$color,
       edge.width = 3,
       main = main,
       vertex.label.font = 2,    # 1=normal, 2=fett (bold), 3=kursiv
       vertex.label.cex = 1.5   # Skalierungsfaktor für die Textgröße (1.5 = 150%)
       )
}
dev.off()
plot_remove_edge(g, NA, "")
legend("topright", legend=c("positiv", "negativ"), col=c("blue", "red"), lty=1, lwd=2)

par(mfrow = c(1, 4))

plot_remove_edge(g, c(4, 5, 6), "")
plot_remove_edge(g, c(3, 4, 5), "")
plot_remove_edge(g, c(3, 4, 6), "")
plot_remove_edge(g, c(3, 5, 6), "")

