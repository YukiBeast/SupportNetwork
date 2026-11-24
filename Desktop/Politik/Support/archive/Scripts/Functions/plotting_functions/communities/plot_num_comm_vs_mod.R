plot_num_comm_vs_mod <- function(communities_dt, coeff = 30, path) {
  library(ggplot2)
  
  # Fattore di scala per allineare i decimali della modularità ai numeri interi delle communities

  p <- ggplot(communities_dt, aes(x = Window, group = 1)) +
    # Linea blu per il numero di Communities
    geom_line(aes(y = Num_Communities), color = "steelblue", size = 1.2) +
    
    # Linea rossa per la Modularity Score (moltiplicata per il coeff per essere visibile)
    geom_line(aes(y = Modularity_Score * coeff), color = "firebrick", size = 1.2) +
    
    # Configurazione del doppio asse Y
    scale_y_continuous(
      name = "Number of Communities",
      sec.axis = sec_axis(~ . / coeff, name = "Modularity Score")
    ) +
    
    geom_vline(xintercept = "1987-1991", linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = "1997-2001", linetype = "dashed", color = "gray50") +
    
    # Estetica e leggibilità
    theme_minimal(base_size = 18) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1), # Ruota le etichette temporali
      axis.title.y.left = element_text(color = "steelblue", face = "bold"),
      axis.title.y.right = element_text(color = "firebrick", face = "bold")
    )
  
  save_interactive(p, path)
}

# plot_num_comm_vs_mod(results$metrics, "Results/Pics/communities/#comm_vs_mod.pdf")
