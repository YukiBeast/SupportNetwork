source("Scripts/Functions/load_functions.R")

library(ggplot2)

line1 <- geom_vline(xintercept = 1991,
                           col = "darkgray",
                           linetype = "dashed",
                           linewidth = 0.7)
line2 <- geom_vline(xintercept = 2001,
             col = "darkgray",
             linetype = "dashed",
             linewidth = 0.7)

nocoalitions <- load_support_nocoalitions()
###
count_total_dyads_supporters(nocoalitions, "Results/redo/total_dyads_supporters.pdf",
                             caption = "Total Dyads (salmon) and External Supporters (cyan) per year.")
###

###
count_support_types_isn(nocoalitions, "Results/redo/support_types_isn.pdf",
                        caption = "Total interventions per year supporting Government (aquamarine) and Rebel groups (snake green)")
###

###
support_abs_nc <- count_support(nocoalitions,
                                absolute = TRUE,
                                coalitions = FALSE)

figure4(support_abs_nc, "Results/redo/support_count_abs.pdf",
         caption = "changed colors:
        total count per year of dyads where external supporters
        supported only government (aquamarine4, triangles), only rebels (snakegreen, squares),
        both (black circles) or where no external actor intervened (grey routes).",
        share = FALSE)

# Added geom_line connecting data points and reduced their alpha for readability.
# ---
support_per_nc <- count_support(nocoalitions,
                                absolute = FALSE,
                                coalitions = FALSE)

figure4(support_per_nc, "Results/redo/support_count_rel.pdf",
        share = TRUE,
         caption = "colors changed")
# yearly share of dyads (out of all dyads that year) where external supporters 
# supported only government (biscuit, triangles), only rebels (gold, squares),
# both (black circles) or where no external actor intervened (grey routes).
# Added geom_line connecting data points and reduced their alpha for readability.
# Corrected the y-label to 'Share'
###

###
avg_conflicts(nocoalitions, "Results/redo/avg_conflicts.pdf",
              caption = "salmon: Average Conflicts per Actor (in how many conflicts an acotr intervenes on avg.),
              cyan: Average Actors per Conflict (how many actors intervene in a conflict on avg.)",
              upper_limit = 5)
###

###
most_interventionist(nocoalitions, "Results/redo/most_interventionist.pdf")
###

###
san <- load_san(nocoalitions)
dyadic_relation(san,
                  "United States of America",
                  "Russia (Soviet Union)",
                "Results/redo/usa_rus.pdf",
                caption = "line type:
                continued black line: actor relation;
                upper dashed line: Co-Interventions;
                lower dashed line: opposite interventions;
                
                relation type:
                gray = ambivalent,
                darkred = strictly negative,
                green = strictly positve
                "
)

dyadic_relation(san,
                "United States of America",
                "China",
                "Results/redo/usa_chn.pdf"
)

dyadic_relation(san,
                "United States of America",
                "Cuba",
                "Results/redo/usa_cub.pdf"
)

dyadic_relation(san,
                "United States of America",
                "Iran",
                "Results/redo/usa_irn.pdf"
)

dyadic_relation(san,
                "Russia (Soviet Union)",
                "Cuba",
                "Results/redo/rus_cub.pdf"
)

dyadic_relation(san,
                "Russia (Soviet Union)",
                "China",
                "Results/redo/rus_chn.pdf"
)
###

###
plot_mean_distance(nocoalitions, "Results/redo/mean_distance.pdf")
###

###
plot_relation_types_san(san, "Results/redo/relation_types.pdf",
                        caption = "
                        count how many of each relation type were there for every year.
                        
                        legend (relation type):
                        grey = ambivalent,
                        darkorange = strictly negative,
                        steelblue = strictly positive
                        ")
###

# disaggregated_san <- compute_dyadic_relationships(nocoalitions)
###
plot_count_ties_san(disaggregated_san, "Results/redo/count_ties_san.pdf",
                    "count ties-types in disaggregated san (i.e. 1 tie for every shared conflict.
                    
                    legend:
                    darkred = negative ties,
                    dargreen = positive ties,
                    black = total ties")
###

###
plot_total_nodes_san(disaggregated_san, "Results/redo/total_nodes_san.pdf")
###

###
area_bal_imb(imb, "Results/redo/area_bal_imb.pdf")
stacked_bal_imb(imb, "Results/redo/stacked_bal_imb.pdf")

stacked_imb_share(imb, "Results/redo/stacked_imb_share.pdf")
###

###
plot_san_net(san, 1985, "Results/redo/network_1985_san.pdf",
             legend.position = "none",
             caption = " colors adjusted,
             SAN in 1985,
             
             legend (edge type):
             red = negative balance,
             green = positive balance,
             grey = balanced")

plot_san_net(san, 1995, "Results/redo/network_1995_san.pdf",
             legend.position = "none",
             caption = "SAN in 1995, added weighted edges, same legend as for 1985")

plot_san_net(san, 2005, "Results/redo/network_2005_san.pdf",
             legend.position = "none",
             caption = "SAN in 2005, added weighted edges, same legend as for 1985")
###

###
plot_isn_net(nocoalitions, 1985, "Results/redo/isn_1985.pdf",
             legend.position = "none",
             caption = "ISN in 1985,
             
             legend:
             Node type:
             salmon square: dyad,
             cyan circle: intervening actor,
             
             Support type:
             aquamrine = government,
             snakegreen = rebel")

plot_isn_net(nocoalitions, 1995, "Results/redo/isn_1995.pdf",
             legend.position = "none")

plot_isn_net(nocoalitions, 2005, "Results/redo/isn_2005.pdf",
             legend.position = "none",
             caption = "removed legend from all network plots.")
