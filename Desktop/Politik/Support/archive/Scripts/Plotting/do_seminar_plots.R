# seminarplots
# redo all plots from the seminar - presentation but with clean functional code

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
###
count_total_dyads_supporters(isn, "Results/seminar/total_dyads_supporters.pdf", 160)
###

###
count_support_types_isn(isn, "Results/seminar/support_types_isn.pdf", 600)
###

###
support_abs_wc <- count_support(isn,
                                absolute = TRUE,
                                coalitions = TRUE)

figure4(support_abs_wc, "Results/seminar/support_count_abs.pdf")
# ---
support_per_wc <- count_support(isn,
                                absolute = FALSE,
                                coalitions = TRUE)

figure4(support_per_wc, "Results/seminar/support_count_rel.pdf")
###

###
avg_conflicts(isn, "Results/seminar/avg_conflicts.pdf", 8)
###

###
most_interventionist(isn, "Results/seminar/most_interventionist.pdf")
###

###
san_coal <- load_san(isn)
dyadic_relation(san_coal,
                "United States of America",
                "Russia (Soviet Union)",
                "Results/seminar/usa_rus.pdf"
)

dyadic_relation(san_coal,
                "United States of America",
                "China",
                "Results/seminar/usa_chn.pdf"
)

dyadic_relation(san_coal,
                "United States of America",
                "Cuba",
                "Results/seminar/usa_cub.pdf"
)

dyadic_relation(san_coal,
                "United States of America",
                "Iran",
                "Results/seminar/usa_irn.pdf"
)

dyadic_relation(san_coal,
                "Russia (Soviet Union)",
                "Cuba",
                "Results/seminar/rus_cub.pdf"
)

dyadic_relation(san_coal,
                "Russia (Soviet Union)",
                "China",
                "Results/seminar/rus_chn.pdf"
)
###

###
plot_mean_distance(isn, "Results/seminar/mean_distance.pdf")
###

###
plot_relation_types_san(san_coal, "Results/seminar/relation_types.pdf")
###

###
plot_count_ties_san(disaggregated_san, "Results/seminar/count_ties_san.pdf")
###

###
plot_total_nodes_san(disaggregated_san, "Results/seminar/total_nodes_san.pdf")
###

###
area_bal_imb(imb, "Results/seminar/area_bal_imb.pdf")
stacked_bal_imb(imb, "Results/seminar/stacked_bal_imb.pdf")

stacked_imb_share(imb, "Results/seminar/stacked_imb_share.pdf")
###

###
san_coal[, sign := sign(balance)]
plot_san_net(san_coal, 1985, "Results/seminar/network_1985_san.pdf")

plot_san_net(san_coal, 1995, "Results/seminar/network_1995_san.pdf")

plot_san_net(san_coal, 2005, "Results/seminar/network_2005_san.pdf")
###

###
plot_isn_net(isn, 1985, "Results/seminar/isn_1985.pdf")

plot_isn_net(isn, 1995, "Results/seminar/isn_1995.pdf")

plot_isn_net(isn, 2005, "Results/seminar/isn_2005.pdf")

