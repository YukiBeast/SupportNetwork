# bipolarity analysis: run smon_consensus to find avg. partition with k=2

epochs <- list(1975:1991, 1992:2001, 2002:2008, 2009:2017)

comm_all  <- list()

for (period in epochs) {
  epoch_label <- paste0(min(period), "-", max(period))
  
  san_filtered <- san[year %in% period, .(state1, state2, balance)]

  # consensus partition at k=2
  cons <- smon_consensus(san_filtered, k = 2, runs = 50)
  
  # tag with epoch and collect
  comm_all[[epoch_label]] <- cons$communities
  comm_all[[epoch_label]]$epoch <- epoch_label

  cat("epoch", epoch_label, "\n")
}

comm_epochs_bi <- rbindlist(comm_all)  # actors, community, prob, epoch
comm_epochs_bi[comm_epochs_bi$prob == 0]$community <- NA

comm_epochs_bi[!is.na(community), .(avg_prob = mean(prob)), by = c("epoch")]

plot_smon_ggraph(san[year %in% 2002:2008, .(state1, state2, balance)],
                 communities = comm_epochs_bi[epoch == "2002-2008", ])

