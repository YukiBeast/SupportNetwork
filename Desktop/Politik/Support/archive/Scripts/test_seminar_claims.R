library(data.table)
isn <- as.data.table(load_support_coalitions())
# Assicuriamoci di usare il dataset corretto (escludendo gli anni dopo il 2017 se presenti)
isn <- isn[year >= 1975 & year <= 2017]

cat("=== TESTING CLAIM 1: 80% OF CONFLICTS RECEIVED SUPPORT ===\n")
# Claim: "Between 1975 and 2017 in around 80 percent of all conflicts... received external support"
total_dyads <- uniqueN(isn$dyad_id)
dyads_with_support <- uniqueN(isn[!is.na(ext_name), dyad_id])
share_support <- round((dyads_with_support / total_dyads) * 100, 2)
cat("Total dyads:", total_dyads, "\n")
cat("Dyads with support:", dyads_with_support, "\n")
cat("Share:", share_support, "%\n\n")


cat("=== TESTING CLAIM 2 & 5: USA IN 1/3 OF CIVIL WARS & 83 DOMESTIC BELLIGERENTS ===\n")
# Claim: "The United States alone intervened in more than one third of all civil wars... supporting 83 distinct domestic belligerents"
usa_data <- isn[ext_name == "United States of America"]
usa_dyads <- uniqueN(usa_data$dyad_id)
usa_share <- round((usa_dyads / total_dyads) * 100, 2)
usa_total_actors <- uniqueN(usa_data$actor_name)

cat("USA intervened in:", usa_dyads, "conflicts\n")
cat("USA share of total conflicts:", usa_share, "%\n")
cat("Total distinct domestic belligerents supported by USA:", usa_total_actors, "\n\n")


cat("=== TESTING CLAIM 3 & 4: USA BACKED 56 GOVERNMENTS & 27 REBELS ===\n")
# Claim: "...backing 56 different governments... while also supporting 27 distinct rebel groups"
# signed_support == 1 -> government, signed_support == -1 -> rebels
usa_gov_actors <- uniqueN(usa_data[signed_support == 1, actor_name])
usa_reb_actors <- uniqueN(usa_data[signed_support == -1, actor_name])

cat("USA backed", usa_gov_actors, "governments\n")
cat("USA backed", usa_reb_actors, "rebel groups\n\n")


cat("=== TESTING CLAIM 6 & 7: USA FACED 145 STATES & 85 NON-STATE ACTORS ===\n")
# Claim: "...they faced the astonishing number of 145 different states... 85 distinct non-state actor"
# Dobbiamo trovare tutti i conflitti (dyad_id) in cui c'erano gli USA, e poi contare gli ALTRI attori esterni.
usa_dyad_list <- unique(usa_data$dyad_id)
actors_facing_usa <- isn[dyad_id %in% usa_dyad_list & 
                           ext_name != "United States of America" & 
                           !is.na(ext_name)]

states_faced <- uniqueN(actors_facing_usa[ext_nonstate == 0, ext_name])
nonstates_faced <- uniqueN(actors_facing_usa[ext_nonstate == 1, ext_name])

cat("USA faced", states_faced, "distinct state actors\n")
cat("USA faced", nonstates_faced, "distinct non-state actors\n\n")


cat("=== TESTING CLAIM 8 & 11: ANGOLA (USA/ZAF vs CUB/RUS) ===\n")
# Claim: USA & South Africa supported FNLA/UNITA; Cuba & Soviet Union supported MPLA (Government)
angola_check <- isn[dyad_country == "Angola" & 
                      ext_name %in% c("United States of America", "South Africa", "Cuba", "Russia (Soviet Union)"),
                    .(actor_name, ext_name, signed_support)]
# Mostra chi ha supportato chi, rimuovendo i duplicati
print(unique(angola_check[order(signed_support)]))
cat("\n")


cat("=== TESTING CLAIM 9 & 12: SRI LANKA (USA/PAK vs IND/PRK) ===\n")
# Claim: USA & Pakistan supported Gov in 1980s & 2000s; India & North Korea supported LTTE
sri_lanka_check <- isn[dyad_country == "Sri Lanka (Ceylon)" & 
                         ext_name %in% c("United States of America", "Pakistan", "India", "North Korea"),
                       .(year, actor_name, ext_name, signed_support)]
sri_lanka_check[, decade := paste0(floor(year / 10) * 10, "s")]
print(unique(sri_lanka_check[, .(actor_name, ext_name, signed_support, decade)][order(signed_support)]))
cat("\n")


cat("=== TESTING CLAIM 10: AFGHAN ALLIANCE OF 51 STATES POST-9/11 ===\n")
# Claim: "...USA managed to form an alliance of up to 51 other states... in support of the new Afghan government"
# (Nota: Il tuo codice originariamente esclude le coalizioni 'ISAF', quindi qui controlliamo l'anno di picco dei supporter individuali o la somma)
afghan_gov_supporters <- isn[dyad_country == "Afghanistan" & 
                               year >= 2001 & 
                               signed_support == 1 & 
                               ext_nonstate == 0 & 
                               ext_name != "United States of America" &
                               !is.na(ext_name)]

# Anno con il massimo numero di stati supportanti
max_afghan_states <- afghan_gov_supporters[, .(num_states = uniqueN(ext_name)), by = year]
cat("Peak number of allied states supporting Afghan Gov post-2001 in a single year:\n")
print(max_afghan_states[order(-num_states)][1, ])
cat("\n")


cat("=== TESTING CLAIM 13: TALEBAN SUPPORT (AL-QAIDA, ETIM, LeT) ===\n")
# Claim: "...the Taleban received support from multiple external non-state actors, including al-Qaida, ETIM, Lashkar-e-Taiba"
taleban_supporters <- isn[dyad_country == "Afghanistan" & 
                            grepl("Taleban", actor_name, ignore.case = TRUE) & 
                            ext_nonstate == 1 &
                            !is.na(ext_name),
                          .(ext_name)]
# Stampiamo solo quelli menzionati per conferma, più il numero totale
cat("Total non-state actors supporting Taleban:", uniqueN(taleban_supporters$ext_name), "\n")
cat("Did they include Al-Qaida, ETIM, or LeT? \n")
print(unique(taleban_supporters[grepl("Qaida|Turkistan|Lashkar", ext_name)]))
cat("\n")