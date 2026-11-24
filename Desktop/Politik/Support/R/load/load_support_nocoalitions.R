load_support_nocoalitions <- function() {

  library(readxl)
  library(data.table)

  support <- read_excel("Data/raw/ucdp-esd-ty-181.xlsx")
  support <- as.data.table(support)
  support <- support[civil == 1 & (is.na(ext_coalition) | ext_coalition == 0), ]
  
  
  # Pre-filter NAs to avoid the `.SD[, -c("NA")]` crash later
  clean_support <- support[ext_bothsides == 0 | is.na(ext_bothsides), ]
  clean_support[, signed_support := ifelse(actor_nonstate == 1, -1, 1)]
  
  nocoalitions <- clean_support[, .(year, dyad_id,
                                    dyad_country = country_a,
                                    actor_name = gsub("Government of ", "", actor_name),
                                    actor_nonstate,
                                    ext_name = gsub("Government of ", "", ext_name),
                                    ext_nonstate,
                                    ext_coalition,
                                    ext_sup,
                                    signed_support,
                                    troups_or_weapons = ifelse(is.na(ext_name), NA,
                                                               as.numeric(ext_x | ext_w)))]

  return(nocoalitions)
}

isn_nocoalitions <- load_support_nocoalitions()
saveRDS(isn_nocoalitions, file = "Data/isn_nocoalitions.rds")
