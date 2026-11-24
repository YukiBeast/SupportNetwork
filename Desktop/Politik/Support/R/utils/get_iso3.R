# ------------------------------------------------------------------
# Short node labels for network plots, legends and appendix lists.
#
# - governments:                 ISO3 code (countrycode), e.g. "RUS"
# - unofficial state "elements": ISO3 code + "-E", e.g. "Russian elements" -> "RUS-E"
#                                (distinct actors from the government)
# - non-state actors:            abbreviations from custom_match below
# - anything not matched:        the original name
#
# countrycode searches for country names *inside* a name, so every
# non-state actor whose name contains a country needs its own entry in
# custom_match; otherwise it would receive the country's code.
# Check for collisions with:
#   types <- unique(isn[!is.na(ext_name), .(name = ext_name)])
#   types[, label := get_iso3(name)][label %in% label[duplicated(label)]]
# ------------------------------------------------------------------
get_iso3 <- function(names_vec) {
  
  custom_match <- c(
    # --- historical / disputed / special states ---
    "Russia (Soviet Union)"                  = "RUS",
    "DR Congo (Zaire)"                       = "COD",
    "Vietnam (North Vietnam)"                = "VNM",
    "South Yemen"                            = "S-YMD",
    "Yemen (North Yemen)"                    = "N-YMD",
    "East Germany"                           = "DDR",
    
    # --- state elements countrycode does not recognize ---
    "Thai elements"                          = "THA-E",
    
    # --- long-form state entities and coalitions ---
    "Military Junta for the Consolidation of Democracy, Peace and Justice" = "MJCDPJ",
    "al-Moutasem Brigade, Authenticity and Development Front, Fistaqim Kama Umirat, Free Idlib Army, Hamza Division, Jaysh al-Nukhba, Kita'ib al-Safwat al-Islamiya, Levant Front, Liwa al-Sultan Murad, Sham Legion" = "TFSA",
    
    # --- non-state actors whose names contain a country ---
    "Hizb-i Islami-yi Afghanistan"           = "HIA",
    "Ittihad-i Islami Bara-yi Azadi-yi Afghanistan" = "IIA",
    "Indian Mujahideen"                      = "IM",
    "Puntland State of Somalia"              = "PUNT",
    "Republic of Somaliland"                 = "SOML",
    "Serbian Republic of Krajina"            = "RSK",
    "Serbian Republic of Bosnia-Herzegovina" = "RS",
    "APRDC"                                  = "APRDC",
    
    # --- other non-state armed groups and coalitions ---
    "Interahamwe, ex-FAR"                    = "INT/FAR",
    "Jemaah Islamiya"                        = "JI",
    "Lashkar-e-Taiba"                        = "LeT",
    "Lashkar-e-Islam"                        = "LeI",
    "Jamaat-ul-Ahrar"                        = "JuA",
    "al-Qaida"                               = "AQ",
    "Ansar al-Islam"                         = "AAI",
    "Ansar Dine"                             = "AD",
    "Ansar Bayt al-Maqdis"                   = "ABM",
    "Abdullah Azzam Brigades"                = "AAB",
    "Benghazi Revolutionaries Shura Council" = "BRSC",
    "Liwa Ahrar al-Sunna in Baalbek"         = "LASB",
    "Jama'atu Ahlis Sunna Lidda'awati wal-Jihad" = "BOKO",
    "Signed-in-Blood Battalion"              = "SiB",
    "Kashmir insurgents"                     = "KASH",
    "Achik Special Dragon Party"             = "ASDP",
    "Raia Mutomboki"                         = "RM",
    "Agwelek Forces"                         = "AGW",
    "Nyatura Domi -FPC"                      = "NYA-D",
    "Nyatura Love"                           = "NYA-L",
    "Groupe de Sécurité (Nyatura Kasongo)"   = "NYA-K"
  )
  
  iso3 <- countrycode::countrycode(
    sourcevar    = names_vec,
    origin       = "country.name",
    destination  = "iso3c",
    custom_match = custom_match,
    warn         = FALSE
  )
  
  # unofficial state elements: country code + "-E"
  is_element <- grepl(" elements$", names_vec) &
    !(names_vec %in% names(custom_match)) &
    !is.na(iso3)
  iso3[is_element] <- paste0(iso3[is_element], "-E")
  
  # fall back to the original name if nothing matched
  ifelse(is.na(iso3), names_vec, iso3)
}