library(data.table)
imbalance_dt <- read.csv2("Data/imbalance_contributions.csv")
imbalance_dt <- as.data.table(imbalance_dt)
imbalance_dt[, X := NULL]

# add state attributes:
library(vdemdata)
vdem <- as.data.table(vdemdata::vdem)
vdem <- vdem[, .(
  country_text_id,
  "state" = country_name,
  year,
  "democ" = v2x_regime,
  v2caviol,
  v2cacamps,
  v2svstterr,
  anoc = as.numeric(v2x_regime == 1)
  )]

library(countrycode)
vdem[, continent := countrycode(sourcevar = country_text_id, 
                                   origin = "iso3c", 
                                   destination = "continent")]

# Falls die Zuordnung nicht perfekt ist, kannst du auch "region" nutzen:
vdem[, region := countrycode(sourcevar = country_text_id, 
                                origin = "iso3c", 
                                destination = "region")]

imbalance_dt <- vdem[imbalance_dt, on = c("state", "year")]
unique(imbalance_dt$region)

imbalance_dt[, is_state := ifelse(!is.na(region), 1, 0)]

known_states <- c(
  "Turkey", "Congo", "Czech Republic", "Bosnia-Herzegovina", "Gambia", "Brunei",
  "Myanmar (Burma)", "Vietnam (North Vietnam)", "Yemen (North Yemen)",
  "Cambodia (Kampuchea)", "Russia (Soviet Union)", "DR Congo (Zaire)",
  "Serbia (Yugoslavia)", "Zimbabwe (Rhodesia)"
)

# 1. Deine Liste als data.table
mapping_dt <- data.table(state_name = known_states)

# 2. Mappe Kontinente und Regionen
# 'origin' wird automatisch erkannt, wir nutzen 'country.name'
mapping_dt[, continent := countrycode(state_name, origin = 'country.name', destination = 'continent')]
mapping_dt[, region := countrycode(state_name, origin = 'country.name', destination = 'region')]

imbalance_dt[, region := fcase(
  # Manuelle Korrektur für historische/spezielle Staaten
  state %in% c("Russia (Soviet Union)", "Serbia (Yugoslavia)", "Bosnia-Herzegovina", "Turkey"), "Europe & Central Asia",
  state %in% c("Vietnam (North Vietnam)", "Myanmar (Burma)", "Cambodia (Kampuchea)", "Brunei"), "East Asia & Pacific",
  state %in% c("DR Congo (Zaire)", "Congo", "Zimbabwe (Rhodesia)", "Gambia"), "Sub-Saharan Africa",
  state %in% c("Yemen (North Yemen)"), "Middle East & North Africa",
  
  # Standard-Mapping korrigieren (countrycode Namen auf deine Liste mappen)
  region == "Northern America", "North America",
  region == "South Asia", "South Asia",
  region == "Latin America & the Caribbean", "Latin America & Caribbean",
  
  # Wenn immer noch NA, dann setzen wir es auf "Unknown"
  default = region
)]

# 3. Falls historische Namen nicht gefunden werden (manuelle Korrektur)
# countrycode hilft hier schon sehr viel, aber bei "Yemen (North Yemen)" 
# oder "Vietnam (North Vietnam)" müssen wir manchmal nachhelfen:
mapping_dt[state_name == "Vietnam (North Vietnam)", continent := "Asia"]
mapping_dt[state_name == "Yemen (North Yemen)", continent := "Asia"]
mapping_dt[state_name == "Serbia (Yugoslavia)", continent := "Europe"]
mapping_dt[state_name == "Russia (Soviet Union)", continent := "Europe"] # oder Asia, je nach Fokus
mapping_dt[state_name == "Congo", continent := "Africa"]

# 4. Den kompletten Datensatz mergen (Angenommen dein Haupt-DT heißt imbalance_dt)
imbalance_dt[mapping_dt, on = .(state = state_name), `:=`(continent = i.continent, region = i.region)]

# 2. Korrigiere die is_state Spalte
# Wir setzen is_state auf 1, wenn der Name in der Liste ist, sonst 0 (oder basierend auf einer anderen Logik)
imbalance_dt[is_state == 0, is_state := ifelse(state %in% known_states, 1, 0)]

imbalance_dt[, v2caviol_cat := cut(v2caviol, 
                                   breaks = c(-Inf, -1, 1, Inf), 
                                   labels = c("Low", "Medium", "High"),
                                   include.lowest = TRUE)]

# Liste der NATO-Mitglieder mit ihrem Beitrittsjahr
nato_data <- data.table(
  state_name = c("United States of America", "Canada", "United Kingdom", "France", "Norway", 
                 "Denmark", "Iceland", "Italy", "Portugal", "Netherlands", "Belgium", 
                 "Luxembourg", "Turkey", "Greece", "Germany", "Spain", "Czech Republic", 
                 "Hungary", "Poland", "Bulgaria", "Estonia", "Latvia", "Lithuania", 
                 "Romania", "Slovakia", "Slovenia", "Albania", "Croatia", "Montenegro", 
                 "North Macedonia", "Finland", "Sweden"),
  joined = c(1949, 1949, 1949, 1949, 1949, 1949, 1949, 1949, 1949, 1949, 1949, 
             1949, 1952, 1952, 1955, 1982, 1999, 1999, 1999, 2004, 2004, 2004, 
             2004, 2004, 2004, 2004, 2009, 2009, 2017, 2020, 2023, 2024)
)


# Merge der NATO-Informationen
# Hinweis: Du musst sicherstellen, dass die Namen in 'state' den Namen in 'nato_data' entsprechen
imbalance_dt[nato_data, on = .(state = state_name), nato_joined := i.joined]

# Erstelle die Variable is_nato
imbalance_dt[, is_nato := ifelse(!is.na(nato_joined) & year >= nato_joined, "YES", "NO")]

# Jetzt die übergeordnete Kategorie setzen:
# Wenn es ein Staat ist, bleibt es YES/NO, sonst "NON STATE"
# Annahme: Du hast eine Spalte 'is_state' (1 = Staat, 0 = Nicht-Staat)
imbalance_dt[is_state == 0, actor_type := "NON STATE"]
imbalance_dt[is.na(region), actor_type := "NON STATE"]
imbalance_dt[is_state == 1 & is_nato == "YES", actor_type := "YES"] # NATO-Staat
imbalance_dt[is_state == 1 & is_nato == "NO", actor_type := "NO"]   # Nicht-NATO-Staat

imbalance_dt[, actor_type := factor(actor_type, levels = c("YES", "NO", "NON STATE"))]

imb <-
  imbalance_dt[, .(country_iso = country_text_id,
                   state,
                   year,
                   imb_per_state,
                   imb_per_total,
                   democ,
                   anoc,
                   continent,
                   region,
                   is_state,
                   nato = actor_type)]

rm(imbalance_dt, nato_data, vdem)

imb[state == "Russia (Soviet Union)", country_iso := "RUS"]
data.table::nafill()
setnafill(imb, cols = c("imb_per_state", "imb_per_total"), fill = 0)
