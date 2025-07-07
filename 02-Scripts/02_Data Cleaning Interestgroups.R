# Load packages
library(tidyverse)
library(readxl)
library(writexl)
library(lubridate) 

# Load datasets
interestgroups_coded <- read_xlsx("01-Data/df_merged_cleaned.xlsx", sheet = "subgruppen")
people <- read_xlsx("01-Data/df_merged_cleaned.xlsx", sheet = "Personen")

people_cleaned <- people %>%
  select(-interessengruppe_einfluss)

merged_data_interests <- left_join(
  people_cleaned,
  interestgroups_coded, by = c("organisation_name_de" = "Organisation"))

## only relevant
cleaned_data <- merged_data_interests %>%
  filter(Hauptcode != 99) %>%
  mutate(
    Subcode = if_else(is.na(Subcode), Hauptcode, Subcode))


# Save the cleaned data
write_xlsx(cleaned_data, "01-Data/df_merged_cleaned_interests.xlsx")
write_csv(cleaned_data, "01-Data/df_merged_cleaned_interests.csv")

# Extract ID
data_id <- cleaned_data %>%
  select(parlamentarier_anzeige_name, parlamentarier_id) %>%
  mutate(parlamentarier_anzeige_name = gsub(",", "", parlamentarier_anzeige_name)) %>%
  distinct()

write_csv(data_id, "01-Data/data_id.csv")

## Reorganising
library(dplyr)
library(tidyr)

# 1. Nur relevante Spalten, keine NAs
subcode_wide <- cleaned_data %>%
  select(parlamentarier_id, Subcode) %>%
  filter(!is.na(Subcode)) %>%
  distinct()  # Doppelte Einträge raus

# 2. Laufende Nummer für jede Subcode pro Person
subcode_wide <- subcode_wide %>%
  group_by(parlamentarier_id) %>%
  mutate(subcode_num = paste0("subcode_", row_number())) %>%
  ungroup()

# 3. In breite Form pivotieren
subcode_wide <- subcode_wide %>%
  pivot_wider(
    names_from = subcode_num,
    values_from = Subcode
  )



# 1. Relevante Spalten extrahieren
interesse_df <- cleaned_data %>%
  select(organisation_uid, parlamentarier_id) %>%
  filter(!is.na(organisation_uid), !is.na(parlamentarier_id)) %>%
  distinct()

# 2. IDs pro Organisation gruppieren
org_ids_df <- interesse_df %>%
  group_by(organisation_uid) %>%
  summarise(parlamentarier_ids = list(sort(unique(parlamentarier_id))), .groups = "drop")

# 3. Nur Gruppen mit mindestens 2 Personen
org_ids_df <- org_ids_df %>%
  filter(lengths(parlamentarier_ids) > 1)

# 4. Kombiniere pro Organisation alle möglichen Paare
pair_df <- org_ids_df %>%
  mutate(pairs = map(parlamentarier_ids, ~ combn(.x, 2, simplify = FALSE))) %>%
  select(pairs) %>%
  unnest(pairs) %>%
  mutate(
    id1 = map_chr(pairs, 1),
    id2 = map_chr(pairs, 2)
  ) %>%
  select(id1, id2)

# 5. Zähle wie oft jedes ID-Paar gemeinsam Interessenbindungen hat
pair_count_interessen <- pair_df %>%
  count(id1, id2, name = "gemeinsame_interessenbindungen")

# 6. Speichern
write_csv(pair_count_interessen, "01-Data/gemeinsame_interessen.csv")
