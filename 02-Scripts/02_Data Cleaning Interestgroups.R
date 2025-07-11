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

# ---------------------------------------------
# 1. IDs extrahieren
# ---------------------------------------------
data_id <- cleaned_data %>%
  select(parlamentarier_anzeige_name, parlamentarier_id) %>%
  mutate(parlamentarier_anzeige_name = gsub(",", "", parlamentarier_anzeige_name)) %>%
  distinct()

write_csv(data_id, "01-Data/data_id.csv")

# ---------------------------------------------
# 2. Subcode wide (optional, für spätere Analysen)
# ---------------------------------------------
subcode_wide <- cleaned_data %>%
  select(parlamentarier_id, Subcode) %>%
  filter(!is.na(Subcode)) %>%
  distinct() %>%
  group_by(parlamentarier_id) %>%
  mutate(subcode_num = paste0("subcode_", row_number())) %>%
  ungroup() %>%
  pivot_wider(names_from = subcode_num, values_from = Subcode)

# ---------------------------------------------
# 3. Gemeinsame Organisationen
# ---------------------------------------------
organisation_df <- cleaned_data %>%
  select(organisation_uid, parlamentarier_id) %>%
  filter(!is.na(organisation_uid), !is.na(parlamentarier_id)) %>%
  distinct()

org_ids_df <- organisation_df %>%
  group_by(organisation_uid) %>%
  summarise(parlamentarier_ids = list(sort(unique(parlamentarier_id))), .groups = "drop") %>%
  filter(lengths(parlamentarier_ids) > 1)

pair_df <- org_ids_df %>%
  mutate(pairs = map(parlamentarier_ids, ~ combn(.x, 2, simplify = FALSE))) %>%
  select(pairs) %>%
  unnest(pairs) %>%
  mutate(
    id1 = map_chr(pairs, 1),
    id2 = map_chr(pairs, 2)
  ) %>%
  select(id1, id2)

pair_count_organisation <- pair_df %>%
  count(id1, id2, name = "gemeinsame_organisationen")

write_csv(pair_count_organisation, "01-Data/gemeinsame_organisationen.csv")

# ---------------------------------------------
# 4. Interessen Kategorisiert (Subcode)
# ---------------------------------------------
subinteresse_df <- cleaned_data %>%
  select(Subcode, parlamentarier_id) %>%
  filter(!is.na(Subcode), !is.na(parlamentarier_id)) %>%
  distinct()

subcode_ids_df <- subinteresse_df %>%
  group_by(Subcode) %>%
  summarise(parlamentarier_ids = list(sort(unique(parlamentarier_id))), .groups = "drop") %>%
  filter(lengths(parlamentarier_ids) > 1)

subcode_pair_df <- subcode_ids_df %>%
  mutate(pairs = map(parlamentarier_ids, ~ combn(.x, 2, simplify = FALSE))) %>%
  select(pairs) %>%
  unnest(pairs) %>%
  mutate(
    id1 = map_chr(pairs, 1),
    id2 = map_chr(pairs, 2)
  ) %>%
  select(id1, id2)

pair_count_subcode <- subcode_pair_df %>%
  count(id1, id2, name = "gemeinsame_subinteressen")

write_csv(pair_count_subcode, "01-Data/gemeinsame_subinteressen.csv")


# Beispiel-Datensatz (bitte bei dir durch deinen ersetzen!)
partei_df <- cleaned_data %>%
  select(parlamentarier_id, parlamentarier_partei) %>%
  distinct() %>%
  drop_na()

# Erzeuge alle Kombinationen von IDs (ohne Wiederholung)
id_combos <- t(combn(partei_df$parlamentarier_id, 2)) %>%
  as_tibble() %>%
  rename(id1 = V1, id2 = V2)

# Parteien für beide IDs mergen
id_partei_merge <- id_combos %>%
  left_join(partei_df, by = c("id1" = "parlamentarier_id")) %>%
  rename(partei1 = parlamentarier_partei) %>%
  left_join(partei_df, by = c("id2" = "parlamentarier_id")) %>%
  rename(partei2 = parlamentarier_partei)

# Prüfen ob gleich
id_partei_merge <- id_partei_merge %>%
  mutate(gleiche_partei = if_else(partei1 == partei2, 1, 0)) %>%
  select(id1, id2, gleiche_partei)

write_csv(id_partei_merge, "01-Data/gleiche_partei.csv")


## Kommission
# 1. ID + Kommission vorbereiten
kommission_df <- cleaned_data %>%
  select(parlamentarier_id, parlamentarier_kommissionen) %>%
  filter(!is.na(parlamentarier_kommissionen), parlamentarier_kommissionen != "") %>%
  mutate(
    parlamentarier_kommissionen = str_replace_all(parlamentarier_kommissionen, "\\s+", ""),
    parlamentarier_kommissionen = str_remove_all(parlamentarier_kommissionen, "\\.$|,$|\\s*$|^\\s*")
  ) %>%
  separate_rows(parlamentarier_kommissionen, sep = ",") %>%
  filter(parlamentarier_kommissionen != "" & !is.na(parlamentarier_kommissionen)) %>%
  distinct()

# 2. Erzeuge alle Kombinationen von IDs
alle_ids <- kommission_df %>% pull(parlamentarier_id) %>% unique()
id_combos <- t(combn(alle_ids, 2)) %>%
  as_tibble() %>%
  rename(id1 = V1, id2 = V2)

# 3. Kommissionen pro ID listen
kom_list <- kommission_df %>%
  group_by(parlamentarier_id) %>%
  summarise(kommissionen = list(parlamentarier_kommissionen), .groups = "drop")

# 4. Join Kommissionen zu beiden IDs
id_kom_merged <- id_combos %>%
  left_join(kom_list, by = c("id1" = "parlamentarier_id")) %>%
  rename(kom1 = kommissionen) %>%
  left_join(kom_list, by = c("id2" = "parlamentarier_id")) %>%
  rename(kom2 = kommissionen)

# 5. Prüfen, ob Schnittmenge vorhanden
id_kom_merged <- id_kom_merged %>%
  mutate(
    n_gemeinsame_kommissionen = map2_int(kom1, kom2, ~ length(intersect(.x, .y)))
  ) %>%
  select(id1, id2, n_gemeinsame_kommissionen)

# 6. Optional speichern
write_csv(id_kom_merged, "01-Data/gemeinsame_kommissionen.csv")



