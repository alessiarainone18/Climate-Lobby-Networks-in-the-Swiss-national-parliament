# Load packages
library(tidyverse)
library(readxl)
library(lubridate) 
library(igraph)
library(dplyr)
library(purrr)
library(tidyr)


# Load datajets
mitunterzeichende <- read.csv("01-Data/mitunterzeichende.csv")
mitunterzechende_motionen <- read.csv("01-Data/mitunterzeichende_motionen.csv")
vorstoesse <- read_xlsx("01-Data/Vorstoesse.xlsx")
motionen <- read_xlsx("01-Data/Motionen.xlsx")

# Merge
merged_data_postulate_interpellation <- left_join(
  vorstoesse, 
  mitunterzeichende, 
  by = c("Link zur Geschäftsseite" = "url")
)

merged_data_motionen <- left_join(
  motionen, 
  mitunterzechende_motionen, 
  by = c("Link zur Geschäftsseite" = "url")
)

merged_data <- bind_rows(merged_data_postulate_interpellation, merged_data_motionen)


# Clean & Filter web scraped data (only from actual parliament 
# since October 2023 and only vortoesse with at least 2 mitunterzeichner)

merged_data$eingereicht_am <- dmy(merged_data$eingereicht_am)  


merged_data_sig_cleaned <- merged_data %>%
  filter(
    eingereicht_am >= as.Date("2023-10-01"),                       
    str_count(mitunterzeichner, ",") >= 1,                         
    !is.na(Urheber)                                                
  )


max_split <- max(str_count(merged_data_sig_cleaned$mitunterzeichner, ","), na.rm = TRUE) + 1
merged_data_split <- merged_data_sig_cleaned %>%
  separate(
    col = mitunterzeichner,
    into = paste0("unterzeichner", 1:max_split),
    sep = ",\\s*",
    fill = "right",  
    extra = "merge"  
  )

data_id <- read_csv("01-Data/data_id.csv")

# Liste aller relevanten Namensspalten
name_cols <- c("Urheber", paste0("unterzeichner", 1:46))

# Starte mit dem Original-Datensatz
vorstoesse_df <- merged_data_split

# Iteriere über alle Spalten und merge die ID
for (col in name_cols) {
  merged_data_split_id <- merged_data_split %>%
    left_join(
      data_id,
      by = setNames("parlamentarier_anzeige_name", col)
    ) %>%
    rename(!!paste0("id_", col) := parlamentarier_id)
}



## Umformen

####
library(tidyverse)

# Einlesen der ID-Daten
data_id <- read_csv("01-Data/data_id.csv")

# Liste aller Namensspalten
name_cols <- c("Urheber", paste0("unterzeichner", 1:46))

# Beginne mit dem Original-Datensatz
merged_data_split_id <- merged_data_split

# IDs zuordnen
for (col in name_cols) {
  merged_data_split_id <- merged_data_split_id %>%
    left_join(
      data_id,
      by = setNames("parlamentarier_anzeige_name", col)
    ) %>%
    rename(!!paste0("id_", col) := parlamentarier_id)
}

# IDs der Unterzeichner:innen extrahieren
# Stelle sicher, dass in den id_* Spalten wirklich IDs stehen (aus data_id)
id_cols <- grep("^id_", names(merged_data_split_id), value = TRUE)

# Konvertiere alle ID-Spalten zu Charakter, um Kombi-Probleme zu vermeiden
merged_data_split_id[id_cols] <- lapply(merged_data_split_id[id_cols], as.character)

# Jetzt richtig zusammenfügen
postulate_df <- merged_data_split_id %>%
  rowwise() %>%
  mutate(unterzeichner_ids = list(na.omit(c_across(all_of(id_cols))))) %>%
  ungroup()

write_csv(postulate_df, "01-Data/vorstoesse_common.csv")

# Alle ID-Kombinationen aus den unterzeichner_ids generieren
pair_df <- postulate_df %>%
  filter(lengths(unterzeichner_ids) > 1) %>%
  mutate(pairs = map(unterzeichner_ids, ~ {
    combn(sort(.x), 2, simplify = FALSE)  # Kombis als Liste von Vektoren
  })) %>%
  select(pairs) %>%
  unnest(pairs) %>%  # jede Paarung als eigene Zeile
  mutate(
    id1 = map_chr(pairs, 1),
    id2 = map_chr(pairs, 2)
  ) %>%
  select(id1, id2)

# Anzahl gemeinsamer Postulate zählen
pair_count_df <- pair_df %>%
  count(id1, id2, name = "gemeinsame_postulate")

write_csv(pair_count_df, "01-Data/gemeinsame_vorstoesse.csv")

