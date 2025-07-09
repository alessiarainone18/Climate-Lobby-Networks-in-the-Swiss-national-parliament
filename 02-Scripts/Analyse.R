# Analyse
library(tidyverse)
library(readxl)
library(lubridate) 
library(igraph)
library(dplyr)
library(purrr)
library(tidyr)
library(car)
library(statnet) 

gemeinsame_vorstoesse <- read_csv("01-Data/gemeinsame_vorstoesse.csv")

# Hilfsfunktion zum Sortieren von ID-Paaren
sort_ids <- function(df) {
  df %>%
    mutate(
      id_min = pmin(id1, id2),
      id_max = pmax(id1, id2)
    ) %>%
    select(-id1, -id2) %>%                # Alte id1 und id2 entfernen
    rename(id1 = id_min, id2 = id_max) %>%  # Neue id1 und id2 setzen
    select(id1, id2, everything()) %>%
    distinct()
}

# Einlesen & Sortieren
gemeinsame_vorstoesse <- read_csv("01-Data/gemeinsame_vorstoesse.csv") %>%
  rename(gemeinsame_vorstoesse = gemeinsame_postulate) %>%
  sort_ids()

gemeinsame_organisationen <- read_csv("01-Data/gemeinsame_organisationen.csv") %>% sort_ids()
gemeinsame_hauptinteressen <- read_csv("01-Data/gemeinsame_hauptinteressen.csv") %>% sort_ids()
gemeinsame_subinteressen <- read_csv("01-Data/gemeinsame_subinteressen.csv") %>% sort_ids()
gleiche_partei <- read_csv("01-Data/gleiche_partei.csv") %>% sort_ids()
gemeinsame_kommissionen <- read_csv("01-Data/gemeinsame_kommissionen.csv") %>% sort_ids()

### 3. Alle Ebenen zusammenführen ----

# Alle ID-Kombinationen aus den vier Dateien
all_pairs <- bind_rows(
  gemeinsame_vorstoesse %>% select(id1, id2),
  gemeinsame_organisationen %>% select(id1, id2),
  gleiche_partei %>% select(id1, id2),
  gemeinsame_kommissionen %>% select(id1, id2),
  gemeinsame_hauptinteressen %>% select(id1, id2),
  gemeinsame_subinteressen %>% select(id1, id2)
) %>%
  distinct()


# Jetzt alle Daten joinen
df_all <- all_pairs %>%
  left_join(gemeinsame_vorstoesse %>% rename(n_vorstoesse = gemeinsame_vorstoesse), by = c("id1", "id2")) %>%
  left_join(gemeinsame_organisationen %>% rename(n_organisationen = gemeinsame_organisationen), by = c("id1", "id2")) %>%
  left_join(gemeinsame_hauptinteressen %>% rename(n_hauptinteressen = gemeinsame_hauptinteressen), by = c("id1", "id2")) %>%
  left_join(gemeinsame_subinteressen %>% rename(n_subinteressen = gemeinsame_subinteressen), by = c("id1", "id2")) %>%
  left_join(gleiche_partei, by = c("id1", "id2")) %>%
  left_join(gemeinsame_kommissionen %>% rename(n_kommissionen = n_gemeinsame_kommissionen), by = c("id1", "id2")) %>%
  mutate(
    n_vorstoesse = replace_na(n_vorstoesse, 0),
    n_organisationen = replace_na(n_organisationen, 0),
    n_hauptinteressen = replace_na(n_hauptinteressen, 0),
    n_subinteressen = replace_na(n_subinteressen, 0),
    gleiche_partei = replace_na(gleiche_partei, 0)
  )

write_csv(df_all, "01-Data/alle_beziehungen.csv")

## 1 Homophilie
# H1_A: Ähnliche Interessensvertretungen führen zu höherer Wahrscheinlichkeit zu Koopeartion, kontrolliert für Partei, Kommission und Sprache.
model_1a <- glm(n_vorstoesse ~ n_subinteressen, family = poisson(link = "log"), data = df_all)
summary(model_1a)
exp(coef(model_1a)["n_subinteressen"])

model_1b <- glm(n_vorstoesse ~  n_subinteressen + gleiche_partei + n_kommissionen, family = poisson(link = "log"), data = df_all)
summary(model_1b)


# H1_B: Kumulativer Homophilie Effekt: Je mehr Gemeinsamkeiten, desto höher die WAhrscheinlichkeit der Koopeartion.
df_all$gleiche_kommission <- ifelse(df_all$n_kommissionen >= 1, 1, 0)
df_all$gleiches_subinteresse <- ifelse(df_all$n_subinteressen >= 1, 1, 0)
df_all$homophilie_count <- df_all$gleiche_partei + df_all$gleiche_kommission + df_all$gleiches_subinteresse
model_1c <- glm(n_vorstoesse ~ homophilie_count, family = poisson(link = "log"), data = df_all)
summary(model_1c)
exp(coef(model_1c)["homophilie_count"])

## H2 Principal Agent Theorie
# H2_A: Bei spezifischer Organisation, höhere Kooperation
cor(df_all$n_subinteressen, df_all$n_organisationen, use = "complete.obs")

model_2a <- glm(n_vorstoesse ~ n_organisationen + gleiche_partei + n_kommissionen, family = poisson(link = "log"), data = df_all)
summary(model_2a)
exp(coef(model_2a)["n_organisationen"])


# H2_B: Wenn sie mehrere gemeinsame Organisationen vertreten, ist die Kooperation noch stärker.
df_all <- df_all %>%
  mutate(
    anzahl_gemeinsame_organisationen = n_organisationen,  # falls noch nicht vorhanden
    organisationen_kat = case_when(
      anzahl_gemeinsame_organisationen == 0 ~ "keine",
      anzahl_gemeinsame_organisationen == 1 ~ "eine",
      anzahl_gemeinsame_organisationen > 1  ~ "mehrere",
      TRUE ~ NA_character_
    ),
    organisationen_kat = factor(organisationen_kat, levels = c("keine", "eine", "mehrere"))
  )
model_2b <- glm(n_vorstoesse ~ organisationen_kat + gleiche_partei + n_kommissionen,
  data = df_all,
  family = poisson(link = "log"))
summary(model_2b)

linearHypothesis(model_2b, "organisationen_katmehrere - organisationen_kateine = 0")

# H3_A: Die Erklärungskraft des Principal-Agent-Ansatzes und der Homophilie unterscheidet sich hinsichtlich ihrer Wirkung auf politische Kooperation.
model_3a <- glm(n_vorstoesse ~ n_organisationen + homophilie_count,
                data = df_all,
                family = poisson(link = "log"))
summary(model_3a)

linearHypothesis(model_3a, "n_organisationen - homophilie_count = 0")

# H3_B: Es besteht eine Wechselwirkung zwischen Homophilie und Principal-Agent-Verbindungen bei der Erklärung politischer Kooperation.
model_3b <- glm(n_vorstoesse ~ n_organisationen * homophilie_count,
                data = df_all,
                family = poisson(link = "log"))
summary(model_3b)

## Netzwerkanalyse


