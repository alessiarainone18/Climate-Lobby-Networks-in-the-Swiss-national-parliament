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
library(MASS)
library(pscl)
library(emmeans)
library(margins)
library(glmmTMB)
library(sjPlot)
library(HYPEtools)

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
gemeinsame_interessen <- read_csv("01-Data/gemeinsame_interessen.csv") %>% sort_ids()
gleicher_kanton <- read_csv("01-Data/gleicher_kanton.csv") %>% sort_ids()

### 3. Alle Ebenen zusammenführen ----

# Alle ID-Kombinationen aus den vier Dateien
all_pairs <- bind_rows(
  gemeinsame_vorstoesse %>% select(id1, id2),
  gemeinsame_organisationen %>% select(id1, id2),
  gleiche_partei %>% select(id1, id2),
  gemeinsame_kommissionen %>% select(id1, id2),
  gemeinsame_hauptinteressen %>% select(id1, id2),
  gemeinsame_subinteressen %>% select(id1, id2),
  gemeinsame_interessen %>% select(id1, id2),
  gleicher_kanton %>% select(id1, id2)
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
  left_join(gemeinsame_interessen %>% rename(n_interessen = gemeinsame_interessen), by = c("id1", "id2")) %>%
  left_join(gleicher_kanton, by = c("id1", "id2")) %>%
  mutate(
    n_vorstoesse = replace_na(n_vorstoesse, 0),
    n_organisationen = replace_na(n_organisationen, 0),
    n_hauptinteressen = replace_na(n_hauptinteressen, 0),
    n_subinteressen = replace_na(n_subinteressen, 0),
    gleiche_partei = replace_na(gleiche_partei, 0),
    n_interessen = replace_na(n_interessen, 0),
    gleicher_kanton = replace_na(gleiche_kanton, 0)
  )

write_csv(df_all, "01-Data/alle_beziehungen.csv")

## Modell Test----
# Poisson
model_poisson <- glm(n_vorstoesse ~ n_interessen, family = poisson(link = "log"), data = df_all)
summary(model_poisson)
# Quasi Poisson
model_qpoisson = glm(n_vorstoesse ~ n_interessen + gleiche_partei + n_kommissionen + gleicher_kanton, family = "quasipoisson",
                     data = df_all)
summary(model_qpoisson)
# Negativ Binomial
model_nb <- glm.nb(n_vorstoesse ~  n_interessen + gleiche_partei + n_kommissionen + gleicher_kanton, data = df_all)
summary(model_nb)

# Test: Quasi Poisson vs. Poisson
odTest(model_nb)

# Visual Test: Quasi Poisson vs. Negativ Binomial
yhat <- predict(model_nb)                      
y_obs <- model_nb$y                            

breaks  <- unique(quantile(yhat, seq(0, 1, 0.1), na.rm = TRUE))
breaks[1] <- -Inf                              
groups <- cut(yhat,
              breaks = breaks,
              include.lowest = TRUE)           

m <- tapply(y_obs, groups, mean, na.rm = TRUE)
v <- tapply(y_obs, groups, var , na.rm = TRUE)

pr  <- residuals(model_poisson, type = "pearson")
phi <- sum(pr^2) / df.residual(model_poisson)

x <- seq(min(m, na.rm = TRUE), max(m, na.rm = TRUE), length.out = 500)
line_data <- data.frame(
  x     = rep(x, 2),
  y     = c(x *      phi,
            x * (1 + x / model_nb$theta)),
  model = rep(c("Quasi‑Poisson", "Neg. Binomial"), each = length(x))
)

ggplot() +
  geom_point(aes(x = m, y = v)) +
  geom_line(aes(x = x, y = y, linetype = model), data = line_data) +
  theme_minimal() +
  labs(x = "Mittelwert", y = "Varianz", linetype = NULL) +
  scale_linetype_manual(values = c("solid", "dashed"))

# Negativ Binomial passt besser.

## 1 Homophilie------
# H1_A: Ähnliche Interessensvertretungen führen zu höherer Wahrscheinlichkeit zu Koopeartion, kontrolliert für Partei, Kommission und Sprache.
model_0 <- glmmTMB(
  n_vorstoesse ~ n_interessen +
    (1 | id1) + (1 | id2),
  family = nbinom2,
  data = df_all)
summary(model_0)

model_1a <- glmmTMB(
  n_vorstoesse ~ n_interessen + gleiche_partei + n_kommissionen + gleicher_kanton +
    (1 | id1) + (1 | id2),
  family = nbinom2,
  data = df_all)
summary(model_1a)


# H1_B: Kumulativer Homophilie Effekt: Je mehr Gemeinsamkeiten, desto höher die WAhrscheinlichkeit der Koopeartion.
df_all$gleiche_kommission <- ifelse(df_all$n_kommissionen >= 1, 1, 0)
df_all$gleiches_interesse <- ifelse(df_all$n_interessen >= 1, 1, 0)
df_all$homophilie_count <- df_all$gleiche_partei + df_all$gleiche_kommission + df_all$gleiches_interesse + df_all$gleicher_kanton
model_1b <- glmmTMB(
  n_vorstoesse ~ homophilie_count +
    (1 | id1) + (1 | id2),
  family = nbinom2,
  data = df_all)
summary(model_1b)

## H2 Principal Agent Theorie
# H2_A: Bei spezifischer Organisation, höhere Kooperation
model_2a <- glmmTMB(
  n_vorstoesse ~ n_organisationen + gleiche_partei + n_kommissionen + gleicher_kanton +
    (1 | id1) + (1 | id2),
  family = nbinom2,
  data = df_all)
summary(model_2a)

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

model_2b <- glmmTMB(
  n_vorstoesse ~ organisationen_kat + gleiche_partei + n_kommissionen + gleicher_kanton +
    (1 | id1) + (1 | id2),
  family = nbinom2,
  data = df_all
)
summary(model_2b)
emm <- emmeans(model_2b, ~ organisationen_kat)
diff1 <- contrast(emm, method = list(
  diff_diff = c(1, -2, 1)  # Gewichtung für (keine - eine) - (eine - mehrere)
))
summary(diff1)

# H3_A: Die Erklärungskraft des Principal-Agent-Ansatzes und der Homophilie unterscheidet sich hinsichtlich ihrer Wirkung auf politische Kooperation.
model_3a <- glmmTMB(
  n_vorstoesse ~ n_organisationen + homophilie_count +
    (1 | id1) + (1 | id2),
  family = nbinom2,
  data = df_all)
summary(model_3a)


# H3_B: Es besteht eine Wechselwirkung zwischen Homophilie und Principal-Agent-Verbindungen bei der Erklärung politischer Kooperation.
model_3b <- glmmTMB(
  n_vorstoesse ~ n_organisationen + homophilie_count + n_organisationen * homophilie_count +
    (1 | id1) + (1 | id2),
  family = nbinom2,
  data = df_all)
summary(model_3b)




# Tabellarische Darstellung
tab_model(
  model_1a, model_1b,
  model_2a, model_2b,
  model_3a, model_3b,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.aic = TRUE,
  show.r2 = FALSE,
  dv.labels = c("H1a: Interessens-Homophilie", "H1b: Kombinierte Homophilie", 
                "H2a: Organisationen (Principal Agents)", "H2b: Schwellenhypothese (Prinicpal Agents)", 
                "H3a: Homophilie + Organisationen", "H3b: Interaktion Homophilie + Organisationen)",
  title = "Negativ-Binomial Mixed Models zur Erklärung klimarelevanter Kooperationen"
))

