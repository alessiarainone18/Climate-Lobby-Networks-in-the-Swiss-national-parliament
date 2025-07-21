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
# Hilfsfunktion definieren
sort_ids <- function(df) {
  df %>%
    mutate(
      id_min = pmin(id1, id2),
      id_max = pmax(id1, id2)
    ) %>%
    dplyr::select(-id1, -id2) %>%
    rename(id1 = id_min, id2 = id_max) %>%
    dplyr::select(id1, id2, everything()) %>%
    distinct()
}

gemeinsame_organisationen <- read_csv("01-Data/gemeinsame_organisationen.csv") %>% sort_ids()
gemeinsame_hauptinteressen <- read_csv("01-Data/gemeinsame_hauptinteressen.csv") %>% sort_ids()
gemeinsame_subinteressen <- read_csv("01-Data/gemeinsame_subinteressen.csv") %>% sort_ids()
gleiche_partei <- read_csv("01-Data/gleiche_partei.csv") %>% sort_ids()
gemeinsame_kommissionen <- read_csv("01-Data/gemeinsame_kommissionen.csv") %>% sort_ids()
gemeinsame_interessen <- read_csv("01-Data/gemeinsame_interessen.csv") %>% sort_ids()
gleicher_kanton <- read_csv("01-Data/gleicher_kanton.csv") %>% sort_ids()
gleiche_sprache <- read_csv("01-Data/gleiche_sprache.csv") %>% sort_ids()
gleiches_geschlecht <- read_csv("01-Data/gleicher_geschlecht.csv") %>% sort_ids()

### 3. Alle Ebenen zusammenführen ----
standardisiere_ids <- function(df) {
  df %>%
    mutate(
      id_a = pmin(id1, id2),
      id_b = pmax(id1, id2)
    ) %>%
    dplyr::select(-id1, -id2) %>%
    rename(id1 = id_a, id2 = id_b)
}

# Alle ID-Kombinationen aus den vier Dateien
all_pairs <- standardisiere_ids(all_pairs)
gemeinsame_vorstoesse <- standardisiere_ids(gemeinsame_vorstoesse)
gemeinsame_organisationen <- standardisiere_ids(gemeinsame_organisationen)
gemeinsame_hauptinteressen <- standardisiere_ids(gemeinsame_hauptinteressen)
gemeinsame_subinteressen <- standardisiere_ids(gemeinsame_subinteressen)
gemeinsame_kommissionen <- standardisiere_ids(gemeinsame_kommissionen)
gemeinsame_interessen <- standardisiere_ids(gemeinsame_interessen)
gleiche_partei <- standardisiere_ids(gleiche_partei)
gleicher_kanton <- standardisiere_ids(gleicher_kanton)
gleiche_sprache <- standardisiere_ids(gleiche_sprache)
gleiches_geschlecht <- standardisiere_ids(gleiches_geschlecht)


gemeinsame_vorstoesse <- gemeinsame_vorstoesse %>%
  mutate(gemeinsame_vorstoesse = as.numeric(gemeinsame_postulate))

df_all <- all_pairs %>%
  left_join(gemeinsame_vorstoesse %>% rename(n_vorstoesse = gemeinsame_vorstoesse), by = c("id1", "id2")) %>%
  left_join(gemeinsame_organisationen %>% rename(n_organisationen = gemeinsame_organisationen), by = c("id1", "id2")) %>%
  left_join(gemeinsame_hauptinteressen %>% rename(n_hauptinteressen = gemeinsame_hauptinteressen), by = c("id1", "id2")) %>%
  left_join(gemeinsame_subinteressen %>% rename(n_subinteressen = gemeinsame_subinteressen), by = c("id1", "id2")) %>%
  left_join(gemeinsame_kommissionen %>% rename(n_kommissionen = n_gemeinsame_kommissionen), by = c("id1", "id2")) %>%
  left_join(gemeinsame_interessen %>% rename(n_interessen = gemeinsame_interessen), by = c("id1", "id2")) %>%
  left_join(gleiche_partei, by = c("id1", "id2")) %>%
  left_join(gleicher_kanton, by = c("id1", "id2")) %>%
  left_join(gleiche_sprache, by = c("id1", "id2")) %>%
  left_join(gleiches_geschlecht, by = c("id1", "id2")) %>%
  mutate(
    across(starts_with("n_"), ~ replace_na(., 0))
  )

write_csv(df_all, "01-Data/alle_beziehungen.csv")


df_all <- read.csv("01-Data/alle_beziehungen.csv") 

vars <- c("n_vorstoesse", "n_organisationen", "n_hauptinteressen", "n_subinteressen", "gleiche_partei", "n_interessen", "gleicher_kanton")

# Funktion, die für eine Variable Mittelwert, Anteil Nullen und SD berechnet
summary_stats <- function(df, var) {
  x <- df[[var]]
  mean_x <- mean(x)
  prop_zero <- mean(x == 0)
  sd_x <- sd(x)
  
  tibble(
    variable = var,
    mean = mean_x,
    prop_zero = prop_zero,
    sd = sd_x
  )
}


result <- bind_rows(lapply(vars, summary_stats, df = df_all))
print(result)

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

# Negativ Binomial passt besser. Random Effects für id1 und id2 besser?
model_nb_fixed <- glmmTMB(
  n_vorstoesse ~ n_interessen + gleiche_partei + n_kommissionen + gleicher_kanton,
  family = nbinom2,
  data = df_all
)
summary(model_nb_fixed)

model_mixed_nb <- glmmTMB(
  n_vorstoesse ~ n_interessen + gleiche_partei + n_kommissionen + gleicher_kanton +
    (1 | id1) + (1 | id2),
  family = nbinom2,
  data = df_all
)

anova(model_nb_fixed, model_mixed_nb)
## --> mixed ist besser!

# Zero-Inflated Negative Binomial Modell
model_mixed_zinb <- glmmTMB(
  n_vorstoesse ~ n_interessen + gleiche_partei + n_kommissionen + gleicher_kanton + 
    (1 | id1) + (1 | id2),
  ziformula = ~ 1,       # Modell für Zero-Inflation 
  family = nbinom2,
  data = df_all
)
summary(model_zinb)

# Vergleich via AIC
AIC(model_mixed_nb, model_mixed_zinb)
anova(model_mixed_nb, model_mixed_zinb)

## --> ohne zero Inflated

## 1 Homophilie------
# H1_A: Ähnliche Interessensvertretungen führen zu höherer Wahrscheinlichkeit zu Koopeartion, kontrolliert für Partei, Kommission und Sprache.
model_0 <- glmmTMB(
  n_vorstoesse ~ n_interessen +
    (1 | id1) + (1 | id2),
  family = nbinom2,
  data = df_all)
summary(model_0)

model_1a <- glmmTMB(
  n_vorstoesse ~ n_interessen + n_kommissionen  + gleiche_partei + gleiche_kanton + gleiche_sprache + gleiche_geschlecht +
    (1 | id1) + (1 | id2),
  family = nbinom2,
  data = df_all)
summary(model_1a)


# H1_B: Kumulativer Homophilie Effekt: Je mehr Gemeinsamkeiten, desto höher die WAhrscheinlichkeit der Koopeartion.
df_all$gleiche_kommission <- ifelse(df_all$n_kommissionen >= 1, 1, 0)
df_all$gleiches_interesse <- ifelse(df_all$n_interessen >= 1, 1, 0)
df_all$homophilie_count <- df_all$gleiche_partei + df_all$gleiche_kommission + df_all$gleiches_interesse + df_all$gleiche_kanton + df_all$gleiche_sprache
+ df_all$gleiche_geschlecht 
model_1b <- glmmTMB(
  n_vorstoesse ~ homophilie_count +
    (1 | id1) + (1 | id2),
  family = nbinom2,
  data = df_all)
summary(model_1b)

## H2 Principal Agent Theorie
# H2_A: Bei spezifischer Organisation, höhere Kooperation
model_2a <- glmmTMB(
  n_vorstoesse ~ n_organisationen + gleiche_partei + n_kommissionen + gleiche_kanton + gleiche_sprache + gleiche_geschlecht +
    (1 | id1) + (1 | id2),
  family = nbinom2,
  data = df_all)
summary(model_2a)

# H2_B: Wenn sie mehrere gemeinsame Organisationen vertreten, ist die Kooperation noch stärker.
df_all <- df_all %>%
  mutate(
    anzahl_gemeinsame_organisationen = n_organisationen,  
    organisationen_kat = case_when(
      anzahl_gemeinsame_organisationen == 0 ~ "keine",
      anzahl_gemeinsame_organisationen == 1 ~ "eine",
      anzahl_gemeinsame_organisationen > 1  ~ "mehrere",
      TRUE ~ NA_character_
    ),
    organisationen_kat = factor(organisationen_kat, levels = c("keine", "eine", "mehrere"))
  )

model_2b <- glmmTMB(
  n_vorstoesse ~ organisationen_kat + gleiche_partei + n_kommissionen + gleiche_kanton + gleiche_sprache + gleiche_geschlecht +
    (1 | id1) + (1 | id2),
  family = nbinom2,
  data = df_all
)
summary(model_2b)
emm <- emmeans(model_2b, ~ organisationen_kat)
diff1 <- contrast(emm, method = list(
  diff_diff = c(1, -2, 1)  
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



statistik <- df_all %>%
  mutate(homophilie_count = as.numeric(homophilie_count),
         n_kommissionen = as.numeric(n_kommissionen)) %>%
  summarise(
    `Anzahl gemeinsamer Vorstösse` = list(c(min(n_vorstoesse), max(n_vorstoesse), mean(n_vorstoesse))),
    `Anzahl gemeinsamer Interessen` = list(c(min(n_interessen), max(n_interessen), mean(n_interessen))),
    `Anzahl gemeinsamer Kommissionen` = list(c(min(n_kommissionen), max(n_kommissionen), mean(n_kommissionen))),
    `Anzahl gemeinsamer Organisationen` = list(c(min(n_organisationen), max(n_organisationen), mean(n_organisationen))),
    `Homophilie-Score (0–4)` = list(c(min(homophilie_count), max(homophilie_count), mean(homophilie_count)))
  ) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Werte") %>%
  unnest_wider(Werte, names_sep = "_") %>%
  rename(Minimum = Werte_1, Maximum = Werte_2, Mittelwert = Werte_3) %>%
  mutate(across(c(Mittelwert), round, digits = 2))


data <- read_csv("01-Data/df_filtered_interests.csv")
library(dplyr)

# Zuerst: Nur eindeutige Kombinationen zählen
df_stats <- data %>%
  group_by(parlamentarier_id) %>%
  summarise(
    n_organisationen = n_distinct(organisation_uid, na.rm = TRUE),
    n_hauptkategorien = n_distinct(Hauptkategorie, na.rm = TRUE),
    n_interessengruppen = n_distinct(interessengruppe, na.rm = TRUE),
    
  )

# Deskriptive Statistik
df_stats %>%
  summarise(
    `Organisationen - min` = min(n_organisationen),
    `Organisationen - max` = max(n_organisationen),
    `Organisationen - mean` = round(mean(n_organisationen), 2),
    
    `Hauptkategorien - min` = min(n_hauptkategorien),
    `Hauptkategorien - max` = max(n_hauptkategorien),
    `Hauptkategorien - mean` = round(mean(n_hauptkategorien), 2),
    
    `Interessengruppen - min` = min(n_interessengruppen),
    `Interessengruppen - max` = max(n_interessengruppen),
    `Interessengruppen - mean` = round(mean(n_interessengruppen), 2),
    
    `Vortöesse - min` = min(n_vorstoesse),
    `Vortöesse - max` = max(n_vorstoesse),
    `Vortöesse - mean` = round(mean(n_vorstoesse), 2))
    
    
  ) %>%
  pivot_longer(everything(), names_to = "Kennzahl", values_to = "Wert")


data <- read_csv("01-Data/alle_beziehungen.csv")

# Zuerst: Nur eindeutige Kombinationen zählen
df_stats <- data %>%
  summarise(
    
    `Gemeinsame Vorstöesse - min` = min(n_vorstoesse),
    `Gemeinsame Vorstöesse - max` = max(n_vorstoesse),
    `Gemeinsame Vorstöesse - mean` = round(mean(n_vorstoesse), 2),
    
    `Ähnliche Interessen - min` = min(n_interessen),
    `Ähnliche Interessen - max` = max(n_interessen),
    `Ähnliche Interessen - mean` = round(mean(n_interessen), 2),
    
    `Gemeinsame Organisationen - min` = min(n_organisationen),
    `Gemeinsame Organisationen - max` = max(n_organisationen),
    `Gemeinsame Organisationen - mean` = round(mean(n_organisationen), 2),
    
    `Gemeinsame Kommissionen - min` = min(n_kommissionen),
    `Gemeinsame Kommissionen - max` = max(n_kommissionen),
    `Gemeinsame Kommissionen - mean` = round(mean(n_kommissionen), 2)

) %>%
  pivot_longer(everything(), names_to = "Kennzahl", values_to = "Wert")

