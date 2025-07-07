# Analyse

# Lese beide Dateien
gemeinsame_vorstoesse <- read_csv("01-Data/gemeinsame_vorstoesse.csv")
gemeinsame_interessen <- read_csv("01-Data/gemeinsame_interessen.csv")

# Vereinheitliche Spaltennamen
df_all <- full_join(
  gemeinsame_vorstoesse %>% rename(n_vorstoesse = gemeinsame_postulate),
  gemeinsame_interessen %>% rename(n_interessen = gemeinsame_interessenbindungen),
  by = c("id1", "id2")
)

# NAs ersetzen durch 0 (keine gemeinsamen Postulate/Interessen)
df_all <- df_all %>%
  mutate(
    n_vorstoesse = replace_na(n_vorstoesse, 0),
    n_interessen = replace_na(n_interessen, 0)
  )


# Poisson-Regression: Modellierung der Anzahl gemeinsamer Postulate
model <- glm(n_vorstoesse ~ n_interessen, family = poisson(link = "log"), data = df_all)

summary(model)
exp(coef(model)["n_interessen"])
# 22 % mehr gemeinsame Postulate pro zusätzlicher Interessenteilung

## Netzwerkanalyse
library(igraph)

# Daten einlesen und kombinieren
postulate_df <- read_csv("01-Data/gemeinsame_vorstoesse.csv") %>%
  rename(n_postulate = gemeinsame_postulate)

interesse_df <- read_csv("01-Data/gemeinsame_interessen.csv") %>%
  rename(n_interesse = gemeinsame_interessenbindungen)

# Mergen
combined_edges <- full_join(postulate_df, interesse_df, by = c("id1", "id2")) %>%
  mutate(across(c(n_postulate, n_interesse), ~replace_na(.x, 0))) %>%
  filter(n_postulate > 0 | n_interesse > 0)

# Netzwerkgraf
g_multi <- graph_from_data_frame(combined_edges, directed = FALSE)

# Kantenattribute setzen
E(g_multi)$postulate <- combined_edges$n_postulate
E(g_multi)$interesse <- combined_edges$n_interesse
E(g_multi)$multiplex_type <- case_when(
  combined_edges$n_postulate > 0 & combined_edges$n_interesse > 0 ~ "beides",
  combined_edges$n_postulate > 0 ~ "nur_postulat",
  combined_edges$n_interesse > 0 ~ "nur_interesse"
)

# Farben je nach Typ
edge_colors <- case_when(
  E(g_multi)$multiplex_type == "beides" ~ "purple",
  E(g_multi)$multiplex_type == "nur_postulat" ~ "blue",
  E(g_multi)$multiplex_type == "nur_interesse" ~ "red"
)

plot(g_multi,
     vertex.size = 5,
     vertex.label = NA,
     edge.color = edge_colors,
     edge.width = 1 + log1p(E(g_multi)$postulate + E(g_multi)$interesse),
     layout = layout_with_fr(g_multi),
     main = "Multiplex-Netzwerk: Postulate (blau), Interessen (rot), Beides (lila)")

cor(combined_edges$n_postulate, combined_edges$n_interesse, method = "spearman")

# Für Postulate:
g_post <- delete_edges(g_multi, E(g_multi)[E(g_multi)$postulate == 0])
cl_post <- cluster_louvain(g_post)

# Für Interessen:
g_int <- delete_edges(g_multi, E(g_multi)[E(g_multi)$interesse == 0])
cl_int <- cluster_louvain(g_int)

