## webscarper for data

# Load packages
library(rvest)
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite) 
library(chromote)
library(readxl)
library(purrr)

url <- "https://www.parlament.ch/de/ratsbetrieb/suche-curia-vista/geschaeft?AffairId=20234417"
page <- read_html(url)

# ## DATUM
# 
# 
# # 1. Starte Chromote-Session
# b <- ChromoteSession$new()
# 
# # 2. Lade die Zielseite
# url <- "https://www.parlament.ch/de/ratsbetrieb/suche-curia-vista/geschaeft?AffairId=20234417"
# b$Page$navigate(url)
# b$Page$loadEventFired()
# Sys.sleep(3)  # Gib JS-Inhalten Zeit zum Laden
# 
# # 3. Hole gerendertes HTML
# html <- b$DOM$getDocument()$root$nodeId
# outer_html <- b$DOM$getOuterHTML(nodeId = html)$outerHTML
# page <- read_html(outer_html)
# 
# # 4. Extrahiere das Datum aus dem <div class="col-sm-8 meta-value ng-binding">
# datum <- page %>%
#   html_elements("div.col-sm-8.meta-value") %>%
#   html_text2()
# 
# print(datum[1])
# 
# datum <- page %>%
#   html_elements("div.col-sm-8.meta-value") %>%
#   html_text2()
# datum <- if(length(datum) > 0) datum[1] else NA_character_
# 
# 
# ##UNTERZEICHENDE
# # 1. Starte Chromote
# b <- ChromoteSession$new()
# 
# # 2. Navigiere zur Seite
# url <- "https://www.parlament.ch/de/ratsbetrieb/suche-curia-vista/geschaeft?AffairId=20234417"
# b$Page$navigate(url)
# b$Page$loadEventFired()  # Warten bis Seite fertig geladen ist
# 
# # 3. Warte etwas länger für dynamischen Inhalt (z. B. Mitunterzeichner-Tab)
# Sys.sleep(3)  # ggf. länger bei langsamer Verbindung
# 
# # 4. Hole das DOM (sichtbares HTML)
# html <- b$DOM$getDocument()$root$nodeId
# outer_html <- b$DOM$getOuterHTML(nodeId = html)$outerHTML
# 
# # 5. Parsen mit rvest
# page <- read_html(outer_html)
# 
# # 6. Suche alle Mitunterzeichner-Links
# unterzeichner <- page %>%
#   html_elements("a.person-name") %>%
#   html_text2()
# 
# print(unterzeichner)


## Load data---- 
vorstoesse <- read_xlsx("Vorstoesse.xlsx", sheet = "LINKS")
links <- unique(vorstoesse$URL)

# links_test <- links[10:20]


### Funktion ---

scrape_vorstoss <- function(url) {
  b <- ChromoteSession$new()
  b$Page$navigate(url)
  b$Page$loadEventFired()
  Sys.sleep(runif(1, 3, 6))  # zufällig 3–6 Sekunden
  
  html <- b$DOM$getDocument()$root$nodeId
  outer_html <- b$DOM$getOuterHTML(nodeId = html)$outerHTML
  page <- read_html(outer_html)
  
  # Extrahiere Inhalte
  eingereicht <- page %>%
    html_elements("div.col-sm-8.meta-value") %>%
    html_text2()
  eingereicht <- if(length(eingereicht) > 0) eingereicht[1] else NA_character_
  
  
  unterzeichner <- page %>%
    html_elements("a.person-name") %>%
    html_text2() %>%
    paste(collapse = ", ")
  
  b$close()
  
  # Rückgabe als Liste / Zeile
  tibble(
    url = url,
    eingereicht_am = eingereicht,
    mitunterzeichner = unterzeichner
  )
}

# Achtung: das dauert lange bei 1000 Links!
resultate <- map_dfr(links, possibly(scrape_vorstoss, otherwise = tibble(
  url = NA, eingereicht_am = NA, mitunterzeichner = NA
)))

