library(sf)
library(ggplot2)
library(googlesheets4)
library(dplyr)

ss <- "https://docs.google.com/spreadsheets/d/1alAUCWPliyF0F4Pj6y2UFHXbySwdKObUiMH4S9AsENk/edit#gid=0"



# trips osm ---------------------------------------------------------------

trips_osm <- st_read("../../data/smtr_malha_cicloviaria/3.2-od_trechos/od_trechos.gpkg")

# week peak
trips_osm %>%
  st_set_geometry(NULL) %>%
  arrange(desc(trips_weekday_peak_morning)) %>%
  select(osm_id, name, trips_weekday_peak_morning) %>%
  slice(1:100) %>%
  write_sheet(ss = ss,
              sheet = "OD OSM - Semana Pico Manhã")

trips_osm %>%
  st_set_geometry(NULL) %>%
  arrange(desc(trips_weekday_peak_afternoon)) %>%
  select(osm_id, name, trips_weekday_peak_afternoon) %>%
  slice(1:100) %>%
  write_sheet(ss = ss,
              sheet = "OD OSM - Semana Pico Tarde")

trips_osm %>%
  st_set_geometry(NULL) %>%
  arrange(desc(trips_weekday_offpeak)) %>%
  select(osm_id, name, trips_weekday_offpeak) %>%
  slice(1:100) %>%
  write_sheet(ss = ss,
              sheet = "OD OSM - Semana Fora Pico")

trips_osm %>%
  st_set_geometry(NULL) %>%
  arrange(desc(trips_weekend)) %>%
  select(osm_id, name, trips_weekend) %>%
  slice(1:100) %>%
  write_sheet(ss = ss,
              sheet = "OD OSM - Final de semana")

# cenarios ----------------------------------------------------------------


cenario1 <- st_read("../../data/smtr_malha_cicloviaria/4-cenarios_trechos/cenario1_trechos.gpkg")
cenario2 <- st_read("../../data/smtr_malha_cicloviaria/4-cenarios_trechos/cenario2_trechos.gpkg")
cenario3 <- st_read("../../data/smtr_malha_cicloviaria/4-cenarios_trechos/cenario3_trechos.gpkg")


mapview(cenario3)



cenario1 %>%
  st_set_geometry(NULL) %>%
  arrange(desc(trips_total)) %>%
  slice(1:100) %>%
  write_sheet(ss = ss,
              sheet = "Cenário 1 - Amostra")

cenario2 %>%
  st_set_geometry(NULL) %>%
  arrange(desc(trips_total)) %>%
  slice(1:100) %>%
  write_sheet(ss = ss,
              sheet = "Cenário 2 - Amostra")

cenario3 %>%
  st_set_geometry(NULL) %>%
  arrange(desc(trips_total)) %>%
  slice(1:100) %>%
  write_sheet(ss = ss,
              sheet = "Cenário 3 - Amostra")

# iso -----------------

cenario1_iso <- readr::read_rds(sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_otp_%s_group_raw.rds", "cenario1"))
cenario2_iso <- readr::read_rds(sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_otp_%s_group_raw.rds", "cenario2"))
# hex
hex <- readr::read_rds("../../data-raw/smtr_malha_cicloviaria/hex_agregado_rio_09.rds") %>%
  mutate(hex_area = st_area(.)) %>% mutate(sigla_muni = "rio")

mapview(cenario2_iso %>% st_sf())

# socio

cenario1_socio <- fread(sprintf("../../data/smtr_malha_cicloviaria/5.1-indicators/bike_indicators_%s_city.csv", "cenario1"))
cenario1_socio <- fread(sprintf("../../data/smtr_malha_cicloviaria/5.1-indicators/bike_indicators_%s_city.csv", "cenario1"))
