library(data.table)
library(sf)
library(dplyr)
library(mapview)
library(leaflet)
library(Hmisc)
sf::sf_use_s2(FALSE)
mapviewOptions(fgb = FALSE)

# abrir cenarios
cenario1 <- st_read("../../data/smtr_malha_cicloviaria/3-malha_trechos/malha_atual_trechos.gpkg")
cenario2 <- st_read("../../data/smtr_malha_cicloviaria/3-malha_trechos/malha_planejada_trechos.gpkg")

# group cenario - pode ter osm_id repetido
cenario1 <- cenario1 %>%
  group_by(osm_id, name, highway, fase) %>%
  summarise(OBJECTID = first(OBJECTID), Rota = first(Rota))
cenario2 <- cenario2 %>%
  group_by(osm_id, name, highway, fase) %>%
  summarise(OBJECTID = first(OBJECTID), Rota = first(Rota))

# trazer dados de carregamento para cada segmento osm
# od_weekday_peak_group_vias <- st_read("../../data/smtr_malha_cicloviaria/3.2-osm_trips/osm_trips_weekday_peak.gpkg")
od_group_vias <- st_read("../../data/smtr_malha_cicloviaria/3.2-od_trechos/od_trechos.gpkg")

# fazer juncao
cenario1 <- cenario1 %>% 
  left_join(od_group_vias %>% select(osm_id, trips_total, trips_weekday_peak, trips_weekday_offpeak, trips_weekend) %>% st_set_geometry(NULL))
cenario2 <- cenario2 %>% 
  left_join(od_group_vias %>% select(osm_id, trips_total, trips_weekday_peak, trips_weekday_offpeak, trips_weekend) %>% st_set_geometry(NULL))
summary(cenario2$trips_total)

# trazer os trechos vazios
osm_bike_vazios <- st_read("../../data/smtr_malha_cicloviaria/3.3-vazios_trechos/od_vazios_trechos.gpkg") %>%
  mutate(OBJECTID = NA, Rota = NA, fase = "fase3") 
# mapview(osm_bike_vazios)

# juntar esses vazios ao cenario3 para compor o ultimo cenario
cenario3 <- rbind(cenario2, osm_bike_vazios)

# mapview(cenario1)
# mapview(cenario2)
# mapview(cenario3)

file.remove("../../data/smtr_malha_cicloviaria/4-cenarios_trechos/cenario1_trechos.gpkg")
file.remove("../../data/smtr_malha_cicloviaria/4-cenarios_trechos/cenario2_trechos.gpkg")
file.remove("../../data/smtr_malha_cicloviaria/4-cenarios_trechos/cenario3_trechos.gpkg")

# salvar
st_write(cenario1, "../../data/smtr_malha_cicloviaria/4-cenarios_trechos/cenario1_trechos.gpkg", append = FALSE)
st_write(cenario2, "../../data/smtr_malha_cicloviaria/4-cenarios_trechos/cenario2_trechos.gpkg", append = FALSE)
st_write(cenario3, "../../data/smtr_malha_cicloviaria/4-cenarios_trechos/cenario3_trechos.gpkg", append = FALSE)

