library(data.table)
library(sf)
library(dplyr)
library(mapview)
library(leaflet)
library(Hmisc)
sf::sf_use_s2(FALSE)
mapviewOptions(fgb = FALSE)

# abrir cenarios
cenario1 <- kauetools::read_data("osm_rede/osm_bike_now.gpkg")
cenario2 <- kauetools::read_data("osm_rede/osm_bike_planejada.gpkg")

# trazer dados de carregamento para cada segmento osm
od_weekday_peak_group_vias <- kauetools::read_data("osm_trechos_trips/osm_trechos_trips_weekday_peak.gpkg")

# fazer juncao
cenario1 <- cenario1 %>% left_join(od_weekday_peak_group_vias %>% select(osm_id, trips_sum) %>% st_set_geometry(NULL))
cenario2 <- cenario2 %>% left_join(od_weekday_peak_group_vias %>% select(osm_id, trips_sum) %>% st_set_geometry(NULL))
summary(cenario2$trips_sum)

# trazer os trechos vazios
osm_bike_vazios <- kauetools::read_data("osm_trechos_vazios/osm_trechos_vazios_weekday_peak_planejada.gpkg") %>%
  mutate(OBJECTID = NA, Rota = NA)

# juntar esses vazios ao cenario3 para compor o ultimo cenario
cenario3 <- rbind(cenario2, osm_bike_vazios)

mapview(cenario1)
mapview(cenario2)
mapview(cenario3)

# salvar
kauetools::write_data(cenario1, "osm_cenarios_final/osm_cenario1_final.gpkg")
kauetools::write_data(cenario2, "osm_cenarios_final/osm_cenario2_final.gpkg")
kauetools::write_data(cenario3, "osm_cenarios_final/osm_cenario3_final.gpkg")
