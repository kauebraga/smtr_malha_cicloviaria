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
  mutate(OBJECTID = NA, Rota = NA, fase = "fase3") 
# mapview(osm_bike_vazios)

# juntar esses vazios ao cenario3 para compor o ultimo cenario
cenario3 <- rbind(cenario2, osm_bike_vazios)

# mapview(cenario1)
# mapview(cenario2)
# mapview(cenario3)

# salvar
kauetools::write_data(cenario1, "osm_cenarios_final/osm_cenario1_final.gpkg", append = FALSE)
kauetools::write_data(cenario2, "osm_cenarios_final/osm_cenario2_final.gpkg", append = FALSE)
kauetools::write_data(cenario3, "osm_cenarios_final/osm_cenario3_final.gpkg", append = FALSE)


# group by osm name and save
cenario1_group <- cenario1 %>%
  group_by(name, fase) %>%
  summarise(trips_sum = sum(trips_sum, na.rm = TRUE))
cenario2_group <- cenario2 %>%
  group_by(name, fase) %>%
  summarise(trips_sum = sum(trips_sum, na.rm = TRUE))
cenario3_group <- cenario3 %>%
  group_by(name, fase) %>%
  summarise(trips_sum = sum(trips_sum, na.rm = TRUE))

# mapview(cenario1_group)

kauetools::write_data(cenario1_group, "osm_cenarios_final/osm_cenario1_group_final.gpkg", append = FALSE)
kauetools::write_data(cenario2_group, "osm_cenarios_final/osm_cenario2_group_final.gpkg", append = FALSE)
kauetools::write_data(cenario3_group, "osm_cenarios_final/osm_cenario3_group_final.gpkg", append = FALSE)
