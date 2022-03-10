library(data.table)
library(sf)
library(dplyr)
library(mapview)
library(leaflet)
library(Hmisc)
library(kauetools)
mapviewOptions(platform = "mapdeck")
mapdeck::set_token(fread("../../data/mapbox_key.csv")$key)
sf::sf_use_s2(FALSE)




# comparar com a rede atual -------------------------------------------------------------------


# trazer rede de infraestrutura
bike_network_now <- st_read("../../data-raw/smtr_malha_cicloviaria/bike_network_atual/bike_network_atual.gpkg")
# delete z coordinate
bike_network_now <- st_zm(bike_network_now) %>% select(OBJECTID, Rota)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(data = od_bike_sf_common, color = "red") %>%
  addPolylines(data = bike_network_now)

# identificar trechos importantes que nao tem infraestrutura cicloviaria
# criar buffer em relacao a duas redes
bike_network_now_buffer <- st_transform(bike_network_now, crs = 31983)
bike_network_now_buffer <- st_buffer(bike_network_now_buffer, dist = 20)
bike_network_now_buffer <- st_transform(bike_network_now_buffer, crs = 4326)
bike_network_now_buffer <- st_union(bike_network_now_buffer)

# leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(data = bike_network_now_buffer, color = "red") %>%
#   addPolylines(data = od_bike_group_sf_n)


od_weekday_peak_vias_atual_vazio <- st_difference(od_weekday_peak_group_vias, bike_network_now_buffer)
od_weekday_offpeak_vias_atual_vazio <- st_difference(od_weekday_offpeak_group_vias, bike_network_now_buffer)
od_weekday_weekend_vias_atual_vazio <- st_difference(od_weekend_group_vias, bike_network_now_buffer)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(data = od_weekday_peak_vias_atual_vazio, color = "red") %>%
  # addPolylines(data = od_bike_vazio, color = "red") %>%
  addPolygons(data = bike_network_now_buffer, color = "blue")




sum(od_bike_vazio_vias_group$trips_sum)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(data = od_bike_vazio_vias_group, color = "red") %>%
  addPolygons(data = bike_network_now_buffer)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(data = od_bike_vazio_vias_group[3,], color = "red")


st_write(od_weekday_peak_vias_atual_vazio, "../../data/smtr_malha_cicloviaria/osm_trechos_vazios/osm_trechos_vazios_weekday_peak_atual.gpkg")
st_write(od_weekday_offpeak_vias_atual_vazio, "../../data/smtr_malha_cicloviaria/osm_trechos_vazios/osm_trechos_vazios_weekday_offpeak_atual.gpkg")
st_write(od_weekday_weekend_vias_atual_vazio, "../../data/smtr_malha_cicloviaria/osm_trechos_vazios/osm_trechos_vazios_weekend_atual.gpkg")















# comparar com a rede projetada ---------------------------------------------------------------
od_weekday_peak_group_vias <- kauetools::read_data("osm_trechos_trips/osm_trechos_trips_weekday_peak.gpkg")

fs::dir_ls("../../data/smtr_malha_cicloviaria/osm_rede")
osm_bike_planejada <- kauetools::read_data("osm_rede/osm_bike_planejada.gpkg")

# quais trechos estao na OD mas nao estao na rede planejada?
osm_bike_vazios <- od_weekday_peak_group_vias %>% filter(osm_id %nin% osm_bike_planejada$osm_id)

# salvar
kauetools::write_data(osm_bike_vazios, 
                      "osm_trechos_vazios/osm_trechos_vazios_weekday_peak_planejada.gpkg",
                      append = FALSE)





# leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolylines(data = osm_bike_planejada, color = "red") %>%
#   addPolylines(data = osm_bike_vazios, color = "blue")
# 
# 
# 
# # identificar trechos importantes que nao tem infraestrutura cicloviaria
# # criar buffer em relacao a duas redes
# bike_network_planejada_buffer <- st_transform(bike_network_planejada, crs = 31983)
# bike_network_planejada_buffer <- st_buffer(bike_network_planejada_buffer, dist = 20)
# bike_network_planejada_buffer <- st_transform(bike_network_planejada_buffer, crs = 4326)
# bike_network_planejada_buffer <- st_union(bike_network_planejada_buffer)
# 
# leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(data = bike_network_planejada, color = "red")
# 
# 
# od_weekday_peak_vias_atual_planejada <- st_difference(od_weekday_peak_group_vias, bike_network_planejada_buffer)
# od_weekday_offpeak_vias_atual_planejada <- st_difference(od_weekday_offpeak_group_vias, bike_network_planejada_buffer)
# od_weekday_weekend_vias_atual_planejada <- st_difference(od_weekend_group_vias, bike_network_planejada_buffer)
# 
# st_write(od_weekday_peak_vias_atual_planejada, "../../data/smtr_malha_cicloviaria/osm_trechos_vazios/osm_trechos_vazios_weekday_peak_planejada.gpkg")
# st_write(od_weekday_offpeak_vias_atual_planejada, "../../data/smtr_malha_cicloviaria/osm_trechos_vazios/osm_trechos_vazios_weekday_offpeak_planejada.gpkg")
# st_write(od_weekday_weekend_vias_atual_planejada, "../../data/smtr_malha_cicloviaria/osm_trechos_vazios/osm_trechos_vazios_weekend_planejada.gpkg")
