library(data.table)
library(sf)
library(dplyr)
library(mapview)
library(leaflet)
sf::sf_use_s2(FALSE)


# open ttmatrix
ttm1_detailed <- readr::read_rds("data/ttmatrix_detailed_rio_bike.rds")
# open OD
od_bike <- fread("data-raw/bike_trips/trips_BikeRio_20210901.csv")


ttm1_detailed <- ttm1_detailed %>% select(initial_station_name = fromId, final_station_name = toId, ttime_r5r = total_duration,
                                          dist = distance)

# trazer as rotas para o arquivo de ttmatrix
od_bike_group_sf <- od_bike %>%
  # get unique OD
  group_by(initial_station_name, final_station_name) %>%
  summarise(trips_n = n(), ttime_bike = mean(ttime)) %>% ungroup() %>%
  # deletar origem = destino
  filter(initial_station_name != final_station_name) %>%
  # trazer as rotas!
  left_join(ttm1_detailed, by = c("initial_station_name", "final_station_name")) %>%
  mutate(ttime_dif = log(ttime_bike / ttime_r5r)) %>%
  # mutate(ttime_df = round(ttime_bike - ttime_r5r)) %>%
  arrange(desc(trips_n)) %>%
  st_sf()

# plot ttime_dif
# ttime_dif > 0 -> ttime_r5 < ttime_bike
hist(od_bike_group_sf$ttime_dif, breaks = 50)
summary(od_bike_group_sf$ttime_dif)


# get most common routes
od_bike_group_sf_n <- od_bike_group_sf %>%
  slice(1:5000)




# fazer intersecao da rotas OD com os trechos do OSM ----------------------

osm_rio_vias <- readr::read_rds("data/osm_rio.rds") %>% select(osm_id, name, highway)
osm_rio_vias_buffer <- st_transform(osm_rio_vias, crs = 31983)
osm_rio_vias_buffer <- st_buffer(osm_rio_vias_buffer, dist = 20)
osm_rio_vias_buffer <- st_transform(osm_rio_vias_buffer, crs = 4326)
# od_bike_vazio_vias <- od_bike_vazio %>% st_join(osm_rio_vias_buffer, largest = TRUE)
od_bike_group_sf_n_vias <- od_bike_group_sf_n %>% st_intersection(osm_rio_vias_buffer)
od_bike_group_sf_n_vias <- od_bike_group_sf_n_vias %>% mutate(length_piece_intersect = as.numeric(st_length(.)))

# regra: tem que ter pelo menos 100m de intersecao do pedaco do trecho com o segmento do OSM para considerar
# aquele segmento do OSM
od_bike_group_sf_n_vias_ok <- od_bike_group_sf_n_vias %>% filter(length_piece_intersect > 75)

# agrupar por vias semelhantes e calcular carregamento total em cada trecho
od_bike_group_sf_n_vias_ok <- od_bike_group_sf_n_vias_ok %>%
  st_set_geometry(NULL) %>%
  group_by(osm_id, name, highway) %>%
  summarise(trips_sum = sum(trips_n, na.rm = TRUE)) %>%
  # trazer geom de volta
  left_join(osm_rio_vias %>% select(osm_id)) %>%
  st_sf(crs = 4326) %>%
  arrange(desc(trips_sum))







# comparar com a rede atual -------------------------------------------------------------------


# trazer rede de infraestrutura
bike_network_now <- st_read("data-raw/bike_network_atual/bike_network_atual.gpkg")
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


od_bike_vazio <- st_difference(od_bike_group_sf_n, bike_network_now_buffer)
od_bike_vazio <- od_bike_vazio %>% mutate(length_piece = st_length(.)) %>%
  mutate(id_piece = 1:n())

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(data = od_bike_vazio, color = "red") %>%
  # addPolylines(data = od_bike_vazio, color = "red") %>%
  addPolygons(data = bike_network_now_buffer, color = "blue")


# fazer intersecao com o OSM para saber a via de cada um dos trechos sem ciclovia
osm_rio_vias <- readr::read_rds("data/osm_rio.rds") %>% select(osm_id, name, highway)
osm_rio_vias_buffer <- st_transform(osm_rio_vias, crs = 31983)
osm_rio_vias_buffer <- st_buffer(osm_rio_vias_buffer, dist = 20)
osm_rio_vias_buffer <- st_transform(osm_rio_vias_buffer, crs = 4326)
# od_bike_vazio_vias <- od_bike_vazio %>% st_join(osm_rio_vias_buffer, largest = TRUE)
od_bike_vazio_vias <- od_bike_vazio %>% st_intersection(osm_rio_vias_buffer)
od_bike_vazio_vias <- od_bike_vazio_vias %>% mutate(length_piece_intersect = as.numeric(st_length(.)))

# regra: tem que ter pelo menos 100m de intersecao do pedaco do trecho com o segmento do OSM para considerar
# aquele segmento do OSM
od_bike_vazio_vias_ok <- od_bike_vazio_vias %>% filter(length_piece_intersect > 75)

# agrupar por vias semelhantes e calcular carregamento total em cada trecho
od_bike_vazio_vias_group <- od_bike_vazio_vias_ok %>%
  st_set_geometry(NULL) %>%
  group_by(osm_id, name, highway) %>%
  summarise(trips_sum = sum(trips_n, na.rm = TRUE)) %>%
  # trazer geom de volta
  left_join(osm_rio_vias %>% select(osm_id)) %>%
  st_sf(crs = 4326) %>%
  arrange(desc(trips_sum))

sum(od_bike_vazio_vias_group$trips_sum)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(data = od_bike_vazio_vias_group, color = "red") %>%
  addPolygons(data = bike_network_now_buffer)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(data = od_bike_vazio_vias_group[3,], color = "red")


st_write(od_bike_vazio_vias_group, "data/osm_bike_network_now_missing.gpkg")










# comparar com a rede projetada ---------------------------------------------------------------
bike_network_planejada <- st_read("data-raw/bike_network_planejada/bike_network_planejada.gpkg")
bike_network_planejada <- st_zm(bike_network_planejada) %>% select(OBJECTID, Rota = Name)
bike_network_planejada <- rbind(bike_network_now, bike_network_planejada)

# identificar trechos importantes que nao tem infraestrutura cicloviaria
# criar buffer em relacao a duas redes
bike_network_planejada_buffer <- st_transform(bike_network_planejada, crs = 31983)
bike_network_planejada_buffer <- st_buffer(bike_network_planejada_buffer, dist = 20)
bike_network_planejada_buffer <- st_transform(bike_network_planejada_buffer, crs = 4326)
bike_network_planejada_buffer <- st_union(bike_network_planejada_buffer)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = bike_network_planejada, color = "red")


od_bike_planejada_vazio <- st_difference(od_bike_group_sf_n, bike_network_planejada_buffer)
od_bike_planejada_vazio <- od_bike_planejada_vazio %>% mutate(length_piece = st_length(.)) %>%
  mutate(id_piece = 1:n())

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(data = od_bike_planejada_vazio, color = "red")


leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(data = od_bike_vazio, color = "red") %>%
  # addPolylines(data = od_bike_vazio, color = "red") %>%
  addPolygons(data = bike_network_now_buffer, color = "blue")


# fazer intersecao com o OSM para saber a via de cada um dos trechos sem ciclovia
osm_rio_vias <- readr::read_rds("data/osm_rio.rds") %>% select(osm_id, name, highway)
osm_rio_vias_buffer <- st_transform(osm_rio_vias, crs = 31983)
osm_rio_vias_buffer <- st_buffer(osm_rio_vias_buffer, dist = 20)
osm_rio_vias_buffer <- st_transform(osm_rio_vias_buffer, crs = 4326)

od_bike_planejada_vazio_vias <- od_bike_planejada_vazio %>% st_intersection(osm_rio_vias_buffer)
od_bike_planejada_vazio_vias <- od_bike_planejada_vazio_vias %>% mutate(length_piece_intersect = as.numeric(st_length(.)))

# regra: tem que ter pelo menos 100m de intersecao do pedaco do trecho com o segmento do OSM para considerar
# aquele segmento do OSM
od_bike_planejada_vazio_vias_ok <- od_bike_planejada_vazio_vias %>% filter(length_piece_intersect > 75)

# agrupar por vias semelhantes e calcular carregamento total em cada trecho
od_bike_planejada_vazio_vias_group <- od_bike_planejada_vazio_vias_ok %>%
  st_set_geometry(NULL) %>%
  group_by(osm_id, name, highway) %>%
  summarise(trips_sum = sum(trips_n, na.rm = TRUE)) %>%
  # trazer geom de volta
  left_join(osm_rio_vias %>% select(osm_id)) %>%
  st_sf(crs = 4326) %>%
  arrange(desc(trips_sum))

st_write(od_bike_planejada_vazio_vias_group, "data/osm_bike_network_planned_missing.gpkg")
