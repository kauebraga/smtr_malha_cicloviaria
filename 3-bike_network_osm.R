# Esse script transforma a malha cicloviaria (atual e planejada) para trechos
# do OSM

library(data.table)
library(sf)
library(dplyr)
library(mapview)
library(leaflet)
library(mapdeck)
sf::sf_use_s2(FALSE)
mapviewOptions(fgb = FALSE)


# open network
bike_network_now <- st_read("../../data-raw/smtr_malha_cicloviaria/bike_network_atual/bike_network_atual.gpkg") %>%
  select(OBJECTID, Rota) %>% st_zm(bike_network_planejada) %>% mutate(fase = "fase1")
bike_network_planejada <- st_read("../../data-raw/smtr_malha_cicloviaria/bike_network_planejada/bike_network_planejada.gpkg")
bike_network_planejada <- st_zm(bike_network_planejada) %>% select(OBJECTID, Rota = Name) %>% mutate(fase = "fase2")
bike_network_planejada <- rbind(bike_network_now, bike_network_planejada)



osm_rio_vias <- readr::read_rds("../../data/smtr_malha_cicloviaria/2-osm_rio/osm_rio.rds") %>% select(osm_id, name, highway) %>% mutate(length_osm = st_length(.))
count(osm_rio_vias %>% st_set_geometry(NULL), highway, sort = TRUE)
# manter somente certos tipos de highway
osm_rio_vias <- osm_rio_vias %>% filter(highway %in% c("primary", "secondary", "tertiary", "trunk", "residential", 
                                                       "unclassified", "living_street", "pedestrian",
                                                       "trunk_link", "primary_link", "secondary_link", "tertiary_link", 
                                                       "motorway", "cycleway"))
# export
kauetools::write_data(osm_rio_vias, "2-osm_rio/osm_rio_filter.gpkg", append = FALSE)

# osm_rio_vias_buffer <- st_transform(osm_rio_vias, crs = 31983)
# osm_rio_vias_buffer <- st_buffer(osm_rio_vias_buffer, dist = 10)
# osm_rio_vias_buffer <- st_transform(osm_rio_vias_buffer, crs = 4326)

# bike_network <- bike_network_now

intersecao_bike_osm <- function(bike_network) {
  
  bike_buffer <- st_transform(bike_network, crs = 31983)
  bike_buffer <- st_buffer(bike_buffer, dist = 30)
  bike_buffer <- st_transform(bike_buffer, crs = 4326)
  
  od_group_vias <- osm_rio_vias %>% st_intersection(bike_buffer)
  od_group_vias <- od_group_vias %>% mutate(length_piece_intersect = as.numeric(st_length(.))) %>%
    # calculate percentage
    mutate(percent_piece_intersect = as.numeric(length_piece_intersect) / as.numeric(length_osm))
  
  
  
  # leaflet() %>%
  #   addProviderTiles(providers$CartoDB.Positron) %>%
  #   addPolylines(data = od_group_vias, color = "red") %>%
  # addPolylines(data = od, color = "blue")
  # 
  # 
  # 
  # mapdeck() %>%
  # add_path(data = osm_rio_vias, tooltip = "highway")
  # addPolylines(data = bike_network_now, color = "blue")
  
  # regra: tem que ter pelo menos 50m e 70% de intersecao do pedaco do trecho com o segmento do OSM para considerar
  # aquele segmento do OSM
  od_group_vias <- od_group_vias %>% mutate(ok = ifelse(length_piece_intersect > 100 & percent_piece_intersect > 0.4, TRUE,
                                                        ifelse(length_piece_intersect > 20 & percent_piece_intersect > 0.9, TRUE,
                                                               ifelse(length_piece_intersect > 50 & percent_piece_intersect > 0.6, TRUE, FALSE))))
  od_group_vias_ok <- od_group_vias %>% filter(ok)
  
  od_group_vias_ok <- od_group_vias_ok %>%
    select(osm_id, name, highway, OBJECTID, Rota, fase)
  
  # agrupar por nome de via ??????????????
  
  
}

osm_bike_now <- intersecao_bike_osm(bike_network_now)
osm_bike_planejada <- intersecao_bike_osm(bike_network_planejada)

# mapview(osm_bike_now) + bike_network_now

# save
kauetools::write_data(osm_bike_now,       "3-osm_malha/osm_malha_atual.gpkg", append = FALSE)
kauetools::write_data(osm_bike_planejada, "3-osm_malha/osm_malha_planejada.gpkg", append = FALSE)


