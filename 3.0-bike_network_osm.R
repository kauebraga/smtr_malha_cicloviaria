library(data.table)
library(sf)
library(dplyr)
library(mapview)
library(leaflet)
library(mapdeck)
sf::sf_use_s2(FALSE)


bike_network_now <- st_read("../../data-raw/smtr_malha_cicloviaria/bike_network_atual/bike_network_atual.gpkg") %>%
  select(OBJECTID, Rota) %>% st_zm(bike_network_planejada)
bike_network_planejada <- st_read("../../data-raw/smtr_malha_cicloviaria/bike_network_planejada/bike_network_planejada.gpkg")
bike_network_planejada <- st_zm(bike_network_planejada) %>% select(OBJECTID, Rota = Name)
bike_network_planejada <- rbind(bike_network_now, bike_network_planejada)



osm_rio_vias <- readr::read_rds("../../data/smtr_malha_cicloviaria/osm_rio.rds") %>% select(osm_id, name, highway) %>% mutate(length_osm = st_length(.))
# osm_rio_vias_buffer <- st_transform(osm_rio_vias, crs = 31983)
# osm_rio_vias_buffer <- st_buffer(osm_rio_vias_buffer, dist = 10)
# osm_rio_vias_buffer <- st_transform(osm_rio_vias_buffer, crs = 4326)

# bike_network <- bike_network_now

intersecao_bike_osm <- function(bike_network) {
  
  bike_buffer <- st_transform(bike_network, crs = 31983)
  bike_buffer <- st_buffer(bike_buffer, dist = 20)
  bike_buffer <- st_transform(bike_buffer, crs = 4326)
  
  od_group_vias <- osm_rio_vias %>% st_intersection(bike_buffer)
  od_group_vias <- od_group_vias %>% mutate(length_piece_intersect = as.numeric(st_length(.)))
  
  
  
  # leaflet() %>%
  #   addProviderTiles(providers$CartoDB.Positron) %>%
  #   addPolylines(data = od_group_vias, color = "red") %>%
  # addPolylines(data = od, color = "blue")
  # 
  # 
  # 
  # mapdeck() %>%
  #   addProviderTiles(providers$CartoDB.Positron) %>%
  #   addPolylines(data = od_group_vias_ok, color = "red") %>%
  # addPolylines(data = bike_network_now, color = "blue")
  
  # regra: tem que ter pelo menos 100m de intersecao do pedaco do trecho com o segmento do OSM para considerar
  # aquele segmento do OSM
  od_group_vias_ok <- od_group_vias %>% filter(length_piece_intersect > 50)
  
  # agrupar por vias semelhantes e calcular carregamento total em cada trecho
  od_group_vias_ok <- od_group_vias_ok %>%
    st_set_geometry(NULL) %>%
    # trazer geom de volta
    left_join(osm_rio_vias %>% select(osm_id)) %>%
    st_sf(crs = 4326)
  
}

osm_bike_now <- intersecao_od_osm(bike_network_now)
osm_bike_planejada <- intersecao_od_osm(bike_network_planejada)

# save
st_write(osm_bike_now, "../../data/smtr_malha_cicloviaria/osm_bike_now.gpkg")
st_write(osm_bike_planejada, "../../data/smtr_malha_cicloviaria/osm_bike_planejada.gpkg")



leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(data = osm_bike_now, color = "red") %>%
  addPolylines(data = bike_network_now, color = "blue")
