# Esse script transforma a malha cicloviaria (atual e planejada) para trechos
# do OSM

library(data.table)
library(sf)
library(dplyr)
library(mapview)
library(leaflet)
# library(mapdeck)
sf::sf_use_s2(FALSE)
mapviewOptions(fgb = FALSE)


# open bike network ------------
bike_network_now <- st_read("../../data-raw/smtr_malha_cicloviaria/bike_network_atual/Rede_Existente_Final_20220325.geojson") %>%
  mutate(OBJECTID = 1:n()) %>%
  select(OBJECTID, Rota) %>% st_zm(.) %>% mutate(fase = "fase1")
st_geometry(bike_network_now) = "geom"

bike_network_planejada <- st_read("../../data-raw/smtr_malha_cicloviaria/bike_network_planejada/redefinal_Oficina_v1.geojson") %>%
  # create ID
  mutate(OBJECTID = 1:n(), cenario = "cenario2") %>%
  # create fase
  mutate(fase = ifelse(status == "existente", "fase1", "fase2")) %>%
  select(OBJECTID,cenario,  fase, Rota = Trecho)
# convert to linestring
# st_cast("LINESTRING")
# delete emty
bike_network_planejada <- bike_network_planejada %>% filter(!st_is_empty(.))
bike_network_planejada <- st_make_valid(bike_network_planejada)


# open OSM ------------
osm_rio_vias <- readr::read_rds("../../data/smtr_malha_cicloviaria/2-osm_rio/osm_rio.rds") %>% select(osm_id, name, highway, faixas) %>% mutate(length_osm = st_length(.))
count(osm_rio_vias %>% st_set_geometry(NULL), highway, sort = TRUE)
# manter somente certos tipos de highway
osm_rio_vias <- osm_rio_vias %>% filter(highway %in% c("primary", "secondary", "tertiary", "trunk", "residential", 
                                                       "unclassified", "living_street", "pedestrian",
                                                       "trunk_link", "primary_link", "secondary_link", "tertiary_link", 
                                                       "motorway", "cycleway"))

# osm_rio_vias %>%
  # filter(name %ilike% "mato alto") %>% mapview() + bike_network_now


# export
file.remove("../../data/smtr_malha_cicloviaria/2-osm_rio/osm_rio_filter.gpkg")
st_write(osm_rio_vias, "../../data/smtr_malha_cicloviaria/2-osm_rio/osm_rio_filter.gpkg", append = FALSE)
# osm_rio_vias <- st_read("../../data/smtr_malha_cicloviaria/2-osm_rio/osm_rio_filter.gpkg")

# agrupoar por nome, tipo de via e qtde de faixas
osm_rio_vias_group <- osm_rio_vias %>%
  group_by(name, highway) %>%
  summarise(length_osm = sum(length_osm)) %>%
  ungroup() %>%
  mutate(osm_id = 1:n())

st_write(osm_rio_vias_group, "../../data/smtr_malha_cicloviaria/2-osm_rio/osm_rio_filter_group.gpkg", append = FALSE)
# osm_rio_vias_group <- st_read("../../data/smtr_malha_cicloviaria/2-osm_rio/osm_rio_filter_group.gpkg")

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
  
  # a <- od_group_vias %>%
  #   filter(name %ilike% "mato alto")
  # 
  # bike_network_now %>% filter(OBJECTID %in% c(9, 46, 47)) %>% mapview()
  # 
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
                                                        ifelse(length_piece_intersect > 15 & percent_piece_intersect > 0.9, TRUE,
                                                               ifelse(length_piece_intersect > 50 & percent_piece_intersect > 0.6, TRUE, FALSE))))
  od_group_vias_ok <- od_group_vias %>% filter(ok)
  
  od_group_vias_ok <- od_group_vias_ok %>%
    select(osm_id, name, highway, OBJECTID, Rota, fase)
    # select(name, highway, OBJECTID, Rota, fase)
  
  # agrupar por nome de via ??????????????
  od_group_vias_ok_group <- od_group_vias_ok %>%
    group_by(name, highway, fase) %>%
    summarise()
  
  # mapview(od_group_vias_ok %>% sf::st_set_crs(4326))   +
  # mapview(bike_network_now)
  
  return(od_group_vias_ok)
  
  
}

osm_bike_now <- intersecao_bike_osm(bike_network_now)
osm_bike_planejada <- intersecao_bike_osm(bike_network_planejada)

# a <-osm_bike_now %>% filter(is.na(name))
# table(a$highway)
# mapview(a)
# mapview(a %>% filter(highway == "residential"))

# mapview(osm_bike_now) + mapview(bike_network_now)


# save
file.remove("../../data/smtr_malha_cicloviaria/3-malha_trechos/malha_atual_trechos.gpkg")
file.remove("../../data/smtr_malha_cicloviaria/3-malha_trechos/malha_planejada_trechos.gpkg")
st_write(osm_bike_now,       "../../data/smtr_malha_cicloviaria/3-malha_trechos/malha_atual_trechos.gpkg", append = FALSE)
st_write(osm_bike_planejada, "../../data/smtr_malha_cicloviaria/3-malha_trechos/malha_planejada_trechos.gpkg", append = FALSE)


