library(data.table)
library(sf)
library(dplyr)
library(mapview)
library(leaflet)
library(Hmisc)
sf::sf_use_s2(FALSE)


# calculate daily number of trips by each OD pair ---------------------------------------------



# open ttmatrix
ttmatrix <- readr::read_rds("../../data/smtr_malha_cicloviaria/ttmatrix_detailed_rio_bike.rds")
ttmatrix <- ttmatrix %>% select(initial_station_name = fromId, final_station_name = toId, ttime_r5r = total_duration,
                                dist = distance)
# open OD
od_bike <- fread("../../data-raw/smtr_malha_cicloviaria/bike_trips/trips_BikeRio_20210901.csv")
# separar entre semana e final de semana
od_bike[, dia := as.Date(start_time)]
od_bike[, start_hour := hour(start_time)]
od_bike[, weekday := lubridate::wday(dia)]
od_bike_weekday <- od_bike[weekday %in% c(1, 2, 3, 4, 5)]
od_bike_weekend <- od_bike[weekday %in% c(6, 7)]

# fazr pico - fora pco
od_bike_weekday_peak <- od_bike_weekday[start_hour %in% c(6, 7, 8)]
od_bike_weekday_offpeak <- od_bike_weekday[start_hour %in% c(14, 15)]

# agrupar OD por rota
agrupar_od_por_rota <- function(od) {
  
  # trazer as rotas para o arquivo de ttmatrix
  od_rota <- od %>%
    # get unique OD
    filter(initial_station_name != final_station_name) %>%
    group_by(initial_station_name, final_station_name) %>%
    summarise(trips_n = n()) %>%
    # , ttime_bike = mean(ttime)) %>% 
    ungroup() %>%
    # deletar origem = destino
    # trazer as rotas!
    left_join(ttmatrix, by = c("initial_station_name", "final_station_name")) %>%
    arrange(desc(trips_n)) %>%
    st_sf()
  
}

od_group <- agrupar_od_por_rota(od_bike)
od_weekday_peak_group <- agrupar_od_por_rota(od_bike_weekday_peak) %>% mutate(trips_mean = trips_n/22) %>% filter(trips_mean >= 1)
od_weekday_offpeak_group <- agrupar_od_por_rota(od_bike_weekday_offpeak) %>% mutate(trips_mean = trips_n/22) %>% filter(trips_mean >= 1)
od_weekend_group <- agrupar_od_por_rota(od_bike_weekend) %>% mutate(trips_mean = trips_n/8) %>% filter(trips_mean >= 1)

fwrite(od_weekday_peak_group %>% st_set_geometry(NULL), "../../data/smtr_malha_cicloviaria/trips_per_route_weekdays_peak.csv")
fwrite(od_weekday_offpeak_group %>% st_set_geometry(NULL), "../../data/smtr_malha_cicloviaria/trips_per_route_weekdays_offpeak.csv")
st_write(od_weekday_peak_group, "../../data/smtr_malha_cicloviaria/trips_per_route_weekdays_peak.gpkg", append = FALSE)
st_write(od_weekday_offpeak_group, "../../data/smtr_malha_cicloviaria/trips_per_route_weekdays_offpeak.gpkg", append = FALSE)
fwrite(od_weekend_group %>% st_set_geometry(NULL), "../../data/smtr_malha_cicloviaria/trips_per_route_weekend.csv")
st_write(od_weekend_group, "../../data/smtr_malha_cicloviaria/trips_per_route_weekends.gpkg", append = FALSE)

# maps

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(data = od_weekday_group[1:100,], color = "red") 

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(data = od_weekend_group[1:100,], color = "red") 





# fazer intersecao da rotas OD com os trechos do OSM ----------------------

# abrir dados viagens de pico de dia da semana
od_weekday_peak_group <- kauetools::read_data("trips_per_route_weekdays_peak.gpkg")

# abrir osm p/ rio
osm_rio_vias <- readr::read_rds("../../data/smtr_malha_cicloviaria/osm_rio.rds") %>% select(osm_id, name, highway) %>% mutate(length_osm = st_length(.))
# manter somente certos tipos de highway
osm_rio_vias <- osm_rio_vias %>% filter(highway %in% c("primary", "secondary", "tertiary", "trunk", "residential", "unclassified", "living_street",
                                                       "trunk_link", "primary_link", "secondary_link", "tertiary_link", 
                                                       "motorway", "cycleway"))

# od <- od_weekday_peak_group

intersecao_od_osm <- function(od) {
  
  od_buffer <- st_transform(od, crs = 31983)
  od_buffer <- st_buffer(od_buffer, dist = 20)
  od_buffer <- st_transform(od_buffer, crs = 4326)
  
  osm_od <- osm_rio_vias %>% st_intersection(od_buffer)
  osm_od <- osm_od %>% mutate(length_piece_intersect = as.numeric(st_length(.))) %>%
    # calculate percentage
    mutate(percent_piece_intersect = as.numeric(length_piece_intersect) / as.numeric(length_osm))
  
  
  # regra: tem que ter pelo menos 50m e 70% de intersecao do pedaco do trecho com o segmento do OSM para considerar
  # aquele segmento do OSM
  osm_od <- osm_od %>% mutate(ok = ifelse(length_piece_intersect > 100, TRUE,
                                          ifelse(length_piece_intersect > 50 & percent_piece_intersect > 0.5, TRUE, FALSE)))
  osm_od_ok <- osm_od %>% filter(ok)
  
  # algumas trechos osm podem ter mais de uma rota OD passando por ele
  # para calcuilar o carregamento total em cada trecho osm, eh necssario agrupar por cada um
  # deles e somar a quantidade de viagens por ali
  
  # agrupar por vias semelhantes e calcular carregamento total em cada trecho
  osm_od_ok_group <- osm_od_ok %>%
    st_set_geometry(NULL) %>%
    group_by(osm_id, name, highway) %>%
    summarise(trips_sum = sum(trips_n, na.rm = TRUE)) %>%
    # trazer geom de volta
    left_join(osm_rio_vias %>% select(osm_id)) %>%
    st_sf(crs = 4326) %>%
    arrange(desc(trips_sum))
  
}

od_weekday_peak_group_vias <- intersecao_od_osm(od_weekday_peak_group)
# od_weekday_offpeak_group_vias <- intersecao_od_osm(od_weekday_offpeak_group)
# od_weekend_group_vias <- intersecao_od_osm(od_weekend_group)

st_write(od_weekday_peak_group_vias,    "../../data/smtr_malha_cicloviaria/osm_trechos_trips/osm_trechos_trips_weekday_peak.gpkg", append = FALSE)
# st_write(od_weekday_offpeak_group_vias, "../../data/smtr_malha_cicloviaria/osm_trechos_trips/osm_trechos_trips_weekday_offpeak.gpkg", append = FALSE)
# st_write(od_weekend_group_vias,         "../../data/smtr_malha_cicloviaria/osm_trechos_trips/osm_trechos_trips_weekend.gpkg", append = FALSE)


leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(data = od_weekday_peak_group_vias, color = "red") 
  # addPolylines(data = od_weekday_group, color = "blue") 


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
kauetools::write_data(osm_bike_vazios, "osm_trechos_vazios/osm_trechos_vazios_weekday_peak_planejada.gpkg")





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
