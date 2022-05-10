# Esse script transforma as rotas com viagens que foram estimada a partir
# dos dados de bikesharing em trechos OSM e calcula a quantidade de viagens em cada trecho OSM

library(data.table)
library(sf)
library(dplyr)
library(mapview)
library(leaflet)
library(Hmisc)
# mapviewOptions(platform = "mapdeck")
mapviewOptions(fgb = FALSE)
# mapdeck::set_token(fread("../../data/mapbox_key.csv")$key)
sf::sf_use_s2(FALSE)



# fazer intersecao da rotas OD com os trechos do OSM ----------------------

# seria interessante juntar todos esses trechos em um so!
od_group <- st_read("../../data/smtr_malha_cicloviaria/3.1-od_group/trips_group.gpkg")
# excluir pares OD com menos de 15 viagens - 1 viagens a cada dois dias
od_group <- od_group %>% filter(trips_total > 15)

# abrir osm p/ rio
osm_rio_vias <- st_read("../../data/smtr_malha_cicloviaria/2-osm_rio/osm_rio_filter.gpkg")


# od <- od_group
# od <-   od_group %>% filter(initial_station_name == "1 - Central do Brasil", final_station_name == "205 - Praça Barão de Ladário")

intersecao_od_osm <- function(od) {
  
  od_buffer <- st_transform(od, crs = 31983)
  od_buffer <- st_buffer(od_buffer, dist = 20)
  od_buffer <- st_transform(od_buffer, crs = 4326)
  
  osm_od <- osm_rio_vias %>% st_intersection(od_buffer)
  osm_od <- osm_od %>% mutate(length_piece_intersect = as.numeric(st_length(.))) %>%
    # calculate percentage
    mutate(percent_piece_intersect = as.numeric(length_piece_intersect) / as.numeric(length_osm))
  
  # mapview(od_buffer) + osm_od 
  # mapview(osm_od, zcol = "osm_id") 
  # mapview(osm_od) + od_buffer + filter(osm_rio_vias, name %like% "Otoni")
  # mapview(filter(osm_rio_vias, name %like% "Otoni"))
  
  # regra: tem que ter pelo menos 50m e 70% de intersecao do pedaco do trecho com o segmento do OSM para considerar
  # aquele segmento do OSM
  osm_od <- osm_od %>%  mutate(ok = ifelse(length_piece_intersect > 100 & percent_piece_intersect > 0.4, TRUE,
                                           ifelse(length_piece_intersect > 20 & percent_piece_intersect > 0.9, TRUE,
                                                  ifelse(length_piece_intersect > 50 & percent_piece_intersect > 0.6, TRUE, FALSE))))
  osm_od_ok <- osm_od %>% filter(ok)
  
  # mapview(osm_od_ok)
  
  # algumas trechos osm podem ter mais de uma rota OD passando por ele
  # para calcuilar o carregamento total em cada trecho osm, eh necssario agrupar por cada um
  # deles e somar a quantidade de viagens por ali
  
  # agrupar por vias semelhantes e calcular carregamento total em cada trecho
  osm_od_ok_group <- osm_od_ok %>%
    st_set_geometry(NULL) %>%
    # group_by(name, highway) %>%
    group_by(osm_id, name, highway) %>%
    summarise(trips_total = sum(trips_total, na.rm = TRUE),
              trips_weekday_peak_morning = sum(trips_weekday_peak_morning, na.rm = TRUE),
              trips_weekday_peak_afternoon = sum(trips_weekday_peak_afternoon, na.rm = TRUE),
              trips_weekday_offpeak = sum(trips_weekday_offpeak, na.rm = TRUE),
              trips_weekend = sum(trips_weekend, na.rm = TRUE)) %>%
    # trazer geom de volta
    left_join(osm_rio_vias %>% select(osm_id)) %>%
    st_sf(crs = 4326) %>%
    arrange(desc(trips_total))
  
  # mapview(osm_od_ok_group)
  
  # # por via
  # osm_od_ok_group_group <- osm_od_ok_group %>%
  #   group_by(name, highway) %>%
  #   # quando agrupar por via, a quantidade de viagens em cada via vai ser a 
  #   # quantidade de viagens encontrada no trecho com maior numero de viagens
  #   # daquela via
  #   summarise(trips_sum = max(trips_sum, na.rm = TRUE))
  
  
  return(osm_od_ok_group)
  
}

od_group_vias <- intersecao_od_osm(od_group)


# a <- st_read("../../../../Downloads/osm_trips_weekday_peak.gpkg")
file_path <- "../../data/smtr_malha_cicloviaria/3.2-od_trechos/od_trechos.gpkg" 
file.remove(file_path)
st_write(od_group_vias, file_path, append = FALSE)



# update on drive folder
googledrive::drive_ls(path = "SRTM - Infraestrutura cicloviaria")
googledrive::drive_put(media = file_path,
                       path = "SRTM - Infraestrutura cicloviaria/3.2-od_trechos",
                       name = basename(file_path))
