# calcular a quantidade de viagens de cada arquivo OD
# A partir da OD: 
# qual eh o caminho? Quantos usuários usam cada caminho
# Quantida de viagens por cada caminho
# Caminhos por dia tipico e horario
# Distancia de viagem por dia tipico e horario
# Desagregador dia ultil/final de semana (olhar feriados)
# Pico e fora pico
# Fazer tabela com o perfil do usuário
# Quantidad de viagens por usuário (dia ultil/não utilO
# Tabelas de referencia
# Perfil de viagens por hora
# Qdte de viagens por dia
# Qtde de viagens por estação (origem / destino)


library(data.table)
library(dplyr)
library(sf)

# open ttmatrix
ttmatrix <- readr::read_rds("../../data/smtr_malha_cicloviaria/1-ttmatrix_stations/ttmatrix_detailed_rio_bike.rds")
ttmatrix <- ttmatrix %>% select(initial_station_name = fromId, final_station_name = toId, ttime_r5r = total_duration,
                                dist = distance)
# open OD
od_bike <- fread("../../data-raw/smtr_malha_cicloviaria/bike_trips/trips_BikeRio_20210901.csv")
# separar entre semana e final de semana
od_bike[, dia := as.Date(start_time)]
od_bike[, start_hour := hour(start_time)]
od_bike[, weekday := lubridate::wday(dia)]
od_bike[, day_type := fifelse(weekday %in% c(1, 2, 3, 4, 5), "semana", "final_de_semana")]
# fazr pico - fora pco
od_bike[, hour_type := fifelse(start_hour %in% c(6, 7, 8), "pico", "fora_pico")]

# agrupar OD por rota
agrupar_od_por_rota <- function(od) {
  
  # trazer as rotas para o arquivo de ttmatrix
  od_rota <- od %>%
    # get unique OD
    # filter(initial_station_name != final_station_name) %>%
    group_by(initial_station_name, final_station_name, day_type, hour_type) %>%
    summarise(trips_n = n()) %>%
    # , ttime_bike = mean(ttime)) %>% 
    ungroup() %>%
    arrange(desc(trips_n))
  # deletar origem = destino
  # trazer as rotas!
  # left_join(ttmatrix, by = c("initial_station_name", "final_station_name")) %>%
  # st_sf()
  
}

od_bike_group <- agrupar_od_por_rota(od_bike)


# quantidade de viagens por cada par OD -------------------------------------------------------

# quantidade de viagens diarias por pico-fora pico
od_pico_semana <- od_bike_group %>% filter(day_type == "semana", hour_type == "pico") %>%
  arrange(desc(trips_n))
# quantidade de viagens diarias por pico-fora pico
od_fpico_semana <- od_bike_group %>% filter(day_type == "semana", hour_type == "fora_pico") %>%
  arrange(desc(trips_n))
# quantidade de viagens diarias por final de semana
od_fsemana <- od_bike_group %>% filter(day_type == "final_de_semana") %>%
  group_by(initial_station_name, final_station_name, day_type) %>%
  summarise(trips_n = sum(trips_n)) %>%
  arrange(desc(trips_n))



# identificar entregadores: mesma origm e destino ---------------------------------------------
od_bike_entregadores <- od_bike %>%
  filter(initial_station_name == final_station_name) %>%
  # pegar somente com mais de 5 minutos
  filter(duration_seconds >= 300) %>%
  # agrupar
  group_by(initial_station_name, final_station_name) %>%
  summarise(trips_n = n(), duration_mean = mean(duration_seconds / 60)) %>%
  arrange(desc(trips_n)) %>%
  # trazer localizacao das estacoes
  left_join(od_bike %>% select(initial_station_name, 
                               lon = initial_station_longitude,
                               lat = initial_station_latitude),
            by = "initial_station_name") %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_sf()



# viagens que comecam/terminam proxima a terminais --------------------------------------------

# abrir estacoes
estacoes <- geojsonsf::geojson_sf("../../data-raw/smtr_malha_cicloviaria/Estacoes_mediaalta_transporte.geojson")
estacoes <- st_zm(estacoes)
estacoes <- st_set_crs(estacoes, 32723)
estacoes <- st_transform(estacoes, 4326)
estacoes <- estacoes %>%
  mutate(station_id = 1:n()) %>%
  select(station_id, Station = Nome)

# buffer das estacoes
estacoes_buffer <- st_transform(estacoes, crs = 31983) %>%
  st_buffer(dist = 300) %>%
  st_transform(crs = 4326)

# viagens de origem 
od_bike_origem_sf <- od_bike %>% 
  select(trip_id, initial_station_name, lon = initial_station_longitude, lat = initial_station_latitude) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# join
od_bike_origem_sf <- od_bike_origem_sf %>%
  st_join(estacoes_buffer)

od_bike_origem_sf <- od_bike_origem_sf %>%
  mutate(perto_estacao = ifelse(is.na(station_id), "nao", "sim"))

prop.table(table(od_bike_origem_sf$perto_estacao))


# viagens de destino 
od_bike_destino_sf <- od_bike %>% 
  select(trip_id, final_station_name, lon = final_station_longitude, lat = final_station_latitude) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# join
od_bike_destino_sf <- od_bike_destino_sf %>%
  st_join(estacoes_buffer)

od_bike_destino_sf <- od_bike_destino_sf %>%
  mutate(perto_estacao = ifelse(is.na(station_id), "nao", "sim")) 

prop.table(table(od_bike_destino_sf$perto_estacao))

# juntar com a matriz original
od_bike <- od_bike %>%
  left_join(od_bike_origem_sf %>% st_set_geometry(NULL) %>% distinct(trip_id, perto_estacao),
            by = "trip_id") %>%
  left_join(od_bike_destino_sf %>% st_set_geometry(NULL) %>% distinct(trip_id, perto_estacao),
            by = "trip_id") %>%
  mutate(perto_estacao = ifelse(perto_estacao.x == "sim" | perto_estacao.y == "sim", "sim", "nao"))
  
prop.table(table(od_bike$perto_estacao))
  
  
