# Esse script pega a matriz de viagens do bikesharing e agrupa para cada par
# origem-destino (estacao dos sistema) e calcula o total de viagens considerando:
# semana e final de semana
# pico e fora pico

library(data.table)
library(sf)
library(dplyr)
library(mapview)
library(leaflet)
library(Hmisc)
mapviewOptions(platform = "mapdeck")
mapdeck::set_token(fread("../../data/mapbox_key.csv")$key)
sf::sf_use_s2(FALSE)


# calculate daily number of trips by each OD pair ---------------------------------------------



# open ttmatrix
ttmatrix <- readr::read_rds("../../data/smtr_malha_cicloviaria/1-ttmatrix_od/ttmatrix_detailed_rio_bike.rds")
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
od_bike_weekday_peak_morning <- od_bike_weekday[start_hour %in% c(6, 7)]
od_bike_weekday_peak_afternoon <- od_bike_weekday[start_hour %in% c(16, 17)]
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

od_group <- agrupar_od_por_rota(od_bike) %>% rename(trips_total = trips_n)
od_weekday_peak_morning_group <- agrupar_od_por_rota(od_bike_weekday_peak_morning) %>% st_set_geometry(NULL) %>% 
  select(-ttime_r5r, -dist) %>% 
  rename(trips_weekday_peak_morning = trips_n)
od_weekday_peak_afternoon_group <- agrupar_od_por_rota(od_bike_weekday_peak_afternoon) %>% st_set_geometry(NULL) %>% 
  select(-ttime_r5r, -dist) %>% 
  rename(trips_weekday_peak_afternoon = trips_n)
od_weekday_offpeak_group <- agrupar_od_por_rota(od_bike_weekday_offpeak) %>% st_set_geometry(NULL) %>% 
  select(-ttime_r5r, -dist) %>% 
  rename(trips_weekday_offpeak = trips_n)
od_weekend_group <- agrupar_od_por_rota(od_bike_weekend) %>% st_set_geometry(NULL) %>% 
  select(-ttime_r5r, -dist) %>%
  rename(trips_weekend = trips_n)

# seria muito interessante juntar todos em um so!
od_group <- left_join(od_group, od_weekday_peak_morning_group, by = c("initial_station_name", "final_station_name")) 
od_group <- left_join(od_group, od_weekday_peak_afternoon_group, by = c("initial_station_name", "final_station_name")) 
od_group <- left_join(od_group, od_weekday_offpeak_group, by = c("initial_station_name", "final_station_name")) 
od_group <- left_join(od_group, od_weekend_group, by = c("initial_station_name", "final_station_name")) 
od_group <- od_group %>% select(initial_station_name, final_station_name,
                                trips_total, trips_weekday_peak_morning, trips_weekday_peak_afternoon, trips_weekday_offpeak, trips_weekend,
                                ttime_r5r, dist)


file.remove("../../data/smtr_malha_cicloviaria/3.1-od_group/trips_group.gpkg")
st_write(od_group, "../../data/smtr_malha_cicloviaria/3.1-od_group/trips_group.gpkg")


b <- st_read("../../data/smtr_malha_cicloviaria/3.1-trips_group/trips_group.gpkg")
c <- st_read("../../data/smtr_malha_cicloviaria/3.1-trips_group/trips_group_weekdays_peak.gpkg")


# fwrite(od_group %>% st_set_geometry(NULL),    "../../data/smtr_malha_cicloviaria/3.1-trips_group/trips_group.csv")
# fwrite(od_weekday_peak_group %>% st_set_geometry(NULL),    "../../data/smtr_malha_cicloviaria/3.1-trips_group/trips_group_weekdays_peak.csv")
# fwrite(od_weekday_offpeak_group %>% st_set_geometry(NULL), "../../data/smtr_malha_cicloviaria/3.1-trips_group/trips_group_weekdays_offpeak.csv")
# fwrite(od_weekend_group %>% st_set_geometry(NULL),         "../../data/smtr_malha_cicloviaria/3.1-trips_group/trips_group_weekend.csv")
# 
# st_write(od_group,                 "../../data/smtr_malha_cicloviaria/3.1-trips_group/trips_group.gpkg")
# st_write(od_weekday_peak_group,    "../../data/smtr_malha_cicloviaria/3.1-trips_group/trips_group_weekdays_peak.gpkg", append = FALSE)
# st_write(od_weekday_offpeak_group, "../../data/smtr_malha_cicloviaria/3.1-trips_group/trips_group_weekdays_offpeak.gpkg", append = FALSE)