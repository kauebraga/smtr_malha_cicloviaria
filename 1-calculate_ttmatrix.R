
# A ideia é pegar as origens e destinos para rotear os deslocamentos e traçar vias mais usadas com base 
# na quantidade de viagens entre cada estação. A partir desse mapeamento a gente pretende cruzar com rede 
# existente e proposta para refinar a rede cicloviária que será consolidada no plano que precisamos 
# entregar até julho.

# - calcular as rotas pelo r5r entre estações;
# - juntar as rotas do r5r com a matriz de viagem;
# - gerar dado de uso por segmento de via;
# - comparar com rede existente e futura projetada;
# - identificar ligações com alto uso sem infra existente ou prevista na rede futura;
# - propor novas ligações;
# - priorizar ligações com base em vias mais usadas identificadas pelo r5r.



options(java.parameters = '-Xmx15G')
library(data.table)
library(sf)
library(dplyr)
library(r5r)
library(mapview)
library(leaflet)
library(osmextract)
sf::sf_use_s2(FALSE)

# open OD
od_bike <- fread("../../data-raw/smtr_malha_cicloviaria/bike_trips/trips_BikeRio_20210901.csv")
# extract day
od_bike[, dia := as.Date(start_time)]
table(od_bike$dia)
# extract ttime
od_bike[, ttime := as.numeric(difftime(end_time, start_time, units = "mins"))]

# unique coords
# join both with unique coordinates
origins_dest_unique <- data.table(id = unique(c(od_bike$initial_station_name, od_bike$final_station_name)))
# bring those coordinates
origins_dest_unique[od_bike, on = c("id" = "initial_station_name"),
                    c("lon", "lat") :=
                      list(i.initial_station_longitude, i.initial_station_latitude)]
origins_dest_unique[od_bike, on = c("id" = "final_station_name"),
                    c("lon", "lat") :=
                      list(i.final_station_longitude, i.final_station_latitude)]

# check locations
origins_dest_unique_sf <- origins_dest_unique %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
# mapview(origins_dest_unique_sf, legend = FALSE)




# setup r5
r5r_core <- setup_r5(data_path = "../curso_r_transportes/r5r/network/rio_atual", verbose = FALSE)

# routes
# create coordinates matrix
od_combinations <- expand.grid(origins_dest_unique$id, origins_dest_unique$id)
coords_matrix_origin <- left_join(select(od_combinations, Var1), origins_dest_unique, by = c("Var1" = "id")) %>% rename(id = Var1)
coords_matrix_dest <- left_join(select(od_combinations, Var2), origins_dest_unique, by = c("Var2" = "id")) %>% rename(id = Var2)

# calculate detailed ttmatrix
ttmatrix_bike <- function(lts) {
  
  ttm1_detailed <- detailed_itineraries(r5r_core = r5r_core,
                                        origins = coords_matrix_origin,
                                        destinations = coords_matrix_dest,
                                        mode = "BICYCLE",
                                        max_trip_duration = 120,
                                        max_lts = lts)
  
  # save
  ttm1_detailed <- ttm1_detailed %>% select(-option, -segment, -segment_duration, -wait, -route, -mode)
  readr::write_rds(ttm1_detailed, 
                   sprintf("../../data/smtr_malha_cicloviaria/1-ttmatrix_od/ttmatrix_detailed_rio_bike_lts%s.rds", lts))
  
}

ttmatrix_bike(lts = 2)
ttmatrix_bike(lts = 3)

# upload to drive
googledrive::drive_put(media = "../../data/smtr_malha_cicloviaria/1-ttmatrix_od/ttmatrix_detailed_rio_bike_lts2.rds",
                       path = "SRTM - Infraestrutura cicloviaria/1-ttmatrix_od",
                       name = "ttmatrix_detailed_rio_bike_lts2.rds")
googledrive::drive_put(media = "../../data/smtr_malha_cicloviaria/1-ttmatrix_od/ttmatrix_detailed_rio_bike_lts3.rds",
                       path = "SRTM - Infraestrutura cicloviaria/1-ttmatrix_od",
                       name = "ttmatrix_detailed_rio_bike_lts3.rds")
