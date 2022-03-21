options(java.parameters = '-Xmx20G')
library(r5r)
library(data.table)
library(dplyr)
library(sf)

# setup network
setup <- r5r::setup_r5("../../data/smtr_malha_cicloviaria/r5/graphs/rio")

# open points
points <- fread("../../data/smtr_malha_cicloviaria/r5/graphs/rio/points.csv")

# calculate ttmatrix
ttmatrix_bike <- r5r::travel_time_matrix(r5r_core = setup,
                                    origins = points,
                                    destinations = points,
                                    mode = "BICYCLE",
                                    max_trip_duration = 5)

# save
fwrite(ttmatrix_bike, "../../data/smtr_malha_cicloviaria/ttmatrix_iso_bike.csv")


# 300 metros de bike = 1.5 minutos

# open malha
cenario1 <- st_read("../../data/smtr_malha_cicloviaria/4-osm_cenarios/osm_cenario1.gpkg") %>% mutate(cenario = "cenario1")
# cenario <- cenario1


calcular_isco <- function(cenario) {
  
  # identificar quais pontos estao dentro dos cenarios
  cenario_buffer <- st_transform(cenario, crs = 31983)
  cenario_buffer <- st_buffer(cenario_buffer, dist = 30)
  cenario_buffer <- st_transform(cenario_buffer, crs = 4326)
  
  
  
}