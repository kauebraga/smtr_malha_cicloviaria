options(java.parameters = '-Xmx10G')
library(r5r)
library(data.table)
library(dplyr)
library(sf)

# open scenario
cenario1 <- st_read("../../data/smtr_malha_cicloviaria/4-osm_cenarios/osm_cenario1.gpkg") %>% mutate(cenario = "cenario1")

# break scenario in points every 20 meters
# standardize shape resolution - at least every 20 meters
cenario_points <- st_segmentize(cenario1, dfMaxLength = 20)
# shapes_linhas_filter <- sfheaders::sf_cast(shapes_linhas_filter, "POINT")
# transform to lon lat - these points will be the origins
cenario_points_coords <- sfheaders::sf_to_df(cenario_points, fill = TRUE) %>% 
  mutate(id = 1:n()) %>%
  select(id, osm_id, name, highway, fase, cenario, lon = x, lat = y) %>%
  setDT()

# open points - these will be the destination
points <- fread("../../data/smtr_malha_cicloviaria/r5/graphs/rio/points.csv")

# setup network
setup <- r5r::setup_r5("../../data/smtr_malha_cicloviaria/r5/graphs/rio")

# calculate ttmatrix
ttmatrix_bike <- r5r::travel_time_matrix(r5r_core = setup,
                                         origins = cenario_points_coords,
                                         destinations = points,
                                         mode = "WALK",
                                         max_trip_duration = 5)

# trazer osm id para a ttmatrix


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