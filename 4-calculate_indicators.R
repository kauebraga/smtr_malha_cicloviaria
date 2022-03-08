library(data.table)
library(sf)
library(dplyr)
library(mapview)
library(leaflet)
library(kauetools)
sf::sf_use_s2(FALSE)


# open cenarios
cenario1 <- read_data("osm_trechos_trips/osm_trechos_trips_weekday_peak.gpkg")
cenario2 <- read_data("osm_trechos_trips/osm_trechos_trips_weekday_peak.gpkg")
cenario3 <- read_data("osm_trechos_trips/osm_trechos_trips_weekday_peak.gpkg")


# get hex with socioeconomic variables
hex <- aopdata::read_landuse(city = "rio", geometry = TRUE) %>%
  mutate(hex_area = st_area(.))

# buffer each cenario
cenario <- cenario1

calculate_buffer <- function(cenario) {
  
  cenario_buffer <- st_transform(cenario, crs = 31983)
  cenario_buffer <- st_buffer(cenario_buffer, dist = 500)
  cenario_buffer <- st_transform(cenario_buffer, crs = 4326)
  
  # combine isocronas
  cenario_buffer_combine <- st_sf(geom = st_union(cenario_buffer))
  
  
  # qual a proporcao da area de cada hexagono que esta dentro de uma isocrona?
  a <- st_intersection(cenario_buffer,
                       hex) %>%
    # calcular a area do pedaco
    mutate(pedaco_area = st_area(.)) %>%
    # calcular a proporcao da area total do hex que esta dentro da isocrona
    mutate(area_prop_hex = as.numeric(pedaco_area) / as.numeric(hex_area))
  
  a_combine <- st_intersection(cenario_buffer_combine,
                       hex) %>%
    # calcular a area do pedaco
    mutate(pedaco_area = st_area(.)) %>%
    # calcular a proporcao da area total do hex que esta dentro da isocrona
    mutate(area_prop_hex = as.numeric(pedaco_area) / as.numeric(hex_area))
  
  
  a1 <- a %>%
    st_set_geometry(NULL) %>%
    group_by(abbrev_muni, osm_id, name) %>%
    # multiplcar a area proporcional pela variavel do setor
    # https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones/45947867#45947867
    summarise(across(starts_with(c("P00", "E00", "S00")), .fns = ~sum(.x * area_prop_hex, na.rm = TRUE)),
              trips_sum = first(trips_sum))
  
  a1_combine <- a_combine %>%
    st_set_geometry(NULL) %>%
    group_by(abbrev_muni) %>%
    # multiplcar a area proporcional pela variavel do setor
    # https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones/45947867#45947867
    summarise(across(starts_with(c("P00", "E00", "S00")), .fns = ~sum(.x * area_prop_hex, na.rm = TRUE)))
  
  return(list(buffer_cenario = a1, buffer_cenario_combine = a1_combine))
  
}



