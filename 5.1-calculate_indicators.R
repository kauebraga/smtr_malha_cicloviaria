options(scipen = 999999)
library(data.table)
library(sf)
library(dplyr)
library(mapview)
library(leaflet)
library(kauetools)
library(aopdata)
sf::sf_use_s2(FALSE)
mapviewOptions(fgb = FALSE)
mapviewOptions(platform = "mapdeck")
mapdeck::set_token(fread("../../data/mapbox_key.csv")$key)


# open cenarios
cenario1 <- read_data("osm_cenarios_final/osm_cenario1_group_final.gpkg") %>% mutate(cenario = "cenario1")
cenario2 <- read_data("osm_cenarios_final/osm_cenario2_group_final.gpkg")  %>% mutate(cenario = "cenario2")
cenario3 <- read_data("osm_cenarios_final/osm_cenario3_group_final.gpkg")  %>% mutate(cenario = "cenario3")

# mapview(cenario1) + cenario2
# mapview(cenario2) + cenario3


# get hex with socioeconomic variables
hex <- kauetools::read_data("hex_agregado_rio_09.rds", folder = "data-raw") %>%
  mutate(hex_area = st_area(.)) %>% mutate(sigla_muni = "rio")

# buffer each cenario
# cenario <- cenario1

calculate_buffer <- function(cenario) {
  
  cenario_buffer <- st_transform(cenario, crs = 31983)
  cenario_buffer <- st_buffer(cenario_buffer, dist = 300)
  cenario_buffer <- st_transform(cenario_buffer, crs = 4326)
  
  # combine isocronas
  cenario_buffer_combine <- st_sf(geom = st_union(cenario_buffer)) %>% mutate(cenario = unique(cenario_buffer$cenario))
  
  
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
    group_by(sigla_muni, name, cenario, fase) %>%
    # multiplcar a area proporcional pela variavel do setor
    # https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones/45947867#45947867
    # summarise(across(starts_with(c("P00", "E00", "S00")), .fns = ~sum(.x * area_prop_hex, na.rm = TRUE)),
    summarise(across(starts_with(c("cor_", "edu_", "saude_")), .fns = ~round(sum(.x * area_prop_hex, na.rm = TRUE))),
              fase = first(fase),
              trips_sum = first(trips_sum)) 
  
  a1_combine <- a_combine %>%
    st_set_geometry(NULL) %>%
    group_by(sigla_muni, cenario) %>%
    # multiplcar a area proporcional pela variavel do setor
    # https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones/45947867#45947867
    # summarise(across(starts_with(c("P00", "E00", "S00")), .fns = ~sum(.x * area_prop_hex, na.rm = TRUE)))
    summarise(across(starts_with(c("cor_", "edu_", "saude_")), .fns = ~round(sum(.x * area_prop_hex, na.rm = TRUE))))
  
  return(list(buffer_cenario = a1, buffer_cenario_combine = a1_combine))
  
}

cenario1_socio <- calculate_buffer(cenario1)
cenario2_socio <- calculate_buffer(cenario2)
cenario3_socio <- calculate_buffer(cenario3)

# juntar
cenarios_socio <- list(cenario1_socio, cenario2_socio, cenario3_socio)
cenarios_socio <- purrr::transpose(cenarios_socio)
cenarios_socio <- lapply(cenarios_socio, rbindlist)

cenarios_socio$buffer_cenario_combine
cenarios_socio$buffer_cenario %>% View()
