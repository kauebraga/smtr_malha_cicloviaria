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
# mapviewOptions(platform = "mapdeck")
# mapviewOptions(platform = "leafgl")
mapdeck::set_token(fread("../../data/mapbox_key.csv")$key)


# open cenarios
cenario1 <- st_read("../../data/smtr_malha_cicloviaria/4-osm_cenarios/osm_cenario1.gpkg") %>% mutate(cenario = "cenario1")
cenario2 <- st_read("../../data/smtr_malha_cicloviaria/4-osm_cenarios/osm_cenario2.gpkg")  %>% mutate(cenario = "cenario2")
cenario3 <- st_read("../../data/smtr_malha_cicloviaria/4-osm_cenarios/osm_cenario3.gpkg")  %>% mutate(cenario = "cenario3")

# mapview(cenario1) + cenario2
# mapview(cenario2) + cenario3


# get hex with socioeconomic variables
hex <- kauetools::read_data("hex_agregado_rio_09.rds", folder = "data-raw") %>%
  mutate(hex_area = st_area(.)) %>% mutate(sigla_muni = "rio")
# identificar a regiao de cada hex
regioes <- st_read("../../data-raw/smtr_malha_cicloviaria/regioes_planejamento.geojson") %>% select(NOME_RP)
# juntar com hex
hex <- st_join(hex, regioes, largest = TRUE)
table(hex$NOME_RP, useNA = "always")

# buffer each cenario
# cenario <- cenario1

calculate_buffer <- function(cenario) {
  
  cenario_buffer <- st_transform(cenario, crs = 31983)
  cenario_buffer <- st_buffer(cenario_buffer, dist = 300)
  cenario_buffer <- st_transform(cenario_buffer, crs = 4326)
  
  # combine isocronas
  cenario_buffer_combine <- st_sf(geom = st_union(cenario_buffer)) %>% mutate(cenario = unique(cenario_buffer$cenario))
  mapview(cenario_buffer_combine)
  
  # group by name
  cenario_buffer_group <- 
    
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
    # group_by(sigla_muni, name, cenario, fase) %>%
    group_by(sigla_muni, osm_id, name, cenario, fase) %>%
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
  
  # by regiao
  a1_combine_regiao <- a_combine %>%
    st_set_geometry(NULL) %>%
    group_by(sigla_muni, NOME_RP, cenario) %>%
    summarise(across(starts_with(c("cor_", "edu_", "saude_")), .fns = ~round(sum(.x * area_prop_hex, na.rm = TRUE))))
    
    
  
  
  return(list(buffer_cenario = a1, buffer_cenario_combine = a1_combine, buffer_cenario_combine_regioes = a1_combine_regiao))
  
}

cenario1_socio <- calculate_buffer(cenario1)
cenario2_socio <- calculate_buffer(cenario2)
cenario3_socio <- calculate_buffer(cenario3)


# teste
# cenario1_socio$buffer_cenario %>% View()
# cenario1_socio$buffer_cenario_combine
# cenario1_socio$buffer_cenario_combine_regioes

# juntar
cenarios_socio <- list(cenario1_socio, cenario2_socio, cenario3_socio)
cenarios_socio <- purrr::transpose(cenarios_socio)
cenarios_socio <- lapply(cenarios_socio, rbindlist)

cenarios_socio$buffer_cenario_combine
cenarios_socio$buffer_cenario %>% View()


# check only fase 3
a <- cenarios_socio$buffer_cenario %>%
  filter(fase == "fase3")
