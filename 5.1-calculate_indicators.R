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
# mapdeck::set_token(fread("../../data/mapbox_key.csv")$key)


# open cenarios
cenario1 <- st_read("../../data/smtr_malha_cicloviaria/4-cenarios_trechos/cenario1_trechos.gpkg") %>% mutate(cenario = "cenario1")
cenario2 <- st_read("../../data/smtr_malha_cicloviaria/4-cenarios_trechos/cenario2_trechos.gpkg")  %>% mutate(cenario = "cenario2")
cenario3 <- st_read("../../data/smtr_malha_cicloviaria/4-cenarios_trechos/cenario3_trechos.gpkg")  %>% mutate(cenario = "cenario3")

# mapview(cenario1) + cenario2
# mapview(cenario2) + cenario3


# get hex with socioeconomic variables
hex <- readr::read_rds("../../data-raw/smtr_malha_cicloviaria/hex_agregado_rio_09.rds") %>%
  mutate(hex_area = st_area(.)) %>% mutate(sigla_muni = "rio")
# identificar a regiao de cada hex
regioes <- st_read("../../data-raw/smtr_malha_cicloviaria/regioes_planejamento.geojson") %>% select(NOME_RP)
# juntar com hex
hex <- st_join(hex, regioes, largest = TRUE)
table(hex$NOME_RP, useNA = "always")

hex_totals <- hex %>% st_set_geometry(NULL) %>%
  group_by(sigla_muni) %>%
  summarise(across(starts_with(c("pop_", "cor_", "edu_", "saude_")), .fns = ~sum(.x, na.rm = TRUE)))

# hex %>% filter(is.na(NOME_RP)) %>% mapview()

# calculate totals


# buffer each cenario
# cenario <- cenario1

calculate_buffer <- function(cenario) {
  
  # cenario_buffer_old <- st_transform(cenario, crs = 31983)
  # cenario_buffer_old <- st_buffer(cenario_buffer_old, dist = 300)
  # cenario_buffer_old <- st_transform(cenario_buffer_old, crs = 4326)
  
  cenario_buffer <- readr::read_rds("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_otp_cenario1_group.rds") %>% st_sf()
  cenario_buffer_raw <- readr::read_rds("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_otp_cenario1_group_raw.rds") %>% st_sf()
  
  # mapview(cenario_buffer %>% filter(osm_id == 116701312)) + 
  #   cenario_buffer_old %>% filter(osm_id == 116701312)
  
  # combine isocronas
  cenario_buffer_combine <- st_sf(geom = st_union(cenario_buffer_raw)) %>% mutate(cenario = unique(cenario_buffer$cenario))
  # mapview(cenario_buffer_combine)
  
  # qual a proporcao da area de cada hexagono que esta dentro de uma isocrona?
  a <- st_intersection(cenario_buffer,
                       hex) %>%
    # calcular a area do pedaco
    mutate(pedaco_area = st_area(.)) %>%
    # calcular a proporcao da area total do hex que esta dentro da isocrona
    mutate(area_prop_hex = as.numeric(pedaco_area) / as.numeric(hex_area))
  
  # para a malha como um todo
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
    summarise(across(starts_with(c("pop_", "cor_", "edu_", "saude_")), .fns = ~round(sum(.x * area_prop_hex, na.rm = TRUE))),
              fase = first(fase)
              # trips_sum = first(trips_sum)
    )  %>%
    # trazer geom
    left_join(select(cenario, osm_id), by = "osm_id")
  
  a1_combine <- a_combine %>%
    st_set_geometry(NULL) %>%
    group_by(sigla_muni, cenario) %>%
    # multiplcar a area proporcional pela variavel do setor
    # https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones/45947867#45947867
    # summarise(across(starts_with(c("P00", "E00", "S00")), .fns = ~sum(.x * area_prop_hex, na.rm = TRUE)))
    summarise(across(starts_with(c("pop_", "cor_", "edu_", "saude_")), .fns = ~round(sum(.x * area_prop_hex, na.rm = TRUE)))) %>%
    ungroup() %>% 
    mutate(tipo = "total") %>%
    setDT()
  # mutate(across(starts_with(c("pop_", "cor_", "edu_", "saude_")), .fns = ~round(.x / hex_totals$.x)))
  
  # calcular proporcoes
  a1_combine_prop <- 
    purrr::map2_dfr(select(a1_combine, pop_total:saude_alta), select(hex_totals, pop_total:saude_alta),
                    function(x, y) round((x / y) * 100, 2)) %>% mutate(tipo = "proporcao") %>% setDT()
    
  # bind
  a1_combine <- rbind(a1_combine, a1_combine_prop, fill = TRUE)
  
  # by regiao
  a1_combine_regiao <- a_combine %>%
    st_set_geometry(NULL) %>%
    group_by(sigla_muni, NOME_RP, cenario) %>%
    summarise(across(starts_with(c("pop_", "cor_", "edu_", "saude_")), .fns = ~round(sum(.x * area_prop_hex, na.rm = TRUE)))) %>%
    mutate(tipo = "total") %>%
    setDT()
  
  # calcular proporcoes
  a1_combine_regiao_prop <- 
    purrr::map2_dfr(select(a1_combine_regiao, pop_total:saude_alta), select(hex_totals, pop_total:saude_alta),
                    function(x, y) round((x / y)*100, 2)) %>% mutate(tipo = "proporcao") %>% setDT()
  # bind
  a1_combine <- rbind(a1_combine_regiao, a1_combine_regiao_prop, fill = TRUE) %>%
    # trazer geom
    left_join(select(regioes, NOME_RP), by = "NOME_RP") 
  
  
  # a_combine %>%
  #   filter(NOME_RP == "Bangu") %>%
  #   group_by(sigla_muni, NOME_RP, cenario) %>%
  #   summarise(across(starts_with(c("pop_", "cor_", "edu_", "saude_")), .fns = ~round(sum(.x * area_prop_hex, na.rm = TRUE)))) %>%
  #   mapview() +
  #   filter(regioes, NOME_RP == "Bangu") + 
  #   cenario1
  #   
  # a_combine %>%
  #   filter(NOME_RP == "Madureira") %>%
  #   group_by(sigla_muni, NOME_RP, cenario) %>%
  #   summarise(across(starts_with(c("pop_", "cor_", "edu_", "saude_")), .fns = ~round(sum(.x * area_prop_hex, na.rm = TRUE)))) %>%
  #   mapview() +
  #   filter(regioes, NOME_RP == "Madureira")+
  # cenario1
  
  
  return(list(buffer_cenario = a1, buffer_cenario_combine = a1_combine, buffer_cenario_combine_regioes = a1_combine_regiao))
  
}

cenario1_socio <- calculate_buffer(cenario1)
cenario2_socio <- calculate_buffer(cenario2)
cenario3_socio <- calculate_buffer(cenario3)


# teste
# cenario1_socio$buffer_cenario %>% View()
# cenario1_socio$buffer_cenario_combine
# cenario1_socio$buffer_cenario_combine_regioes %>% View()

# juntar
cenarios_socio <- list(cenario1_socio)
# cenarios_socio <- list(cenario1_socio, cenario2_socio, cenario3_socio)
cenarios_socio <- purrr::transpose(cenarios_socio)
cenarios_socio <- lapply(cenarios_socio, rbindlist)

# save
cenarios_socio[[1]] %>% 
  st_sf() %>%
  st_write("../../data/smtr_malha_cicloviaria/5.1-indicators/bike_indicators_trechos.gpkg")

cenarios_socio[[2]] %>% 
  fwrite("../../data/smtr_malha_cicloviaria/5.1-indicators/bike_indicators_city.csv")

cenarios_socio[[3]] %>% st_sf() %>%
  st_write("../../data/smtr_malha_cicloviaria/5.1-indicators/bike_indicators_regioes.gpkg")



# checks
sum(cenarios_socio[[1]]$pop_total)
sum(cenarios_socio[[2]]$pop_total)
sum(cenarios_socio[[3]]$pop_total)
