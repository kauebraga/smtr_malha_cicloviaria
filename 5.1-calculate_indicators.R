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
# cenario1 <- st_read("../../data/smtr_malha_cicloviaria/4-cenarios_trechos/cenario1_trechos.gpkg") %>% mutate(cenario = "cenario1")
cenario2 <- st_read("../../data/smtr_malha_cicloviaria/4-cenarios_trechos/cenario2_trechos.gpkg")  %>% mutate(cenario = "cenario2")
# cenario3 <- st_read("../../data/smtr_malha_cicloviaria/4-cenarios_trechos/cenario3_trechos.gpkg")  %>% mutate(cenario = "cenario3")

# mapview(cenario1) + cenario2
# mapview(cenario2) + cenario3


# get hex with socioeconomic variables
hex <- readr::read_rds("../../data-raw/smtr_malha_cicloviaria/hex_agregado_rio_09.rds") %>%
  mutate(hex_area = st_area(.)) %>% mutate(sigla_muni = "rio")
# identificar a regiao de cada hex
regioes <- st_read("../../data-raw/smtr_malha_cicloviaria/regioes_planejamento.geojson") %>% select(NOME_RP, AP, RP)
# juntar com hex
hex <- st_join(hex, regioes, largest = TRUE)
hex <- hex %>% filter(!is.na(NOME_RP))
table(hex$NOME_RP, useNA = "always")

# mapview(hex %>% filter(is.na(NOME_RP)))

# calculate totals
hex_totals <- hex %>% st_set_geometry(NULL) %>%
  group_by(sigla_muni) %>%
  summarise(across(starts_with(c("pop_", "cor_", "empregos_total",  "edu_", "saude_")), .fns = ~sum(.x, na.rm = TRUE))) %>%
  ungroup()

hex_totals_regioes <- hex %>% st_set_geometry(NULL) %>%
  group_by(sigla_muni, NOME_RP) %>%
  summarise(across(starts_with(c("pop_", "cor_",  "empregos_total", "edu_", "saude_")), .fns = ~sum(.x, na.rm = TRUE))) %>%
  ungroup()

# hex %>% filter(is.na(NOME_RP)) %>% mapview()

# abir estacoes
estacoes <- geojsonsf::geojson_sf("../../data-raw/smtr_malha_cicloviaria/Estacoes_mediaalta_transporte.geojson")
estacoes <- st_zm(estacoes)
estacoes <- st_set_crs(estacoes, 32723)
estacoes <- st_transform(estacoes, 4326)
estacoes <- estacoes %>%
  mutate(sigla_muni = "rio") %>%
  select(sigla_muni, Station = Nome)
# estacoes <- st_read("../../data-raw/smtr_malha_cicloviaria/estacoes_capacidade_ITDP/estacoes_2019.shp") %>%
#   filter(City == "Rio de Janeiro", Status == "Operational") %>%
#   mutate(sigla_muni = "rio") %>%
#   select(sigla_muni, Station)

# juntar com regioes
estacoes <- st_join(estacoes, regioes)
# tirar estacoes fora do rio
estacoes <- estacoes %>% filter(!is.na(NOME_RP))

estacoes_totals <- estacoes %>% st_set_geometry(NULL) %>%
  count(sigla_muni, name = "estacoes_n") %>% ungroup()

estacoes_totals_regioes <- estacoes %>% st_set_geometry(NULL) %>%
  count(sigla_muni, NOME_RP, name = "estacoes_n") %>% ungroup()

# juntar esses valores com os hex totals
hex_totals <- left_join(hex_totals, estacoes_totals)
hex_totals_regioes <- left_join(hex_totals_regioes, estacoes_totals_regioes, by = c("sigla_muni", "NOME_RP"))
# mapview(estacoes)

# buffer each cenario
# cenario <- cenario1

# cenario <- "cenario2" 

calculate_buffer <- function(cenario) {
  
  # cenario_buffer_old <- st_transform(cenario, crs = 31983)
  # cenario_buffer_old <- st_buffer(cenario_buffer_old, dist = 300)
  # cenario_buffer_old <- st_transform(cenario_buffer_old, crs = 4326)
  
  cenario_buffer <- readr::read_rds(  sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_otp_%s_group.rds", cenario)) %>% st_sf()
  cenario_buffer_raw <- readr::read_rds(sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_otp_%s_group_raw.rds", cenario)) %>%
    st_sf()
  
  # mapview(cenario_buffer %>% filter(osm_id == 116701312)) + 
  #   cenario_buffer_old %>% filter(osm_id == 116701312)
  
  # combine isocronas
  cenario_buffer_combine <- st_sf(geom = st_union(cenario_buffer_raw)) %>% mutate(cenario = cenario)
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
    summarise(across(starts_with(c("pop_", "cor_", "empregos_total", "edu_", "saude_")), .fns = ~round(sum(.x * area_prop_hex, na.rm = TRUE))),
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
    summarise(across(starts_with(c("pop_", "cor_",  "empregos_total", "edu_", "saude_")), .fns = ~round(sum(.x * area_prop_hex, na.rm = TRUE)))) %>%
    ungroup() %>% 
    setDT()
  # mutate(across(starts_with(c("pop_", "cor_", "edu_", "saude_")), .fns = ~round(.x / hex_totals$.x)))
  
  # para estacoes de media/alta capacidade
  a1_combine_estacoes <- st_join(cenario_buffer_combine,
                                 estacoes) %>%
    st_set_geometry(NULL) %>% count(sigla_muni, name = "estacoes_n")
  
  # fazer juncao
  a1_combine <- left_join(a1_combine, a1_combine_estacoes, by = "sigla_muni") %>%
    mutate(tipo = "total")
  
  
  # calcular proporcoes
  a1_combine_prop <- 
    purrr::map2_dfr(select(a1_combine, pop_total:estacoes_n), select(hex_totals, pop_total:estacoes_n),
                    function(x, y) round((x / y) * 100, 2)) %>% mutate(sigla_muni = "rio", cenario = cenario, 
                                                                       tipo = "proporcao") %>% setDT()
  
  # bind
  a1_combine <- rbind(a1_combine, a1_combine_prop)
  
  # trazer os totais da cidade
  hex_totals1 <- hex_totals %>%
    mutate(cenario = "cenario1", 
           tipo = "total_cidade") %>% setDT()
  a1_combine <- rbind(a1_combine, hex_totals1)
  
  # by regiao
  a1_combine_regiao <- a_combine %>%
    st_set_geometry(NULL) %>%
    group_by(sigla_muni, NOME_RP, cenario) %>%
    summarise(across(starts_with(c("pop_", "cor_", "empregos_total", "edu_", "saude_")), .fns = ~round(sum(.x * area_prop_hex, na.rm = TRUE)))) %>%
    setDT()
  
  # adicionar alguma regiao q esteja faltando
  dif_regiao <- setdiff(hex$NOME_RP, a1_combine_regiao$NOME_RP)
  
  # add
  if (length(dif_regiao) > 0) {
    a1_combine_regiao <- rbind(a1_combine_regiao,
                               data.table(sigla_muni = "rio", NOME_RP = dif_regiao, cenario = "cenario1"),
                               fill = TRUE)
  }
  
  # para estacoes de media/alta capacidade
  a1_combine_estacoes_regiao <- st_join(cenario_buffer_combine,
                                        estacoes) %>%
    st_set_geometry(NULL) %>% count(sigla_muni, NOME_RP, name = "estacoes_n")
  
  # fazer juncao
  a1_combine_regiao <- left_join(a1_combine_regiao, a1_combine_estacoes_regiao, by = c("sigla_muni", "NOME_RP")) %>%
    mutate(tipo = "total")
  
  a1_combine_regiao <- arrange(a1_combine_regiao, NOME_RP)
  
  # calcular proporcoes
  a1_combine_regiao_prop <- 
    purrr::map2_dfr(select(arrange(a1_combine_regiao, NOME_RP), pop_total:estacoes_n), 
                    select(hex_totals_regioes, pop_total:estacoes_n),
                    function(x, y) round((x / y)*100, 2)) %>% 
    mutate(NOME_RP = a1_combine_regiao$NOME_RP,
           sigla_muni = "rio", cenario = cenario, 
           tipo = "proporcao") %>% setDT()
  
  # bind
  a1_combine_regiao <- rbind(a1_combine_regiao, a1_combine_regiao_prop)
  
  # trazer os totais de cada regiao
  hex_totals_regioes1 <- hex_totals_regioes %>%
    mutate(cenario = cenario, 
           tipo = "total_RP") %>% setDT()
  a1_combine_regiao <- rbind(a1_combine_regiao, hex_totals_regioes1)
  # trazer geom
  # left_join(regioes %>% select(NOME_RP), by = "NOME_RP") %>%
  # select(sigla_muni, AP, RP, NOME_RP, tipo,
  # starts_with(c("pop_", "cor_", "edu_", "saude_")))
  
  # trocar NAS por zero
  a1_combine_regiao[is.na(a1_combine_regiao)] <- 0
  
  # trazer pra regiao
  regioes_fim <- regioes %>%
    select(AP, RP, NOME_RP) %>%
    left_join(a1_combine_regiao, by = "NOME_RP")
  
  
  # regioes_fim %>% filter(NOME_RP == "Inhaúma")
  
  # mapview(cenario_buffer_raw) + regioes %>% filter(NOME_RP == "Inhaúma")
  # mapview(cenario_buffer_raw) + regioes %>% filter(NOME_RP == "Centro")
  
  
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
  
  
  return(list(buffer_cenario = a1, buffer_cenario_combine = a1_combine, buffer_cenario_combine_regioes = regioes_fim))
  
}

cenario1_socio <- calculate_buffer(cenario1)
# cenario2_socio <- calculate_buffer(cenario2)
# cenario3_socio <- calculate_buffer(cenario3)


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

# cenarios_socio[[2]] %>% 
a1_combine %>%
  fwrite(sprintf("../../data/smtr_malha_cicloviaria/5.1-indicators/bike_indicators_%s_city.csv", cenario))

# cenarios_socio[[3]] %>%
regioes_fim %>%
  st_write(sprintf("../../data/smtr_malha_cicloviaria/5.1-indicators/bike_indicators_%s_regioes.gpkg", cenario))



# checks
sum(cenarios_socio[[1]]$pop_total)
sum(cenarios_socio[[2]]$pop_total)
sum(cenarios_socio[[3]]$pop_total)
