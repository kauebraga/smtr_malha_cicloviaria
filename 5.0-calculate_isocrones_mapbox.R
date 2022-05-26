library(readr)
library(dplyr)
library(opentripplanner)
library(data.table)
library(sf)
library(purrr)
library(mapview)
library(mapboxapi)
mapviewOptions(fgb = FALSE)
# sf::sf_use_s2(FALSE)

# open scenario
cenario1 <- st_read("../../data/smtr_malha_cicloviaria/4-cenarios_trechos/cenario1_trechos.gpkg") %>% mutate(cenario = "cenario1")
cenario1_raw <- st_read("../../data-raw/smtr_malha_cicloviaria/bike_network_atual/Rede_Existente_Final_20220325.geojson") %>%
  mutate(OBJECTID = 1:n()) %>% select(OBJECTID, Rota) %>% st_zm(.) %>% mutate(cenario = "cenario1", fase = "fase1")
# delete emty
cenario1_raw <- cenario1_raw %>% filter(!st_is_empty(.))

cenario2 <- st_read("../../data/smtr_malha_cicloviaria/4-cenarios_trechos/cenario2_trechos.gpkg") %>% mutate(cenario = "cenario2")
cenario2_raw <- st_read("../../data-raw/smtr_malha_cicloviaria/bike_network_planejada/redefinal_Oficina_v1.geojson") %>%
  # create ID
  mutate(OBJECTID = 1:n(), cenario = "cenario2") %>%
  # create fase
  mutate(fase = ifelse(status == "existente", "fase1", "fase2")) %>%
  select(OBJECTID,cenario,  fase, Rota = Trecho)
# convert to linestring
# st_cast("LINESTRING")
# delete emty
cenario2_raw <- cenario2_raw %>% filter(!st_is_empty(.))
cenario2_raw <- st_make_valid(cenario2_raw)

cenario3 <- st_read("../../data/smtr_malha_cicloviaria/4-cenarios_trechos/cenario3_trechos.gpkg") %>% 
  mutate(cenario = "cenario3")


# cenario_raw <- cenario2_raw

calculate_iso_cenario <- function(cenario = NULL, cenario_raw = NULL) {
  
  # get cenario
  cenario_id <- unique(cenario_raw$cenario)
  
  # Get samples at every 100 meters
  cenario_raw_utm <- st_transform(cenario_raw, 3857) %>%
    st_cast("LINESTRING")
  cenario_raw_points <- st_line_sample(cenario_raw_utm, density = 1/100)
  
  # Transform back to a sf data.frame
  cenario_raw_points <- map(cenario_raw_points, function(x) data.frame(geometry = st_geometry(x))) %>%
    map_df(as.data.frame) %>% st_sf() %>%
    mutate(OBJECTID = cenario_raw_utm$OBJECTID, 
           cenario = cenario_raw_utm$cenario,
           fase = cenario_raw_utm$fase,
           Rota = cenario_raw_utm$Rota) %>%
    st_cast("POINT") %>%
    st_set_crs(3857) %>%
    st_transform(4326) %>%
    mutate(id = 1:n())
  
  # run iso
  iso_mapbox1 <- mb_isochrone(cenario_raw_points[1:1000,],
               profile = "walking",
               distance = 300,
               id_column = "id",
               access_token = "pk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNqa2JoN3VodDMxa2YzcHFxMzM2YWw1bmYifQ.XAhHAgbe0LcDqKYyqKYIIQ")
  iso_mapbox1 <- iso_mapbox1 %>% left_join(cenario_raw_points %>% st_set_geometry(NULL), by = "id")
  readr::write_rds(iso_mapbox1, sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_otp_%s_raw_mapbox1.rds", cenario_id))
  iso_mapbox2 <- mb_isochrone(cenario_raw_points[1001:2000,],
               profile = "walking",
               distance = 300,
               id_column = "id",
               access_token = "pk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNqa2JoN3VodDMxa2YzcHFxMzM2YWw1bmYifQ.XAhHAgbe0LcDqKYyqKYIIQ")
  iso_mapbox2 <- iso_mapbox2 %>% left_join(cenario_raw_points %>% st_set_geometry(NULL), by = "id")
  readr::write_rds(iso_mapbox2, sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_otp_%s_raw_mapbox2.rds", cenario_id))
  iso_mapbox3 <- mb_isochrone(cenario_raw_points[2001:7000,],
               profile = "walking",
               distance = 300,
               id_column = "id",
               access_token = "pk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNqa2JoN3VodDMxa2YzcHFxMzM2YWw1bmYifQ.XAhHAgbe0LcDqKYyqKYIIQ")
  iso_mapbox3 <- iso_mapbox3 %>% left_join(cenario_raw_points %>% st_set_geometry(NULL), by = "id")
  readr::write_rds(iso_mapbox3, sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_otp_%s_raw_mapbox3.rds", cenario_id))
  iso_mapbox4 <- mb_isochrone(cenario_raw_points[7001:nrow(cenario_raw_points),],
               profile = "walking",
               distance = 300,
               id_column = "id",
               access_token = "pk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNqa2JoN3VodDMxa2YzcHFxMzM2YWw1bmYifQ.XAhHAgbe0LcDqKYyqKYIIQ")
  iso_mapbox4 <- iso_mapbox4 %>% left_join(cenario_raw_points %>% st_set_geometry(NULL), by = "id")
  readr::write_rds(iso_mapbox4, sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_otp_%s_raw_mapbox4.rds", cenario_id))
  
  
  # mapview(a) + a_raw
  
  # quais pontinhos nao foram roteados?
  a_faltantes <- setdiff(cenario_points_coords$id, a$fromPlace) # 2915 / 33900 = 9% vs 4120 / 33900 = 12%
  # para esses, fazer um buffer comum de 200 metros
  cenario_faltantes <- cenario_points_coords %>% filter(id %in% a_faltantes) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(crs = 31983) %>%
    st_buffer(dist = 200) %>%
    st_transform(crs = 4326)
  
  a_raw_faltantes <- setdiff(cenario_raw_points_coords$id, a_raw$fromPlace) # 2097 / 19117 = 11% vs 2898 / 19117 = 15%
  # para esses, fazer um buffer comum de 200 metros
  cenario_raw_faltantes <- cenario_raw_points_coords %>% filter(id %in% a_raw_faltantes) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(crs = 31983) %>%
    st_buffer(dist = 200) %>%
    st_transform(crs = 4326)
  
  # trazer osm_id
  a1<- a %>%
    # st_cast("POLYGON") %>%
    select(-id, -time) %>%
    rename(id = fromPlace) %>%
    left_join(cenario_points_coords %>% select(id, osm_id))
  
  a1 <- a1 %>% st_make_valid()
  
  a1_raw <- a_raw %>%
    # st_cast("POLYGON") %>%
    select(-id, -time) %>%
    rename(id = fromPlace) %>%
    left_join(cenario_raw_points_coords %>% select(id, OBJECTID))
  # juntar os faltantes
  a1 <- a1 %>%
    rbind(cenario_faltantes %>% select(-fase, -cenario, -name, -highway))
  a1_raw <- a1_raw %>%
    rbind(cenario_raw_faltantes %>% select(-fase, -cenario))
  
  a1 <- a1 %>% st_make_valid()
  a1_raw <- a1_raw %>% st_make_valid()
  
  # oi <- a1 %>% group_by(osm_id) %>% summarise()
  oi <- aggregate(a1, by = list(a1$osm_id), FUN = first)
  oi_raw <- aggregate(a1_raw, by = list(a1_raw$OBJECTID), FUN = first)
  
  # cenario_unique <- cenario1 %>% 
  #   st_set_geometry(NULL) %>%
  #   distinct(osm_id, name, highway, cenario, fase, Rota, trips_sum) 
  # cenario_unique_raw <- cenario1_raw %>% 
  #   st_set_geometry(NULL) %>%
  #   distinct(OBJECTID, Rota,cenario) 
  # 
  # # bring vars
  # oi1 <- oi %>%
  #   left_join(cenario_unique, by = "osm_id")
  # oi1_raw <- oi_raw %>%
  #   left_join(cenario_unique_raw, by = "OBJECTID")
  
  # finalizar
  cenario_iso <- cenario %>%
    st_set_geometry(NULL) %>%
    distinct(osm_id, name, fase, cenario, trips_total, trips_weekday_peak_morning, trips_weekday_peak_afternoon, trips_weekday_offpeak,trips_weekend ) %>%
    left_join(oi %>% select(-Group.1, -id), by = "osm_id")
  cenario_raw_iso <- cenario_raw %>%
    st_set_geometry(NULL) %>%
    left_join(oi_raw %>% select(-Group.1, -id), by = "OBJECTID")
  
  
  # save
  readr::write_rds(cenario_iso,     sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_otp_%s_group.rds", cenario_id))
  st_write(cenario_iso,     sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_otp_%s_group.gpkg", cenario_id))
  readr::write_rds(cenario_raw_iso, sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_otp_%s_group_raw.rds", cenario_id))
  sf::st_write(cenario_raw_iso %>% st_sf(), sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_otp_%s_group_raw.gpkg", cenario_id))
  
}

# mapview(cenario1) + cenario1_raw


# build buffer --------------------------------------------------------------------------------

# cenario <- "cenario3"

build_buffer_combine <- function(cenario) {
  
  
  
  
  if (cenario %in% c("cenario1", "cenario2")) {
    
    
    cenario_buffer_raw <- readr::read_rds(sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_otp_%s_group_raw.rds", cenario)) %>%
      st_sf()
    
    # combine isocronas
    cenario_buffer_combine <- st_sf(geom = st_union(cenario_buffer_raw)) %>% mutate(cenario = cenario)
    
  } else {
    
    # p/ cenario 3
    cenario_buffer <- readr::read_rds(sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_otp_%s_group.rds", cenario)) %>% st_sf()
    
    cenario_buffer_raw <- readr::read_rds(sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_otp_%s_group_raw.rds", "cenario2")) %>%
      st_sf() %>%
      select(cenario)
    
    # pegar somente os vazios, q sao da fase 3
    cenario_buffer_vazios <- cenario_buffer %>% filter(fase == "fase3") %>% select(cenario)
    # juntar
    cenario_buffer_combine <- rbind(cenario_buffer_raw, cenario_buffer_vazios)
    # combine isocronas
    cenario_buffer_combine <- st_sf(geom = st_union(cenario_buffer_combine)) %>% mutate(cenario = cenario)
    
  }
  
  file <- sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_%s_combine.gpkg", cenario)
  file.remove(file)
  # export combine
  st_write(cenario_buffer_combine, file)
  
}


purrr::walk(c("cenario1", "cenario2", "cenario3"), build_buffer_combine)

googledrive::drive_put(media = "../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_cenario1_combine.gpkg",
                       path = "SRTM - Infraestrutura cicloviaria/5.0-isocronas",
                       name = "iso_cenario1_combine.gpkg")
googledrive::drive_put(media = "../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_cenario2_combine.gpkg",
                       path = "SRTM - Infraestrutura cicloviaria/5.0-isocronas",
                       name = "iso_cenario2_combine.gpkg")
googledrive::drive_put(media = "../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_cenario3_combine.gpkg",
                       path = "SRTM - Infraestrutura cicloviaria/5.0-isocronas",
                       name = "iso_cenario3_combine.gpkg")
