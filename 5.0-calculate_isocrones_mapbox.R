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


# para cenarios 1 e 2 -----------------------------------------------------


cenario2_raw <- st_read("../../data-raw/smtr_malha_cicloviaria/bike_network_planejada/redefinal_Oficina_v1.geojson") %>%
  # create ID
  mutate(OBJECTID = 1:n(), cenario = "cenario2") %>%
  # create fase
  mutate(fase = ifelse(status == "existente", "fase1", "fase2")) %>%
  select(OBJECTID,cenario,  fase, Rota = Trecho)
# delete emty
cenario2_raw <- cenario2_raw %>% filter(!st_is_empty(.))
cenario2_raw <- st_make_valid(cenario2_raw)



# get cenario
cenario_id <- "cenario2"

# Get samples at every 100 meters
cenario_raw_utm <- st_transform(cenario2_raw, 3857) %>%
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
# juntar tudo
a <- lapply(sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_otp_%s_raw_%s.rds", c(cenario_id), c("mapbox1", "mapbox2", "mapbox3", "mapbox4")),
            read_rds)
# ajustar classe da geometria do objeto 2
class(a[[2]]$geometry) <- c("sfc_GEOMETRY", "sfc")
# juntar
a <- rbindlist(a) %>% st_sf()

# oi <- a1 %>% group_by(osm_id) %>% summarise()
oi_raw <- aggregate(a, by = list(a$OBJECTID), FUN = first)
mapview(oi_raw)

cenario2_raw_iso <- cenario_raw %>%
  st_set_geometry(NULL) %>%
  left_join(oi_raw %>% select(OBJECTID), by = "OBJECTID") %>%
  st_sf()

# dividir cenario1
cenario1_raw_iso <- cenario2_raw_iso %>%
  filter(fase == "fase1")


# save
sf::st_write(cenario1_raw_iso, "../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_mapbox_cenario1.gpkg")
sf::st_write(cenario2_raw_iso, "../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_mapbox_cenario2.gpkg")



# para cenario3 -----------------------------------------------------------

# trazer os trechos vazios
fase3 <- st_read("../../data/smtr_malha_cicloviaria/3.3-vazios_trechos/od_vazios_trechos.gpkg") %>%
  mutate(OBJECTID = NA, Rota = NA, fase = "fase3") 

# Get samples at every 100 meters
fase3_utm <- st_transform(fase3, 3857) %>%
  st_cast("LINESTRING")
fase3_points <- st_line_sample(fase3_utm, density = 1/100)

# Transform back to a sf data.frame
fase3_points <- map(fase3_points, function(x) data.frame(geometry = st_geometry(x))) %>%
  map_df(as.data.frame) %>% st_sf() %>%
  mutate(OBJECTID = NA, 
         cenario = NA,
         fase = fase3_utm$fase,
         Rota = NA) %>%
  st_cast("POINT") %>%
  st_set_crs(3857) %>%
  st_transform(4326) %>%
  mutate(id = 1:n())

# run iso
iso_mapbox_cenario3 <- mb_isochrone(fase3_points,
                                    profile = "walking",
                                    distance = 300,
                                    id_column = "id",
                                    access_token = "pk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNqa2JoN3VodDMxa2YzcHFxMzM2YWw1bmYifQ.XAhHAgbe0LcDqKYyqKYIIQ")
iso_mapbox_cenario3 <- iso_mapbox_cenario3 %>% left_join(fase3_points %>% st_set_geometry(NULL), by = "id")
readr::write_rds(iso_mapbox_cenario3, "../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_otp_fase3_raw_mapbox.rds")







# build buffer --------------------------------------------------------------------------------

# cenario <- "cenario2"

build_buffer_combine <- function(cenario) {
  
  cenario_buffer_raw <- sf::st_read(sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_mapbox_%s.gpkg", cenario))
  
  # combine isocronas
  cenario_buffer_combine <- st_sf(geom = st_union(cenario_buffer_raw)) %>% mutate(cenario = cenario)
  
  file <- sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_combine_mapbox_%s.gpkg", cenario)
  file.remove(file)
  # export combine
  st_write(cenario_buffer_combine, file)
  
}


purrr::walk(c("cenario1", "cenario2", "cenario3"), build_buffer_combine)

googledrive::drive_put(media = "../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_combine_mapbox_cenario1.gpkg",
                       path = "SRTM - Infraestrutura cicloviaria/5.0-isocronas",
                       name = "iso_combine_mapbox_cenario1.gpkg")
googledrive::drive_put(media = "../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_combine_mapbox_cenario2.gpkg",
                       path = "SRTM - Infraestrutura cicloviaria/5.0-isocronas",
                       name = "iso_combine_mapbox_cenario2.gpkg")
googledrive::drive_put(media = "../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_combine_mapbox_cenario3.gpkg",
                       path = "SRTM - Infraestrutura cicloviaria/5.0-isocronas",
                       name = "iso_combine_mapbox_cenario3.gpkg")
