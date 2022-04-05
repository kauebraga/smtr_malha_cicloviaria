library(readr)
library(dplyr)
library(opentripplanner)
library(data.table)
library(sf)
library(purrr)
library(mapview)
mapviewOptions(fgb = FALSE)

# open scenario
cenario1 <- st_read("../../data/smtr_malha_cicloviaria/4-cenarios_trechos/cenario1_trechos.gpkg") %>% mutate(cenario = "cenario1")
cenario1_raw <- st_read("../../data-raw/smtr_malha_cicloviaria/bike_network_atual/Rede_Existente_Final_20220325.geojson") %>%
  mutate(OBJECTID = 1:n()) %>% select(OBJECTID, Rota) %>% st_zm(.) %>% mutate(cenario = "cenario1", fase = "fase1")
# delete emty
cenario1_raw <- cenario1_raw %>% filter(!st_is_empty(.))

# mapview(cenario1) + cenario1_raw

# break scenario in points every 20 meters
# standardize shape resolution - at least every 20 meters
cenario_points <- st_segmentize(cenario1, dfMaxLength = 50)
# shapes_linhas_filter <- sfheaders::sf_cast(shapes_linhas_filter, "POINT")
# transform to lon lat - these points will be the origins
cenario_points_coords <- sfheaders::sf_to_df(cenario_points, fill = TRUE) %>% 
  mutate(id = 1:n()) %>%
  select(id, osm_id, name, highway, fase, cenario, lon = x, lat = y) %>%
  setDT()

# break scenario in points every 20 meters
# standardize shape resolution - at least every 20 meters
cenario_raw_points <- st_segmentize(cenario1_raw, dfMaxLength = 50)
# shapes_linhas_filter <- sfheaders::sf_cast(shapes_linhas_filter, "POINT")
# transform to lon lat - these points will be the origins
cenario_raw_points_coords <- sfheaders::sf_to_df(cenario_raw_points, fill = TRUE) %>% 
  mutate(id = 1:n()) %>%
  select(id, OBJECTID, fase, cenario, lon = x, lat = y) %>%
  setDT()

# build graph
otp_build_graph(otp = "otp/programs/otp-1.5.0-shaded.jar",
                dir = "otp", 
                router = "rio")

# run this and wait until the message "INFO (GrizzlyServer.java:153) Grizzly server running." show up
# may take a few minutes for a big city
otp_setup(otp = "otp/programs/otp-1.5.0-shaded.jar", 
          dir = "otp", 
          router = "rio", 
          memory = 8000,
          port = 8080, wait = FALSE)

# register the router
otp_rio <- otp_connect(router = "rio")
# otp_rio <- list(hostname = "localhost", router = "rio", url = NULL, 
#                port = "8080", ssl = FALSE, otp_version = "1.5.0")
# class(otp_rio) <- "otpconnect"

# apply function
# a <- opentripplanner::otp_isochrone(otpcon = otp_rio,
#                                     fromPlace = c(-43.67406, -22.8927),
#                                     mode = "WALK",
#                                     # dist = c(1000, 2000, 3000),
#                                     cutoffSec = 300)
# 
# mapview::mapview(a)

coords_list <- purrr::map2(as.numeric(cenario_points_coords$lon), as.numeric(cenario_points_coords$lat), c)
coords_list <- matrix(c(cenario_points_coords$lon, cenario_points_coords$lat), ncol = 2)

coords_raw_list <- purrr::map2(as.numeric(cenario_raw_points_coords$lon), as.numeric(cenario_raw_points_coords$lat), c)
coords_raw_list <- matrix(c(cenario_raw_points_coords$lon, cenario_raw_points_coords$lat), ncol = 2)

routingOptions <- otp_routing_options()
routingOptions$walkSpeed <- 1
routingOptions <- otp_validate_routing_options(routingOptions)

my_iso <- function(coords, id = NULL, time, mode1, connection) {
  
  otp_isochrone(otpcon = connection,
                fromPlace = coords,
                fromID = id,
                cutoffSec = time,
                mode = mode1,
                ncores = 10,
                routingOptions = routingOptions
  )
  
}


# # apply isochrones to list of coordinates
# a <- lapply(coords_list[1:100], purrr::possibly(my_iso, otherwise = "erro"),
#             id = cenario_points_coords$id,
#             time = c(300), # default walkspeed is 1.333 m/s, so time for 500 meters is 500/1.33 = 376 secs
#             mode1 = "WALK",
#             connection = otp_rio)


a <- my_iso(coords_list,
            id = cenario_points_coords$id,
            time = c(300), # default walkspeed is 1.333 m/s, so time for 500 meters is 500/1.33 = 376 secs
            mode1 = "WALK",
            connection = otp_rio)

a_raw <- my_iso(coords_raw_list,
            id = cenario_raw_points_coords$id,
            time = c(300), # default walkspeed is 1.333 m/s, so time for 500 meters is 500/1.33 = 376 secs
            mode1 = "WALK",
            connection = otp_rio)

readr::write_rds(a, "../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_otp_cenario1.rds")
readr::write_rds(a_raw, "../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_otp_cenario1_raw.rds")
a <- readr::read_rds("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_otp_cenario1.rds")
a_raw <- readr::read_rds("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_otp_cenario1_raw.rds")

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
cenario1_iso <- cenario1 %>%
  st_set_geometry(NULL) %>%
  distinct(osm_id, name, fase, cenario, trips_sum) %>%
  left_join(oi %>% select(-Group.1, -id), by = "osm_id")
cenario1_raw_iso <- cenario1_raw %>%
  st_set_geometry(NULL) %>%
  left_join(oi_raw %>% select(-Group.1, -id), by = "OBJECTID")

# quantos segmentos nao foram estimados?
setdiff(cenario1$osm_id, oi1$osm_id) # pouquissimmos

# save
readr::write_rds(cenario1_iso, "../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_otp_cenario1_group.rds")
readr::write_rds(cenario1_raw_iso, "../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_otp_cenario1_group_raw.rds")
sf::st_write(cenario1_raw_iso %>% st_sf(), "../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_otp_cenario1_group_raw.gpkg")
