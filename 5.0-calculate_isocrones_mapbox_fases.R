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
mapbox_token <- fread("../../data-raw/smtr_malha_cicloviaria/mapbox_key.txt", header = FALSE)$V1


# para cenarios 1 e 2 -----------------------------------------------------


fase1 <- st_read("../../data-raw/smtr_malha_cicloviaria/bike_network_atual/Rede_Existente_Final_20220531.shp") %>%
  # add variables
  mutate(OBJECTID = 1:n()) %>%
  mutate(fase = "fase1") %>%
  select(OBJECTID, Rota, fase, Status)

# remove z coordinates
fase1 <- st_zm(fase1)

# Get samples at every 100 meters
fase1_utm <- st_transform(fase1, 3857) %>%
  st_cast("LINESTRING")
fase1_utm_points <- st_line_sample(fase1_utm, density = 1/150)

# Transform back to a sf data.frame
fase1_points <- map(fase1_utm_points, function(x) data.frame(geometry = st_geometry(x))) %>%
  map_df(as.data.frame) %>% st_sf() %>%
  mutate(OBJECTID = fase1_utm$OBJECTID, 
         fase = fase1_utm$fase,
         Rota = fase1_utm$Rota) %>%
  st_cast("POINT") %>%
  st_set_crs(3857) %>%
  st_transform(4326) %>%
  mutate(id = 1:n())

my_iso_mapbox <- function(df) {
  
  iso_mapbox <- mb_isochrone(df,
                             profile = "walking",
                             distance = 300,
                             id_column = "id",
                             access_token = mapbox_token)
  
  iso_mapbox <- iso_mapbox %>% left_join(fase1_points %>% st_set_geometry(NULL), by = "id")  
  
}

# run iso
iso_mapbox1 <- my_iso_mapbox(fase1_points[1:1000,])
readr::write_rds(iso_mapbox1, sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_mapbox_fase1_raw1.rds"))

iso_mapbox2 <- my_iso_mapbox(fase1_points[1001:2000,])
readr::write_rds(iso_mapbox2, sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_mapbox_fase1_raw2.rds"))

iso_mapbox3 <- my_iso_mapbox(fase1_points[2001:nrow(fase1_points),])
readr::write_rds(iso_mapbox3, sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_mapbox_fase1_raw3.rds"))


# mapview(a) + a_raw
# juntar tudo
a <- lapply(sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_mapbox_fase1_%s.rds", c("raw1", "raw2", "raw3")),
            read_rds)
# ajustar classe da geometria do objeto 2
class(a[[2]]$geometry) <- c("sfc_GEOMETRY", "sfc")
# juntar
a <- rbindlist(a) %>% st_sf()

# oi <- a1 %>% group_by(osm_id) %>% summarise()
oi_raw <- aggregate(a, by = list(a$OBJECTID), FUN = first)
# mapview(oi_raw) + fase1

fase1_iso <- fase1 %>%
  st_set_geometry(NULL) %>%
  left_join(oi_raw %>% select(OBJECTID), by = "OBJECTID") %>%
  st_sf()

fase1_iso <- fase1_iso %>% filter(Status %nin% c("Interditada", "Não identificado"))

# save
sf::st_write(fase1_iso, "../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_mapbox_fase1.gpkg", append = FALSE)



# para cenario3 -----------------------------------------------------------







# build buffer --------------------------------------------------------------------------------

# cenario <- "cenario1"

build_buffer_combine <- function(cenario) {
  
  if (cenario == "cenario1") {
    
  cenario_buffer_raw <- sf::st_read(sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_mapbox_fase1.gpkg"))
  
    
  } else if (cenario == "cenario2") {
    
  cenario_buffer_raw <- rbind(sf::st_read(sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_mapbox_fase1.gpkg")),
                              sf::st_read(sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_mapbox_fase2.gpkg")))
    
    
  }
  
  # combine isocronas
  cenario_buffer_combine <- st_sf(geom = st_union(cenario_buffer_raw)) %>% mutate(cenario = cenario)
  
  file <- sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/combine/iso_combine_mapbox_%s.gpkg", cenario)
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
