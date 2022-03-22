options(java.parameters = '-Xmx20G')
library(r5r)
library(data.table)
library(dplyr)
library(sf)
library(mapview)

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
setnames(ttmatrix_bike, old = "fromId", new = "id")
ttmatrix_bike[, id := as.integer(id)]
ttmatrix_bike[cenario_points_coords, on = "id",
              c("osm_id") := i.osm_id]

# bring coordinates
ttmatrix_bike[points, on = c("toId" = "id"),
              c("lon", "lat") := list(i.lon, i.lat)]



# save
fwrite(ttmatrix_bike, "../../data/smtr_malha_cicloviaria/ttmatrix_iso_bike.csv")
ttmatrix_bike <- fread("../../data/smtr_malha_cicloviaria/ttmatrix_iso_bike.csv")



# osm_id1 <- "15807931"
# funcao para cada osm_id
iso_osm_id <- function(osm_id1) {
  
  
  # para cada pontinho de cada osm id
  # list all pontos to osm id
  points_osm_id <- ttmatrix_bike[osm_id == osm_id1]
  points_osm_id <- st_as_sf(points_osm_id, coords = c("lon", "lat"), crs = 4326)
  # mapview(points_osm_id)
  # st_as_sfc(st_bbox(points_osm_id)) %>% mapview() + points_osm_id
  # iso <- st_as_sfc(st_bbox(points_osm_id), crs = 4326)
  
  # construct polygon after these points
  
  # para cada pontinho, pegar os 3 mais longes?
  
  # variables <- 1
  iso_pontinho <- function(variables) {

    points_osm_id_pontinho <- points_osm_id %>% filter(id == variables)
    iso <- st_as_sfc(st_bbox(points_osm_id_pontinho), crs = 4326)
    # mapview(iso)
    # plot(iso)
    # iso_osm_pontinho <- sfheaders::sf_polygon(points_osm_id_pontinho, x = "lon", y= "lat")
    # mapview(iso_osm_pontinho)
    # 
    # 
    # library(stars)
    # iso_osm_pontinho_star <- stars::st_as_stars(iso_osm_pontinho)
    # st_contour(iso_osm_pontinho_star)
    # 
    # iso_osm_pontinho <- st_as_sf(points_osm_id_pontinho, coords = c("lon", "lat"), crs = 4326)
    # mapview(iso_osm_pontinho)
    # iso_osm_pontinho_hux <- st_convex_hull(iso_osm_pontinho)
    # mapview(iso_osm_pontinho_hux)

  }
  
  iso_todos <- lapply(points_osm_id$id, iso_pontinho)
  a <- do.call(c, iso_todos)
  a <- sf::st_union(a)
  b <- data.frame(osm_id = osm_id1)
  st_geometry(b) <- a
  
}


plot(b)

# open malha
cenario1 <- st_read("../../data/smtr_malha_cicloviaria/4-osm_cenarios/osm_cenario1.gpkg") %>% mutate(cenario = "cenario1")
# cenario <- cenario1


calcular_isco <- function(cenario) {
  
  # identificar quais pontos estao dentro dos cenarios
  cenario_buffer <- st_transform(cenario, crs = 31983)
  cenario_buffer <- st_buffer(cenario_buffer, dist = 30)
  cenario_buffer <- st_transform(cenario_buffer, crs = 4326)
  
  
  
}