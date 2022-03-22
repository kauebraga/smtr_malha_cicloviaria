library(readr)
library(dplyr)
library(opentripplanner)
library(data.table)
library(sf)
library(purrr)
library(mapview)

# open scenario
cenario1 <- st_read("../../data/smtr_malha_cicloviaria/4-osm_cenarios/osm_cenario1.gpkg") %>% mutate(cenario = "cenario1")

# break scenario in points every 20 meters
# standardize shape resolution - at least every 20 meters
cenario_points <- st_segmentize(cenario1, dfMaxLength = 100)
# shapes_linhas_filter <- sfheaders::sf_cast(shapes_linhas_filter, "POINT")
# transform to lon lat - these points will be the origins
cenario_points_coords <- sfheaders::sf_to_df(cenario_points, fill = TRUE) %>% 
  mutate(id = 1:n()) %>%
  select(id, osm_id, name, highway, fase, cenario, lon = x, lat = y) %>%
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
          port = 8080, wait = FALSE)

# register the router
otp_rio <- otp_connect(router = "rio")
# otp_rio <- list(hostname = "localhost", router = "rio", url = NULL, 
#                port = "8080", ssl = FALSE, otp_version = "1.5.0")
# class(otp_rio) <- "otpconnect"

# apply function
a <- opentripplanner::otp_isochrone(otpcon = otp_rio,
                                    fromPlace = c(-43.67406, -22.8927),
                                    mode = "WALK",
                                    # dist = c(1000, 2000, 3000),
                                    cutoffSec = 300)
# 
# mapview::mapview(a)

coords_list <- purrr::map2(as.numeric(cenario_points_coords$lon), as.numeric(cenario_points_coords$lat), c)
coords_list <- matrix(c(cenario_points_coords$lon, cenario_points_coords$lat), ncol = 2)

my_iso <- function(coords, id = NULL, time, mode1, connection) {
  
  otp_isochrone(otpcon = connection,
                fromPlace = coords,
                fromID = id,
                cutoffSec = time,
                mode = mode1
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

readr::write_rds(a, "iso_otp_cenario1.rds")


# trazer osm_id
a1<- a %>%
  # st_cast("POLYGON") %>%
  select(-id, -time) %>%
  rename(id = fromPlace) %>%
  left_join(cenario_points_coords %>% select(id, osm_id))

a1 <- a1 %>% st_make_valid()

# oi <- a1 %>% group_by(osm_id) %>% summarise()
oi <- aggregate(a1, by = list(a1$osm_id), FUN = mean)

