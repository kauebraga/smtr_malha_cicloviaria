library(osmdata)
library(dodgr)
library(data.table)
library(sf)
library(dplyr)
library(mapview)

cenario1 <- st_read("../../data/smtr_malha_cicloviaria/4-osm_cenarios/osm_cenario1.gpkg") %>% mutate(cenario = "cenario1")


dat <- opq ("rio de janeiro") %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sc()


# readr::write_rds(dat, "osmdata_sc_rio.rds")
dat <- readr::read_rds("osmdata_sc_rio.rds")

graph <- weight_streetnet (dat, wt_profile = "foot")
setDT(graph)

# extract nodes that are in osm_id
nodes_ok <- graph[object_ %in% cenario1$osm_id]

from <- sample (nodes_ok$.vx0, size = 1000)
tlim <- c (5) * 60 # times in seconds
# tlim <- c (5, 10, 20, 30, 60) * 60 # times in seconds
RcppParallel::setThreadOptions (numThreads = 6)
x <- dodgr_isochrones (graph, from = from, tlim)
setDT(x)

# trazer o osm_id
x[graph, on = c("from" = ".vx0"),
  c("osm_id") := i.object_]


# x1 <- x[from == 7842398724]
x[, N := .N, by = from]
x1 <- x[N >= 4]


a1 <- x1 %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  group_by(from) %>%
  summarise() %>%
  st_cast("POLYGON")

mapview::mapview(a1)



# teste -------------------------------------------------------------------


a1 <- cenario1 %>% filter(osm_id == 630711361) %>% select(osm_id)
a2 <- graph[object_ == 630711361]
a2 <- a2 %>% st_as_sf(coords = c(".vx0_x", ".vx0_y"), crs = 4326)

mapview(a1) + a2
