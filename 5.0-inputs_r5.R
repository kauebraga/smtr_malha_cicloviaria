options(java.parameters = '-Xmx10G')
library(r5r)
library(h3jsr)
library(dplyr)
library(sf)

sfc_as_cols <- function(x, names = c("lon","lat")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  ui <- dplyr::bind_cols(x,ret)
  st_set_geometry(ui, NULL)
}

# create network
r5r::setup_r5("../../data/smtr_malha_cicloviaria/r5/graphs/rio")


# setup points
muni <- geobr::read_municipality(3304557)
# get the unique h3 ids of the hexagons intersecting your polygon at a given resolution
hex_ids <- polyfill(muni, res = 10, simple = TRUE)
# pass the h3 ids to return the hexagonal grid
hex_grid <- hex_ids %>%  
  h3_to_polygon(simple = FALSE) %>%
  rename(id_hex = h3_address) %>%
  st_centroid() %>%
  sfc_as_cols()

# save
hex_grid %>%
  select(id = id_hex, lon, lat) %>%
  data.table::fwrite("../../data/smtr_malha_cicloviaria/r5/graphs/rio/points.csv")

