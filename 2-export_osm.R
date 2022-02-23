library(data.table)
library(sf)
library(dplyr)
library(osmextract)

# trazer rede do OSM --------------------------------------------------------------------------

osm_rio <- oe_get("Rio de Janeiro", stringsAsFactors = FALSE, quiet = FALSE)

# filter only recife
# download recife shape
shape_rio <- geobr::read_municipality(3304557) %>% st_transform(4326)
osm_rio_go <- st_join(osm_rio, shape_rio)
osm_rio_go <- osm_rio_go %>% filter(!is.na(code_muni))
# mapview(osm_recife_go) + shape_rec

# select only highways (vias)
table(osm_rio$highway, useNA = 'always')
osm_rio_vias <- osm_rio_go %>%
  filter(!is.na(highway))
# mapview(osm_recife_vias) + shape_rec

readr::write_rds(osm_rio_vias, "../../data/smtr_malha_cicloviaria/osm_rio.rds")


