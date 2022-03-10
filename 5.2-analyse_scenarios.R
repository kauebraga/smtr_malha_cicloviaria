library(data.table)
library(sf)
library(dplyr)
library(mapview)
library(leaflet)
library(kauetools)
library(Hmisc)
sf::sf_use_s2(FALSE)
mapviewOptions(fgb = FALSE)
mapviewOptions(platform = "mapdeck")
mapdeck::set_token(fread("../../data/mapbox_key.csv")$key)


# open cenarios
cenario1 <- kauetools::read_data("osm_cenarios_final/osm_cenario1_final.gpkg") %>% mutate(cenario = "cenario1")
cenario2 <- kauetools::read_data("osm_cenarios_final/osm_cenario2_final.gpkg")  %>% mutate(cenario = "cenario2")
cenario3 <- kauetools::read_data("osm_cenarios_final/osm_cenario3_final.gpkg")  %>% mutate(cenario = "cenario3")

# extract voids
cenario_voids <- cenario3 %>% filter(osm_id %nin% cenario2$os)





# quais das infraestrutura previstas no cenario 3 podem ter mais impacto nas variaveis e no total de viagens?