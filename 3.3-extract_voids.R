library(data.table)
library(sf)
library(dplyr)
library(mapview)
library(leaflet)
library(Hmisc)
library(kauetools)
# mapviewOptions(platform = "mapdeck")
mapdeck::set_token(fread("../../data/mapbox_key.csv")$key)
sf::sf_use_s2(FALSE)
mapviewOptions(fgb = FALSE)


# abrir rotas da OD agrupadas ---------------------------------------------
od_group_vias <- st_read("../../data/smtr_malha_cicloviaria/3.2-od_trechos/od_trechos.gpkg")




# comparar com a rede projetada ---------------------------------------------------------------

osm_bike_planejada <- st_read("../../data/smtr_malha_cicloviaria/3-malha_trechos/malha_planejada_trechos.gpkg")

# quais trechos estao na OD mas nao estao na rede planejada?
# osm_vazios_weekday_peak <- od_weekday_peak_group_vias %>% filter(osm_id %nin% osm_bike_planejada$osm_id)
# osm_vazios_weekday_offpeak <- od_weekday_offpeak_group_vias %>% filter(osm_id %nin% osm_bike_planejada$osm_id)
# osm_vazios_weekend <- od_weekend_group_vias %>% filter(osm_id %nin% osm_bike_planejada$osm_id)
osm_vazios <- od_group_vias %>% filter(osm_id %nin% osm_bike_planejada$osm_id)

# mapview(osm_vazios) + osm_bike_planejada

# salvar
file_path <- "../../data/smtr_malha_cicloviaria/3.3-vazios_trechos/od_vazios_trechos.gpkg"
file.remove(file_path)
st_write(osm_vazios, file_path)

# update on drive folder
googledrive::drive_ls(path = "SRTM - Infraestrutura cicloviaria")
googledrive::drive_put(media = file_path,
                       path = "SRTM - Infraestrutura cicloviaria/3.3-vazios_trechos",
                       name = basename(file_path))





