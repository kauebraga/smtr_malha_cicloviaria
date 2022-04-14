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
# od_weekday_peak_group_vias <-    st_read("../../data/smtr_malha_cicloviaria/3.2-osm_trips/osm_trips_weekday_peak.gpkg")
# od_weekday_offpeak_group_vias <- st_read("../../data/smtr_malha_cicloviaria/3.2-osm_trips/osm_trips_weekday_offpeak.gpkg")
# od_weekend_group_vias <-         st_read("../../data/smtr_malha_cicloviaria/3.2-osm_trips/osm_trips_weekend.gpkg")

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
file.remove("../../data/smtr_malha_cicloviaria/3.3-vazios_trechos/od_vazios_trechos.gpkg")
st_write(osm_vazios, "../../data/smtr_malha_cicloviaria/3.3-vazios_trechos/od_vazios_trechos.gpkg")


# file.remove("../../data/smtr_malha_cicloviaria/3.3-osm_vazios/osm_vazios_planejada_weekday_peak.gpkg")
# file.remove("../../data/smtr_malha_cicloviaria/3.3-osm_vazios/osm_vazios_planejada_weekday_offpeak.gpkg")
# file.remove("../../data/smtr_malha_cicloviaria/3.3-osm_vazios/osm_vazios_planejada_weekend.gpkg")
# st_write(osm_vazios_weekday_peak, 
#          "../../data/smtr_malha_cicloviaria/3.3-osm_vazios/osm_vazios_planejada_weekday_peak.gpkg",
#          append = FALSE)
# st_write(osm_vazios_weekday_offpeak, 
#          "../../data/smtr_malha_cicloviaria/3.3-osm_vazios/osm_vazios_planejada_weekday_offpeak.gpkg",
#          append = FALSE)
# st_write(osm_vazios_weekend, 
#          "../../data/smtr_malha_cicloviaria/3.3-osm_vazios/osm_vazios_planejada_weekend.gpkg",
#          append = FALSE)





