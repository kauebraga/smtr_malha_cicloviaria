library(rdeck)
library(dplyr)
library(sf)
# Sys.setenv(MAPBOX_ACCESS_TOKEN = "sk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNqa2JoN3VodDMxa2YzcHFxMzM2YWw1bmYifQ.XAhHAgbe0LcDqKYyqKYIIQ")
Sys.setenv(MAPBOX_ACCESS_TOKEN = "pk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNqa2JoN3VodDMxa2YzcHFxMzM2YWw1bmYifQ.XAhHAgbe0LcDqKYyqKYIIQ")

st_geometry(hex) <- "polygon"



color1 <- col

rdeck(map_style = mapbox_light()) %>%
  # add_polygon_layer(data = hex,
  #                   get_fill_color = scale_color_threshold("P001", breaks = breaks_linear()))
  rdeck::add_mvt_layer(
    # group = "teste",
    id = "teste",
    # data = "https://c.tile.openstreetmap.org/{z}/{x}/{y}.png"
    # data = "https://api.mapbox.com/v4/kauebraga.hex_spo_pop/{z}/{x}/{y}.mvt",
    # data = "https://api.mapbox.com/v4/mapbox.satellite/{z}/{x}/{y}.mvt"
    # data = "https://a.tiles.mapbox.com/v4/mapbox.mapbox-terrain-dem-v1/{z}/{x}/{y}.mvt"
    data = "https://raw.githubusercontent.com/kauebraga/smtr_malha_cicloviaria/main/teste2/{z}/{x}/{y}.pbf",
    # data = "https://raw.githubusercontent.com/cornhundred/vector_tile_pbf_test/master/RhodesIslandDC/{z}/{x}/{y}.pbf",
    pickable = TRUE,
    position_format = "XY",
    opacity = 0.6,
    get_fill_color = scale_color_threshold("P001", breaks = breaks_linear()),
    tooltip = "P001"

  )
