library(mapboxer)
Sys.setenv(MAPBOX_API_TOKEN = "pk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNqa2JoN3VodDMxa2YzcHFxMzM2YWw1bmYifQ.XAhHAgbe0LcDqKYyqKYIIQ")
Sys.setenv(MAPBOX_ACCESS_TOKEN = "pk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNqa2JoN3VodDMxa2YzcHFxMzM2YWw1bmYifQ.XAhHAgbe0LcDqKYyqKYIIQ")


a <- mapbox_source(
  type = "vector",
  url = "mapbox://kauebraga.hex_spo_pop"
)

b <- mapbox_source(
  type = "vector",
  url = "mapbox://mapbox.mapbox-terrain-v2"
)

layer <- list(
  "id" = "hex_spo_pop1",
  "type" = "fill",
  "source" = a,
  "source-layer" = "hex_spo_pop"
  # "paint" = list("fill-color" = list("property" = "P001",
                                   # "stops" = "[[3000, '#fff'], [7000, '#f00']])"))
  
)

layer <- list(
  id = "terrain-data",
  type = "line",
  source = b
)

map <- mapboxer(
  # source = a,
  # center = c(-75.789, 41.874),
  zoom = 5,
  style = basemaps$Mapbox$streets_v11,
  # element_id = "hex_spo_pop1"
) %>%
  # add_fill_layer(fill_color = "black")
  add_layer(layer
            ) %>%
  add_scale_control(layer_id = "hex_spo_pop1")

map
