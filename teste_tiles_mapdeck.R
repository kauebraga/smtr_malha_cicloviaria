library(mapdeck)
library(data.table)
mapdeck::set_token(fread("../../data/mapbox_key.csv")$key)
library(mapboxapi)
library(mapdeck)
library(httr)
mapboxapi::mb_access_token("sk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNqa2JoN3VodDMxa2YzcHFxMzM2YWw1bmYifQ.XAhHAgbe0LcDqKYyqKYIIQ", install = TRUE)


# Get the Microsoft buildings data for Texas and unzip
GET("https://usbuildingdata.blob.core.windows.net/usbuildings-v1-1/Texas.zip",
    write_disk("Texas.zip", overwrite = TRUE), progress())

unzip("Texas.zip")

# teste para aop
hex <- aopdata::read_population("spo", geometry = TRUE)
# ui <- sf::st_read("RhodeIsland.geojson")

sf::st_write(dplyr::select(hex, P001), "hex.geojson")

# straight to pbf
system("tippecanoe -e teste2 --no-tile-compression hex.geojson")
# system("tippecanoe -e teste2 --drop-densest-as-needed --extend-zooms-if-still-dropping --no-tile-compression hex.geojson")

# https://observablehq.com/@cornhundred/deck-gl-mvt-layer-custom


# nao precisa mais disso ---------------
# Use tippecanoe to make a dynamic .mbtiles file that visualizes large data appropriately
# at any zoom level.  sf objects can also be used as input!
# (requires installing tippecanoe on your machine separately first)
tippecanoe(input = hex %>% dplyr::select(P001),
           output = "hex_pop001.mbtiles",
           layer_name = "hex_spo_pop",
           other_options = "--no-tile-compression")

system("ogr2ogr -f MVT ./teste1 hex_pop001.mbtiles -dsco COMPRESS=FALSE -dsco TILING_SCHEME=EPSG:4326,-180,180,360")

# Upload the generated tileset to your Mapbox account (requires a Mapbox secret access token
# to be set as an environment variable)
upload_tiles(input = "hex_pop001.mbtiles", 
             username = "kauebraga",
             tileset_id = "hex_spo_pop001",
             tileset_name = "hex_spo_pop001",
             access_token = "sk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNsMWIwczl5ajJueHEzaXB3bTNiemx0enEifQ.Ji-C9-SW6R4Worzt8OVnJg",
             multipart = TRUE)



mapdeck(
        style = "mapbox://styles/kauebraga/cl1b02lii000s14qui20q7jcs/draft",
        zoom = 6,
        location = c(-98.7382803, 31.7678448))
