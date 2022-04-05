# install.packages('leaflet')
# install.packages('mapboxapi')
# install.packages('aopdata')

library(leaflet)
library(mapboxapi)
# https://rpubs.com/walkerke/rstudio-mapbox

# load mapbox token
mapboxapi::mb_access_token(token = "your_mapbox_token")

c <- aopdata::read_population("spo", geometry = TRUE)

pal <- colorNumeric(
  palette = "Oranges",
  domain = c(min(c$P001):max(c$P001))
)



leaflet() %>%
  addTiles(group = "title1") %>%
  addMapboxTiles(style_id = "cl1b8ovgb001b14nuzqlt28sz",
                 username = "kauebraga",
                 group = "Population tile") %>%
  addLayersControl(overlayGroups = c("Population tile"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright", 
            pal = pal, values = c$P001,
            title = "Population",
            group = "Population tile",
            opacity = 1
  )
