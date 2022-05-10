


library(ggplot2)
library(ggnewscale)
library(dplyr)
library(sf)

hex <- read_rds("../../data-raw/smtr_malha_cicloviaria/hex_agregado_rio_09.rds")
tiles <- read_rds("../../data-raw/smtr_malha_cicloviaria/maptile_crop_mapbox_rio_2019.rds")

maptile_ggplot <- geom_raster(data = tiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill()

# cenario <- "cenario1"


map_cenario <- function(cenario) {
  
  iso <- st_read(sprintf("../../data/smtr_malha_cicloviaria/5.0-isocronas/iso_%s_combine.gpkg", cenario))
  
  map <- ggplot()+
    geom_raster(data = tiles, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity()+
    # nova escala
    new_scale_fill()+
    # geom_sf(data = st_transform(hex, 3857), aes(fill = pop_total), color = NA) +
    # scale_fill_brewer(palette = "Blues")+
    geom_sf(data = st_transform(iso, 3857))+
    theme_void()+
    theme(legend.position = "bottom")

  # save
  ggsave(filename = sprintf("figures/maps/map_%s.png", cenario),
         plot = map,
         units = "cm", width = 16, height = 12)
  
  
  }
