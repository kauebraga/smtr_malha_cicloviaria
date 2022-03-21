options(java.parameters = '-Xmx10G')
library(r5r)

# create network
r5r::setup_r5("../../data/smtr_malha_cicloviaria/r5/graphs/rio")

# open malha

cenario1 <- st_read("../../data/smtr_malha_cicloviaria/4-osm_cenarios/osm_cenario1.gpkg") %>% mutate(cenario = "cenario1")

calcular_isco <- function(cenario) {
  
  
  
}