# calcular a quantidade de viagens de cada arquivo OD

library(data.table)
library(dplyr)


diagnosys_od <- function(od_file) {
  
  # abrir arquivo
  od <- fread(od_file) %>% nrow()
  
  fim <- data.frame(file = basename(od_file),
                    trips = od)
  
}

files <- dir("../../data-raw/smtr_malha_cicloviaria/bike_trips", pattern = ".csv$", full.names = TRUE)
trips <- lapply(files, diagnosys_od) %>% rbindlist()
