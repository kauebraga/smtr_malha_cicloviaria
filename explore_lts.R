library(readr)
library(data.table)
library(ggplot2)


# seria interessante juntar todos esses trechos em um so!
od_group <- st_read("../../data/smtr_malha_cicloviaria/3.1-od_group/trips_group.gpkg")
# excluir pares OD com menos de 15 viagens - 1 viagens a cada dois dias
od_group <- od_group %>% filter(trips_total > 15)

ttmatrix_lts2 <- read_rds("../../data/smtr_malha_cicloviaria/1-ttmatrix_od/ttmatrix_detailed_rio_bike.rds")
ttmatrix_lts3 <- read_rds("../../data/smtr_malha_cicloviaria/1-ttmatrix_od/ttmatrix_detailed_rio_bike_lts3.rds")
  


# join
ttmatrix_lts2 <- left_join(ttmatrix_lts2, ttmatrix_lts3 %>% st_set_geometry(NULL) %>%select(fromId, toId, total_duration, distance),
                           by = c("fromId", "toId"))


ttmatrix_lts2 <- ttmatrix_lts2 %>% mutate(dif_dist = distance.x - distance.y,
                         dif_time = total_duration.x - total_duration.y)

# em teoria: dist x > dist y

# dif entre lts2 e lts3
# exemplo
teste_lts2 <- ttmatrix_lts2 %>% filter(fromId == "205 - Praça Barão de Ladário", 
                                       toId == "1 - Central do Brasil")
teste_lts3 <- ttmatrix_lts3 %>% filter(fromId == "205 - Praça Barão de Ladário", 
                                       toId == "1 - Central do Brasil")

mapview(teste_lts2, color = "red") + teste_lts3
# exemplo
teste_lts2 <- ttmatrix_lts2 %>% filter(fromId == "5 - Largo da Carioca", 
                                       toId == "1 - Central do Brasil")
teste_lts3 <- ttmatrix_lts3 %>% filter(fromId == "5 - Largo da Carioca", 
                                       toId == "1 - Central do Brasil")

mapview(teste_lts2, color = "red") + teste_lts3
# exemplo
from_teste <- "140 - Largo do Machado"
to_teste <- "144 - Casas Casadas"
teste_lts2 <- ttmatrix_lts2 %>% filter(fromId == from_teste, 
                                       toId == to_teste)
teste_lts3 <- ttmatrix_lts3 %>% filter(fromId == from_teste, 
                                       toId == to_teste)

mapview(teste_lts2, color = "red") + teste_lts3




# pegar somente os pares que tem viagem
ttmatrix_lts2_filter <- ttmatrix_lts2 %>%
  right_join(od_group %>% st_set_geometry(NULL) %>% 
              select(fromId = initial_station_name, toId = final_station_name, ttime_od, trips_total))


summary(ttmatrix_lts2_filter$total_duration.x)
summary(ttmatrix_lts2_filter$ttime_od)
# corelacao para LTS 2
summary(lm(ttime_od ~ total_duration.x, data = ttmatrix_lts2_filter))

summary(ttmatrix_lts2_filter$dif_dist)
summary(ttmatrix_lts2_filter$dif_time)


ggplot()+
  geom_boxplot(data = ttmatrix_lts2_filter, aes(x = 1, y = dif_dist))
  # geom_boxplot(data = ttmatrix_lts2_filter, aes(x = 1, y = dif_time))


ggplot()+
  geom_point(data = ttmatrix_lts2_filter, aes(x = total_duration.y, y = ttime_od))+
  geom_abline()


ttmatrix_lts2_filter <- ttmatrix_lts2_filter %>%
  mutate(dif_lts2 = total_duration.x - ttime_od) %>%
  mutate(dif_lts3 = total_duration.y - ttime_od)

ttmatrix_lts2_dif <- ttmatrix_lts2_filter %>%
  st_set_geometry(NULL) %>%
  select(fromId, toId, dif_lts2, dif_lts3) %>%
  tidyr::pivot_longer(cols = dif_lts2:dif_lts3, names_to = "var", values_to = "values")

ggplot()+
  geom_boxplot(data = ttmatrix_lts2_dif, aes(x = var, y = values))
# em geral: ttime(r5) < ttime(od)

summary(ttmatrix_lts2_filter$dif_lts2)  
summary(ttmatrix_lts2_filter$dif_lts3)  

