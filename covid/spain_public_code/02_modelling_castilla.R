rm(list=ls())
#========================================================================================================================
library(dplyr)
library(spdplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(INLA)
library(rstan)
library(rgdal)
library(sp)
library(geosphere)
setwd("D:\\01_PhD\\Spain")

options(dplyr.summarise.inform = FALSE)

#some basic functions
gm_mean = function(x){
  exp(mean(log(x)))
}
#=========================================================================================================================

castilla_cases_covariates <- read.csv("data//01March_08Jan_CYL_combined.csv") %>%
  dplyr::select(hzone_name, hzone_code, total_pop, lat, lon, starts_with("infected_day") ) %>%
  pivot_longer(
    cols = starts_with("infected_day"),
    names_to = "time",
    values_to = "cases",
    values_drop_na = TRUE
  ) %>% mutate(time = as.numeric(str_sub(time, start=14))) %>% 
  filter(time <= 92) %>% #restrict cases to before June 1, 95 is number of days between March 01 and May 31 (inclusive)
  mutate(hzone_code=ifelse(hzone_code %in% c(170813,170814), 170811, hzone_code )) %>% # codes 170813, 170814 are collapsed
  group_by(hzone_code) %>% 
  summarise(cases = sum(cases), lat=first(lat), lon=first(lon), hzone_name=first(hzone_name), total_pop=sum(unique(total_pop)) ) %>%
  arrange(hzone_code) %>% 
  ungroup() 

files <- list.files(path = "data\\castilla_mobility\\Castilla Y Leon",pattern=".csv")[1:92] #only take movement up until end of first wave

distance_matrix <- distm(x= cbind(castilla_cases_covariates$lon,castilla_cases_covariates$lat))/1000

min(distance_matrix[32,])

castilla_mobility_matrix <- matrix(0, nrow= 245, ncol = 245)
total_movement <- vector(length = length(files))

dates = seq.Date(from = as.Date("2020-03-01"), to = as.Date("2020-05-31"), by = "day")

# sum up mobility data
for (i in 1:length(files)){
  tmp <- read.csv(paste0("data\\castilla_mobility\\Castilla Y Leon\\", files[i]) )
  castilla_mobility_matrix <- castilla_mobility_matrix + tmp[,-1]
  total_movement[i] <- sum(tmp[,-1])
}

castilla_mobility_matrix_week1 <- matrix(0, nrow= 245, ncol =245)
total_movement_week1 <- vector(length = 7)

for (i in 1:7){
  tmp <- read.csv(paste0("data\\castilla_mobility\\Castilla Y Leon\\", files[i]) )
  castilla_mobility_matrix_week1 <- castilla_mobility_matrix_week1 + tmp[,-1]
  total_movement_week1[i] <- sum(tmp[,-1])
}

pdf(file = "manuscript//plots//castilla_total_move.pdf", height = 4, width = 5.5)
  par(oma=c(0,0,0,0), mar = c(2,4,0.5,0.5), cex = 1.2)
  plot(dates, total_movement/1000000, type = "l", ylab = "Daily Trips (millions)", xlab = "", xaxt = "n")
  axis(1, at = c(as.Date("2020-03-01"), as.Date("2020-04-01"), as.Date("2020-05-01"), as.Date("2020-06-01")), labels = c("March", "April", "May", "June"))
  abline(v = as.Date("2020-03-14"), col = "red", lty = 2)
  abline(v = as.Date("2020-05-11"), col = "blue", lty = 2)
  legend("topright",legend = c("lockdown start","lockdown lifted"), col=c("red","blue"), lty=c(2,2), bg = "white")
dev.off()

node_names <- castilla_cases_covariates %>% select(hzone_name,hzone_code, total_pop)
node_names$node_int = node_names$hzone_code %>% as.factor %>% as.numeric


#=======================================================================================================================

#castilla_cases_covariates$hzone_code[c(1:182,185:247)] == as.numeric(str_sub(colnames(castilla_mobility_matrix_week1), 2))# codes 170813

mobility_matrix <- castilla_mobility_matrix_week1

castilla_cases_covariates$within_movement <- diag(mobility_matrix %>% as.matrix) 

mobility_matrix = mobility_matrix - diag(diag(mobility_matrix %>% as.matrix))

mobility_matrix[mobility_matrix < 0.5] = 0 # if estimated number of trips is less then 0.5, 0 it out to improve matrix sparsity
mobility_matrix$from_hzone_code = castilla_cases_covariates$hzone_code

edgelist <- mobility_matrix %>% 
  pivot_longer(cols = starts_with("X"),
               names_to = "to_hzone_code",
               values_to = "trips") %>%
  mutate(to_hzone_code = as.numeric(str_sub(to_hzone_code, start = 2))) %>%
  filter(trips !=0) %>% rowwise() %>%
  mutate(smaller = min(to_hzone_code, from_hzone_code),
         larger = max(to_hzone_code, from_hzone_code)) %>%
  select(smaller,larger, trips) %>%
  group_by(smaller, larger) %>%
  summarise(trips = sum(trips)) %>% ungroup() %>% 
  left_join(node_names %>% select(-hzone_name), by = c("smaller"="hzone_code")) %>% 
  left_join(node_names %>% select(-hzone_name), by = c("larger"="hzone_code"))%>%
  select(node_int.x, node_int.y, trips) %>%
  rename(node1= node_int.x, node2 = node_int.y) %>%
  left_join(node_names %>% select(node_int, total_pop), by = c("node1" = "node_int"  )) %>%
  rename(pop1 = total_pop) %>% 
  left_join(node_names %>% select(node_int, total_pop), by = c("node2" = "node_int"  )) %>% 
  rename(pop2 = total_pop)

adjacency <- matrix(0, nrow = nrow(mobility_matrix), ncol=nrow(mobility_matrix))

el<- edgelist %>% select(node1,node2) %>% as.matrix() 

transformed_weights <- edgelist$trips/1000

for (i in 1: nrow(el)){
  adjacency[el[i,1], el[i,2]] <- transformed_weights[i]
  adjacency[el[i,2], el[i,1]] <- transformed_weights[i]
}

diagonal <- diag(rowSums(adjacency))
Q = diagonal - adjacency

n = dim(Q)[1]

Q.scaled = inla.scale.model(Q, constr = list(A= matrix(1,1,n), e=0))

dataForStan_BYM2_weighted <- list(N= nrow(mobility_matrix),
                                  N_edges = nrow(edgelist),
                                  node1 = edgelist$node1,
                                  node2 = edgelist$node2,
                                  y=castilla_cases_covariates$cases,
                                  E=castilla_cases_covariates$total_pop,
                                  weight = transformed_weights,
                                  s = gm_mean(diag(INLA:::inla.ginv(Q)))/gm_mean(diag(INLA:::inla.ginv(Q.scaled))),
                                  x = cbind((castilla_cases_covariates$within_movement/castilla_cases_covariates$total_pop - 
                                               mean(castilla_cases_covariates$within_movement/castilla_cases_covariates$total_pop))/
                                              sd(castilla_cases_covariates$within_movement/castilla_cases_covariates$total_pop)),
                                  K=1)

thin = 10
iter = thin * 300
chains = 4


stan_code <- rstan::stanc(file = "BYM2_slater.stan") # convert to C++ code
stan_model <- rstan::stan_model(stanc_ret = stan_code)     # compile generated code

castilla_movement_samples_week1 <- rstan::sampling( stan_model, 
                                             data = dataForStan_BYM2_weighted, 
                                             iter = iter,chains = chains,cores=chains,thin = thin, refresh =100 )


#=====================Physical BYM model===============================================================================================
muni <- readOGR("D:\\01_PhD\\Spain\\data\\castilla_mobility\\boundary") #fix this file path at some point?

muni <- muni %>% arrange(hzcode)

neighbor_mat <- spdep::poly2nb(muni, row.names = muni$hzcode)

output=NULL

for (i in 1:245){
  tmp <- matrix(ncol = 2, nrow = length(neighbor_mat[[i]]))
  tmp[,1] <- i
  tmp[,2] <- neighbor_mat[[i]]
  output <- rbind(output,tmp)
}

edges <- output %>% data.frame() %>%rowwise() %>% mutate(node1 = min(X1,X2), node2 = max(X1,X2)) %>% select(node1,node2) %>% ungroup() %>% 
  group_by(node1, node2) %>%
  filter(row_number() == 1) %>% ungroup() %>% 
  group_by(node1) %>% 
  mutate(num_neighbours = n())


###
el<- edges %>% select(-num_neighbours) %>% as.matrix()

el[113,] = c(32, 44) # Condado de Trevino is completely surrounded by some other AC, force an edge to its closest neighbours.

adjacency <- matrix(0, 245,245)

for (i in 1: nrow(el)){
  adjacency[el[i,1], el[i,2]] <- 1
  adjacency[el[i,2], el[i,1]] <- 1
}

diagonal <- diag(rowSums(adjacency))
Q = diagonal - adjacency


n = dim(Q)[1]

Q.scaled = inla.scale.model(Q, constr = list(A= matrix(1,1,n), e=0))


dataForStan_physical_BYM <- list(N= nrow(castilla_cases_covariates),
                                 N_edges = nrow(el),
                                 node1 = el[,1],
                                 node2 = el[,2],
                                 y=castilla_cases_covariates$cases,
                                 E=castilla_cases_covariates$total_pop,
                                 weight = rep(1,nrow(edges)), 
                                 s = gm_mean(diag(INLA:::inla.ginv(Q))),
                                 x = cbind((castilla_cases_covariates$within_movement/castilla_cases_covariates$total_pop - 
                                              mean(castilla_cases_covariates$within_movement/castilla_cases_covariates$total_pop))/
                                             sd(castilla_cases_covariates$within_movement/castilla_cases_covariates$total_pop)),
                                 K=1)

BYM_samples_physical <- rstan::sampling( stan_model, data = dataForStan_physical_BYM, iter = iter,
                                         chains = chains, cores=chains, thin = thin, refresh =100, control = list(max_treedepth = 15) )

save(castilla_movement_samples_week1,  BYM_samples_physical, file = paste0("castilla_1spatial", Sys.Date(),".RData") )


#========================================================================================================================================

dataForStan_2spatial <- list(N= nrow(castilla_cases_covariates),
                             N_edges_movement = nrow(edgelist),
                             N_edges_physical = nrow(edges),
                             node1_movement = edgelist$node1,
                             node2_movement = edgelist$node2,
                             node1_physical = el[,1],
                             node2_physical = el[,2],
                             y=castilla_cases_covariates$cases,
                             E=castilla_cases_covariates$total_pop,
                             weight_movement = transformed_weights , 
                             weight_physical = rep(1, nrow(edges)),
                             s = c(dataForStan_BYM2_weighted$s, dataForStan_physical_BYM$s), #movement then physical
                             x = cbind((castilla_cases_covariates$within_movement/castilla_cases_covariates$total_pop - 
                                          mean(castilla_cases_covariates$within_movement/castilla_cases_covariates$total_pop))/
                                         sd(castilla_cases_covariates$within_movement/castilla_cases_covariates$total_pop)),
                             K=1)


stan_code_2spatial <- rstan::stanc(file = "BYM2_2spatialeffects.stan") # convert to C++ code
stan_model_2spatial <- rstan::stan_model(stanc_ret = stan_code_2spatial)     # compile generated code

castilla_2spatial <- rstan::sampling( stan_model_2spatial, data = dataForStan_2spatial, 
                                    iter = iter,chains = chains, cores = chains, thin = thin, refresh =100 )

save(castilla_movement_samples_week1, BYM_samples_physical, castilla_2spatial, 
     file = paste0("results\\","castilla_samples_identity",Sys.Date(),".RData"))
#load("castilla_samples2021-01-28.RData")

rho_2spatial <- extract(castilla_2spatial)["rho"]$rho %>% data.frame() %>% 
  rename(Movement= X1, Neighbour=X2, NonStructured=X3)%>% 
  pivot_longer(cols = c("Movement","Neighbour","NonStructured"))

rho_2spatial %>% ggplot(aes(x=value, fill=name))+
  geom_density(alpha=0.5)+
  theme_bw()+
  labs(x= "rho", title= "Proportion of variance explained by the 3 spatial effects", fill = NULL)
