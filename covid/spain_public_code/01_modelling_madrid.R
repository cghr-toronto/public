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

madrid_cases_covariates <- read.csv("data//26Feb_05Jan_Madrid_Muni_Combined.csv") %>%
  dplyr::select(MunicipalityName, MunicipalityCode,TotalPopulation, Lat, Lon, starts_with("day") ) %>%
  pivot_longer(
    cols = starts_with("day"),
    names_to = "time",
    values_to = "cases",
    values_drop_na = TRUE
  ) %>% mutate(time = as.numeric(str_sub(time, start=5))) %>% 
  filter(time <= 95) %>% #restrict cases to before June 1, 95 is number of days between Feb 26 and May 31 (inclusive), 2020 was a leap year.
  group_by(MunicipalityName,TotalPopulation, MunicipalityCode, Lat, Lon) %>% 
  summarise(cases = sum(cases)) %>%
  arrange(MunicipalityCode) %>% 
  ungroup()

distance_matrix <- distm(x= cbind(madrid_cases_covariates$Lon,madrid_cases_covariates$Lat))/1000

files <- list.files(path = "data\\madrid_mobility\\Madrid",pattern=".csv")[1:92] #only take movement up until end of first wave

madrid_mobility_matrix <- matrix(0, nrow= 179, ncol =179)
total_movement <- vector(length = length(files))

dates = seq.Date(from = as.Date("2020-03-01"), to = as.Date("2020-05-31"), by = "day")

# sum up mobility data
for (i in 1:length(files)){
  tmp <- read.csv(paste0("data\\madrid_mobility\\Madrid\\", files[i]) )
  madrid_mobility_matrix <- madrid_mobility_matrix + tmp[,-1]
  total_movement[i] <- sum(tmp[,-1])
}

pdf(file = "manuscript//plots//madrid_total_move.pdf", height = 4, width = 5.5)
  par(oma=c(0,0,0,0), mar = c(2,4,0.5,0.5), cex=1.2)
  plot(dates,total_movement/1000000, type = "l", ylab = "Daily Trips (millions)", xlab = "", xaxt = "n")
  axis(1, at = c(as.Date("2020-03-01"), as.Date("2020-04-01"), as.Date("2020-05-01"), as.Date("2020-06-01")), labels = c("March", "April", "May", "June"))
  abline(v = as.Date("2020-03-14"), col = "red", lty = 2)
  abline(v = as.Date("2020-05-11"), col = "blue", lty =2)
  legend("topright",legend = c("lockdown start","lockdown lifted"), col=c("red","blue"), lty=c(2,2), bg = "white")
dev.off()




madrid_mobility_matrix_week1 <- matrix(0, nrow= 179, ncol =179)
total_movement_week1 <- vector(length = 7)

for (i in 1:7){
  tmp <- read.csv(paste0("data\\madrid_mobility\\Madrid\\", files[i]) )
  madrid_mobility_matrix_week1 <- madrid_mobility_matrix_week1 + tmp[,-1]
  total_movement_week1[i] <- sum(tmp[,-1])
}

#make a reference dataset that contains names, codes, ands ints for each region. Can reference this going forward
node_names <- madrid_cases_covariates %>% select(MunicipalityName,MunicipalityCode, TotalPopulation)
node_names$node_int = node_names$MunicipalityCode %>% as.factor %>% as.numeric


setupStanData <- function(mobility_matrix) {

  madrid_cases_covariates$within_movement <- diag(mobility_matrix %>% as.matrix) 
  
  mobility_matrix = mobility_matrix - diag(diag(mobility_matrix %>% as.matrix))
  
  mobility_matrix[mobility_matrix < 0.5] = 0 # if estimated number of trips is less then 0.5, 0 it out to improve matrix sparsity
  mobility_matrix$from_MunicipalityCode = madrid_cases_covariates$MunicipalityCode
  
  edgelist <- mobility_matrix %>% 
    pivot_longer(cols = starts_with("X"),
                 names_to = "to_MunicipalityCode",
                 values_to = "trips") %>%
    mutate(to_MunicipalityCode = as.numeric(str_sub(to_MunicipalityCode, start = 2))) %>%
    filter(trips !=0) %>% rowwise() %>%
    mutate(smaller = min(to_MunicipalityCode, from_MunicipalityCode),
           larger = max(to_MunicipalityCode, from_MunicipalityCode)) %>%
    select(smaller,larger, trips) %>%
    group_by(smaller, larger) %>%
    summarise(trips = sum(trips)) %>% ungroup() %>% 
    left_join(node_names %>% select(-MunicipalityName), by = c("smaller"="MunicipalityCode")) %>% 
    left_join(node_names %>% select(-MunicipalityName), by = c("larger"="MunicipalityCode"))%>%
    select(node_int.x, node_int.y, trips) %>%
    rename(node1= node_int.x, node2 = node_int.y) %>%
    left_join(node_names %>% select(node_int, TotalPopulation), by = c("node1" = "node_int"  )) %>%
    rename(pop1 = TotalPopulation) %>% 
    left_join(node_names %>% select(node_int, TotalPopulation), by = c("node2" = "node_int"  )) %>% 
    rename(pop2 = TotalPopulation)



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
                                    y=madrid_cases_covariates$cases,
                                    E=madrid_cases_covariates$TotalPopulation,
                                    weight = transformed_weights,
                                    s = gm_mean(diag(INLA:::inla.ginv(Q)))/gm_mean(diag(INLA:::inla.ginv(Q.scaled))),
                                    x = cbind((madrid_cases_covariates$within_movement/madrid_cases_covariates$TotalPopulation - 
                                                 mean(madrid_cases_covariates$within_movement/madrid_cases_covariates$TotalPopulation))/
                                                sd(madrid_cases_covariates$within_movement/madrid_cases_covariates$TotalPopulation)),
                                    K=1)

  
  return(list(data = dataForStan_BYM2_weighted,
              Q=Q,
              edgelist=edgelist,
              check_move=madrid_cases_covariates))
}

thin = 10
iter = thin * 200
chains = 4

stan_code <- rstan::stanc(file = "BYM2_slater.stan") # convert to C++ code
stan_model <- rstan::stan_model(stanc_ret = stan_code)     # compile generated code
 
madrid_movement_samples_week1 <- rstan::sampling( stan_model, 
                                             data = setupStanData(mobility_matrix=madrid_mobility_matrix_week1)$data, 
                                             iter = iter,chains = chains,cores=chains,thin = thin, refresh =100 )
 
# madrid_movement_samples_all <- rstan::sampling( stan_model, 
#                                            data = setupStanData(mobility_matrix=madrid_mobility_matrix)$data, 
#                                             iter = iter,chains = chains, cores=chains, thin = thin, refresh =100 )

#===============================================================================================================================================


#=====================Physical BYM model===============================================================================================
muni <- readOGR("D:\\01_PhD\\Covid\\Spain Regional\\spain_20201025\\spain_spatial\\madrid\\Shape File-Madrid-Municipalities") #fix this file path at some point?

muni$natcode[which(muni$nameunit=="Madrid")] <- "34132828000" #Match spatial units to Jorge's coding

muni <- muni %>% arrange(natcode)  

neighbor_mat <- spdep::poly2nb(muni, row.names = muni$natcode)

output=NULL

for (i in 1:179){
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

adjacency <- matrix(0, 179,179)

for (i in 1: nrow(el)){
  adjacency[el[i,1], el[i,2]] <- 1
  adjacency[el[i,2], el[i,1]] <- 1
}

diagonal <- diag(rowSums(adjacency))
Q = diagonal - adjacency


n = dim(Q)[1]

Q.scaled = inla.scale.model(Q, constr = list(A= matrix(1,1,n), e=0))

x = diag(madrid_mobility_matrix %>% as.matrix) 


dataForStan_physical_BYM <- list(N= nrow(madrid_cases_covariates),
                                 N_edges = nrow(edges),
                                 node1 = edges$node1,
                                 node2 = edges$node2,
                                 y=madrid_cases_covariates$cases,
                                 E=madrid_cases_covariates$TotalPopulation,
                                 weight = rep(1,nrow(edges)), 
                                 s = gm_mean(diag(INLA:::inla.ginv(Q))),
                                 x = cbind((x/madrid_cases_covariates$TotalPopulation - 
                                              mean(x/madrid_cases_covariates$TotalPopulation))/
                                             sd(x/madrid_cases_covariates$TotalPopulation)),
                                 K=1)

BYM_samples_physical <- rstan::sampling( stan_model, data = dataForStan_physical_BYM, iter = iter,
                                          chains = chains, cores=chains, thin = thin, refresh =100, control = list(max_treedepth = 15) )

#save(madrid_movement_samples_week1,  BYM_samples_physical, file = paste0("madrid_1spatial", Sys.Date(),".RData") )
     
#load("movement_samples_2021-01-20.RData") # used log(trips) as weights

#load("movement_samples_2021-01-22.RData") # used per population as weights
#========================================= Results ============================================================================================

rho_movement_week1 = extract(madrid_movement_samples_week1)["rho"]
rho_movement_all = extract(madrid_movement_samples_all)["rho"]
rho_physical = extract(BYM_samples_physical)["rho"]

sigma_movement_week1 = extract(madrid_movement_samples_week1)["sigma"]
sigma_movement_all = extract(madrid_movement_samples_all)["sigma"]
sigma_physical = extract(BYM_samples_physical)["sigma"]

betas_movement_week1 = extract(madrid_movement_samples_week1)["betas"]
betas_movement_all = extract(madrid_movement_samples_all)["betas"]
betas_physical = extract(BYM_samples_physical)["betas"]

colors <- c("Week1 movement" = "red", "All movement" = "blue", "Physical neighbours" = "green")

gm_mean(apply(extract(madrid_movement_samples_week1)["phi"]$phi, FUN=sd, MARGIN = 2)^2/0.002)
  
gm_mean(apply(extract(BYM_samples_physical)["phi"]$phi, FUN=sd, MARGIN = 2)^2)
  
  
apply(extract(madrid_movement_samples_week1)["theta"]$theta, FUN=sd, MARGIN = 2)

cbind(rho_movement_week1$rho, rho_physical$rho) %>% data.frame() %>% 
  ggplot()+
  geom_density(aes(x=X1, col="Week1 movement"))+
  #geom_density(aes(x=X2, col="All movement"))+
  geom_density(aes(x=X2, col="Physical neighbours"))+
  theme_bw()+
  labs(x = "rho", title = "Posterior density of rho in BYM2 models", legend = NULL)+
  scale_color_manual(values = colors)


cbind(sigma_movement_week1$sigma,sigma_movement_all$sigma,sigma_physical$sigma) %>% data.frame() %>% 
  ggplot() +
  geom_density(aes(x=X1, col="Week1 movement"))+
  geom_density(aes(x=X2, col="All movement"))+
  geom_density(aes(x=X3, col="Physical neighbours"))+
  theme_bw()+
  labs(x="sigma", title = "posterior density of sigma for the 3 models", legend = NULL)+
  scale_color_manual(values = colors)

cbind(betas_movement_week1$betas, betas_movement_all$betas, betas_physical$betas) %>% data.frame() %>% 
  ggplot()+
  geom_density(aes(x=X1, col="Week1 movement"))+
  geom_density(aes(x=X2, col="All movement"))+
  geom_density(aes(x=X3, col="Physical neighbours"))+
  scale_color_manual(values = colors)+
  labs(legend=NULL, title = "Posterior density of Beta - the within-region movement parameter")+
  labs(colour= NULL)+
  theme_bw()

#===========================================2 parameter model===============================================

dataForStan_2spatial <- list(N= nrow(madrid_cases_covariates),
                             N_edges_movement = nrow(setupStanData(mobility_matrix=madrid_mobility_matrix_week1)$edgelist),
                             N_edges_physical = nrow(edges),
                             node1_movement = setupStanData(mobility_matrix=madrid_mobility_matrix_week1)$edgelist$node1,
                             node2_movement = setupStanData(mobility_matrix=madrid_mobility_matrix_week1)$edgelist$node2,
                             node1_physical = edges$node1,
                             node2_physical = edges$node2,
                             y=madrid_cases_covariates$cases,
                             E=madrid_cases_covariates$TotalPopulation,
                             weight_movement = setupStanData(mobility_matrix=madrid_mobility_matrix_week1)$data$weight , 
                             weight_physical = rep(1,nrow(edges)),
                             s = c(setupStanData(mobility_matrix=madrid_mobility_matrix_week1)$data$s, 
                                        gm_mean(diag(INLA:::inla.ginv(Q)))), #movement then physical
                             x = cbind((x/madrid_cases_covariates$TotalPopulation - 
                                          mean(x/madrid_cases_covariates$TotalPopulation))/
                                         sd(x/madrid_cases_covariates$TotalPopulation)),
                             K=1)


stan_code_2spatial <- rstan::stanc(file = "BYM2_2spatialeffects.stan") # convert to C++ code
stan_model_2spatial <- rstan::stan_model(stanc_ret = stan_code_2spatial)     # compile generated code

madrid_2spatial <- rstan::sampling( stan_model_2spatial, data = dataForStan_2spatial, 
                                                iter = iter,chains = chains, cores = chains, thin = thin, refresh =100 )

#save(madrid_2spatial, file = paste0("results//madrid_2spatial_identity", Sys.Date(),".RData") )

load("results//samples_2spatial_2021-01-26.RData")

traceplot(madrid_2spatial, pars = "rho")

rho_2spatial <- extract(madrid_2spatial)["rho"]$rho %>% data.frame() %>% 
  rename(Movement= X1, Neighbour=X2, NonStructured=X3)%>% 
  pivot_longer(cols = c("Movement","Neighbour","NonStructured"))

sigma_2spatial<- extract(madrid_2spatial)["sigma"]$sigma %>% data.frame()

phi_physical <- extract(madrid_2spatial)["phi_physical"]$phi_physical



rho_2spatial %>% ggplot(aes(x=value, fill=name))+
  geom_density(alpha=0.5)+
  theme_bw()+
  labs(x= "rho", title= "Proportion of variance explained by the 3 spatial effects", fill = NULL)
  
ggplot(data=sigma_2spatial, aes(x=.))+
  geom_density()

#=========== check to see if scaling worked ===========================================================================================


gm_mean(apply(extract(madrid_2spatial)["phi_movement"]$phi_movement/ sqrt(dataForStan_2spatial$s[1]), FUN = var, MARGIN = 2))
gm_mean(apply(extract(madrid_2spatial)["phi_physical"]$phi_physical/ sqrt(dataForStan_2spatial$s[2]), FUN = var, MARGIN = 2))
gm_mean(apply(extract(madrid_2spatial)["theta"]$theta, FUN = var, MARGIN = 2))

apply( extract(madrid_2spatial)["phi_movement"]$phi_movement/ sqrt(dataForStan_2spatial$s[1]), FUN = quantile,  MARGIN  = 2, probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  data.frame(rep(1:179)) %>% 
  ggplot(aes(x=rep.1.179., y=X50.))+
  geom_point()+
  geom_errorbar(aes(min=X2.5.,max=X97.5.))+
  labs(x="region_index", y = "phi/sqrt(s)", title= "Prior check: Movement data")


apply( extract(madrid_2spatial)["phi_physical"]$phi_physical/ sqrt(dataForStan_2spatial$s[2]), FUN = quantile,  MARGIN  = 2, probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  data.frame(rep(1:179)) %>% 
  ggplot(aes(x=rep.1.179., y=X50.))+
  geom_point()+
  geom_errorbar(aes(min=X2.5.,max=X97.5.))+
  labs(x="region index", y = "phi/sqrt(s)", title="Prior check: Physical")

apply( extract(madrid_2spatial)["theta"]$theta, FUN = quantile,  MARGIN  = 2, probs = c(0.025,0.5,0.975)) %>% 
  t() %>% 
  data.frame(rep(1:179)) %>% 
  ggplot(aes(x=rep.1.179., y=X50.))+
  geom_point()+
  geom_errorbar(aes(min=X2.5.,max=X97.5.))+
  labs(x="region index", y = "theta", title="Prior check: theta")

