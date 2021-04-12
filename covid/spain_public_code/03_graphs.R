rm(list=ls())

library(dplyr)
library(spdplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(INLA)
library(rstan)
library(rgdal)
library(sp)
library(sf)
library(geosphere)
library(mapview)
library(spatialEco)
library(xtable)
library(rgdal)
library(mapmisc)

#=======================================2spatial plots======================================================
load("results//madrid_2spatial_identity2021-02-02.RData")
rho_2spatial_madrid <- rstan::extract(madrid_2spatial)["rho"]$rho %>% 
  cbind.data.frame(rstan::extract(madrid_2spatial)["beta0"]$beta0,
                   rstan::extract(madrid_2spatial)["betas"]$betas,
                   rstan::extract(madrid_2spatial)["sigma"]$sigma) %>% data.frame() %>% 
  rename(Movement= X1, Neighbour=X2, NonStructured=X3, 
         mu = rstan..extract.madrid_2spatial...beta0...beta0, 
         beta = rstan..extract.madrid_2spatial...betas...betas,
         sigma = rstan..extract.madrid_2spatial...sigma...sigma) %>% 
  pivot_longer(cols = c("Movement","Neighbour","NonStructured", "mu", "beta", "sigma"))

pdf(file = "manuscript//plots//madrid_2spatial_rho.pdf", height = 4, width = 5.5)
rho_2spatial_madrid %>% filter(name %in% c("Movement", "Neighbour", "NonStructured")) %>% 
  ggplot(aes(x=value, fill=name))+
  geom_density(alpha=0.5)+
  theme_bw()+
  labs(x= "rho", fill = NULL)+
  theme(legend.position = c(0.75,0.75))
dev.off()

pdf(file = "manuscript//plots//madrid_2spatial_traceplot_rho.pdf", height = 4, width = 6)
  traceplot(madrid_2spatial, pars = c("rho"))
dev.off()


load("results//castilla_samples_identity2021-03-17.RData")
rho_2spatial_castilla <- rstan::extract(castilla_2spatial)["rho"]$rho %>% 
  cbind.data.frame(rstan::extract(castilla_2spatial)["beta0"]$beta0,
                   rstan::extract(castilla_2spatial)["betas"]$betas,
                   rstan::extract(castilla_2spatial)["sigma"]$sigma) %>% data.frame() %>% 
  rename(Movement= X1, Neighbour=X2, NonStructured=X3, 
         mu = rstan..extract.castilla_2spatial...beta0...beta0, 
         beta = rstan..extract.castilla_2spatial...betas...betas,
         sigma = rstan..extract.castilla_2spatial...sigma...sigma) %>% 
  pivot_longer(cols = c("Movement","Neighbour","NonStructured", "mu", "beta", "sigma"))

pdf(file = "manuscript//plots//castilla_2spatial_rho.pdf", height = 4, width = 5.5)
rho_2spatial_castilla %>% filter(name %in% c("Movement", "Neighbour", "NonStructured")) %>% 
  ggplot(aes(x=value, fill=name))+
  geom_density(alpha=0.5)+
  theme_bw()+
  labs(x= "rho", fill = NULL)+
  theme(legend.position = c(0.75,0.75))
dev.off()

cbind(
  rho_2spatial_madrid %>% group_by(name) %>% summarise(X2.5. = quantile(value,probs = 0.025),
                                                       X50. = quantile(value, probs = 0.5),
                                                       X97.5. = quantile(value,probs = 0.975)) %>% 
    mutate(madrid_report = paste0(format(round(X50.,2), nsmall=2)," (", format(round(X2.5.,2), nsmall=2), ", ", format(round(X97.5.,2), nsmall=2),")")) %>% 
    dplyr::select(name, madrid_report),
  
  rho_2spatial_castilla %>% group_by(name) %>% summarise(X2.5. = quantile(value,probs = 0.025),
                                                       X50. = quantile(value, probs = 0.5),
                                                       X97.5. = quantile(value,probs = 0.975)) %>% 
    mutate(castilla_report = paste0(format(round(X50.,2), nsmall=2)," (", format(round(X2.5.,2), nsmall=2), ", ", format(round(X97.5.,2), nsmall=2),")")) %>% 
    dplyr::select(castilla_report)
) %>% xtable %>% print(file = "manuscript//plots//rho_2spatial.tex", compress=FALSE)


print(sum(rstan::extract(madrid_2spatial)["rho"]$rho[,1]>rstan::extract(madrid_2spatial)["rho"]$rho[,2])/nrow(rstan::extract(madrid_2spatial)["rho"]$rho))
print(sum(rstan::extract(castilla_2spatial)["rho"]$rho[,1]>rstan::extract(castilla_2spatial)["rho"]$rho[,2])/nrow(rstan::extract(castilla_2spatial)["rho"]$rho))

pdf(file = "manuscript//plots//castilla_2spatial_traceplot_rho.pdf", height = 4, width = 6)
  traceplot(castilla_2spatial, pars = c("rho"))
dev.off()



#====================================================== Case plot Madrid===========================================================
summary_long <- read.csv("data//26Feb_05Jan_Madrid_Muni_Combined.csv") %>%
  dplyr::select( starts_with("day")) %>%
  pivot_longer(
    cols = starts_with("day"),
    names_to = "time",
    values_to = "cases",
    values_drop_na = TRUE
  ) %>% ungroup()%>% mutate(time = as.numeric(str_sub(time, start=5))) %>% group_by(time)%>% summarise(cases=sum(cases)) %>% 
  filter(time < 97)


time_idx = c(seq.Date(from = as.Date("2020/02/26"), by="day", to= as.Date("2020/05/31")))

summary_long$time = rep(time_idx, nrow(summary_long)/length(time_idx) )

total_cases<- summary_long %>% 
  group_by(time) %>% 
  summarise(total_cases = sum(cases)) 

pdf(file = "manuscript//plots//madrid_cases.pdf", height = 4, width = 5.5)
par(oma=c(0,0,0,0), mar = c(2,4,0.5,0.5), cex = 1.2)
  plot(total_cases$time, total_cases$total_cases, type = "l",col =, ylab = "Daily Cases", xlab = "", xaxt="n")
  axis(1, at = c(as.Date("2020-03-01"), as.Date("2020-04-01"), as.Date("2020-05-01"), as.Date("2020-06-01")), labels = c("March", "April", "May", "June"))
  abline(v = as.Date("2020-03-14"), col = "red", lty = 2)
  abline(v = as.Date("2020-05-11"), col = "blue", lty = 2)
  legend("topright",legend = c("lockdown start","lockdown lifted"), col=c("red","blue", "purple"), lty=c(2,2), bg = "white")
dev.off()
#=================================================Case plot Castilla==============================================================
summary_long <- read.csv("data//01March_08Jan_CYL_combined.csv") %>%
  dplyr::select(hzone_name, hzone_code, total_pop, lat, lon, starts_with("infected_day") ) %>%
  pivot_longer(
    cols = starts_with("infected_day"),
    names_to = "time",
    values_to = "cases",
    values_drop_na = TRUE
  ) %>% mutate(time = as.numeric(str_sub(time, start=14))) %>% 
  filter(time <= 92) #restrict cases to before June 1, 95 is number of days between March 01 and May 31 (inclusive)


time_idx = c(seq.Date(from = as.Date("2020/03/01"), by="day", to= as.Date("2020/05/31")))

summary_long$time = rep(time_idx, nrow(summary_long)/length(time_idx) )

total_cases<- summary_long %>% 
  group_by(time) %>% 
  summarise(total_cases = sum(cases)) 

pdf(file = "manuscript//plots//castilla_cases.pdf", height = 4, width = 5.5)
par(oma=c(0,0,0,0), mar = c(2,4,0.5,0.5), cex = 1.2)
  plot(total_cases$time, total_cases$total_cases, type = "l",col =, ylab = "Daily Cases", xlab = "", xaxt = "n")
  axis(1, at = c(as.Date("2020-03-01"), as.Date("2020-04-01"), as.Date("2020-05-01"), as.Date("2020-06-01")), labels = c("March", "April", "May", "June"))
  abline(v = as.Date("2020-03-14"), col = "red", lty = 2)
  abline(v = as.Date("2020-05-11"), col = "blue", lty = 2)
  legend("topright",legend = c("lockdown start","lockdown lifted"), col=c("red","blue", "purple"), lty=c(2,2), bg = "white")
dev.off()
#========================1spatial plots==================

load("results//madrid_1spatial2021-02-04.RData")

madrid_rho_movement_week1 = rstan::extract(madrid_movement_samples_week1)["rho"]
madrid_rho_physical = rstan::extract(BYM_samples_physical)["rho"]

madrid_samples_physical <- BYM_samples_physical

pdf(file = "manuscript//plots//madrid_1spatial_rho.pdf", height = 4, width = 5.5)
cbind(madrid_rho_movement_week1$rho, madrid_rho_physical$rho) %>% data.frame() %>% 
  rename(Movement = X1, Physical = X2) %>% 
  pivot_longer(cols = c("Movement", "Physical")) %>% 
  ggplot(aes(x=value, fill=name))+
  geom_density(alpha=0.5)+
  theme_bw()+
  labs(x= "rho", fill = NULL)+
  theme(legend.position = c(0.25,0.75))+
  xlim(0, 1)
dev.off()
  

load("results//castilla_samples_identity2021-02-15.RData")

castilla_rho_movement_week1 = rstan::extract(castilla_movement_samples_week1)["rho"]
castilla_rho_physical = rstan::extract(BYM_samples_physical)["rho"]

castilla_samples_physical <- BYM_samples_physical

pdf(file = "manuscript//plots//castilla_1spatial_rho.pdf", height = 4, width = 5.5)
cbind(castilla_rho_movement_week1$rho, castilla_rho_physical$rho) %>% data.frame() %>% 
  rename(Movement = X1, Physical = X2) %>% 
  pivot_longer(cols = c("Movement", "Physical")) %>% 
  ggplot(aes(x=value, fill=name))+
    geom_density(alpha=0.5)+
    theme_bw()+
    labs(x= "rho", fill = NULL)+
    theme(legend.position = c(0.25,0.75))+
  xlim(0, 1)
dev.off()


madrid_rho_movement_week1$rho %>% quantile(probs = c(0.025,0.5,0.975))
madrid_rho_physical$rho %>% quantile(probs = c(0.025,0.5,0.975))

castilla_rho_movement_week1$rho %>% quantile(probs = c(0.025,0.5,0.975))
castilla_rho_physical$rho %>% quantile(probs = c(0.025,0.5,0.975))

#table of results for section
rho_report <- cbind(rbind(rstan::extract(madrid_movement_samples_week1)["rho"]$rho %>% quantile(probs = c(0.025,0.5,0.975)),
                          rstan::extract(madrid_movement_samples_week1)["beta0"]$beta0 %>% quantile(probs = c(0.025,0.5,0.975)),
                          rstan::extract(madrid_movement_samples_week1)["betas"]$betas %>% quantile(probs = c(0.025,0.5,0.975)),
                          rstan::extract(madrid_movement_samples_week1)["sigma"]$sigma %>% quantile(probs = c(0.025,0.5,0.975)),
                          rstan::extract(madrid_samples_physical)["rho"]$rho %>% quantile(probs = c(0.025,0.5,0.975)),
                          rstan::extract(madrid_samples_physical)["beta0"]$beta0 %>% quantile(probs = c(0.025,0.5,0.975)),
                          rstan::extract(madrid_samples_physical)["betas"]$betas %>% quantile(probs = c(0.025,0.5,0.975)),
                          rstan::extract(madrid_samples_physical)["sigma"]$sigma %>% quantile(probs = c(0.025,0.5,0.975))),
      
                    rbind(rstan::extract(castilla_movement_samples_week1)["rho"]$rho %>% quantile(probs = c(0.025,0.5,0.975)),
                          rstan::extract(castilla_movement_samples_week1)["beta0"]$beta0 %>% quantile(probs = c(0.025,0.5,0.975)),
                          rstan::extract(castilla_movement_samples_week1)["betas"]$betas %>% quantile(probs = c(0.025,0.5,0.975)),
                          rstan::extract(castilla_movement_samples_week1)["sigma"]$sigma %>% quantile(probs = c(0.025,0.5,0.975)),
                          rstan::extract(castilla_samples_physical)["rho"]$rho %>% quantile(probs = c(0.025,0.5,0.975)),
                          rstan::extract(castilla_samples_physical)["beta0"]$beta0 %>% quantile(probs = c(0.025,0.5,0.975)),
                          rstan::extract(castilla_samples_physical)["betas"]$betas %>% quantile(probs = c(0.025,0.5,0.975)),
                          rstan::extract(castilla_samples_physical)["sigma"]$sigma %>% quantile(probs = c(0.025,0.5,0.975)))) %>% 
  data.frame() %>%
  mutate(report_madrid = paste0(format(round(X50.,2), nsmall=2), " ","(",format(round(X2.5.,2), nsmall=2), ", ", format(round(X97.5.,2), nsmall=2),")" ),
         report_castilla=paste0(format(round(X50..1,2), nsmall=2), " ","(",format(round(X2.5..1,2), nsmall=2), ", ", format(round(X97.5..1,2), nsmall=2),")" )) %>% 
  select(report_madrid,report_castilla)

 rho_report$colname =c("rho", "mu", "betas", "sigma","rho", "mu", "betas", "sigma")
# 
# rho_report$prob = c(sum(madrid_rho_movement_week1$rho>0.75)/400, 
#                     sum(madrid_rho_physical$rho>0.75)/400,
#                     sum(castilla_rho_movement_week1$rho>0.75)/400,
#                     sum(castilla_rho_physical$rho>0.75)/400)


rho_report <- rho_report %>% select(colname, report_madrid, report_castilla)

#colnames(rho_report) <- c("Model", "rho", "prob")


print(xtable(rho_report), file = "manuscript//plots//rho_report.tex", compress=FALSE)


#==========================================show difference in maps maybe==========================
muni <- readOGR("D:\\01_PhD\\Spain\\data\\castilla_mobility\\boundary") #fix this file path at some point?
#muni_toMatch <- readOGR("D:\\01_PhD\\Covid\\Spain Regional\\spain_20201025\\spain_spatial\\castilla_leon\\Castilla Shape Files\\health zones") #fix this file path at some point?
#muni_test <- readOGR("C:\\Users\\justi\\OneDrive\\Desktop\\shapeFiles")

attributes(muni)$proj4string = sp::CRS("+init=epsg:4326")

muni_data <-  read.csv("data//01March_08Jan_CYL_combined.csv") %>%
  dplyr::select(hzone_name, hzone_code, total_pop, lat, lon, starts_with("infected_day") ) %>%
  pivot_longer(
    cols = starts_with("infected_day"),
    names_to = "time",
    values_to = "cases",
    values_drop_na = TRUE) %>% 
  mutate(time = as.numeric(str_sub(time, start=14))) %>% 
  filter(time <= 92) %>% #restrict cases to before June 1, 95 is number of days between March 01 and May 31 (inclusive)
  mutate(hzone_code=ifelse(hzone_code %in% c(170813,170814), 170811, hzone_code )) %>% # codes 170813, 170814 are collapsed
  group_by(hzone_code) %>% 
  summarise(cases = sum(cases), lat=first(lat), lon=first(lon), hzone_name=first(hzone_name), total_pop=sum(unique(total_pop)) ) %>%
  arrange(hzone_code) %>% 
  ungroup() %>%
  mutate(hzcode = as.character(hzone_code), cases_per_thousand = (cases/total_pop)*1000)


castilla_sf <- muni %>% left_join(muni_data, by = c("hzcode")) %>% arrange(as.numeric(hzcode))

pdf(file = "manuscript//plots//castilla_cases_per_thousand.pdf", height = 4, width = 5.5)
  #spplot(castilla_sf, "cases_per_thousand", 
  #       par.settings = list(axis.line = list(col = 'transparent')),col.regions = rev(heat.colors(20)))
  scale = mapmisc::colourScale(castilla_sf$cases_per_thousand, 
                               breaks = c(0,2,5,10,20,30,50,100), 
                               style = "fixed", 
                               col=heat.colors, revCol=TRUE) #c(0,1,2,5,10,20,50,150)
  par(mar = c(1, 1, 1, 1))
  plot(castilla_sf, col=scale$plot)
  plot(spain, add=TRUE)
  mapmisc::scaleBar(castilla_sf, pos = "bottomleft", seg.len=3, bty = "n", outer=TRUE, cex=0.8)
  legendBreaks("topright", breaks =scale, bty = "n", cex=0.8)
  insetMap(castilla_sf, "bottomright", map=theBackground, width=0.25, cropInset = extent(-15,5,30,50))
dev.off()

load("results//castilla_samples_identity2021-03-17.RData")
load("data//castilla_input.RData")


castilla_sf$eta = NA

for (i in 1:245){
  castilla_sf$eta[i] <- median(exp(rstan::extract(castilla_2spatial)["eta"]$eta[,i])*1000/castilla_sf$total_pop[i])
}

for (i in 1:245){
  castilla_sf$eta_sd[i] <- sd(exp(rstan::extract(castilla_2spatial)["eta"]$eta[,i])*1000/castilla_sf$total_pop[i])
}

for (i in 1:245){
  castilla_sf$phi_movement[i] <- median(rstan::extract(castilla_2spatial)["sigma"]$sigma * rstan::extract(castilla_2spatial)["phi_movement"]$phi_movement[,i] * 
                                        sqrt(rstan::extract(castilla_2spatial)["rho"]$rho[,1]/castilla_mobility_input$s))
}

for (i in 1:245){
  castilla_sf$phi_physical[i] <- median(extract(castilla_2spatial)["sigma"]$sigma * extract(castilla_2spatial)["phi_physical"]$phi_physical[,i] * 
                                          sqrt(extract(castilla_2spatial)["rho"]$rho[,2]/castilla_physical_input$s))
}


pdf(file = "manuscript//plots//castilla_relative_risks.pdf", height = 4, width = 4)
  scale = mapmisc::colourScale(castilla_sf$eta, 
                               breaks = c(0,2,5,10,20,30,50,100), 
                               style = "fixed",
                               col=heat.colors,
                               rev=TRUE)
  par(mar = c(2, 2, 2, 2))
  plot(castilla_sf, col=scale$plot)
  mapmisc::scaleBar(castilla_sf,pos = "bottomright", seg.len=3, bty = "n", outer=TRUE, title.cex=1.2)
  legendBreaks("topright", breaks =scale, bty = "n", cex=0.8)
dev.off()

pdf(file = "manuscript//plots//castilla_eta_sd.pdf", height = 4, width = 4)
  scale = mapmisc::colourScale(castilla_sf$eta_sd, 
                               breaks = c(0,1,2,3,5,10), 
                               style = "fixed",
                               col=heat.colors,
                               rev=TRUE)
  par(mar = c(2, 2, 2, 2))
  plot(castilla_sf, col=scale$plot)
  mapmisc::scaleBar(castilla_sf,pos = "bottomright", seg.len=3, bty = "n", outer=TRUE, title.cex=1.2)
  legendBreaks("topright", breaks =scale, bty = "n", cex=0.8)
dev.off()
  
castilla_sf$convolved_re <- colMeans(extract(castilla_2spatial)["convolved_re"]$convolved_re)

pdf(file = "manuscript//plots//castilla_phi_movement.pdf", height = 4, width = 4)
#  spplot(castilla_sf, "phi_movement",col.regions = rev(heat.colors(11)),
#         par.settings = list(axis.line = list(col = 'transparent')),
#         at = seq(-1.5,1.5,0.3), 
#         colorkey = list(
#           at = 1:11,
#           labels = as.character(seq(-1.5,1.5,0.3) )))
  scale = mapmisc::colourScale(castilla_sf$phi_movement, 
                               breaks = c(-2.5,-1.00,-0.50,-0.25, 0.25, 0.50, 1.00, 2.5), 
                               style = "fixed",
                               col=heat.colors,
                               rev=TRUE)
  par(mar = c(2, 2, 2, 2))
  plot(castilla_sf, col=scale$plot)
  mapmisc::scaleBar(castilla_sf,pos = "bottomright", seg.len=3, bty = "n", outer=TRUE, title.cex=1.2)
  legendBreaks("topright", breaks =scale, bty = "n", cex=0.8)
dev.off()

pdf(file = "manuscript//plots//castilla_phi_physical.pdf", height = 4, width = 4)
# spplot(castilla_sf, "phi_physical",col.regions = rev(heat.colors(11)),
#         par.settings = list(axis.line = list(col = 'transparent')),
#         at = seq(-1.5,1.5,0.3), 
#         colorkey = list(
#           at = 1:11,
#           labels = as.character(seq(-1.5,1.5,0.3) )))
  scale = mapmisc::colourScale(castilla_sf$phi_physical, 
                               breaks = c(-2.5,-1.00,-0.50,-0.25, 0.25, 0.50, 1.00, 2.5), 
                               style = "fixed",
                               col=heat.colors,
                               rev=TRUE)
  par(mar = c(2, 2, 2, 2))
  plot(castilla_sf, col=scale$plot)
  mapmisc::scaleBar(castilla_sf,pos = "bottomright", seg.len=3, bty = "n", outer=TRUE, title.cex=1.2)
  legendBreaks("topright", breaks =scale, bty = "n", cex=0.8)
dev.off()


files <- list.files(path = "data\\castilla_mobility\\Castilla Y Leon",pattern=".csv")[1:92] #only take movement up until end of first wave
castilla_mobility_matrix_week1 <- matrix(0, nrow= 245, ncol =245)

for (i in 1:7){
  tmp <- read.csv(paste0("data\\castilla_mobility\\Castilla Y Leon\\", files[i]) )
  castilla_mobility_matrix_week1 <- castilla_mobility_matrix_week1 + tmp[,-1]
}

castilla_sf$within <- diag(castilla_mobility_matrix_week1 %>% as.matrix)

castilla_mobility_matrix_week1 <- castilla_mobility_matrix_week1- diag(diag(castilla_mobility_matrix_week1 %>% as.matrix))

castilla_sf$from <- rowSums(castilla_mobility_matrix_week1)

castilla_sf$to<-colSums(castilla_mobility_matrix_week1)


pdf(file = "manuscript//plots//castilla_to.pdf", height = 4, width = 4)
#spplot(castilla_sf, "to", 
#       par.settings = list(axis.line = list(col = 'transparent')),
#       at = c(0, 10, 50, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000), # actual location
#       colorkey = NULL)
  scale = mapmisc::colourScale(castilla_sf$to, 
                               breaks = c(0, 10, 50, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000, 10000000, 50000000), 
                               style = "fixed",
                               col=terrain.colors,
                               rev=TRUE)
  par(mar = c(0, 1, 1, 1))
  plot(castilla_sf, col=scale$plot)
  #mapmisc::scaleBar(castilla_sf,pos = "bottomright", bty = "n", outer=TRUE, title.cex=1.2)
dev.off()

pdf(file = "manuscript//plots//castilla_from.pdf", height = 4, width = 4.5)
#spplot(castilla_sf, "from", 
#       par.settings = list(axis.line = list(col = 'transparent')),
#       at = c(0, 10, 50, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000), # actual location
#       colorkey = NULL)
  scale = mapmisc::colourScale(castilla_sf$from, 
                               breaks = c(0, 10, 50, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000, 10000000, 50000000), 
                               style = "fixed",
                               col=terrain.colors,
                               rev=TRUE)
  par(mar = c(0, 1, 1, 4))
  plot(castilla_sf, col=scale$plot)
  #mapmisc::scaleBar(castilla_sf,pos = "bottomright", bty = "n", outer=TRUE)
  legendBreaks("right", breaks =scale, bty = "n", cex=1, 
              legend =c("0","10","50", "100", "500","1k","5k","10k","50k","100k","500k","1M","5M","10M", "50M"))
dev.off()

pdf(file = "manuscript//plots//castilla_within.pdf", height = 4, width = 4)
#spplot(castilla_sf, "within", 
#       par.settings = list(axis.line = list(col = 'transparent')),
#       at = c(0, 10, 50, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000), # actual location
#       colorkey = list(
#         at = 1:12,
#         labels = c("1","10","50", "100", "500","1k","5k","10k","50k","100k","500k","1M")))
  scale = mapmisc::colourScale(castilla_sf$within, 
                               breaks = c(0, 10, 50, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000, 10000000, 50000000), 
                               style = "fixed",
                               col=terrain.colors,
                               rev=TRUE)
  par(mar = c(0, 1, 1, 1))
  plot(castilla_sf, col=scale$plot)
  mapmisc::scaleBar(castilla_sf,pos = "bottomright", bty = "n", outer=TRUE, title.cex=1.2)
dev.off()



#=======================================Madrid Case maps==============================================
muni <- readOGR("D:\\01_PhD\\Covid\\Spain Regional\\spain_20201025\\spain_spatial\\madrid\\Shape File-Madrid-Municipalities") #fix this file path at some point?
muni_data <- read.csv("data//26Feb_05Jan_Madrid_Muni_Combined.csv") %>%
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
  ungroup() %>% 
  mutate(natcode = paste0("341328", as.character(MunicipalityCode)),
         cases_per_thousand = (cases/TotalPopulation)*1000)

muni$natcode[which(muni$nameunit=="Madrid")] <- "34132828000" #Match spatial units to Jorge's coding

muni <- muni %>% arrange(as.numeric(natcode))
madrid_sf <-muni %>%  left_join(muni_data, by = c("natcode"))

pdf(file = "manuscript//plots//madrid_cases_per_thousand.pdf", height = 4, width = 5.5)
  scale = mapmisc::colourScale(madrid_sf$cases_per_thousand, 
                               breaks = c(0,2,5,10,20,30,50,100), 
                               style = "fixed", 
                               col=heat.colors, revCol=TRUE) #c(0,1,2,5,10,20,50,150)
  par(mar = c(1, 1, 1, 1))
  plot(madrid_sf, col=scale$plot)
  #plot(spain, add=TRUE)
  mapmisc::scaleBar(madrid_sf,pos = "bottomright", seg.len=5, bty = "n", outer=TRUE, cex=0.8)
  legendBreaks("topright", breaks =scale, bty = "n", cex=0.8)
  insetMap(madrid_sf, "topleft", map=theBackground, width=0.25, cropInset = extent(-15,10,30,50))
dev.off()


load("results//madrid_2spatial_identity2021-02-02.RData")
load("data//madrid_input.RData")

madrid_sf$eta = NA

for (i in 1:179){
  madrid_sf$eta[i] <- median(exp(rstan::extract(madrid_2spatial)["eta"]$eta[,i])*1000/madrid_sf$TotalPopulation[i])
}

for (i in 1:179){
  madrid_sf$eta_sd[i] <- sd(exp(extract(madrid_2spatial)["eta"]$eta[,i])*1000/madrid_sf$TotalPopulation[i])
}

for (i in 1:179){
  madrid_sf$phi_movement[i] <- median(extract(madrid_2spatial)["sigma"]$sigma*extract(madrid_2spatial)["phi_movement"]$phi_movement[,i] * 
    sqrt(extract(madrid_2spatial)["rho"]$rho[,1]/madrid_mobility_input$s))
}

for (i in 1:179){
  madrid_sf$phi_physical[i] <- median(extract(madrid_2spatial)["sigma"]$sigma*extract(madrid_2spatial)["phi_physical"]$phi_physical[,i] * 
    sqrt(extract(madrid_2spatial)["rho"]$rho[,2]/madrid_physical_input$s))
}


madrid_sf$convolved_re <- colMeans(extract(madrid_2spatial)["convolved_re"]$convolved_re)

pdf(file = "manuscript//plots//madrid_phi_movement.pdf", height = 4, width = 4)
#  spplot(madrid_sf, "phi_movement", col.regions = rev(heat.colors(11)),
#         par.settings = list(axis.line = list(col = 'transparent')),
#         at = seq(-2.5,2.5,0.5), 
#         colorkey = list(
#           at = 1:11,
#           labels = as.character(seq(-2.5,2.5,0.5))))
  scale = mapmisc::colourScale(madrid_sf$phi_movement, 
                               breaks = c(-2.5,-1.00,-0.50,-0.25,0.25,0.50,1.00,2.5), 
                               style = "fixed",
                               col=heat.colors,
                               rev=TRUE)
  par(mar = c(1, 1, 1, 1))
  plot(madrid_sf, col=scale$plot)
  mapmisc::scaleBar(madrid_sf,pos = "bottomright", seg.len=3, bty = "n", outer=TRUE, title.cex=1.2)
  legendBreaks("topright", breaks =scale, bty = "n", cex=0.8)
dev.off()

pdf(file = "manuscript//plots//madrid_phi_physical.pdf", height = 4, width = 4)
#  spplot(madrid_sf, "phi_physical",col.regions = rev(heat.colors(11)),
#         par.settings = list(axis.line = list(col = 'transparent')),
#         at = seq(-2.5,2.5,0.5), 
#         colorkey = list(
#           at = 1:11,
#           labels = as.character(seq(-2.5,2.5,0.5))))

  scale = mapmisc::colourScale(madrid_sf$phi_physical, 
                               breaks = c(-2.5,-1.00,-0.50,-0.25,0.25,0.50,1.00,2.5), 
                               style = "fixed",
                               col=heat.colors,
                               rev=TRUE)
  par(mar = c(1, 1, 1, 1))
  plot(madrid_sf, col=scale$plot)
  mapmisc::scaleBar(madrid_sf,pos = "bottomright", seg.len=3, bty = "n", outer=TRUE, title.cex=1.2)
  legendBreaks("topright", breaks =scale, bty = "n", cex=0.8)
dev.off()
  
pdf(file = "manuscript//plots//madrid_relative_risks.pdf", height = 4, width = 4)
  scale = mapmisc::colourScale(madrid_sf$eta, 
                               breaks = c(0,2,5,10,20,30,50,100), 
                               style = "fixed",
                               col=heat.colors,
                               rev=TRUE)
  par(mar = c(2, 2, 2, 2))
  plot(madrid_sf, col=scale$plot)
  mapmisc::scaleBar(madrid_sf,pos = "bottomright", seg.len=3, bty = "n", outer=TRUE, title.cex=1.2)
  legendBreaks("topright", breaks =scale, bty = "n", cex=0.8)
dev.off()

pdf(file = "manuscript//plots//madrid_eta_sd.pdf", height = 4, width = 4)
  scale = mapmisc::colourScale(madrid_sf$eta_sd, 
                               breaks = c(0,1,2,3,5,10), 
                               style = "fixed",
                               col=heat.colors,
                               rev=TRUE)
  par(mar = c(2, 2, 2, 2))
  plot(madrid_sf, col=scale$plot)
  mapmisc::scaleBar(madrid_sf,pos = "bottomright", seg.len=3, bty = "n", outer=TRUE, title.cex=1.2)
  legendBreaks("topright", breaks =scale, bty = "n", cex=0.8)
dev.off()


#===================================================================================================================

files <- list.files(path = "data\\madrid_mobility\\Madrid",pattern=".csv")[1:92] #only take movement up until end of first wave
madrid_mobility_matrix_week1 <- matrix(0, nrow= 179, ncol =179)

for (i in 1:7){
  tmp <- read.csv(paste0("data\\madrid_mobility\\Madrid\\", files[i]) )
  madrid_mobility_matrix_week1 <- madrid_mobility_matrix_week1 + tmp[,-1]
}

madrid_sf$within <- diag(madrid_mobility_matrix_week1 %>% as.matrix)

madrid_mobility_matrix_week1 <- madrid_mobility_matrix_week1- diag(diag(madrid_mobility_matrix_week1 %>% as.matrix))

madrid_sf$from <- rowSums(madrid_mobility_matrix_week1)

madrid_sf$to<-colSums(madrid_mobility_matrix_week1)

#col.regions = rev(heat.colors(20)

#at = c(10, 50, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000, 10000000, 50000000)

pdf(file = "manuscript//plots//madrid_to.pdf", height = 4, width = 4)
#spplot(madrid_sf, "to", 
#       par.settings = list(axis.line = list(col = 'transparent')),
#       at = c(10, 50, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000, 10000000, 50000000), # actual location
#       colorkey = NULL)
  scale = mapmisc::colourScale(madrid_sf$to, 
                               breaks = c(0, 10, 50, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000, 10000000, 50000000), 
                               style = "fixed",
                               col=terrain.colors,
                               rev=TRUE)
  par(mar = c(1, 1, 1, 1))
  plot(madrid_sf, col=scale$plot)
  #mapmisc::scaleBar(madrid_sf,pos = "bottomright", seg.len=3, bty = "n", outer=TRUE, title.cex=1.2)
dev.off()

pdf(file = "manuscript//plots//madrid_from.pdf", height = 4, width = 4)
#spplot(madrid_sf, "from", 
#       par.settings = list(axis.line = list(col = 'transparent')),
#       at = c(10, 50, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000, 10000000, 50000000), # actual location
#       colorkey = NULL)
scale = mapmisc::colourScale(madrid_sf$from, 
                             breaks = c(0, 10, 50, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000, 10000000, 50000000), 
                             style = "fixed",
                             col=terrain.colors,
                             rev=TRUE)
par(mar = c(1, 1, 1, 1))
plot(madrid_sf, col=scale$plot)
#mapmisc::scaleBar(madrid_sf,pos = "bottomright", seg.len=3, bty = "n", outer=TRUE, title.cex=1.2)
dev.off()


pdf(file = "manuscript//plots//madrid_within.pdf", height = 4, width = 4)
#spplot(madrid_sf, "within", 
#       par.settings = list(axis.line = list(col = 'transparent')),
#       at = c(10, 50, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000, 10000000, 50000000), # actual location
#       colorkey = list(
#         at = 1:14,
#         labels = c("10","50", "100", "500","1k","5k","10k","50k","100k","500k","1M","5M","10M", "50M")))
  scale = mapmisc::colourScale(madrid_sf$within, 
                               breaks = c(0, 10, 50, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000, 10000000, 50000000), 
                               style = "fixed",
                               col=terrain.colors,
                               rev=TRUE)
  par(mar = c(1, 1, 1, 1))
  plot(madrid_sf, col=scale$plot)
  mapmisc::scaleBar(madrid_sf,pos = "bottomright", seg.len=3, bty = "n", outer=TRUE, title.cex=1.2)
dev.off()

##as 1 diagram, realised that I can't do the subfigure thing
# spplot(madrid_sf, c("from","to","within"),  layout = c(3,1), identify=FALSE, par.strip.text=list(cex=0, col = "transparent"),
#        par.settings = list(axis.line = list(col = 'transparent'), 
#                            strip.background = list(col=c("transparent", "transparent", "transparent")),
#                            strip.border=list(col=c("transparent", "transparent", "transparent"))),
#        at = c(10, 50, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000, 10000000, 50000000), # actual location
#        colorkey=list(
#          at = 1:14,
#          labels = c("10","50", "100", "500","1k","5k","10k","50k","100k","500k","1M","5M","10M", "50M")))



madrid_mobility_matrix_week1 <- madrid_mobility_matrix_week1- diag(diag(madrid_mobility_matrix_week1 %>% as.matrix))

madrid_sf$from_madrid <- as.numeric(madrid_mobility_matrix_week1[1,])

madrid_sf$to_madrid<-madrid_mobility_matrix_week1[,1]

pdf(file = "manuscript//plots//madrid_to_madrid.pdf", height = 4, width = 4)
#spplot(madrid_sf, "to_madrid", 
#       par.settings = list(axis.line = list(col = 'transparent')),
#       at = c(10, 50, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000), # actual location
#       colorkey = NULL)

scale = mapmisc::colourScale(madrid_sf$to_madrid, 
                             breaks = c(0, 10, 50, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000), 
                             style = "fixed",
                             col=terrain.colors,
                             rev=TRUE)
par(mar = c(0, 1, 1, 4))
plot(madrid_sf, col=scale$plot)
mapmisc::scaleBar(madrid_sf,pos = "bottomright", seg.len=3, bty = "n", outer=TRUE, title.cex=1.0)
legendBreaks("topright", breaks =scale, bty = "n", cex=1, 
             legend =c("0","10","50", "100", "500","1k","5k","10k","50k","100k","500k","1M","5M"))
dev.off()

pdf(file = "manuscript//plots//madrid_from_madrid.pdf", height = 4, width = 4.2)
# spplot(madrid_sf, "from_madrid", 
#        par.settings = list(axis.line = list(col = 'transparent')),
#        at = c(10, 50, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000), # actual location
#        colorkey = list(
#          at = 1:10,
#          labels = c("10","50", "100", "500","1k","5k","10k","50k","100k","500k")))

scale = mapmisc::colourScale(madrid_sf$from_madrid, 
                             breaks = c(0, 10, 50, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000), 
                             style = "fixed",
                             col=terrain.colors,
                             rev=TRUE)
par(mar = c(0, 1, 1, 4))
plot(madrid_sf, col=scale$plot)
mapmisc::scaleBar(madrid_sf,pos = "bottomright", seg.len=3, bty = "n", outer=TRUE, title.cex=1.0)
legendBreaks("topright", breaks =scale, bty = "n", cex=1, 
             legend =c("0","10","50", "100", "500","1k","5k","10k","50k","100k","500k","1M","5M"))

dev.off()