
library(rstan)
library(robustbase)
library(data.table)
library(lubridate)
library(dplyr)
library(sp)
library(rgdal)
library(here)
library(INLA)
library(Epi)
library(survival)
library(denstrip)


fitClogitResults <- function(diseaseCause, sample_index)
{
  
  tempSeq = seq(-40,40,len=201)
  
  new_DataSub <- mortCity[mortCity$year>=1996 & mortCity$year<=2012,]
  new_DataSub <- new_DataSub[new_DataSub$age>=36 & new_DataSub$age<=66,]
  new_DataSub <- new_DataSub[,c("date", "sex", "age","count","cause")]
  new_DataSub <- new_DataSub[new_DataSub$count!=0,]
  new_DataSub$case <- 1
  #add the previous day exposure
  exposure_columns <- data.frame(pm25=dailymeanpm25[,(1+sample_index)]
                                 ,no2=dailymeanno2[,(1+sample_index)]
                                 ,o3=dailymeano3[,(1+sample_index)]
  )
  preday_exposure <- rbind(exposure_columns[1,],zoo::rollapply(exposure_columns, FUN = mean, width = 2))
  
  preday_exposure <- cbind(date=dailymeanpm25$date, preday_exposure)
  preday_exposure$date <- as.Date(preday_exposure$date)
  dailytemp_city$date <- as.Date(dailytemp_city$date)
  
  preday_exposure <- dplyr::left_join(preday_exposure, dailytemp_city, by="date")
  
  preweek_exposure <- preday_exposure
  preTWOweek_exposure <- preday_exposure
  preweek_exposure$date <- preweek_exposure$date+7
  preTWOweek_exposure$date <- preTWOweek_exposure$date+14
  
  Final_data1 <- left_join(new_DataSub, preday_exposure, by="date")
  Final_data1$stratum <- 1:nrow(Final_data1)
  
  Final_data2 <- left_join(new_DataSub, preweek_exposure, by="date")
  Final_data2$stratum <- 1:nrow(Final_data2)
  Final_data2$case <- 0
  
  Final_data3 <- left_join(new_DataSub, preTWOweek_exposure, by="date")
  Final_data3$stratum <- 1:nrow(Final_data3)
  Final_data3$case <- 0
  
  Final_data <- rbind(Final_data1, Final_data2, Final_data3)
  remove(Final_data1)
  remove(Final_data2)
  remove(Final_data3)
  
  ## predict splines
  
  refTemp = 18
  Sp_predict = Ns(tempSeq, df = 4, ref=refTemp)
  attributes(Sp_predict)$ref = refTemp
  
  Sp_temp <- Ns(Final_data$max_of_max, knots = attributes(Sp_predict)$knots, 
                Boundary.knots = attributes(Sp_predict)$Boundary.knots, 
                ref = attributes(Sp_predict)$ref) 
  
  colnames(Sp_temp) <- c("spline1", "spline2","spline3","spline4")
  Final_data <- cbind(Final_data, Sp_temp)
  
  model_all <- clogit(case ~  pm25 + no2 + o3 + spline1 + spline2 + spline3 + spline4 + strata(stratum),
                      weights = count, method = "approximate", data=Final_data[Final_data$cause==diseaseCause, ])
  
  return(model_all)
  
}

########################################
file_path <- path_fitmodel_toronto
# get the disease data and exposure data ready
load(file.path(file_path, "pm25dailymeansample.RData"))
dailymeanpm25 <- dailymean
load(file.path(file_path, "no2dailymeansample.RData"))
dailymeanno2 <- dailymean
load(file.path(file_path, "o3dailymeansample.RData"))
dailymeano3 <- dailymean
remove(dailymean)
#########################################
load(file.path(report_path, 'dailytemp_toronto.RData'))
dailytemp_city <- dailytemp_toronto

load(file.path(report_path, 'mortToronto.RData'))
mortCity <- mortToronto

for (fit_index in 1:50)
{
  
  Toronto_mort_all <- fitClogitResults(diseaseCause = "all", sample_index = fit_index)
  Toronto_mort_circ <- fitClogitResults(diseaseCause = "circ", sample_index = fit_index)
  Toronto_mort_pulm <- fitClogitResults(diseaseCause = "pulm", sample_index = fit_index)
  
  save(Toronto_mort_all, file=file.path(path_fitmodel_Manuscript,"all_clogit_model_results",paste0("Toronto_mort_all_",fit_index,".RData")))
  save(Toronto_mort_circ, file=file.path(path_fitmodel_Manuscript,"all_clogit_model_results",paste0("Toronto_mort_circ_",fit_index,".RData")))
  save(Toronto_mort_pulm, file=file.path(path_fitmodel_Manuscript,"all_clogit_model_results",paste0("Toronto_mort_pulm_",fit_index,".RData")))
  
}
rm(Toronto_mort_all)
rm(Toronto_mort_circ)
rm(Toronto_mort_pulm)

### morbidity
load(file.path(path_fitmodel_Manuscript, 'morbToronto.RData'))
mortCity <- morbToronto

for (fit_index in 1:50)
{
  
  Toronto_morb_all <- fitClogitResults(diseaseCause = "all", sample_index = fit_index)
  Toronto_morb_circ <- fitClogitResults(diseaseCause = "cir", sample_index = fit_index)
  Toronto_morb_pulm <- fitClogitResults(diseaseCause = "Pulm", sample_index = fit_index)
  
  save(Toronto_morb_all, file=file.path(path_fitmodel_Manuscript,"all_clogit_model_results",paste0("Toronto_morb_all_",fit_index,".RData")))
  save(Toronto_morb_circ, file=file.path(path_fitmodel_Manuscript,"all_clogit_model_results",paste0("Toronto_morb_circ_",fit_index,".RData")))
  save(Toronto_morb_pulm, file=file.path(path_fitmodel_Manuscript,"all_clogit_model_results",paste0("Toronto_morb_pulm_",fit_index,".RData")))
}
rm(Toronto_morb_all)
rm(Toronto_morb_circ)
rm(Toronto_morb_pulm)









