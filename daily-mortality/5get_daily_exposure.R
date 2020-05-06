library(rstan)
library(robustbase)
library(data.table)
library(lubridate)
library(dplyr)

library(sp)
library(rgdal)
library(here)
setwd(here("Model","Toronto1996_2012","fit model"))

load("data_all_toronto.RData")

data_all_toronto$date = strptime(
  as.character(data_all_toronto$date), format = '%Y-%m-%d %H:%M:%S',
  tz='UTC'
)


# the firstDay is used for day difference calculate
firstDay = as.Date("1995-12-31")
endDay = as.Date("2012-12-31")


# get the covariates ready for stan model ---------------------------------

data_all_toronto$day = trunc(data_all_toronto$date, units='days')

data_all_toronto$cos12 = cos(2*pi*as.numeric(data_all_toronto$day)/(365.25*24*60*60))
data_all_toronto$cos6 = cos(2*2*pi*as.numeric(data_all_toronto$day)/(365.25*24*60*60))
data_all_toronto$cos3 = cos(2*2*2*pi*as.numeric(data_all_toronto$day)/(365.25*24*60*60))
data_all_toronto$sin12 = sin(2*pi*as.numeric(data_all_toronto$day)/(365.25*24*60*60))
data_all_toronto$sin6 = sin(2*2*pi*as.numeric(data_all_toronto$day)/(365.25*24*60*60))
data_all_toronto$sin3 = sin(2*2*2*pi*as.numeric(data_all_toronto$day)/(365.25*24*60*60))


data_all_toronto$dow = factor(weekdays(data_all_toronto$day), levels = 
                                weekdays(seq(ISOdate(2000,1,3), len=7, by='days')))

data_all_toronto$dayInt = as.numeric(difftime(data_all_toronto$day, firstDay, units='days'))

data_all_toronto$hour = as.numeric(format(data_all_toronto$date, format='%H'))+1

data_all_toronto$pollutant <- as.numeric(factor(data_all_toronto$pollutant, levels = c("pm25","pm10","no2","o3")))
data_all_toronto$station <- as.numeric(factor(data_all_toronto$station))


temp <- length(unique(data_all_toronto$station))
All_data <- NULL
for(i in 1:temp)
{
  
  stuff1 <- data.frame(date=seq(min(data_all_toronto$date),max(data_all_toronto$date), by="hour"),pollutant=1,station=i)
  stuff2 <- data.frame(date=seq(min(data_all_toronto$date),max(data_all_toronto$date), by="hour"),pollutant=2,station=i)
  stuff3 <- data.frame(date=seq(min(data_all_toronto$date),max(data_all_toronto$date), by="hour"),pollutant=3,station=i)
  stuff4 <- data.frame(date=seq(min(data_all_toronto$date),max(data_all_toronto$date), by="hour"),pollutant=4,station=i)
  
  All_data <- rbind(All_data, stuff1, stuff2, stuff3, stuff4)
}

All_data$date = as.character(All_data$date)
data_all_toronto$date=as.character(data_all_toronto$date)
data_all_toronto$day=as.character(data_all_toronto$day)

All_data <- dplyr::left_join(All_data, data_all_toronto, by=c("pollutant","station","date"))

All_data$date = strptime(
  as.character(All_data$date), format = '%Y-%m-%d %H:%M:%S',
  tz='UTC'
)



All_data$day = trunc(All_data$date, units='days')

All_data$dow = factor(weekdays(All_data$day), levels = 
                        weekdays(seq(ISOdate(2000,1,3), len=7, by='days')))

All_data$dow = as.numeric(All_data$dow)

All_data$dayInt = as.numeric(difftime(All_data$day, firstDay, units='days'))

All_data$hour = as.numeric(format(All_data$date, format='%H'))+1


# get daily summaries
# exclude the hourly outliers

pollutantVar <- c("pm25", "pm10","no2","o3")
for (j in c(1,3,4))
{
  mixed_no_outlier <- NULL
 for(i in 1:11)
 {
    # setwd("/store/guowen/JASA/Toronto1996_2012")
    load( paste0("mixed_complete_",i,"_",j,".RData"))
    load( paste0("sample_store_",i,"_",j,".RData"))
    load( paste0("prob_store_",i,"_",j,".RData"))
    
    #if outliers, using the predicted values from gamma
    temp_index <- which(apply(prob_store,1,mean)<0.5)
    mixed_complete[temp_index,] <- sample_store[temp_index,]
    
    mixed_complete <- data.frame(date=stuff1$date,V=mixed_complete)
    mixed_no_outlier <- rbind(mixed_no_outlier, mixed_complete)
    print(i)
 }
  
  mixed_no_outlier$date <- strptime(
    as.character(mixed_no_outlier$date), format = '%Y-%m-%d %H:%M:%S',
    tz='UTC'
  )
  mixed_no_outlier$date <- date(mixed_no_outlier$date)

dailymean <- aggregate(.~date, data=mixed_no_outlier, FUN = mean)
save(dailymean, file = paste0(pollutantVar[j],"dailymeansample.RData"))
}

