
library(sp)
library(rgdal)
library(dplyr)
library(here)
library(tidyverse)
library(lubridate)

load("~/NEW_pollution/data_2000_2019.RData")

colnames(data_2000_2019) <- c("pollutant","station","location","province","lat","lon","date","00",paste0("0",seq(1:9)),seq(10,23))

data_2000_2019$pollutant[data_2000_2019$pollutant=="PM2.5"] <- "pm25"
data_2000_2019$pollutant[data_2000_2019$pollutant=="PM10"] <- "pm10"
data_2000_2019$pollutant[data_2000_2019$pollutant=="NO2"] <- "no2"
data_2000_2019$pollutant[data_2000_2019$pollutant=="O3"] <- "o3"

data_2000_2019 <- data_2000_2019 %>% pivot_longer(cols = -c(1:7), names_to = "hour")

data_2000_2019$date <- paste0(data_2000_2019$date, data_2000_2019$hour,"0000")
data_2000_2019$date <- lubridate::ymd_hms(data_2000_2019$date)

#check whether there are those only having data for 2000 & 2019

temp_station <- unique(data_2000_2019$station[! is.na(data_2000_2019$value)])
temp_station2 <- unique(data_2000_2019$station[(! is.na(data_2000_2019$value)) & year(data_2000_2019$date)>2000 & year(data_2000_2019$date) <2019])
out_station <- setdiff(temp_station, temp_station2)
data_2000_2019 <- data_2000_2019[! data_2000_2019$station %in% out_station,]



fit_JASA <- function(start_year=2000,
                     end_year=2019,
                     cityname,
                     # id_index,
                     citysearchkeyword=NULL,
                     id_index_low,
                     id_index_high,
                     chain_seed,
                     save_chain_name,
                     firstChain=FALSE
                     )
{
### create folders for city
output_dir <- file.path(here::here(), "Model", paste0(cityname,start_year,"_",end_year))
if (!dir.exists(output_dir)){
  dir.create(output_dir)
} else {
  print("Dir already exists!")
}
output_dir <- file.path(here::here(), "Model", paste0(cityname,start_year,"_",end_year),"fit model")

if (!dir.exists(output_dir)){
  dir.create(output_dir)
} else {
  print("Dir already exists!")
}

if(firstChain) # the second time to fit the model, do not need to tidy the data again.
{
### read in data
  if(is.null(citysearchkeyword))
  {
    data_all <- data_2000_2019 %>% dplyr::filter(station>=id_index_low & station<= id_index_high)
  }else{
    
data_all <- data_2000_2019 %>% dplyr::filter(grepl(citysearchkeyword,location))
  }
  
data_all$station=as.numeric(as.character(data_all$station))
data_all$value <- as.numeric(data_all$value)

## delete NA and invalid values
data_all <- data_all[-which(is.na(data_all$value)),]
data_all <- data_all[data_all$value>=0,]

# since PM2.5 might have a few methods for measuring, so there are duplicated date. we aggregate them by using max.

data_all <- data_all %>% group_by(pollutant,station, location, province, lat, lon, date,hour) %>% summarise(newvalue=max(value,na.rm=T)) %>% rename("value"="newvalue")

#check any duplicated date for each station.
if_duplicated <- NULL
id_index <- unique(data_all$station)
id_index <- id_index[order(id_index)]
for(pollutant in c("no2", "pm25","pm10","o3"))
{
  for(j in 1:length(id_index))
  {
    if_duplicated <- c(if_duplicated,(sum(duplicated(data_all$date[data_all$station==id_index[j] & data_all$pollutant==pollutant]))))
    
  }
}

if(sum(if_duplicated) > 0) stop("there are duplicated date, station, pollutants.")


save(data_all, file =file.path(output_dir, "data_all.RData"))
###################################
# get the input data
###################################

library(rstan)
library(robustbase)
library(data.table)
library(here)

load(file.path(output_dir, "data_all.RData"))

data_all$date = strptime(
  as.character(data_all$date), format = '%Y-%m-%d %H:%M:%S',
  tz='UTC'
)

# the firstDay is used for day difference calculate
firstDay = as.Date(paste0(start_year-1,"-12-31"))
endDay = as.Date(paste0(end_year,"-12-31"))

# get the covariates ready for stan model ---------------------------------

data_all$day = trunc(data_all$date, units='days')

data_all$cos12 = cos(2*pi*as.numeric(data_all$day)/(365.25*24*60*60))
data_all$cos6 = cos(2*2*pi*as.numeric(data_all$day)/(365.25*24*60*60))
data_all$cos3 = cos(2*2*2*pi*as.numeric(data_all$day)/(365.25*24*60*60))
data_all$sin12 = sin(2*pi*as.numeric(data_all$day)/(365.25*24*60*60))
data_all$sin6 = sin(2*2*pi*as.numeric(data_all$day)/(365.25*24*60*60))
data_all$sin3 = sin(2*2*2*pi*as.numeric(data_all$day)/(365.25*24*60*60))


data_all$dow = factor(weekdays(data_all$day), levels = 
                                 weekdays(seq(ISOdate(2000,1,3), len=7, by='days')))

data_all$dayInt = as.numeric(difftime(data_all$day, firstDay, units='days'))

data_all$hour = as.numeric(format(data_all$date, format='%H'))+1

data_all$pollutant <- as.numeric(factor(data_all$pollutant, levels = c("pm25","pm10","no2","o3")))
data_all$station <- as.numeric(factor(data_all$station, levels = as.character(id_index)))

# check the 99.5% for each pollutant, so that to use for h-cauchy distribution
cauchy_scale <-c(quantile(data_all$value[data_all$pollutant==1], na.rm=T, c(0.995)),
                 quantile(data_all$value[data_all$pollutant==2], na.rm=T, c(0.995)),
                 quantile(data_all$value[data_all$pollutant==3], na.rm=T, c(0.995)),
                 quantile(data_all$value[data_all$pollutant==4], na.rm=T, c(0.995)))

save(cauchy_scale, file =file.path(output_dir, "cauchy_scale.RData"))

# covariates for varying covariance
data_temp <- seq(as.Date(paste0(start_year,"-01-01")), as.Date(paste0(end_year,"-12-31")), by="day")

x_cov = data.frame(cos12=cos(2*pi*as.numeric(data_temp)/(365.25))
                   ,cos6=cos(2*2*pi*as.numeric(data_temp)/(365.25))
                   ,sin12=sin(2*pi*as.numeric(data_temp)/(365.25))
                   ,sin6=sin(2*2*pi*as.numeric(data_temp)/(365.25))
)

# add rho index column

data_all$rho_index <- 1
data_all$rho_index[year(data_all$date) > 2010] <- 2


# seperate data into censored or not, because the raw data set is rounded to integer 

data_allcens <- data_all[(data_all$value==0),]
data_all_noncens <- data_all[!(data_all$value==0),]

# get the estimated hour effects using robust method
factor_h <- array(NA, c(4,24))
coef_store <- NULL
for(i in 1:4)
{
  model <- glmrob(value ~ relevel(as.factor(hour),"12")+as.factor(dow)+cos12+cos6+cos3+sin12+sin6+sin3, data = data_all_noncens[data_all_noncens$pollutant==i,], family = Gamma(link="log"))
  factor_h[i,] <- c(summary(model)$coef[2:12,1],0, summary(model)$coef[13:24,1])
  coef_store=rbind(coef_store,(summary(model)$coef[c(1,31:36),1]))
}

save(factor_h, file =file.path(output_dir, "factor_h.RData"))
save(coef_store, file =file.path(output_dir, "coef_store.RData"))

data_all_noncens <- data_all_noncens[,c("value","pollutant","station","cos12", "cos6","cos3", "sin12", "sin6","sin3", "dow","dayInt","hour","rho_index")]

# build two columns to indicate outlier or not, daily complete or not.
data_all_noncens$dailyobs <- NA
data_all_noncens$outlierday <- NA

obtain_data_daily_comp <- function(data_fun)
{
  pdf(file = NULL)
  for(i_s in 1: max(data_fun$station))
  {
    
    stuff <- which(data_fun$station== i_s)
    if(length(stuff)!=0)
    {
      for(i_day in unique(data_fun$dayInt[stuff]))
      {
        print(paste(i_s,i_day))
        temp_index <- which(data_fun$station== i_s & data_fun$dayInt== i_day)
        
        if(length(temp_index)==24)
        {
          data_fun$dailyobs[temp_index] <- 24  
          if(length(boxplot(log(data_fun$value[temp_index]))$out)>0)
          {
            data_fun$outlierday[temp_index] <- 1  
          }
        }
      }
    }
  }
  dev.off()
  return(data_fun)
}

temp_dataall <- NULL


for(i_p in 1:4) # 
{
  temp_dataall <- rbind(temp_dataall, obtain_data_daily_comp(data_all_noncens[data_all_noncens$pollutant==i_p,]))
}

save(temp_dataall, file =file.path(output_dir, "temp_dataall.RData"))

data_all_noncens <- temp_dataall

data_all_noncens$value_weighted <- NA
for(i in 1:4)
{
  data_all_noncens$value_weighted[data_all_noncens$pollutant==i] <- data_all_noncens$value[data_all_noncens$pollutant==i]/exp(factor_h[i,data_all_noncens$hour[data_all_noncens$pollutant==i]])
}


data_daily_comp <- data_all_noncens[which(data_all_noncens$dailyobs==24 & is.na(data_all_noncens$outlierday)),]
data_allnorm <- data_all_noncens[-which((data_all_noncens$dailyobs==24 & is.na(data_all_noncens$outlierday))),]


#get the daily complete data the summaries used in stan

data_daily_comp$sumobs <- NA
data_daily_comp$sumlog <- NA

obtain_data_daily_summary <- function(data_fun)
{
  for(i_s in 1: max(data_fun$station))
  {
    for(i_day in unique(data_fun$dayInt))
    { print(paste(i_s,i_day))
      temp_index <- which(data_fun$station== i_s & data_fun$dayInt== i_day)
      if(length(temp_index)==24)
      {
        data_fun$sumobs[temp_index] <- sum((data_fun$value_weighted[temp_index]))
        data_fun$sumlog[temp_index] <- sum(log(data_fun$value_weighted[temp_index]))
      }
    }
  }
  return(data_fun)
}

temp_dataall_summary <- NULL

for(i_p in 1:4) # 
{
  temp_dataall_summary <- rbind(temp_dataall_summary, obtain_data_daily_summary(data_daily_comp[data_daily_comp$pollutant==i_p,]))
}

temp_dataall_summary <- temp_dataall_summary[temp_dataall_summary$hour==1,]

save(temp_dataall_summary, file =file.path(output_dir, "temp_dataall_summary.RData"))

data_daily_comp <- temp_dataall_summary

### for norm data set seperate into two: take out default 2 maximum 0 minimum out for each day to fit mixture model in stan.
obtain_data_daily_2max <- function(data_fun, leave_out_max=2, leave_out_min=0)
{
  hour_data <- NULL
  daily_data <- NULL
  for(i_s in 1: max(data_fun$station))
  {
    
    stuff <- which(data_fun$station== i_s)
    if(length(stuff)!=0)
    {
      for(i_day in unique(data_fun$dayInt[stuff]))
      {
        print(paste(i_s,i_day))
        temp_index <- which(data_fun$station== i_s & data_fun$dayInt== i_day)
        
        if(length(temp_index)>(leave_out_max+leave_out_min))
        {
          # data_fun$dailyobs[temp_index] <- length(temp_index) - leave_out_max - leave_out_min
          data_fun$dailyobs[temp_index] <- length(temp_index) - leave_out_max
          # min_index=max_index=NULL
          # if(leave_out_min>0)
          # {
          # min_index <- temp_index[order(data_fun$value[temp_index])][1:leave_out_min]
          # hour_data <- rbind(hour_data, data_fun[min_index,])
          # }
          # if(leave_out_max>0)
          # {
          max_index <- temp_index[order(data_fun$value[temp_index],decreasing = T)][1:leave_out_max]
          hour_data <- rbind(hour_data, data_fun[max_index,])
          # }
          # daily_data <- rbind(daily_data, data_fun[setdiff(temp_index, c(max_index, min_index)),])
          daily_data <- rbind(daily_data, data_fun[setdiff(temp_index, max_index),])
          
        }else{
          hour_data <- rbind(hour_data, data_fun[temp_index,])
        }
        
      }
    }
  }
  return(list(hour_data=hour_data, daily_data=daily_data))
}

hour_data <- NULL
daily_data <- NULL


for(i_p in 1:4) # 
{
  result=obtain_data_daily_2max(data_allnorm[data_allnorm$pollutant==i_p,])
  hour_data <- rbind(hour_data, result$hour_data)
  daily_data <- rbind(daily_data, result$daily_data)
}



daily_data$sumobs <- NA
daily_data$sumlog <- NA

obtain_data_daily_summary_non24 <- function(data_fun)
{
  compressed_data <- NULL
  for(i_s in 1: max(data_fun$station))
  {
    stuff <- which(data_fun$station== i_s)
    for(i_day in unique(data_fun$dayInt[stuff]))
    { 
      print(paste(i_s,i_day))
      temp_index <- which(data_fun$station== i_s & data_fun$dayInt== i_day)
      if(length(temp_index)!=0)
      {
        data_fun$sumobs[temp_index] <- sum((data_fun$value_weighted[temp_index]))
        data_fun$sumlog[temp_index] <- sum(log(data_fun$value_weighted[temp_index]))
        data_fun$dailyobs[temp_index] <- length(temp_index)
        compressed_data <- rbind(compressed_data, data_fun[temp_index[1],])
      }
    }
  }
  return(compressed_data)
}

compressed_daily <- NULL

for(i_p in 1:4) # 
{
  compressed_daily <- rbind(compressed_daily, obtain_data_daily_summary_non24(daily_data[daily_data$pollutant==i_p,]))
}

save(compressed_daily, file =file.path(output_dir, "compressed_daily.RData"))

data_daily_comp_final <- rbind(data_daily_comp, compressed_daily)

save(hour_data, file =file.path(output_dir, "hour_data.RData"))
#set the shard number
n_shards <- 8
M <- floor(nrow(hour_data)/n_shards)

stuff <- aggregate(dailyobs~rho_index+pollutant, data=data_daily_comp_final, sum)

stuff %>% tidyr::complete(rho_index,pollutant, fill=list(dailyobs=0)) %>% 
  pivot_wider(names_from = rho_index, values_from = dailyobs) %>% 
  dplyr::select(-pollutant)  %>%   as.matrix() -> sumcomplete


current_data <- list(N=nrow(hour_data), K=6, x=hour_data[,c("cos12", "cos6","cos3", "sin12", "sin6","sin3")], y=hour_data$value_weighted
                     , Nh=24, hour=hour_data$hour
                     , Nd=nrow(x_cov), day=hour_data$dayInt
                     , Nw=7, week=as.numeric(hour_data$dow)
                     , N_cens=nrow(data_allcens), censored_threshold=0.5
                     , x_cens=data_allcens[,c("cos12", "cos6","cos3", "sin12", "sin6","sin3")],hour_cens=data_allcens$hour
                     , day_cens=data_allcens$dayInt
                     , week_cens=as.numeric(data_allcens$dow)
                     , rho_index_cens=data_allcens$rho_index
                     ,x_cov=x_cov
                     ,N_cov=ncol(x_cov)
                     ,N_p=length(unique(data_all$pollutant))
                     ,N_s=length(unique(data_all$station))
                     ,station=as.numeric(hour_data$station)
                     ,pollutant=as.numeric(hour_data$pollutant)
                     ,station_cens=as.numeric(data_allcens$station)
                     ,pollutant_cens=as.numeric(data_allcens$pollutant)
                     ,n_shards=n_shards
                     ,M=M
                     # deal with daily complete data set
                     ,factor_h=as.matrix(factor_h)
                     , N_complete=nrow(data_daily_comp_final)
                     , x_complete=data_daily_comp_final[,c("cos12", "cos6","cos3", "sin12", "sin6","sin3")]
                     , day_complete=data_daily_comp_final$dayInt
                     , week_complete=as.numeric(data_daily_comp_final$dow)
                     ,station_complete=as.numeric(data_daily_comp_final$station)
                     ,pollutant_complete=as.numeric(data_daily_comp_final$pollutant)
                     ,sumobs=data_daily_comp_final$sumobs
                     ,sumlog=data_daily_comp_final$sumlog
                     ,N_rho=length(unique(hour_data$rho_index))
                     ,rho_index=hour_data$rho_index # this is true, as only this data set needs mixture structure
                     ,sumcomplete=sumcomplete
                     ,H_complete=data_daily_comp_final$dailyobs
                     
)

save(current_data, file =file.path(output_dir, "current_data.RData"))
# initial values
N_p=current_data$N_p
initf <- function() list(
  alpha=rep(4,N_p)
  ,mixture_rho=array(0.95,c(current_data$N_p, current_data$N_rho))
  ,miu=coef_store[,1]
  ,arcoef_phi=rep(0.3,N_p)
  ,betas=coef_store[,2:7]
  ,factor_w=array(0,c(N_p,7))
  ,factor_d=matrix(0, nrow=current_data$Nd, ncol = N_p)
  ,factor_q=matrix(0, nrow=current_data$N_s, ncol = N_p)
  ,factor_c=array(0, c(current_data$N_s,current_data$Nd,N_p))
)
save(initf, file =file.path(output_dir, "initf.RData"))

}
### fit model

# short version to run the model ------------------------------------------
library(rstan)
library(here)


load(file.path(output_dir,"initf.RData"))
load(file.path(output_dir,"coef_store.RData"))
load(file.path(output_dir,"current_data.RData"))
load(file.path(output_dir,"cauchy_scale.RData"))

# delete cos 3, sin 3
coef_store <- coef_store[,-c(4,7)]
current_data$K=4
current_data$x <- current_data$x[,-c(3,6)]
current_data$x_cens <- current_data$x_cens[,-c(3,6)]
current_data$x_complete <- current_data$x_complete[,-c(3,6)]

n_shards <- 4
current_data$n_shards <- n_shards
current_data$M <- 10000/n_shards
current_data$cauchy_scale <- as.numeric(cauchy_scale)

current_data$para_hour=floor(current_data$N/(current_data$M*n_shards));
current_data$para_cens=floor(current_data$N_cens/(current_data$M*n_shards));
current_data$para_comp=floor(current_data$N_complete/(current_data$M*n_shards));

current_data$cauchy_scale <- current_data$cauchy_scale/tan(pi/8)

N_p=current_data$N_p

options(mc.cores=parallel::detectCores())
Sys.setenv(STAN_NUM_THREADS=n_shards)

initf <- function() list(
  alpha=rep(2,N_p)+rnorm(N_p, mean=0, sd=0.01)
  ,mixture_rho=array(0.995+rnorm(8, mean=0, sd=0.001),c(current_data$N_p, current_data$N_rho))
  ,miu=coef_store[,1]+rnorm(N_p, mean=0, sd=0.01)
  ,arcoef_phi=rep(0.3,N_p)+rnorm(N_p, mean=0, sd=0.01)
  # ,betas=coef_store[,2:7]+rnorm(N_p*ncol(coef_store[,2:7]), mean=0, sd=0.01) # as we do not use cos sin 3
  ,betas=coef_store[,2:5]+rnorm(N_p*ncol(coef_store[,2:5]), mean=0, sd=0.01)
  ,factor_w_pre=array(0,c(N_p,6))+rnorm(N_p*6, mean=0, sd=0.01)
  ,factor_d=matrix(0, nrow=current_data$Nd, ncol = N_p)+rnorm(current_data$Nd*N_p, mean=0, sd=0.01)
  ,factor_q=matrix(0, nrow=current_data$N_s, ncol = N_p)+rnorm(current_data$N_s*N_p, mean=0, sd=0.01)
  ,factor_c=array(0, c(current_data$N_s,current_data$Nd,N_p))+rnorm(current_data$N_s*current_data$Nd*N_p, mean=0, sd=0.01)
)

# add time trend needed data
current_data$trend_reference_day <- 2923 # which uses the Jan 1st, 2004 as the reference day. 

# fit.code <- stanc(here::here("Model","JASA_pollution_model.stan")) # convert to C++ code
# fit.model <- stan_model(stanc_ret = fit.code) # compile C++ code
load("~/NEW_pollution/fit.model.RData")
rstan_options(auto_write = TRUE)


# Sample from Stan model
fit1 <- rstan::sampling(fit.model, data=current_data,chains = 1, iter = 1000,
                        control = list(max_treedepth = 10, adapt_delta=0.8 ,stepsize_jitter=0.6)
                        ,seed = chain_seed
                        , pars =c("betas", "miu","alpha","mixture_rho","factor_w","factor_q","factor_d", "kappa", "G", "arcoef_phi","tau","diaggamma","miu_cov","betas_cov","factor_c_save","L_save","factor_trend")
                        , verbose=T
                        # ,diagnostic_file="diagnostic_file.csv"
                        ,init = initf)

save(fit1, file=file.path(output_dir,paste0(save_chain_name,".RData")))

}

fit_JASA(start_year=2000,
         end_year=2019,
         cityname="Winnipeg",
         # id_index,
         id_index_low = 70101,
         id_index_high = 70199,
         chain_seed=100,
         save_chain_name="newchain1",
         firstChain=TRUE
)

# fit_JASA(start_year=2000,
#          end_year=2019,
#          cityname="Windsor",
#          # id_index,
#          id_index_low = 60201,
#          id_index_high = 60299,
#          chain_seed=100,
#          save_chain_name="newchain1",
#          firstChain=TRUE
# )

# fit_JASA(start_year=2000,
#          end_year=2019,
#          cityname="Montreal",
#          # id_index,
#          id_index_low = 50101,
#          id_index_high = 50199,
#          chain_seed=100,
#          save_chain_name="newchain1",
#          firstChain=TRUE
# )

# fit_JASA(start_year=2000,
#          end_year=2019,
#          cityname="Hamilton",
#          # id_index,
#          id_index_low = 60501,
#          id_index_high = 60599,
#          chain_seed=100,
#          save_chain_name="newchain1",
#          firstChain=TRUE
# )

# fit_JASA(start_year=2000,
#          end_year=2019,
#          cityname="Edmonton",
#          # id_index,
#          id_index_low = 90100,
#          id_index_high = 90199,
#          chain_seed=200,
#          save_chain_name="newchain2",
#          firstChain=FALSE
# )

# fit_JASA(start_year=2000,
#          end_year=2019,
#          cityname="Edmonton",
#          # id_index,
#          id_index_low = 90100,
#          id_index_high = 90199,
#          chain_seed=100,
#          save_chain_name="newchain1",
#          firstChain=TRUE
# )


# fit_JASA(start_year=2000,
#          end_year=2019,
#          cityname="Vancouver",
#          # id_index,
#          id_index_low = 100100,
#          id_index_high = 100199,
#          chain_seed=200,
#          save_chain_name="newchain2",
#          firstChain=FALSE
# )

# fit_JASA(start_year=2000,
#          end_year=2019,
#          cityname="Vancouver",
#          # id_index,
#          id_index_low = 100100,
#          id_index_high = 100199,
#          chain_seed=100,
#          save_chain_name="newchain1",
#          firstChain=TRUE
# )

# fit_JASA(start_year=2000,
#          end_year=2019,
#          cityname="Toronto",
#          # id_index,
#          citysearchkeyword="oronto",
#          chain_seed=200,
#          save_chain_name="newchain2",
#          firstChain=FALSE
# )

# fit_JASA(start_year=2000,
#          end_year=2019,
#          cityname="Toronto",
#          # id_index,
#          citysearchkeyword="oronto",
#          chain_seed=100,
#          save_chain_name="newchain1",
#          firstChain=TRUE
# )

# fit_JASA(start_year=2000,
#                      end_year=2019,
#                      cityname="Calgary",
#                      # id_index,
#                      citysearchkeyword="algary",
#                      chain_seed=300,
#                      save_chain_name="newchain2",
#                      firstChain=FALSE
# )

# fit_JASA(start_year=2000,
#                      end_year=2019,
#                      cityname="Calgary",
#                      # id_index,
#                      citysearchkeyword="algary",
#                      chain_seed=100,
#                      save_chain_name="newchain1",
#                      firstChain=TRUE
# )

## the above is new 2021








#####################################

# cityname <- "HalifaxNewSaintJ"
# id_index <- c(30118,30120,  40203,40206,40207) #
# 
# chain_seed <- 300  # 200, 300, 400 for other chains
# save_chain_name <- "newchain2"
# 
# fit_JASA(start_year=1996,
#          end_year=2012,
#          cityname=cityname,
#          id_index=id_index,
#          chain_seed=chain_seed,
#          save_chain_name=save_chain_name,
#          firstChain=F)


# cityname <- "SudburySaultSM"
# id_index <- c(60607,  60707,60708,60709) #
# 
# chain_seed <- 100  # 200, 300, 400 for other chains
# save_chain_name <- "newchain1"
# 
# fit_JASA(start_year=1996,
#          end_year=2012,
#          cityname=cityname,
#          id_index=id_index,
#          chain_seed=chain_seed,
#          save_chain_name=save_chain_name,
#          firstChain=TRUE)

# cityname <- "DurYorPeeHalWatNia"
# id_index <- c(61701,61702,   63201,65101,  60415,60428,60432,60434,   61602,63001,61603,  61502,61501,  61302) #
# 
# chain_seed <- 200  # 200, 300, 400 for other chains
# save_chain_name <- "newchain2"
# 
# fit_JASA(start_year=1996,
#          end_year=2012,
#          cityname=cityname,
#          id_index=id_index,
#          chain_seed=chain_seed,
#          save_chain_name=save_chain_name,
#          firstChain=FALSE)

# cityname <- "WinnipegReginaSaskatoon"
# id_index <- c(70118, 70119, 80110, 80211) # the first two is for winnipeg, then regina, saskatoon
# 
# chain_seed <- 200  # 200, 300, 400 for other chains
# save_chain_name <- "newchain2"
# 
# fit_JASA(start_year=1996,
#          end_year=2012,
#          cityname=cityname,
#          id_index=id_index,
#          chain_seed=chain_seed,
#          save_chain_name=save_chain_name,
#          firstChain=FALSE)

# cityname <- "SaintJohn"
# id_index <- c(40501, 40206, 40203, 40207, 40208)
# 
# chain_seed <- 100  # 200, 300, 400 for other chains
# save_chain_name <- "newchain1"
# firstChain <- FALSE
# 
# fit_JASA(start_year=1996,
#          end_year=2012,
#          cityname=cityname,
#          id_index=id_index,
#          chain_seed=chain_seed,
#          save_chain_name=save_chain_name,
#          firstChain=FALSE)


# cityname <- "Ottawa"
# id_index <- c(52401, 50203, 60101, 60104, 60106, 50109) # 50109 is from montreal to help with PM10
# 
# chain_seed <- 600  # 200, 300, 400 for other chains
# save_chain_name <- "newchain4"
# firstChain <- FALSE
# 
# fit_JASA(start_year=1996,
#          end_year=2012,
#          cityname=cityname,
#          id_index=id_index,
#          chain_seed=chain_seed,
#          save_chain_name=save_chain_name,
#          firstChain=FALSE)

# cityname <- "Quebec"
# id_index <- c(50309, 50306, 50307, 50308, 50310, 50311) 
# 
# chain_seed <- 100  # 200, 300, 400 for other chains
# save_chain_name <- "newchain1"
# 
# fit_JASA(start_year=1996,
#          end_year=2012,
#          cityname=cityname,
#          id_index=id_index,
#          chain_seed=chain_seed,
#          save_chain_name=save_chain_name,
#          firstChain=TRUE)

# cityname <- "Montreal"
# id_index <- c(50102:50105, 50109,50110, 50115, 50116, 50120, 50123, 50126, 50128, 50129, 50130) 
# 
# chain_seed <- 200  # 200, 300, 400 for other chains
# save_chain_name <- "newchain2"
# 
# fit_JASA(start_year=1996,
#          end_year=2012,
#          cityname=cityname,
#          id_index=id_index,
#          chain_seed=chain_seed,
#          save_chain_name=save_chain_name,
#          firstChain=FALSE)




