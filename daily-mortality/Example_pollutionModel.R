library(rstan)
library(robustbase)
library(data.table)
library(here)

######################################### load data #####################################
# Please define your output directory, output_dir
output_dir <- "/store/guowen/project_CGHR/Paper/JASA/gitHubCode"

load(file.path(output_dir, "data_all_toronto.RData"))

######################################### get model input format #####################################

start_year <- 1996
end_year <- 2012

data_all_toronto$date = strptime(
  as.character(data_all_toronto$date), format = '%Y-%m-%d %H:%M:%S',
  tz='UTC'
)

# the firstDay is used for day difference calculate
firstDay = as.Date(paste0(start_year-1,"-12-31"))
endDay = as.Date(paste0(end_year,"-12-31"))

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

# check the 99.5% for each pollutant, so that to use for h-cauchy distribution
cauchy_scale <-c(quantile(data_all_toronto$value[data_all_toronto$pollutant==1], na.rm=T, c(0.995)),
                 quantile(data_all_toronto$value[data_all_toronto$pollutant==2], na.rm=T, c(0.995)),
                 quantile(data_all_toronto$value[data_all_toronto$pollutant==3], na.rm=T, c(0.995)),
                 quantile(data_all_toronto$value[data_all_toronto$pollutant==4], na.rm=T, c(0.995)))

save(cauchy_scale, file =file.path(output_dir, "cauchy_scale.RData"))

# covariates for varying covariance
data_temp <- seq(as.Date(paste0(start_year,"-01-01")), as.Date(paste0(end_year,"-12-31")), by="day")

x_cov = data.frame(cos12=cos(2*pi*as.numeric(data_temp)/(365.25))
                   ,cos6=cos(2*2*pi*as.numeric(data_temp)/(365.25))
                   ,sin12=sin(2*pi*as.numeric(data_temp)/(365.25))
                   ,sin6=sin(2*2*pi*as.numeric(data_temp)/(365.25))
)

# add rho index column

data_all_toronto$rho_index <- 1
data_all_toronto$rho_index[year(data_all_toronto$date) > 2003] <- 2


# seperate data into censored or not, because the raw data set is rounded to integer 

data_all_torontocens <- data_all_toronto[(data_all_toronto$value==0),]
data_all_toronto_noncens <- data_all_toronto[!(data_all_toronto$value==0),]

# get the estimated hour effects using robust method
factor_h <- array(NA, c(4,24))
coef_store <- NULL
for(i in 1:4)
{
  model <- glmrob(value ~ relevel(as.factor(hour),"12")+as.factor(dow)+cos12+cos6+cos3+sin12+sin6+sin3, data = data_all_toronto_noncens[data_all_toronto_noncens$pollutant==i,], family = Gamma(link="log"))
  factor_h[i,] <- c(summary(model)$coef[2:12,1],0, summary(model)$coef[13:24,1])
  coef_store=rbind(coef_store,(summary(model)$coef[c(1,31:36),1]))
}

save(factor_h, file =file.path(output_dir, "factor_h.RData"))
save(coef_store, file =file.path(output_dir, "coef_store.RData"))

data_all_toronto_noncens <- data_all_toronto_noncens[,c("value","pollutant","station","cos12", "cos6","cos3", "sin12", "sin6","sin3", "dow","dayInt","hour","rho_index")]

# build two columns to indicate outlier or not, daily complete or not.
data_all_toronto_noncens$dailyobs <- NA
data_all_toronto_noncens$outlierday <- NA

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
  temp_dataall <- rbind(temp_dataall, obtain_data_daily_comp(data_all_toronto_noncens[data_all_toronto_noncens$pollutant==i_p,]))
}

save(temp_dataall, file =file.path(output_dir, "temp_dataall.RData"))

data_all_toronto_noncens <- temp_dataall

data_all_toronto_noncens$value_weighted <- NA
for(i in 1:4)
{
  data_all_toronto_noncens$value_weighted[data_all_toronto_noncens$pollutant==i] <- data_all_toronto_noncens$value[data_all_toronto_noncens$pollutant==i]/exp(factor_h[i,data_all_toronto_noncens$hour[data_all_toronto_noncens$pollutant==i]])
}


data_daily_comp <- data_all_toronto_noncens[which(data_all_toronto_noncens$dailyobs==24 & is.na(data_all_toronto_noncens$outlierday)),]
data_all_torontonorm <- data_all_toronto_noncens[-which((data_all_toronto_noncens$dailyobs==24 & is.na(data_all_toronto_noncens$outlierday))),]


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
  result=obtain_data_daily_2max(data_all_torontonorm[data_all_torontonorm$pollutant==i_p,])
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
                     , N_cens=nrow(data_all_torontocens), censored_threshold=0.5
                     , x_cens=data_all_torontocens[,c("cos12", "cos6","cos3", "sin12", "sin6","sin3")],hour_cens=data_all_torontocens$hour
                     , day_cens=data_all_torontocens$dayInt
                     , week_cens=as.numeric(data_all_torontocens$dow)
                     , rho_index_cens=data_all_torontocens$rho_index
                     ,x_cov=x_cov
                     ,N_cov=ncol(x_cov)
                     ,N_p=length(unique(data_all_toronto$pollutant))
                     ,N_s=length(unique(data_all_toronto$station))
                     ,station=as.numeric(hour_data$station)
                     ,pollutant=as.numeric(hour_data$pollutant)
                     ,station_cens=as.numeric(data_all_torontocens$station)
                     ,pollutant_cens=as.numeric(data_all_torontocens$pollutant)
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

######################################### fit model #####################################

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

fit.code <- stanc(file.path(output_dir, "JASA_pollution_model.stan")) # convert to C++ code
fit.model <- stan_model(stanc_ret = fit.code) # compile C++ code
rstan_options(auto_write = TRUE)


# Sample from Stan model
fit1 <- rstan::sampling(fit.model, data=current_data,chains = 1, iter = 1000,
                        control = list(max_treedepth = 10, adapt_delta=0.8 ,stepsize_jitter=0.6)
                        ,seed = 100
                        , pars =c("betas", "miu","alpha","mixture_rho","factor_w","factor_q","factor_d", "kappa", "G", "arcoef_phi","tau","diaggamma","miu_cov","betas_cov","factor_c_save","L_save","factor_trend")
                        , verbose=T
                        # ,diagnostic_file="diagnostic_file.csv"
                        ,init = initf)

fit2 <- rstan::sampling(fit.model, data=current_data,chains = 1, iter = 1000,
                        control = list(max_treedepth = 10, adapt_delta=0.8 ,stepsize_jitter=0.6)
                        ,seed = 200
                        , pars =c("betas", "miu","alpha","mixture_rho","factor_w","factor_q","factor_d", "kappa", "G", "arcoef_phi","tau","diaggamma","miu_cov","betas_cov","factor_c_save","L_save","factor_trend")
                        , verbose=T
                        # ,diagnostic_file="diagnostic_file.csv"
                        ,init = initf)


######################################### get prediction #####################################

library(rstan)
library(robustbase)
library(data.table)
library(lubridate)
library(dplyr)

library(sp)
library(rgdal)
library(here)


  load(file.path(output_dir,"data_all_toronto.RData"))
  
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
  
  All_data <- dplyr::left_join(All_data, data_all, by=c("pollutant","station","date"))
  
  All_data$date = strptime(
    as.character(All_data$date), format = '%Y-%m-%d %H:%M:%S',
    tz='UTC'
  )
  
  
  All_data$day = trunc(All_data$date, units='days')
  
  All_data$cos12 = cos(2*pi*as.numeric(All_data$day)/(365.25*24*60*60))
  All_data$cos6 = cos(2*2*pi*as.numeric(All_data$day)/(365.25*24*60*60))
  All_data$cos3 = cos(2*2*2*pi*as.numeric(All_data$day)/(365.25*24*60*60))
  All_data$sin12 = sin(2*pi*as.numeric(All_data$day)/(365.25*24*60*60))
  All_data$sin6 = sin(2*2*pi*as.numeric(All_data$day)/(365.25*24*60*60))
  All_data$sin3 = sin(2*2*2*pi*as.numeric(All_data$day)/(365.25*24*60*60))
  
  
  All_data$dow = factor(weekdays(All_data$day), levels = 
                          weekdays(seq(ISOdate(2000,1,3), len=7, by='days')))
  
  All_data$dow = as.numeric(All_data$dow)
  
  All_data$dayInt = as.numeric(difftime(All_data$day, firstDay, units='days'))
  
  All_data$hour = as.numeric(format(All_data$date, format='%H'))+1
  

fit <- sflist2stanfit(list(fit1, fit2))
 
  list_of_draws <- rstan::extract(fit)

  
  All_data$rho_index <- 1
  All_data$rho_index[year(All_data$date) > 2003] <- 2
  
  
  TOTAL_DATA <- All_data
  
  load(file.path(output_dir,"factor_h.RData"))
  load(file.path(output_dir,"cauchy_scale.RData"))
  cauchy_scale <- as.numeric(cauchy_scale)/tan(pi/8)
  
  trend_reference_day <- 2923
  
  
  get_prediction <- function(station, pollutant,sample_size)
  {
    
    n <- sample_size
    
    # predict one station
    All_data <- TOTAL_DATA[TOTAL_DATA$station==station & TOTAL_DATA$pollutant==pollutant,]
    
    sample_store <- array(NA,c(nrow(All_data),n))
    sample_store2 <- array(NA,c(nrow(All_data),n))
    prob_store <- array(NA,c(nrow(All_data),n))
    prob_store2 <- array(NA,c(nrow(All_data),n))
    
    for( i in 1:n)
    {
      print(i)
      jj <- sample_index[i]
      #get shape
      alpha_p = list_of_draws$alpha[jj,]
      #get mixture proportion
      rho_p = list_of_draws$mixture_rho[jj,,]
      miu_p = list_of_draws$miu[jj,]
      
      Q_sp =list_of_draws$factor_q[jj,,]
      D_tp =list_of_draws$factor_d[jj,,]
      
      C_stp_new=list_of_draws$factor_c[jj,,,]
      C_stp <- array(NA, dim(C_stp_new)+c(0,0,1))
      C_stp[,,1] <- C_stp_new[,,1]
      C_stp[,,2] <- C_stp_new[,,2]
      C_stp[,,3] <- C_stp_new[,,2]
      C_stp[,,4] <- C_stp_new[,,3]
      
      H_sph=factor_h # hiden in design matrix in the model, but in stan code, it is seperated
      W_spw=list_of_draws$factor_w[jj,,]  # hiden in design matrix in the model, but in stan code, it is seperated
      
      beta_p = list_of_draws$betas[jj,,]
      
      time_trend <- list_of_draws$factor_trend[jj,]
      
      #get eta
      pred_data <- All_data
      pred_data$miu=
        pred_data$Q=
        pred_data$D=
        pred_data$C=
        pred_data$H=
        pred_data$W=
        pred_data$cos12_new=
        pred_data$cos6_new=
        pred_data$cos3_new=
        pred_data$sin12_new=
        pred_data$sin6_new=
        pred_data$sin3_new=
        pred_data$alpha=
        pred_data$rho=
        pred_data$time_trend=NA    ### need to be very carefull for the order. ok, not matters, will fix the following code, to not rely on order
      
      j=pollutant #  4 pollutants
      
      pred_data$alpha[pred_data$pollutant==j] = alpha_p[j]
      pred_data$time_trend[pred_data$pollutant==j] = ((pred_data$dayInt[pred_data$pollutant==j]-trend_reference_day)/3652.5)*time_trend[j]
      
      for(kk in 1:length(unique(TOTAL_DATA$rho_index)))
      {
        pred_data$rho[pred_data$pollutant==j & pred_data$rho_index==kk] = rho_p[j,kk]
      }
      
      pred_data$miu[pred_data$pollutant==j] = miu_p[j]
      
      pred_data$cos12_new[pred_data$pollutant==j] <- pred_data$cos12[pred_data$pollutant==j]*beta_p[j,1]
      pred_data$cos6_new[pred_data$pollutant==j] <-  pred_data$cos6[pred_data$pollutant==j]*beta_p[j,2]
      pred_data$sin12_new[pred_data$pollutant==j] <- pred_data$sin12[pred_data$pollutant==j]*beta_p[j,3]
      pred_data$sin6_new[pred_data$pollutant==j] <-  pred_data$sin6[pred_data$pollutant==j]*beta_p[j,4]
      
      for(t in 1: max(pred_data$dayInt))
      {
        
        pred_data$D[pred_data$dayInt==t& pred_data$pollutant==j] = D_tp[t,j]
      }
      
      #### 
      k=station
      pred_data$Q[pred_data$station==k & pred_data$pollutant==j] = Q_sp[k,j]
      
      
      for(t in 1: max(pred_data$dayInt))
      {
        pred_data$C[pred_data$station==k & pred_data$dayInt==t & pred_data$pollutant==j] = C_stp[k,t,j]
        # pred_data$C[pred_data$station==k & pred_data$dayInt==t & pred_data$pollutant==j] = 0
      }
      
      for(h in 1:24)
      {
        pred_data$H[pred_data$pollutant==j & pred_data$hour==h] = H_sph[j, h]
      }
      
      for(w in 1:7)
      {
        pred_data$W[pred_data$pollutant==j & pred_data$dow==w] = W_spw[j, w]
      }
      
      
      
      
      # pred_data$eta <- apply(pred_data[, c("miu", "Q","D","C","H","W","cos12_new","cos6_new","cos3_new","sin12_new","sin6_new","sin3_new")],1,sum)
      pred_data$eta <- apply(pred_data[, c("miu", "Q","D","C","H","W","cos12_new","cos6_new","sin12_new","sin6_new","time_trend")],1,sum)
      pred_data$beta <- pred_data$alpha/exp(pred_data$eta)
      
      # get the probability given the observation
      pred_data$sample <- NA
      
      pred_data$sample <- rgamma(n=nrow(pred_data),pred_data$alpha,pred_data$beta)
      
      pred_data$prob <- (dgamma(pred_data$value, shape = pred_data$alpha, rate=pred_data$beta)*pred_data$rho)/(dgamma(pred_data$value, shape = pred_data$alpha, rate=pred_data$beta)*pred_data$rho
                                                                                                               + (1-pred_data$rho)*2*dcauchy(pred_data$value, scale = cauchy_scale[j]))
      
      # if the observation is 0, then integrate for gamma and divided by two for cauchy
      census_index <- (which(!is.na(pred_data$value) & pred_data$value<1))
      
      
      pred_data$prob[census_index] <- (pgamma(1, shape = pred_data$alpha[census_index], rate=pred_data$beta[census_index])*pred_data$rho[census_index] )/(pgamma(1, shape = pred_data$alpha[census_index], rate=pred_data$beta[census_index])*pred_data$rho[census_index] 
                                                                                                                                                          + (1-pred_data$rho[census_index])*2*(pcauchy(0.5, scale = cauchy_scale[j])-0.5))
      
      sample_store[,i] <- pred_data$sample
      prob_store[,i] <- pred_data$prob
    }
    mixed_complete <- sample_store
    mixed_complete[!is.na(All_data$value),] <- All_data$value[!is.na(All_data$value)]
    
    save(mixed_complete, file = file.path(output_dir,paste0("mixed_complete_",station, "_",pollutant ,".RData")))
    save(sample_store, file = file.path(output_dir,paste0("sample_store_",station, "_",pollutant ,".RData")))
    save(prob_store, file = file.path(output_dir,paste0("prob_store_",station, "_",pollutant ,".RData")))
    
  }
  
  set.seed(100)
  sample_size=50
  sample_index <- sample(x=1:nrow(list_of_draws$tau), size=sample_size)
  
  for(station in 1:length(unique(All_data$station)))
  {
    for (pollutant in c(1,3,4)) 
    {
      print(paste("station:",station,"pollutant:", pollutant))
      get_prediction(station = station, pollutant = pollutant, sample_size = sample_size)
    }
  }
  
  # get daily summaries
  # exclude the hourly outliers
  
  pollutantVar <- c("pm25", "pm10","no2","o3")
  for (j in c(1,3,4))
  {
    mixed_no_outlier <- NULL
    for(i in setdiff(1:length(unique(All_data$station)), excludeStnIndex))
    {
      load(file.path(output_dir, paste0("mixed_complete_",i,"_",j,".RData")))
      load(file.path(output_dir, paste0("sample_store_",i,"_",j,".RData")))
      load(file.path(output_dir, paste0("prob_store_",i,"_",j,".RData")))
      
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
    save(dailymean, file = file.path(output_dir,paste0(pollutantVar[j],"dailymeansample.RData")))
  }
  





