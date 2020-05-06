library(rstan)
library(robustbase)
library(data.table)
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

load("newchain2.RData")
fit2 <- fit1
load("newchain3.RData")
fit <- sflist2stanfit(list(fit1, fit2))

list_of_draws <- extract(fit)
remove(fit)
remove(fit1)
remove(fit2)
remove(stuff1)
remove(stuff2)
remove(stuff3)
remove(stuff4)
remove(data_all_toronto)

All_data$rho_index <- 1
All_data$rho_index[year(All_data$date) > 2003] <- 2


TOTAL_DATA <- All_data

load("factor_h.RData")
load("cauchy_scale.RData")
cauchy_scale <- as.numeric(cauchy_scale)/tan(pi/8)

load('trend_reference_day.RData')


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
    
    # pred_data$cos12_new[pred_data$pollutant==j] <- pred_data$cos12[pred_data$pollutant==j]*beta_p[j,1]
    # pred_data$cos6_new[pred_data$pollutant==j] <-  pred_data$cos6[pred_data$pollutant==j]*beta_p[j,2]
    # pred_data$cos3_new[pred_data$pollutant==j] <-  pred_data$cos3[pred_data$pollutant==j]*beta_p[j,3]
    # pred_data$sin12_new[pred_data$pollutant==j] <- pred_data$sin12[pred_data$pollutant==j]*beta_p[j,4]
    # pred_data$sin6_new[pred_data$pollutant==j] <-  pred_data$sin6[pred_data$pollutant==j]*beta_p[j,5]
    # pred_data$sin3_new[pred_data$pollutant==j] <-  pred_data$sin3[pred_data$pollutant==j]*beta_p[j,6]
    
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
  
  save(mixed_complete, file = paste0("mixed_complete_",station, "_",pollutant ,".RData"))
  save(sample_store, file = paste0("sample_store_",station, "_",pollutant ,".RData"))
  save(prob_store, file = paste0("prob_store_",station, "_",pollutant ,".RData"))
  
}

set.seed(100)
sample_size=50
sample_index <- sample(x=1:nrow(list_of_draws$tau), size=sample_size)

for(station in 1:11)
{
  for (pollutant in c(1,3,4)) 
  {
    print(paste("station:",station,"pollutant:", pollutant))
    get_prediction(station = station, pollutant = pollutant, sample_size = sample_size)
  }
}




