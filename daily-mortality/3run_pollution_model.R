
# short version to run the model ------------------------------------------
library(rstan)
library(here)
setwd(here("Model","Toronto1996_2012","fit model"))


load("initf.RData")
load("coef_store.RData")
load("current_data.RData")
load("cauchy_scale.RData")

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

fit.code <- stanc(file = "JASA_pollution_model.stan") # convert to C++ code
fit.model <- stan_model(stanc_ret = fit.code) # compile C++ code
rstan_options(auto_write = TRUE)


# Sample from Stan model
fit1 <- rstan::sampling(fit.model, data=current_data,chains = 1, iter = 1000,
                         control = list(max_treedepth = 10, adapt_delta=0.8 ,stepsize_jitter=0.6)
                         ,seed = 100 # 200, 300, 400 for other chains
                        , pars =c("betas", "miu","alpha","mixture_rho","factor_w","factor_q","factor_d", "kappa", "G", "arcoef_phi","tau","diaggamma","miu_cov","betas_cov","factor_c_save","L_save","factor_trend")
                        , verbose=T
                         # ,diagnostic_file="diagnostic_file.csv"
                         ,init = initf)

save(fit1, file="newchain1.RData")


##############################using the all cos 12, 6, 3, sin 12, 6,3
# # short version to run the model ------------------------------------------
# library(rstan)
# 
# load("initf.RData")
# load("coef_store.RData")
# load("current_data.RData")
# load("cauchy_scale.RData")
# 
# 
# n_shards <- 8
# current_data$n_shards <- n_shards
# current_data$M <- 10000/n_shards
# current_data$cauchy_scale <- as.numeric(cauchy_scale)
# 
# current_data$para_hour=floor(current_data$N/(current_data$M*n_shards));
# current_data$para_cens=floor(current_data$N_cens/(current_data$M*n_shards));
# current_data$para_comp=floor(current_data$N_complete/(current_data$M*n_shards));
# 
# current_data$cauchy_scale <- current_data$cauchy_scale/tan(pi/8)
# 
# N_p=current_data$N_p
# 
# options(mc.cores=parallel::detectCores())
# Sys.setenv(STAN_NUM_THREADS=n_shards)
# 
# initf <- function() list(
#   alpha=rep(2,N_p)+rnorm(N_p, mean=0, sd=0.01)
#   ,mixture_rho=array(0.995+rnorm(8, mean=0, sd=0.001),c(current_data$N_p, current_data$N_rho))
#   ,miu=coef_store[,1]+rnorm(N_p, mean=0, sd=0.01)
#   ,arcoef_phi=rep(0.3,N_p)+rnorm(N_p, mean=0, sd=0.01)
#   ,betas=coef_store[,2:7]+rnorm(N_p*ncol(coef_store[,2:7]), mean=0, sd=0.01)
#   ,factor_w_pre=array(0,c(N_p,6))+rnorm(N_p*6, mean=0, sd=0.01)
#   ,factor_d=matrix(0, nrow=current_data$Nd, ncol = N_p)+rnorm(current_data$Nd*N_p, mean=0, sd=0.01)
#   ,factor_q=matrix(0, nrow=current_data$N_s, ncol = N_p)+rnorm(current_data$N_s*N_p, mean=0, sd=0.01)
#   ,factor_c=array(0, c(current_data$N_s,current_data$Nd,N_p))+rnorm(current_data$N_s*current_data$Nd*N_p, mean=0, sd=0.01)
# )
# 
# fit.code <- stanc(file = "Airpollutionmodel20190125weightedcompressedalldataparalliseall_saveneeded_10000parallised.stan") # convert to C++ code
# fit.model <- stan_model(stanc_ret = fit.code) # compile C++ code
# rstan_options(auto_write = TRUE)
# 
# 
# # Sample from Stan model
# fit1 <- rstan::sampling(fit.model, data=current_data,chains = 1, iter = 700,
#                          control = list(max_treedepth = 10, adapt_delta=0.6 ,stepsize_jitter=0.6)
#                          ,seed = 100
#                         , pars =c("betas", "miu","alpha","mixture_rho","factor_w","factor_q","factor_d", "kappa", "G", "arcoef_phi","tau","diaggamma","miu_cov","betas_cov","factor_c_save","L_save")
#                         , verbose=T
#                          # ,diagnostic_file="diagnostic_file.csv"
#                          ,init = initf)
# 
# save(fit1, file="newchain1.RData")
# 
# 
# 



