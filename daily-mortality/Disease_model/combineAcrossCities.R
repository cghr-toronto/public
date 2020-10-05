
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

fit.code <- stanc(file = file.path(path_fitmodel_Manuscript,"MCMCcombine.stan")) # convert to C++ code
fit.model <- stan_model(stanc_ret = fit.code) # compile C++ code
rstan_options(auto_write = TRUE)

for(disease_type in c("mort_all","mort_circ","mort_pulm","morb_all","morb_circ","morb_pulm"))
{
  n_sample_index <- 50
  cityCombine <- vector(mode='list', length=n_sample_index)
  
  for(sample_index in 1:n_sample_index)
  {
    print(sample_index)
    
    for (city in c("Toronto", "Vancouver","Calgary","Edmonton"))
    {
      load(file.path(path_fitmodel_Manuscript,"all_clogit_model_results",paste0(city,"_",disease_type,"_",sample_index,".RData")))
    }
    #
    Toronto_disease_model <- get(eval(paste0("Toronto_",disease_type)))
    Vancouver_disease_model <- get(eval(paste0("Vancouver_",disease_type)))
    Calgary_disease_model <- get(eval(paste0("Calgary_",disease_type)))
    Edmonton_disease_model <- get(eval(paste0("Edmonton_",disease_type)))
    
    betaKhat <- rbind(
      Toronto_disease_model$coefficients[1:3],
      Vancouver_disease_model$coefficients[1:3],
      Calgary_disease_model$coefficients[1:3],
      Edmonton_disease_model$coefficients[1:3]
    )
    SigmaKhat <- array(NA, c(4,3,3))
    SigmaKhat[1,,] <- Toronto_disease_model$var[1:3,1:3]
    SigmaKhat[2,,] <- Vancouver_disease_model$var[1:3,1:3]
    SigmaKhat[3,,] <- Calgary_disease_model$var[1:3,1:3]
    SigmaKhat[4,,] <- Edmonton_disease_model$var[1:3,1:3]
    
    options(mc.cores=parallel::detectCores())
    
    current_data <- list(P=3,
                         K=4
                         ,betaKhat=as.matrix(betaKhat)
                         ,SigmaKhat=SigmaKhat)
    
    set.seed(Sys.time())
    inits <- function()list(
      kappa= abs(rnorm(1, sd=0.01)),
      betaK= t(array(rep(apply(betaKhat,2,mean),4),c(3,4)))+rnorm(12,sd=0.001),
      betaBar=apply(betaKhat,2,mean)+rnorm(3,sd=0.001)
    )
    
    # Sample from Stan model
    fit1 <- rstan::sampling(fit.model, data=current_data,chains = 1, iter = 50000, warmup=30000, thin =2000, 
                            control = list(max_treedepth = 10, adapt_delta=0.95 ,stepsize_jitter=0.6)
                            , pars =c("betaK", "G","betaBar","kappa")
                            , verbose=T
                            ,init = inits
    )
    
    # save(fit1, file="newchain1.RData")
    cityCombine[[sample_index]] <- extract(fit1)
    
  }
  betaBarSample <-R.utils::wrap(simplify2array( mapply(function(x)x$betaBar, cityCombine, SIMPLIFY = FALSE)), map=list(NA,2))
  betaKSample <-R.utils::wrap(simplify2array( mapply(function(x)x$betaK, cityCombine, SIMPLIFY = FALSE)), map=list(NA,3,2))
  kappaSample <- c(mapply(function(x)x$kappa, cityCombine))
  
  cityCombineSample <- list(betaBarSample=betaBarSample, betaKSample=betaKSample, kappaSample=kappaSample)
  save(cityCombineSample, file=file.path(path_fitmodel_Manuscript,paste0("StancityComb",disease_type,".RData")))
}
