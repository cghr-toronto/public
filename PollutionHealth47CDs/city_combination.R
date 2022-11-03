library(rstan)
library(dplyr)

fit.code <- stanc(file = "MCMCcombinePatrickHalfNS.stan") # convert to C++ code
fit.model <- stan_model(stanc_ret = fit.code) # compile C++ code
rstan_options(auto_write = TRUE)

model_number <- 47

obtain_combineRR <- function(model_result)
{
  ## coef
  betaKhat <- t(mapply(function(x){x$coef[1:3]}, model_result))
  SigmaKhat <- mapply(function(x){x$var[1:3,1:3]}, model_result, SIMPLIFY = F)
  SigmaKhat <- simplify2array(SigmaKhat)
  SigmaKhat <- aperm(SigmaKhat, c(3,2,1))
  options(mc.cores=parallel::detectCores())
  current_data <- list(P=3, #param
                       K=model_number # citties
                       ,betaKhat=as.matrix(betaKhat)
                       ,SigmaKhat=SigmaKhat)
  set.seed(Sys.time())
  inits <- function()list(
    kappa= abs(rnorm(3, sd=0.01)),
    betaK= t(array(rep(apply(betaKhat,2,mean),model_number),c(3,model_number)))+rnorm(3*model_number,sd=0.001),
    betaBar=abs(apply(betaKhat,2,mean)+rnorm(3,sd=0.001))
  )
  # Sample from Stan model
  fit1 <- rstan::sampling(fit.model, data=current_data,chains = 1, iter = 100000, warmup=50000, thin =100, 
                          control = list(max_treedepth = 10, adapt_delta=0.95 ,stepsize_jitter=0.6)
                          , pars =c("betaK", "G","betaBar","kappa")
                          , verbose=T
                          ,init = inits
  )
  cityCombine <-rstan::extract(fit1)
  cityCombine
  # betaBarSample <- cityCombine$betaBar
  # apply(cityCombine$betaBar,2,quantile,c(0.5,0.025,0.975))
  # betaKSample <-  cityCombine$betaK
}

load("model_resultmorbidityall-causeall-ageall-yearpm25no2o3.RData")

combine_result_mort <- obtain_combineRR(model_result)
   
  