# daily-mortality-and-air-quality
varying covariances pollution model in Stan and daily mortality study


The hourly monitoring pollution data can be downloaded by “downloadPollution.R”.

Temperature data can be downloaded by “obtainTemperature.R”.

Then, the varying pollution model is running by R script 1,2,3,4,5, with the external defined Stan code for pollution model (JASA_pollution_model.stan).

The disease model code is “fitdiseasemodel.R”.

The code for combining health effects across cities can be seen in “combineAcrossCities.R” with the external defined Stan code “MCMCcombine.stan”. 
