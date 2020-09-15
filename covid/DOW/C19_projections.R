library(dplyr)
library(sn)
library(rstan)
library(ggplot2)
library(smooth) 
library(stringr)
library(knitr)
library(zoo)
library(reshape2)

source("forecast.R")

### same preamble as C19_model.R
covid_expected = get_covid_expected_counts()

covid_expected$world$country[covid_expected$world$country=="Bosnia and Herzegovina"] = "Bosnia & Herzegovina"

nCov_All = get_covid_data_cva(covid_expected)

nCov_All= nCov_All[nCov_All$reporting_unit != 'Veteran Affair', ]

#On April 14, New York City reported 3,778 additional deaths that have occurred since March 11 and have been classified as "probable," defined as follows: “decedent [...] had no known positive laboratory test for SARS-CoV-2 (COVID-19) but the death certificate lists as a cause of death “COVID-19” or an equivalent"
reassignDate = which(nCov_All$reporting_unit == 'New York' & 
                       nCov_All$time == as.Date('2020/04/18'))
# impute mean of day before and after
toReassign = nCov_All[reassignDate,'dead'] - mean(c(574, 506))

nCov_All[reassignDate,'dead'] = nCov_All[reassignDate,'dead'] - toReassign
reassignRange = which(
  nCov_All$time >= as.Date('2020/03/11') & 
    nCov_All$time <= as.Date('2020/04/18') & 
    nCov_All$reporting_unit == 'New York')
theResample = round(toReassign * nCov_All[reassignRange,'dead'] / sum(nCov_All[reassignRange,'dead']))
nCov_All[reassignRange,'dead'] = nCov_All[reassignRange,'dead'] + theResample

reassignTimes = nCov_All[reassignRange, 'time']


reassignDateUS = which(
  nCov_All$time == nCov_All[reassignDate,'time'] & 
    nCov_All$reporting_unit == 'United States')

reassignRangeUS = which(
  nCov_All$time %in% reassignTimes &
    nCov_All$reporting_unit == 'United States')

reassignDateUsIndex = match(nCov_All[reassignRangeUS,'time'],
                            nCov_All[reassignRange,'time'])
reassignRangeUS = reassignRangeUS[reassignDateUsIndex]

nCov_All[reassignDateUS, 'dead'] = nCov_All[reassignDateUS, 'dead'] -
  toReassign
nCov_All[reassignRangeUS,'dead'] = nCov_All[reassignRangeUS,'dead'] + theResample

nCov_All[nCov_All$reporting_unit %in% c('New York','United States'), 1:6]

# latest days's coronavirus.app data sometimes has low counts
getRid = which(nCov_All$time >= (as.Date(gsub(" .*", "", Sys.time()))-1))
if(length(getRid)) {
  nCov_All = nCov_All[-getRid, ]
}

nCov_All = rbind( nCov_All[!(nCov_All$reporting_unit %in% 
                               c('Belgium','France')), ], 
                  xFrance[,colnames(nCov_All)],
                  belgiumAgg[,colnames(nCov_All)])
nCov_All = nCov_All[order(nCov_All$reporting_unit, nCov_All$time), ]


popData <- global_Pop_Data() %>% filter(country == "Italy")

ratesByAge = data.frame(age = c(0,10,20,30,40,50,60,70,80,90),
                        deaths = c(0,0,1,20,81,340,1073,3206,3652,845))
popItaly = popData[popData$country == 'Italy',]
ratesByAge$pop = tapply(popItaly$pop, 
                        cut(popItaly$AgeGrpStart, c(ratesByAge$age, Inf), right=FALSE), 
                        sum)
ratesByAge$rate = ratesByAge$deaths / ratesByAge$pop


popItaly = popData[popData$country == 'Italy',]
ratesByAge$pop = tapply(popItaly$pop, 
                        cut(popItaly$AgeGrpStart, c(ratesByAge$age, Inf), right=FALSE), 
                        sum)
ratesByAge$rate = ratesByAge$deaths / ratesByAge$pop


world_Expected <- function(popData, ratesByAge, 
                           ageVar = 'AgeGrpStart', regionVar = 'country') {
  popData$ageCut = cut(popData[[ageVar]], c(ratesByAge$age, Inf),
                       labels = ratesByAge$age, right=FALSE)
  
  popData$rate = ratesByAge[match(
    as.character(popData$ageCut), as.character(ratesByAge$age)),
    'rate']
  popData$Expected = popData$rate * popData$pop
  result = aggregate(popData[,c('Expected','pop')], popData[,regionVar,drop=FALSE],
                     sum)
  result$mMult = 1e6/result$pop
  result
}
####

setwd("D:\\01_PhD\\Covid")


age_groups <- c("0 to 4 years", "5 to 9 years", "10 to 14 years", "15 to 19 years", "20 to 24 years", "25 to 29 years", "30 to 34 years", "35 to 39 years", "40 to 44 years", 
                "45 to 49 years", "50 to 54 years", "55 to 59 years", "60 to 64 years", "65 to 69 years", "70 to 74 years", "75 to 79 years", 
                "80 to 84 years", "85 to 89 years", "90 to 94 years", "95 to 99 years")


get_canada_expected <- read.csv("17100005.csv", encoding = "UTF-8" ) %>% rename(ref.date = X.U.FEFF.REF_DATE) %>% filter(Sex == "Both sexes") %>%
  select(ref.date, GEO, Age.group, VALUE) %>%
  filter(Age.group %in% age_groups) %>% group_by(GEO) %>% filter(ref.date == max(ref.date), GEO !="Northwest Territories including Nunavut") %>%
  mutate(age.cat = case_when(
    Age.group %in% c("0 to 4 years", "5 to 9 years") ~ 0,
    Age.group %in% c("10 to 14 years", "15 to 19 years") ~ 10,
    Age.group %in% c("20 to 24 years", "25 to 29 years") ~ 20,
    Age.group %in% c("30 to 34 years", "35 to 39 years") ~ 30,
    Age.group %in% c("40 to 44 years", "45 to 49 years") ~ 40,
    Age.group %in% c("50 to 54 years", "55 to 59 years") ~ 50,
    Age.group %in% c("60 to 64 years", "65 to 69 years") ~ 60,
    Age.group %in% c("70 to 74 years", "75 to 79 years") ~ 70,
    Age.group %in% c("80 to 84 years", "85 to 89 years") ~ 80,
    Age.group %in% c("90 to 94 years", "95 to 99 years") ~ 90
  )) %>%
  rename(pop = VALUE)


canada_expected <- world_Expected(popData=get_canada_expected, ratesByAge, 
                                  ageVar = 'age.cat', regionVar = 'GEO')

###spain expected
setwd("D:\\01_PhD\\Covid")
get_spain_expected <- readxl::read_excel("31304.xlsx") %>% data.frame() %>%
  mutate(age_group = ifelse(str_detect(Total, "year"), Total, NA)) %>% zoo::na.locf() %>%
  filter(age_group != "85 years old and over") %>%
  filter(!str_detect(Total, "year")) %>%
  mutate(age.cat = case_when(
    age_group %in% c("0 years old", "1 year old", "2 years old", "3 years old","4 years old", "5 years old", "6 years old", "7 years old", "8 years old", "9 years old") ~ 0,
    age_group %in% c("10 years old","11 years old","12 years old","13 years old","14 years old", "15 years old", "16 years old", "17 years old", "18 years old", "19 years old") ~ 10,
    age_group %in% c("20 years old","21 years old","22 years old","23 years old","24 years old", "25 years old", "26 years old", "27 years old", "28 years old", "29 years old") ~ 20,
    age_group %in% c("30 years old","31 years old","32 years old","33 years old","34 years old", "35 years old", "36 years old", "37 years old", "38 years old", "39 years old") ~ 30,
    age_group %in% c("40 years old","41 years old","42 years old","43 years old","44 years old", "45 years old", "46 years old", "47 years old", "48 years old", "49 years old") ~ 40,
    age_group %in% c("50 years old","51 years old","52 years old","53 years old","54 years old", "55 years old", "56 years old", "57 years old", "58 years old", "59 years old") ~ 50,
    age_group %in% c("60 years old","61 years old","62 years old","63 years old","64 years old", "65 years old", "66 years old", "67 years old", "68 years old", "69 years old") ~ 60,
    age_group %in% c("70 years old","71 years old","72 years old","73 years old","74 years old", "75 years old", "76 years old", "77 years old", "78 years old", "79 years old") ~ 70,
    age_group %in% c("80 years old","81 years old","82 years old","83 years old","84 years old", "85 years old", "86 years old", "87 years old", "88 years old", "89 years old") ~ 80,
    age_group %in% c("90 years old","91 years old","92 years old","93 years old","94 years old", "95 years old", "96 years old", "97 years old", "98 years old", "99 years old", 
                     "100 years or more") ~ 90
  )) %>% group_by(Total, age.cat) %>% summarise(pop = sum(pop)) %>% rename(spain_region_names = Total) %>%ungroup()


spain_region_names <- unique(get_spain_expected$spain_region_names); spain_region_names
spain_subregion_names <- unique(nCov_All[which(nCov_All$country == "Spain"),]$reporting_unit); spain_subregion_names

spain_correspondence <- c("País Vasco","Castilla La Mancha", "C. Valenciana", "Andalucía", "Castilla y León", 
                          "Extremadura", "Islas Baleares", "Cataluña", "Castilla y León", "Extremadura", 
                          "Andalucía", "C. Valenciana","Castilla La Mancha", "Andalucía", "Galicia",
                          "Castilla La Mancha", "Cataluña", "Cataluña","Castilla La Mancha", "País Vasco",
                          "Andalucía", "Aragón", "Andalucía", "Castilla y León", "Cataluña",
                          "La Rioja","Galicia", "Comunidad de Madrid", "Andalucía", "Región de Murcia",
                          "Comunidad Foral de Navarra", "Galicia", "Principado de Asturias",  "Castilla y León", "Canarias",
                          "Galicia","Castilla y León", "Canarias", "Cantabria", "Castilla y León",
                          "Andalucía", "Castilla y León","Cataluña", "Aragón","Castilla La Mancha",
                          "C. Valenciana", "Castilla y León", "País Vasco", "Castilla y León", "Aragón",
                          'Ceuta', 'Melilla')


spain_region_names <- cbind(spain_region_names, spain_correspondence) %>% data.frame()

get_spain_expected <- get_spain_expected %>% left_join(spain_region_names, by = "spain_region_names") %>% group_by(spain_correspondence, age.cat) %>% summarise(pop = sum(pop))

spain_expected <- world_Expected(popData=get_spain_expected, ratesByAge, 
                                 ageVar = 'age.cat', regionVar = 'spain_correspondence')


get_brazil_expected <- readxl::read_excel("Brazil_census.xls") %>% data.frame() %>% 
  mutate(X90.anos.ou.mais = round(0.2*X80.anos.ou.mais), X80.a.90.anos = round(0.8*X80.anos.ou.mais)) %>%
  select(-X80.anos.ou.mais) %>%
  reshape2::melt( id = "Region") %>%
  mutate(age.cat = case_when(
    variable %in% c("Menos.de.1.ano", "X1.a.4.anos",'X5.a.9.anos') ~ 0,
    variable %in% c("X10.a.14.anos", "X15.a.19.anos") ~ 10,
    variable %in% c("X20.a.24.anos", "X25.a.29.anos") ~ 20,
    variable %in% c("X30.a.34.anos", "X35.a.39.anos") ~ 30,
    variable %in% c("X40.a.44.anos", "X45.a.49.anos") ~ 40,
    variable %in% c("X50.a.54.anos", "X55.a.59.anos") ~ 50,
    variable %in% c("X60.a.64.anos", "X65.a.69.anos") ~ 60,
    variable %in% c("X70.a.74.anos", "X75.a.79.anos") ~ 70,
    variable %in% c("X80.a.90.anos") ~ 80,
    variable %in% c("X90.anos.ou.mais") ~ 90)) %>%
  group_by(Region, age.cat) %>% summarise(pop = sum(value))%>%
  ungroup()

brazil_expected <- world_Expected(popData=get_brazil_expected, ratesByAge, 
                                  ageVar = 'age.cat', regionVar = 'Region')




#######loop this
nCov_code <- rstan::stanc(file = "C19_model.stan") # convert to C++ code
nCov_model <- rstan::stan_model(stanc_ret = nCov_code)     # compile generated code

nCov_code2 <- rstan::stanc(file = "C19_model_noDOW.stan") # convert to C++ code
nCov_model2 <- rstan::stan_model(stanc_ret = nCov_code2)     # compile generated code


dates_to_project = c("2020-04-01", "2020-04-07", "2020-04-13", "2020-04-17", "2020-04-22", "2020-04-28", "2020-05-4", 
                     "2020-05-04", "2020-05-12", "2020-05-20", "2020-05-25", "2020-06-05", "2020-06-10", "2020-06-15",
                     "2020-06-25")

dow_model_results <- list()
no_dow_model_results <- list()


for (date_idx in 1:length(dates_to_project)){

date_i = dates_to_project[date_idx]

print(date_i)

included_regions <- nCov_All %>% group_by(reporting_unit, country) %>% filter(country %in% c("Canada", "Spain", "United States")) %>% 
  filter(time <= date_i) %>%
  summarise(total_dead = sum(dead)) %>% filter( total_dead >=50) %>% 
  select(reporting_unit) %>% unique() %>%
  filter(!(reporting_unit %in% c("Canada", "United States", "Spain", "Brazil", "Galicia", "Comunidad Foral de Navarra", "Navajo Nation", "Veteran Affair", "Tennessee", "Región de Murcia", "Federal Bureau of Prisons") ) )


canada_US <- nCov_All %>% filter(reporting_unit %in% included_regions$reporting_unit) %>% filter(time <= date_i)


canada_US$Expected[which(canada_US$reporting_unit == "Alberta")] = canada_expected$Expected[which(canada_expected$GEO == "Alberta")]
canada_US$Expected[which(canada_US$reporting_unit == "Ontario")] = canada_expected$Expected[which(canada_expected$GEO == "Ontario")]
canada_US$Expected[which(canada_US$reporting_unit == "British Columbia")] = canada_expected$Expected[which(canada_expected$GEO == "British Columbia")]
canada_US$Expected[which(canada_US$reporting_unit == "Quebec")] = canada_expected$Expected[which(canada_expected$GEO == "Quebec")]


for (name in unique(spain_region_names$spain_correspondence)){
  canada_US$Expected[which(canada_US$reporting_unit==name)] = spain_expected$Expected[which(spain_expected$spain_correspondence==name)]
}

for (name in unique(brazil_expected$Region)){
  canada_US$Expected[which(canada_US$reporting_unit==name)] = brazil_expected$Expected[which(brazil_expected$Region==name)]
}

#### add IHME data

ihme_data <- read.csv("IHME_data.csv", encoding="UTF-8") %>% data.frame() %>% rename(reporting_unit=location_name, time = date) %>% mutate(time = as.Date(time))
ihme_data$reporting_unit[which(ihme_data$reporting_unit == "Andalucia")] <- "Andalucía"
ihme_data$reporting_unit[which(ihme_data$reporting_unit == "Aragon")] <- "Aragón"
ihme_data$reporting_unit[which(ihme_data$reporting_unit == "Valencian Community")] <- "C. Valenciana"
ihme_data$reporting_unit[which(ihme_data$reporting_unit == "Canary Islands")] <- "Canarias"
ihme_data$reporting_unit[which(ihme_data$reporting_unit == "Castilla-La Mancha")] <- "Castilla La Mancha"
ihme_data$reporting_unit[which(ihme_data$reporting_unit == "Castile and León")] <- "Castilla y León"
ihme_data$reporting_unit[which(ihme_data$reporting_unit == "Catalonia")] <- "Cataluña"
ihme_data$reporting_unit[which(ihme_data$reporting_unit == "Community of Madrid")] <- "Comunidad de Madrid"
ihme_data$reporting_unit[which(ihme_data$reporting_unit == "Georgia")] <- "Georgia (US)"
ihme_data$reporting_unit[which(ihme_data$reporting_unit == "Balearic Islands")] <- "Islas Baleares"
ihme_data$reporting_unit[which(ihme_data$reporting_unit == "Basque Country")] <- "País Vasco"
ihme_data$reporting_unit[which(ihme_data$reporting_unit == "Asturias")] <- "Principado de Asturias"



canada_US <- canada_US %>% left_join(ihme_data, by = c("reporting_unit","time")) %>%
  mutate(ihme_clean_dead = case_when(
    is.na(deaths_mean) ~ dead,
    deaths_mean < 0 ~dead,
    deaths_mean >=0 ~ round(deaths_mean))) #some of the IHME outcome data is decimals... round it for now.



canada_US$timeNumeric = as.numeric(canada_US$time)
canada_US$reporting_unit_fac = factor(canada_US$reporting_unit)
canada_US$reporting_unit_int = as.integer(canada_US$reporting_unit_fac)
canada_US$country_int = as.integer(factor(canada_US$country))
canada_US$logExpected = log(canada_US$Expected)
canada_US$dow = as.numeric(factor(weekdays(canada_US$time)))

regions <- unique(cbind(canada_US$reporting_unit, canada_US$country, canada_US$reporting_unit_int)) %>% data.frame()
names(regions) <- c("region", "country", "index")
countries <- unique(cbind(canada_US$country, canada_US$country_int)) %>% data.frame() %>% arrange(X2)
names(countries) <- c("country", "index")


################################################################################################################################################
######################################################## DOW MODEL #############################################################################
################################################################################################################################################

dataForStan <- list(N = nrow(canada_US),
                    R = nlevels(canada_US$reporting_unit_fac),
                    M = nlevels(factor(canada_US$country)),
                    n_brazil = length(which(regions$country == "Brazil")),
                    time_index = canada_US$timeNumeric,
                    region_index = canada_US$reporting_unit_int,
                    country_index = canada_US$country_int,
                    brazil_index = which(regions$country == "Brazil"),
                    not_brazil_index  = which(regions$country != "Brazil"),
                    logExpected = canada_US$logExpected,
                    dow = canada_US$dow,
                    y_obs = canada_US$ihme_clean_dead )

thin = 10
iter = 1 * 400 * thin
chains = 4

north_america_samples <- rstan::sampling( nCov_model, data = dataForStan, iter = iter,
                                          chains = chains, cores=chains, thin = thin, refresh =100, warmup = 0.75*iter,
                                          init = list( 
                                            list( 
                                              A = rep(as.numeric(as.Date(c('2020/4/1'))),dataForStan$R),
                                              B = rep(15,dataForStan$R),
                                              C = rep(18, dataForStan$R),
                                              K = rep(3, dataForStan$R),
                                              D = rep(2, dataForStan$R),
                                              eta= rep(1, dataForStan$M),
                                              alpha= rep(1, dataForStan$M),
                                              day= matrix(data = 1, nrow= 6, ncol = dataForStan$M),
                                              theta_c=2,
                                              theta_b=9,
                                              theta_k=3,
                                              inv_sqrt_phi = rep(2, dataForStan$M))
                                          )[rep(1, chains)],
                                          control = list( adapt_delta = 0.95,
                                                          max_treedepth = 16 ) )

fit <- north_america_samples

#save(fit, file = paste0("C19DOW_projections_", date_i, ".RData"))
       

ranefNames = c('A','B','C','K', 'D', 'test', "theta_c", "theta_k", "theta_b", "phi")

theSamples  = extract(fit)[ranefNames]

A =theSamples$A
B =theSamples$B
C =theSamples$C
D =theSamples$D
K =theSamples$K
test =theSamples$test
theta_c =theSamples$theta_c
theta_b =theSamples$theta_b
theta_k =theSamples$theta_k
phi =theSamples$phi


estimates <- matrix(ncol = 3, nrow =length(regions$region))

T = seq(as.Date(date_i), as.Date("2020-06-30"), by = "1 days")

weekday = as.numeric(factor(weekdays(T)))

for (r in 1:length(regions$region)){
  region = regions$region[r]
  region_data <- canada_US %>% filter(reporting_unit == region)
  region_idx = which(regions[,1]== region)
  country_idx = region_data$country_int[1]
  
  lambda <- matrix(nrow = iter/thin, ncol = length(T))
  
  for (i in 1:iter/thin){
    for (t_idx in 1:length(T)){
      wd = weekday[t_idx]
      time = as.numeric(T[t_idx])
      lambda[i, t_idx] = test[i, wd, country_idx] * region_data$Expected[1] *(C[i,region_idx]* dsn(x=time, xi= A[i,region_idx], omega = B[i,region_idx], alpha = K[i,region_idx]) + D[i,region_idx])
    }
  }
  estimates[r,] = log(quantile(rowSums(lambda), probs = c(0.025,0.5,0.975)))
}

observed <- canada_US %>% filter(time <=date_i) %>% group_by(reporting_unit) %>% summarise(observed = log(sum(ihme_clean_dead)))


estimates <- cbind(regions$region, estimates, observed$observed)

dow_model_results[[date_idx]] <- estimates


################################################################################################################################################
######################################################## NO DOW MODEL ##########################################################################
################################################################################################################################################

thin = 10
iter = 1 * 400 * thin
chains = 4

north_america_samples <- rstan::sampling( nCov_model2, data = dataForStan, iter = iter,
                                          chains = chains, cores=chains, thin = thin, refresh =100, warmup = 0.75*iter,
                                          init = list( 
                                            list( 
                                              A = rep(as.numeric(as.Date(c('2020/4/1'))),dataForStan$R),
                                              B = rep(15,dataForStan$R),
                                              C = rep(18, dataForStan$R),
                                              K = rep(3, dataForStan$R),
                                              D = rep(2, dataForStan$R),
                                              eta= rep(1, dataForStan$M),
                                              alpha= rep(1, dataForStan$M),
                                              theta_c=2,
                                              theta_b=9,
                                              theta_k=3,
                                              inv_sqrt_phi = rep(2, dataForStan$M))
                                          )[rep(1, chains)],
                                          control = list( adapt_delta = 0.95,
                                                          max_treedepth = 16 ) )

fit <- north_america_samples

save(fit, file = paste0("C19NONDOW_projections_", date_i, ".RData"))


ranefNames = c('A','B','C','K', 'D', 'test', "theta_c", "theta_k", "theta_b", "phi")

theSamples  = extract(fit)[ranefNames]

A =theSamples$A
B =theSamples$B
C =theSamples$C
D =theSamples$D
K =theSamples$K
theta_c =theSamples$theta_c
theta_b =theSamples$theta_b
theta_k =theSamples$theta_k
phi =theSamples$phi


estimates <- matrix(ncol = 3, nrow =length(regions$region))

for (r in 1:length(regions$region)){
  region = regions$region[r]
  region_data <- canada_US %>% filter(reporting_unit == region)
  region_idx = which(regions[,1]== region)
  country_idx = region_data$country_int[1]
  
  lambda <- matrix(nrow = iter/thin, ncol = length(T))
  
  for (i in 1:iter/thin){
    for (t_idx in 1:length(T)){
      time = as.numeric(T[t_idx])
      lambda[i, t_idx] = region_data$Expected[1] *(C[i,region_idx]* dsn(x=time, xi= A[i,region_idx], omega = B[i,region_idx], alpha = K[i,region_idx]) + D[i,region_idx])
    }
  }
  estimates[r,] = log(quantile(rowSums(lambda), probs = c(0.025,0.5,0.975)))
}

estimates <- cbind(regions$region, estimates, observed$observed)

no_dow_model_results[[date_idx]] <- estimates

}



#####################################################################################################################################################################################
#######################################################################IHME Estimates################################################################################################
#####################################################################################################################################################################################

description <- "Min death = 50, 15 dates, end june 30th"
#save(description, dow_model_results, no_dow_model_results, file = "C19_projections_jul28.RData")
load("C19_projections_jul28.RData")

present_results <- dow_model_results

present_results[[1]] = present_results[[1]] %>% data.frame() %>% mutate(date = dates_to_project[1])
present_results[[2]] = present_results[[2]] %>% data.frame() %>% mutate(date = dates_to_project[2])
present_results[[3]] = present_results[[3]] %>% data.frame() %>% mutate(date = dates_to_project[3])
present_results[[4]] = present_results[[4]] %>% data.frame() %>% mutate(date = dates_to_project[4])
present_results[[5]] = present_results[[5]] %>% data.frame() %>% mutate(date = dates_to_project[5])
present_results[[6]] = present_results[[6]] %>% data.frame() %>% mutate(date = dates_to_project[6])
present_results[[7]] = present_results[[8]] %>% data.frame() %>% mutate(date = dates_to_project[8])
present_results[[8]] = present_results[[9]] %>% data.frame() %>% mutate(date = dates_to_project[9])
present_results[[9]] = present_results[[10]] %>% data.frame() %>% mutate(date = dates_to_project[10])
present_results[[10]] = present_results[[11]] %>% data.frame() %>% mutate(date = dates_to_project[11])
present_results[[11]] = present_results[[12]] %>% data.frame() %>% mutate(date =dates_to_project[12])
present_results[[12]] = present_results[[13]] %>% data.frame() %>% mutate(date = dates_to_project[13])
present_results[[13]] = present_results[[14]] %>% data.frame() %>% mutate(date = dates_to_project[14])
present_results[[14]] = present_results[[15]] %>% data.frame() %>% mutate(date = dates_to_project[15])

present_results <- rbind(present_results[[1]], 
                         present_results[[2]], 
                         present_results[[3]],
                         present_results[[4]],
                         present_results[[5]],
                         present_results[[6]],
                         present_results[[7]],
                         present_results[[8]],
                         present_results[[9]],
                         present_results[[10]],
                         present_results[[11]],
                         present_results[[12]],
                         present_results[[13]],
                         present_results[[14]]) %>% 
  data.frame() %>% mutate(X2= as.numeric(X2), X3 = as.numeric(X3), X4 = as.numeric(X4), X5=as.numeric(X5), source = "DOW Model") %>%
  mutate(X2= log(exp(X2) +exp(X5) ) , X3 =  log(exp(X3) +exp(X5) ), X4 = log(exp(X4) +exp(X5) )   ) %>%
  select(-X5)

present_results2 <- no_dow_model_results

present_results2[[1]] = present_results2[[1]] %>% data.frame() %>% mutate(date = dates_to_project[1])
present_results2[[2]] = present_results2[[2]] %>% data.frame() %>% mutate(date = dates_to_project[2])
present_results2[[3]] = present_results2[[3]] %>% data.frame() %>% mutate(date = dates_to_project[3])
present_results2[[4]] = present_results2[[4]] %>% data.frame() %>% mutate(date = dates_to_project[4])
present_results2[[5]] = present_results2[[5]] %>% data.frame() %>% mutate(date = dates_to_project[5])
present_results2[[6]] = present_results2[[6]] %>% data.frame() %>% mutate(date = dates_to_project[6])
present_results2[[7]] = present_results2[[8]] %>% data.frame() %>% mutate(date = dates_to_project[8])
present_results2[[8]] = present_results2[[9]] %>% data.frame() %>% mutate(date = dates_to_project[9])
present_results2[[9]] = present_results2[[10]] %>% data.frame() %>% mutate(date = dates_to_project[10])
present_results2[[10]] = present_results2[[11]] %>% data.frame() %>% mutate(date = dates_to_project[11])
present_results2[[11]] = present_results2[[12]] %>% data.frame() %>% mutate(date =dates_to_project[12])
present_results2[[12]] = present_results2[[13]] %>% data.frame() %>% mutate(date = dates_to_project[13])
present_results2[[13]] = present_results2[[14]] %>% data.frame() %>% mutate(date = dates_to_project[14])
present_results2[[14]] = present_results2[[15]] %>% data.frame() %>% mutate(date = dates_to_project[15])

present_results2 <- rbind(present_results2[[1]], 
                         present_results2[[2]], 
                         present_results2[[3]],
                         present_results2[[4]],
                         present_results2[[5]],
                         present_results2[[6]],
                         present_results2[[7]],
                         present_results2[[8]],
                         present_results2[[9]],
                         present_results2[[10]],
                         present_results2[[11]],
                         present_results2[[12]],
                         present_results2[[13]],
                         present_results2[[14]]) %>% 
  data.frame() %>% mutate(X2= as.numeric(X2), X3 = as.numeric(X3), X4 = as.numeric(X4), X5 = as.numeric(X5), source = "non-DOW Model") %>%
  mutate(X2= log(exp(X2) +exp(X5) ) , X3 =  log(exp(X3) +exp(X5) ), X4 = log(exp(X4) +exp(X5) )   ) %>%
  select(-X5)


files = c("IHME_projections\\ihme_apr1.csv",
          "IHME_projections\\ihme_apr7.csv",
          "IHME_projections\\ihme_apr13.csv",
          "IHME_projections\\ihme_apr17.csv",
          "IHME_projections\\ihme_apr22.csv",
          "IHME_projections\\ihme_apr28.csv",
          "IHME_projections\\ihme_may4.csv",
          "IHME_projections\\ihme_may12.csv",
          "IHME_projections\\ihme_may20.csv",
          "IHME_projections\\ihme_may25.csv",
          "IHME_projections\\ihme_may29.csv",
          "IHME_projections\\ihme_june5.csv",
          "IHME_projections\\ihme_june10.csv",
          "IHME_projections\\ihme_june15.csv",
          "IHME_projections\\ihme_june25.csv")

ihme_results<- list()


for (i in 1:length(files)){
  
  file = files[i]
  
  iDatRaw <- read.csv(file, encoding="UTF-8")
  iDatRaw$time = as.Date(iDatRaw$date)
  
  iDatFull = iDatRaw[iDatRaw$time < "2020-06-30", ] %>% select(location_name, deaths_mean, deaths_lower, deaths_upper) %>% rename(reporting_unit = location_name) %>%
    group_by(reporting_unit) %>% summarise( X2 = sum(deaths_lower, na.rm = TRUE), X3 = sum(deaths_mean, na.rm = TRUE), X4 = sum(deaths_upper, na.rm = TRUE))
  
  iDatFull$reporting_unit[which(iDatFull$reporting_unit == "Andalucia")] <- "Andalucía"
  iDatFull$reporting_unit[which(iDatFull$reporting_unit == "Aragon")] <- "Aragón"
  iDatFull$reporting_unit[which(iDatFull$reporting_unit == "Valencian Community")] <- "C. Valenciana"
  iDatFull$reporting_unit[which(iDatFull$reporting_unit == "Canary Islands")] <- "Canarias"
  iDatFull$reporting_unit[which(iDatFull$reporting_unit == "Castilla-La Mancha")] <- "Castilla La Mancha"
  iDatFull$reporting_unit[which(iDatFull$reporting_unit == "Castile and León")] <- "Castilla y León"
  iDatFull$reporting_unit[which(iDatFull$reporting_unit == "Catalonia")] <- "Cataluña"
  iDatFull$reporting_unit[which(iDatFull$reporting_unit == "Community of Madrid")] <- "Comunidad de Madrid"
  iDatFull$reporting_unit[which(iDatFull$reporting_unit == "Georgia")] <- "Georgia (US)"
  iDatFull$reporting_unit[which(iDatFull$reporting_unit == "Balearic Islands")] <- "Islas Baleares"
  iDatFull$reporting_unit[which(iDatFull$reporting_unit == "Basque Country")] <- "País Vasco"
  iDatFull$reporting_unit[which(iDatFull$reporting_unit == "Asturias")] <- "Principado de Asturias"
  
  
  iDatFull <- iDatFull %>% filter(reporting_unit %in% regions$region) %>% data.frame() %>% mutate(X2 = log(X2), X3 = log(X3), X4 = log(X4))
  
  ihme_results[[i]] <- iDatFull
}


ihme_results[[1]] = ihme_results[[1]] %>% data.frame() %>% mutate(date = dates_to_project[1])
ihme_results[[2]] = ihme_results[[2]] %>% data.frame() %>% mutate(date = dates_to_project[2])
ihme_results[[3]] = ihme_results[[3]] %>% data.frame() %>% mutate(date = dates_to_project[3])
ihme_results[[4]] = ihme_results[[4]] %>% data.frame() %>% mutate(date = dates_to_project[4])
ihme_results[[5]] = ihme_results[[5]] %>% data.frame() %>% mutate(date = dates_to_project[5])
ihme_results[[6]] = ihme_results[[6]] %>% data.frame() %>% mutate(date = dates_to_project[6])
ihme_results[[7]] = ihme_results[[8]] %>% data.frame() %>% mutate(date = dates_to_project[8])
ihme_results[[8]] = ihme_results[[9]] %>% data.frame() %>% mutate(date = dates_to_project[9])
ihme_results[[9]] = ihme_results[[10]] %>% data.frame() %>% mutate(date = dates_to_project[10])
ihme_results[[10]] = ihme_results[[11]] %>% data.frame() %>% mutate(date = dates_to_project[11])
ihme_results[[11]] = ihme_results[[12]] %>% data.frame() %>% mutate(date =dates_to_project[12])
ihme_results[[12]] = ihme_results[[13]] %>% data.frame() %>% mutate(date = dates_to_project[13])
ihme_results[[13]] = ihme_results[[14]] %>% data.frame() %>% mutate(date = dates_to_project[14])
ihme_results[[14]] = ihme_results[[15]] %>% data.frame() %>% mutate(date = dates_to_project[15])

ihme_results <- rbind(ihme_results[[1]], 
                      ihme_results[[2]], 
                      ihme_results[[3]],
                      ihme_results[[4]],
                      ihme_results[[5]],
                      ihme_results[[6]],
                      ihme_results[[7]],
                      ihme_results[[8]],
                      ihme_results[[9]],
                      ihme_results[[10]],
                      ihme_results[[11]],
                      ihme_results[[12]],
                      ihme_results[[13]],
                      ihme_results[[14]]
                      ) %>% data.frame() %>% mutate(source = "IHME Model") %>% rename( X1 = reporting_unit)

all_results <- rbind(present_results, present_results2, ihme_results)


observed_results <- nCov_All %>% filter(reporting_unit %in% present_results$X1) %>% filter(time <= "2020-06-30") %>% group_by(reporting_unit) %>% summarise(observed = sum(dead)) %>%
  rename(X1 = reporting_unit) %>% filter()

all_results <- all_results %>% left_join(observed_results, by = "X1") %>%
  mutate(contains_true = ifelse(log(observed)< X4 & log(observed) > X2, 1, 0),
         length_interval = X4-X2)

all_results <- all_results[-which(all_results$length_interval == "NaN"),] %>% filter(X1 %in% present_results$X1)

#Canada
pdf("D:\\01_PhD\\Covid\\dow manuscript\\Figures\\validation_canada.pdf", width = 8, height=6)
all_results %>% filter(X1 %in% regions$region[which(regions$country == "Canada")]) %>% ggplot(aes(x = date, y = X3, color = source)) +
  geom_point(position = position_dodge(width = 0.4)) + 
  geom_errorbar( aes(ymin = X2, ymax = X4),size = 1.2, width = 0.3, position = position_dodge(width = 0.4)) +
  facet_wrap(. ~ X1, scales = "free_y")+
  geom_hline( data= all_results %>% filter(X1 %in% regions$region[which(regions$country == "Canada")]), aes(yintercept = log(observed)), linetype = "dashed", color = "black")+
  labs( y = "Log Cumulative Deaths", x = "", color = NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1))
dev.off()


#US
pdf("D:\\01_PhD\\Covid\\dow manuscript\\Figures\\validation_US.pdf", width = 8, height=6)
all_results %>% filter(X1 %in% c("New York", "New Jersey", "Texas", "California") ) %>% ggplot(aes(x = date, y = X3, color = source)) +
  geom_point(position = position_dodge(width = 0.4)) + 
  geom_errorbar( aes(ymin = X2, ymax = X4),size = 1.2, width = 0.3, position = position_dodge(width = 0.4)) +
  facet_wrap(. ~ X1, scales = "free_y")+
  geom_hline( data= all_results %>% filter(X1 %in% c("New York", "New Jersey", "Texas", "California") ), aes(yintercept = log(observed)), linetype = "dashed", color = "black")+
  labs( y = "Log Cumulative Deaths", x = "", color = NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1))
dev.off()

pdf("D:\\01_PhD\\Covid\\dow manuscript\\Figures\\validation_spain.pdf", width = 8, height=6)
all_results %>% filter(X1 %in% c("Comunidad de Madrid","Cataluña","Castilla La Mancha","País Vasco")) %>% ggplot(aes(x = date, y = X3, color = source)) +
  geom_point(position = position_dodge(width = 0.4)) + 
  geom_errorbar( aes(ymin = X2, ymax = X4),size = 1.2, width = 0.3, position = position_dodge(width = 0.4)) +
  facet_wrap(. ~ X1, scales = "free_y")+
  geom_hline( data= all_results %>% filter(X1 %in% c("Comunidad de Madrid","Cataluña","Castilla La Mancha","País Vasco")), aes(yintercept = log(observed)), linetype = "dashed", color = "black")+
  labs( y = "Log Cumulative Deaths", x = "", color = NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1))
dev.off()



png("D:\\01_PhD\\Covid\\dow manuscript\\Figures\\validation_all.png", width = 3000, height=1500, dpi = 700)
all_results %>% ggplot(aes(x = date, y = X3, color = source)) +
  geom_point(position = position_dodge(width = 0.4)) + 
  geom_errorbar( aes(ymin = X2, ymax = X4),size = 1.2, width = 0.3, position = position_dodge(width = 0.4)) +
  facet_wrap(. ~ X1, scales = "free_y")+
  geom_hline( data= all_results , aes(yintercept = log(observed)), linetype = "dashed", color = "black")+
  labs( y = "Log Cumulative Deaths", x = "", color = NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1))
dev.off()

###percent of intervals that contain the true value
ints_containing_true <- all_results %>% group_by(source, date) %>% summarise(pct = sum(contains_true)/n() ) %>% ungroup() %>% data.frame()

transpose= reshape(ints_containing_true, direction = "wide", idvar = "date", timevar = "source", times = c("DOW Model", "IHME Model", "non-DOW Model"), v.names = "pct") %>% data.frame()
xtable(transpose, type = "latex")


##proportion
pdf("D:\\01_PhD\\Covid\\dow manuscript\\Figures\\interval_contains_true.pdf", width = 4, height=4)
ints_containing_true %>% ungroup %>%
  ggplot(aes(y=pct, x= date, group =source, color = source)) +
  geom_point()+
  geom_line()+
  #geom_bar(stat = "identity", position = "dodge") +
  labs(x= "", y="Proportion",  color = NULL) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1), legend.position = "none")
dev.off()


###overlap

left <- all_results %>% arrange( source, X1,date) %>% group_by(X1) %>%mutate(lagX2 = dplyr::lag(X2, n=1, default = NA), 
                                                                                                     lagX3 = dplyr::lag(X3, n=1, default = NA),
                                                                                                     lagX4 = dplyr::lag(X4, n=1, default = NA)) %>%ungroup()%>%
  mutate(length_overlap = pmin(X4,lagX4) - pmax(X2, lagX2)) %>%
  mutate(proportion_overlap = pmax(0,length_overlap/length_interval)) %>%
  group_by(source,date) %>%
  summarise(mean_overlap = mean(proportion_overlap, na.rm = TRUE)) %>%
  filter(date != "2020-04-01")


pdf("D:\\01_PhD\\Covid\\dow manuscript\\Figures\\interval_overlap.pdf", width = 4, height=4)
left %>% ungroup %>%
  ggplot(aes(y=mean_overlap, x= date,group = source, color = source)) +
  geom_point() +
  geom_line()+
  labs( y="Proportion ", x = "", colour = NULL) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1), legend.position = "none")
dev.off()

###Length
pdf("D:\\01_PhD\\Covid\\dow manuscript\\Figures\\interval_length.pdf", width = 4, height=4)
all_results %>% group_by(source, date) %>% summarise(length = mean(length_interval) ) %>% ungroup() %>% data.frame() %>%
  ggplot(aes(y=length, x= date, group =source, color = source)) +
  geom_point()+
  geom_line()+
  #geom_bar(stat = "identity", position = "dodge") +
  labs(x= "", y="Mean Log-length",  color = NULL) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1), legend.position = c(0.7,0.7))
dev.off()

###deaths from other causes canada
days_of_week <- as.factor(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


deaths_by_dow <- read.csv("allcausemortality.csv") %>% data.frame() %>%select(-count) %>% mutate(weekday = as.factor(weekday)) %>%
  mutate(cause = case_when(
    cause == "all" ~ "All Causes",
    cause == "cir" ~ "Circulatory",
    cause == "CP" ~ "Circulatory and Pulmonary",
    cause == "NCP" ~ "Non-Circulatory Pulmonary",
    cause == "pulm" ~ "Pulmonary"
  )) %>% filter(cause != "All Causes") 


sunday_vals = deaths_by_dow %>%
  group_by(area, cause) %>%
  filter(row_number()==n())

deaths_by_dow$standardized <- deaths_by_dow$percent/rep(sunday_vals$percent, each = 7)

library(xtable)

transpose= deaths_by_dow %>% filter(area=="Canada") %>% select(-area) %>% reshape(direction = "wide", idvar = "cause", timevar = "weekday", times = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), 
                                                                v.names = "standardized") %>% data.frame()
xtable(transpose, type = "latex", digits = 3)



png("D:\\01_PhD\\Covid\\dow manuscript\\Figures\\dow_mortality_all.png", width = 960, height=960)
deaths_by_dow %>% filter(weekday !="Sunday")%>%
  ggplot(aes(x = weekday, y=standardized, colour = area,group = area)) + 
  geom_point()+
  geom_line(size = 1.2) +
  facet_wrap(~cause, scale = "free")+
  scale_x_discrete (limits = days_of_week) +
  theme_bw()+
  labs(y= "Percent", x= "Day of the Week", color = NULL)
dev.off()


