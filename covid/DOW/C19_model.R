library(dplyr)
library(sn)
library(rstan)
library(ggplot2)
library(smooth) 
library(stringr)
library(knitr)
library(zoo)
library(reshape2)

setwd("D:\01_PhD\papers\covid\src\publicCovid\DOW_model_code")

source("forecast.R")

covid_expected = get_covid_expected_counts()

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
nCov_All = nCov_All %>% filter(time <= "2020-08-31") ## last date of data fixed at August 31st

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


# included_regions <- nCov_All %>% group_by(reporting_unit, country) %>% filter(country %in% c("Spain", "Canada", "United States") || reporting_unit %in% c("São Paulo","Rio de Janeiro","Ceará", "Pará","Pernambuco",
#                                                                                                                                                           "Amazonas", "Maranhão", "Bahia", "Espírito Santo","Alagoas")) %>% 
#   summarise(total_dead = sum(dead)) %>% filter( total_dead >=10) %>% 
#   select(reporting_unit) %>% unique() %>%
#   filter(!(reporting_unit %in% c("Canada", "United States", "Spain", "Brazil", "Galicia", "Comunidad Foral de Navarra", "Navajo Nation", "Veteran Affair", "Tennessee", "Región de Murcia") ) )

included_regions <- nCov_All %>% group_by(reporting_unit, country) %>% filter(country %in% c("Canada", "United States", "Spain", "Brazil")) %>% filter(time <= "2020-06-25") %>%
  summarise(total_dead = sum(dead)) %>% filter( total_dead >=50) %>% 
  select(reporting_unit) %>% unique() %>%
  filter(!(reporting_unit %in% c("Canada", "United States", "Spain", "Brazil", "Navajo Nation", "Veteran Affair", "Federal Bureau of Prisons") ) )


canada_US <- nCov_All %>% filter(reporting_unit %in% included_regions$reporting_unit)  %>% filter(time <= "2020-06-25")
rest_observed <- nCov_All %>% filter(reporting_unit %in% included_regions$reporting_unit) %>% filter(time > "2020-06-25" )

####quick data cleaning: Spain has a bunch of reported deaths reported retrospectively on June 19th, set these to 0
####Michigan, Delaware and Galicia, CFdN all have outliers, set these to an average of adjacent points (this should have a small but neglible effect on our DoW estimates)
canada_US$dead[which(canada_US$country == "Spain" & canada_US$time == "2020-06-19")] = 0

canada_US$dead[which(canada_US$reporting_unit == "Michigan" & canada_US$time == "2020-06-05")] = mean(canada_US$dead[which(canada_US$reporting_unit == "Michigan" & canada_US$time == "2020-06-04")],
                                                                                                      canada_US$dead[which(canada_US$reporting_unit == "Michigan" & canada_US$time == "2020-06-06")])

canada_US$dead[which(canada_US$reporting_unit == "Delaware" & canada_US$time == "2020-06-23")] = mean(canada_US$dead[which(canada_US$reporting_unit == "Delaware" & canada_US$time == "2020-06-22")],
                                                                                                      canada_US$dead[which(canada_US$reporting_unit == "Delaware" & canada_US$time == "2020-06-24")])

canada_US$dead[which(canada_US$reporting_unit == "Galicia" & canada_US$time == "2020-04-29")] = mean(canada_US$dead[which(canada_US$reporting_unit == "Galicia" & canada_US$time == "2020-04-28")],
                                                                                                    canada_US$dead[which(canada_US$reporting_unit == "Galicia" & canada_US$time == "2020-04-30")])

canada_US$dead[which(canada_US$reporting_unit == "Comunidad Foral de Navarra" & canada_US$time == "2020-04-17")] = mean(canada_US$dead[which(canada_US$reporting_unit == "Comunidad Foral de Navarra" & canada_US$time == "2020-04-16")],
                                                                                                                  canada_US$dead[which(canada_US$reporting_unit == "Comunidad Foral de Navarra" & canada_US$time == "2020-04-18")])


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

canada_US$timeNumeric = as.numeric(canada_US$time)
canada_US$reporting_unit_fac = factor(canada_US$reporting_unit)
canada_US$reporting_unit_int = as.integer(canada_US$reporting_unit_fac)
canada_US$country_int = as.integer(factor(canada_US$country))
canada_US$logExpected = log(canada_US$Expected)

regions <- unique(cbind(canada_US$reporting_unit, canada_US$country, canada_US$reporting_unit_int)) %>% data.frame()
names(regions) <- c("region", "country", "index")
countries <- unique(cbind(canada_US$country, canada_US$country_int)) %>% data.frame() %>% arrange(X2)
names(countries) <- c("country", "index")


#Prep data and run the model
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
                    dow = as.numeric(factor(weekdays(canada_US$time))),
                    y_obs = canada_US$dead )



thin = 10
iter = 1 * 400 * thin
chains = 4

nCov_code <- rstan::stanc(file = "C19_model.stan") # convert to C++ code
nCov_model <- rstan::stan_model(stanc_ret = nCov_code)     # compile generated code

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

#load(file = "C19_projections_AUG08.RData") #this file contains the posterior samples for our run, saved in an object called "fit"
traceplot(fit, pars = c("A"))
traceplot(fit, pars = c("B"))
traceplot(fit, pars = c("C"))
traceplot(fit, pars = c("test"))
traceplot(fit, pars = c("K"))
traceplot(fit, pars = c("D"))

traceplot(fit, pars = c("theta_c", "theta_k", "theta_b", "phi", "eta","zeta"))

#save(fit, file = "C19_projections_AUG10.RData")

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

T = seq(as.Date("2020/3/1"), as.Date("2020/10/1"), by = "1 days")
dow = as.numeric(factor(weekdays(T)))
T_num = as.numeric(T)



forYcount <- 10^seq(1, 7)
forYcount <- sort(unique(c(forYcount, forYcount / 2, forYcount / 5)))
forYtext <- format(forYcount, scientific = FALSE)
forYtext <- gsub("000000$", "M", forYtext)
forYtext <- gsub("000$", "K", forYtext)
forYtext <- gsub("[[:space:]]", "", forYtext)

get_populations <- get_canada_expected %>%group_by(GEO) %>% summarise(province_pop = sum(pop))



#6 functions that are all basically the same, meant for plotting daily/cumulative death counts
plot_province= function(region_name, country_name, plot = TRUE, legend_pos = "topright", cex=1.0, legend.cex = 1.0){
  
  region_idx = as.numeric(regions$index[which(regions$region==region_name)])
  country_idx = as.numeric(countries$index[which(countries$country==country_name)])
  
  #population = get_populations$province_pop[which(get_populations$province==region_name)]
  
  region_data <- canada_US %>% filter(reporting_unit == region_name)
  rest_data <- rest_observed %>%filter(reporting_unit == region_name)
  maxy = max(c(log(region_data$dead),log(rest_data$dead)))
  
  lambda = matrix(nrow = 400, ncol = length(T))
  
  lambda[1,] = sum(table(dow)*(test[1,dow,country_idx][1:7]))/sum(table(dow))* region_data$Expected[1] *(C[1, region_idx]* dsn(x=T_num, xi= A[1,region_idx], omega = B[1,region_idx], alpha = K[1,region_idx]) + D[1,region_idx])
  
  if (plot==TRUE){
 
  plot(T, log(lambda[1,]), col = "#00000010",  ylim = c(0, 1.2*maxy), type = "l", lwd = 0.5, ylab = "Daily Deaths", xlab = "", yaxt = "n", cex.lab=cex, cex.axis=cex, cex.main=cex, cex.sub=cex)
  axis(side = 2, at = log(forYcount), labels = forYtext, cex.axis=cex)
  }
  
  for (i in 2:400){
    lambda[i,] = sum(table(dow)*(test[i,dow,country_idx][1:7]))/sum(table(dow))*region_data$Expected[1]* (C[i,region_idx] * dsn(x=T_num, xi= A[i,region_idx], omega = B[i,region_idx], alpha = K[i,region_idx]) + D[i,region_idx])
    if(plot == TRUE){lines(T, log(lambda[i,]), col = "#00000010", lwd = 0.5)}
  }
  
  if (plot == TRUE){
    points(as.numeric(region_data$time), log(region_data$dead), type = "p", col = "red", pch = 16)
    points(as.numeric(rest_data$time), log(rest_data$dead), type = "p", col = "purple", pch = 16)
    lines(T,log(colMeans(lambda)), col = "green", lwd = 2)
    legend(legend_pos, legend=c("Observed Data since June 25th", "Data used for model fitting", "Posterior Mean", "Posterior Samples"), 
           col = c("purple","red", "green", "black"), lty = c(NA, NA, 1, 1), pch = c(16, 16, NA, NA), cex=legend.cex)
  }
  
  return(lambda)
} # function that averages day of the week effects when plotting
plot_province_osc= function(region_name, country_name, plot = TRUE, legend_pos = "topright", cex=1.0, legend.cex = 1.0, title = ""){
  
  region_idx = as.numeric(regions$index[which(regions$region==region_name)])
  country_idx = as.numeric(countries$index[which(countries$country==country_name)])
  
  #population = get_populations$province_pop[which(get_populations$province==region_name)]
  
  region_data <- canada_US %>% filter(reporting_unit == region_name)
  rest_data <- rest_observed %>%filter(reporting_unit == region_name)
  maxy = max(c(log(region_data$dead),log(rest_data$dead)))
  
  lambda = matrix(nrow = 400, ncol = length(T))
  
  lambda[1,] = (test[1,dow, country_idx]) * region_data$Expected[1] *(C[1, region_idx]* dsn(x=T_num, xi= A[1,region_idx], omega = B[1,region_idx], alpha = K[1,region_idx]) + D[1,region_idx])
  
  if (plot==TRUE){
    
    plot(T, log(lambda[1,]), col = "#00000010",  ylim = c(0, 1.2*maxy), type = "l", lwd = 0.5, ylab = "Daily Deaths", xlab = "", yaxt = "n", cex.lab=cex, cex.axis=cex, cex.main=cex, cex.sub=cex, main = title)
    axis(side = 2, at = log(forYcount), labels = forYtext, cex.axis=cex)
  }
  
  for (i in 2:400){
    lambda[i,] = (test[i,dow,country_idx])*region_data$Expected[1]* (C[i,region_idx] * dsn(x=T_num, xi= A[i,region_idx], omega = B[i,region_idx], alpha = K[i,region_idx]) + D[i,region_idx])
    if(plot == TRUE){lines(T, log(lambda[i,]), col = "#00000010", lwd = 0.5)}
  }
  
  if (plot == TRUE){
    points(as.numeric(region_data$time), log(region_data$dead), type = "p", col = "red", pch = 16)
    points(as.numeric(rest_data$time), log(rest_data$dead), type = "p", col = "purple", pch = 16)
    lines(T,log(colMeans(lambda)), col = "green", lwd = 2)
    if (!is.null(legend_pos)) {
    legend(legend_pos, legend=c("Observed Data since June 25th", "Data used for model fitting", "Posterior Mean", "Posterior Samples"), 
           col = c("purple","red", "green", "black"), lty = c(NA, NA, 1, 1), pch = c(16, 16, NA, NA), cex=legend.cex)
      }
  }
  
  return(lambda)
} #does not average day of the week effects when plotting (oscillatory)
plot_province_cum = function(region_name, country_name, plot = TRUE, legend_pos = "bottomright", plot_factor = 0.01, cex=1.0, legend.cex = 1.0){
  
  region_idx = as.numeric(regions$index[which(regions$region==region_name)])
  country_idx = as.numeric(countries$index[which(countries$country==country_name)])
  
  #population = get_populations$province_pop[which(get_populations$province==region_name)]
  
  region_data <- canada_US %>% filter(reporting_unit == region_name)
  rest_data <- rest_observed %>% filter(reporting_unit == region_name)
  maxy = max(log(sum(c(region_data$dead,rest_data$dead))))
  miny = max(log(sum(region_data$dead)))
  
  
  lambda = matrix(nrow = 400, ncol = length(T))
  
  
  replace_indices = T %>%data.frame() #need to replace posterior with actual data
  names(replace_indices) = "time"
  replace_indices <- replace_indices %>% left_join(region_data, by = "time") %>% select(time, dead)
  replace_indices <- which(!is.na(replace_indices$dead))
  
  
  lambda[1,] = sum(table(dow)*(test[1,dow,country_idx][1:7]))/sum(table(dow))* region_data$Expected[1] *(C[1, region_idx]* dsn(x=T_num, xi= A[1,region_idx], omega = B[1,region_idx], alpha = K[1,region_idx]) + D[1,region_idx])
  lambda[1, replace_indices] = region_data$dead
  
  
  if (plot == TRUE){
  
  plot(T[which(T>"2020-06-28")], log(cumsum(lambda[1,]))[which(T>"2020-06-28")], col = "#00000010",  ylim = c(exp(-plot_factor)*miny, exp(plot_factor)*maxy), type = "l", lwd = 0.5, 
       ylab = "Cumulative Deaths", xlab = "", yaxt = "n", cex.lab=cex, cex.axis=cex, cex.main=cex, cex.sub=cex)
  axis(side = 2, at = log(forYcount)[which(forYcount > exp(-plot_factor)*miny)], labels = forYtext[which(forYcount > exp(-plot_factor)*miny)], cex.axis=cex)
  }
  
  for (i in 2:400){
    lambda[i,] = sum(table(dow)*(test[i,dow,country_idx][1:7]))/sum(table(dow))*region_data$Expected[1]* (C[i,region_idx] * dsn(x=T_num, xi= A[i,region_idx], omega = B[i,region_idx], alpha = K[i,region_idx]) + D[i,region_idx])
    lambda[i,replace_indices] = region_data$dead
    if (plot == TRUE){lines(T, log(cumsum(lambda[i,])), col = "#00000010", lwd = 0.5)}
  }
  
   if (plot == TRUE){
    # points(as.numeric(region_data$time), log(cumsum(region_data$dead)), type = "p", col = "red", pch = 16)
     points(as.numeric(rest_data$time), log(sum(region_data$dead) + cumsum(rest_data$dead)), type = "p", col = "purple", pch = 16)
     lines(T[which(T > "2020-06-25")], log(cumsum(colMeans(lambda)))[which(T > "2020-06-25")], col = "green", lwd = 2)
     legend(legend_pos, legend=c("Observed Data since June 25th", "Posterior Mean", "Posterior Samples"), 
            col = c("purple","green", "black"), lty = c(NA, 1, 1), pch = c(16, NA, NA), cex=legend.cex)
     }
  return(lambda)
} #cumulative death counts

plot_country <- function(country_name, legend_pos = "topright", cex = 1.0, legend.cex = 1.0){

  country_data = canada_US %>% filter(country == country_name) %>% group_by(time) %>% summarise(sum_dead = sum(dead))
  country_data_rest = rest_observed %>% filter(country == country_name) %>% group_by(time) %>% summarise(sum_dead = sum(dead))
  maxy = max(c(log(country_data$sum_dead)),log(country_data_rest$sum_dead))
  brazil_regions <- regions$region[which(regions$country == country_name)]
  
  temp = matrix(0, nrow = 400, ncol = length(T))
  
  for (region in brazil_regions){
    temp <- temp + plot_province(region, country_name, plot = FALSE)
  }
  
  plot(T, log(temp[1,]), col = "#00000010", type = "l", ylim = c(0, 1.2*maxy), lwd = 0.5, ylab = "Daily Deaths", yaxt = "n", xlab = "", cex.lab=cex, cex.axis=cex, cex.main=cex, cex.sub=cex)
  axis(side = 2, at = log(forYcount), labels = forYtext, cex.axis = cex)
  
  for (i in 2:400){
    lines(T, log(temp[i,]), col = "#00000010", lwd = 0.5)
  }
  
  points(as.numeric(country_data$time), log(country_data$sum_dead), type = "p", col = "red", pch = 16)
  points(as.numeric(country_data_rest$time), log(country_data_rest$sum_dead), type = "p", col = "purple", pch = 16)
  lines(T,log(colMeans(temp)), col = "green", lwd = 2)
  legend(legend_pos, legend=c("Observed Data since June 25th", "Data used for model fitting", "Posterior Mean", "Posterior Samples"), 
         col = c("purple","red", "green", "black"), lty = c(NA,NA,1,1), pch = c(16,16,NA,NA), cex=legend.cex)

}
plot_country_osc <- function(country_name, legend_pos = "topright", cex = 1.0, legend.cex = 1.0){
  
  country_data = canada_US %>% filter(country == country_name) %>% group_by(time) %>% summarise(sum_dead = sum(dead))
  country_data_rest = rest_observed %>% filter(country == country_name) %>% group_by(time) %>% summarise(sum_dead = sum(dead))
  maxy = max(c(log(country_data$sum_dead)),log(country_data_rest$sum_dead))
  brazil_regions <- regions$region[which(regions$country == country_name)]
  
  temp = matrix(0, nrow = 400, ncol = length(T))
  
  for (region in brazil_regions){
    temp <- temp + plot_province_osc(region, country_name, plot = FALSE)
  }
  
  plot(T, log(temp[1,]), col = "#00000010", type = "l", ylim = c(0, 1.2*maxy), lwd = 0.5, ylab = "Daily Deaths", yaxt = "n", xlab = "", cex.lab=cex, cex.axis=cex, cex.main=cex, cex.sub=cex)
  axis(side = 2, at = log(forYcount), labels = forYtext, cex.axis = cex)
  
  for (i in 2:400){
    lines(T, log(temp[i,]), col = "#00000010", lwd = 0.5)
  }
  
  points(as.numeric(country_data$time), log(country_data$sum_dead), type = "p", col = "red", pch = 16)
  points(as.numeric(country_data_rest$time), log(country_data_rest$sum_dead), type = "p", col = "purple", pch = 16)
  lines(T,log(colMeans(temp)), col = "green", lwd = 2)
  legend(legend_pos, legend=c("Observed Data since June 25th", "Data used for model fitting", "Posterior Mean", "Posterior Samples"), 
         col = c("purple","red", "green", "black"), lty = c(NA,NA,1,1), pch = c(16,16,NA,NA), cex=legend.cex)
  
}
plot_country_cum <- function(country_name, legend_pos = "bottomright", plot_factor = 0.01, cex=1.0, legend.cex = 1.0){
  
  country_data = canada_US %>% filter(country == country_name) %>% group_by(time) %>% summarise(dead = sum(dead))
  country_data_rest = rest_observed %>% filter(country == country_name) %>% group_by(time) %>% summarise(dead = sum(dead))
  maxy = max(log(sum(c(country_data_rest$dead, country_data$dead))))
  brazil_regions <- regions$region[which(regions$country == country_name)]
  miny = max(log(sum(country_data$dead)))
  
  
  temp = matrix(0, nrow = 400, ncol = length(T))
  
  
  
  replace_indices = T %>%data.frame() #need to replace posterior with actual data
  names(replace_indices) = "time"
  replace_indices <- replace_indices %>% left_join(country_data, by = "time") %>% select(time, dead)
  replace_indices <- which(!is.na(replace_indices$dead))
  
  for (region in brazil_regions){
    temp <- temp + plot_province(region, country_name, plot = FALSE)
  }
  temp[1, replace_indices] = country_data$dead
  
  plot(T[which(T>"2020-06-28")], log(cumsum(temp[1,]))[which(T>"2020-06-28")], col = "#00000010", ylim = c(exp(-plot_factor)*miny, exp(plot_factor)*maxy), type = "l",lwd = 0.5, 
       ylab = "Cumulative Deaths", yaxt = "n", xlab="", cex.lab=cex, cex.axis=cex, cex.main=cex, cex.sub=cex)
  axis(side = 2, at = log(forYcount[which(forYcount > exp(-plot_factor)*miny)]), labels = forYtext[which(forYcount > exp(-plot_factor)*miny)], cex.axis=cex)
  
  for (i in 2:400){
    temp[i, replace_indices] = country_data$dead
    lines(T, log(cumsum(temp[i,])), col = "#00000010", lwd = 0.5)
  }
  
  #points(as.numeric(country_data$time), log(cumsum(country_data$dead)), type = "p", col = "red", pch = 16)
  points(as.numeric(country_data_rest$time), log(sum(country_data$dead)+cumsum(country_data_rest$dead)), type = "p", col = "purple", pch = 16)
  lines(T,log(cumsum(colMeans(temp))), col = "green", lwd = 2)
  legend(legend_pos, legend=c("Observed Data since June 25th", "Posterior Mean", "Posterior Samples"), 
         col = c("purple", "green", "black"), lty = c(NA,1,1), pch = c(16,NA,NA), cex=legend.cex)
  
}



###Brazil
pdf("D:\\01_PhD\\Covid\\dow manuscript\\Figures\\brazil_daily_osc.pdf", width =6, height =6 )
plot_country_osc("Brazil", legend_pos = "bottomright", cex = 1.2)
dev.off()

# pdf("D:\\01_PhD\\Covid\\dow manuscript\\Figures\\brazil_cum.pdf", width =6, height =6 )
# plot_country_cum("Brazil", plot_factor = 0.05, cex=1.2)
# dev.off()

pdf("D:\\01_PhD\\Covid\\dow manuscript\\Figures\\sao_paulo_daily_osc.pdf", width =6, height =6 )
plot_province_osc("São Paulo", "Brazil", legend_pos = "bottomright", cex=1.2)
dev.off()

# pdf("D:\\01_PhD\\Covid\\dow manuscript\\Figures\\sao_paulo_cum.pdf", width =6, height =6 )
# plot_province_cum("São Paulo", "Brazil", plot_factor = 0.05, cex=1.2)
# dev.off()

pdf("D:\\01_PhD\\Covid\\dow manuscript\\Figures\\acre_daily_osc.pdf", width =6, height =6 )
plot_province_osc("Acre", "Brazil", cex=1.2)
dev.off()

# pdf("D:\\01_PhD\\Covid\\dow manuscript\\Figures\\acre_cum.pdf", width =6, height =6 )
# plot_province_cum("Acre", "Brazil", plot_factor=0.1, cex=1.2)
# dev.off()


###U.S

pdf("D:\\01_PhD\\Covid\\dow manuscript\\Figures\\US_daily_osc.pdf", width =6, height =6 )
plot_country_osc("United States", legend_pos = "bottomright", cex = 1.2)
dev.off()

# pdf("D:\\01_PhD\\Covid\\dow manuscript\\Figures\\US_cum.pdf", width =6, height =6 )
# plot_country_cum("United States", plot_factor = 0.02, cex=1.2)
# dev.off()

pdf("D:\\01_PhD\\Covid\\dow manuscript\\Figures\\illinois_daily_osc.pdf", width =6, height =6 )
plot_province_osc("Illinois", "United States", legend_pos = "topright", cex=1.2)
dev.off()

# pdf("D:\\01_PhD\\Covid\\dow manuscript\\Figures\\illinois_cum.pdf", width =6, height =6 )
# plot_province_cum("Illinois", "United States", plot_factor = 0.05, cex=1.2)
# dev.off()

pdf("D:\\01_PhD\\Covid\\dow manuscript\\Figures\\california_daily_osc.pdf", width =6, height =6 )
plot_province_osc("California", "United States", cex=1.2, legend_pos = "bottomright")
dev.off()

# pdf("D:\\01_PhD\\Covid\\dow manuscript\\Figures\\california_cum.pdf", width =6, height =6 )
# plot_province_cum("California", "United States", plot_factor=0.05, cex=1.2)
# dev.off()

#Canada and Spain

pdf("D:\\01_PhD\\Covid\\dow manuscript\\Figures\\canada_daily_osc.pdf", width =6, height =6 )
plot_country_osc("Canada", legend_pos = "topright", cex = 1.2)
dev.off()

# pdf("D:\\01_PhD\\Covid\\dow manuscript\\Figures\\canada_cum.pdf", width =6, height =6 )
# plot_country_cum("Canada", plot_factor = 0.07, cex=1.2)
# dev.off()

pdf("D:\\01_PhD\\Covid\\dow manuscript\\Figures\\spain_daily_osc.pdf", width =6, height =6 )
plot_country_osc("Spain", legend_pos = "topright", cex = 1.2)
dev.off()

# pdf("D:\\01_PhD\\Covid\\dow manuscript\\Figures\\spain_cum.pdf", width =6, height =6 )
# plot_country_cum("Spain", plot_factor = 0.1, cex=1.2)
# dev.off()

#day of the week plot
dow_effects <- apply(test, c(2,3), quantile, probs = c(0.025,0.5,0.975))[,-4,] %>% aperm(perm = c(2,1,3) ) %>% apply(2, function(x) x) %>% data.frame() %>% 
  mutate(day = rep(c("Friday", "Monday", "Saturday", "Thursday", "Tuesday", "Wednesday"), nrow(countries)))

dow_effects <- dow_effects %>% mutate(country = floor(as.numeric(rownames(dow_effects))/6 + 0.99)) 

dow_effects$country_name = countries$country[dow_effects$country]

order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

pdf("D:\\01_PhD\\Covid\\dow manuscript\\Figures\\dow_bycountry.pdf", width =8, height =6 )
ggplot(data = dow_effects, aes(x= day, y= X50., col = country_name)) +
  geom_point( size = 2,position = position_dodge(width = 0.4))+
  geom_errorbar(aes(ymin=X2.5.,ymax = X97.5.), size = 1.2, width = 0.5, position = position_dodge(width = 0.4))+
  scale_x_discrete ( limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))+
  labs( x="",y = "Relative Risk", color = NULL)+
  theme_bw() +
  theme(text=element_text(size=15))
dev.off()

###Figure 1 of manuscript

###
pdf(file = "dow manuscript\\Figures\\first_wave_US.pdf", width = 7, height =5)
par(mar = c(3, 6, 3, 3))
plot_US =canada_US %>% filter(country == "United States") %>% group_by(time) %>% summarise(dead=sum(dead)) %>% mutate(dead= ifelse(dead==0, 1,dead))
maxy = max(log(plot_US$dead))
plot(plot_US$time, log(plot_US$dead), type = "l", ylim = c(0, 1.2*maxy), lwd = 1, ylab = "Daily Mortality Counts", xlab = "", yaxt = "n", cex.axis = 0.8)
axis(side = 2, at = log(forYcount), labels = forYtext, cex.axis = 0.8)
dev.off()

###make all plots to host on web
pdf(file = "dow manuscript\\Figures\\all_daily.pdf")
for (region in regions$region){
  if (region == "Acre"){legend_position = "topright"} else {legend_position = NULL}
  plot_province_osc(region, regions$country[which(region ==regions$region)], legend_pos = legend_position, cex=1.2, title = region)
}
dev.off()
