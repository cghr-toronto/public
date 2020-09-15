authorization_code = ""

match_Global_Region_Names <- function(popData) { 
  popData$country = gsub("[[:space:]]?[(].*[)]", "", popData$Location)
  rename= c('Republic of Korea' = 'South Korea',
    'Russian Federation' = 'Russia',
    'Viet Nam' = 'Vietnam',
#    'Czechia' = 'Czech Republic',
    'Republic of Moldova' = 'Moldova',
    'District of Columbia' = 'Washington, D.C.',
    'United States of America'= 'United States')
  for(D in names(rename))
    popData$country = gsub(D, rename[D], popData$country)
  return( popData )
}

scale_Global_Population_Data <- function(popData) { 
  popData$pop = 1000*popData$PopTotal
  return( popData )
}


global_Pop_Data <- function() {
  popFile = Pmisc::downloadIfOld(
    'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationByAgeSex_OtherVariants.csv')
  popData = read.csv(popFile)
  popData = popData[popData$Time == 2020 &
    popData$Variant == 'Median PI', ]
  popData <- match_Global_Region_Names(popData)
  popData <- scale_Global_Population_Data(popData)

  return( popData )
}

# discard_Global_NonMatching_Names <- function(popData) { 
#  unique(nCov_data$global$country)[!
#  unique(nCov_data$global$country) %in% popData$country]
#  as.character(popData$country)[
#  !popData$country %in% unique(nCov_data$global$country)]
# }


us_Pop_Data <- function() { 
  usPopFile = Pmisc::downloadIfOld(
    'https://www2.census.gov/programs-surveys/popest/tables/2010-2018/state/asrh/PEP_2018_PEPASR6H.zip')
  usPopFile = usPopFile[which.max(file.info(usPopFile)$size)]
  usPop = read.csv(usPopFile, stringsAsFactors=FALSE, header=TRUE, skip=1)
  names(usPop) <- us_Pop_Data_Headers()
  usPop <- process_US_Pop_Data(usPop)
  usPop$state = gsub("District of Columbia", 'Washington, D.C.', usPop$state)
  return( usPop )
}

us_Pop_Data_Headers <- function() { 
  usPopFile = Pmisc::downloadIfOld(
    'https://www2.census.gov/programs-surveys/popest/tables/2010-2018/state/asrh/PEP_2018_PEPASR6H.zip')
  usPopFile = usPopFile[which.max(file.info(usPopFile)$size)]
  usPopHead = read.csv(usPopFile, stringsAsFactors=FALSE, header=FALSE, nrow=1)
  return( unlist(usPopHead) ) 
}

process_US_Pop_Data <- function(usPop) {
  # Subset 
  usPop = usPop[grep("2018", usPop$'year.display-label'), ]
  usPop = usPop[grep("Total", usPop$'hisp.display-label'), ]
  usPop = usPop[,grep("GEO.display|totpop_sex0_age([[:digit:]]+to|85)", names(usPop))]
  usPop = reshape2::melt(usPop, id.vars = 'GEO.display-label', value.name='pop' )
  # Age
  usPop$age = gsub("^.*[_]age", "", usPop$variable)
  usPop = usPop[grep("_$", usPop$age, invert=TRUE), ]
  usPop$age = gsub("plus", 'to120', usPop$age)
  usPop$ageLow = as.numeric(gsub("[[:alpha:]].+", "", usPop$age))
  usPop$ageHigh = as.numeric(gsub(".*[[:alpha:]]", "", usPop$age))
  usPop$ageDiff= usPop$ageHigh - usPop$ageLow
  usPop = usPop[usPop$ageDiff == 4 | usPop$ageLow == 85, ]
  # Name columns
  colnames(usPop) = gsub("GEO.*", "state", colnames(usPop))
  return(usPop)
}


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


china_Expected <- function(chinaPop, ratesByAge) {
  chinaPop$rate = ratesByAge[as.character(chinaPop$ageCut)]
  chinaPop$Expected = chinaPop$rate * chinaPop$pop
  suppressWarnings(chinaMelt <- reshape2::melt(chinaPop, 
    id.vars=c('province','ageLow','ageHigh','ageCut'),
    measure.vars = c('pop','Expected')))
  chinaExpected = reshape2::dcast(chinaMelt, province ~ variable,
    fun.aggregate=sum, value.var='value')
  chinaExpected$mMult = 1e6/sum(chinaExpected$pop)
  return(chinaExpected)
}

get_covid_expected_counts = function() {
  # Run global population data processing functions
popData <- global_Pop_Data()

# Run US population data processing functions
usPop <- us_Pop_Data()

# table 2.12
# china http://www.stats.gov.cn/tjsj/ndsj/2018/indexeh.htm
# http://www.stats.gov.cn/tjsj/ndsj/2018/html/EN0212.jpg


Sage = c(0, 15, 65, Inf)
popData$ageCutChina = cut(popData$AgeGrpStart, Sage, right=FALSE)
popItaly = popData[popData$country == 'Italy',]
popItalyAgg = tapply(popItaly$pop, 
  cut(popItaly$AgeGrpStart, Sage, right=FALSE), 
  sum)

deathsByAge = c(0, 1+20+81+1073/2, 1073/2+3206+3652+845)
names(deathsByAge) = levels(popData$ageCutChina)
ratesByAge = deathsByAge/popItalyAgg[names(deathsByAge)]

chinaPop = data.frame(
  province = 'Hubei',ageLow = c(0,15, 65), ageHigh=c(14,64,Inf), 
  pop = c(7701, 35028, 5955)/0.000824)
chinaPop$ageCut = cut(chinaPop$ageLow, Sage, right=FALSE)

chinaExpected <- china_Expected(chinaPop, ratesByAge)


# https://www.epicentro.iss.it/coronavirus/bollettino/Infografica_29marzo%20ENG.pdf

ratesByAge = data.frame(age = c(0,10,20,30,40,50,60,70,80,90),
  deaths = c(0,0,1,20,81,340,1073,3206,3652,845))
popItaly = popData[popData$country == 'Italy',]
ratesByAge$pop = tapply(popItaly$pop, 
  cut(popItaly$AgeGrpStart, c(ratesByAge$age, Inf), right=FALSE), 
  sum)
ratesByAge$rate = ratesByAge$deaths / ratesByAge$pop

worldExpected <- world_Expected(popData, ratesByAge)
usExpected <- world_Expected(usPop, ratesByAge, ageVar = 'ageLow',
  regionVar = 'state')

# total us population and mmult
usData = as.data.frame(lapply(
  usExpected[,c('Expected','pop')], sum))

usData$state = 'United States'
usData$mMult = 1e6/usData$pop
usExpected= rbind(usExpected, usData[,colnames(usExpected)])
usExpected= usExpected[
  usExpected$state != 'United States', ]
  list(china = chinaExpected, us = usExpected, world = worldExpected)
}

get_covid_data_cva_db = function(covid_expected) {
  RPostgreSQL::PostgreSQL()
  drv <- DBI::dbDriver("PostgreSQL")
  con = DBI::dbConnect(drv,
      dbname='covid_19',
      host='68.169.59.43',
      user='brownpa',
      password='brownpa1505')
  nCov_DB <- DBI::dbGetQuery(con, "SELECT * FROM cvapp_covid19_v1")
  nCov_DB$countryCode = nCov_DB$country
  nCov_DB$country = countrycode::countrycode(nCov_DB$countryCode, 
    origin = 'iso2c', destination='country.name')
  nCov_DB_agg = aggregate(nCov_DB$dead, nCov_DB[,c('countryCode','country','day')], 
    sum)
  nCov_DB_agg$reporting_unit = nCov_DB_agg$country
  names(nCov_DB_agg) = gsub("^x$", 'dead', names(nCov_DB_agg))

  nCov_DB_subnational = nCov_DB[nCov_DB$countryCode %in% c('US','CN'), ]
  nCov_DB_subnational = nCov_DB_subnational[
    grep("Unknown", nCov_DB_subnational$place, invert=TRUE), ]
  nCov_DB_subnational$reporting_unit = nCov_DB_subnational$place  

  toBindCols = c('country','reporting_unit','day','countryCode','dead')
  nCov_All_bind = rbind(
    nCov_DB_agg[,toBindCols],
    nCov_DB_subnational[,toBindCols]
    )
  totalCounts = tapply(nCov_All_bind$dead, nCov_All_bind$reporting_unit, max)

  nCov_sub = nCov_All_bind[
      nCov_All_bind$reporting_unit %in% names(which(totalCounts>200)),]
  colnames(nCov_sub) = gsub("^day$", "time", colnames(nCov_sub))
  colnames(nCov_sub) = gsub("^dead$", "cum_dead", colnames(nCov_sub))

  nCov_combined = split(nCov_sub, nCov_sub[,'reporting_unit', drop=FALSE])

for (ii in 1:length(nCov_combined)) {
  nCov_combined[[ii]] =   nCov_combined[[ii]][order(  nCov_combined[[ii]]$time), ]
  nCov_combined[[ii]]$dead = 
    diff(c(0, nCov_combined[[ii]]$cum_dead))
  # look for negatives
  getRid = which(nCov_combined[[ii]]$dead < 0)
  if(length(getRid)) {
    getRid = max(getRid)
    nCov_combined[[ii]] = nCov_combined[[ii]][-(1:getRid),]
  }
  # day after first death
  firstDeath = min(c(Inf,which(nCov_combined[[ii]]$dead>0)))
  if(firstDeath < nrow(nCov_combined[[ii]])) {
    nCov_combined[[ii]] = 
      nCov_combined[[ii]][seq(firstDeath+1, nrow(nCov_combined[[ii]])),]
  }
}

nCov_All_counts = do.call(rbind, nCov_combined)
covid_expected$us$state = gsub("Georgia$", "Georgia (US)", covid_expected$us$state)
covid_expected$us$reporting_unit = covid_expected$us$state

covid_expected$world$reporting_unit = covid_expected$world$country
covid_expected$china$reporting_unit = covid_expected$china$province

toBindExp =     c('reporting_unit','pop','Expected','mMult')
covid_expected_bind = rbind(
  covid_expected$us[,toBindExp],
  covid_expected$world[,toBindExp],
  covid_expected$china[,toBindExp]
  )

nCov_All = merge(
  nCov_All_counts,
  covid_expected_bind,
  all.x=TRUE, all.y=FALSE, by = 'reporting_unit')
nCov_All$timeNumeric = as.numeric(nCov_All$time)
nCov_All$reporting_unit_fac = factor(nCov_All$reporting_unit)
nCov_All$reporting_unit_int = as.integer(nCov_All$reporting_unit_fac)
nCov_All$logExpected = log(nCov_All$Expected)
nCov_All
}



#curl -X GET 'https://coronavirus.app/get-places' -H 'authorization: Bearer XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX' -H 'content-type: application/json'
getPlaces = function(theHeaders, deathThreshold = 50) {
xPlace = httr::content(httr::GET(
  'https://coronavirus.app/get-places',
  theHeaders
  ))$data
xPlace = data.frame(
  id = unlist(Biobase::subListExtract(xPlace, 'id')),
  country = unlist(Biobase::subListExtract(xPlace, 'country')),
  dead = unlist(Biobase::subListExtract(xPlace, 'dead')),
  name = unlist(Biobase::subListExtract(xPlace, 'name')),
  stringsAsFactors=FALSE)
xPlace$totalDead = unlist(tapply(xPlace$dead, xPlace$country, sum))[
  xPlace$country]
xPlace = xPlace[xPlace$totalDead > 50 & xPlace$dead > 0, ]
xPlace = xPlace[xPlace$country != 'CN' | xPlace$name == 'Hubei', ]
xPlace
}
getCountry = function(id, headers) {
x2 = httr::GET(
 paste0('https://coronavirus.app/get-history?id=', id),
 headers)
xHere = data.frame(
  date = 
    as.Date(unlist(
      Biobase::subListExtract(httr::content(x2)$data$history, 'day')
      ), format='%Y%m%d'),
  cum_dead = unlist(
      Biobase::subListExtract(httr::content(x2)$data$history, 'dead')
      ),
  countryCode = httr::content(x2)$data$country,
  name = httr::content(x2)$data$name
  )
  xHere
}
get_covid_data_cva = function(covid_expected, min_deaths=100) {
cacheFile = 'cvaCache.rds'
reload = !file.exists(cacheFile)
if(!reload) {
  if(as.numeric(diff(as.Date(c(file.info(cacheFile)$mtime, Sys.time()))))) {
    reload = TRUE
  }
}
if(reload) {
    message("reloading")
theHeaders =   httr::add_headers(
  authorization = paste0("authorization: Bearer ", authorization_code),
    'content-type' = 'application/json')
xPlace = getPlaces(theHeaders, min_deaths)
xPlace = xPlace[!(xPlace$country %in% c('FR','BE')),]
    xAllRaw = mapply(getCountry, id=xPlace$id, MoreArgs = list(headers = theHeaders),
        SIMPLIFY=FALSE)
   saveRDS(xAllRaw, cacheFile)
 } else {
xAllRaw = readRDS(cacheFile)
}

  nCov_DB = do.call(rbind, xAllRaw)
  nCov_DB_agg = aggregate(nCov_DB$cum_dead, nCov_DB[,c('countryCode','date')], 
    sum)
  names(nCov_DB_agg) = gsub("^x$", 'cum_dead', names(nCov_DB_agg))

  nCov_DB_agg$country = countrycode::countrycode(
    nCov_DB_agg$countryCode, 
    origin = 'iso2c', destination='country.name')
  nCov_DB_agg$reporting_unit = nCov_DB_agg$country


  nCov_DB_subnational = nCov_DB[nCov_DB$countryCode %in% c('US','CN','CA', 'ES', 'BR'), ] #JS added CA,ES so we do not aggregate over the provinces
  nCov_DB_subnational = nCov_DB_subnational[
    grep("Unknown", nCov_DB_subnational$name, invert=TRUE), ]
  nCov_DB_subnational$reporting_unit = nCov_DB_subnational$name  
  nCov_DB_subnational$country = countrycode::countrycode(
    nCov_DB_subnational$countryCode, 
    origin = 'iso2c', destination='country.name')

  nCov_DB_subnational$reporting_unit = gsub("Georgia$", "Georgia (US)", 
    nCov_DB_subnational$reporting_unit)

  toBindCols = c('country','reporting_unit','date','countryCode','dead','cum_dead')
  nCov_All_list = lapply(
    c(split(nCov_DB_agg, nCov_DB_agg$reporting_unit), 
      split(nCov_DB_subnational, nCov_DB_subnational$reporting_unit)),
    function(xx) {
      xx = xx[order(xx$date), ]
      xx$dead = diff(c(0, xx$cum_dead))
      xx = xx[which(xx$cum_dead > 0),]

  # day after first death
  firstDeath = min(c(Inf,which(xx$dead>0)))
  if(firstDeath < nrow(xx)) {
    xx = xx[seq(firstDeath+1, nrow(xx)),]
  }
    xx[,toBindCols]
    })


  nCov_All_bind = do.call(rbind, nCov_All_list)
  totalCounts = tapply(nCov_All_bind$cum_dead, nCov_All_bind$reporting_unit, max)

  nCov_sub = nCov_All_bind[
      nCov_All_bind$reporting_unit %in% names(which(totalCounts>min_deaths)),]
  colnames(nCov_sub) = gsub("^date$", "time", colnames(nCov_sub))



covid_expected$us$state = gsub("Georgia$", "Georgia (US)", covid_expected$us$state)
covid_expected$us$reporting_unit = covid_expected$us$state

covid_expected$world$reporting_unit = covid_expected$world$country
covid_expected$china$reporting_unit = covid_expected$china$province

toBindExp =     c('reporting_unit','pop','Expected','mMult')
covid_expected_bind = rbind(
  covid_expected$us[,toBindExp],
  covid_expected$world[,toBindExp],
  covid_expected$china[,toBindExp]
  )

nCov_All = merge(
  nCov_sub,
  covid_expected_bind,
  all.x=TRUE, all.y=FALSE, by = 'reporting_unit')

nCov_All[order(nCov_All$reporting_unit, nCov_All$time), ]

}

get_covid_data = function(covid_expected){
chinaExpected = covid_expected$china
usExpected = covid_expected$us 
worldExpected = covid_expected$world


suppressWarnings(
  nCov_data <- nCov2019::load_nCov2019(lang = 'en', source = "github")
)




# Process-Global-Data
nCov_data$global2 = merge(nCov_data$global, worldExpected,
  by='country')
nCov_data$global3= nCov_data$global2[
  order(nCov_data$global2$country, nCov_data$global2$time), ]
nCov_data$global3$reporting_unit = nCov_data$global3$country
nCov_global <- split(nCov_data$global3, nCov_data$global3[,"reporting_unit"])

# Process-Province-Data
nCov_data$province2 = merge(nCov_data$province, chinaExpected,
  by='province', all=TRUE)
nCov_data$province2= nCov_data$province2[
  order(nCov_data$province2$province, nCov_data$province2$time), ]
#nCov_data$province2$country = 'China'
nCov_data$province2$reporting_unit = nCov_data$province2$province
nCov_province <- split(nCov_data$province2, nCov_data$province2[,"province"])

# usData
# https://github.com/nytimes/covid-19-data/
nCov_US = read.csv(url(
"https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
  ), stringsAsFactors=FALSE)

nCov_US = data.frame(
  time = as.Date(nCov_US$date),
  reporting_unit = nCov_US$state,
  cum_confirm = nCov_US$cases, 
  cum_heal=NA, cum_dead = nCov_US$deaths,
  country = 'United States')

nCov_US = merge(nCov_US, usExpected, by.x='reporting_unit', by.y='state', all=TRUE)
nCov_US = nCov_US[order(nCov_US$reporting_unit, nCov_US$time), ]
nCov_US_split = split(nCov_US, nCov_US$reporting_unit)

nCov_combined = c(nCov_global, nCov_province, nCov_US_split)
for (ii in 1:length(nCov_combined)) {
  nCov_combined[[ii]]$dead = 
    diff(c(0, nCov_combined[[ii]]$cum_dead))
  # look for negatives
  getRid = which(nCov_combined[[ii]]$dead < 0)
  if(length(getRid)) {
    getRid = max(getRid)
    nCov_combined[[ii]] = nCov_combined[[ii]][-(1:getRid),]
  }
  # day after first death
  firstDeath = min(c(Inf,which(nCov_combined[[ii]]$dead>0)))
  if(firstDeath < nrow(nCov_combined[[ii]])) {
    nCov_combined[[ii]] = 
      nCov_combined[[ii]][seq(firstDeath+1, nrow(nCov_combined[[ii]])),]
  }
}


# clean french data
#April 4 (GMT)
#7788 new cases and 1053 new deaths in France [source]
#https://www.worldometers.info/coronavirus/country/france/
# France: on April 3 the French Government reported 17,827 additional cases and 532 additional deaths from nursing homes that had not been reported previously. On April 2, it had reported 884 additional deaths.
#April 3 (GMT)
#23060 new cases and 1120 new deaths in France
# On April 2, France reported 884 additional deaths that have occurred in nursing homes 
#toFix = grep("2020-04-0[34]",as.character(nCov_combined$France$time))
#nCov_combined$France[toFix, 'dead'] = c(1355, 1120)- c(884,532)


# Combine-Data-At-Reporting-Unit-Level}
nCov_combined2 = lapply(nCov_combined, 
  function(xx) xx[,c('country','reporting_unit','time','dead','Expected','mMult','pop')] )
nCov_All = do.call(rbind, nCov_combined2)
#nCov_All = nCov_All[
#  grep("China|United States", nCov_All$reporting_unit, invert=TRUE),] # remove China - have provinces
# remove regions with fewer than 20 death
nCov_All = nCov_All[!is.na(nCov_All$time), ]
whichOver = names(which(tapply(nCov_All$dead,
   nCov_All$reporting_unit, sum, na.rm=TRUE)>200))
#ßwhichOver = c('Hubei','Italy','Spain', 'New York', 'Canada', 
#               'Washington','France','United Kingdom')
nCov_All = nCov_All[nCov_All$reporting_unit %in% whichOver, ]
nCov_All = nCov_All[!is.na(nCov_All$Expected), ]
nCov_All$reporting_unit = factor(as.character(nCov_All$reporting_unit))
nCov_All$reporting_unit_int = as.integer(nCov_All$reporting_unit)
nCov_All$timeNumeric = as.numeric(nCov_All$time)
nCov_All$logExpected = log(nCov_All$Expected)

nCov_US = nCov_All[nCov_All$country == 'United States' & 
    !nCov_All$reporting_unit == 'United States', ]
nCov_US_agg = aggregate(
  nCov_US[,c('dead','Expected','pop')],
  nCov_US[,c('country','time')],
   FUN=sum, na.rm=TRUE)

nCov_US_agg[c('cum_confirm','cum_heal','cum_dead',
  'timeNumeric','logExpected','reporting_unit_int')] = NA
nCov_US_agg$reporting_unit = 'United States'
nCov_US_agg$mMult = 1e6/nCov_US_agg$pop
nCov_US_agg = nCov_US_agg[,names(nCov_All)]

  list(all=nCov_All, us=nCov_US_agg)
}

# forecast.R
forecast_ahead = function(region_name, samples, allSamples, Stime, data, phi_samples) {

  Nk = 100
  Sprob = c(0.01, 0.025, 0.1, 0.25,0.5)
  Sglobal = c(0.25,0.5, 0.8,0.95)

  Sprob= sort(unique(c(Sprob, 1-Sprob)))

  StimeInt = as.numeric(Stime)
  if(missing(samples)) {
    samples = allSamples[,region_name,]
  }
  sampleHere = samples

  Nfull = dim(sampleHere)[1]
  Nk <- min(c(Nfull, Nk))
  k = sort(unique(round(seq(1, Nfull, len=Nk))))


  if(!missing(region_name)) {
    if(region_name %in% data$reporting_unit) {
      data = data[data$reporting_unit == region_name, ]
    }    
  }
  theOffset = data[1,'Expected']



  lambdaSansEta = exp(rep(
  	log(theOffset) + log(sampleHere[,'C']), 
  	each=length(StimeInt)) +
      sn::dsn(
      rep(StimeInt, nrow(sampleHere)),
      rep(sampleHere[,'A'], each=length(StimeInt)),
      rep(sampleHere[,'B'], each=length(StimeInt)),
      rep(sampleHere[,'K'], each=length(StimeInt)),      
      log=TRUE
    )) 
  lambdaSansEta = matrix(lambdaSansEta, length(StimeInt), nrow(sampleHere))
  lambda = lambdaSansEta + rep(sampleHere[,'eta'], each=length(StimeInt))


  #if(FALSE) {
    cases = matrix(
      MASS::rnegbin(
      	n=length(lambda), 
        mu=as.vector(lambda), 
        theta = rep(phi_samples, each=length(lambda))),
      nrow = nrow(lambda))
    cases[is.na(cases)] = 1e10
    cases[cases>1e10] = 1e10
  
    if(!missing(data)) {
      data = data[order(data$time), ]
      StimeBefore = Stime[Stime < min(data$time)]
      StimeInRange = data$time
      StimePredict = setdiff(Stime, c(StimeBefore, StimeInRange))  

    casesWithData1 = rbind(
      matrix(0, length(StimeBefore), ncol(cases)),
      as.matrix(data[, rep('dead', ncol(cases))]),
      cases[match(StimePredict, Stime), ]
      )
    casesWithData1 = casesWithData1[order(c(
      StimeBefore, StimeInRange, StimePredict)), ]
    casesWithData = apply(casesWithData1,2,cumsum)
  } else {
    casesWithData = NULL
  }

  # when epidemic is less than one case per day
  indSeq = seq(1, dim(lambda)[1])
  whenOver = t(apply(lambdaSansEta, 2, function(xx) {
    postMode = which.max(xx)
    c(peak=StimeInt[postMode], 
      over=StimeInt[min(which(indSeq > postMode & xx < 1))])
  }))
  whenOver = cbind(whenOver, maxDaily = apply(cases, 2, max))

  result = list(cases=cases, lambda = lambda, 
                cumulative = casesWithData,
                peak = whenOver)

  forecastQuant = forecast_quant(result, Stime, Sglobal, Sprob)

if(FALSE) {
  if(!missing(region_name)) {
    forecastQuant$reporting_unit = region_name
  } else if(!missing(data)){
    forecastQuant$reporting_unit = data$reporting_unit[1]    
  }
}
  result$intervals = forecastQuant
  result
}


forecast_quant = function(x, Stime,   
  Sglobal = c(0.25,0.5, 0.8,0.95),
  Sprob = c(0.01, 0.025, 0.1, 0.25,0.5)
  ){
  Sprob= sort(unique(signif(c(Sprob, 1-Sprob), 10)))

  Soutcome = c(cases = 'cases', lambda = 'intensity')

  forecastQuant = data.frame(time = Stime,
    central_cases = apply(x$cases, 1, median),
    central_intensity = apply(x$lambda, 1, median))


 for(Doutcome in names(Soutcome)) {
   for(Dglobal in Sglobal) {
    bob = as.data.frame(GET::central_region(GET::create_curve_set(list(
      r = as.numeric(Stime),
      obs=x[[Doutcome]]
    )), coverage = Dglobal))[,c('lo','hi')]

    colnames(bob) = paste(colnames(bob), Dglobal, 
                          Soutcome[Doutcome], sep='_')
    forecastQuant = cbind(forecastQuant, bob)

  }}

  bobCumul = 
    t(apply(x$cumulative, 1, quantile, prob=Sprob))
  colnames(bobCumul) = paste(colnames(bobCumul), 'cumulative', sep='_')

  theCbind = cbind(forecastQuant, as.data.frame(bobCumul))


  theCbind
}


covid_forecast = function(theSamples, nCov_fit, mc.cores=4, min_deaths=50,
  regionNamesAll) {

Stime = seq(as.Date("2020/01/01"), as.Date("2020/09/30"), by = '1 days')
nDeaths= tapply(nCov_fit$data$dead, nCov_fit$data$reporting_unit, sum)
if(missing(regionNamesAll))
  regionNamesAll = names(nDeaths[which(nDeaths >= min_deaths)])
#regionNamesAll = c('Hubei','Italy','Spain')

if(!all(regionNamesAll %in% dimnames(theSamples)[[2]])) {
  warning("regions missing", paste(
    regionNamesAll[!regionNamesAll %in% dimnames(theSamples)[[2]]]), collapse=', ')
}

theForecasts = parallel::mcmapply(forecast_ahead, 
  region_name = regionNamesAll,
  SIMPLIFY=FALSE, mc.cores=mc.cores,
  MoreArgs = list(data = nCov_fit$data, Stime = Stime,
                  allSamples = theSamples, phi_samples = nCov_fit$phi))
names(theForecasts) = regionNamesAll

# aggregate US
theStates = setdiff(
  unique(as.character(nCov_fit$data[nCov_fit$data$country == 'United States', 
  'reporting_unit'])),
  'United States')

theForecastUsSeparate = theForecasts[
  names(theForecasts) %in% as.character(theStates)]
theForecastUs = list()
if(length(theForecastUsSeparate)) {
for(DD in setdiff(names(theForecastUsSeparate[[1]]),c('peak','intervals'))) {
theForecastUs[[DD]] = Reduce('+',
  Biobase::subListExtract(theForecastUsSeparate, DD))
}


theForecastUs$intervals = forecast_quant(
  theForecastUs, Stime)
theForecastUs$intervals$reporting_unit = 'United States'


  # when epidemic is less than one case per day
  indSeq = seq(1, dim(theForecastUs$lambda)[1])
  whenOver = t(apply(theForecastUs$lambda, 2, function(xx) {
    postMode = which.max(xx)
    suppressWarnings(xxx <- c(peak=as.numeric(Stime)[postMode], 
      over=as.numeric(Stime)[min(which(indSeq > postMode & xx < 10))]))
  }))
  whenOver = cbind(whenOver, maxDaily = apply(theForecastUs$cases, 2, max))
  whenOver[is.na(whenOver[,'over']), 'over']= 99999
  theForecastUs$peak = whenOver

theForecasts$'United States Aggregated' = theForecastUs
}

list(forecasts=theForecasts, time=Stime)
}


forecast_summaries = function(
  theForecasts, Stime, threshold=NULL,
  nCov_data
    ) {
Nk = 100
Nfull = ncol(theForecasts[[1]]$cases)
Nk <- min(c(Nfull, Nk))
k = sort(unique(round(seq(1, Nfull, len=Nk))))

forecastsLambda = forecastQuant = forecastSummaries = list()
Sprob = c(0.01, 0.025, 0.1, 0.25,0.5)
Sglobal = c(0.25,0.5, 0.8,0.95)
Sprob= sort(unique(c(Sprob, 1-Sprob)))
Soutcome = c(cases = 'cases', lambda = 'intensity')


for (D in names(theForecasts)) {

  theForecasts[[D]]$cases[is.na(theForecasts[[D]]$cases)] = 1e10
  theForecasts[[D]]$cases[theForecasts[[D]]$cases>1e10] = 1e10
  theForecasts[[D]]$cumulative[is.na(theForecasts[[D]]$cumulative)] = 1e10
  theForecasts[[D]]$cumulative[is.na(theForecasts[[D]]$cumulative)] = 1e10

  forecastsLambda[[D]] = data.frame(
    region = D, 
    time = rep(Stime, length(k)),
    sample = rep(1:length(k), each=length(Stime)),
    intensity = as.vector(theForecasts[[D]]$lambda[,k]),
    cases = as.vector(theForecasts[[D]]$cases[,k]),
    cumulative = as.vector(theForecasts[[D]]$cumulative[,k])
    )
   # forecastQuant[[D]] = cbind(data.frame(region=D,
   #   time = Stime),
   #   t(apply(theForecasts[[D]]$cases, 1, quantile, na.rm=TRUE, prob=Sprob))    )

   forecastQuant[[D]] = theForecasts[[D]]$intervals
   forecastQuant[[D]]$reporting_unit = D
   forecastSummaries[[D]] = data.frame(
    reporting_unit = D,
    totalDeaths = theForecasts[[D]]$cumulative[
      nrow(theForecasts[[D]]$cumulative), ]
    )

   if(!is.null(theForecasts[[D]]$peak)) {
     forecastSummaries[[D]] = cbind(
         forecastSummaries[[D]],
         as.data.frame(theForecasts[[D]]$peak)
         )
   } else {
      for(D2 in colnames(theForecasts[[1]]$peak))
        forecastSummaries[[D]][[D2]]=NA
   }
   if(!is.null(threshold)) {
      if(!is.na(threshold[D])) {
        forecastSummaries[[D]]$overThreshold =
          apply(theForecasts[[D]]$cases > threshold[D],2, sum)
      }
    }
    if(!length(forecastSummaries[[D]]$overThreshold))
      forecastSummaries[[D]]$overThreshold = NA
}


toMerge = do.call(rbind, forecastQuant)

intervalsAndData1 = merge(
  toMerge,
  nCov_data[,c('reporting_unit','time','dead')], 
  by = c('time','reporting_unit'),
  all=TRUE)

toMerge2 = nCov_data[!is.na(nCov_data$mMult),]
toMerge3 = toMerge2[!duplicated(toMerge2$reporting_unit), ]

intervalsAndData = merge(
  intervalsAndData1,
  toMerge3[,c('reporting_unit','country','pop','Expected','mMult')],
  all=TRUE
  )
intervalsAndData = intervalsAndData[!is.na(intervalsAndData$central_cases), ]
intervalsAndData = intervalsAndData[order(intervalsAndData$reporting_unit, intervalsAndData$time), ]
#intervalsAndData$cum_dead = 

toSave = list(samples = do.call(rbind, forecastsLambda),
  intervals = intervalsAndData, summaries = do.call(rbind, forecastSummaries))
colnames(toSave$intervals) = gsub("[.]", "_", colnames(toSave$intervals))
colnames(toSave$intervals) = gsub("%", "pct", colnames(toSave$intervals))

toSave
}



covid_polygons = function(covIntervals, Sregion = unique(covIntervals$reporting_unit),
  Scol = NULL,
 verbose=FALSE) {
forecastPoly  = forecastPolyScaled = 
  list(cumulative=list(), intensity=list())
intervalMat = cbind(c(25,75), c(10,90), gsub("[.]", "_", c(2.5, 97.5)), c(1, 99))
intervalIntensity = outer(c('lo','hi'), 
  paste0('_0_', c(25,5,8,95), '_intensity'), paste0)

intervalString = matrix(paste0('X',intervalMat, 'pct_cumulative'), ncol=ncol(intervalMat))

opacityString = as.hexmode(floor(255*c(0.8, seq(0.5, 0.1, len=ncol(intervalMat)-1))))

toScale = c('X50pct_cumulative',as.vector(intervalString),
  as.vector(intervalIntensity), 'central_cases', 'dead',
  'central_intensity')

covIntervalsScaled = covIntervals
covIntervalsScaled[,toScale] = 
  covIntervalsScaled[,toScale] * covIntervals[,'mMult']
for(D in Sregion) {
  if(verbose) cat(D, ' ')
  xHere = covIntervalsScaled[which(covIntervalsScaled$reporting_unit == D), ]
  if(!nrow(xHere)) warning("cant find", D, "in covIntervals")
  xHereUn = covIntervals[which(covIntervals$reporting_unit == D), ]
  StimeInt = as.numeric(xHere$time)
  theCoords=  list()
  thePolys = thePolysScaled = list(cumulative=list(), intensity=list())
  for(Dpoly in 1:ncol(intervalMat)) {
  theCoords[[Dpoly]] = cbind(
    c(StimeInt, rev(StimeInt)),
    # upper and lower scaled
    log(pmax(0.0001,c(xHere[,intervalString[1,Dpoly]],
      rev(xHere[,intervalString[2,Dpoly]])))),
    log(pmax(0.0001,c(xHere[,intervalIntensity[1, Dpoly]],
        rev(xHere[,intervalIntensity[2, Dpoly]])))),
    # upper and loew runscaled
    log(pmax(0.0001,c(xHereUn[,intervalString[1,Dpoly]],
      rev(xHereUn[,intervalString[2,Dpoly]])))),
    log(pmax(0.0001,c(xHereUn[,intervalIntensity[1, Dpoly]],
        rev(xHereUn[,intervalIntensity[2, Dpoly]]))))
    )
  }
  if(D %in% names(Scol)) {
    colHere = paste0(Scol[D], opacityString)
  } else {
    colHere = paste0('#D3D3D3', opacityString)
  }
  for(Dtype in 1:2) {
    coords1 = theCoords[[1]][,c(1, 1+Dtype)]
    thePolysScaled[[Dtype]][[1]] = Polygons(list(
      Polygon(coords1, 
      hole=FALSE)), ID=1)
    thePolys[[Dtype]][[1]] = Polygons(list(
      Polygon(theCoords[[1]][,c(1, 3+Dtype)], 
      hole=FALSE)), ID=1)
    for(Dpoly in 2:length(theCoords)) {
      thePolysScaled[[Dtype]][[Dpoly]] = Polygons(list(
          Polygon(theCoords[[Dpoly]][,c(1, 1+Dtype)], hole=FALSE),
          Polygon(theCoords[[Dpoly-1]][,c(1, 1+Dtype)], hole=TRUE)
        ), ID = Dpoly)
      thePolys[[Dtype]][[Dpoly]] = Polygons(list(
          Polygon(theCoords[[Dpoly]][,c(1, 3+Dtype)], hole=FALSE),
          Polygon(theCoords[[Dpoly-1]][,c(1, 3+Dtype)], hole=TRUE)
        ), ID = Dpoly)
    }
    forecastPoly[[Dtype]][[D]] = SpatialPolygonsDataFrame(
      SpatialPolygons(thePolys[[Dtype]]),
      data.frame(col = colHere,
        quant = apply(intervalMat, 1, paste),
      stringsAsFactors=FALSE))
    forecastPolyScaled[[Dtype]][[D]] = SpatialPolygonsDataFrame(
      SpatialPolygons(thePolysScaled[[Dtype]]),
      data.frame(col = colHere,
        quant = apply(intervalMat, 1, paste),
      stringsAsFactors=FALSE))
  }
}
  if(verbose) cat('\n')

list(unscaled = forecastPoly, scaled=forecastPolyScaled, 
  intervals = covIntervals,
  intervalsScaled = covIntervalsScaled,
  intervalNames = intervalIntensity)
}

