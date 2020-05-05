library(sp)
library(dplyr)
library(here)

setwd(here("Data","Canada","Pollution","1pollution data"))

source("function_define.R")

# use for Adding coordinates to data set ---------------------------------------------
pollution_station <- read.csv("Stations2017_v3.csv")
pollution_station <- data.frame(NAPS=pollution_station$NAPS_ID,
                                lon=pollution_station$Long_Decimal,
                                lat=pollution_station$Lat_Decimal)
pollution_station <- pollution_station[-which(is.na(pollution_station$lon)),]


#2016 data
pmLong2016 <- data.save(Dyear = 2016,
                    url="http://maps-cartes.ec.gc.ca/CESIServices/napsservice/downloads/package/92",
                    pm25pattern = "PM25.HLY",
                    no2pattern = "_NO2_",
                    o3pattern = "_O3_",
                    pm10pattern = "_PM10_")

pmLong2016 <- add_coordinate(pmLong2016, pollution_station)
write.csv(pmLong2016,"pollutant2016.csv", row.names = F)


# for other years ---------------------------------------------------------

#2015 data
pmLong <- data.save(Dyear = 2015,
                    url="http://maps-cartes.ec.gc.ca/CESIServices/napsservice/downloads/package/90",
                    pm25pattern = "_PM25_BAM35",
                    no2pattern = "_NO2_",
                    o3pattern = "_O3_",
                    pm10pattern = "_PM10_"
)

#2014 data
pmLong <- data.save(Dyear = 2014,
                    url="http://maps-cartes.ec.gc.ca/CESIServices/napsservice/downloads/package/87",
                    pm25pattern = "PM25.HLY",
                    no2pattern = "_NO2_",
                    o3pattern = "_O3_",
                    pm10pattern = "_PM10_"
)

#2013 data
pmLong <- data.save(Dyear = 2013,
                    url="http://maps-cartes.ec.gc.ca/CESIServices/napsservice/downloads/package/84",
                    pm25pattern = "_PM25_v",
                    no2pattern = "_NO2_",
                    o3pattern = "_O3_",
                    pm10pattern = "_PM10_"
)


#2012 data
pmLong <- data.save(Dyear = 2012,
                    url="http://maps-cartes.ec.gc.ca/CESIServices/napsservice/downloads/package/82",
                    pm25pattern = "_PM25_TEOM_SES_v",
                    no2pattern = "_NO2_",
                    o3pattern = "_O3_",
                    pm10pattern = "_PM10_"
)
pmLong <- add_coordinate(pmLong, pollution_station)
Alldata <- pmLong
save(Alldata,file = "pollutant2012.RData")

#2011 data
pmLong <- data.save(Dyear = 2011,
                    url="http://maps-cartes.ec.gc.ca/CESIServices/napsservice/downloads/package/8",
                    pm25pattern = "_PM25_TEOM_SES_v",
                    no2pattern = "_NO2_",
                    o3pattern = "_O3_",
                    pm10pattern = "_PM10_"
)
pmLong <- add_coordinate(pmLong, pollution_station)
Alldata <- pmLong
save(Alldata,file = "pollutant2011.RData")
#2010 data
pmLong <- data.save(Dyear = 2010,
                    url="http://maps-cartes.ec.gc.ca/CESIServices/napsservice/downloads/package/9",
                    pm25pattern = "_PM25_TEOM_SES_v",
                    no2pattern = "_NO2_",
                    o3pattern = "_O3_",
                    pm10pattern = "_PM10_"
)
pmLong <- add_coordinate(pmLong, pollution_station)
Alldata <- pmLong
save(Alldata,file = "pollutant2010.RData")
#2009 data
pmLong <- data.save(Dyear = 2009,
                    url="http://maps-cartes.ec.gc.ca/CESIServices/napsservice/downloads/package/10",
                    pm25pattern = "SES",
                    no2pattern = "NO2",
                    o3pattern = "O3",
                    pm10pattern = "PM10"
)
pmLong <- add_coordinate(pmLong, pollution_station)
Alldata <- pmLong
save(Alldata,file = "pollutant2009.RData")
#2008-6 data
year_index <- 2008:2006
download_index <- 11:13

for(kkk in 1:3)
{
  
  
  
  pmLong <- data.save(Dyear = year_index[kkk],
                      url=paste0("http://maps-cartes.ec.gc.ca/CESIServices/napsservice/downloads/package/",download_index[kkk]),
                      pm25pattern = "SES",
                      no2pattern = "NO2",
                      o3pattern = "O3",
                      pm10pattern = "PM10"
  )
  pmLong <- add_coordinate(pmLong, pollution_station)
  Alldata <- pmLong
  save(Alldata,file=paste0("pollutant",year_index[kkk],".RData"))
}




#2005-1
year_index <- 2005:2001
download_index <- 14:18
pm25pattern <- c("SES","SES","SES","TEOM", "TEOM")

for(kkk in 1:5)
{
  
  
  
  pmLong <- data.save(Dyear = year_index[kkk],
                      url=paste0("http://maps-cartes.ec.gc.ca/CESIServices/napsservice/downloads/package/",download_index[kkk]),
                      pm25pattern = pm25pattern[kkk],
                      no2pattern = "NO2",
                      o3pattern = "O3",
                      pm10pattern = "PM10"
  )
  pmLong <- add_coordinate(pmLong, pollution_station)
  Alldata <- pmLong
  
  save(Alldata,file=paste0("pollutant",year_index[kkk],".RData"))
}

#2000-1999
year_index <- 2000:1999
download_index <- 19:20

for(kkk in 1:2)
{
  
  
  
  pmLong <- data.save(Dyear = year_index[kkk],
                      url=paste0("http://maps-cartes.ec.gc.ca/CESIServices/napsservice/downloads/package/",download_index[kkk]),
                      pm25pattern = "TEOMPM25",
                      no2pattern = "NO2",
                      o3pattern = "O3",
                      pm10pattern = "PM10"
  )
}

#1998-5
year_index <- 1998:1995
download_index <- c(34,35,77,36)

for(kkk in 3:4)
{
  
  
  
  pmLong <- data.save(Dyear = year_index[kkk],
                      url=paste0("http://maps-cartes.ec.gc.ca/CESIServices/napsservice/downloads/package/",download_index[kkk]),
                      pm25pattern = "TEOMPM25",
                      no2pattern = "NO2",
                      o3pattern = "O3",
                      pm10pattern = "PM10"
  )
}

#1994-2
year_index <- 1994:1992
download_index <- c(37,38,46)

for(kkk in 1:3)
{
  
  
  
  pmLong <- data.save(Dyear = year_index[kkk],
                      url=paste0("http://maps-cartes.ec.gc.ca/CESIServices/napsservice/downloads/package/",download_index[kkk]),
                      # pm25pattern = "TEOMPM25",
                      no2pattern = "NO2",
                      o3pattern = "O3",
                      pm10pattern = "PM10"
  )
}

#1991-1980
year_index <- 1991:1980
download_index <- c(seq(48,60,by=2),66:70)

for(kkk in 1:12)
{
  
  pmLong <- data.save(Dyear = year_index[kkk],
                      url=paste0("http://maps-cartes.ec.gc.ca/CESIServices/napsservice/downloads/package/",download_index[kkk]),
                      # pm25pattern = "TEOMPM25",
                      no2pattern = "NO2",
                      o3pattern = "O3"
                      # ,pm10pattern = "PM10"
  )
}









