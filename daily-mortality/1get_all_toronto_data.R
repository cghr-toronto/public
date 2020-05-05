
library(sp)
library(rgdal)
library(dplyr)
library(here)
setwd(here("Model","Toronto1996_2012","fit model"))

data_all_toronto <- NULL
for(i in 1996:2012)
{
  
  load(paste0(here("Data","Canada","Pollution"),"/Pretty",i,".RData"))
  Alldata$station=as.numeric(as.character(Alldata$station))
  Alldata$value <- as.numeric(Alldata$value)
  temp <- which(Alldata$station>60400 & Alldata$station <60499)
  data_all_toronto <- rbind(data_all_toronto,Alldata[temp,])
  
  print(i)
  print(nrow(Alldata))
  
}
remove(Alldata)

# check the map whether all the stations are reall in toronto city
station_id <- unique(data_all_toronto$station)
station_coord <- unique(cbind(data_all_toronto$lon, data_all_toronto$lat))
station_coord <- unique(round(station_coord,3)) # because a few stations are the same, but the coordinates are slightly different

delhiPointsSmall = SpatialPoints(
  station_coord,
   proj4string=mapmisc::crsLL
) 

delhiBgSmall = mapmisc::openmap(delhiPointsSmall, buffer=0.03)


mapmisc::map.new(delhiPointsSmall, buffer=0.03)
plot(delhiBgSmall, add=TRUE, maxpixels=10^7)
points(delhiPointsSmall, col='blue', pch=18, cex=2.5)

#there are 4 stations not within toronto, those  "lon <  -79.6"
data_all_toronto <- data_all_toronto[data_all_toronto$lon > -79.6, ]
station_id <- unique(data_all_toronto$station)
station_coord <- unique(cbind(data_all_toronto$lon, data_all_toronto$lat))
station_coord <- unique(round(station_coord,3))

data_all_toronto <- data_all_toronto[-which(is.na(data_all_toronto$value)),]
data_all_toronto <- data_all_toronto[data_all_toronto$value>=0,]

#check any duplicated date for each station.
for(pollutant in c("no2", "pm25","pm10","o3"))
{
  for(j in 1:length(station_id))
  {
    print(sum(duplicated(data_all_toronto$date[data_all_toronto$station==station_id[j] & data_all_toronto$pollutant==pollutant])))
    
  }
}

save(data_all_toronto, file = "data_all_toronto.RData")






