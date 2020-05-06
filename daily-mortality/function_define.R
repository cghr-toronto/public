
# Define two functions use to download long dataset -----------------------

data.reform <- function(pattern,pollutant,Sfiles,Ddir)
{
  pmHly = grep(pattern, Sfiles, value=TRUE)
  pmDat = read.fwf(file.path(Ddir, pmHly), widths = c(3,6,4, 2,2,rep(4,27)), na.strings='-999')
  
  hVars= paste0('h', 0:23)
  idVars =  c( 'station','year','month','day')
  colnames(pmDat) = c('pollutant',idVars,'mean','min','max', hVars)
  if(length(which(pmDat$station=="NA"))!=0)
  {
    pmDat = pmDat[-which(pmDat$station=="NA"), ]
  }
  
  pmDat$pollutant=pollutant
  return(pmDat)
}

data.save <- function(Dyear,url,pm25pattern,no2pattern,o3pattern,pm10pattern)
{
  
  destDir = "/store/guowen/project_CGHR/data download/1pollution data"
  dir.create(destDir, showWarnings=FALSE)
  
  Dfile = paste0(Dyear, 'all', '.zip')
  DfileFull = file.path(destDir, Dfile)
  Ddir = file.path(destDir, Dyear)
  
  if(!file.exists(DfileFull))
    download.file(url= url,
                  destfile = DfileFull,
                  method = "auto")
  
  Sfiles = unzip(DfileFull, list=TRUE)$Name
  unzip(DfileFull, exdir=Ddir)
  
  
  #for pm2.5 data
  outputdata <- data.reform(pattern = pm25pattern,pollutant = "pm25",Sfiles=Sfiles,Ddir=Ddir)
  dataAll <- outputdata
  
  #for no2 data
  outputdata <- data.reform(pattern = no2pattern,pollutant = "no2",Sfiles=Sfiles,Ddir=Ddir)
  dataAll <- rbind(dataAll,outputdata)
  
  #for o3 data
  outputdata <- data.reform(pattern = o3pattern,pollutant = "o3",Sfiles=Sfiles,Ddir=Ddir)
  dataAll <- rbind(dataAll,outputdata)
  
  #for PM10 data
  outputdata <- data.reform(pattern = pm10pattern,pollutant = "pm10",Sfiles=Sfiles,Ddir=Ddir)
  dataAll <- rbind(dataAll,outputdata)
  
  hVars= paste0('h', 0:23)
  idVars =  c( 'station','year','month','day')
  
  pmLong = reshape2::melt(dataAll[,c("pollutant",idVars, hVars)], id.vars = c("pollutant",idVars), measure.vars=hVars)
  pmLong$hour = as.integer(gsub("^h", "", pmLong$variable))
  pmLong$date = ISOdatetime(pmLong$year, pmLong$month, pmLong$day, pmLong$hour, 0,0, tz='UTC')
  pmLong <- subset(pmLong, select = -c(year,month,day,variable, hour))
  
  
  if(sum(is.na(pmLong$station))!=0)
  {
    pmLong = pmLong[-which(is.na(pmLong$station)), ]
  }
  
  return(pmLong)
}


# add coordinate function

add_coordinate <- function(pmLong, pollution_station)
{
  
station_id <- unique(pmLong$station)
pmLong$lon <- NA
pmLong$lat <- NA
for(i in 1:length(unique(pmLong$station)))
{
  coor_index <- which(pollution_station[,1]==as.numeric(as.character(station_id[i])))
  if(length(coor_index)!=0)
  {
  pmLong$lon[ which(pmLong$station==station_id[i])] <- pollution_station[coor_index,2]
  pmLong$lat[ which(pmLong$station==station_id[i])] <- pollution_station[coor_index,3]
  }
}
pmLong = pmLong[-which(is.na(pmLong$lon)), ]
return(pmLong)
}
