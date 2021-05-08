

# PM25, PM10, NO2, O3
data_PM25_2000_2019 <- NULL
data_PM25_2000_2019 <- rbind(
  read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2000/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM25_2000.csv", skip=8,sep=",", header = F)
, read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2001/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM25_2001.csv", skip=8,sep=",", header = F)
, read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2002/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM25_2002.csv", skip=8,sep=",", header = F)
, read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2003/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM25_2003.csv", skip=8,sep=",", header = F)
, read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2004/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM25_2004.csv", skip=8,sep=",", header = F)
, read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2005/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM25_2005.csv", skip=8,sep=",", header = F)
, read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2006/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM25_2006.csv", skip=8,sep=",", header = F)
, read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2007/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM25_2007.csv", skip=8,sep=",", header = F)
, read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2008/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM25_2008.csv", skip=8,sep=",", header = F)
, read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2009/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM25_2009.csv", skip=8,sep=",", header = F)
, read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2010/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM25_2010.csv", skip=8,sep=",", header = F)
, read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2011/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM25_2011.csv", skip=8,sep=",", header = F)
, read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2012/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM25_2012.csv", skip=8,sep=",", header = F)
, read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2013/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM25_2013.csv", skip=8,sep=",", header = F)
, read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2014/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM25_2014.csv", skip=8,sep=",", header = F)
, read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2015/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM25_2015.csv", skip=8,sep=",", header = F)
, read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2016/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM25_2016.csv", skip=8,sep=",", header = F)
, read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2017/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM25_2017.csv", skip=8,sep=",", header = F)
, read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2018/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM25_2018.csv", skip=8,sep=",", header = F)
, read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2019/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM25_2019.csv", skip=8,sep=",", header = F)
)
#PM10
data_PM10_2000_2019 <- NULL
data_PM10_2000_2019 <- rbind(
  read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2000/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM10_2000.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2001/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM10_2001.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2002/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM10_2002.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2003/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM10_2003.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2004/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM10_2004.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2005/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM10_2005.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2006/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM10_2006.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2007/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM10_2007.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2008/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM10_2008.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2009/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM10_2009.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2010/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM10_2010.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2011/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM10_2011.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2012/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM10_2012.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2013/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM10_2013.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2014/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM10_2014.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2015/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM10_2015.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2016/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM10_2016.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2017/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM10_2017.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2018/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM10_2018.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2019/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/PM10_2019.csv", skip=8,sep=",", header = F)
)
#NO2
data_NO2_2000_2019 <- NULL
data_NO2_2000_2019 <- rbind(
  read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2000/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/NO2_2000.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2001/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/NO2_2001.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2002/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/NO2_2002.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2003/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/NO2_2003.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2004/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/NO2_2004.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2005/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/NO2_2005.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2006/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/NO2_2006.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2007/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/NO2_2007.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2008/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/NO2_2008.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2009/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/NO2_2009.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2010/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/NO2_2010.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2011/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/NO2_2011.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2012/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/NO2_2012.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2013/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/NO2_2013.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2014/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/NO2_2014.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2015/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/NO2_2015.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2016/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/NO2_2016.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2017/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/NO2_2017.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2018/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/NO2_2018.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2019/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/NO2_2019.csv", skip=8,sep=",", header = F)
)
#O3
data_O3_2000_2019 <- NULL
data_O3_2000_2019 <- rbind(
  read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2000/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/O3_2000.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2001/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/O3_2001.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2002/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/O3_2002.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2003/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/O3_2003.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2004/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/O3_2004.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2005/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/O3_2005.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2006/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/O3_2006.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2007/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/O3_2007.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2008/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/O3_2008.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2009/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/O3_2009.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2010/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/O3_2010.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2011/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/O3_2011.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2012/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/O3_2012.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2013/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/O3_2013.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2014/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/O3_2014.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2015/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/O3_2015.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2016/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/O3_2016.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2017/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/O3_2017.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2018/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/O3_2018.csv", skip=8,sep=",", header = F)
  , read.csv(file = "http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2019/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/O3_2019.csv", skip=8,sep=",", header = F)
)

# delete the colum "method code from PM25"
data_PM25_2000_2019 <- data_PM25_2000_2019[,-2]
colnames(data_PM25_2000_2019) <- colnames(data_PM10_2000_2019)

#combine all 4 pollutants
library(tidyverse)
data_2000_2019 <- rbind(data_PM25_2000_2019, data_PM10_2000_2019, data_NO2_2000_2019,data_O3_2000_2019)
# data_2000_2019 <- data_2000_2019 %>% dplyr::select(V1,V2,V5:V31)

# data_2000_2019$V2 <- as.character(data_2000_2019$V2)
data_2000_2019$V8[data_2000_2019$V8 <0 ] <- NA
data_2000_2019$V9[data_2000_2019$V9 <0 ] <- NA
data_2000_2019$V10[data_2000_2019$V10 <0 ] <- NA
data_2000_2019$V11[data_2000_2019$V11 <0 ] <- NA
data_2000_2019$V12[data_2000_2019$V12 <0 ] <- NA
data_2000_2019$V13[data_2000_2019$V13 <0 ] <- NA
data_2000_2019$V14[data_2000_2019$V14 <0 ] <- NA
data_2000_2019$V15[data_2000_2019$V15 <0 ] <- NA
data_2000_2019$V16[data_2000_2019$V16 <0 ] <- NA
data_2000_2019$V17[data_2000_2019$V17 <0 ] <- NA
data_2000_2019$V18[data_2000_2019$V18 <0 ] <- NA
data_2000_2019$V19[data_2000_2019$V19 <0 ] <- NA
data_2000_2019$V20[data_2000_2019$V20 <0 ] <- NA
data_2000_2019$V21[data_2000_2019$V21 <0 ] <- NA
data_2000_2019$V22[data_2000_2019$V22 <0 ] <- NA
data_2000_2019$V23[data_2000_2019$V23 <0 ] <- NA
data_2000_2019$V24[data_2000_2019$V24 <0 ] <- NA
data_2000_2019$V25[data_2000_2019$V25 <0 ] <- NA
data_2000_2019$V26[data_2000_2019$V26 <0 ] <- NA
data_2000_2019$V27[data_2000_2019$V27 <0 ] <- NA
data_2000_2019$V28[data_2000_2019$V28 <0 ] <- NA
data_2000_2019$V29[data_2000_2019$V29 <0 ] <- NA
data_2000_2019$V30[data_2000_2019$V30 <0 ] <- NA
data_2000_2019$V31[data_2000_2019$V31 <0 ] <- NA


save(data_2000_2019, file="~/NEW_pollution/data_2000_2019.RData")













