library(weathercan)
library(dplyr)

#get the station
tempDatabase <- NULL
for(city in c("toronto","vancouver","calgary","edmonton"))
{

city_id <- stations_search(city)
city_id <- city_id[!(as.numeric(city_id$start)>2012 | as.numeric(city_id$end)<1996), ]

data <-  weather_dl(station_ids = city_id$station_id,interval = "day",format = TRUE, trim = FALSE,
                        start = "1996-01-01", end = "2012-12-31")


city_temperature <- data %>% select(station_id,date,mean_temp,min_temp,max_temp)
dailytemp_city_mean <- (aggregate(as.numeric(mean_temp)~date, data = city_temperature, mean))
dailytemp_city_mean_of_min <- (aggregate(as.numeric(min_temp)~date, data = city_temperature, mean))
dailytemp_city_mean_of_max <- (aggregate(as.numeric(max_temp)~date, data = city_temperature, mean))
dailytemp_city_median_of_min <- (aggregate(as.numeric(min_temp)~date, data = city_temperature, median))
dailytemp_city_median_of_max <- (aggregate(as.numeric(max_temp)~date, data = city_temperature, median))
dailytemp_city_min_of_min <- (aggregate(as.numeric(min_temp)~date, data = city_temperature, min))
dailytemp_city_max_of_max <- (aggregate(as.numeric(max_temp)~date, data = city_temperature, max))

dailytemp_city <- data.frame(city=city,
                       date=dailytemp_city_mean[,1],
                       mean=dailytemp_city_mean[,2],
                       mean_of_min=dailytemp_city_mean_of_min[,2],
                       mean_of_max=dailytemp_city_mean_of_max[,2],
                       median_of_min=dailytemp_city_median_of_min[,2],
                       median_of_max=dailytemp_city_median_of_max[,2],
                       min_of_min=dailytemp_city_min_of_min[,2],
                       max_of_max=dailytemp_city_max_of_max[,2]
                       )

tempDatabase <- rbind(tempDatabase, dailytemp_city)

}