
library(lubridate)
library(tidyverse)

load("exposure_column.RData")
exposure_column <- exposure_column[ year(exposure_column$Date) >=2001 & year(exposure_column$Date)<=2018,]

exposure_column <- exposure_column %>% group_by(cdcode) %>% summarise(pm25mean=mean(pm25,na.rm = T), 
                                                                      pm25min=min(pm25,na.rm = T), 
                                                                      pm25max=max(pm25,na.rm = T), 
                                                                      pm25_10=quantile(pm25,0.1,na.rm = T), 
                                                                      pm25_90=quantile(pm25,0.9,na.rm = T), 
                                                                      no2mean=mean(no2,na.rm = T), 
                                                                      no2min=min(no2,na.rm = T), 
                                                                      no2max=max(no2,na.rm = T), 
                                                                      no2_10=quantile(no2,0.1,na.rm = T), 
                                                                      no2_90=quantile(no2,0.9,na.rm = T), 
                                                                      o3mean=mean(o3,na.rm = T), 
                                                                      o3min=min(o3,na.rm = T), 
                                                                      o3max=max(o3,na.rm = T), 
                                                                      o3_10=quantile(o3,0.1,na.rm = T), 
                                                                      o3_90=quantile(o3,0.9,na.rm = T), 
                                                                      tempmean=mean(temp,na.rm = T),
                                                                      tempmin=min(temp,na.rm = T),
                                                                      tempmax=max(temp,na.rm = T),
                                                                      temp_10=quantile(temp,0.1,na.rm = T), 
                                                                      temp_90=quantile(temp,0.9,na.rm = T)
) %>% dplyr::left_join(CD_name, by=c("cdcode"="CDUID")) %>% 
  relocate(CDNAME)

exposure_column$CDNAME[exposure_column$CDNAME=="Greater Sudbury / Grand Sudbury"] <- "Greater Sudbury"
exposure_column$CDNAME[exposure_column$CDNAME=="Stormont, Dundas and Glengarry"] <- "SDG"

colnames(exposure_column) <- c("CDNAME","cdcode","pm25mean","pm25Min","pm25Max","pm25_10","pm25_90","no2mean","no2Min","no2Max","no2_10","no2_90","o3mean","o3Min","o3Max","o3_10","o3_90","temperatureM","temperatureMin","temperatureMax","temp_10","temp_90")

library(kableExtra)

exposure_column%>% rename( "Name"="CDNAME") %>% arrange(cdcode)%>% relocate(Name, .after = cdcode) %>% 
  kable(format = "latex",  booktabs=T, linesep="", digits = 1, 
        col.names=c("cdcode","name","mean","min","max","q10","q90","mean","min","max","q10","q90","mean","min","max","q10","q90","mean","min","max","q10","q90"),
        caption="Summary statistics of exposure for each CD during study period.", label = "datasummary2") %>% 
  add_header_above(c(" "=2, "PM$_{2.5}$"=5, "NO$_2$"=5, "O$_3$"=5, "Temperature"=5), bold = T, escape = F)%>% 
  kable_styling(font_size = 9)