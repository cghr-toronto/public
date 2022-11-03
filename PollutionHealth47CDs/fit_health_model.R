
fit_clogit <- function(disease_type,disease_cause, age, season,pollutants)
{
  ## mort or morb?
  if(disease_type=="mortality" & disease_cause=="all-cause")
  {
    health_60CD <-data_mortality_all
  }
  if(disease_type=="morbidity" & disease_cause=="all-cause")
  {
    health_60CD <- data_morbidity_all
  }
  
  if(disease_type=="mortality" & disease_cause=="respiratory")
  {
    health_60CD <-data_mortality_resp
  }
  if(disease_type=="morbidity" & disease_cause=="respiratory")
  {
    health_60CD <- data_morbidity_resp
  }
  
  if(disease_type=="mortality" & disease_cause=="circulatory")
  {
    health_60CD <-data_mortality_circ
  }
  if(disease_type=="morbidity" & disease_cause=="circulatory")
  {
    health_60CD <- data_morbidity_circ
  }
  
  ## age group
  if (age=="all-age")
  {
    if(disease_cause == "all-cause")# other disease types has different format
    {
      health_60CD <- health_60CD %>% mutate(count = rowSums(.[3:40])) %>% dplyr::select(cdcode,Date,count)
    }
    
    if(disease_cause != "all-cause")# other disease types has different format
    {
      health_60CD <- health_60CD %>% mutate(count = rowSums(.[6:9])) %>% dplyr::select(cdcode=cd,Date,count)
    }
  }
  
  if (age=="non-senior")
  {
    if(disease_cause == "all-cause")# other disease types has different format
    {
      var_names <- names(health_60CD)
      var_names <- stringr::str_sub(var_names, -2,-1)
      var_names <- as.numeric(var_names)
      non_senior_index <- which(var_names<=65) # take the last two digits to compare
      health_60CD <- health_60CD[, c(which(names(health_60CD)=="cdcode"),which(names(health_60CD)=="Date"),non_senior_index)]
      health_60CD <-  health_60CD%>%
        mutate(count = rowSums(.[3:ncol(health_60CD)])) %>% dplyr::select(cdcode,Date,count)
    }
    
    if(disease_cause != "all-cause")# other disease types has different format
    {
      health_60CD <- health_60CD %>% dplyr::select(cdcode=cd,Date,contains(".A1")) %>% mutate(count=rowSums(.[3:4]))
    }
    
  }
  
  if (age=="senior")
  {
    if(disease_cause == "all-cause")# other disease types has different format
    {
      var_names <- names(health_60CD)
      var_names <- stringr::str_sub(var_names, -2,-1)
      var_names <- as.numeric(var_names)
      non_senior_index <- which(var_names<=65) # take the last two digits to compare
      health_60CD <- health_60CD[, -non_senior_index]
      health_60CD <-  health_60CD%>%
        mutate(count = rowSums(.[3:ncol(health_60CD)])) %>% dplyr::select(cdcode,Date,count)
    }
    
    if(disease_cause != "all-cause")# other disease types has different format
    {
      health_60CD <- health_60CD %>% dplyr::select(cdcode=cd,Date,contains(".A8")) %>% mutate(count=rowSums(.[3:4]))
    }
    
  }
  
  
  if(disease_type=="mortality")
  {
    
    health_60CD$Date <- as.Date(health_60CD$Date)
    health_60CD <- health_60CD[year(health_60CD$Date)>=2001 & year(health_60CD$Date)<=2015,] # note that 2001 because no2 available for more cities. the max year is 2015 for mort
    health_60CD <- health_60CD %>% dplyr::filter(cdcode %in% CD_fit_sep, count>0)
    
    new_exposure <- exposure_column %>% 
      mutate(year=year(Date), month=month(Date), wday=wday(Date),yearmonthwday=0) %>% 
      dplyr::filter(cdcode %in% CD_fit_sep, year>=2001, year<=2015) %>% 
      unite(yearmonthwday,year:wday,sep="_") 
    
    health_60CD$case=0
    health_60CD$stratum <- 1:nrow(health_60CD)
    health_60CD <- health_60CD %>% 
      mutate(year=year(Date), month=month(Date), wday=wday(Date),yearmonthwday=0) %>% 
      unite(yearmonthwday,year:wday,sep="_")
    
    health_60CD <- health_60CD %>% dplyr::left_join(new_exposure,by=c("cdcode","yearmonthwday"))
    health_60CD$case[health_60CD$Date.x==health_60CD$Date.y]=1
    
  }
  
  if(disease_type=="morbidity")
  {
    health_60CD$Date <- as.Date(health_60CD$Date)
    health_60CD <- health_60CD[year(health_60CD$Date)>=2001 & year(health_60CD$Date)<=2018,] # note that 2001 because no2 available for more cities. the max year is 2018 for mort
    health_60CD <- health_60CD %>% dplyr::filter(cdcode %in% CD_fit_sep, count>0)
    
    new_exposure <- exposure_column %>% 
      mutate(year=year(Date), month=month(Date), wday=wday(Date),yearmonthwday=0) %>% 
      dplyr::filter(cdcode %in% CD_fit_sep, year>=2001, year<=2018) %>% 
      unite(yearmonthwday,year:wday,sep="_") 
    
    health_60CD$case=0
    health_60CD$stratum <- 1:nrow(health_60CD)
    health_60CD <- health_60CD %>% 
      mutate(year=year(Date), month=month(Date), wday=wday(Date),yearmonthwday=0) %>% 
      unite(yearmonthwday,year:wday,sep="_")
    
    health_60CD <- health_60CD %>% dplyr::left_join(new_exposure,by=c("cdcode","yearmonthwday"))
    health_60CD$case[health_60CD$Date.x==health_60CD$Date.y]=1
  }
  
  
  Final_data <- health_60CD
  
  #add temperature spline function
  library(Epi)
  refTemp = 18
  tempSeq = seq(-40,40,len=201)
  Sp_predict = Ns(tempSeq, df = 4, ref=refTemp)
  attributes(Sp_predict)$ref = refTemp
  
  Sp_temp <- Ns(Final_data$temp, knots = attributes(Sp_predict)$knots, 
                Boundary.knots = attributes(Sp_predict)$Boundary.knots, 
                ref = attributes(Sp_predict)$ref) 
  
  colnames(Sp_temp) <- c("spline1", "spline2","spline3","spline4")
  Final_data <- cbind(Final_data, Sp_temp)
  
  ## seasons warm(Apr to Sep), Cold(Oct to Mar)
  
  if(season=="warm")
  {
    Final_data <- Final_data[which(lubridate::month(Final_data$Date)>=4 & lubridate::month(Final_data$Date)<=9),]
  }
  
  if(season=="cold")
  {
    Final_data <- Final_data[-which(lubridate::month(Final_data$Date)>=4 & lubridate::month(Final_data$Date)<=9),]
  }
  
  ## pollutants in the model
  
  library(survival)
  Pformula <- as.formula(paste("case ~",pollutants,"+ spline1 + spline2 + spline3 + spline4","+ strata(stratum)"))
  
  # control 12
  model_result <- vector("list",length = length(CD_fit_sep))
  
  for(i in 1:(length(model_result)))
  {
    
    model_result[[i]] <- clogit(Pformula,
                                weights = count, method = "approximate", data=Final_data[Final_data$cdcode==CD_fit_sep[i],])
    
    print(i)
  }
  
  names(model_result) <- CD_fit_sep
  save_pol_name <- gsub("[+]","",pollutants)
  save(model_result, file=paste0("model_result",disease_type,disease_cause,age,season,save_pol_name,".RData"))
}

for(v_disease_type in c("mortality","morbidity"))
{
  for(v_disease_cause in c("all-cause","respiratory","circulatory")) ## this one needs extend
  {
    for(v_age in c("all-age"))
    {
      # for(v_season in c("all-year","warm","cold"))
      for(v_season in c("all-year"))
      {
        # for(v_pollutants in c("pm25+no2+o3","pm25+no2","pm25+o3","no2+o3","pm25","no2","o3"))
        for(v_pollutants in c("pm25+no2+o3"))
        {
          
          fit_clogit(disease_type=v_disease_type,disease_cause=v_disease_cause, age=v_age, season=v_season,pollutants=v_pollutants)
        }
      }
    }
  }
}

