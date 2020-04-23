# CGHR's COVID-19 data

-   [Introduction](#introduction)
-	[How to use this repository](#how-to-use-this-repository)
-   [COVID-19 data in this repository](#covid-19-data-in-this-repository)
	-	[COVID-19 database](#covid-19-database)
	-	[How to use the data with R](#how-to-use-the-data-with-r)
	-   [Data dictionary](#data-dictionary)
-   [COVID-19 R code in this repository](#covid-19-r-code-in-this-repository)
	-	[COVID-19 Forecasting R code](#covid-19-forecasting-r-code)
-	[COVID-19 data visualizations](#covid-19-data-visualizations)
	-	[Forescasting data visualizations](#forescasting-data-visualizations)
	-	[Interactive map](#interactive-map)
	-	[Other data visualizations](#other-data-visualizations)
-   [Terms of use](#terms-of-use)

## Introduction
This repository has Coronavirus (Covid-19) data and code related to CGHR's COVID-19 initiative.  All data and code posted here are free for the public to use.

There several sources of COVID-19 data on the web. Here we list some sources that we have identified so far.
<br>

<b>Data sources on the web:</b><br>
* [John Hopinks CSSE](https://github.com/CSSEGISandData/COVID-19)
* [Coronavirus App](https://coronavirus.app/map)
* [INED](https://dc-covid.site.ined.fr/en/data/)
* [European Centre for Disease Prevention and Control](https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide)
* [French data]()
* [Italian data]()
* [US county data]()
* [Our World in Data]()
* [Worldometers](https://www.worldometers.info/coronavirus/)

In our [COVID-19 forecasting paper](https://medrxiv.org/cgi/content/short/2020.04.17.20069161v1) we mostly used data from Coronavirus App. The forecasting R script shared in this repository (R folder above) uses the publicly available John Hopinks CSSE data so that anyone can run our models.


## How to use this repository

The data shown here can be accessed in two ways. First, you can download the CSV files found in the data folder (see above). Alternatively, you can access our COVID-19 relational database (db).  This db was created using [PostgreSQL](https://www.postgresql.org/) and you can connect to it using most stats programs (R, Stata, Excel, etc.)
  

## COVID-19 data in this repository

Currently, the following data sets are shared in this repository,

|Data|Description|Source|
|--------|-----------|-----------|
|intervals|COVID-19 forecasting models output data|CGHR|
|jh_ts_covid19_deaths_global|COVID-19 global deaths by day (long format)|John Hopkins CSSE|
|jh_ts_covid19_confirmed_global|COVID-19 global confirmed cases by day (restructured to long format)|John Hopkins CSSE|
|jh_ts_covid19_recovered_global|COVID-19 global recovered cases by day (restructured to long format)|John Hopkins CSSE|
|ined_age_sex_covid19_deaths|COVID-19 deaths by age group and gender for selected European countries (restructured to long format) |INED|


**Note**: The data above will be updated every 48 hours during the duration of the global pandemic.

### COVID-19 database

CGHR has compiled the open data listed in the table above and consolidated these into a single database.
You'll can access our COVID-19 db by using the following connection parameters:

  - Host: IP
  - Port: 5432
  - Database: covid_19
  - User name: covid_ruser
  - Password: ?


### How to use the data with R
R is an open-source statistical and computational program. You can download it here [R-project](https://www.r-project.org/).

* Sample code to grab data from CGHR's public COVID-19 db


		#Install libraries
		install.packages("DBI")
				
		# Load library
		library(DBI)
	    
		# create connection to db
        con <- DBI::dbConnect(drv = RPostgres::Postgres(), user='covid_ruser', password='?', host='IP', port=5432, dbname='covid_19')

		# get table from db
		res <- dbSendQuery(con, "SELECT * FROM jh_ts_covid19_deaths_global")
		cv19 <- dbFetch(res)
		
		# See data table dimensions
		dim(cv19)

		# See first and last 6 rows
		head(cv19)
		tail(cv19)
		
		# write table as CSV file
		write.table(cv19, "/path/cv19.txt", sep=",", row.names=FALSE)
	
		# remove res object
		dbClearResult(res)

		# disconnect from db
		dbDisconnect(con)


### Data dictionary

Here we provide a data dictionary for all data shared in this repository.

* File/table name: intervals.csv

|Variable|Description|
|--------|-----------|


* File/Table: jh_ts_covid19_confirmed_global, jh_ts_covid19_deaths_global, jh_ts_covid19_recovered_global 

|Variable|Description|
|--------|-----------|
|serial_id|Primary Key|
|province_state|Province or state|
|country_region|Country or region|
|lat|latitude|
|lon|longitude|
|fecha1|Date in YYYY-MM-DD|
|value1|Confirmed cases, deaths or recovered depending on the table|

* File/Table: ined_covid19_age_sex_deaths 

|Variable|Description|
|--------|-----------|
|rowname|order of row in original XLS sheet|
|age_group|Age group|
|pop_m|Male population (see XLS sheet for source)|
|pop_m_p|Male population percent|
|pop_f|Female population (see XLS sheet for source)|
|pop_f_p|Femal population percent|
|pop_bs|Box sexes population (see XLS sheet for source)|
|pop_bs_p|Both sexes population percent|
|males|Male deaths|
|males_p|Male deaths percent|
|females|Female deaths|
|females_p|Female deaths percent|
|unknown|Unknown sex deaths|
|both_sexes|Both sexes deaths|
|both_sexes_p|Both sexes deaths percent|
|date1|Date in format YYYY-MM-DD|
|country|Country|
|path1|File name of original data|
|sheet1|Excel sheet name|
|serial_id|Serial number, primary key|

## COVID-19 R code in this repository

### COVID-19 Forecasting R code

## COVID-19 Visualizations

### Forescasting data visualizations
* []()

### Interactive map
* []()

### Other visualizations
* []()


## Terms of use

The content presented here is free for the public to use. 
<br>
<b>Contact us:
* Patrick Brown: brownpa@smh.ca
* CGHR: cghr@smh.ca
