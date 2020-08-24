# Usage Notes

## Running the files

Before running these files, you will need to supply credentials that are used in Lines 133-137 and Lines 236 of the covid_data.R file. Right now they are sourced from the credentials.R file.

To get the data produced from the Stan model, run this command: `make Coronavirus_Stan.html` . If this is not working, then there may be a package that is not installed on your system. If that isn't the case, try running this: 

`Rscript -e "rmarkdown::render('Coronavirus_Stan.Rmd, 'pdf_document')"`

Running this may take a few hours. Make note of the RDS file produced with this name: covidRes(date).rds, as it will be used in the other files.

After you have the covidRes(date).rds file, either from running the Stan model or using an older copy, you can create figures from data processed from the model and export processed data. If you're using an older copy or one downloaded, make sure that the RDS file is in this directory.

To create the figures, run `make Coronavirus_Model.html`.

To export data, such as for intervals.csv, run `make Coronavirus_Export.html`.

If you are running these on a different date than when the RDS was created, you will need to update the `today` variable in the Rmd file you're running with the date corresponding to the RDS file.
For example, if your RDS file is named covidForecast2020_05_05.rds, then in the Rmd file you want to run, set `today <- "2020_05_05"`, below where the today variable is declared.

More info on the files and the data they used is below.

## Input Files

There are three main R source files with functions that we use:

**covid_expected_counts.R**

This file contains the get_covid_expected_counts function and its helper functions; it returns population, expected counts and mMult for countries of the world, states of the US, and China's Hubei province.

External Data Used:

China: http://www.stats.gov.cn/tjsj/ndsj/2018/indexeh.htm ,
       http://www.stats.gov.cn/tjsj/ndsj/2018/html/EN0212.jpg
       
Italy: https://www.epicentro.iss.it/coronavirus/bollettino/Infografica_29marzo%20ENG.pdf

Global Pop Data: https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationByAgeSex_OtherVariants.csv

USA Population: https://www2.census.gov/programs-surveys/popest/tables/2010-2018/state/asrh/PEP_2018_PEPASR6H.zip


**covid_data.R**

This file contains the 'data getter' functions such as get_covid_data and get_covid_data_cva. Functions in this file take in the data frame produced in covid_expected_counts.R and perform further processing on it, for use in the forecasting functions.

External Data Used:

USA: https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv

Place and historical data: https://coronavirus.app/get-places ,
                           https://coronavirus.app/get-history?id=

**forecast.R**

This contains the main forecasting function covid_forecast (which takes in the data produced by covid_data.R), and its helper functions. Also contains the forecast_summaries function, which creates summaries from the covid_forecast function.

## Scripts

**Coronavirus_Stan.Rmd**

Gets data frame from covid_data.R, modifies it and adds in new data, then compiles and runs the Stan model. Output is saved in an RDS file, "covidRes(date).rds"

External Data Used:

US States: https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv

France: https://raw.githubusercontent.com/opencovid19-fr/data/master/dist/chiffres-cles.csv

Belgium: https://epistat.sciensano.be/Data/COVID19BE_MORT.csv


**Coronavirus_Model.Rmd**

Uses the RDS file produced from Coronavirus_Stan.Rmd and data produced by covid_forecast (in forecast.R) to create models, these will be stored locally in the Coronavirus_Model_files directory.

External Data Used:

Same as in Coronavirus_Stan.Rmd

**Coronavirus_Export.Rmd**

Uses the RDS file produced from Coronavirus_Stan.Rmd and data produced by covid_forecast and forecast_summaries to create file of data for export. Output will be saved in a folder named (current_date), containing intervals.csv, samples.csv, and summaries.csv. Also, produces ../doc/historicalTable.csv in the doc folder.

External Data Used:

../doc/all_deaths_age20pl_all_states_2015_04292020.csv

../doc/PI_deaths_all_states_2015_04292020.csv

../doc/Table age 20 Plus Repiratory death_Pop_WHODB.csv

## Output Files

**cvaCache.rds**

Cache file used in get_covid_data_cva in covid_data.R.

**covidRes(date).rds**

This is the output file produced by the Stan model in Coronavirus_Stan.Rmd. It's used in Coronavirus_Model.Rmd and Coronavirus_Export.Rmd.

**covidForecast(date).rds**

This is the output file containing forecast data after the data from covidRes(date).rds is processed and ran through the covid_forecast from forecast.R. This file is produced by and used in Coronavirus_Model.Rmd and Coronavirus_Export.Rmd.

**data(date).rds**

**Coronavirus_Model_files**

Directory produced by Coronavirus_Model.Rmd, contains the figures it produces.

**(YYYY_MM_DD)**

Directory produced by Coronavirus_Export.Rmd with its name corresponding to the date you ran it (i.e. if ran on May 5, 2020, its name is 2020_05_05). This contains all the files containing the exported data : data.csv, intervals.csv, samples.csv, and summaries.csv.








