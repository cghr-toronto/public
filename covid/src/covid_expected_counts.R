#' Expected counts for countries in the world, US states, and Hubei in China
#'
#' Output three dataframes containing population, expected mortality, and mMult
#' for countries of the world, the states in the US, and China's Hubei province.
get_covid_expected_counts <- function() {
  # Population data for states around the world
  popData <- global_Pop_Data()

  # Population data for states in US
  usPop <- us_Pop_Data()

  # Population data for Maharashtra state in India
  indiaPop <- india_Pop_Data()

  # table 2.12
  # china http://www.stats.gov.cn/tjsj/ndsj/2018/indexeh.htm
  # http://www.stats.gov.cn/tjsj/ndsj/2018/html/EN0212.jpg

  Sage <- c(0, 15, 65, Inf)
  popData$ageCutChina <- cut(popData$AgeGrpStart, Sage, right = FALSE)

  # Aggregate Italian population data, compute mortality rates
  popItaly <- popData[popData$country == "Italy", ]
  popItalyAgg <- tapply(
    popItaly$pop,
    cut(popItaly$AgeGrpStart, Sage, right = FALSE),
    sum
  )
  deathsByAge <- c(0, 1 + 20 + 81 + 1073 / 2, 1073 / 2 + 3206 + 3652 + 845)
  names(deathsByAge) <- levels(popData$ageCutChina)
  ratesByAge <- deathsByAge / popItalyAgg[names(deathsByAge)]

  chinaPop <- data.frame(
    province = "Hubei", ageLow = c(0, 15, 65), ageHigh = c(14, 64, Inf),
    pop = c(7701, 35028, 5955) / 0.000824
  )

  # Intervals for age cut
  chinaPop$ageCut <- cut(chinaPop$ageLow, Sage, right = FALSE)

  # Population and expected mortality for provinces in China
  chinaExpected <- china_Expected(chinaPop, ratesByAge)

  # https://www.epicentro.iss.it/coronavirus/bollettino/Infografica_29marzo%20ENG.pdf
  ratesByAge <- data.frame(
    age = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90),
    deaths = c(0, 0, 1, 20, 81, 340, 1073, 3206, 3652, 845)
  )
  popItaly <- popData[popData$country == "Italy", ]
  ratesByAge$pop <- tapply(
    popItaly$pop,
    cut(popItaly$AgeGrpStart, c(ratesByAge$age, Inf), right = FALSE),
    sum
  )
  ratesByAge$rate <- ratesByAge$deaths / ratesByAge$pop

  # Expected mortality, population, deaths per million for countries around the
  # world
  worldExpected <- world_Expected(popData, ratesByAge)

  # Expected mortality, population, deaths per million for US states
  usExpected <- world_Expected(usPop, ratesByAge,
    ageVar = "ageLow",
    regionVar = "state"
  )

  # Total expected mortality and population in US.
  usData <- as.data.frame(lapply(
    usExpected[, c("Expected", "pop")], sum
  ))
  usData$state <- "United States"
  usData$mMult <- 1e6 / usData$pop
  usExpected <- rbind(usExpected, usData[, colnames(usExpected)])
  #
  usExpected <- usExpected[
    usExpected$state != "United States",
  ]

  # Population, expected mortality, deaths per million for Maharashtra state
  indiaExpected <- world_Expected(indiaPop, ratesByAge, ageVar = "age", regionVar = "state")

  list(china = chinaExpected, us = usExpected, india = indiaExpected, world = worldExpected)
  # list(china = chinaExpected, us = usExpected, world = worldExpected)
}

#' 2020 world population data
#'
#' Return 2020 Median PI for countries around the world, adjusting names and
#' scaling.
#'
#' This function makes use of UN population data, and returns a data frame
#' containing population data for various countries around the world in 2020,
#' split by age groups and gender.
global_Pop_Data <- function() {
  popFile <- Pmisc::downloadIfOld(
    "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationByAgeSex_OtherVariants.csv"
  )
  # popFile <- "../doc/WPP2019_PopulationByAgeSex_OtherVariants.csv"
  popData <- read.csv(popFile)
  popData <- popData[popData$Time == 2020 &
    popData$Variant == "Median PI", ]
  popData <- match_Global_Region_Names(popData)
  popData <- scale_Global_Population_Data(popData)

  return(popData)
}

#' Change region names
#'
#' Change region names for various countries to aid in data processing later.
#' i.e. Republic of Korea to South Korea.
#'
#' @param popData A dataframe which must contain a char vector column "country",
#'   consisting of various country names.
match_Global_Region_Names <- function(popData) {
  popData$country <- gsub("[[:space:]]?[(].*[)]", "", popData$Location)
  rename <- c(
    "Republic of Korea" = "South Korea",
    "Russian Federation" = "Russia",
    "Viet Nam" = "Vietnam",
    #    'Czechia' = 'Czech Republic',
    "Republic of Moldova" = "Moldova",
    "District of Columbia" = "Washington, D.C.",
    "United States of America" = "United States"
  )
  for (D in names(rename)) {
    popData$country <- gsub(D, rename[D], popData$country)
  }
  return(popData)
}

#' Scale population data
#'
#' Multiply popTotal by 1000.
#'
#' @param popData A dataframe which contains a numerical vector column
#' "PopTotal".
scale_Global_Population_Data <- function(popData) {
  popData$pop <- 1000 * popData$PopTotal
  return(popData)
}


# discard_Global_NonMatching_Names <- function(popData) {
#  unique(nCov_data$global$country)[!
#  unique(nCov_data$global$country) %in% popData$country]
#  as.character(popData$country)[
#  !popData$country %in% unique(nCov_data$global$country)]
# }


#' Population data for USA
#'
#' Returns population data from each US state, split between age groups.
us_Pop_Data <- function() {
  usPopFile <- Pmisc::downloadIfOld(
    "https://www2.census.gov/programs-surveys/popest/tables/2010-2018/state/asrh/PEP_2018_PEPASR6H.zip"
  )
  usPopFile <- usPopFile[which.max(file.info(usPopFile)$size)]
  usPop <- read.csv(usPopFile, stringsAsFactors = FALSE, header = TRUE, skip = 1)
  names(usPop) <- us_Pop_Data_Headers()
  usPop <- process_US_Pop_Data(usPop)
  usPop$state <- gsub("District of Columbia", "Washington, D.C.", usPop$state)
  return(usPop)
}


#' Headers for USA population data as vector
us_Pop_Data_Headers <- function() {
  usPopFile <- Pmisc::downloadIfOld(
    "https://www2.census.gov/programs-surveys/popest/tables/2010-2018/state/asrh/PEP_2018_PEPASR6H.zip"
  )
  usPopFile <- usPopFile[which.max(file.info(usPopFile)$size)]
  usPopHead <- read.csv(usPopFile, stringsAsFactors = FALSE, header = FALSE, nrow = 1)
  return(unlist(usPopHead))
}


#' Process US population data
#'
#' Get desired subset of US population data, adding in columns related to age.
#'
#' Subset the US population data for total population data from 2018, and add
#' columns relating to lower and upper bounds on ages and their differences.
#'
#' @param usPop Dataframe containing information on population in the United
#' States as produced in the us_Pop_Data function.
process_US_Pop_Data <- function(usPop) {
  # Subset
  usPop <- usPop[grep("2018", usPop$"year.display-label"), ]
  usPop <- usPop[grep("Total", usPop$"hisp.display-label"), ]
  usPop <- usPop[, grep("GEO.display|totpop_sex0_age([[:digit:]]+to|85)", names(usPop))]

  # !
  usPop <- reshape2::melt(usPop, id.vars = "GEO.display-label", value.name = "pop")
  # Age
  usPop$age <- gsub("^.*[_]age", "", usPop$variable)
  usPop <- usPop[grep("_$", usPop$age, invert = TRUE), ]
  usPop$age <- gsub("plus", "to120", usPop$age)
  usPop$ageLow <- as.numeric(gsub("[[:alpha:]].+", "", usPop$age))
  usPop$ageHigh <- as.numeric(gsub(".*[[:alpha:]]", "", usPop$age))
  usPop$ageDiff <- usPop$ageHigh - usPop$ageLow
  usPop <- usPop[usPop$ageDiff == 4 | usPop$ageLow == 85, ]
  # Name columns
  colnames(usPop) <- gsub("GEO.*", "state", colnames(usPop))
  return(usPop)
}


#' Population data for Maharashtra and Delhi in India
#'
#' Contains population data for Maharashtra and Delhi from ages 0 - 100
india_Pop_Data <- function() {
  library("readxl")

  # Data for Maharashtra
  mahaUrl <- "https://censusindia.gov.in/2011census/C-series/c-13/DDW-2700C-13.xls"
  mahaFile <- tempfile(fileext = ".xls")
  system(paste("curl --insecure --output", mahaFile, mahaUrl))
  mahaPop <- as.data.frame(read_excel(mahaFile, skip = 6))
  mahaPop <- mahaPop[grep("MAHAR", mahaPop[, 4]), 5:6]
  colnames(mahaPop) <- c("age", "pop")
  mahaPop$state <- "Maharashtra"

  # Data for Delhi
  delhiUrl <- "https://censusindia.gov.in/2011census/C-series/c-13/DDW-0700C-13.xls"
  delhiFile <- tempfile(fileext = ".xls")
  system(paste("curl --insecure --output", delhiFile, delhiUrl))
  delhiPop <- as.data.frame(read_excel(delhiFile, skip = 6))
  delhiPop <- delhiPop[grep("DELHI", delhiPop[, 4]), 5:6]
  colnames(delhiPop) <- c("age", "pop")
  delhiPop$state <- "Delhi"

  # Further processing
  indiaPop <- rbind(mahaPop, delhiPop)
  indiaPop <- indiaPop[grep("not stated", indiaPop$age, invert = TRUE), ]
  indiaPop$age <- gsub("[[:punct:]]", "", as.character(indiaPop$age))
  indiaPop[, "age"] <- as.numeric(indiaPop[, "age"])
  indiaPop[, "pop"] <- as.numeric(indiaPop[, "pop"])
  indiaPop
}


#' Process China population data
#'
#' Return population and expected mortality per province in China
#'
#' @param chinaPop Dataframe containing columns about the province in China,
#'   ageLow (lower bound on age for row), ageHigh (upper bound on age for row),
#'   pop (population) and ageCut (interval of ageLow and ageHigh).
#' @param ratesByAge Dataframe containing mortality rates for each of the age
#' intervals in chinaPop
china_Expected <- function(chinaPop, ratesByAge) {
  # Get rates from ratesByAge corresponding to chinaPops ageCut variable
  chinaPop$rate <- ratesByAge[as.character(chinaPop$ageCut)]
  chinaPop$Expected <- chinaPop$rate * chinaPop$pop

  # Split pop and Expected into different rows
  suppressWarnings(chinaMelt <- reshape2::melt(chinaPop,
    id.vars = c("province", "ageLow", "ageHigh", "ageCut"),
    measure.vars = c("pop", "Expected")
  ))

  # Sum through pop and Expected
  chinaExpected <- reshape2::dcast(chinaMelt, province ~ variable,
    fun.aggregate = sum, value.var = "value"
  )
  chinaExpected$mMult <- 1e6 / sum(chinaExpected$pop)
  return(chinaExpected)
}


#' Expected mortality for regions
#'
#' Return expected mortality rate, population, and deaths per million for
#' regions given to the function. Regions could be countries around the world,
#' states within a country.
#'
#' @param popData Dataframe containing detailed population statistics for
#'   regions. Must contain columns c(ageVar, regionVar, "pop")
#' @param ratesByAge  Dataframe containing info on mortality rates per age
#'   group.
#' @param ageVar Name of column that contains lower bound of age groups. Default
#'   is "AgeGrpStart".
#' @param regionVar Name of column that contains regions. Default is "country".
world_Expected <- function(popData, ratesByAge,
                           ageVar = "AgeGrpStart", regionVar = "country") {
  popData$ageCut <- cut(popData[[ageVar]], c(ratesByAge$age, Inf),
    labels = ratesByAge$age, right = FALSE
  )

  # Assign rates from ratesByAge to correspondng ageCut in rate variable
  popData$rate <- ratesByAge[
    match(
      as.character(popData$ageCut), as.character(ratesByAge$age)
    ),
    "rate"
  ]
  popData$Expected <- popData$rate * popData$pop

  # Aggregate population and expected mortality
  result <- aggregate(
    popData[, c("Expected", "pop")], popData[, regionVar, drop = FALSE],
    sum
  )
  result$mMult <- 1e6 / result$pop
  result
}
