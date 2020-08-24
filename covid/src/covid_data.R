# Functions in this file perform the final processing of the
# data, to be used in the forecast functions.

source("credentials.R")


#' Daily COVID19 data for countries around the world, from "nCov2019" package
#'
#' Return data frame containing daily COVID19 stats such as deaths and
#' cumulative deaths, and also static stats such as population, expected deaths,
#' and expected deaths per million.
#'
#' Observed data for this function is sourced from the "nCov2019" R package and
#' NY Times data.
#'
#' @param covid_expected Data frame corresponding to the output of the
#'   get_covid_expected_counts function from covid_expected_counts.R
get_covid_data <- function(covid_expected) {
  chinaExpected <- covid_expected$china
  usExpected <- covid_expected$us
  worldExpected <- covid_expected$world

  suppressWarnings(
    nCov_data <- nCov2019::load_nCov2019(lang = "en", source = "github")
  )

  # Merge new data with the covid_expected data, and split into groups based on
  # country, province, or state.

  # Merge global data with expected counts, split by counts
  nCov_data$global2 <- merge(nCov_data$global, worldExpected,
    by = "country"
  )
  nCov_data$global3 <- nCov_data$global2[
    order(nCov_data$global2$country, nCov_data$global2$time),
  ]
  nCov_data$global3$reporting_unit <- nCov_data$global3$country
  nCov_global <- split(nCov_data$global3, nCov_data$global3[, "reporting_unit"])

  # Merge China data with expected counts, split by province
  nCov_data$province2 <- merge(nCov_data$province, chinaExpected,
    by = "province", all = TRUE
  )
  nCov_data$province2 <- nCov_data$province2[
    order(nCov_data$province2$province, nCov_data$province2$time),
  ]
  # nCov_data$province2$country = 'China'
  nCov_data$province2$reporting_unit <- nCov_data$province2$province
  nCov_province <- split(nCov_data$province2, nCov_data$province2[, "province"])

  # usData
  # https://github.com/nytimes/covid-19-data/
  nCov_US <- read.csv(url(
    "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
  ), stringsAsFactors = FALSE)

  nCov_US <- data.frame(
    time = as.Date(nCov_US$date),
    reporting_unit = nCov_US$state,
    cum_confirm = nCov_US$cases,
    cum_heal = NA, cum_dead = nCov_US$deaths,
    country = "United States"
  )

  # Merge US data with expected counts, split by state
  nCov_US <- merge(nCov_US, usExpected, by.x = "reporting_unit", by.y = "state", all = TRUE)
  nCov_US <- nCov_US[order(nCov_US$reporting_unit, nCov_US$time), ]
  nCov_US_split <- split(nCov_US, nCov_US$reporting_unit)

  # Combine the split data sets and process them
  nCov_combined <- c(nCov_global, nCov_province, nCov_US_split)
  for (ii in 1:length(nCov_combined)) {
    # Computer daily deaths
    nCov_combined[[ii]]$dead <-
      diff(c(0, nCov_combined[[ii]]$cum_dead))
    # Look for negatives
    getRid <- which(nCov_combined[[ii]]$dead < 0)
    if (length(getRid)) {
      getRid <- max(getRid)
      nCov_combined[[ii]] <- nCov_combined[[ii]][-(1:getRid), ]
    }
    # Start data frames from day after first death
    firstDeath <- min(c(Inf, which(nCov_combined[[ii]]$dead > 0)))
    if (firstDeath < nrow(nCov_combined[[ii]])) {
      nCov_combined[[ii]] <-
        nCov_combined[[ii]][seq(firstDeath + 1, nrow(nCov_combined[[ii]])), ]
    }
  }

  # clean french data
  # April 4 (GMT)
  # 7788 new cases and 1053 new deaths in France [source]
  # https://www.worldometers.info/coronavirus/country/france/
  # France: on April 3 the French Government reported 17,827 additional cases and 532 additional deaths from nursing homes that had not been reported previously. On April 2, it had reported 884 additional deaths.
  # April 3 (GMT)
  # 23060 new cases and 1120 new deaths in France
  # On April 2, France reported 884 additional deaths that have occurred in nursing homes
  # toFix = grep("2020-04-0[34]",as.character(nCov_combined$France$time))
  # nCov_combined$France[toFix, 'dead'] = c(1355, 1120)- c(884,532)

  # Combine-Data-At-Reporting-Unit-Level}
  # Subset specific columns of split data
  nCov_combined2 <- lapply(
    nCov_combined,
    function(xx) xx[, c("country", "reporting_unit", "time", "dead", "Expected", "mMult", "pop")]
  )

  # Combine split data frames into one data frame
  nCov_All <- do.call(rbind, nCov_combined2)
  # nCov_All = nCov_All[
  #  grep("China|United States", nCov_All$reporting_unit, invert=TRUE),] # remove China - have provinces

  # Remove regions with fewer than 200 death and further processing
  nCov_All <- nCov_All[!is.na(nCov_All$time), ]
  whichOver <- names(which(tapply(nCov_All$dead,
    nCov_All$reporting_unit, sum,
    na.rm = TRUE
  ) > 200))
  # ßwhichOver = c('Hubei','Italy','Spain', 'New York', 'Canada',
  #               'Washington','France','United Kingdom')
  nCov_All <- nCov_All[nCov_All$reporting_unit %in% whichOver, ]
  nCov_All <- nCov_All[!is.na(nCov_All$Expected), ]
  nCov_All$reporting_unit <- factor(as.character(nCov_All$reporting_unit))
  nCov_All$reporting_unit_int <- as.integer(nCov_All$reporting_unit)
  nCov_All$timeNumeric <- as.numeric(nCov_All$time)
  nCov_All$logExpected <- log(nCov_All$Expected)

  # Create data frame for US states
  nCov_US <- nCov_All[nCov_All$country == "United States" &
    !nCov_All$reporting_unit == "United States", ]
  nCov_US_agg <- aggregate(
    nCov_US[, c("dead", "Expected", "pop")],
    nCov_US[, c("country", "time")],
    FUN = sum, na.rm = TRUE
  )

  nCov_US_agg[c(
    "cum_confirm", "cum_heal", "cum_dead",
    "timeNumeric", "logExpected", "reporting_unit_int"
  )] <- NA
  nCov_US_agg$reporting_unit <- "United States"
  nCov_US_agg$mMult <- 1e6 / nCov_US_agg$pop
  nCov_US_agg <- nCov_US_agg[, names(nCov_All)]

  list(all = nCov_All, us = nCov_US_agg)
}

#' Daily COVID19 data for countries around the world, from local database
#'
#' Return data frame containing daily COVID19 stats such as deaths and
#' cumulative deaths, and also static stats such as population, expected deaths,
#' and expected deaths per million. Observed data  sourced from a local database
#' containing data from coronavirus.app.
#'
#' @param covid_expected Data frame corresponding to the output of the
#'   get_covid_expected_counts function from covid_expected_counts.R
get_covid_data_cva_db <- function(covid_expected) {
  RPostgreSQL::PostgreSQL()
  drv <- DBI::dbDriver("PostgreSQL")

  # Credentials stored in credentials.R
  con <- DBI::dbConnect(drv,
    dbname = dbname,
    host = host,
    user = user,
    password = password
  )

  # Daily COVID19 data from cities around the world
  nCov_DB <- DBI::dbGetQuery(con, "SELECT * FROM cvapp_covid19_v1")
  nCov_DB$countryCode <- nCov_DB$country
  nCov_DB$country <- countrycode::countrycode(nCov_DB$countryCode,
    origin = "iso2c", destination = "country.name"
  )

  # Aggregate deaths by countryCode, country, day (u.e. sum over cities)
  nCov_DB_agg <- aggregate(
    nCov_DB$dead, nCov_DB[, c("countryCode", "country", "day")],
    sum
  )
  nCov_DB_agg$reporting_unit <- nCov_DB_agg$country
  names(nCov_DB_agg) <- gsub("^x$", "dead", names(nCov_DB_agg))

  # Subset city data for USA and China
  nCov_DB_subnational <- nCov_DB[nCov_DB$countryCode %in% c("US", "CN"), ]
  nCov_DB_subnational <- nCov_DB_subnational[
    grep("Unknown", nCov_DB_subnational$place, invert = TRUE),
  ]
  nCov_DB_subnational$reporting_unit <- nCov_DB_subnational$place

  # Subset and bind columns from aggregated and subnational,
  toBindCols <- c("country", "reporting_unit", "day", "countryCode", "dead")
  nCov_All_bind <- rbind(
    nCov_DB_agg[, toBindCols],
    nCov_DB_subnational[, toBindCols]
  )

  # Get cumulative deaths, subset reporting units with total counts > 200
  totalCounts <- tapply(nCov_All_bind$dead, nCov_All_bind$reporting_unit, max)
  nCov_sub <- nCov_All_bind[
    nCov_All_bind$reporting_unit %in% names(which(totalCounts > 200)),
  ]
  colnames(nCov_sub) <- gsub("^day$", "time", colnames(nCov_sub))
  colnames(nCov_sub) <- gsub("^dead$", "cum_dead", colnames(nCov_sub))

  # Split nCov_sub into data frames by reporting unit, process each of them
  nCov_combined <- split(nCov_sub, nCov_sub[, "reporting_unit", drop = FALSE])
  for (ii in 1:length(nCov_combined)) {
    # Order data frames by day, compute daily deaths
    nCov_combined[[ii]] <- nCov_combined[[ii]][order(nCov_combined[[ii]]$time), ]
    nCov_combined[[ii]]$dead <-
      diff(c(0, nCov_combined[[ii]]$cum_dead))
    # Look for negatives
    getRid <- which(nCov_combined[[ii]]$dead < 0)
    if (length(getRid)) {
      getRid <- max(getRid)
      nCov_combined[[ii]] <- nCov_combined[[ii]][-(1:getRid), ]
    }
    # Start data frames from day after first death
    firstDeath <- min(c(Inf, which(nCov_combined[[ii]]$dead > 0)))
    if (firstDeath < nrow(nCov_combined[[ii]])) {
      nCov_combined[[ii]] <-
        nCov_combined[[ii]][seq(firstDeath + 1, nrow(nCov_combined[[ii]])), ]
    }
  }

  # Merge split data frames into one data frame again
  nCov_All_counts <- do.call(rbind, nCov_combined)

  covid_expected$us$state <- gsub("Georgia$", "Georgia (US)", covid_expected$us$state)
  covid_expected$us$reporting_unit <- covid_expected$us$state
  covid_expected$world$reporting_unit <- covid_expected$world$country
  covid_expected$china$reporting_unit <- covid_expected$china$province

  # Row bind subset of columns from data frames in covid_expected
  toBindExp <- c("reporting_unit", "pop", "Expected", "mMult")
  covid_expected_bind <- rbind(
    covid_expected$us[, toBindExp],
    covid_expected$world[, toBindExp],
    covid_expected$china[, toBindExp]
  )

  # Merge data per country, including data on deaths per day/cumulative deaths
  # and expected total deaths
  nCov_All <- merge(
    nCov_All_counts,
    covid_expected_bind,
    all.x = TRUE, all.y = FALSE, by = "reporting_unit"
  )
  nCov_All$timeNumeric <- as.numeric(nCov_All$time)
  nCov_All$reporting_unit_fac <- factor(nCov_All$reporting_unit)
  nCov_All$reporting_unit_int <- as.integer(nCov_All$reporting_unit_fac)
  nCov_All$logExpected <- log(nCov_All$Expected)
  nCov_All
}

#' Daily COVID19 data for countries around the world, from cache or online
#'
#' Return data frame containing daily COVID19 stats such as deaths and
#' cumulative deaths, and also static stats such as population, expected deaths,
#' and expected deaths per million. Observed data sourced from either a cached
#' RDS file of data from coronavirus.app, or if it doesn't exist, the function
#' coronavirus.app and creates one.
#'
#' @param covid_expected data frame corresponding to the output of the
#'   get_covid_expected_counts function from covid_expected_counts.R
#' @param min_deaths Minimum total death threshold to include a country.
#' @param exclude String vector containing country codes of countries to
#'   exclude. Default is c("FR, "BE")
get_covid_data_cva <- function(covid_expected,
                               min_deaths = 100,
                               exclude = c("FR", "BE")) {
  cacheFile <- "cvaCache.rds"
  # reload <- TRUE

  # If cache file doesn't exist, reload
  reload <- !file.exists(cacheFile)
  if (!reload) {
    if (as.numeric(diff(as.Date(c(file.info(cacheFile)$mtime, Sys.time()))))) {
      reload <- TRUE
    }
  }
  if (reload) {
    message("reloading")
    library("httr")
    theHeaders <- httr::add_headers(
      authorization = paste0("authorization: Bearer ", bearer),
      "content-type" = "application/json"
    )
    # Get data on deaths from cities from coronavirus.app
    xPlace <- getPlaces(theHeaders, min_deaths)
    xPlace <- xPlace[!(xPlace$country %in% exclude), ]
    # Get data per country, based on id from xPlace, save into cache file
    xAllRaw <- mapply(getCountry,
      id = xPlace$id, MoreArgs = list(headers = theHeaders),
      SIMPLIFY = FALSE
    )
    saveRDS(xAllRaw, cacheFile)
  } else {
    xAllRaw <- readRDS(cacheFile)
  }

  # Row bind the data frames in xAllRaw into one data frame
  nCov_DB <- do.call(rbind, xAllRaw)

  # Aggregate deaths by countryCode, date (i.e. sum over regions)
  nCov_DB_agg <- aggregate(
    nCov_DB$cum_dead, nCov_DB[, c("countryCode", "date")],
    sum
  )
  names(nCov_DB_agg) <- gsub("^x$", "cum_dead", names(nCov_DB_agg))
  # Add country names
  nCov_DB_agg$country <- countrycode::countrycode(
    nCov_DB_agg$countryCode,
    origin = "iso2c", destination = "country.name"
  )
  nCov_DB_agg$reporting_unit <- nCov_DB_agg$country

  # Subset subnational region data for USA, China (Hubei), and India
  # (Maharashtra)
  nCov_DB_subnational <- nCov_DB[nCov_DB$countryCode %in% c("US", "CN", "IN"), ]
  nCov_DB_subnational <- nCov_DB_subnational[
    grep("Unknown", nCov_DB_subnational$name, invert = TRUE),
  ]
  nCov_DB_subnational$reporting_unit <- nCov_DB_subnational$name
  nCov_DB_subnational$country <- countrycode::countrycode(
    nCov_DB_subnational$countryCode,
    origin = "iso2c", destination = "country.name"
  )
  nCov_DB_subnational$reporting_unit <- gsub(
    "Georgia$", "Georgia (US)",
    nCov_DB_subnational$reporting_unit
  )

  # Split  global and subnational data into data frames per region, process each
  # i.e. processing world on country level, and specific countries on region level
  toBindCols <- c("country", "reporting_unit", "date", "countryCode", "dead", "cum_dead")
  nCov_All_list <- lapply(
    c(
      split(nCov_DB_agg, nCov_DB_agg$reporting_unit),
      split(nCov_DB_subnational, nCov_DB_subnational$reporting_unit)
    ),
    function(xx) {
      # Order by date, get daily deaths, and filter out regions with no total
      # deaths
      xx <- xx[order(xx$date), ]
      xx$dead <- diff(c(0, xx$cum_dead))
      xx <- xx[which(xx$cum_dead > 0), ]

      # Start data frames from day after first death
      firstDeath <- min(c(Inf, which(xx$dead > 0)))
      if (firstDeath < nrow(xx)) {
        xx <- xx[seq(firstDeath + 1, nrow(xx)), ]
      }
      xx[, toBindCols]
    }
  )

  # Combine data frames from nCov_All_list
  nCov_All_bind <- do.call(rbind, nCov_All_list)

  # Get cumulative deaths, subset reporting units with total counts > min_deaths
  totalCounts <- tapply(nCov_All_bind$cum_dead, nCov_All_bind$reporting_unit, max)
  nCov_sub <- nCov_All_bind[
    nCov_All_bind$reporting_unit %in% names(which(totalCounts > min_deaths)),
  ]
  colnames(nCov_sub) <- gsub("^date$", "time", colnames(nCov_sub))

  # Process covid_expected data
  covid_expected$us$state <- gsub("Georgia$", "Georgia (US)", covid_expected$us$state)
  covid_expected$us$reporting_unit <- covid_expected$us$state

  covid_expected$world$reporting_unit <- covid_expected$world$country
  covid_expected$china$reporting_unit <- covid_expected$china$province
  covid_expected$india$reporting_unit <- covid_expected$india$state

  # Row bind US, China, and world's data for subset of columns in covid_expected
  toBindExp <- c("reporting_unit", "pop", "Expected", "mMult")
  covid_expected_bind <- rbind(
    covid_expected$us[, toBindExp],
    covid_expected$world[, toBindExp],
    covid_expected$china[, toBindExp],
    covid_expected$india[, toBindExp]
  )

  # Merge data per country, including data on deaths per day/cumulative deaths
  # and expected total deaths
  nCov_All <- merge(
    nCov_sub,
    covid_expected_bind,
    all.x = TRUE, all.y = FALSE, by = "reporting_unit"
  )
  nCov_All[order(nCov_All$reporting_unit, nCov_All$time), ]
  # covid_expected_bind
}


#' COVID19 mortality data from cities around the world
#'
#' Returns data frame containing info on deaths per city and total deaths in its
#' country for cities around the world. Data sourced from coronavirus.app.
#'
#' This function returns data for cities where the number of deaths is > 0 and
#' the number of deaths in its country is > 50.
#'
#' @param theHeaders Headers for httr GET request. Includes authorization bearer
#'   token and content-type.
#' @param deathThreshold Minimum death threshold to get data from. Default is
#'   50.
getPlaces <- function(theHeaders, deathThreshold = 50) {
  xPlace <- httr::content(httr::GET(
    "https://coronavirus.app/get-places",
    theHeaders
  ))$data

  # Convert the JSON return data to a data frame
  xPlace <- data.frame(
    id = unlist(Biobase::subListExtract(xPlace, "id")),
    country = unlist(Biobase::subListExtract(xPlace, "country")),
    dead = unlist(Biobase::subListExtract(xPlace, "dead")),
    name = unlist(Biobase::subListExtract(xPlace, "name")),
    stringsAsFactors = FALSE
  )

  # Total dead in the city's country
  xPlace$totalDead <- unlist(tapply(xPlace$dead, xPlace$country, sum))[
    xPlace$country
  ]
  # .
  xPlace <- xPlace[xPlace$totalDead > 50 & xPlace$dead > 0, ]
  xPlace <- xPlace[xPlace$country != "CN" | xPlace$name == "Hubei", ]
  xPlace <- xPlace[xPlace$country != "IN" | xPlace$name == "Maharashtra" |
    xPlace$name == "Delhi", ]
  xPlace
}

#' Coronavirus data for country
#'
#' Return data frame of historical coronavirus data for specified country. Data
#' from coronavirus.app.
#'
#' @param id ID of country.
#' @param headers Headers for httr GET request. Includes authorization bearer
#'   token and content-type.
getCountry <- function(id, headers) {
  x2 <- httr::GET(
    paste0("https://coronavirus.app/get-history?id=", id),
    headers
  )
  xHere <- data.frame(
    date =
      as.Date(unlist(
        Biobase::subListExtract(httr::content(x2)$data$history, "day")
      ), format = "%Y%m%d"),
    cum_dead = unlist(
      Biobase::subListExtract(httr::content(x2)$data$history, "dead")
    ),
    countryCode = httr::content(x2)$data$country,
    name = httr::content(x2)$data$name
  )
  xHere
}

#' Redistribute excess amount from outliers
#'
#' This function proportionally redistributes death counts from outliers onto
#' previous dates
#'
#' @param nCov Data frame containing columns in c('country', 'reporting_unit', 'time', 'dead')
#' @param country Country of outlier
#' @param reporting_unit Reporting unit of outlier
#' @param start_date Start date to distribute over (i.e. from start_date to outlier_date)
#' @param outlier_date Date of the outlier
#' @param outlier_actual Value to replace the outlier amount
redistribute <- function(nCov,
                         country,
                         reporting_unit,
                         start_date,
                         outlier_date,
                         outlier_actual) {

  # Subset for the country and reporting unit, in the date range specified
  counts <- nCov[nCov$country == country &
    nCov$reporting_unit == reporting_unit &
    nCov$time >= start_date &
    nCov$time <= outlier_date, ]

  outlier_amount <- counts$dead[counts$time == outlier_date]
  # Replace the outlier value with user supplied value
  counts$dead[counts$time == outlier_date] <- outlier_actual
  excess <- outlier_amount - outlier_actual

  # Work with data apart from the outlier row
  counts2 <- counts[!(counts$time == outlier_date), ]

  # Calculate amount to add to each date, proportionally
  total <- sum(counts2$dead)
  proportion <- counts2$dead / total
  toAdd <- round(proportion * excess)

  # If there's overflow or underflow due to rounding, subtract/add from non-zero
  # entries in toAdd to balance it out
  difference <- sum(toAdd) - excess
  counter <- length(toAdd)
  while (difference != 0 & counter > 0) {
    # To avoid negatives, or adding onto days with no recorded deaths
    if (toAdd[counter] > 0) {
      if (difference > 0) {
        toAdd[counter] <- toAdd[counter] - 1
        difference <- difference - 1
      } else if (difference < 0) {
        toAdd[counter] <- toAdd[counter] + 1
        difference <- difference + 1
      }
    }
    counter <- counter - 1
  }

  # Redistribute the excess amount
  counts$dead[counts$time >= start_date & counts$time < outlier_date] <-
    counts$dead[counts$time >= start_date & counts$time < outlier_date] + toAdd

  # Update original data frame
  nCov$dead[nCov$country == country &
    nCov$reporting_unit == reporting_unit &
    nCov$time >= start_date &
    nCov$time <= outlier_date] <- counts$dead

  nCov
}

#' Fill NAs in a vector with latest non-NA value
#'
#' @param toFill Vector with NAs to fill.
fillForward <- function(toFill) {
  v <- !is.na(toFill)
  c(NA, toFill[v])[cumsum(v) + 1]
}
