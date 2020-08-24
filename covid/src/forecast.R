
# TODO: Remove bearer tokens, credentials, move to separate file
# --------------------------------------------

# Main function for file
covid_forecast <- function(theSamples, nCov_fit,
                           mc.cores = 4, min_deaths = 50,
                           regionNamesAll, usAgg = TRUE,
                           Stime) {
  if(missing(Stime)) {
  endDate <- as.Date("2020/09/30")
  Stime <- seq(as.Date("2020/01/01"), endDate, by = "1 days")
  }
  ## Total deaths by reporting unit
  nDeaths <- tapply(nCov_fit$data$dead, nCov_fit$data$reporting_unit, sum)
  if (missing(regionNamesAll)) {
    regionNamesAll <- names(nDeaths[which(nDeaths >= min_deaths)])
  }
  # regionNamesAll = c('Hubei','Italy','Spain')

  if (!all(regionNamesAll %in% dimnames(theSamples)[[2]])) {
    warning("regions missing", paste(
      regionNamesAll[!regionNamesAll %in% dimnames(theSamples)[[2]]]
    ), collapse = ", ")
  }

  theForecasts <- parallel::mcmapply(forecast_ahead,
    region_name = regionNamesAll,
    SIMPLIFY = FALSE, mc.cores = mc.cores,
    MoreArgs = list(
      data = nCov_fit$data, Stime = Stime,
      allSamples = theSamples, phi_samples = nCov_fit$phi
    )
  )
  names(theForecasts) <- regionNamesAll

  if (usAgg) {
    # aggregate US
    theStates <- setdiff(
      unique(as.character(nCov_fit$data[
        nCov_fit$data$country == "United States",
        "reporting_unit"
      ])),
      "United States"
    )

    theForecastUsSeparate <- theForecasts[
      names(theForecasts) %in% as.character(theStates)
    ]
    theForecastUs <- list()
    if (length(theForecastUsSeparate)) {
      for (DD in setdiff(names(theForecastUsSeparate[[1]]), c("peak", "intervals"))) {
        theForecastUs[[DD]] <- Reduce(
          "+",
          Biobase::subListExtract(theForecastUsSeparate, DD)
        )
      }


      theForecastUs$intervals <- forecast_quant(
        theForecastUs, Stime
      )
      theForecastUs$intervals$reporting_unit <- "United States"


      # when epidemic is less than one case per day
      indSeq <- seq(1, dim(theForecastUs$lambda)[1])
      whenOver <- t(apply(theForecastUs$lambda, 2, function(xx) {
        postMode <- which.max(xx)
        suppressWarnings(xxx <- c(
          peak = as.numeric(Stime)[postMode],
          over = as.numeric(Stime)[min(which(indSeq > postMode & xx < 10))]
        ))
      }))
      whenOver <- cbind(whenOver,
        maxDaily =
          apply(theForecastUs$cases, 2, max)
      )
      whenOver[is.na(whenOver[, "over"]), "over"] <- 99999
      theForecastUs$peak <- whenOver

      theForecasts$"United States Aggregated" <- theForecastUs
    }
  } # usAgg
  list(forecasts = theForecasts, time = Stime)
}

forecast_ahead <- function(region_name, samples, allSamples, Stime, data, phi_samples) {
  Nk <- 100
  Sprob <- c(0.01, 0.025, 0.1, 0.25, 0.5)
  Sglobal <- c(0.25, 0.5, 0.8, 0.95)

  Sprob <- sort(unique(c(Sprob, 1 - Sprob)))

  StimeInt <- as.numeric(Stime)
  if (missing(samples)) {
    samples <- allSamples[, region_name, ]
  }
  sampleHere <- samples

  Nfull <- dim(sampleHere)[1]
  Nk <- min(c(Nfull, Nk))
  k <- sort(unique(round(seq(1, Nfull, len = Nk))))


  if (!missing(region_name)) {
    if (region_name %in% data$reporting_unit) {
      data <- data[data$reporting_unit == region_name, ]
    }
  }
  theOffset <- data[1, "Expected"]

  lambdaSansEta <- exp(rep(
    log(theOffset) + log(sampleHere[, "C"]),
    each = length(StimeInt)
  ) +
    sn::dsn(
      rep(StimeInt, nrow(sampleHere)),
      rep(sampleHere[, "A"], each = length(StimeInt)),
      rep(sampleHere[, "B"], each = length(StimeInt)),
      rep(sampleHere[, "K"], each = length(StimeInt)),
      log = TRUE
    ))
  lambdaSansEta <- matrix(lambdaSansEta, length(StimeInt), nrow(sampleHere))
  lambda <- lambdaSansEta +
    theOffset * rep(sampleHere[, "eta"], each = length(StimeInt))


  # if(FALSE) {
  cases <- matrix(
    MASS::rnegbin(
      n = length(lambda),
      mu = as.vector(lambda),
      theta = rep(phi_samples, each = length(lambda))
    ),
    nrow = nrow(lambda)
  )
  cases[is.na(cases)] <- 1e10
  cases[cases > 1e10] <- 1e10

  if (!missing(data)) {
    casesWithData <- cases
    maxHaveData <- max(data$time)
    casesWithData[which(Stime <= maxHaveData), ] <- 0
    casesWithData[match(data$time, Stime), ] <- data$dead

    casesWithDataCumulative <- apply(casesWithData, 2, cumsum)
  } else {
    casesWithData <- cases
    casesWithDataCumulative <- NULL
  }

  # when epidemic is less than one case per day
  indSeq <- seq(1, dim(lambda)[1])
  whenOver <- t(apply(lambdaSansEta, 2, function(xx) {
    postMode <- which.max(xx)
    c(
      peak = StimeInt[postMode],
      over = min(c(Inf, StimeInt[min(c(
        Inf,
        which(indSeq > postMode & xx < 1)
      ), na.rm = TRUE)]), na.rm = TRUE)
    )
  }))
  whenOver <- cbind(whenOver,
    maxDaily = apply(casesWithData, 2, max)
  )

  result <- list(
    cases = cases, lambda = lambda,
    cumulative = casesWithDataCumulative,
    peak = whenOver
  )

  forecastQuant <- forecast_quant(result, Stime, Sglobal, Sprob)

  if (FALSE) {
    if (!missing(region_name)) {
      forecastQuant$reporting_unit <- region_name
    } else if (!missing(data)) {
      forecastQuant$reporting_unit <- data$reporting_unit[1]
    }
  }
  result$intervals <- forecastQuant
  result
}


forecast_quant <- function(x, Stime,
                           Sglobal = c(0.25, 0.5, 0.8, 0.95),
                           Sprob = c(0.01, 0.025, 0.1, 0.25, 0.5)) {
  Sprob <- sort(unique(signif(c(Sprob, 1 - Sprob), 10)))

  Soutcome <- c(cases = "cases", lambda = "intensity")

  forecastQuant <- data.frame(
    time = Stime,
    central_cases = apply(x$cases, 1, median),
    central_intensity = signif(apply(x$lambda, 1, median)), 3
  )



  for (Doutcome in names(Soutcome)) {
    for (Dglobal in Sglobal) {
      bob <- as.data.frame(GET::central_region(GET::create_curve_set(list(
        r = as.numeric(Stime),
        obs = x[[Doutcome]]
      )), coverage = Dglobal))[, c("lo", "hi")]

      colnames(bob) <- paste(colnames(bob), Dglobal,
        Soutcome[Doutcome],
        sep = "_"
      )
      forecastQuant <- cbind(forecastQuant, signif(bob, 2))
    }
  }

  bobCumul <-
    t(apply(x$cumulative, 1, quantile, prob = Sprob))
  colnames(bobCumul) <- paste(colnames(bobCumul), "cumulative", sep = "_")

  theCbind <- cbind(forecastQuant, as.data.frame(bobCumul))


  theCbind
}


forecast_summaries <- function(
                               theForecasts, Stime, threshold = NULL,
                               nCov_data) {
  # use only 200 samples if memory below 50gb
  if (as.numeric(benchmarkme::get_ram()) < 50000000000) {
    Nk <- 200
  } else {
    Nk <- 500
  }
  Nfull <- ncol(theForecasts[[1]]$cases)
  Nk <- min(c(Nfull, Nk))
  k <- sort(unique(round(seq(1, Nfull, len = Nk))))

  forecastsLambda <- forecastQuant <- forecastSummaries <- list()
  Sprob <- c(0.01, 0.025, 0.1, 0.25, 0.5)
  Sglobal <- c(0.25, 0.5, 0.8, 0.95)
  Sprob <- sort(unique(c(Sprob, 1 - Sprob)))
  Soutcome <- c(cases = "cases", lambda = "intensity")


  for (D in names(theForecasts)) {
    #    theForecasts[[D]]$cases[is.na(theForecasts[[D]]$cases)] <- 1e10
    #    theForecasts[[D]]$cases[theForecasts[[D]]$cases > 1e10] <- 1e10
    #    theForecasts[[D]]$cumulative[is.na(theForecasts[[D]]$cumulative)] <- 1e10
    #    theForecasts[[D]]$cumulative[is.na(theForecasts[[D]]$cumulative)] <- 1e10

    dataHere <- nCov_data[nCov_data$reporting_unit == D, ]
    casesAndRealData <- theForecasts[[D]]$cases[, k]
    # before end of observation window
    # replace simualted cases with real data
    # if dates are missing in real data, they're zero
    beforeEnd <- which(Stime < max(dataHere$time))
    casesAndRealData[beforeEnd, ] <- 0
    haveData <- match(dataHere$time, Stime)
    casesAndRealData[haveData, ] <- dataHere$dead

    cumulativeWithReal <- apply(casesAndRealData, 2, cumsum)

    forecastsLambda[[D]] <- data.frame(
      region = D,
      time = rep(Stime, length(k)),
      sample = rep(1:length(k), each = length(Stime)),
      intensity = as.vector(theForecasts[[D]]$lambda[, k]),
      cases = as.vector(casesAndRealData),
      cumulative = as.vector(cumulativeWithReal)
    )
    # forecastQuant[[D]] = cbind(data.frame(region=D,
    #   time = Stime),
    #   t(apply(theForecasts[[D]]$cases, 1, quantile, na.rm=TRUE, prob=Sprob))    )

    forecastQuant[[D]] <- theForecasts[[D]]$intervals
    forecastQuant[[D]]$reporting_unit <- D
    forecastSummaries[[D]] <- data.frame(
      reporting_unit = D,
      totalDeaths = theForecasts[[D]]$cumulative[
        nrow(theForecasts[[D]]$cumulative),
      ]
    )

    if (!is.null(theForecasts[[D]]$peak)) {
      forecastSummaries[[D]] <- cbind(
        forecastSummaries[[D]],
        as.data.frame(theForecasts[[D]]$peak)
      )
    } else {
      for (D2 in colnames(theForecasts[[1]]$peak)) {
        forecastSummaries[[D]][[D2]] <- NA
      }
    }
    if (!is.null(threshold)) {
      if (!is.na(threshold[D])) {
        forecastSummaries[[D]]$overThreshold <-
          apply(theForecasts[[D]]$cases > threshold[D], 2, sum)
      }
    }
    if (!length(forecastSummaries[[D]]$overThreshold)) {
      forecastSummaries[[D]]$overThreshold <- NA
    }
  }


  toMerge <- do.call(rbind, forecastQuant)

  intervalsAndData1 <- merge(
    toMerge,
    nCov_data[, c("reporting_unit", "time", "dead")],
    by = c("time", "reporting_unit"),
    all = TRUE
  )

  toMerge2 <- nCov_data[!is.na(nCov_data$mMult), ]
  toMerge3 <- toMerge2[!duplicated(toMerge2$reporting_unit), ]

  intervalsAndData <- merge(
    intervalsAndData1,
    toMerge3[, c("reporting_unit", "country", "Expected", "mMult")],
    all = TRUE
  )
  intervalsAndData <- intervalsAndData[!is.na(intervalsAndData$central_cases), ]
  intervalsAndData$adjustedDead <- round(intervalsAndData$dead * intervalsAndData$mMult, 2)
  
  intervalsAndData <- subset(intervalsAndData, select = -X3)

  # Continent column
  # intervalsAndData$continent <- countrycode::countrycode(
  #   sourcevar = intervalsAndData[, "country"],
  #   origin = "country.name",
  #   destination = "continent"
  # )

  intervalsAndData <- intervalsAndData[
    order(intervalsAndData$reporting_unit, intervalsAndData$time),
  ]

  intervalsAndDataRaw <- intervalsAndData

  # Rename cumulative to cumulativeRaw, and keep mMult
  cumulNames <- colnames(intervalsAndData)[grep("cumulative", colnames(intervalsAndData))]
  cumulCopy <- setNames(intervalsAndData[cumulNames], paste0(cumulNames, "Raw"))

  # Multiply cumulative and intensity by mMult
  intervalsAndData[, grepl("cumulative", names(intervalsAndData))] <-
    round(
      intervalsAndData[, grepl("cumulative", names(intervalsAndData))] * intervalsAndData[, "mMult"],
      2
    )
  
  intervalsAndData[, grepl("intensity", names(intervalsAndData))] <-
    round(
      intervalsAndData[, grepl("intensity", names(intervalsAndData))] * intervalsAndData[, "mMult"],
      2
    )
  
  
  intervalsAndData <- cbind(intervalsAndData, cumulCopy)



  intervalsAndData <- intervalsAndData[order(intervalsAndData$reporting_unit, intervalsAndData$time), ]
  # intervalsAndData$cum_dead =

  # Final listing of dataframes to return
  toSave <- list(
    samples = do.call(rbind, forecastsLambda),
    intervalsPerMillion = intervalsAndData,
    intervals = intervalsAndDataRaw,
    summaries = do.call(rbind, forecastSummaries)
  )
  colnames(toSave$intervals) <- gsub("[.]", "_", colnames(toSave$intervals))
  colnames(toSave$intervals) <- gsub("%", "pct", colnames(toSave$intervals))
  colnames(toSave$intervalsPerMillion) <- gsub("[.]", "_", colnames(toSave$intervalsPerMillion))
  colnames(toSave$intervalsPerMillion) <- gsub("%", "pct", colnames(toSave$intervalsPerMillion))

  toSave
}
