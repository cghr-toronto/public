

covid_polygons2 <- function(x,
                           col = NA,
                           verbose = FALSE) {


  intervalMat <- cbind(c(25, 75), c(10, 90), gsub("[.]", "_", c(2.5, 97.5)), c(1, 99))
  intervalIntensity <- outer(
    paste0("0.", c(25, 5, 8, 95), '.'),
        c("lo", "hi"), paste0)

  intervalString <- intervalMat

  opacityString <- as.hexmode(floor(255 * c(0.8, seq(0.5, 0.1, len = ncol(intervalMat) - 1))))

  mMult = x$mMult
  StimeCumul = x$time
  StimeIntens = x$timeIntensity
    StimeIntIntens <- as.numeric(StimeIntens)
    StimeIntCumul = as.numeric(StimeCumul)

    theCoords <- list()
  forecastPoly <- forecastPolyScaled <-
    list(cumul = list(), intens = list())
    thePolys <- thePolysScaled <- list(cumul = list(), intens = list())


    for (Dpoly in 1:ncol(intervalMat)) {
      theCoords[[Dpoly]] <- list(cumul=cbind(
        c(StimeIntCumul, rev(StimeIntCumul)),
        # upper and lower scaled
        log(pmax(0.0001, c(
          x$cumulCI[, intervalString[1, Dpoly]],
          rev(x$cumulCI[, intervalString[2, Dpoly]])
        )))
        ),
      intens = cbind(
        c(StimeIntIntens, rev(StimeIntIntens)),
        log(pmax(0.0001, c(
          x$intensityCI[, intervalIntensity[Dpoly,1]],
          rev(x$intensityCI[, intervalIntensity[Dpoly,2]])
        ))))
      )
      for(Dtype in c('cumul','intens')) {
        theCoords[[Dpoly]][[Dtype]] = cbind(
          theCoords[[Dpoly]][[Dtype]],
          mMult * theCoords[[Dpoly]][[Dtype]][,2]
          )
      }

    } # Dpoly

    col = col[1[]]
    if(is.na(col)) col = "#D3D3D3"
    colHere <- paste0(col, opacityString)

    for (Dtype in c('cumul','intens')) {
      thePolys[[Dtype]][[1]] <- sp::Polygons(list(
        sp::Polygon(theCoords[[1]][[Dtype]][,1:2],
          hole = FALSE
        )
      ), ID = 1)
      thePolysScaled[[Dtype]][[1]] <- sp::Polygons(list(
        sp::Polygon(theCoords[[1]][[Dtype]][,c(1,3)],
          hole = FALSE
        )
      ), ID = 1)
      for (Dpoly in 2:length(theCoords)) { # columns of intervalsMat
        thePolys[[Dtype]][[Dpoly]] <- sp::Polygons(list(
          sp::Polygon(theCoords[[Dpoly]][[Dtype]][, 1:2], hole = FALSE),
          sp::Polygon(theCoords[[Dpoly - 1]][[Dtype]][, 1:2], hole = TRUE)
        ), ID = Dpoly)
        thePolysScaled[[Dtype]][[Dpoly]] <- sp::Polygons(list(
          sp::Polygon(theCoords[[Dpoly]][[Dtype]][, c(1, 3)], hole = FALSE),
          sp::Polygon(theCoords[[Dpoly - 1]][[Dtype]][, c(1, 3)], hole = TRUE)
        ), ID = Dpoly)
      }
      forecastPoly[[Dtype]] <- sp::SpatialPolygonsDataFrame(
        sp::SpatialPolygons(thePolys[[Dtype]]),
        data.frame(
          col = colHere,
          quant = apply(intervalMat, 1, paste),
          stringsAsFactors = FALSE
        )
      )
      forecastPolyScaled[[Dtype]] <- sp::SpatialPolygonsDataFrame(
        sp::SpatialPolygons(thePolysScaled[[Dtype]]),
        data.frame(
          col = colHere,
          quant = apply(intervalMat, 1, paste),
          stringsAsFactors = FALSE
        )
      )
    }

  if (verbose) cat("\n")

  list(
    unscaled = forecastPoly, 
    scaled = forecastPolyScaled,
    intervalNames = intervalIntensity,
    medians = list(
      unscaled = list(
        intens = cbind(StimeIntens, log(x$intensityCI$median)),
        cumul = cbind(StimeCumul, log(x$cumulCI[,'50']))
        ),
      scaled = list(
        intens = cbind(StimeIntens, log(x$intensityCI$median*mMult)),
        cumul = cbind(StimeCumul, log(x$cumulCI[,'50']*mMult))
        ),
      col = col
      )
  )
}
covid_polygons <- function(covIntervals, 
  Sregion = unique(covIntervals$reporting_unit),
                           Scol = NULL,
                           verbose = FALSE) {
  forecastPoly <- forecastPolyScaled <-
    list(cumulative = list(), intensity = list())
  intervalMat <- cbind(c(25, 75), c(10, 90), gsub("[.]", "_", c(2.5, 97.5)), c(1, 99))
  intervalIntensity <- outer(
    c("lo", "hi"),
    paste0("_0_", c(25, 5, 8, 95), "_intensity"), paste0
  )

  intervalString <- matrix(paste0("X", intervalMat, "pct_cumulative"), ncol = ncol(intervalMat))

  opacityString <- as.hexmode(floor(255 * c(0.8, seq(0.5, 0.1, len = ncol(intervalMat) - 1))))

  toScale <- c(
    "X50pct_cumulative", as.vector(intervalString),
    as.vector(intervalIntensity), "central_cases", "dead",
    "central_intensity"
  )

  covIntervals = covIntervals[order(covIntervals$reporting_unit,
    covIntervals$time), ]
  covIntervalsScaled <- covIntervals
  covIntervalsScaled[, toScale] <-
    covIntervalsScaled[, toScale] * covIntervals[, "mMult"]
  for (D in Sregion) {
    if (verbose) cat(D, " ")
    xHere <- covIntervalsScaled[which(covIntervalsScaled$reporting_unit == D), ]
    if (!nrow(xHere)) warning("cant find", D, "in covIntervals")
    xHere = xHere[order(xHere$time), ]
    xHereUn <- covIntervals[which(covIntervals$reporting_unit == D), ]
    StimeInt <- as.numeric(xHere$time)
    theCoords <- list()
    thePolys <- thePolysScaled <- list(cumulative = list(), intensity = list())
    for (Dpoly in 1:ncol(intervalMat)) {
      theCoords[[Dpoly]] <- cbind(
        c(StimeInt, rev(StimeInt)),
        # upper and lower scaled
        log(pmax(0.0001, c(
          xHere[, intervalString[1, Dpoly]],
          rev(xHere[, intervalString[2, Dpoly]])
        ))),
        log(pmax(0.0001, c(
          xHere[, intervalIntensity[1, Dpoly]],
          rev(xHere[, intervalIntensity[2, Dpoly]])
        ))),
        # upper and loew runscaled
        log(pmax(0.0001, c(
          xHereUn[, intervalString[1, Dpoly]],
          rev(xHereUn[, intervalString[2, Dpoly]])
        ))),
        log(pmax(0.0001, c(
          xHereUn[, intervalIntensity[1, Dpoly]],
          rev(xHereUn[, intervalIntensity[2, Dpoly]])
        )))
      )
    }
    if (D %in% names(Scol)) {
      colHere <- paste0(Scol[D], opacityString)
    } else {
      colHere <- paste0("#D3D3D3", opacityString)
    }
    for (Dtype in 1:2) {
      coords1 <- theCoords[[1]][, c(1, 1 + Dtype)]
      thePolysScaled[[Dtype]][[1]] <- sp::Polygons(list(
        sp::Polygon(coords1,
          hole = FALSE
        )
      ), ID = 1)
      thePolys[[Dtype]][[1]] <- sp::Polygons(list(
        sp::Polygon(theCoords[[1]][, c(1, 3 + Dtype)],
          hole = FALSE
        )
      ), ID = 1)
      for (Dpoly in 2:length(theCoords)) {
        thePolysScaled[[Dtype]][[Dpoly]] <- sp::Polygons(list(
          sp::Polygon(theCoords[[Dpoly]][, c(1, 1 + Dtype)], hole = FALSE),
          sp::Polygon(theCoords[[Dpoly - 1]][, c(1, 1 + Dtype)], hole = TRUE)
        ), ID = Dpoly)
        thePolys[[Dtype]][[Dpoly]] <- sp::Polygons(list(
          sp::Polygon(theCoords[[Dpoly]][, c(1, 3 + Dtype)], hole = FALSE),
          sp::Polygon(theCoords[[Dpoly - 1]][, c(1, 3 + Dtype)], hole = TRUE)
        ), ID = Dpoly)
      }
      forecastPoly[[Dtype]][[D]] <- sp::SpatialPolygonsDataFrame(
        sp::SpatialPolygons(thePolys[[Dtype]]),
        data.frame(
          col = colHere,
          quant = apply(intervalMat, 1, paste),
          stringsAsFactors = FALSE
        )
      )
      forecastPolyScaled[[Dtype]][[D]] <- sp::SpatialPolygonsDataFrame(
        sp::SpatialPolygons(thePolysScaled[[Dtype]]),
        data.frame(
          col = colHere,
          quant = apply(intervalMat, 1, paste),
          stringsAsFactors = FALSE
        )
      )
    }
  }
  if (verbose) cat("\n")

  list(
    unscaled = forecastPoly, scaled = forecastPolyScaled,
    intervals = covIntervals,
    intervalsScaled = covIntervalsScaled,
    intervalNames = intervalIntensity
  )
}
