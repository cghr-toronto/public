

# get province name     
library(sp)
library(dplyr)
library(mapmisc)
load(file="CD_fit_sep.RData")
cdShp <- readRDS("lcd_000a16a_e.rds")
cdShp <- data.frame(cdShp)
cdShp$CDUID <- as.numeric(as.character(cdShp$CDUID))
CD_fit_sep_name <- data.frame(CDUID=CD_fit_sep,name=1)
cdShp <- cdShp %>% dplyr::left_join(CD_fit_sep_name, by=c("CDUID"))
cdShp <- cdShp[which(cdShp$name==1),]


obtain_nice_data <- function(input_model, pollutant, xlim=c(0.9,1.1),nationalLine)
{
  
  est <- exp(mapply(function(x){x$coefficients[1:3]}, input_model)*c(7.6, 12.8, 14))
  
  se <- mapply(function(x){diag(x$var)[1:3]}, input_model)
  
  lower <- exp(mapply(function(x){x$coefficients[1:3]- 1.96*(sqrt(diag(x$var)[1:3]))
  }, input_model)*c(7.6, 12.8, 14))
  
  upper <- exp(mapply(function(x){x$coefficients[1:3]+ 1.96*(sqrt(diag(x$var)[1:3]))
  }, input_model)*c(7.6, 12.8, 14))
  
  rownames(se) <- rownames(est)
  x = data.frame(CD=c(CD_fit_sep), est=est[pollutant,], se_raw=se[pollutant,], lower=lower[pollutant,], upper=upper[pollutant,])
  
  x <-  x[order(x$est), ]
  x$index = rank(x$est)
  x$col= c("#0000FF","#FF0000")[1+x$CD %in% cdShp$CDUID[cdShp$PRNAME=="Ontario"]]
  x$colTrans = paste0(x$col, '40')
  x$colLine = paste0(x$col, '60')
  
  x$cex = -log(x$se) 
  x$cex = x$cex - min(x$cex)
  x$cex = x$cex / max(x$cex)
  
  x$textpos = rep_len(c(4,2), nrow(x))
  
  x$textloc = x$est
  par(mar=c(2,0,2,0), bty='n')
  plot(x$est, x$index, yaxt='n', xlim = xlim,
       #xlim = range(x[,c('lower','upper')]),
       xlab="", ylab='', pch=20, col=col2html(x$colTrans,x$cex) , cex=1)
  # plot(x$est, x$index, yaxt='n', xlim = xlim,
  #      #xlim = range(x[,c('lower','upper')]),
  #      xlab="", ylab='', pch=15, col=x$colTrans , cex=x$cex)
  abline(v=1, col='grey')
  abline(v=nationalLine, col='black')
  segments(x$lower, x$index, x$upper, x$index, pch=15, col=col2html(x$colLine,x$cex))
  x$cityName=""
x$cityName[x$CD==3506] <- "Ottawa"
x$cityName[x$CD==3518] <- "Durham"
x$cityName[x$CD==3519] <- "York"
x$cityName[x$CD==3520] <- "Toronto"
x$cityName[x$CD==3521] <- "Peel"
x$cityName[x$CD==3524] <- "Halton"
x$cityName[x$CD==3525] <- "Hamilton"
x$cityName[x$CD==3530] <- "Waterloo"
x$cityName[x$CD==5915] <- "Vancouver"
  text(
    x$textloc, 
    x$index, x$cityName,
    pos = x$textpos,
    col=x$col,
    cex=0.8, offset=1)
}

load("model_resultmortalityall-causeall-ageall-yearpm25no2o3.RData")
model_result_mort <- model_result

load("model_resultmorbidityall-causeall-ageall-yearpm25no2o3.RData")
model_result_morb <- model_result

load("combine_result_mort_all-causeall-ageall-yearS.RData")
load("combine_result_morb_all-causeall-ageall-yearS.RData")

national_mort <- exp(apply(combine_result_mort$betaBar,2,median)*c(7.6, 12.8, 14))
national_morb <- exp(apply(combine_result_morb$betaBar,2,median)*c(7.6, 12.8, 14))

obtain_nice_data(model_result_mort,"pm25",nationalLine=national_mort[1])
# obtain_nice_data(model_result_morb,"pm25",nationalLine=national_morb[1])
# obtain_nice_data(model_result_mort,"no2",nationalLine=national_mort[2])
# obtain_nice_data(model_result_morb,"no2",nationalLine=national_morb[2])
# obtain_nice_data(model_result_mort,"o3",nationalLine=national_mort[3])
# obtain_nice_data(model_result_morb,"o3",nationalLine=national_morb[3])  
