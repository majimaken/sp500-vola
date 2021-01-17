############  Project Financial Econometrics  ###########
# Course:   Financial Econometrics ECO3031-01
# Title:    Comparison of Traditional GARCH Models, Dynamic Conditional 
#           Score Volatility Models and Market Expectation
# Authors:  Ken Geeler (2020318460)
#           Sean Goedgeluk (2020318485)
#
# Project consists of two R files, first execute the Models.R file
# then execute the Analysis.R file (current file).

library(readxl)
library(ggplot2)
library(ggpubr)
library(jtools)
library(MCS)
library(multDM)
library(clipr)

#An adjusted version of DM.test from the multDM library: allows for use of the loss functions as specified in our research paper.
DMTestAdj <- function (f1, f2, y, loss.type = "SE", h = 1, c = FALSE, H1 = "same") 
{
  n <- paste(deparse(substitute(f1)), deparse(substitute(f2)), 
             deparse(substitute(y)), sep = " and ")
  n1 <- deparse(substitute(f1))
  n2 <- deparse(substitute(f2))
  e1 <- f1 - y
  e2 <- f2 - y
  e1_adj <- f1^2 - y^2
  e2_adj <- f2^2 - y^2
  
  #Added the correct loss functions below
  if (loss.type == "SE") {
    g1 <- e1^2
    g2 <- e2^2
  }
  if (loss.type == "SE2") {
    g1 <- e1_adj^2
    g2 <- e2_adj^2
  }
  if (loss.type == "AE") {
    g1 <- abs(e1)
    g2 <- abs(e2)
  }
  if (loss.type == "AE2") {
    g1 <- abs(e1_adj)
    g2 <- abs(e2_adj)
  }
  if (loss.type == "QLIKE") {
    g1 <- LossVol(y,f1,which = 'QLIKE')
    g2 <- LossVol(y,f2,which = 'QLIKE')
  }
  if (loss.type == "RLOG") {
    g1 <- LossVol(y,f1,which = 'R2LOG')
    g2 <- LossVol(y,f2,which = 'R2LOG')
  }
  d <- g1 - g2
  print(which(is.na(d)==TRUE))
  T <- length(d)
  dbar <- mean(d)
  print(dbar)
  gammahat <- function(k) {
    temp1 <- d - dbar
    temp2 <- rep(NA, abs(k))
    temp2 <- c(temp2, temp1)
    temp2 <- temp2[1:T]
    temp2 <- temp2 - dbar
    temp <- temp1 * temp2
    temp <- temp[(1 + abs(k)):T]
    temp <- sum(temp)/T
    return(temp)
  }
  if (h > 1) {
    gdk <- lapply(seq(from = 1, to = h - 1, by = 1), gammahat)
    gdk <- unlist(gdk)
  }
  else {
    gdk <- 0
  }
  gdk <- gammahat(0) + 2 * sum(gdk)
  DM <- dbar/sqrt(gdk/T)
  if (H1 == "same") {
    pval <- 2 * min(pnorm(DM, lower.tail = FALSE), 1 - pnorm(DM, 
                                                             lower.tail = FALSE))
  }
  if (H1 == "less") {
    pval <- pnorm(DM, lower.tail = FALSE)
  }
  if (H1 == "more") {
    pval <- 1 - pnorm(DM, lower.tail = FALSE)
  }
  if (c) {
    DM <- DM * sqrt((T + 1 - 2 * h + h * (h - 1))/T)
    if (H1 == "same") {
      pval <- 2 * min(pt(q = DM, df = T - 1, lower.tail = FALSE), 
                      1 - pt(q = DM, df = T - 1, lower.tail = FALSE))
    }
    if (H1 == "less") {
      pval <- pt(q = DM, df = T - 1, lower.tail = FALSE)
    }
    if (H1 == "more") {
      pval <- 1 - pt(q = DM, df = T - 1, lower.tail = FALSE)
    }
  }
  names(DM) <- "statistic"
  names(h) <- "forecast horizon"
  if (H1 == "same") {
    alt <- "Forecast f1 and f2 have different accuracy."
  }
  if (H1 == "less") {
    alt <- "Forecast f1 is less accurate than f2."
  }
  if (H1 == "more") {
    alt <- "Forecast f1 is more accurate than f2."
  }
  ret <- list(DM, h, paste(alt), pval, "Diebold-Mariano test", 
              n)
  names(ret) <- c("statistic", "parameter", "alternative", 
                  "p.value", "method", "data.name")
  class(ret) <- "htest"
  return(ret)
}

#Import the predictions
export <- read_excel("predictions.xlsx")
export <- cbind(export, vix_5=export$vix*sqrt(5/250), vix_21=export$vix*sqrt(21/250))

#Was used to plot the predicted volatilities
ggplot(data = export, aes(x=Date, y=value)) +
  ylab('') + xlab('') +
  geom_line(aes(y=rv2_5, col='RV'), size=.4, alpha=1, colour="black")   +
  geom_line(aes(y=GARCH5, col='GARCH'), size=.4, alpha=1, colour="red")   +
  geom_line(aes(y=GJR.GARCH5, col='GJR-GARCH'),  size=.4, alpha=1, colour="blue") +
  geom_line(aes(y=Beta.t.EGARCH5, col='Beta-t-EGARCH'),  size=.4, alpha=1, colour="purple") +
  geom_line(aes(y=Two.Beta.t.EGARCH5, col='2-Beta-t-EGARCH'),  size=.4, alpha=1, colour="green") +
  theme_apa()

#MZ Regressions Target Variable 1, h = 5
{
MZ_GARCH_5_Tgt1   <- lm(rv1_5 ~ GARCH5,export,)
MZ_GJR_5_Tgt1     <- lm(rv1_5 ~ GJR.GARCH5,export)
MZ_Beta_5_Tgt1    <- lm(rv1_5 ~ Beta.t.EGARCH5,export)
MZ_Beta2_5_Tgt1   <- lm(rv1_5 ~ Two.Beta.t.EGARCH5,export)
MZ_VIX_5_Tgt1     <- lm(rv1_5 ~ vix_5,export)

#MZ Regressions Target Variable 2, h = 5
MZ_GARCH_5_Tgt2   <- lm(rv2_5 ~ GARCH5,export)
MZ_GJR_5_Tgt2     <- lm(rv2_5 ~ GJR.GARCH5,export)
MZ_Beta_5_Tgt2    <- lm(rv2_5 ~ Beta.t.EGARCH5,export)
MZ_Beta2_5_Tgt2   <- lm(rv2_5 ~ Two.Beta.t.EGARCH5,export)
MZ_VIX_5_Tgt2     <- lm(rv2_5 ~ vix_5,export)

#MZ Regressions Target Variable 1, h = 21
MZ_GARCH_21_Tgt1  <- lm(rv1_21 ~ GARCH5,export)
MZ_GJR_21_Tgt1    <- lm(rv1_21 ~ GJR.GARCH5,export)
MZ_Beta_21_Tgt1   <- lm(rv1_21 ~ Beta.t.EGARCH5,export)
MZ_Beta2_21_Tgt1  <- lm(rv1_21 ~ Two.Beta.t.EGARCH5,export)
MZ_VIX_21_Tgt1    <- lm(rv1_21 ~ vix_5,export)

#MZ Regressions Target Variable 2, h = 21
MZ_GARCH_21_Tgt2  <- lm(rv2_21 ~ GARCH5,export)
MZ_GJR_21_Tgt2    <- lm(rv2_21 ~ GJR.GARCH5,export)
MZ_Beta_21_Tgt2   <- lm(rv2_21 ~ Beta.t.EGARCH5,export)
MZ_Beta2_21_Tgt2  <- lm(rv2_21 ~ Two.Beta.t.EGARCH5,export)
MZ_VIX_21_Tgt2    <- lm(rv2_21 ~ vix_5,export)
}

#Print information for an MZ regression to go into LaTeX table
getMzDetails <- function(currmodel) {
  currmodel <- summary(currmodel)
  
  #Get coefficients and errors
  c1 <- signif(currmodel$coefficients[1],6)
  e1 <- signif(currmodel$coefficients[2],6)
  c2 <- signif(currmodel$coefficients[2,1],6)
  e2 <- signif(currmodel$coefficients[2,2],6)
  rmse <- signif(sqrt(mean(currmodel$residuals^2)),6)
  r2 <- signif(currmodel$r.squared,6)
  
  #Determine significance
  if(currmodel$coefficients[1,4] < 0.01) {
    sign1 <- "$^{***}$"
  } else if (currmodel$coefficients[1,4] < 0.05) {
    sign1 <- "$^{**}$"
  } else if (currmodel$coefficients[1,4] < 0.1) {
    sign1 <- "$^{*}$"
  } else {
    sign1 <- ""
  }
  
  if(abs((currmodel$coefficients[2,1]-1)/currmodel$coefficients[2,2]) > 2.576) {
    sign2 <- "$^{***}$"
  } else if (abs((currmodel$coefficients[2,1]-1)/currmodel$coefficients[2,2]) > 1.960) {
    sign2 <- "$^{**}$"
  } else if (abs((currmodel$coefficients[2,1]-1)/currmodel$coefficients[2,2]) > 1.645) {
    sign2 <- "$^{*}$"
  } else {
    sign2 <- ""
  }
  
  #Print output for LaTeX
  out <- paste(c1, sign1, " & (", e1, ") & ", c2, sign2, " & (", e2, ") & ", rmse, " & ", r2,sep="")
  print(out)
  write_clip(out)
}

#Print all the information
getMzDetails(MZ_GARCH_5_Tgt1)
getMzDetails(MZ_GJR_5_Tgt1)
getMzDetails(MZ_Beta_5_Tgt1)
getMzDetails(MZ_Beta2_5_Tgt1)
getMzDetails(MZ_VIX_5_Tgt1)

getMzDetails(MZ_GARCH_5_Tgt2)
getMzDetails(MZ_GJR_5_Tgt2)
getMzDetails(MZ_Beta_5_Tgt2)
getMzDetails(MZ_Beta2_5_Tgt2)
getMzDetails(MZ_VIX_5_Tgt2)

getMzDetails(MZ_GARCH_21_Tgt1)
getMzDetails(MZ_GJR_21_Tgt1)
getMzDetails(MZ_Beta_21_Tgt1)
getMzDetails(MZ_Beta2_21_Tgt1)
getMzDetails(MZ_VIX_21_Tgt1)

getMzDetails(MZ_GARCH_21_Tgt2)
getMzDetails(MZ_GJR_21_Tgt2)
getMzDetails(MZ_Beta_21_Tgt2)
getMzDetails(MZ_Beta2_21_Tgt2)
getMzDetails(MZ_VIX_21_Tgt2)

##Loss Functions - https://www.rdocumentation.org/packages/MCS/versions/0.1.3/topics/LossVol
#For the specific functions, check https://faculty.washington.edu/ezivot/econ589/DoesAnythingBeatGarch11.pdf

#Determines the coefficient significance using the bonferroni correction
getAsterisks <- function(pval) {
  if(is.nan(pval)) {
    sign1 <- ""
  } else if(pval < 0.00166667) {
    sign1 <- "$^{***}$"
  } else if (pval < 0.00833333) {
    sign1 <- "$^{**}$"
  } else if (pval < 0.01666667) {
    sign1 <- "$^{*}$"
  } else {
    sign1 <- ""
  }
  return(sign1)
}

#Print information for the loss functions and DM-tests to go into LaTeX table
getLossDetails <- function(currmodel, rv_current, model_current, model_compare, horizon){
  #MSE1
  mse1 <- signif((1/length(na.omit(model_current[-1:-500])))*sum(LossVol(rv_current[-1:-500],model_current[-1:-500],which = 'SE1'),na.rm=T),6)
  
  #MSE2
  mse2 <- signif((1/length(na.omit(model_current[-1:-500])))*sum(LossVol(rv_current[-1:-500],model_current[-1:-500],which = 'SE2'),na.rm=T),6)
  
  #QLIKE
  qlike <- signif((1/length(na.omit(model_current[-1:-500])))*sum(LossVol(rv_current[-1:-500],model_current[-1:-500],which = 'QLIKE'),na.rm=T),6)
  
  #R2LOG
  r2log <- signif((1/length(na.omit(model_current[-1:-500])))*sum(LossVol(rv_current[-1:-500],model_current[-1:-500],which = 'R2LOG'),na.rm=T),6)
  
  #MAE1
  mae1 <- signif((1/length(na.omit(model_current[-1:-500])))*sum(LossVol(rv_current[-1:-500],model_current[-1:-500],which = 'AE1'),na.rm=T),6)
  
  #MAE2
  mae2 <- signif((1/length(na.omit(model_current[-1:-500])))*sum(LossVol(rv_current[-1:-500],model_current[-1:-500],which = 'AE2'),na.rm=T),6)
  
  test1 <- getAsterisks(DMTestAdj(model_current[-1:-500],model_compare[-1:-500],rv_current[-1:-500],loss="SE",horizon,c=FALSE,H1="more")$p.value)
  test2 <- getAsterisks(DMTestAdj(model_current[-1:-500],model_compare[-1:-500],rv_current[-1:-500],loss="SE2",horizon,c=FALSE,H1="more")$p.value)
  test3 <- getAsterisks(DMTestAdj(model_current[-1:-500],model_compare[-1:-500],rv_current[-1:-500],loss="QLIKE",horizon,c=FALSE,H1="more")$p.value)
  test4 <- getAsterisks(DMTestAdj(model_current[-1:-500],model_compare[-1:-500],rv_current[-1:-500],loss="RLOG",horizon,c=FALSE,H1="more")$p.value)
  test5 <- getAsterisks(DMTestAdj(model_current[-1:-500],model_compare[-1:-500],rv_current[-1:-500],loss="AE",horizon,c=FALSE,H1="more")$p.value)
  test6 <- getAsterisks(DMTestAdj(model_current[-1:-500],model_compare[-1:-500],rv_current[-1:-500],loss="AE2",horizon,c=FALSE,H1="more")$p.value)
  
  out <- paste(mse1, test1, " & ", mse2, test2," & ", qlike, test3, " & ", r2log, test4, " & ", mae1, test5, " & ", mae2, test6,sep="")
  print(out)
  write_clip(out)
}

#Get all the details of the loss functions and DM-tests
getLossDetails(MZ_GARCH_5_Tgt1, export$rv1_5, export$GARCH5, export$vix_5, 5)
getLossDetails(MZ_GJR_5_Tgt1, export$rv1_5, export$GJR.GARCH5, export$vix_5, 5)
getLossDetails(MZ_Beta_5_Tgt1, export$rv1_5, export$Beta.t.EGARCH5, export$vix_5, 5)
getLossDetails(MZ_Beta2_5_Tgt1, export$rv1_5, export$Two.Beta.t.EGARCH5, export$vix_5, 5)
getLossDetails(MZ_VIX_5_Tgt1, export$rv1_5, export$vix_5, export$vix_5, 5)

getLossDetails(MZ_GARCH_5_Tgt2, export$rv2_5, export$GARCH5, export$vix_5, 5)
getLossDetails(MZ_GJR_5_Tgt2, export$rv2_5, export$GJR.GARCH5, export$vix_5, 5)
getLossDetails(MZ_Beta_5_Tgt2, export$rv2_5, export$Beta.t.EGARCH5, export$vix_5, 5)
getLossDetails(MZ_Beta2_5_Tgt2, export$rv2_5, export$Two.Beta.t.EGARCH5, export$vix_5, 5)
getLossDetails(MZ_VIX_5_Tgt2, export$rv2_5, export$vix_5, export$vix_5, 5)

getLossDetails(MZ_GARCH_21_Tgt1, export$rv1_21, export$GARCH21, export$vix_21, 21)
getLossDetails(MZ_GJR_21_Tgt1, export$rv1_21, export$GJR.GARCH21, export$vix_21, 21)
getLossDetails(MZ_Beta_21_Tgt1, export$rv1_21, export$Beta.t.EGARCH21, export$vix_21, 21)
getLossDetails(MZ_Beta2_21_Tgt1, export$rv1_21, export$TWo.Beta.t.EGARCH21, export$vix_21, 21)
getLossDetails(MZ_VIX_21_Tgt1, export$rv1_21, export$vix_21, export$vix_21, 21)

getLossDetails(MZ_GARCH_21_Tgt2, export$rv2_21, export$GARCH21, export$vix_21, 21)
getLossDetails(MZ_GJR_21_Tgt2, export$rv2_21, export$GJR.GARCH21, export$vix_21, 21)
getLossDetails(MZ_Beta_21_Tgt2, export$rv2_21, export$Beta.t.EGARCH21, export$vix_21, 21)
getLossDetails(MZ_Beta2_21_Tgt2, export$rv2_21, export$TWo.Beta.t.EGARCH21, export$vix_21, 21)
getLossDetails(MZ_VIX_21_Tgt2, export$rv2_21, export$vix_21, export$vix_21, 21)

#Run the MCS tests
#Replace the inputs within the LossVol to change the MCS that is run.
#The alpha can be adjusted to run the test with a different alpha value. 
MCS_21_rv1_SE1 <- MCSprocedure(Loss=as.matrix(cbind(LossVol(export$GARCH21,export$rv1_21,which = 'SE1'),
                                             LossVol(export$GJR.GARCH21,export$rv1_21,which = 'SE1'),
                                             LossVol(export$Beta.t.EGARCH21,export$rv1_21,which = 'SE1'),
                                             LossVol(export$TWo.Beta.t.EGARCH21,export$rv1_21,which = 'SE1'))),alpha = 0.1,statistic = "TR")
MCS_21_rv1_SE2 <- MCSprocedure(Loss=as.matrix(cbind(LossVol(export$GARCH21,export$rv1_21,which = 'SE2'),
                                                   LossVol(export$GJR.GARCH21,export$rv1_21,which = 'SE2'),
                                                   LossVol(export$Beta.t.EGARCH21,export$rv1_21,which = 'SE2'),
                                                   LossVol(export$TWo.Beta.t.EGARCH21,export$rv1_21,which = 'SE2'))),alpha = 0.1,statistic = "TR")
MCS_21_rv1_QLIKE <- MCSprocedure(Loss=as.matrix(cbind(LossVol(export$GARCH21,export$rv1_21,which = 'QLIKE'),
                                                     LossVol(export$GJR.GARCH21,export$rv1_21,which = 'QLIKE'),
                                                     LossVol(export$Beta.t.EGARCH21,export$rv1_21,which = 'QLIKE'),
                                                     LossVol(export$TWo.Beta.t.EGARCH21,export$rv1_21,which = 'QLIKE'))),alpha = 0.01,statistic = "TR")
MCS_21_rv1_RLOG <- MCSprocedure(Loss=as.matrix(cbind(LossVol(export$GARCH21,export$rv1_21,which = 'R2LOG'),
                                                    LossVol(export$GJR.GARCH21,export$rv1_21,which = 'R2LOG'),
                                                    LossVol(export$Beta.t.EGARCH21,export$rv1_21,which = 'R2LOG'),
                                                    LossVol(export$TWo.Beta.t.EGARCH21,export$rv1_21,which = 'R2LOG'))),alpha = 0.1,statistic = "TR")
MCS_21_rv1_AE1 <- MCSprocedure(Loss=as.matrix(cbind(LossVol(export$GARCH21,export$rv1_21,which = 'AE1'),
                                                   LossVol(export$GJR.GARCH21,export$rv1_21,which = 'AE1'),
                                                   LossVol(export$Beta.t.EGARCH21,export$rv1_21,which = 'AE1'),
                                                   LossVol(export$TWo.Beta.t.EGARCH21,export$rv1_21,which = 'AE1'))),alpha = 0.1,statistic = "TR")
MCS_21_rv1_AE2 <- MCSprocedure(Loss=as.matrix(cbind(LossVol(export$GARCH21,export$rv1_21,which = 'AE2'),
                                                   LossVol(export$GJR.GARCH21,export$rv1_21,which = 'AE2'),
                                                   LossVol(export$Beta.t.EGARCH21,export$rv1_21,which = 'AE2'),
                                                   LossVol(export$TWo.Beta.t.EGARCH21,export$rv1_21,which = 'AE2'))),alpha = 0.1,statistic = "TR")

