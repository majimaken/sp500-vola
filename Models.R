############  Project Financial Econometrics  ###########
# Course:   Financial Econometrics ECO3031-01
# Title:    Comparison of Traditional GARCH Models, Dynamic Conditional 
#           Score Volatility Models and Market Expectation
# Authors:  Ken Geeler (2020318460)
#           Sean Goedgeluk (2020318485)
#
# Project consists of two R files, first execute the Models.R (current file) file
# then execute the Analysis.R file.

library(lubridate)
library(tseries)
library(rugarch)
library(betategarch)
library(stargazer)
library(readxl)
library(fGarch)
library(xlsx)

# Transform the data
data <- read_excel("FE_data.xlsx")
View(data)
colnames(data) <- c("date", "return", "rv5_ss", "rv5_ss_scale", "vix", "rv1_5", "rv2_5", "rv1_21","rv2_21")

#Set window size 
n<-nrow(data[data$date>="2018-01-01",])   #Predict from 1 Jan. 208 onwards
window <- 120*21                          #21*120=2520, approx. 120 trading months, or 10 years

#Create predictions matrix
preds <- matrix(nrow = n-21, ncol = 9)

#Generate predictions based on rolling window
j <- 1
for (i in (nrow(data)-n+1):(nrow(data)-21)) {
  print(j)
  
  #Estimate models
  def_garch <- garchFit(~garch(1, 1), data = data$return[(i-window):i],trace = FALSE); 
  gjr_garch.spec <- ugarchspec(variance.model = list(model="gjrGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder=c(0,0)));
  gjr_garch.fit <- ugarchfit(data = data$return[(i-window):i], spec = gjr_garch.spec,trace = FALSE);
  beta_t_egarch <- tegarch(data$return[(i-window):i],components = 1,skew=FALSE);
  two_beta_t_egarch <- tegarch(data$return[(i-window):i],components = 2,skew=FALSE);
  
  #Save predictions
  preds[j,2] <- sqrt(sum(predict(def_garch,n.ahead = 5)$meanError^2));
  preds[j,3] <- sqrt(sum(predict(def_garch,n.ahead = 21)$meanError^2));
  preds[j,4] <- sqrt(sum(sigma(ugarchforecast(gjr_garch.fit,n.ahead = 5))^2));
  preds[j,5] <- sqrt(sum(sigma(ugarchforecast(gjr_garch.fit,n.ahead = 21))^2));
  preds[j,6] <- sqrt(sum(predict(beta_t_egarch,n.ahead = 5)^2));
  preds[j,7] <- sqrt(sum(predict(beta_t_egarch,n.ahead = 21)^2));
  preds[j,8] <- sqrt(sum(predict(two_beta_t_egarch,n.ahead = 5)^2));
  preds[j,9] <- sqrt(sum(predict(two_beta_t_egarch,n.ahead = 21)^2));
  j <- j+1
}

colnames(preds) <- c("Date", "GARCH5", "GARCH21", "GJR.GARCH5", "GJR.GARCH21", "Beta.t.EGARCH5", "Beta.t.EGARCH21", "Two.Beta.t.EGARCH5", "Two.Beta.t.EGARCH21")
preds$Date <- data$date[(nrow(data)-n+1):(nrow(data)-21)]
preds <- preds[,1:9]
preds <- cbind(preds, data[(nrow(data)-n+1):(nrow(data)-21),5:9])

write.xlsx(preds, "predictions.xlsx", sheetName = "Predictions", 
           col.names = TRUE, row.names = FALSE, append = FALSE)