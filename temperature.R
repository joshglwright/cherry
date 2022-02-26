# Loading in packages
library(tidyverse)
library(forecast)
library(lubridate)
library(astsa)
library(here)

i_am('temperature.R')


# Time series analysis
avgts<-ts(dc_temp$Temp_Avg,frequency = 365,start=c(1980,1),end=c(2020,365))

ts.plot(avgts)
plot(decompose(avgts))

par(mfrow=c(2,1))
acf(avgts, lag = 365)
pacf(avgts, lag = 365)

par(mfrow=c(2,1))
acf(avgts)
pacf(avgts)

auto.arima(avgts, ic = 'aic', max.P = 3, max.Q = 3)
# Result of 
# Series: avgts 
# ARIMA(3,0,2)(0,1,0)[365] 
# 
# Coefficients:
#   ar1      ar2     ar3      ma1      ma2
# 1.4331  -0.5772  0.0793  -0.4541  -0.2768
# s.e.  0.0434   0.0421  0.0222   0.0431   0.0340
# 
# sigma^2 = 12.79:  log likelihood = -39321.31
# AIC=78654.62   AICc=78654.62   BIC=78700.15
ts_fit<_sarima(avgts, 3,0,2,0,1,0,365)

Temp_Pred<-sarima.for(avgts,4015, 3,0,2,0,1,0,365)