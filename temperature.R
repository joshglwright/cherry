# Loading in packages
library(tidyverse)
library(forecast)
library(lubridate)
library(astsa)
library(here)

i_am('temperature.R')

tmin_dc <- read_csv(here('peak-bloom-prediction','data','statistics_tmin.csv'))
tmax_dc <- read_csv(here('peak-bloom-prediction','data','statistics_tmax.csv'))

temp_dc <- inner_join(tmin_dc,tmax_dc, by = 'dt',suffix=c('min','max')) %>%
  mutate(tavg=(value_meanmin+value_meanmax)/2) %>%
  select(dt,tavg)

# Time series analysis
avgts_dc<-ts(temp_dc$tavg,frequency = 365,start=c(1980,1),end=c(2020,365))

ts.plot(avgts_dc)

plot(decompose(avgts_dc))

par(mfrow=c(2,1))
acf(avgts_dc, lag = 365)
pacf(avgts_dc, lag = 365)

acf(avgts_dc)
pacf(avgts_dc)

par(mfrow=c(1,1))

auto.arima(avgts_dc, ic = 'aicc', max.P = 3, max.Q = 3, approximation = FALSE, trace = TRUE)

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


ts_fit <- sarima(avgts_dc, 3,0,2,0,1,0,365)

Temp_Pred <- sarima.for(avgts_dc,4015, 3,0,2,0,1,0,365)