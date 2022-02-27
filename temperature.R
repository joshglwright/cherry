# Loading in packages
library(tidyverse)
library(forecast)
library(lubridate)
library(astsa)
library(here)
library(rnoaa)

i_am('temperature.R')

locs <- c("USC00186350","GME00127786","JA000047759","CA001108395")
city <- c("DC","Liestal","Kyoto","Vancouver")

# Original code from demo analysis (https://competition.statistics.gmu.edu/wp-content/uploads/2022/02/demo_analysis.html)
get_temperature <- function (stationid) {
  tbls <- ghcnd_search(stationid = stationid, var = c("tmax","tmin"), 
               date_min = "1980-01-01", date_max = "2022-01-31")
  tmax <- tbls[[1]] %>% select(id,tmax,date)
  tmin <- tbls[[2]] %>% select(tmin,date)
  temp <- inner_join(tmax,tmin,by="date") %>% summarize(id,date,tavg=0.5*(tmax+tmin)/10) %>% # convert unit from 0.1 Celsius to 1 Celsius
    mutate(year = as.integer(format(date, "%Y")))
  return(temp)
}

temp_list <- lapply(locs,get_temperature)
temps <- tibble()

for(i in 1:length(temp_list)){
  temps <- bind_rows(temps,temp_list[[i]])
}



# 
# tmin_dc <- read_csv(here('peak-bloom-prediction','data','statistics_tmin.csv'))
# tmax_dc <- read_csv(here('peak-bloom-prediction','data','statistics_tmax.csv'))
# 
# temp_dc <- inner_join(tmin_dc,tmax_dc, by = 'dt',suffix=c('min','max')) %>%
#   mutate(tavg=(value_meanmin+value_meanmax)/2) %>%
#   select(dt,tavg)
# 
# # Time series analysis
# avgts_dc<-ts(temp_dc$tavg,frequency = 365,start=c(1980,1),end=c(2020,365))
# 
# ts.plot(avgts_dc)
# 
# plot(decompose(avgts_dc))
# 
# par(mfrow=c(2,1))
# acf(avgts_dc, lag = 365)
# pacf(avgts_dc, lag = 365)
# 
# acf(avgts_dc)
# pacf(avgts_dc)
# 
# par(mfrow=c(1,1))
# 
# auto.arima(avgts_dc, ic = 'aicc', max.P = 3, max.Q = 3, approximation = FALSE, trace = TRUE)
# 
# # Result of 
# # Series: avgts 
# # ARIMA(3,0,2)(0,1,0)[365] 
# # 
# # Coefficients:
# #   ar1      ar2     ar3      ma1      ma2
# # 1.4331  -0.5772  0.0793  -0.4541  -0.2768
# # s.e.  0.0434   0.0421  0.0222   0.0431   0.0340
# # 
# # sigma^2 = 12.79:  log likelihood = -39321.31
# # AIC=78654.62   AICc=78654.62   BIC=78700.15
# 
# 
# ts_fit <- sarima(avgts_dc, 3,0,2,0,1,0,365)
# 
# Temp_Pred <- sarima.for(avgts_dc,4015, 3,0,2,0,1,0,365)

ts_fit <- sarima(avgts_dc, 3,0,2,0,1,0,365)

Temp_Pred <- sarima.for(avgts_dc,4015, 3,0,2,0,1,0,365)

# Getting GDD by Year starting on the 50th day of the year
Year<-c()
for (i in 2021:2031) {
  Year<-append(Year,rep(i,365))
}
Temp_Pred2<- as.data.frame(cbind(Year,rep(1:365,11),Temp_Pred$pred))
colnames(Temp_Pred2)<-c('Year','DOY','Temp')
Temp_Pred3 <- Temp_Pred2 %>%
  mutate(Date = as.Date(Num, origin)) %>%
  mutate(Temp_DD = ifelse(Temp-(40-32)*5/9>0,Temp-(40-32)*5/9,0)) %>%
  mutate(NewCalendar = Date %m-% period("49 day"), NewYear=year(NewCalendar), NewDOY=yday(NewCalendar)) %>%
  group_by(NeyYear) %>%
  mutate(GDD = cumsum(Temp_DD),doy = yday(Date))

