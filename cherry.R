# Loading in packages
library(tidyverse)
library(forecast)
library(lubridate)
library(astsa)
library(FitAR)
library(here)

# Establishing workspace location
i_am("cherry.R")

# Loading datasets
dc <- read_csv(here("peak-bloom-prediction/data/washingtondc.csv"))
dc_cur <- dc %>%
  filter(year >= 1980) %>% # we filter due to data being more current and available from Daymet
  rename(Date = bloom_date)

dc_tmin <- read_csv(here("peak-bloom-prediction/data/statistics_tmin.csv"))
dc_tmax <- read_csv(here("peak-bloom-prediction/data/statistics_tmax.csv"))

# Data Refining
dc_temp <- left_join(dc_tmin,dc_tmax, by = 'dt')
dc_temp <- dc_temp %>% 
  select(dt,value_mean.x,value_mean.y) %>%
  rename(Date=dt,Temp_Min=value_mean.x,Temp_Max=value_mean.y)

# Average Daily Temps and Date Info
dc_temp<-dc_temp %>%
  mutate(Temp_Avg = 0.5*(Temp_Min+Temp_Max)) %>%
  mutate(Year = Year(Date)) %>%
  mutate(Month = Month(Date)) 

# Create Function to convert F to C
cel <- function(x) {
  (x-32)*5/9
}

# Calculate GDD (Growing Degree-Days)
thresh <- cel(45)
dc_GDD <- dc_temp %>%
  mutate(Temp_DD = ifelse(Temp_Avg-thresh>0,Temp_Avg-thresh,0)) %>%
  group_by(Year) %>%
  mutate(GDD = cumsum(Temp_DD))

# Calculating Cold Degrees
thresh_chill<-cel(40)
dc_GDD2 <- dc_GDD %>%
  mutate(NewCalendar = Date %m-% period("4 month"),NewYear=Year(NewCalendar)) %>%
  mutate(Temp_CD = ifelse(thresh_chill-Temp_Avg>0,thresh_chill-Temp_Avg,0)) %>%
  group_by(NewYear) %>%
  mutate(CDD = cumsum(Temp_CD))

# Merging GDD with Bloom Dates
dc_data<-left_join(dc_cur,dc_GDD2,by = 'Date') %>% select(-c(lat,long,alt,Year,Month,NewYear,NewCalendar))

# Time series analysis
avgts<-ts(dc_temp$Temp_Avg,frequency = 365,start=c(1980,1),end=c(2020,365))

ts.plot(avgts)
plot(decompose(avgts))

auto.arima(avgts)


