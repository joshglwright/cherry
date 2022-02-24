# Loading in packages
library(tidyverse)
library(forecast)
library(DescTools)
library(randtests)
library(astsa)
library(FitAR)
library(here)
# Establishing workspace location
i_am("peak-bloom-prediction/analysis_dc.R")
# Loading datasets
dc <- read_csv(here("peak-bloom-prediction/data/washingtondc.csv"))
dc_cur <- dc %>%
  filter(year >= 1980) %>% # we filter due to data being more current and available from Daymet
  rename(Date = bloom_date)

dc_tmin<-read_csv(here("peak-bloom-prediction/data/statistics_tmin.csv"))
dc_tmax<-read_csv(here("peak-bloom-prediction/data/statistics_tmax.csv"))
# Data Refining
dc_temp<-left_join(dc_tmin,dc_tmax, by = 'dt')
dc_temp<-dc_temp[,c('dt','value_mean.x','value_mean.y')]
colnames(dc_temp)<-c('Date', 'Temp_Min', 'Temp_Max')
# Average Daily Temps and Date Info
dc_temp<-dc_temp %>%
  mutate(Temp_Avg = 0.5*(Temp_Min+Temp_Max)) %>%
  mutate(Year = substr(Date,1,4)) %>%
  mutate(Month = substr(Date,6,7)) 

# Calculating GDD at 50F()
cel <- function(x) {
  (x-32)*5/9
}
thresh<-cel(45)
dc_temp<-dc_temp %>%
  mutate(Temp_DD = ifelse (Temp_Avg-thresh>0,Temp_Avg-thresh,0)) %>%
  group_by(Year) %>%
  mutate(GDD = cumsum(Temp_DD))

# Calculating Cold Degrees

thresh_chill<-cel(32)
dc_temp<-dc_temp %>%
  mutate(Season = ifelse(Month == '01'|Month == '02'|Month == '03'|Month == '04'|Month == '12','Cold Season', 'Warm Season')) %>%
  mutate(Chill_Group = paste(Season, Year, sep = " ")) %>%
  mutate(Temp_CD = ifelse (thresh_chill-Temp_Avg>0,thresh_chill-Temp_Avg,0)) %>%
  group_by(Chill_Group) %>%
  mutate(CDD = cumsum(Temp_CD))

# Merging GDD with Bloom Dates

dc_data<-left_join(dc_cur,dc_temp, by = 'Date')

# Time series analysis

avgts<-ts(dc_temp$Temp_Avg ,frequency = 365, start=c(1980,1), end=c(2020,365))
ts.plot(avgts)
plot(decompose(avgts))

auto.arima(avgts)


