# Loading in packages
library(tidyverse)
library(forecast)
library(lubridate)
library(astsa)
library(FitAR)
library(here)
library(rnoaa)

# Establishing workspace location
i_am("kyoto.R")

# Loading datasets
jp <- read_csv(here("peak-bloom-prediction/data/kyoto.csv")) %>% filter(year >= 1950)

lm_fit <- lm(data=jp, bloom_doy ~ year)

locs <- c("USC00186350","GME00127786","JA000047759","CA001108395")
city <- c("DC","Liestal","Kyoto","Vancouver")

# Original code from demo analysis (https://competition.statistics.gmu.edu/wp-content/uploads/2022/02/demo_analysis.html)
get_temperature <- function (stationid) {
  tbls <- ghcnd_search(stationid = stationid, var = c("tmax"), 
                       date_min = "1950-01-01", date_max = "2022-01-31")
  tmax <- tbls[[1]] %>% select(id,tmax,date)
  
  return(tmax)
  # tmin <- tbls[[2]] %>% select(tmin,date)
  # temp <- inner_join(tmax,tmin,by="date") %>% summarize(id,date,tavg=0.5*(tmax+tmin)/10) %>% # convert unit from 0.1 Celsius to 1 Celsius
  #   mutate(year = as.integer(format(date, "%Y")))
  # return(temp)
}

temp_list <- lapply(locs,get_temperature)
temps <- tibble()

for(i in 1:length(temp_list)){
  temps <- bind_rows(temps,temp_list[[i]])
}

jp_temp <- temps %>% filter(id=="JA000047759") %>% mutate(tmax=tmax/10,year=year(date)) %>%
  group_by(year) %>%
  mutate(jan_feb=mean(head(tmax,60),na.rm=TRUE),mar=mean(tmax[61:75],na.rm=TRUE))

temp_on_bloom <- jp %>% left_join(jp_temp,by=c("bloom_date"="date"))

lm1 <- lm(data=temp_on_bloom,bloom_doy ~ year.x)
lm2 <- lm(data=temp_on_bloom,bloom_doy ~ year.x+jan_feb)
lm3 <- lm(data=temp_on_bloom,bloom_doy ~ year.x+mar)

summary(lm1)
summary(lm2)
summary(lm3)

temp_on_bloom %>% ggplot(mapping=aes(x=year.x,y=bloom_doy)) +
  geom_point()

