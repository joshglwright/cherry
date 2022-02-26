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
  mutate(year = year(Date)) %>%
  mutate(Month = month(Date)) 

# Create Function to convert F to C
cel <- function(x) {
  (x-32)*5/9
}

# Calculate GDD (Growing Degree-Days)
thresh <- cel(40)
dc_GDD <- dc_temp %>%
  mutate(Temp_DD = ifelse(Temp_Avg-thresh>0,Temp_Avg-thresh,0)) %>%
  group_by(year) %>%
  mutate(GDD = cumsum(Temp_DD),doy = yday(Date))

# Calculating Cold Degrees
thresh_chill<-cel(40)
dc_GDD2 <- dc_GDD %>%
  mutate(NewCalendar = Date %m-% period("4 month"),NewYear=year(NewCalendar)) %>%
  mutate(Temp_CD = ifelse(thresh_chill-Temp_Avg>0,thresh_chill-Temp_Avg,0)) %>%
  group_by(NewYear) %>%
  mutate(CDD = cumsum(Temp_CD))

# Merging GDD with Bloom Dates
dc_data<-left_join(dc_cur,dc_GDD2,by = 'Date') %>% select(-c(lat,long,alt,Year,Month,NewYear,NewCalendar))


# Adding in the Green Bud Phenological Stages
# year <- c(2021:2004)
# green_buds<-c(70,59,64,56,55,68,77,75,70,60,59,73,62,50,64,59,66,64)
# gbuds<-as.data.frame(cbind(year,green_buds))
# gbuds2<-gbuds %>%
#   mutate(gb_doy=paste(year,green_buds,sep = "_"))
# 
# dc_gb<-left_join(gbuds2,dc_GDD,by = 'gb_doy') 

# Simulation for Best GDD Threshold

# Calculate GDD (Growing Degree-Days)
thresh <- cel(seq(0,50,by = 0.05))
thresh_vars <- vector(length=length(thresh))

for (i in 1:length(thresh)) {
dc_GDD3 <- dc_temp %>%
  mutate(Temp_DD = ifelse(Temp_Avg-thresh[i]>0,Temp_Avg-thresh[i],0)) %>%
  group_by(Year) %>%
  mutate(GDD = cumsum(Temp_DD)) %>%
  mutate(gb_doy = paste(Year,yday(Date),sep = "_"))

dc_gb3<-left_join(gbuds2,dc_GDD3,by = 'gb_doy') 
thresh_vars[i]<-var(dc_gb3$GDD, na.rm = TRUE)
}

feh <- function(x) {
  (9/5)*x+32
}

plot(feh(thresh),thresh_vars,type="l")

# Test previous year's bloom doy as predictor
tmp <- as_tibble(sort(dc_test$bloom_doy)[4:length(dc_test$bloom_doy)])

dc_test <- dc_cur %>%
  select(Date,bloom_doy) %>%
  mutate(lag_bloom = lag(bloom_doy),err_lag=abs(bloom_doy - lag_bloom),err_mean=abs(bloom_doy-mean(tmp$value)))

plot(dc_test$Date,dc_test$dif,type="l")
plot(dc_test$Date,dc_test$bloom_doy)
abline(a=mean(dc_test$bloom_doy),b=0,col="red")
abline(a=median(dc_test$bloom_doy),b=0,col="blue")
hist(dc_test$bloom_doy)

thresh <- cel(40)
dc_nlin <- dc_temp %>% 
  mutate(Temp_DD = ifelse(Temp_Avg-thresh>0,Temp_Avg-thresh,0)) %>%
  group_by(Year) %>%
  mutate(GDD = cumsum(Temp_DD))

# Try Phenological Stage model
# import data
phenostage <- read_csv(here("peak-bloom-prediction/data/Phenological_Stage.csv")) %>% mutate(year=rep(2021:2004)) %>%
  left_join(dc_data %>% select(bloom_doy,year),by="year") %>%
  rename(FullBloom=bloom_doy)

dc_pheno <- dc_data %>% left_join(phenostage,by="year")
dc_pheno_model <- phenostage %>%
  pivot_longer(cols=c(GreenBuds,FloretsVisible,FloretsExtension,PeduncleElongation,PuffyWhite,FullBloom),
               values_to="doy",names_to="Stage") %>%
  left_join(dc_GDD4 %>% select(year,NewGDD,doy),by=c("year","doy")) %>%
  filter(year!=2021) %>%
  mutate(StageNum=ifelse(Stage=="GreenBuds",3,
                         ifelse(Stage=="FloretsVisible",4,
                                ifelse(Stage=="FloretsExtension",5,
                                       ifelse(Stage=="PeduncleElongation",6,
                                              ifelse(Stage=="PuffyWhite",7,8))))))

# plot(dc_pheno_model$GDD,dc_pheno_model$StageNum)
dc_GDD4 <- dc_GDD %>%
  mutate(NewCalendar = Date %m-% period("49 day"), NewYear=year(NewCalendar), NewDOY=yday(NewCalendar),
         NewTemp_DD = ifelse(Temp_Avg-thresh>0,Temp_Avg-thresh,0)) %>%
  group_by(NewYear) %>%
  mutate(NewGDD = cumsum(NewTemp_DD)) %>%
  ungroup()

nlin <- nls(StageNum ~ k/(1+(k-3)/3*exp(-r*NewGDD)),data=dc_pheno_model,start=list(k=1,r=0.01),trace=T)

testdata <- dc_GDD4 %>% filter(year==2003) %>% select(NewGDD)
testdata2 <- as.data.frame(seq(100,200,by=0.01))
colnames(testdata2) <- "NewGDD"
preds <- predict(nlin,newdata=testdata,type='response')
preds2 <- predict(nlin,newdata=testdata2,type='response')
testdata2[which(preds2>=8.49)[1],]

#GDD of 144.67 ~ 185.85 rounds to Stage 8

plot(testdata2$NewGDD,preds2)

preds3 <- dc_GDD4 %>% group_by(NewYear) %>%
  filter(NewGDD >= 144.67) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  select(Date,doy) %>%
  rename(GDD145=doy) %>%
  mutate(year=year(Date))

preds4 <- dc_GDD4 %>% group_by(NewYear) %>%
  filter(NewGDD >= 185.85) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  select(Date,doy) %>%
  rename(GDD186=doy) %>%
  mutate(year=year(Date))

dc_test2 <- dc_test %>%
  mutate(year=year(Date)) %>%
  left_join(preds3 %>% select(year,GDD145),by="year") %>%
  mutate(err_GDD145=abs(bloom_doy-GDD145))

dc_test3 <- dc_test2 %>%
  left_join(preds4 %>% select(year,GDD186),by="year") %>%
  mutate(err_GDD186=abs(bloom_doy-GDD186))

summary(dc_test3)

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
