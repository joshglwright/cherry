library(tidyverse)
library(forecast)
library(DescTools)
library(randtests)
library(here)
i_am("peak-bloom-prediction/analysis_dc.R")
dc <- read_csv(here("peak-bloom-prediction/data/washingtondc.csv"))


dc_ts<-ts(data = dc$bloom_doy, start = 1921)
plot.ts(dc_ts)
Box.test(dc_ts, type = "Ljung-Box")
RunsTest(dc$bloom_doy)
BartelsRankTest(dc$bloom_doy)

dc_cur<-dc %>%
  filter(year >= 1980)
dccur_ts<-ts(data = dc_cur$bloom_doy, start = 1980)
plot.ts(dccur_ts)

pheno_ind <- read_csv(here("peak-bloom-prediction/data/USA-NPN_individual_phenometrics_data.csv"))
dc_pheno <- pheno_ind %>%
  filter(State == "DC")


inten_ind <- read_csv(here("peak-bloom-prediction/data/USA-NPN_status_intensity_observations_data.csv"))
dc_inten <- inten_ind %>%
  filter(State == "DC")

dc_dayl<-read_csv(here("peak-bloom-prediction/data/statistics_dayl.csv"))
dcdl_ts<-ts(dc_dayl$value_mean)
plot.ts(dcdl_ts)
dcdlv_ts<-ts(dc_dayl$value_variance)
plot.ts(dcdlv_ts)

dc_tmin<-read_csv(here("peak-bloom-prediction/data/statistics_tmin.csv"))
dctmin_ts<-ts(dc_tmin$value_mean)
plot.ts(dctmin_ts)

dc_tmax<-read_csv(here("peak-bloom-prediction/data/statistics_tmax.csv"))
dctmax_ts<-ts(dc_tmax$value_mean)
plot.ts(dctmax_ts)

### T_max needs to be ar 45 F or lower

dc_tmax_mod<-
  dc_tmax %>%
  mutate(chilled = (value_mean <= ((45-32)*5/9))) %>%
  mutate(year = substr(dt,1,4)) %>%
  mutate(month = substr(dt,6,7)) %>%
  filter((month == '12' & year == '1980')|year != '1980')

yrs<-unique(dc_tmax_mod$year)
chilled_yrtotal<-c()

for (i in 2:(length(yrs))) {
dc_tmax_mod_2<-dc_tmax_mod %>%
  filter((month == '12' & year == yrs[i-1])|(month == '01' & year == yrs[i])
         |(month == '02' & year == yrs[i])|(month == '03' & year == yrs[i])|(month == '04' & year == yrs[i])) %>%
  mutate(chilled_yr = cumsum(chilled))
  chilled_yrtotal<-append(chilled_yrtotal,c(dc_tmax_mod_2$chilled_yr,rep(0,214)),after = length(chilled_yrtotal))
}

chilled_yrtotal_f<-c(chilled_yrtotal,rep(0,30))
dc_tmax_mod_3<-cbind(dc_tmax_mod,chilled_yrtotal_f)
dc_tmax_mod_3<-dc_tmax_mod_3 %>% 
  rename(bloom_date = dt)
dc_mod<-left_join(dc_cur,dc_tmax_mod_3, by = 'bloom_date')

dc_mod %>%
  ggplot(mapping = aes(x = chilled_yrtotal_f , y = bloom_doy))+
  geom_point()

dc_mod %>%
  filter(chilled_yrtotal_f>=40) %>%
  ggplot(mapping = aes(x = chilled_yrtotal_f , y = bloom_doy))+
  geom_point()+
  geom_smooth(method = 'lm')


dc_mod %>%
  lm(formula = bloom_doy ~ chilled_yrtotal_f, data = .) %>%
  summary()


### T_min needs to be ar 45 F or higher

dc_tmin_mod<-
  dc_tmin %>%
  mutate(warmed = (value_mean >= ((35-32)*5/9))) %>%
  mutate(year = substr(dt,1,4)) %>%
  mutate(month = substr(dt,6,7)) %>%
  filter(year != '1980')

yrs<-unique(dc_tmin_mod$year)
warmed_yrtotal<-c()

for (i in 1:(length(yrs))) {
  dc_tmin_mod_2<-dc_tmin_mod %>%
    filter((month == '01' & year == yrs[i])|(month == '02' & year == yrs[i])|
             (month == '03' & year == yrs[i])|(month == '04' & year == yrs[i])) %>%
    mutate(warmed_yr = cumsum(warmed))
  if(i%%4 == 0){
  warmed_yrtotal<-append(warmed_yrtotal,c(dc_tmin_mod_2$warmed_yr,rep(0,244)),after = length(warmed_yrtotal))
  }else{
    warmed_yrtotal<-append(warmed_yrtotal,c(dc_tmin_mod_2$warmed_yr,rep(0,245)),after = length(warmed_yrtotal))
  }
}

dc_tmin_mod_3<-cbind(dc_tmin_mod,warmed_yrtotal)
dc_tmin_mod_3<-dc_tmin_mod_3 %>% 
  rename(bloom_date = dt)

dc_mod_2<-left_join(dc_mod,dc_tmin_mod_3, by = 'bloom_date')

dc_mod_2 %>%
  ggplot(mapping = aes(x = warmed_yrtotal , y = bloom_doy))+
  geom_point()

dc_clean<-dc_mod_2[2:41,c('year.x','bloom_doy','warmed_yrtotal','chilled_yrtotal_f')]

dc_clean %>%
  glm(formula = bloom_doy ~ warmed_yrtotal+chilled_yrtotal_f,
      data = .,
      family = quasipoisson(link = 'log')) %>%
  summary()
