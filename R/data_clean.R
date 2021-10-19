#cleaning data to get facilities to be used in gsynth model
rm(list = ls())

library(gsynth)
library(fst)
library(data.table)
library(tidyverse)
library(parallel)
#library(sf)
library(viridis)
library(ggplot2)
library(stringi)
library(panelView)
library(lubridate)
library(dslabs)
library(stringi)
library(openair)
library(gridExtra)
library(ggmap)
library(Rmisc)

# setwd ("/projects/HAQ_LAB/mrasel/R/causal-study")

setwd ("/Volumes/GoogleDrive/My Drive/R/causal-study-COVID-beyond")

#getting all electric facilities in the US
ampd_daily_units_ec <- read.fst ("data/ampd_daily_emission_met_2010_2020.fst")

ampd_daily_all_units <-  ampd_daily_units_ec 

rm (ampd_daily_units_ec ) #to save R working memory

#taking facilities which were only operating in 2020
ampd_daily_all_units_2020 <- ampd_daily_all_units %>%  filter (year==2020)
facility_operating_2020 <- as.vector(unique(ampd_daily_all_units_2020$ORISPL_CODE))
ampd_facility <- ampd_daily_all_units %>%  filter (ORISPL_CODE %in% facility_operating_2020)


#discarding facilities which only have last one or two years data
ampd_daily_all_units.1<- ampd_facility  
facility <- facility_operating_2020
df.yr <- as.data.frame(matrix(nrow = 1200, ncol = 1))

#facilities 10650, 54768, 10649, 59882, 61035, 59326 etc... has data for last one or two years. 

for (i in 1:length(facility)) {
  rs.1<- ampd_daily_all_units.1 %>% filter (ORISPL_CODE==facility[i] )
  if (min(rs.1$year)>=2015) {
    res.2 <- unique(rs.1$ORISPL_CODE)
    df.yr [i, ] <- res.2 }
} 

y <- unique(df.yr$V1)
y<-as.vector(y[!is.na(y)])

rm(ampd_daily_all_units) #to save R working memory
rm(ampd_facility) #to save R working memory
rm(ampd_daily_all_units_2020) #to save R working memory

ampd_daily_no_less_2015 <- ampd_daily_all_units.1 %>% filter(!ORISPL_CODE %in% y)
facility <- as.vector(unique(ampd_daily_no_less_2015$ORISPL_CODE))

#calculating facilities percentage operation during study period
dfx <- as.data.frame(matrix(nrow = 1200, ncol = 3))
for (i in 1:length(facility)) {
  rs.1<- ampd_daily_no_less_2015 %>% filter (ORISPL_CODE==facility[i])
  rs.1 <- rs.1 %>% select(ORISPL_CODE, CO2..tons.)
  rs.2 <- setDT(rs.1)[, .(op.pct = sum(!is.na(rs.1$CO2..tons.))/length(rs.1$CO2..tons.)*100),
                      by = .(ORISPL_CODE)]
  
  #fac.1 <- as.data.frame(facility[i])
  
  dfx [i, ] <- rs.2
}

rm(ampd_daily_all_units.1)
#facilities operated within 10%%
dfx <- dfx %>% filter(V2<10)
facility.1 <- as.vector(dfx$V1)


#discarding facilities there were not operating 1% of time during 2010 to 2020
ampd_daily_all_units <- ampd_daily_no_less_2015 %>% filter(!ORISPL_CODE %in% facility.1)

facility <- as.vector(unique(ampd_daily_all_units$ORISPL_CODE))

ampd_daily_all_units$SO2..tons.[is.na(ampd_daily_all_units$SO2..tons.)] <-  0


ampd_daily_all_units$day.name <- weekdays(as.Date(ampd_daily_all_units$date))
ampd_daily_all_units$fog <- as.numeric(ampd_daily_all_units$fog)
ampd_daily_all_units$rain <- as.numeric(ampd_daily_all_units$rain)
ampd_daily_all_units$snow <- as.numeric(ampd_daily_all_units$snow)
ampd_daily_all_units$hail <- as.numeric(ampd_daily_all_units$hail)
ampd_daily_all_units$thunder <- as.numeric(ampd_daily_all_units$thunder)
ampd_daily_all_units$tornado <- as.numeric(ampd_daily_all_units$tornado)

rm(ampd_daily_no_less_2015) #to save R working memory

#discarding

ampd_daily_all_units2 <- ampd_daily_all_units

dfm <- as.data.frame(matrix(nrow = 1200, ncol = 1))

for (i in 1:length(facility)) {
  rs.1<- ampd_daily_all_units2 %>% filter (ORISPL_CODE==facility[i])
  if (max(na.omit(rs.1$tornado))==0 | max(na.omit(rs.1$thunder))==0 ) {
    rs.1 <- unique(rs.1$ORISPL_CODE)
    dfm [i, ] <- rs.1 }
}
y <- unique(df.yr$V1)
y<-as.vector(y[!is.na(y)])

