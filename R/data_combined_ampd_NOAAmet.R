#this is script is for combining ampd daily data with NOAA GSOD meteorological data


rm(list = ls())

library(fst)
library(data.table)
library(tidyverse)
library(parallel)
#library(sf) #issues loading on cluster
library(viridis)
library(ggplot2)
library(stringi)
library(panelView)
library(lubridate)
library(dslabs)
library(stringi)
library(openair)
library(ggmap)


# setwd ("/Volumes/GoogleDrive/My Drive/R/causal-study")



setwd ("/projects/HAQ_LAB/mrasel/R/causal-study-COVID-beyond")




#daily emission data and meteorological data read

ampd_daily <- read.fst ("data/ampd_daily_emission.fst")

met <- read.fst ("data/us_county_meteorology_1997_2020.fst")


#I need this dataset to get STATE ID information
fips_kevin_study <- read.fst("data/fips_kevin_study.fst")

#murders data from dslabs package has state name which I'll use to merge FIPS code from meteorology data

data(murders)
state.abb <- murders %>% dplyr::select (state, abb)
state.abb <- dplyr::rename(state.abb, "STATE"= "abb")
rm(murders)


ampd_daily <- merge(ampd_daily, state.abb,  by=c("STATE"), all.x=T )

#getting state name and state code
fips_kevin_study2 <- fips_kevin_study %>% dplyr::rename(state= State.Name, FIPS.Code= County.Code) %>%
  dplyr::select(State.Code,  state)

fips_kevin_study2 <- unique(fips_kevin_study2, by = "state")

#merging Full state name
ampd_daily <-  merge(ampd_daily, fips_kevin_study2, by=c("state" ), all.x=T)

ampd_daily <- as.data.table(ampd_daily)
#creating county FIPS code with 5 digits (first 2 digit comes from state code and last 3 digit from County FIPS code)
ampd_daily$FIPS.Code <- ampd_daily[ , stri_pad_left(ampd_daily$FIPS.Code, pad="0", width=3)]
ampd_daily$State.Code <- ampd_daily[ , stri_pad_left(ampd_daily$State.Code, pad="0", width=2)]
ampd_daily <-ampd_daily [, fips := paste(State.Code, FIPS.Code, sep = "")]

# Puerterico and DC state code added into the fips column
ampd_daily$fips <- with(ampd_daily, ifelse(fips=="NA057", "72057",ifelse(fips=="NA001", "11001", fips)))

#removing facilities with no Facility locations
ampd_daily <- subset(ampd_daily, !is.na(Facility.Latitude))

names(ampd_daily)
names(met)
colnames(met) [24] <- "fips"

ampd_daily_emission_met<- merge(ampd_daily, met, by=c("date", "fips" ), all.x=T)

write.fst(ampd_daily_emission_met, "data/ampd_daily_emission_met_1997_2020.fst")

# ampd_daily_emission_met_2010_2020 <- ampd_daily_emission_met %>% filter (year>=2010 & year <=2020)

# write.fst(ampd_daily_emission_met, "data/ampd_daily_emission_met_2010_2020.fst")

#creating a small file to work on "TX" State data 1997 to 2020

ampd_daily_emission_met_1997_2020 <- read.fst ("data/ampd_daily_emission_met_1997_2020.fst")
ampd_daily_emission_met_2010_2020 <- ampd_daily_emission_met_1997_2020 %>% filter (year>=2010 & year <=2020)

ampd_daily_emission_met_2010_2020 <- ampd_daily_emission_met_2010_2020 %>%
  dplyr::select(date, fips,state, STATE, ORISPL_CODE, UNITID, year, month, day, SO2..tons.,
                NOx..tons., CO2..tons., Facility.Latitude, Facility.Longitude, Fuel.Type..Primary.,
                Fuel.Type..Secondary., SO2.Control.s., NOx.Control.s., PM.Control.s.,
                mean_temp, dewpoint, sea_level_pressure, visibility, wind_speed, wind_gust,
                precipitation, fog, rain, hail, snow, thunder, tornado, ID)

write.fst(ampd_daily_emission_met_2010_2020, "data/ampd_daily_emission_met_2010_2020.fst")

