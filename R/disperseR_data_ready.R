#getting units counterfactual emission data ready for disperseR
#disperseR on SO2 emissions


rm(list = ls())

# library(gsynth)
library(fst)
library(data.table)
library(tidyverse)
# library(parallel)
#library(sf)
library(viridis)
library(ggplot2)
library(stringi)
# library(panelView)
library(lubridate)
library(dslabs)
library(stringi)
# library(openair)
library(gridExtra)
# library(ggmap)
library(Rmisc)
library(reshape2)

# setwd ("/projects/HAQ_LAB/mrasel/R/causal-study")

setwd ("/Volumes/GoogleDrive/My Drive/R/causal-study-COVID-beyond")

load ("data/result_so2_weekly_2020.RData")
load ("data/facility_so2_2020.RData")


# result_so2_weekly_2020_test <- list()
# facility_test <- list()
# 
# for (i in 1:length(result_so2_weekly_2020)){
#   sublist = result_so2_weekly_2020[[i]]
#   if (length(sublist) == 3)
#   {
#     result_so2_weekly_2020_test[[i]] <-result_so2_weekly_2020[[i]] # this does achieve the desired result
#     facility_test[[i]] <- facility[[i]]
#   }
#   else
#   {
#     result_so2_weekly_2020_test[[i]] <- NULL # this does achieve the desired result
#   }
#   
# }
# 
# result_so2_weekly_2020_test2 <- result_so2_weekly_2020_test[-which(sapply(result_so2_weekly_2020_test, is.null))]
# 
# result_so2_weekly_2020 <- result_so2_weekly_2020_test2
# facility <- unlist(facility_test, recursive = TRUE, use.names = TRUE)

# save(result_so2_weekly_2020, file="data/result_so2_weekly_2020.RData")
# save(facility, file="data/facility_so2_2020.RData")

# load ("data/result_so2_weekly_2020.RData")
# load ("data/facility_so2_2020.RData")



#getting each units counterfactual emission into a dataframe
datalist = list()

for (i in 1:length(facility)) {
  if  (nrow(as.data.frame(result_so2_weekly_2020[[i]][[2]]$Y.ct))==53) {
    ct.emis <- as.data.frame(result_so2_weekly_2020[[i]][[2]]$Y.ct)
    datalist[[i]] <- cbind(ct.emis) # add it to your list
  } else {
    datalist[[i]] <- NULL
  }
}

big_data = do.call(cbind, datalist)

big_data$week <- 1:53


#wider data to long data
df_long <- reshape2::melt(big_data, id.vars="week")



# df_long$year <- 2020
names(df_long)[names(df_long) == 'variable'] <- 'ID'
names(df_long)[names(df_long) == 'value'] <- 'SO2.tons.ct'

#creating date

df_long$year <- 2020

df <- df_long

#date for the year under consideration
df$input_year <-make_date(df$year)

#setting customized first monday of the year
df$first_monday_of_year <- floor_date(df$input_year,
                                      unit = "week",
                                      week_start = 1)

#date (first monday of the week)
df$first_monday_of_week <- df$first_monday_of_year + dweeks(df$week-1 )

df<- df %>% mutate(date = ymd(first_monday_of_week)) %>% mutate_at(vars(first_monday_of_week), funs(year, month, day))

df$month[df$year==2019] <- 1
df$year[df$year==2019] <- 2020


# 
# df_long$day <- as.numeric(df_long$day)
# 
# 
# day.number <- as.vector(1:121)
# day <- as.vector(c((101:131), (201:229), (301:331), (401:430)))
# year <- as.vector(rep(2020, 121))
# 
# date.create <- data.frame(day.number, year)
# date.create$origin <- as.Date(paste0(date.create$year, "-01-01"),tz = "UTC") - days(1)
# date.create$date <- as.Date(date.create$day.number, origin = date.create$origin, tz = "UTC")
# date.create <- date.create %>% dplyr::select(-origin)
# # names(date.create)[names(date.create) == 'day'] <- 'day.number'
# date.create$day <- day
# 
# df_long <- merge(df_long, date.create, by= c("day")) %>% dplyr::select(-day, -year, -day.number)

#converting daily data into monthly data
# df_long<- df_long %>% mutate(date = ymd(date)) %>% mutate_at(vars(date), funs(year, month, day))

ampd_monthly_ct <-  setDT(df)[, .(SO2.tons.ct = sum(SO2.tons.ct, na.rm=TRUE )),
                                   by = .(ID, year, month)]
#removing 2020 from ID variable

ampd_monthly_ct <- ampd_monthly_ct %>%  mutate(ID, ID=gsub("2020_", "", ID))


#getting ampd actual monthly emission file
ampd_monthly_ac <- as.data.table(read.fst ("/Volumes/GoogleDrive/My Drive/R/ampd-raw-data-processing/data/ampd_monthly_all.fst"))

id_ct <- as.vector(unique(ampd_monthly_ct$ID))

#filterning actual monthly emission data file: 2020 data extraction, id that are in counterfactual emission file
ampd_monthly_ac <- ampd_monthly_ac %>% filter(year==2020 & Fuel1.IsCoal==1)
ampd_monthly_ac_id_ct <- ampd_monthly_ac %>% filter (ID %in% id_ct)

#merging actual and counterfactual monthly emission data

ampd_ac_ct <- merge (ampd_monthly_ac, ampd_monthly_ct, by= c ("ID", "year", "month"))

#extracting only coal facilities


ampd_ac_ct <- ampd_ac_ct %>% dplyr::select(ORISPL_CODE, UNITID, Facility.Latitude,
                                           Facility.Longitude, year, month, SO2..tons., SO2.tons.ct)

units_ct_emission_2020 <- ampd_ac_ct [, uID := paste(ORISPL_CODE, UNITID, sep = ".")]


names(units_ct_emission_2020)[names(units_ct_emission_2020) == 'ORISPL_CODE'] <- 'FacID'
names(units_ct_emission_2020)[names(units_ct_emission_2020) == 'UNITID'] <- 'Unit.ID'
names(units_ct_emission_2020)[names(units_ct_emission_2020) == 'Facility.Latitude'] <- 'Latitude'
names(units_ct_emission_2020)[names(units_ct_emission_2020) == 'Facility.Longitude'] <- 'Longitude'


save( units_ct_emission_2020, file = 'data/units_ct_emission_2020.rda')
