#Data clean #script 2
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
library(lubridate)
library(dslabs)
library(stringi)
library(openair)
library(gridExtra)
library(ggmap)
library(Rmisc)

# setwd ("/projects/HAQ_LAB/mrasel/R/causal-study-COVID-beyond")

setwd ("/Volumes/GoogleDrive/My Drive/R/causal-study-COVID-beyond")

#getting all electric facilities in the US
ampd_daily_units_ec <- read.fst ("data/emis_met_2010_2020.fst")

ampd_daily_all_units <-  ampd_daily_units_ec 

rm (ampd_daily_units_ec ) #to save R working memory

#taking facilities which were only operating in 2020
ampd_daily_all_units_2020 <- ampd_daily_all_units %>%  filter (year==2020)
id_operating_2020 <- as.vector(unique(ampd_daily_all_units_2020$ID))
ampd_id <- ampd_daily_all_units %>%  filter (ID %in% id_operating_2020)


#discarding facilities which only have last one or two years data
ampd_daily_all_units.1<- ampd_id  
id <- id_operating_2020
df.yr <- as.data.frame(matrix(nrow = 4250, ncol = 1))

#facilities 10650, 54768, 10649, 59882, 61035, 59326 etc... has data for last five years. 

for (i in 1:length(id)) {
  rs.1<- ampd_daily_all_units.1 %>% filter (ID==id[i] )
  if (min(rs.1$year)>=2015) {
    res.2 <- unique(rs.1$ID)
    df.yr [i, ] <- res.2 }
} 

y <- unique(df.yr$V1)
y<-as.vector(y[!is.na(y)])

rm(ampd_daily_all_units) #to save R working memory
rm(ampd_id) #to save R working memory
rm(ampd_daily_all_units_2020) #to save R working memory

ampd_daily_no_less_2015 <- ampd_daily_all_units.1 %>% filter(!ID %in% y)
id <- as.vector(unique(ampd_daily_no_less_2015$ID))




ampd_daily_select <-ampd_daily_no_less_2015

id <- as.vector(unique(ampd_daily_select$ID))

#excluding facilities which doesn't have all the 12 months data (jan-april)
months <- c (1:12)
years <- c (2019:2020)
dfm <- as.data.frame(matrix(nrow = 4000, ncol = 1))
dfn <- as.data.frame(matrix(nrow = 4000, ncol = 1))
for (i in 1:length(id)) {
  for (j in 1: length(years)) {
    rs.1<- ampd_daily_select %>% filter (ID==id[i] )
    rs.2<- rs.1%>% filter (year==years[j] )
    rs.2 <- length(unique(rs.2$month))
    if(rs.2 == length(months)) {
      rs.3 <- unique(rs.1$ID[rs.2 == length(months)])
      dfm [i, ] <- rs.3
    } else {
      dfn [i, ] <- id [i]
    }
  }
  
}

x <- unique(dfn$V1)
x<-as.vector(x[!is.na(x)])

ampd_daily_select<- ampd_daily_select %>% filter(!ID %in% x)




ampd_daily_all_units <- ampd_daily_select


id <- as.vector(unique(ampd_daily_all_units$ID))




ampd_daily_no_less_2015 <- ampd_daily_all_units

ampd_daily_no_less_2015$SUM_OP_TIME[is.na(ampd_daily_no_less_2015$SUM_OP_TIME)] <-  0

#calculating facilities percentage operation during study period (2010-2020)
dfx <- as.data.frame(matrix(nrow = 3605, ncol = 3))
for (i in 1:length(id)) {
  rs.1<- ampd_daily_no_less_2015 %>% filter (ID==id[i])
  rs.1 <- rs.1 %>% select(ID, SUM_OP_TIME)
  rs.2 <- setDT(rs.1)[, .(op.pct = (length(rs.1$SUM_OP_TIME [rs.1$SUM_OP_TIME >0])/
                                                            length(rs.1$SUM_OP_TIME ))*100),
                      by = .(ID)]
  
  #fac.1 <- as.data.frame(id[i])
  
  dfx [i, ] <- rs.2
}

rm(ampd_daily_all_units.1)
#facilities operated within 15%%
dfx <- dfx %>% filter(V2<15)
id.1 <- as.vector(dfx$V1)


#discarding facilities there were not operating 15% of time during 2010 to 2020
ampd_daily_all_units <- ampd_daily_no_less_2015 %>% filter(!ID %in% id.1)

id <- as.vector(unique(ampd_daily_all_units$ID))


ampd_daily_all_units$SO2..tons.[is.na(ampd_daily_all_units$SO2..tons.)] <-  0
ampd_daily_all_units$NOx..tons.[is.na(ampd_daily_all_units$NOx..tons.)] <-  0


#calculating percentage operation of facilities in 2020
#operation in 2020
#discarding facilities operated less than 10% in 2020
# 
operation.pct.id <- function(id.name) {
  ampd_daily_all_unit <- ampd_daily_all_units %>% filter (ID %in% id.name & year==2020)
  ampd_daily_all_unit.op <- ampd_daily_all_unit %>%
    mutate(op.pct =(length(ampd_daily_all_unit$SUM_OP_TIME [ampd_daily_all_unit$SUM_OP_TIME >0])/
                      length(ampd_daily_all_unit$SUM_OP_TIME ))*100)
  ampd_daily_all_unit <- ampd_daily_all_unit.op
  return(ampd_daily_all_unit)
}

op <- lapply(id, operation.pct.id )

operation <- as.data.frame(matrix(nrow = 2707, ncol =50))

for (i in 1:length(id)) {
  operation [i, ] <- as.data.frame((op[i]))
}

#taking facilities opearating more than 10% in 2020

operation <- operation %>%  filter (V50>10)

ID_operating_25pct <- as.vector(unique(operation$V44))

ampd_daily_all_units <- ampd_daily_all_units %>% filter (ID %in% ID_operating_25pct)





ampd_daily_all_units$day.name <- weekdays(as.Date(ampd_daily_all_units$date))
# ampd_daily_all_units$fog <- as.numeric(ampd_daily_all_units$fog)
# ampd_daily_all_units$rain <- as.numeric(ampd_daily_all_units$rain)
# ampd_daily_all_units$snow <- as.numeric(ampd_daily_all_units$snow)
# ampd_daily_all_units$hail <- as.numeric(ampd_daily_all_units$hail)
# ampd_daily_all_units$thunder <- as.numeric(ampd_daily_all_units$thunder)
# ampd_daily_all_units$tornado <- as.numeric(ampd_daily_all_units$tornado)

rm(ampd_daily_no_less_2015) #to save R working memory

#discarding facilities with no meteoroogy
# 
# ampd_daily_all_units2 <- ampd_daily_all_units
# 
# dfm <- as.data.frame(matrix(nrow = 1200, ncol = 1))
# for (i in 1:length(facility)) {
#   rs.1<- ampd_daily_all_units2 %>% filter (ID==facility[i])
#   if (all(is.na(rs.1$mean_temp))==TRUE | all(is.na(rs.1$dewpoint))==TRUE |
#       all(is.na(rs.1$visibility))==T | all(is.na(rs.1$wind_speed))==T |
#       all(is.na(rs.1$precipitation))==T) {
#     rs.2 <- unique(rs.1$ID)
#     dfm [i, ] <- rs.2 } 
# }
# y <- unique(dfm$V1)
# y<-as.vector(y[!is.na(y)])
# 
# ampd_daily_all_units <- ampd_daily_all_units2 %>% filter(!ID %in% y)
# facility <- as.vector(unique(ampd_daily_all_units$ID))
# 
# #discarding facilities 
# ampd_daily_all_units2 <- ampd_daily_all_units
# dfm <- as.data.frame(matrix(nrow = 1200, ncol = 1))
# df.inf <- as.data.frame(matrix(nrow = 1200, ncol = 1))
# 
# for (i in 1:length(facility)) {
#   rs.1<- ampd_daily_all_units2 %>% filter (ID==facility[i])
#   if (max(rs.1$mean_temp, na.rm=T)==0 | max(rs.1$dewpoint, na.rm=T)==0 |
#       max(rs.1$visibility, na.rm=T)==0 | max(rs.1$wind_speed, na.rm=T)==0 |
#       max(rs.1$precipitation, na.rm=T)==0) {
#     rs.2 <- unique(rs.1$ID)
#     dfm [i, ] <- rs.2 } 
#   else {
#     rs.2 <- unique(rs.1$ID)
#     df.inf [i, ] <- rs.2 
#   }
# }
# 
# y <- unique(dfm$V1)
# y<-as.vector(y[!is.na(y)])

# ampd_daily_all_units <- ampd_daily_all_units2 %>% filter(!ID %in% y)
facility <- as.vector(unique(ampd_daily_all_units$ORISPL_CODE))
id <- as.vector(unique(ampd_daily_all_units$ID))

write.fst(ampd_daily_all_units, "data/ampd_daily_cleaned_facility.fst")

# ampd_daily_all_units <-read.fst("data/ampd_daily_cleaned_facility.fst")
# facility <- as.vector(unique(ampd_daily_all_units$ID))
