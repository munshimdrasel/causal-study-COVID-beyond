rm(list = ls())

library(gsynth)
library(fst)
library(data.table)
library(tidyverse)
# library(parallel)
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
ampd_daily_all_units<- read.fst ("data/ampd_daily_cleaned_facility.fst")


ampd_daily_select <- ampd_daily_all_units %>% dplyr::select(ORISPL_CODE, date, STATE, UNITID, ID,
                                                            year, month, day, SO2..tons., NOx..tons., pr, tmmx, rmax, vs, th,
                                                            Fuel.Type..Primary., COUNT_OP_TIME, SUM_OP_TIME)

ampd_daily_select <- ampd_daily_select 
facility <- as.vector(unique(ampd_daily_select$ORISPL_CODE))


ampd_daily_select$SUM_OP_TIME[is.na(ampd_daily_select$SUM_OP_TIME)] <-  0
#operation in 2019
#discarding facilities operated less than 25% in 2019
# 
# operation.pct.facility <- function(facility.name) {
#   ampd_daily_all_unit <- ampd_daily_select %>% filter (ORISPL_CODE %in% facility.name & year==2019)
# 
#   ampd_daily_all_unit.op <- ampd_daily_all_unit %>% mutate(op.pct =(length(ampd_daily_all_unit$SUM_OP_TIME [ampd_daily_all_unit$SUM_OP_TIME >0])/
#                                                                       length(ampd_daily_all_unit$SUM_OP_TIME ))*100)
#   ampd_daily_all_unit <- ampd_daily_all_unit.op
#   return(ampd_daily_all_unit)
# }
# 
# op <- lapply(facility, operation.pct.facility )
# 
# operation <- as.data.frame(matrix(nrow = 1048, ncol =18))
# 
# for (i in 1:length(facility)) {
#   operation [i, ] <- as.data.frame((op[i]))
# }
# 
# #taking facilities opearating more than 25% in 2019
# 
# operation <- operation %>%  filter (V16>25)
# ID_operating_25pct <- as.vector(unique(operation$V5))

# ampd_daily_select <- ampd_daily_select %>% filter (ID %in% ID_operating_25pct)

#facilities opearating in 2019 only

ampd_daily_all_units_2019 <- ampd_daily_select%>%  filter (year==2019)
facility_operating_2019 <- as.vector(unique(ampd_daily_all_units_2019$ORISPL_CODE))
ampd_facility <- ampd_daily_all_units %>%  filter (ORISPL_CODE %in% facility_operating_2019)


ampd_daily_to_weekly <- setDT(ampd_facility)[, .(SO2..tons. = sum(SO2..tons., na.rm=TRUE),
                                                 NOx..tons. = sum(NOx..tons., na.rm=TRUE),
                                                 pr= sum(pr, na.rm=T),
                                                 tmmx=mean(tmmx, na.rm=T),
                                                 vs= mean(vs, na.rm=T),
                                                 rmax= mean(rmax, na.rm=T),
                                                 th= mean(th, na.rm=T),
                                                 SUM_OP_TIME=sum(SUM_OP_TIME, na.rm=T)),
                                             by = .(STATE, ORISPL_CODE, ID, year,
                                                    isoweek(date),Fuel.Type..Primary.)]

ampd_weekly <- ampd_daily_to_weekly %>% filter ( year <=2019 & th>0) #wind direction should not be negative



ampd_daily_all_units <- ampd_weekly
facility <- as.vector(unique(ampd_daily_all_units$ORISPL_CODE))

# save(facility, file="data/facility_nox_2019.RData")
load ("data/facility_nox_2019.RData")

# fileConn<-file("output.txt")
# 
# gsynth.fn <- function(facility.name) {
#   ampd_daily_all_unit <- ampd_daily_all_units %>% filter (ORISPL_CODE %in% facility.name )
#   all_ampd <- ampd_daily_all_unit
#   id_selected <-  unique( ampd_daily_all_unit$ID)
#   all_ampd <-  all_ampd %>%  dplyr::select (STATE, year, isoweek, ORISPL_CODE, ID, NOx..tons.,
#                                             pr, tmmx, rmax, vs, th)
#   # all_ampd$NOx..tons.[is.na(all_ampd$NOx..tons.)] <-  0
# 
#   all_ampd <- as.data.table(all_ampd)
# 
#   #setting intervention
#   all_ampd <- all_ampd[, inputed := 0]
#   all_ampd$inputed[all_ampd$year==2019 & all_ampd$isoweek>8 &  all_ampd$ID %in% id_selected] <- 1
# 
#   all_ampd <- all_ampd [, id := paste(year, ID, sep = "_")]
# 
#   all_ampd_final <- all_ampd%>% dplyr::select (year, isoweek, id, NOx..tons.,  inputed,
#                                                pr, tmmx, rmax, vs, th)
# 
# 
#   synth_eq <- NOx..tons. ~ inputed+ tmmx + rmax + pr + vs #omiting wind direction
# 
#   paste("Facility now running", facility.name, unique(ampd_daily_all_unit$STATE), sep= " ")
# 
#   # synth_eq <- NOx..tons. ~ inputed+  tmmx + rmax + pr + th + vs + day.id
#   tryCatch({
#     #Running gsynth
#     out <<- gsynth(synth_eq,
#                    data=all_ampd_final,
#                    index = c("id","isoweek"), na.rm=T, force = "two-way", se=FALSE,
#                    CV = TRUE, seed =  123, estimator = "mc", parallel=T,
#                    inference = "nonparametric")
# 
#     x <- as.data.frame(out$Y.bar)
#     y <- as.data.frame(out$time)
#     # m <- as.data.frame(rep((facility), times=30))
#     z <- cbind(y,x)
#     colnames(z)[1] <- "time"
#     # somdate2 <- substr(somDate,7,10)
#     # som.date <- as.numeric(gsub("-", "", as.character(somdate2)))
#     # xx <- z %>% filter(out$time <= "8")
# 
#     stats <- as.data.table(modStats(z, mod = "Y.ct.bar", obs = "Y.tr.bar"))
#     # data_plots <- plot(out, type = "missing", theme.bw = TRUE,
#     # main = paste(facility.name),)
#     counterfactual_plots <- plot(out, type = "counterfactual", raw = "none",
#                                  main=paste("Facility", facility.name,unique(ampd_daily_all_unit$STATE), sep= "-")) +
#       scale_x_continuous(breaks = seq(0, 52, by = 5)) + theme(legend.position = "none") 
#     # counterfactual_plots_att <- plot(out, main=paste(facility.name)) +
#     #   scale_x_continuous(breaks = seq(0, 120, by = 10)) +
#     #   theme(axis.text.x = element_text(angle = 90))
# 
#     # all <- list(stats, out, counterfactual_plots, counterfactual_plots_att)
#     all <- list(stats, out,counterfactual_plots)
#   }, error = function(e) {
#     writeLines(paste0("at facility ", facility.name, " something caused error "), fileConn)
#   })
#   return(all)
# }
# result_nox_weekly_2019 <- lapply(facility, gsynth.fn)
# close(fileConn)
# 
# 
# save(result_nox_weekly_2019, file="data/result_nox_weekly_2019_no_wd.RData")
# ===============================================================================================
#                                           Model evaluation
# ===============================================================================================
# 
load ("data/result_nox_weekly_2019_no_wd.RData")
load ("data/facility_nox_2019.RData")
#
# result_nox_weekly_2019_test <- list()
# facility_test <- list()
#
# for (i in 1:length(result_nox_weekly_2019)){
#   sublist = result_nox_weekly_2019[[i]]
#   if (length(sublist) == 2)
#   {
#     result_nox_weekly_2019_test[[i]] <-result_nox_weekly_2019[[i]] # this does achieve the desired result
#     facility_test[[i]] <- facility[[i]]
#   }
#     else
#     {
#       result_nox_weekly_2019_test[[i]] <- NULL # this does achieve the desired result
#     }
#
#   }
#
# result_nox_weekly_2019_test2 <- result_nox_weekly_2019_test[-which(sapply(result_nox_weekly_2019_test, is.null))]
#
# result_nox_weekly_2019 <- result_nox_weekly_2019_test2
# facility <- unlist(facility_test, recursive = TRUE, use.names = TRUE)
#
# # getting each units actual emission into a dataframe====================================================
datalist = list()

for (i in 1:length(facility)) {
  nox.emis <- na.omit(as.data.frame(result_nox_weekly_2019[[i]][[2]]$Y.bar))
  nox.emis <- nox.emis %>% dplyr::select(-Y.co.bar)
  nox.emis$week <- c(1:52)
  nox.emis <- nox.emis %>% filter(week>=9)
  stats <- as.data.table(modStats(nox.emis, mod = "Y.ct.bar", obs = "Y.tr.bar"))
  fac.1 <- as.data.frame(facility[i])
  datalist[[i]] <- cbind(fac.1,stats) # add it to your list
}

ac.ct.nox.emission = do.call(rbind, datalist)

names(ac.ct.nox.emission)[names(ac.ct.nox.emission) == 'facility[i]'] <- 'facility'

#adding Operation hours of 2019

fac.op <- setDT(ampd_weekly)[, .(SUM_OP_TIME=sum(SUM_OP_TIME, na.rm=T)),
                             by = .( ORISPL_CODE,  year)]
fac.op.2019 <- fac.op %>%  filter (year==2019)

fac.op.2019 <- fac.op.2019 %>% filter (ORISPL_CODE %in% facility)

names(fac.op.2019)[names(fac.op.2019) == 'ORISPL_CODE'] <- 'facility'
ac.ct.nox.emission <- merge(ac.ct.nox.emission, fac.op.2019 , by= c("facility" ))

# ac.ct.nox.emission$r.sqd <- (ac.ct.nox.emission$r)^2

ac.ct.nox.emission <- ac.ct.nox.emission #%>%  filter (SUM_OP_TIME>0)

summary(ac.ct.nox.emission)

ac.ct.nox.emission %>%  ggplot(aes(  SUM_OP_TIME, NMGE )) +
    geom_point() + labs(x= "Operation time (hr)", y = "", title = " ")

ac.ct.nox.emission %>%  ggplot(aes(  SUM_OP_TIME, NMB )) +
  geom_point() + labs(x= "Operation time (hr)", y = "", title = " ")


ac.ct.nox.emission %>%
  ggplot(aes(RMSE)) +
  geom_histogram(binwidth = 1, color = "black")




# # ===============================average of 2016, 2017, 2018 vs 2019 emission======================
# ampd_daily_emissions <- read.fst ("/Volumes/GoogleDrive/My Drive/R/ampd-raw-data-processing/data/ampd_daily_emission.fst")
ampd_daily_emissions_2010_2018_9_52 <- ampd_daily_all_units %>%
  filter (year %in% c( 2010:2018) &
            isoweek >=9) %>% dplyr::select(ORISPL_CODE,  isoweek, NOx..tons.)

ampd_daily_emissions_2010_2018_9_52 <- setDT(ampd_daily_emissions_2010_2018_9_52)[, (
                                NOx..tons. = mean(NOx..tons., na.rm=TRUE)),
                                             by = .( ORISPL_CODE, isoweek)]

ampd_daily_emissions_2010_2018_9_52 <- ampd_daily_emissions_2010_2018_9_52 %>%
  filter (ORISPL_CODE %in% facility )


datalist = list()

for (i in 1:length(facility)) {
  ampd_daily_all_unit <- ampd_daily_emissions_2010_2018_9_52 %>% filter (ORISPL_CODE %in% facility[i] )
  names(ampd_daily_all_unit)[names(ampd_daily_all_unit) == 'V1'] <- 'ct.nox.tons' #average of previous years
  avg.nox.emis <- ampd_daily_all_unit
  names(avg.nox.emis)[names(avg.nox.emis) == 'isoweek'] <- 'week'
  avg.nox.emis$week <- as.character(avg.nox.emis$week)

  nox.emis <- na.omit(as.data.frame(result_nox_weekly_2019[[i]][[2]]$Y.bar))
  nox.emis <- nox.emis %>% dplyr::select(Y.tr.bar) #taking average 2019 emission data
  nox.emis$week <- c(1:52)
  nox.emis <- nox.emis %>% filter(week>=9)
  nox.emis$week <- as.character(nox.emis$week)
  nox.emis$ORISPL_CODE <- facility[i]
  names(nox.emis)[names(nox.emis) == 'Y.tr.bar'] <- 'ac.nox.tons' #actual 2019 dataset

  nox.emis.linear <- merge(avg.nox.emis, nox.emis, by= c("ORISPL_CODE", "week"))
  stats <- as.data.table(modStats(nox.emis.linear, mod = "ct.nox.tons", obs = "ac.nox.tons"))
  fac.1 <- as.data.frame(facility[i])
  datalist[[i]] <- cbind(fac.1,stats) # add it to your list
}

sim.avg.ac.ct.nox.emission.9 = do.call(rbind, datalist)

names(sim.avg.ac.ct.nox.emission.9)[names(sim.avg.ac.ct.nox.emission.9) == 'facility[i]'] <- 'facility'

sim.avg.ac.ct.nox.emission.9 <- merge(sim.avg.ac.ct.nox.emission.9, fac.op.2019 , by= c("facility" ))



sim.avg.ac.ct.nox.emission.9 <- sim.avg.ac.ct.nox.emission.9  #2010:2018 simple average vs 2019 emission



# =========2012 to 2018 average
ampd_daily_emissions_2012_2018_9_52 <- ampd_daily_all_units %>%
  filter (year %in% c( 2012:2018) &
            isoweek >=9) %>% dplyr::select(ORISPL_CODE,  isoweek, NOx..tons.)

ampd_daily_emissions_2012_2018_9_52 <- setDT(ampd_daily_emissions_2012_2018_9_52)[, (
  NOx..tons. = mean(NOx..tons., na.rm=TRUE)),
  by = .( ORISPL_CODE, isoweek)]

ampd_daily_emissions_2012_2018_9_52 <- ampd_daily_emissions_2012_2018_9_52 %>%
  filter (ORISPL_CODE %in% facility )


datalist = list()

for (i in 1:length(facility)) {
  ampd_daily_all_unit <- ampd_daily_emissions_2012_2018_9_52 %>% filter (ORISPL_CODE %in% facility[i] )
  names(ampd_daily_all_unit)[names(ampd_daily_all_unit) == 'V1'] <- 'ct.nox.tons' #average of previous years
  avg.nox.emis <- ampd_daily_all_unit
  names(avg.nox.emis)[names(avg.nox.emis) == 'isoweek'] <- 'week'
  avg.nox.emis$week <- as.character(avg.nox.emis$week)

  nox.emis <- na.omit(as.data.frame(result_nox_weekly_2019[[i]][[2]]$Y.bar))
  nox.emis <- nox.emis %>% dplyr::select(Y.tr.bar) #taking average 2019 emission data
  nox.emis$week <- c(1:52)
  nox.emis <- nox.emis %>% filter(week>=9)
  nox.emis$week <- as.character(nox.emis$week)
  nox.emis$ORISPL_CODE <- facility[i]
  names(nox.emis)[names(nox.emis) == 'Y.tr.bar'] <- 'ac.nox.tons' #actual 2019 dataset

  nox.emis.linear <- merge(avg.nox.emis, nox.emis, by= c("ORISPL_CODE", "week"))
  stats <- as.data.table(modStats(nox.emis.linear, mod = "ct.nox.tons", obs = "ac.nox.tons"))
  fac.1 <- as.data.frame(facility[i])
  datalist[[i]] <- cbind(fac.1,stats) # add it to your list
}

sim.avg.ac.ct.nox.emission.7 = do.call(rbind, datalist)

names(sim.avg.ac.ct.nox.emission.7)[names(sim.avg.ac.ct.nox.emission.7) == 'facility[i]'] <- 'facility'

sim.avg.ac.ct.nox.emission.7 <- merge(sim.avg.ac.ct.nox.emission.7, fac.op.2019 , by= c("facility" ))




sim.avg.ac.ct.nox.emission.7<- sim.avg.ac.ct.nox.emission.7   #2012:2018



# =========2014 to 2018 average

ampd_daily_emissions_2014_2018_9_52 <- ampd_daily_all_units %>%
  filter (year %in% c( 2014:2018) &
            isoweek >=9) %>% dplyr::select(ORISPL_CODE,  isoweek, NOx..tons.)

ampd_daily_emissions_2014_2018_9_52 <- setDT(ampd_daily_emissions_2014_2018_9_52)[, (
  NOx..tons. = mean(NOx..tons., na.rm=TRUE)),
  by = .( ORISPL_CODE, isoweek)]

ampd_daily_emissions_2014_2018_9_52 <- ampd_daily_emissions_2014_2018_9_52 %>%
  filter (ORISPL_CODE %in% facility )


datalist = list()

for (i in 1:length(facility)) {
  ampd_daily_all_unit <- ampd_daily_emissions_2014_2018_9_52 %>% filter (ORISPL_CODE %in% facility[i] )
  names(ampd_daily_all_unit)[names(ampd_daily_all_unit) == 'V1'] <- 'ct.nox.tons' #average of previous years
  avg.nox.emis <- ampd_daily_all_unit
  names(avg.nox.emis)[names(avg.nox.emis) == 'isoweek'] <- 'week'
  avg.nox.emis$week <- as.character(avg.nox.emis$week)

  nox.emis <- na.omit(as.data.frame(result_nox_weekly_2019[[i]][[2]]$Y.bar))
  nox.emis <- nox.emis %>% dplyr::select(Y.tr.bar) #taking average 2019 emission data
  nox.emis$week <- c(1:52)
  nox.emis <- nox.emis %>% filter(week>=9)
  nox.emis$week <- as.character(nox.emis$week)
  nox.emis$ORISPL_CODE <- facility[i]
  names(nox.emis)[names(nox.emis) == 'Y.tr.bar'] <- 'ac.nox.tons' #actual 2019 dataset

  nox.emis.linear <- merge(avg.nox.emis, nox.emis, by= c("ORISPL_CODE", "week"))
  stats <- as.data.table(modStats(nox.emis.linear, mod = "ct.nox.tons", obs = "ac.nox.tons"))
  fac.1 <- as.data.frame(facility[i])
  datalist[[i]] <- cbind(fac.1,stats) # add it to your list
}

sim.avg.ac.ct.nox.emission.5 = do.call(rbind, datalist)

names(sim.avg.ac.ct.nox.emission.5)[names(sim.avg.ac.ct.nox.emission.5) == 'facility[i]'] <- 'facility'

sim.avg.ac.ct.nox.emission.5 <- merge(sim.avg.ac.ct.nox.emission.5, fac.op.2019 , by= c("facility" ))


sim.avg.ac.ct.nox.emission.5<- sim.avg.ac.ct.nox.emission.5   #2014:2018


# =========2016 to 2018 average
ampd_daily_emissions_2016_2018_9_52 <- ampd_daily_all_units %>%
  filter (year %in% c( 2016:2018) &
            isoweek >=9) %>% dplyr::select(ORISPL_CODE,  isoweek, NOx..tons.)

ampd_daily_emissions_2016_2018_9_52 <- setDT(ampd_daily_emissions_2016_2018_9_52)[, (
  NOx..tons. = mean(NOx..tons., na.rm=TRUE)),
  by = .( ORISPL_CODE, isoweek)]

ampd_daily_emissions_2016_2018_9_52 <- ampd_daily_emissions_2016_2018_9_52 %>%
  filter (ORISPL_CODE %in% facility )


datalist = list()

for (i in 1:length(facility)) {
  ampd_daily_all_unit <- ampd_daily_emissions_2016_2018_9_52 %>% filter (ORISPL_CODE %in% facility[i] )
  names(ampd_daily_all_unit)[names(ampd_daily_all_unit) == 'V1'] <- 'ct.nox.tons' #average of previous years
  avg.nox.emis <- ampd_daily_all_unit
  names(avg.nox.emis)[names(avg.nox.emis) == 'isoweek'] <- 'week'
  avg.nox.emis$week <- as.character(avg.nox.emis$week)

  nox.emis <- na.omit(as.data.frame(result_nox_weekly_2019[[i]][[2]]$Y.bar))
  nox.emis <- nox.emis %>% dplyr::select(Y.tr.bar) #taking average 2019 emission data
  nox.emis$week <- c(1:52)
  nox.emis <- nox.emis %>% filter(week>=9)
  nox.emis$week <- as.character(nox.emis$week)
  nox.emis$ORISPL_CODE <- facility[i]
  names(nox.emis)[names(nox.emis) == 'Y.tr.bar'] <- 'ac.nox.tons' #actual 2019 dataset

  nox.emis.linear <- merge(avg.nox.emis, nox.emis, by= c("ORISPL_CODE", "week"))
  stats <- as.data.table(modStats(nox.emis.linear, mod = "ct.nox.tons", obs = "ac.nox.tons"))
  fac.1 <- as.data.frame(facility[i])
  datalist[[i]] <- cbind(fac.1,stats) # add it to your list
}

sim.avg.ac.ct.nox.emission.3 = do.call(rbind, datalist)

names(sim.avg.ac.ct.nox.emission.3)[names(sim.avg.ac.ct.nox.emission.3) == 'facility[i]'] <- 'facility'

sim.avg.ac.ct.nox.emission.3 <- merge(sim.avg.ac.ct.nox.emission.3, fac.op.2019 , by= c("facility" ))


sim.avg.ac.ct.nox.emission.3<- sim.avg.ac.ct.nox.emission.3   #2016:2018



# =========2018 to 2018 average means 2018 vs 2019 emissions check

ampd_daily_emissions_2018_2018_9_52 <- ampd_daily_all_units %>%
  filter (year %in% c( 2018:2018) &
            isoweek >=9) %>% dplyr::select(ORISPL_CODE,  isoweek, NOx..tons.)

ampd_daily_emissions_2018_2018_9_52 <- setDT(ampd_daily_emissions_2018_2018_9_52)[, (
  NOx..tons. = mean(NOx..tons., na.rm=TRUE)),
  by = .( ORISPL_CODE, isoweek)]

ampd_daily_emissions_2018_2018_9_52 <- ampd_daily_emissions_2018_2018_9_52 %>%
  filter (ORISPL_CODE %in% facility )


datalist = list()

for (i in 1:length(facility)) {
  ampd_daily_all_unit <- ampd_daily_emissions_2018_2018_9_52 %>% filter (ORISPL_CODE %in% facility[i] )
  names(ampd_daily_all_unit)[names(ampd_daily_all_unit) == 'V1'] <- 'ct.nox.tons' #average of previous years
  avg.nox.emis <- ampd_daily_all_unit
  names(avg.nox.emis)[names(avg.nox.emis) == 'isoweek'] <- 'week'
  avg.nox.emis$week <- as.character(avg.nox.emis$week)

  nox.emis <- na.omit(as.data.frame(result_nox_weekly_2019[[i]][[2]]$Y.bar))
  nox.emis <- nox.emis %>% dplyr::select(Y.tr.bar) #taking average 2019 emission data
  nox.emis$week <- c(1:52)
  nox.emis <- nox.emis %>% filter(week>=9)
  nox.emis$week <- as.character(nox.emis$week)
  nox.emis$ORISPL_CODE <- facility[i]
  names(nox.emis)[names(nox.emis) == 'Y.tr.bar'] <- 'ac.nox.tons' #actual 2019 dataset

  nox.emis.linear <- merge(avg.nox.emis, nox.emis, by= c("ORISPL_CODE", "week"))
  stats <- as.data.table(modStats(nox.emis.linear, mod = "ct.nox.tons", obs = "ac.nox.tons"))
  fac.1 <- as.data.frame(facility[i])
  datalist[[i]] <- cbind(fac.1,stats) # add it to your list
}

sim.avg.ac.ct.nox.emission.1 = do.call(rbind, datalist)

names(sim.avg.ac.ct.nox.emission.1)[names(sim.avg.ac.ct.nox.emission.1) == 'facility[i]'] <- 'facility'

sim.avg.ac.ct.nox.emission.1 <- merge(sim.avg.ac.ct.nox.emission.1, fac.op.2019 , by= c("facility" ))



sim.avg.ac.ct.nox.emission.1<- sim.avg.ac.ct.nox.emission.1   #2018:2018



# big_data<- big_data %>% filter (r !="NA")

# sim.avg.ac.ct.nox.emission %>%
#   ggplot(aes(RMSE)) +
#   geom_histogram(binwidth = 1, color = "black") + ylim(0,80)

sim.avg.ac.ct.nox.emission.9$group <- "9 year mean"
sim.avg.ac.ct.nox.emission.7$group <- "7 year mean"
sim.avg.ac.ct.nox.emission.5$group <- "5 year mean"
sim.avg.ac.ct.nox.emission.3$group <- "3 year mean"
sim.avg.ac.ct.nox.emission.1$group <- "1 year"
ac.ct.nox.emission$group <- "gsynth"
# sensitivity.ac.ct.nox.emission$group <- "sensitivity-gsynth"

all.nox.emission <- rbind(sim.avg.ac.ct.nox.emission.9, sim.avg.ac.ct.nox.emission.7,
                          sim.avg.ac.ct.nox.emission.5, sim.avg.ac.ct.nox.emission.3,
                          sim.avg.ac.ct.nox.emission.1 ,ac.ct.nox.emission)
# all.nox.emission$r.sqd.r <- (all.nox.emission$r)^2


all.nox.emission<- all.nox.emission %>% filter (SUM_OP_TIME>0 & !r=="NA")

write.fst(all.nox.emission, "data/all.nox.emission.2019.fst")

all.nox.emission <- read.fst("data/all.nox.emission.2019.fst")




#box plot consideing all percentage of operation
all.nox.emission %>% ggplot(aes(x=group, y= IOA, fill=group)) + geom_boxplot()  +
  labs(x="", y="Index of Agreement", title = "all percentage of operation in 2020") +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
    geom="text",
    fun = function(y) boxplot.stats(y)$stats,
    position=position_nudge(x=0.45),
    size=3.5) + theme_bw() +theme(legend.position = "none")


all.nox.emission %>% ggplot(aes(x=group, y= NMGE, fill=group)) + geom_boxplot() +
  scale_y_continuous(trans = 'log10',labels = scales::number_format(accuracy = 0.01))+
  labs(x="", y="Normalized Mean Gross Error", title = "all percentage of operation in 2020") +
   theme_bw() +theme(legend.position = "none")

all.nox.emission %>% ggplot(aes(x=group, y= NMB, fill=group)) + geom_boxplot() +
   labs(x="", y="Normalized Mean Bias", title = "all percentage of operation in 2020")


all.nox.emission %>% ggplot(aes(x=group, y= RMSE, fill=group)) + geom_boxplot() +
  scale_y_continuous(trans = 'log10', labels = scales::number_format(accuracy = 0.01)) +
  labs(x="", y="RMSE (NOx tons/week)", title = "all percentage of operation in 2020") +
  theme_bw() +theme(legend.position = "none")

all.nox.emission %>% ggplot(aes(x=group, y= MB, fill=group)) + geom_boxplot() +
  labs(x="", y="MB", title = "all percentage of operation in 2020") +
   theme_bw() +theme(legend.position = "none")

all.nox.emission %>% ggplot(aes(x=group, y= MGE, fill=group)) + geom_boxplot() +
  scale_y_continuous(trans = 'log10', labels = scales::number_format(accuracy = 0.01)) +
  labs(x="", y="MGE (NOx tons/week)", title = "all percentage of operation in 2020") +
  theme_bw() +theme(legend.position = "none")

all.nox.emission %>% ggplot(aes(x=group, y= FAC2, fill=group)) + geom_boxplot() +
  labs(x="", y="Fractional Bias", title = "facilities operating >75% in 2020") +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.5),
               size=3.5) + theme_bw() +theme(legend.position = "none")

all.nox.emission %>% ggplot(aes(x=group, y= r, fill=group)) + geom_boxplot() +
  labs(x="", y="Pearson R", title = "facilities operating >75% in 2020") +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.5),
               size=3.5) + theme_bw() +theme(legend.position = "none")

# #box plot consideing more than 75% operation in 2020
#
# all.nox.emission.75 <- all.nox.emission %>%  filter(SUM_OP_TIME >=0.5*max(all.nox.emission$SUM_OP_TIME)
#                                                     & SUM_OP_TIME <= 0.75*max(all.nox.emission$SUM_OP_TIME))
# all.nox.emission.75 %>% ggplot(aes(x=group, y= IOA, fill=group)) + geom_boxplot()  +
#   labs(x="", y="Index of Agreement", title = "facilities operating >75% in 2020") +
#   stat_summary(aes(label=sprintf("%1.1f", ..y..),),
#                geom="text",
#                fun = function(y) boxplot.stats(y)$stats,
#                position=position_nudge(x=0.45),
#                size=3.5) + theme_bw() +theme(legend.position = "none")
#
# all.nox.emission.75 %>%  ggplot(aes(x=group, y= NMGE, fill=group)) + geom_boxplot() +
#   labs(x="", y="Normalized Mean Gross Error", title = "facilities operating >75% in 2020")   +
#   scale_y_continuous(trans = 'log10',labels = scales::number_format(accuracy = 0.01))
#
# all.nox.emission.75 %>% ggplot(aes(x=group, y= NMB, fill=group)) + geom_boxplot()  +
#   stat_summary(aes(label=sprintf("%1.1f", ..y..),),
#                geom="text",
#                fun = function(y) boxplot.stats(y)$stats,
#                position=position_nudge(x=0.45),
#                size=3.5) + theme_bw() +theme(legend.position = "none")
# all.nox.emission.75 %>% ggplot(aes(x=group, y= RMSE, fill=group)) + geom_boxplot() +
#   scale_y_continuous(trans = 'log10', labels = scales::number_format(accuracy = 0.01)) +
#   labs(x="", y="RMSE (NOx tons/week)", title = "facilities operating >75% in 2020") +
#    theme_bw() +theme(legend.position = "none")
# all.nox.emission.75 %>% ggplot(aes(x=group, y= MB, fill=group)) + geom_boxplot() +
#   labs(x="", y="MB", title = "facilities operating >75% in 2020") +
#   stat_summary(aes(label=sprintf("%1.1f", ..y..),),
#                geom="text",
#                fun = function(y) boxplot.stats(y)$stats,
#                position=position_nudge(x=0.45),
#                size=3.5) + theme_bw() +theme(legend.position = "none")
# all.nox.emission.75 %>% ggplot(aes(x=group, y= MGE, fill=group)) + geom_boxplot() +
#   scale_y_continuous(trans = 'log10', labels = scales::number_format(accuracy = 0.01)) +
#   labs(x="", y="Mean Gross Error (NOx tons/week)", title = "facilities operating >75% in 2020") +
#    theme_bw() +theme(legend.position = "none")
#
# all.nox.emission.75 %>% ggplot(aes(x=group, y= FAC2, fill=group)) + geom_boxplot() +
#   labs(x="", y="Fractional Bias", title = "facilities operating >75% in 2020") +
#   stat_summary(aes(label=sprintf("%1.1f", ..y..),),
#                geom="text",
#                fun = function(y) boxplot.stats(y)$stats,
#                position=position_nudge(x=0.5),
#                size=3.5) + theme_bw() +theme(legend.position = "none")
#
# all.nox.emission.75 %>% ggplot(aes(x=group, y= r, fill=group)) + geom_boxplot() +
#   labs(x="", y="Pearson R", title = "facilities operating >75% in 2020") +
#   stat_summary(aes(label=sprintf("%1.1f", ..y..),),
#                geom="text",
#                fun = function(y) boxplot.stats(y)$stats,
#                position=position_nudge(x=0.5),
#                size=3.5) + theme_bw() +theme(legend.position = "none")

#box plot consideing less than 25% operation
#
# all.nox.emission.25 <- all.nox.emission %>%  filter(SUM_OP_TIME <0.25*max(all.nox.emission$SUM_OP_TIME))
# all.nox.emission.25 %>% ggplot(aes(x=group, y= IOA, fill=group)) + geom_boxplot()
# all.nox.emission.25 %>% ggplot(aes(x=group, y= r.sqd.r, fill=group)) + geom_boxplot() +labs(y="R squared")
# all.nox.emission.25 %>% ggplot(aes(x=group, y= NMGE, fill=group)) + geom_boxplot() + scale_y_continuous(trans = 'log10',
#                                                                                                         labels = scales::number_format(accuracy = 0.01))
# all.nox.emission.25 %>% ggplot(aes(x=group, y= NMB, fill=group)) + geom_boxplot()
# all.nox.emission.25 %>% ggplot(aes(x=group, y= RMSE, fill=group)) + geom_boxplot() +
#   scale_y_continuous(trans = 'log10', labels = scales::number_format(accuracy = 0.01))
# all.nox.emission.25 %>% ggplot(aes(x=group, y= MB, fill=group)) + geom_boxplot()
# all.nox.emission.25 %>% ggplot(aes(x=group, y= MGE, fill=group)) + geom_boxplot()+ scale_y_continuous(trans = 'log10',
#                                                                                                       labels = scales::number_format(accuracy = 0.01))
#
#
# #box plot consideing 25% to 50% operation
#
# all.nox.emission.25.50 <- all.nox.emission %>%  filter(SUM_OP_TIME >=0.25*max(all.nox.emission$SUM_OP_TIME)
#                                                        & SUM_OP_TIME < 0.50*max(all.nox.emission$SUM_OP_TIME))
# all.nox.emission.25.50 %>% ggplot(aes(x=group, y= IOA, fill=group)) + geom_boxplot()
# all.nox.emission.25.50 %>% ggplot(aes(x=group, y= r.sqd.r, fill=group)) + geom_boxplot()
# all.nox.emission.25.50 %>% ggplot(aes(x=group, y= NMGE, fill=group)) + geom_boxplot()
# all.nox.emission.25.50 %>% ggplot(aes(x=group, y= NMB, fill=group)) + geom_boxplot()
# all.nox.emission.25.50 %>% ggplot(aes(x=group, y= RMSE, fill=group)) + geom_boxplot() + scale_y_continuous(trans = 'log10',
#                                                                                                            labels = scales::number_format(accuracy = 0.01))
# all.nox.emission.25.50 %>% ggplot(aes(x=group, y= MB, fill=group)) + geom_boxplot()
# all.nox.emission.25.50 %>% ggplot(aes(x=group, y= MGE, fill=group)) + geom_boxplot() + scale_y_continuous(trans = 'log10',
#                                                                                                           labels = scales::number_format(accuracy = 0.01))
#
#
# #box plot consideing 50% to 75% operation
#
# all.nox.emission.50.75 <- all.nox.emission %>%  filter(SUM_OP_TIME >=0.5*max(all.nox.emission$SUM_OP_TIME)
#                                                     & SUM_OP_TIME <= 0.75*max(all.nox.emission$SUM_OP_TIME))
# all.nox.emission.50.75 %>% ggplot(aes(x=group, y= IOA, fill=group)) + geom_boxplot()
# all.nox.emission.50.75 %>% ggplot(aes(x=group, y= r.sqd.r, fill=group)) + geom_boxplot()
# all.nox.emission.50.75 %>% ggplot(aes(x=group, y= NMGE, fill=group)) + geom_boxplot() + scale_y_continuous(trans = 'log10',
#                                                                                                            labels = scales::number_format(accuracy = 0.01))
# all.nox.emission.50.75 %>% ggplot(aes(x=group, y= NMB, fill=group)) + geom_boxplot()
# all.nox.emission.50.75 %>% ggplot(aes(x=group, y= RMSE, fill=group)) + geom_boxplot() + scale_y_continuous(trans = 'log10',
#                                                                                                            labels = scales::number_format(accuracy = 0.01))
# all.nox.emission.50.75 %>% ggplot(aes(x=group, y= MB, fill=group)) + geom_boxplot()
# all.nox.emission.50.75 %>% ggplot(aes(x=group, y= MGE, fill=group)) + geom_boxplot() + scale_y_continuous(trans = 'log10',
#                                                                                                           labels = scales::number_format(accuracy = 0.01))



# ggsave("R.sq.2019_NOx_9_box.png", path = "./plots/")

# all.nox.emission %>% ggplot(aes(x=MGE, fill=group)) +
#   geom_histogram(binwidth = 1, alpha=0.5, position = 'identity') +
#   labs(x= "NMGE",   y = "Number of facilities",
#        title = "") + theme(legend.position = c(0.8, 0.8)) +
#   guides(fill=guide_legend(title="Model")) +
#   scale_x_continuous(trans = 'log10', labels = scales::number_format(accuracy = 0.01))

# ggsave("NMGE2019_NOx_3.png", path = "./plots/")
