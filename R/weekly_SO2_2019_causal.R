rm(list = ls())

library(gsynth)
library(fst)
library(data.table)
library(tidyverse)
# library(parallel)
# library(sf)
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
library(hrbrthemes)
library(viridis)
library(cowplot)

# setwd ("/projects/HAQ_LAB/mrasel/R/causal-study-COVID-beyond")

setwd ("/Volumes/GoogleDrive/My Drive/R/causal-study-COVID-beyond")

#getting all electric facilities in the US
ampd_daily_all_units<- read.fst ("data/ampd_daily_cleaned_facility.fst")


ampd_daily_select <- ampd_daily_all_units %>% dplyr::select(ORISPL_CODE, date, STATE, UNITID, ID,
                                                            year, month, day, SO2..tons., NOx..tons., pr, tmmx, rmax, vs, th,
                                                            Fuel.Type..Primary., COUNT_OP_TIME, SUM_OP_TIME)

ampd_daily_select <- ampd_daily_select 
facility <- as.vector(unique(ampd_daily_select$ORISPL_CODE))


# ampd_daily_select$SUM_OP_TIME[is.na(ampd_daily_select$SUM_OP_TIME)] <-  0

#operation in 2020
# 
# operation.pct.facility <- function(facility.name) {
#   ampd_daily_all_unit <- ampd_daily_select %>% filter (ORISPL_CODE %in% facility.name & year==2019)
# 
#   ampd_daily_all_unit.op <- ampd_daily_all_unit %>% mutate(op.pct =(length(ampd_daily_all_unit$NOx..tons. [ampd_daily_all_unit$NOx..tons. >0])/
#                                                                       length(ampd_daily_all_unit$NOx..tons. ))*100)
#   ampd_daily_all_unit <- ampd_daily_all_unit.op
#   return(ampd_daily_all_unit)
# }
# 
# op <- lapply(facility, operation.pct.facility )
# 
# operation <- as.data.frame(matrix(nrow = 1048, ncol =17))
# 
# for (i in 1:length(facility)) {
#   operation [i, ] <- as.data.frame((op[i]))
# }
# 
# #taking facilities opearating more than 25% in 2019
# 
# operation <- operation %>%  filter (V17>25)
# ID_operating_25pct <- as.vector(unique(operation$V5))
# 
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

ampd_weekly <- ampd_daily_to_weekly %>% filter (year <=2019 & th>0) #wind direction should not be negative


ampd_daily_all_units <- ampd_weekly
facility <- as.vector(unique(ampd_daily_all_units$ORISPL_CODE))

# save(facility, file="data/facility_so2_2019.RData")
load ("data/facility_so2_2019.RData")
# 
# fileConn<-file("output.txt")
# 
# gsynth.fn <- function(facility.name) {
#   ampd_daily_all_unit <- ampd_daily_all_units %>% filter (ORISPL_CODE %in% facility.name )
#   all_ampd <- ampd_daily_all_unit
#   id_selected <-  unique( ampd_daily_all_unit$ID)
#   all_ampd <-  all_ampd %>%  dplyr::select (STATE, year, isoweek, ORISPL_CODE, ID, SO2..tons.,
#                                             pr, tmmx, rmax, vs, th)
#   # all_ampd$SO2..tons.[is.na(all_ampd$SO2..tons.)] <-  0
# 
#   all_ampd <- as.data.table(all_ampd)
# 
#   #setting intervention
#   all_ampd <- all_ampd[, inputed := 0]
#   all_ampd$inputed[all_ampd$year==2019 & all_ampd$isoweek>8 &  all_ampd$ID %in% id_selected] <- 1
# 
#   all_ampd <- all_ampd [, id := paste(year, ID, sep = "_")]
# 
#   all_ampd_final <- all_ampd%>% dplyr::select (year, isoweek, id, SO2..tons.,  inputed,
#                                                pr, tmmx, rmax, vs, th)
# 
# 
#   synth_eq <- SO2..tons. ~ inputed+ tmmx + rmax + pr + th + vs
# 
#   paste("Facility now running", facility.name, unique(ampd_daily_all_unit$STATE), sep= " ")
# 
#   # synth_eq <- SO2..tons. ~ inputed+  tmmx + rmax + pr + th + vs + day.id
#   tryCatch({
#     #Running gsynth
#     out <<- gsynth(synth_eq,
#                    data=all_ampd_final,
#                    index = c("id","isoweek"), na.rm=T, force = "two-way", se=TRUE,
#                    CV = TRUE, seed =  123, estimator = "mc", parallel=T,
#                     inference = "nonparametric")
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
#             scale_x_continuous(breaks = seq(0, 52, by = 5)) +  labs(x= "weeks" ,y = "") +
#       theme(legend.position = "none") 
#     # counterfactual_plots_att <- plot(out, main=paste(facility.name)) +
#     #   scale_x_continuous(breaks = seq(0, 120, by = 10)) +
#     #   theme(axis.text.x = element_text(angle = 90))
# 
#     # all <- list(stats, out, counterfactual_plots, counterfactual_plots_att)
#     all <- list(stats, out, counterfactual_plots)
#   }, error = function(e) {
#     writeLines(paste0("at facility ", facility.name, " something caused error "), fileConn)
#   })
#   return(all)
# }
# result_so2_weekly_2019 <- lapply(facility, gsynth.fn)
# close(fileConn)
# 
# 
# save(result_so2_weekly_2019, file="data/result_so2_weekly_2019.RData")

load ("data/result_so2_weekly_2019.RData")
load ("data/facility_so2_2019.RData")



# getting counter factual plots

counterfactual_plots <- list()
for (i in 1: length(facility)) {
  counterfactual_plots[[i]] <- result_so2_weekly_2019 [[i]][[3]]
}

layout <- rbind(c(1,2,3), c(4,5,6), c(7,8,9))
pdf(file = 'plots/counterfactual_plots_so2_2019.pdf', onefile = TRUE, paper = 'A4',
    width = 9, height = 9, pointsize = 1)

marrangeGrob(grobs = counterfactual_plots, ncol = 3, nrow = 3, layout_matrix = layout)
dev.off()



#getting 2019 So2 ATT, actual and counterfactual emissions
st = list()

for (i in 1:length(facility)) {
  fac.1 <- as.data.frame(facility[i])
  att.avg <-result_so2_weekly_2019[[i]][[2]]$est.att
  week <- 1:nrow(att.avg)
  st[[i]] <- cbind(fac.1,att.avg, week)
}

st = do.call(rbind, st)

names(st)[names(st) == 'facility[i]'] <- 'facility'



ampd_facility<- read.fst ("data/ampd_daily_cleaned_facility.fst")

ampd_facility <- ampd_facility %>% filter (ORISPL_CODE %in% facility & year==2019)

ampd_facility <- ampd_facility %>%  dplyr::select ( STATE, ORISPL_CODE, Fuel.Type..Primary.,
                                                    Facility.Latitude, Facility.Longitude,
                                                    County, County.Code, FIPS.Code, Source.Category, Unit.Type,
                                                    SO2.Control.s., NOx.Control.s.,PM.Control.s., Hg.Control.s. )


ampd_facility <- distinct(ampd_facility, .keep_all = T)
names(ampd_facility)[names(ampd_facility) == 'ORISPL_CODE'] <- 'facility'

all.facility.so2.2019 <- merge (st, ampd_facility, by = "facility")


# getting each units actual emission into a dataframe====================================================
datalist = list()

for (i in 1:length(facility)) {
  so2.emis <- na.omit(as.data.frame(result_so2_weekly_2019[[i]][[2]]$Y.bar))
  so2.emis <- so2.emis %>% dplyr::select(-Y.co.bar)
  so2.emis$week <- c(1:nrow(so2.emis)) #leap year 2019
  fac.1 <- as.data.frame(facility[i])
  datalist[[i]] <- cbind(fac.1,so2.emis) # add it to your list
}

ac.ct.so2.emission = do.call(rbind, datalist)

names(ac.ct.so2.emission)[names(ac.ct.so2.emission) == 'facility[i]'] <- 'facility'

all.facility.so2.2019 <- merge (all.facility.so2.2019, ac.ct.so2.emission, by = c( "facility", "week" ))

write.fst(all.facility.so2.2019, "data/all.facility.so2.2019.fst")


# ===============================================================================================
#                                           Model evaluation
# ===============================================================================================
# 
# result_so2_weekly_2019_test <- list()
# facility_test <- list()
# 
# for (i in 1:length(result_so2_weekly_2019)){
#   sublist = result_so2_weekly_2019[[i]]
#   if (length(sublist) == 3)
#   {
#     result_so2_weekly_2019_test[[i]] <-result_so2_weekly_2019[[i]] # this does achieve the desired result
#     facility_test[[i]] <- facility[[i]]
#   }
#   else
#   {
#     result_so2_weekly_2019_test[[i]] <- NULL # this does achieve the desired result
#   }
# 
# }
# 
# result_so2_weekly_2019_test2 <- result_so2_weekly_2019_test[-which(sapply(result_so2_weekly_2019_test, is.null))]
# 
# result_so2_weekly_2019 <- result_so2_weekly_2019_test2
# facility <- unlist(facility_test, recursive = TRUE, use.names = TRUE)

# getting each units actual emission into a dataframe====================================================
datalist = list()

for (i in 1:length(facility)) {
  so2.emis <- na.omit(as.data.frame(result_so2_weekly_2019[[i]][[2]]$Y.bar))
  so2.emis <- so2.emis %>% dplyr::select(-Y.co.bar)
  so2.emis$week <- c(1:52)
  so2.emis <- so2.emis %>% filter(week>=9)
  stats <- as.data.table(modStats(so2.emis, mod = "Y.ct.bar", obs = "Y.tr.bar"))
  fac.1 <- as.data.frame(facility[i])
  datalist[[i]] <- cbind(fac.1,stats) # add it to your list
}

ac.ct.so2.emission = do.call(rbind, datalist)

names(ac.ct.so2.emission)[names(ac.ct.so2.emission) == 'facility[i]'] <- 'facility'

#adding Operation hours of 2019

fac.op <- setDT(ampd_weekly)[, .(SUM_OP_TIME=sum(SUM_OP_TIME, na.rm=T)),
                             by = .( ORISPL_CODE,  year)]
fac.op.2019 <- fac.op %>%  filter (year==2019)

fac.op.2019 <- fac.op.2019 %>% filter (ORISPL_CODE %in% facility)

names(fac.op.2019)[names(fac.op.2019) == 'ORISPL_CODE'] <- 'facility'
ac.ct.so2.emission <- merge(ac.ct.so2.emission, fac.op.2019 , by= c("facility" ))

# ac.ct.so2.emission$r.sqd <- (ac.ct.so2.emission$r)^2

ac.ct.so2.emission <- ac.ct.so2.emission #%>%  filter (SUM_OP_TIME>0)

summary(ac.ct.so2.emission)

ac.ct.so2.emission %>%  ggplot(aes(  SUM_OP_TIME, NMGE )) +
  geom_point() + labs(x= "Operation time (hr)", y = "", title = " ")

ac.ct.so2.emission %>%  ggplot(aes(  SUM_OP_TIME, NMB )) +
  geom_point() + labs(x= "Operation time (hr)", y = "", title = " ")


ac.ct.so2.emission %>%
  ggplot(aes(RMSE)) +
  geom_histogram(binwidth = 1, color = "black")


#issue with SO2 emissions data
#some facilities operated long however there was no SO2 emissions from them. facility 9 for example.

# ===============================simple average model evaluation=====================

#calculating 9 year, 7 year, 5 year, 3 year average. these values are counterfactual emissions
#2019 emissions are actual emissions
#We're taking all facilities operating since 2010 to 2019
#if a facility before 2019 got retired, we're placing them on 2019 records as 0 emissions with 0 operation time



ampd_daily_emissions <- read.fst ("/Volumes/GoogleDrive/My Drive/R/ampd-raw-data-processing/data/ampd_daily_emission.fst")


ampd_daily_to_weekly_9 <- setDT(ampd_daily_emissions)[, .(SO2..tons. = sum(SO2..tons., na.rm=TRUE),
                                                          NOx..tons. = sum(NOx..tons., na.rm=TRUE),
                                                          SUM_OP_TIME=sum(SUM_OP_TIME, na.rm=T)),
                                                      by = .(STATE, ORISPL_CODE, ID, year,
                                                             isoweek(date))]
#2010-2018
ampd_daily_emissions_2010_2018 <- ampd_daily_to_weekly_9  %>%
  filter (year %in% c( 2010:2018) &
            isoweek >=9) %>% dplyr::select(ORISPL_CODE,  isoweek, SO2..tons., SUM_OP_TIME)

ampd_daily_emissions_2019 <- ampd_daily_to_weekly_9  %>%
  filter (year %in% c( 2019) &
            isoweek >=9) %>% dplyr::select(ORISPL_CODE,  isoweek, SO2..tons.,SUM_OP_TIME)

ampd_daily_emissions_2010_2018 <- setDT(ampd_daily_emissions_2010_2018)[ , 
                                                                         .(SO2..tons. = mean(SO2..tons., na.rm=TRUE),
                                                                           SUM_OP_TIME=mean(SUM_OP_TIME, na.rm=TRUE)),
                                                                         by = .(ORISPL_CODE, isoweek)]

ampd_daily_emissions_2019<- setDT(ampd_daily_emissions_2019)[, .(
  SO2..tons. = mean(SO2..tons., na.rm=TRUE),
  SUM_OP_TIME=mean(SUM_OP_TIME, na.rm=TRUE)),
  by = .(ORISPL_CODE, isoweek)]

facility.2010.2018 <- as.vector(unique(ampd_daily_emissions_2010_2018$ORISPL_CODE))
facility.2019 <- as.vector(unique(ampd_daily_emissions_2019$ORISPL_CODE))

# facility.9yr <- intersect(facility.9, facility.2019)
# ampd_daily_emissions_2010_2018_9_52 <- ampd_daily_emissions_2010_2018_9_52 %>%
#   filter (ORISPL_CODE %in% facility )

fac.not.2019 <- setdiff(facility.2010.2018,facility.2019)

#putting 0 emissions to the facilities that were not operating on 2019
datalist= list()
for (i in 1:length(fac.not.2019)) {
  isoweek <- c(1:52)
  ORISPL_CODE <- fac.not.2019[i]
  SO2..tons. <- 0
  SUM_OP_TIME <- 0
  df <- data.frame(ORISPL_CODE,isoweek,SO2..tons.,SUM_OP_TIME)
  datalist[[i]] <- df
  
}

hypo <- do.call(rbind, datalist)

ampd_daily_emissions_2019<- rbind(ampd_daily_emissions_2019,hypo) 

facility.2019 <- as.vector(unique(ampd_daily_emissions_2019$ORISPL_CODE))


#actual vs counterfactual statistics
datalist = list()

for (i in 1:length(facility.2010.2018)) {
  ampd_daily_all_unit.2010.2018 <- ampd_daily_emissions_2010_2018 %>% filter (ORISPL_CODE %in% facility.2010.2018[i] )
  names(ampd_daily_all_unit.2010.2018)[names(ampd_daily_all_unit.2010.2018) == 'SO2..tons.'] <- 'ct.so2.tons' #average of previous years
  avg.so2.emis <- ampd_daily_all_unit.2010.2018
  names(avg.so2.emis)[names(avg.so2.emis) == 'isoweek'] <- 'week'
  avg.so2.emis$week <- as.character(avg.so2.emis$week)
  
  ampd_daily_all_unit.2019 <- ampd_daily_emissions_2019 %>% filter (ORISPL_CODE %in% facility.2010.2018[i] )
  names(ampd_daily_all_unit.2019)[names(ampd_daily_all_unit.2019) == 'SO2..tons.'] <- 'ac.so2.tons' #average of previous years
  so2.emis <- ampd_daily_all_unit.2019
  names(so2.emis)[names(so2.emis) == 'isoweek'] <- 'week'
  so2.emis$week <- as.character(so2.emis$week)
  
  
  so2.emis.linear <- merge(avg.so2.emis, so2.emis, by= c("ORISPL_CODE", "week"))
  stats <- as.data.table(modStats(so2.emis.linear, mod = "ct.so2.tons", obs = "ac.so2.tons"))
  fac.1 <- as.data.frame(facility.2010.2018[i])
  datalist[[i]] <- cbind(fac.1,stats) # add it to your list
}

sim.avg.ac.ct.so2.emission.9 = do.call(rbind, datalist)

names(sim.avg.ac.ct.so2.emission.9)[names(sim.avg.ac.ct.so2.emission.9) == 'facility.2010.2018[i]'] <- 'facility'

fac.op.time.sim <- ampd_daily_emissions_2019
fac.op.2019.sim <- fac.op.time.sim %>%  filter (ORISPL_CODE %in% facility.2010.2018)

fac.op.2019.sim <- setDT(fac.op.2019.sim)[, .(SUM_OP_TIME=sum(SUM_OP_TIME, na.rm=T)),
                                          by = .(ORISPL_CODE)]

names(fac.op.2019.sim)[names(fac.op.2019.sim) == 'ORISPL_CODE'] <- 'facility'

sim.avg.ac.ct.so2.emission.9 <- merge(sim.avg.ac.ct.so2.emission.9, fac.op.2019.sim , by= c("facility" ))
sim.avg.ac.ct.so2.emission.9$year <- 2019


sim.avg.ac.ct.so2.emission.9 <- sim.avg.ac.ct.so2.emission.9  #2010:2018 simple average vs 2019 emission



# =========2012 to 2018 average
ampd_daily_emissions_2012_2018 <- ampd_daily_to_weekly_9  %>%
  filter (year %in% c( 2012:2018) &
            isoweek >=9) %>% dplyr::select(ORISPL_CODE,  isoweek, SO2..tons., SUM_OP_TIME)

ampd_daily_emissions_2019 <- ampd_daily_to_weekly_9  %>%
  filter (year %in% c( 2019) &
            isoweek >=9) %>% dplyr::select(ORISPL_CODE,  isoweek, SO2..tons.,SUM_OP_TIME)

ampd_daily_emissions_2012_2018 <- setDT(ampd_daily_emissions_2012_2018)[ , 
                                                                         .(SO2..tons. = mean(SO2..tons., na.rm=TRUE),
                                                                           SUM_OP_TIME=mean(SUM_OP_TIME, na.rm=TRUE)),
                                                                         by = .(ORISPL_CODE, isoweek)]

ampd_daily_emissions_2019<- setDT(ampd_daily_emissions_2019)[, .(
  SO2..tons. = mean(SO2..tons., na.rm=TRUE),
  SUM_OP_TIME=mean(SUM_OP_TIME, na.rm=TRUE)),
  by = .(ORISPL_CODE, isoweek)]

facility.2012.2018 <- as.vector(unique(ampd_daily_emissions_2012_2018$ORISPL_CODE))
facility.2019 <- as.vector(unique(ampd_daily_emissions_2019$ORISPL_CODE))

# facility.9yr <- intersect(facility.9, facility.2019)
# ampd_daily_emissions_2012_2018_9_52 <- ampd_daily_emissions_2012_2018_9_52 %>%
#   filter (ORISPL_CODE %in% facility )

fac.not.2019 <- setdiff(facility.2012.2018,facility.2019)

#putting 0 emissions to the facilities that were not operating on 2019
datalist= list()
for (i in 1:length(fac.not.2019)) {
  isoweek <- c(1:52)
  ORISPL_CODE <- fac.not.2019[i]
  SO2..tons. <- 0
  SUM_OP_TIME <- 0
  df <- data.frame(ORISPL_CODE,isoweek,SO2..tons.,SUM_OP_TIME)
  datalist[[i]] <- df
  
}

hypo <- do.call(rbind, datalist)

ampd_daily_emissions_2019<- rbind(ampd_daily_emissions_2019,hypo) 

facility.2019 <- as.vector(unique(ampd_daily_emissions_2019$ORISPL_CODE))


#actual vs counterfactual statistics
datalist = list()

for (i in 1:length(facility.2012.2018)) {
  ampd_daily_all_unit.2012.2018 <- ampd_daily_emissions_2012_2018 %>% filter (ORISPL_CODE %in% facility.2012.2018[i] )
  names(ampd_daily_all_unit.2012.2018)[names(ampd_daily_all_unit.2012.2018) == 'SO2..tons.'] <- 'ct.so2.tons' #average of previous years
  avg.so2.emis <- ampd_daily_all_unit.2012.2018
  names(avg.so2.emis)[names(avg.so2.emis) == 'isoweek'] <- 'week'
  avg.so2.emis$week <- as.character(avg.so2.emis$week)
  
  ampd_daily_all_unit.2019 <- ampd_daily_emissions_2019 %>% filter (ORISPL_CODE %in% facility.2012.2018[i] )
  names(ampd_daily_all_unit.2019)[names(ampd_daily_all_unit.2019) == 'SO2..tons.'] <- 'ac.so2.tons' #average of previous years
  so2.emis <- ampd_daily_all_unit.2019
  names(so2.emis)[names(so2.emis) == 'isoweek'] <- 'week'
  so2.emis$week <- as.character(so2.emis$week)
  
  
  so2.emis.linear <- merge(avg.so2.emis, so2.emis, by= c("ORISPL_CODE", "week"))
  stats <- as.data.table(modStats(so2.emis.linear, mod = "ct.so2.tons", obs = "ac.so2.tons"))
  fac.1 <- as.data.frame(facility.2012.2018[i])
  datalist[[i]] <- cbind(fac.1,stats) # add it to your list
}

sim.avg.ac.ct.so2.emission.7 = do.call(rbind, datalist)

names(sim.avg.ac.ct.so2.emission.7)[names(sim.avg.ac.ct.so2.emission.7) == 'facility.2012.2018[i]'] <- 'facility'

fac.op.time.sim <- ampd_daily_emissions_2019
fac.op.2019.sim <- fac.op.time.sim %>%  filter (ORISPL_CODE %in% facility.2012.2018)

fac.op.2019.sim <- setDT(fac.op.2019.sim)[, .(SUM_OP_TIME=sum(SUM_OP_TIME, na.rm=T)),
                                          by = .( ORISPL_CODE)]

names(fac.op.2019.sim)[names(fac.op.2019.sim) == 'ORISPL_CODE'] <- 'facility'

sim.avg.ac.ct.so2.emission.7 <- merge(sim.avg.ac.ct.so2.emission.7, fac.op.2019.sim , by= c("facility" ))
sim.avg.ac.ct.so2.emission.7$year <- 2019


sim.avg.ac.ct.so2.emission.7 <- sim.avg.ac.ct.so2.emission.7  #2012:2018 simple average vs 2019 emission





# =========2014 to 2018 average

ampd_daily_emissions_2014_2018 <- ampd_daily_to_weekly_9  %>%
  filter (year %in% c( 2014:2018) &
            isoweek >=9) %>% dplyr::select(ORISPL_CODE,  isoweek, SO2..tons., SUM_OP_TIME)

ampd_daily_emissions_2019 <- ampd_daily_to_weekly_9  %>%
  filter (year %in% c( 2019) &
            isoweek >=9) %>% dplyr::select(ORISPL_CODE,  isoweek, SO2..tons.,SUM_OP_TIME)

ampd_daily_emissions_2014_2018 <- setDT(ampd_daily_emissions_2014_2018)[ , 
                                                                         .(SO2..tons. = mean(SO2..tons., na.rm=TRUE),
                                                                           SUM_OP_TIME=mean(SUM_OP_TIME, na.rm=TRUE)),
                                                                         by = .(ORISPL_CODE, isoweek)]

ampd_daily_emissions_2019<- setDT(ampd_daily_emissions_2019)[, .(
  SO2..tons. = mean(SO2..tons., na.rm=TRUE),
  SUM_OP_TIME=mean(SUM_OP_TIME, na.rm=TRUE)),
  by = .(ORISPL_CODE, isoweek)]

facility.2014.2018 <- as.vector(unique(ampd_daily_emissions_2014_2018$ORISPL_CODE))
facility.2019 <- as.vector(unique(ampd_daily_emissions_2019$ORISPL_CODE))

# facility.9yr <- intersect(facility.9, facility.2019)
# ampd_daily_emissions_2014_2018_9_52 <- ampd_daily_emissions_2014_2018_9_52 %>%
#   filter (ORISPL_CODE %in% facility )

fac.not.2019 <- setdiff(facility.2014.2018,facility.2019)

#putting 0 emissions to the facilities that were not operating on 2019
datalist= list()
for (i in 1:length(fac.not.2019)) {
  isoweek <- c(1:52)
  ORISPL_CODE <- fac.not.2019[i]
  SO2..tons. <- 0
  SUM_OP_TIME <- 0
  df <- data.frame(ORISPL_CODE,isoweek,SO2..tons.,SUM_OP_TIME)
  datalist[[i]] <- df
  
}

hypo <- do.call(rbind, datalist)

ampd_daily_emissions_2019<- rbind(ampd_daily_emissions_2019,hypo) 

facility.2019 <- as.vector(unique(ampd_daily_emissions_2019$ORISPL_CODE))


#actual vs counterfactual statistics
datalist = list()

for (i in 1:length(facility.2014.2018)) {
  ampd_daily_all_unit.2014.2018 <- ampd_daily_emissions_2014_2018 %>% filter (ORISPL_CODE %in% facility.2014.2018[i] )
  names(ampd_daily_all_unit.2014.2018)[names(ampd_daily_all_unit.2014.2018) == 'SO2..tons.'] <- 'ct.so2.tons' #average of previous years
  avg.so2.emis <- ampd_daily_all_unit.2014.2018
  names(avg.so2.emis)[names(avg.so2.emis) == 'isoweek'] <- 'week'
  avg.so2.emis$week <- as.character(avg.so2.emis$week)
  
  ampd_daily_all_unit.2019 <- ampd_daily_emissions_2019 %>% filter (ORISPL_CODE %in% facility.2014.2018[i] )
  names(ampd_daily_all_unit.2019)[names(ampd_daily_all_unit.2019) == 'SO2..tons.'] <- 'ac.so2.tons' #average of previous years
  so2.emis <- ampd_daily_all_unit.2019
  names(so2.emis)[names(so2.emis) == 'isoweek'] <- 'week'
  so2.emis$week <- as.character(so2.emis$week)
  
  
  so2.emis.linear <- merge(avg.so2.emis, so2.emis, by= c("ORISPL_CODE", "week"))
  stats <- as.data.table(modStats(so2.emis.linear, mod = "ct.so2.tons", obs = "ac.so2.tons"))
  fac.1 <- as.data.frame(facility.2014.2018[i])
  datalist[[i]] <- cbind(fac.1,stats) # add it to your list
}

sim.avg.ac.ct.so2.emission.5 = do.call(rbind, datalist)

names(sim.avg.ac.ct.so2.emission.5)[names(sim.avg.ac.ct.so2.emission.5) == 'facility.2014.2018[i]'] <- 'facility'

fac.op.time.sim <- ampd_daily_emissions_2019
fac.op.2019.sim <- fac.op.time.sim %>%  filter (ORISPL_CODE %in% facility.2014.2018)

fac.op.2019.sim <- setDT(fac.op.2019.sim)[, .(SUM_OP_TIME=sum(SUM_OP_TIME, na.rm=T)),
                                          by = .( ORISPL_CODE)]

names(fac.op.2019.sim)[names(fac.op.2019.sim) == 'ORISPL_CODE'] <- 'facility'


sim.avg.ac.ct.so2.emission.5 <- merge(sim.avg.ac.ct.so2.emission.5, fac.op.2019.sim , by= c("facility" ))
sim.avg.ac.ct.so2.emission.5$year <- 2019


sim.avg.ac.ct.so2.emission.5 <- sim.avg.ac.ct.so2.emission.5  #2014:2018 simple average vs 2019 emission




# =========2016 to 2018 average
ampd_daily_emissions_2016_2018 <- ampd_daily_to_weekly_9  %>%
  filter (year %in% c( 2016:2018) &
            isoweek >=9) %>% dplyr::select(ORISPL_CODE,  isoweek, SO2..tons., SUM_OP_TIME)

ampd_daily_emissions_2019 <- ampd_daily_to_weekly_9  %>%
  filter (year %in% c( 2019) &
            isoweek >=9) %>% dplyr::select(ORISPL_CODE,  isoweek, SO2..tons.,SUM_OP_TIME)

ampd_daily_emissions_2016_2018 <- setDT(ampd_daily_emissions_2016_2018)[ , 
                                                                         .(SO2..tons. = mean(SO2..tons., na.rm=TRUE),
                                                                           SUM_OP_TIME=mean(SUM_OP_TIME, na.rm=TRUE)),
                                                                         by = .(ORISPL_CODE, isoweek)]

ampd_daily_emissions_2019<- setDT(ampd_daily_emissions_2019)[, .(
  SO2..tons. = mean(SO2..tons., na.rm=TRUE),
  SUM_OP_TIME=mean(SUM_OP_TIME, na.rm=TRUE)),
  by = .(ORISPL_CODE, isoweek)]

facility.2016.2018 <- as.vector(unique(ampd_daily_emissions_2016_2018$ORISPL_CODE))
facility.2019 <- as.vector(unique(ampd_daily_emissions_2019$ORISPL_CODE))

# facility.9yr <- intersect(facility.9, facility.2019)
# ampd_daily_emissions_2016_2018_9_52 <- ampd_daily_emissions_2016_2018_9_52 %>%
#   filter (ORISPL_CODE %in% facility )

fac.not.2019 <- setdiff(facility.2016.2018,facility.2019)

#putting 0 emissions to the facilities that were not operating on 2019
datalist= list()
for (i in 1:length(fac.not.2019)) {
  isoweek <- c(1:52)
  ORISPL_CODE <- fac.not.2019[i]
  SO2..tons. <- 0
  SUM_OP_TIME <- 0
  df <- data.frame(ORISPL_CODE,isoweek,SO2..tons.,SUM_OP_TIME)
  datalist[[i]] <- df
  
}

hypo <- do.call(rbind, datalist)

ampd_daily_emissions_2019<- rbind(ampd_daily_emissions_2019,hypo) 

facility.2019 <- as.vector(unique(ampd_daily_emissions_2019$ORISPL_CODE))


#actual vs counterfactual statistics
datalist = list()

for (i in 1:length(facility.2016.2018)) {
  ampd_daily_all_unit.2016.2018 <- ampd_daily_emissions_2016_2018 %>% filter (ORISPL_CODE %in% facility.2016.2018[i] )
  names(ampd_daily_all_unit.2016.2018)[names(ampd_daily_all_unit.2016.2018) == 'SO2..tons.'] <- 'ct.so2.tons' #average of previous years
  avg.so2.emis <- ampd_daily_all_unit.2016.2018
  names(avg.so2.emis)[names(avg.so2.emis) == 'isoweek'] <- 'week'
  avg.so2.emis$week <- as.character(avg.so2.emis$week)
  
  ampd_daily_all_unit.2019 <- ampd_daily_emissions_2019 %>% filter (ORISPL_CODE %in% facility.2016.2018[i] )
  names(ampd_daily_all_unit.2019)[names(ampd_daily_all_unit.2019) == 'SO2..tons.'] <- 'ac.so2.tons' #average of previous years
  so2.emis <- ampd_daily_all_unit.2019
  names(so2.emis)[names(so2.emis) == 'isoweek'] <- 'week'
  so2.emis$week <- as.character(so2.emis$week)
  
  
  so2.emis.linear <- merge(avg.so2.emis, so2.emis, by= c("ORISPL_CODE", "week"))
  stats <- as.data.table(modStats(so2.emis.linear, mod = "ct.so2.tons", obs = "ac.so2.tons"))
  fac.1 <- as.data.frame(facility.2016.2018[i])
  datalist[[i]] <- cbind(fac.1,stats) # add it to your list
}

sim.avg.ac.ct.so2.emission.3 = do.call(rbind, datalist)

names(sim.avg.ac.ct.so2.emission.3)[names(sim.avg.ac.ct.so2.emission.3) == 'facility.2016.2018[i]'] <- 'facility'

fac.op.time.sim <- ampd_daily_emissions_2019
fac.op.2019.sim <- fac.op.time.sim %>%  filter (ORISPL_CODE %in% facility.2016.2018)

fac.op.2019.sim <- setDT(fac.op.2019.sim)[, .(SUM_OP_TIME=sum(SUM_OP_TIME, na.rm=T)),
                                          by = .( ORISPL_CODE)]

names(fac.op.2019.sim)[names(fac.op.2019.sim) == 'ORISPL_CODE'] <- 'facility'


sim.avg.ac.ct.so2.emission.3 <- merge(sim.avg.ac.ct.so2.emission.3, fac.op.2019.sim , by= c("facility" ))
sim.avg.ac.ct.so2.emission.3$year <- 2019


sim.avg.ac.ct.so2.emission.3 <- sim.avg.ac.ct.so2.emission.3  #2016:2018 simple average vs 2019 emission





# =========2018 to 2018 average means 2018 vs 2019 emissions check

ampd_daily_emissions_2018_2018 <- ampd_daily_to_weekly_9  %>%
  filter (year %in% c( 2018:2018) &
            isoweek >=9) %>% dplyr::select(ORISPL_CODE,  isoweek, SO2..tons., SUM_OP_TIME)

ampd_daily_emissions_2019 <- ampd_daily_to_weekly_9  %>%
  filter (year %in% c( 2019) &
            isoweek >=9) %>% dplyr::select(ORISPL_CODE,  isoweek, SO2..tons.,SUM_OP_TIME)

ampd_daily_emissions_2018_2018 <- setDT(ampd_daily_emissions_2018_2018)[ , 
                                                                         .(SO2..tons. = mean(SO2..tons., na.rm=TRUE),
                                                                           SUM_OP_TIME=mean(SUM_OP_TIME, na.rm=TRUE)),
                                                                         by = .(ORISPL_CODE, isoweek)]

ampd_daily_emissions_2019<- setDT(ampd_daily_emissions_2019)[, .(
  SO2..tons. = mean(SO2..tons., na.rm=TRUE),
  SUM_OP_TIME=mean(SUM_OP_TIME, na.rm=TRUE)),
  by = .(ORISPL_CODE, isoweek)]

facility.2018.2018 <- as.vector(unique(ampd_daily_emissions_2018_2018$ORISPL_CODE))
facility.2019 <- as.vector(unique(ampd_daily_emissions_2019$ORISPL_CODE))

# facility.9yr <- intersect(facility.9, facility.2019)
# ampd_daily_emissions_2018_2018_9_52 <- ampd_daily_emissions_2018_2018_9_52 %>%
#   filter (ORISPL_CODE %in% facility )

fac.not.2019 <- setdiff(facility.2018.2018,facility.2019)

#putting 0 emissions to the facilities that were not operating on 2019
datalist= list()
for (i in 1:length(fac.not.2019)) {
  isoweek <- c(1:52)
  ORISPL_CODE <- fac.not.2019[i]
  SO2..tons. <- 0
  SUM_OP_TIME <- 0
  df <- data.frame(ORISPL_CODE,isoweek,SO2..tons.,SUM_OP_TIME)
  datalist[[i]] <- df
  
}

hypo <- do.call(rbind, datalist)

ampd_daily_emissions_2019<- rbind(ampd_daily_emissions_2019,hypo) 

facility.2019 <- as.vector(unique(ampd_daily_emissions_2019$ORISPL_CODE))


#actual vs counterfactual statistics
datalist = list()

for (i in 1:length(facility.2018.2018)) {
  ampd_daily_all_unit.2018.2018 <- ampd_daily_emissions_2018_2018 %>% filter (ORISPL_CODE %in% facility.2018.2018[i] )
  names(ampd_daily_all_unit.2018.2018)[names(ampd_daily_all_unit.2018.2018) == 'SO2..tons.'] <- 'ct.so2.tons' #average of previous years
  avg.so2.emis <- ampd_daily_all_unit.2018.2018
  names(avg.so2.emis)[names(avg.so2.emis) == 'isoweek'] <- 'week'
  avg.so2.emis$week <- as.character(avg.so2.emis$week)
  
  ampd_daily_all_unit.2019 <- ampd_daily_emissions_2019 %>% filter (ORISPL_CODE %in% facility.2018.2018[i] )
  names(ampd_daily_all_unit.2019)[names(ampd_daily_all_unit.2019) == 'SO2..tons.'] <- 'ac.so2.tons' #average of previous years
  so2.emis <- ampd_daily_all_unit.2019
  names(so2.emis)[names(so2.emis) == 'isoweek'] <- 'week'
  so2.emis$week <- as.character(so2.emis$week)
  
  
  so2.emis.linear <- merge(avg.so2.emis, so2.emis, by= c("ORISPL_CODE", "week"))
  stats <- as.data.table(modStats(so2.emis.linear, mod = "ct.so2.tons", obs = "ac.so2.tons"))
  fac.1 <- as.data.frame(facility.2018.2018[i])
  datalist[[i]] <- cbind(fac.1,stats) # add it to your list
}

sim.avg.ac.ct.so2.emission.1 = do.call(rbind, datalist)

names(sim.avg.ac.ct.so2.emission.1)[names(sim.avg.ac.ct.so2.emission.1) == 'facility.2018.2018[i]'] <- 'facility'

fac.op.time.sim <- ampd_daily_emissions_2019
fac.op.2019.sim <- fac.op.time.sim %>%  filter (ORISPL_CODE %in% facility.2018.2018)

fac.op.2019.sim <- setDT(fac.op.2019.sim)[, .(SUM_OP_TIME=sum(SUM_OP_TIME, na.rm=T)),
                                          by = .( ORISPL_CODE)]

names(fac.op.2019.sim)[names(fac.op.2019.sim) == 'ORISPL_CODE'] <- 'facility'


sim.avg.ac.ct.so2.emission.1 <- merge(sim.avg.ac.ct.so2.emission.1, fac.op.2019.sim , by= c("facility" ))
sim.avg.ac.ct.so2.emission.1$year <- 2019


sim.avg.ac.ct.so2.emission.1 <- sim.avg.ac.ct.so2.emission.1  #2018:2018 simple average vs 2019 emission



# big_data<- big_data %>% filter (r !="NA")

# sim.avg.ac.ct.so2.emission %>%
#   ggplot(aes(RMSE)) +
#   geom_histogram(binwidth = 1, color = "black") + ylim(0,80)

sim.avg.ac.ct.so2.emission.9$group <- "9 year mean"
sim.avg.ac.ct.so2.emission.7$group <- "7 year mean"
sim.avg.ac.ct.so2.emission.5$group <- "5 year mean"
sim.avg.ac.ct.so2.emission.3$group <- "3 year mean"
sim.avg.ac.ct.so2.emission.1$group <- "1 year"
ac.ct.so2.emission$group <- "gsynth"
# sensitivity.ac.ct.so2.emission$group <- "sensitivity-gsynth"

all.so2.emission <- rbind(sim.avg.ac.ct.so2.emission.9, sim.avg.ac.ct.so2.emission.7,
                          sim.avg.ac.ct.so2.emission.5, sim.avg.ac.ct.so2.emission.3,
                          sim.avg.ac.ct.so2.emission.1 ,ac.ct.so2.emission)
# all.so2.emission$r.sqd.r <- (all.so2.emission$r)^2


# write.fst(all.so2.emission, "data/all.so2.emission.2019.fst")

all.so2.emission <- read.fst("data/all.so2.emission.2019.fst")

all.so2.emission<- all.so2.emission %>% filter (SUM_OP_TIME>0 & r != "NA")

groups <- c( "7 year mean", "5 year mean", "3 year mean", "gsynth")

# groups <- c("3 year mean", "gsynth")



all.so2.emission <- all.so2.emission %>%  filter (group %in% groups )

so2.3 <- all.so2.emission %>%  filter (group %in% c ("3 year mean", "5 year mean", "7 year mean" ))

so2.gsynth <- all.so2.emission %>%  filter (group=="gsynth")

#ttest to compare 3 year mean metrics vs gsynth metrics
t.test(so2.3$NMGE, so2.gsynth$NMGE, var.equal = T)
t.test(so2.3$RMSE, so2.gsynth$RMSE, var.equal = T)
t.test(so2.3$MB, so2.gsynth$MB, var.equal = T)
t.test(so2.3$NMB, so2.gsynth$NMB, var.equal = T)

summary(so2.3)
summary(so2.gsynth)


#box plot consideing all percentage of operation
all.so2.emission %>% ggplot(aes(x=group, y= IOA, fill=group)) + geom_boxplot()  +
  labs(x="", y="Index of Agreement", title = "all percentage of operation in 2020") +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.45),
               size=3.5) + theme_bw() +theme(legend.position = "none")


nmge.so2 <- all.so2.emission %>% ggplot(aes(x=group, y= NMGE)) + geom_boxplot() + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(trans='log10')+
  labs(x="", y="", title = expression(paste(SO[2] , " NMGE(%) in Log-scale"))) +
  theme_bw() +theme(legend.position = "none" ,axis.text = element_text(size = 16),
                    axis.title = element_text(size = 20),
                    title=element_text(size=20)) 


ggsave("NMGE_so2.png", path = "./plots/")

all.so2.emission %>% ggplot(aes(x=group, y= NMB, fill=group)) + geom_boxplot() + ylim(-0.5,0.5)
  labs(x="", y="Normalized Mean Bias", title = "all percentage of operation in 2020") + 
   scale_y_continuous(trans = 'log10',labels = scales::number_format(accuracy = 0.01))


all.so2.emission %>% ggplot(aes(x=group, y= RMSE, fill=group)) + geom_boxplot() + ylim(0,0.1) +
  theme_bw() +theme(legend.position = "none")

all.so2.emission %>% ggplot(aes(x=group, y= MB, fill=group)) + geom_boxplot() + ylim(-0.1,0.1)
  labs(x="", y="MB", title = "all percentage of operation in 2020") +
  theme_bw() +theme(legend.position = "none")

all.so2.emission %>% ggplot(aes(x=group, y= MGE, fill=group)) + geom_boxplot() + ylim(0,0.1)
  scale_y_continuous(trans = 'log10', labels = scales::number_format(accuracy = 0.01)) +
  labs(x="", y="MGE (so2 tons/week)", title = "all percentage of operation in 2020") +
  theme_bw() +theme(legend.position = "none") +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.45),
               size=3.5) + theme_bw() +theme(legend.position = "none")

all.so2.emission %>% ggplot(aes(x=group, y= FAC2, fill=group)) + geom_boxplot() +
  labs(x="", y="Fractional Bias", title = "all percentage of operation in 2020") +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.5),
               size=3.5) + theme_bw() +theme(legend.position = "none")

all.so2.emission %>% ggplot(aes(x=group, y= r, fill=group)) + geom_boxplot() +
  labs(x="", y="Pearson R", title = "all percentage of operation in 2020") +
  theme_bw() +theme(legend.position = "none") +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.5),
               size=3.5) 

ggsave("pearson_R_so2.png", path = "./plots/")


#box plot consideing more than 75% operation in 2020

# based on percentile
summary(all.so2.emission$SUM_OP_TIME)

all.so2.emission.75 <- all.so2.emission %>%  filter(SUM_OP_TIME >= 15402.72)
all.so2.emission.75 %>% ggplot(aes(x=group, y= IOA, fill=group)) + geom_boxplot()  +
  labs(x="", y="Index of Agreement", title = "facilities operating >75% in 2020") +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.45),
               size=3.5) + theme_bw() +theme(legend.position = "none")

all.so2.emission.75 %>%  ggplot(aes(x=group, y= NMGE, fill=group)) + geom_boxplot() +
  labs(x="", y="Normalized Mean Gross Error", title = "facilities operating >75% in 2020")   +
  scale_y_continuous(trans = 'log10', labels = scales::number_format(accuracy = 0.01)) +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.45),
               size=3.5) + theme_bw() +theme(legend.position = "none")

all.so2.emission.75 %>% ggplot(aes(x=group, y= NMB, fill=group)) + geom_boxplot()  +
  scale_y_continuous(trans = 'log10', labels = scales::number_format(accuracy = 0.01)) +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.45),
               size=3.5) + theme_bw() +theme(legend.position = "none")
all.so2.emission.75 %>% ggplot(aes(x=group, y= RMSE, fill=group)) + geom_boxplot() +
  scale_y_continuous(trans = 'log10', labels = scales::number_format(accuracy = 0.01)) +
  labs(x="", y="RMSE (so2 tons/week)", title = "facilities operating >75% in 2020") +
  theme_bw() +theme(legend.position = "none")  +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.45),
               size=3.5) + theme_bw() +theme(legend.position = "none")
all.so2.emission.75 %>% ggplot(aes(x=group, y= MB, fill=group)) + geom_boxplot() +
  scale_y_continuous(trans = 'log10', labels = scales::number_format(accuracy = 0.01)) +
  labs(x="", y="MB", title = "facilities operating >75% in 2020") +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.45),
               size=3.5) + theme_bw() +theme(legend.position = "none")
all.so2.emission.75 %>% ggplot(aes(x=group, y= MGE, fill=group)) + geom_boxplot() +
  scale_y_continuous(trans = 'log10', labels = scales::number_format(accuracy = 0.01)) +
  labs(x="", y="Mean Gross Error (so2 tons/week)", title = "facilities operating >75% in 2020") +
  theme_bw() +theme(legend.position = "none") +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.45),
               size=3.5) + theme_bw() +theme(legend.position = "none")

all.so2.emission.75 %>% ggplot(aes(x=group, y= FAC2, fill=group)) + geom_boxplot() +
  labs(x="", y="Fractional Bias", title = "facilities operating >75% in 2020") +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.5),
               size=3.5) + theme_bw() +theme(legend.position = "none")

all.so2.emission.75 %>% ggplot(aes(x=group, y= r, fill=group)) + geom_boxplot() +
  labs(x="", y="Pearson R", title = "facilities operating >75% in 2020") +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.5),
               size=3.5) + theme_bw() +theme(legend.position = "none")

#box plot consideing less than 25% operation

all.so2.emission.25 <- all.so2.emission %>%  filter(SUM_OP_TIME <3543.98) #25% percentile
all.so2.emission.25 %>% ggplot(aes(x=group, y= IOA, fill=group)) + geom_boxplot() +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.5),
               size=3.5) + theme_bw() +theme(legend.position = "none")

all.so2.emission.25 %>% ggplot(aes(x=group, y= NMGE, fill=group)) + geom_boxplot() + 
  scale_y_continuous(trans = 'log10',  labels = scales::number_format(accuracy = 0.01))+
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.5),
               size=3.5) + theme_bw() +theme(legend.position = "none")
all.so2.emission.25 %>% ggplot(aes(x=group, y= NMB, fill=group)) + geom_boxplot() +
  scale_y_continuous(trans = 'log10',  labels = scales::number_format(accuracy = 0.01))+
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.5),
               size=3.5) + theme_bw() +theme(legend.position = "none")
all.so2.emission.25 %>% ggplot(aes(x=group, y= NMB, fill=group)) + geom_boxplot() +
  scale_y_continuous(trans = 'log10',  labels = scales::number_format(accuracy = 0.01))+
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.5),
               size=3.5) + theme_bw() +theme(legend.position = "none")

all.so2.emission.25 %>% ggplot(aes(x=group, y= RMSE, fill=group)) + geom_boxplot() +
  scale_y_continuous(trans = 'log10', labels = scales::number_format(accuracy = 0.01)) +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.5),
               size=3.5) + theme_bw() +theme(legend.position = "none")
all.so2.emission.25 %>% ggplot(aes(x=group, y= MB, fill=group)) + geom_boxplot()+
  scale_y_continuous(trans = 'log10',  labels = scales::number_format(accuracy = 0.01))+
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.5),
               size=3.5) + theme_bw() +theme(legend.position = "none")
all.so2.emission.25 %>% ggplot(aes(x=group, y= MGE, fill=group)) + geom_boxplot()+ 
  scale_y_continuous(trans = 'log10',labels = scales::number_format(accuracy = 0.01)) +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.5),
               size=3.5) + theme_bw() +theme(legend.position = "none")


#box plot consideing 25% to 100% operation

all.so2.emission.25.100 <- all.so2.emission %>%  filter(SUM_OP_TIME >=3543.98)
all.so2.emission.25.100 %>% ggplot(aes(x=group, y= IOA, fill=group)) + geom_boxplot()+
  labs(x="", y="Index of Agreement", title = "facilities operating >25% in 2020") +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.45),
               size=3.5) + theme_bw() +theme(legend.position = "none")
all.so2.emission.25.100 %>% ggplot(aes(x=group, y= NMGE, fill=group)) + geom_boxplot()+
  scale_y_continuous(trans = 'log10', labels = scales::number_format(accuracy = 0.01)) +
  labs(x="", y="NMGE", title = "facilities operating >25% in 2020") +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.45),
               size=3.5) + theme_bw() +theme(legend.position = "none")
all.so2.emission.25.100 %>% ggplot(aes(x=group, y= NMB, fill=group)) + geom_boxplot()+
  scale_y_continuous(trans = 'log10', labels = scales::number_format(accuracy = 0.01))+
  labs(x="", y="NMB", title = "facilities operating >25% in 2020") +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.45),
               size=3.5) + theme_bw() +theme(legend.position = "none")
all.so2.emission.25.100 %>% ggplot(aes(x=group, y= RMSE, fill=group)) + geom_boxplot() + 
  scale_y_continuous(trans = 'log10', labels = scales::number_format(accuracy = 0.01))+
  labs(x="", y="RMSE", title = "facilities operating >25% in 2020") +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.45),
               size=3.5) + theme_bw() +theme(legend.position = "none")
all.so2.emission.25.100 %>% ggplot(aes(x=group, y= MB, fill=group)) + geom_boxplot() +
  scale_y_continuous(trans = 'log10', labels = scales::number_format(accuracy = 0.01))+
  labs(x="", y="MB", title = "facilities operating >25% in 2020") +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.45),
               size=3.5) + theme_bw() +theme(legend.position = "none")
all.so2.emission.25.100 %>% ggplot(aes(x=group, y= MGE, fill=group)) + geom_boxplot() + 
  scale_y_continuous(trans = 'log10',labels = scales::number_format(accuracy = 0.01)) +
  labs(x="", y="MGE", title = "facilities operating >25% in 2020") +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.45),
               size=3.5) + theme_bw() +theme(legend.position = "none")


#box plot consideing more than 50%

all.so2.emission.50.100 <- all.so2.emission %>%  filter(SUM_OP_TIME >=11103.02)
all.so2.emission.50.100 %>% ggplot(aes(x=group, y= IOA, fill=group)) + geom_boxplot()

all.so2.emission.50.100 %>% ggplot(aes(x=group, y= NMGE, fill=group)) + geom_boxplot() +
  scale_y_continuous(trans = 'log10', labels = scales::number_format(accuracy = 0.01))  +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.45),
               size=3.5) + theme_bw() +theme(legend.position = "none")
all.so2.emission.50.100 %>% ggplot(aes(x=group, y= NMB, fill=group)) + geom_boxplot()  +
  scale_y_continuous(trans = 'log10', labels = scales::number_format(accuracy = 0.01))  +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.45),
               size=3.5) + theme_bw() +theme(legend.position = "none")
all.so2.emission.50.100 %>% ggplot(aes(x=group, y= RMSE, fill=group)) + geom_boxplot() + 
  scale_y_continuous(trans = 'log10',  labels = scales::number_format(accuracy = 0.01)) +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.45),
               size=3.5) + theme_bw() +theme(legend.position = "none")
all.so2.emission.50.100 %>% ggplot(aes(x=group, y= MB, fill=group)) + geom_boxplot()  +
  scale_y_continuous(trans = 'log10', labels = scales::number_format(accuracy = 0.01))  +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.45),
               size=3.5) + theme_bw() +theme(legend.position = "none")
all.so2.emission.50.100 %>% ggplot(aes(x=group, y= MGE, fill=group)) + geom_boxplot() + 
  scale_y_continuous(trans = 'log10',  labels = scales::number_format(accuracy = 0.01))  +
  stat_summary(aes(label=sprintf("%1.1f", ..y..),),
               geom="text",
               fun = function(y) boxplot.stats(y)$stats,
               position=position_nudge(x=0.45),
               size=3.5) + theme_bw() +theme(legend.position = "none")




# ggsave("R.sq.2019_so2_9_box.png", path = "./plots/")

all.so2.emission %>% ggplot(aes(x=RMSE, fill=group)) +
  geom_histogram(binwidth = 1, alpha=0.5, position = 'identity') +
  labs(x= "RMSE",   y = "Number of facilities",
       title = "") + theme(legend.position = c(0.9, 0.8)) +
  guides(fill=guide_legend(title="Model")) +
  scale_x_continuous(trans = 'log2', labels = scales::number_format(accuracy = 0.01))

# ggsave("NMGE2019_so2_3.png", path = "./plots/")

# ============================================================================================
## so2 weekly check gsynth vs actual vs simple average

ampd_daily_emissions <- read.fst ("/Volumes/GoogleDrive/My Drive/R/ampd-raw-data-processing/data/ampd_daily_emission.fst")


ampd_daily_to_weekly_9 <- setDT(ampd_daily_emissions)[, .(SO2..tons. = sum(SO2..tons., na.rm=TRUE),
                                                          NOx..tons. = sum(NOx..tons., na.rm=TRUE),
                                                          SUM_OP_TIME=sum(SUM_OP_TIME, na.rm=T)),
                                                      by = .(STATE, ORISPL_CODE, ID, year,
                                                             isoweek(date))]

so2.2019 <- read.fst("data/all.facility.so2.2019.fst")

#GSYNTH  and actual

so2.gsynth.actual<- setDT(so2.2019)[, .(actual.so2.emis = sum(Y.tr.bar, na.rm=T),
                                         ct.so2.emis = sum(Y.ct.bar, na.rm=T)), 
                                     by = .(week)]


#7 year average
# =========2016 to 2018 average
ampd_daily_emissions_2012_2018 <- ampd_daily_to_weekly_9  %>%
  filter (year %in% c( 2012:2018) & isoweek >=1& isoweek<=52) %>% dplyr::select(ORISPL_CODE,  isoweek, SO2..tons.)

ampd_daily_emissions_2012_2018 <- setDT(ampd_daily_emissions_2012_2018)[ , 
                                                                         .(SO2..tons. = mean(SO2..tons., na.rm=TRUE)),
                                                                         by = .(ORISPL_CODE, isoweek)]

ampd_daily_emissions_2012_2018 <- setDT(ampd_daily_emissions_2012_2018)[ , 
                                                                         .(SO2..tons. = sum(SO2..tons., na.rm=TRUE)),
                                                                         by = .(isoweek)]


# 5 year average

# =========2016 to 2018 average
ampd_daily_emissions_2014_2018 <- ampd_daily_to_weekly_9  %>%
  filter (year %in% c( 2014:2018) & isoweek >=1& isoweek<=52) %>% dplyr::select(ORISPL_CODE,  isoweek, SO2..tons.)

ampd_daily_emissions_2014_2018 <- setDT(ampd_daily_emissions_2014_2018)[ , 
                                                                         .(SO2..tons. = mean(SO2..tons., na.rm=TRUE)),
                                                                         by = .(ORISPL_CODE,isoweek)]

ampd_daily_emissions_2014_2018 <- setDT(ampd_daily_emissions_2014_2018)[ , 
                                                                         .(SO2..tons. = sum(SO2..tons., na.rm=TRUE)),
                                                                         by = .(isoweek)]
# 3 year average

# =========2016 to 2018 average
ampd_daily_emissions_2016_2018 <- ampd_daily_to_weekly_9  %>%
  filter (year %in% c( 2016:2018) & isoweek >=1 & isoweek<=52) %>% dplyr::select(ORISPL_CODE,  isoweek, SO2..tons.)

ampd_daily_emissions_2016_2018 <- setDT(ampd_daily_emissions_2016_2018)[ , 
                                                                         .(SO2..tons. = mean(SO2..tons., na.rm=TRUE)),
                                                                         by = .(ORISPL_CODE, isoweek)]

ampd_daily_emissions_2016_2018 <- setDT(ampd_daily_emissions_2016_2018)[ , 
                                                                         .(SO2..tons. = sum(SO2..tons., na.rm=TRUE)),
                                                                         by = .(isoweek)]


names(so2.gsynth.actual)[names(so2.gsynth.actual) == 'ct.so2.emis'] <- 'gsynth.counterfactual.2019'
names(so2.gsynth.actual)[names(so2.gsynth.actual) == 'actual.so2.emis'] <- 'actual.2019'
names(ampd_daily_emissions_2016_2018)[names(ampd_daily_emissions_2016_2018) == 'SO2..tons.'] <- '3-year-counterfactual'
names(ampd_daily_emissions_2014_2018)[names(ampd_daily_emissions_2014_2018) == 'SO2..tons.'] <- '5-year-counterfactual'
names(ampd_daily_emissions_2012_2018)[names(ampd_daily_emissions_2012_2018) == 'SO2..tons.'] <- '7-year-counterfactual'
names(ampd_daily_emissions_2016_2018)[names(ampd_daily_emissions_2016_2018) == 'isoweek'] <- 'week'
names(ampd_daily_emissions_2014_2018)[names(ampd_daily_emissions_2014_2018) == 'isoweek'] <- 'week'
names(ampd_daily_emissions_2012_2018)[names(ampd_daily_emissions_2012_2018) == 'isoweek'] <- 'week'

df_list <- list(so2.gsynth.actual, ampd_daily_emissions_2016_2018,ampd_daily_emissions_2014_2018, ampd_daily_emissions_2012_2018)

#merge all data frames in list
df <- df_list %>% reduce(full_join, by='week')

df <- melt(df, id.vars = "week", measure.vars = c("actual.2019", "gsynth.counterfactual.2019",
                                            "3-year-counterfactual", "5-year-counterfactual",
                                            "7-year-counterfactual"))

names(df)[names(df) == 'variable'] <- 'group'
names(df)[names(df) == 'value'] <- 'SO2.tons'


dy <- df
compare.so2 <- dy %>%  ggplot(aes(x=week, y=SO2.tons, color=group)) + 
  geom_line(aes( size = group) ) +
    geom_vline(xintercept=8) + geom_vline(xintercept=16) +  
   annotate("rect", xmin = 8, xmax = 52, ymin = 0, ymax = Inf, fill = "blue", alpha = .1, color = NA) +
  annotate("rect", xmin = 8, xmax = 16, ymin = 0, ymax = Inf, fill = "green", alpha = .1, color = NA) +
  theme_bw() + scale_x_continuous(expand = c(0, 0), limits = c(0, 52)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 25000)) + theme (legend.position = c(0.5,0.12),
                                                                   legend.title = element_blank(),
                                                                   legend.text = element_text(size = 17),
                                                                   axis.text = element_text(size = 15),
                                                                   axis.title = element_text(size = 20),
                                                                   plot.title = element_text(size=25),
                                                                   legend.direction = "horizontal") +
  labs(x="Weeks in 2019", y="", title = expression(paste(SO[2], " Emissions from EGUs, tons"))) +
  scale_size_manual("group", values=c(2,2,1,1,1), guide="none")+
   scale_color_manual(values = c("red", "black", "blue", "orange", "yellow"), 
                      labels = c("actual", "GSYNTH",
                                 "3 year mean", "5 year mean",
                                 "7 year mean")) 

ggsave("compare_so2_2019.png", path = "./plots/awma")

## NOx weekly check gsynth vs actual vs simple average

# ampd_daily_emissions <- read.fst ("/Volumes/GoogleDrive/My Drive/R/ampd-raw-data-processing/data/ampd_daily_emission.fst")


ampd_daily_to_weekly_9 <- setDT(ampd_daily_emissions)[, .(SO2..tons. = sum(SO2..tons., na.rm=TRUE),
                                                          NOx..tons. = sum(NOx..tons., na.rm=TRUE),
                                                          SUM_OP_TIME=sum(SUM_OP_TIME, na.rm=T)),
                                                      by = .(STATE, ORISPL_CODE, ID, year,
                                                             isoweek(date))]

nox.2019 <- read.fst("data/all.facility.nox.2019.fst")

#GSYNTH  and actual

nox.gsynth.actual<- setDT(nox.2019)[, .(actual.nox.emis = sum(Y.tr.bar, na.rm=T),
                                        ct.nox.emis = sum(Y.ct.bar, na.rm=T)), 
                                    by = .(week)]


#7 year average
# =========2016 to 2018 average
ampd_daily_emissions_2012_2018 <- ampd_daily_to_weekly_9  %>%
  filter (year %in% c( 2012:2018) & isoweek >=1& isoweek<=52) %>% dplyr::select(ORISPL_CODE,  isoweek, NOx..tons.)

ampd_daily_emissions_2012_2018 <- setDT(ampd_daily_emissions_2012_2018)[ , 
                                                                         .(NOx..tons. = mean(NOx..tons., na.rm=TRUE)),
                                                                         by = .(ORISPL_CODE, isoweek)]

ampd_daily_emissions_2012_2018 <- setDT(ampd_daily_emissions_2012_2018)[ , 
                                                                         .(NOx..tons. = sum(NOx..tons., na.rm=TRUE)),
                                                                         by = .(isoweek)]


# 5 year average

# =========2016 to 2018 average
ampd_daily_emissions_2014_2018 <- ampd_daily_to_weekly_9  %>%
  filter (year %in% c( 2014:2018) & isoweek >=1& isoweek<=52) %>% dplyr::select(ORISPL_CODE,  isoweek, NOx..tons.)

ampd_daily_emissions_2014_2018 <- setDT(ampd_daily_emissions_2014_2018)[ , 
                                                                         .(NOx..tons. = mean(NOx..tons., na.rm=TRUE)),
                                                                         by = .(ORISPL_CODE,isoweek)]

ampd_daily_emissions_2014_2018 <- setDT(ampd_daily_emissions_2014_2018)[ , 
                                                                         .(NOx..tons. = sum(NOx..tons., na.rm=TRUE)),
                                                                         by = .(isoweek)]
# 3 year average

# =========2016 to 2018 average
ampd_daily_emissions_2016_2018 <- ampd_daily_to_weekly_9  %>%
  filter (year %in% c( 2016:2018) & isoweek >=1 & isoweek<=52) %>% dplyr::select(ORISPL_CODE,  isoweek, NOx..tons.)

ampd_daily_emissions_2016_2018 <- setDT(ampd_daily_emissions_2016_2018)[ , 
                                                                         .(NOx..tons. = mean(NOx..tons., na.rm=TRUE)),
                                                                         by = .(ORISPL_CODE, isoweek)]

ampd_daily_emissions_2016_2018 <- setDT(ampd_daily_emissions_2016_2018)[ , 
                                                                         .(NOx..tons. = sum(NOx..tons., na.rm=TRUE)),
                                                                         by = .(isoweek)]


names(nox.gsynth.actual)[names(nox.gsynth.actual) == 'ct.nox.emis'] <- 'gsynth.counterfactual.2019'
names(nox.gsynth.actual)[names(nox.gsynth.actual) == 'actual.nox.emis'] <- 'actual.2019'
names(ampd_daily_emissions_2016_2018)[names(ampd_daily_emissions_2016_2018) == 'NOx..tons.'] <- '3-year-counterfactual'
names(ampd_daily_emissions_2014_2018)[names(ampd_daily_emissions_2014_2018) == 'NOx..tons.'] <- '5-year-counterfactual'
names(ampd_daily_emissions_2012_2018)[names(ampd_daily_emissions_2012_2018) == 'NOx..tons.'] <- '7-year-counterfactual'
names(ampd_daily_emissions_2016_2018)[names(ampd_daily_emissions_2016_2018) == 'isoweek'] <- 'week'
names(ampd_daily_emissions_2014_2018)[names(ampd_daily_emissions_2014_2018) == 'isoweek'] <- 'week'
names(ampd_daily_emissions_2012_2018)[names(ampd_daily_emissions_2012_2018) == 'isoweek'] <- 'week'

df_list <- list(nox.gsynth.actual, ampd_daily_emissions_2016_2018,ampd_daily_emissions_2014_2018, ampd_daily_emissions_2012_2018)

#merge all data frames in list
df <- df_list %>% reduce(full_join, by='week')

df <- melt(df, id.vars = "week", measure.vars = c("actual.2019", "gsynth.counterfactual.2019",
                                                  "3-year-counterfactual", "5-year-counterfactual",
                                                  "7-year-counterfactual"))

names(df)[names(df) == 'variable'] <- 'group'
names(df)[names(df) == 'value'] <- 'nox.tons'


compare.nox <- df%>%  ggplot(aes(x=week, y=nox.tons, color=group)) + 
  geom_line(aes( size = group) ) +
  geom_vline(xintercept=8) + geom_vline(xintercept=16) +  
  annotate("rect", xmin = 8, xmax = 52, ymin = 0, ymax = Inf, fill = "blue", alpha = .1, color = NA) +
  annotate("rect", xmin = 8, xmax = 16, ymin = 0, ymax = Inf, fill = "green", alpha = .1, color = NA) +
  theme_bw() + scale_x_continuous(expand = c(0, 0), limits = c(0, 52)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,15000)) + theme (legend.position = "none",
                                                                   legend.title = element_blank(),
                                                                   legend.text = element_text(size = 17),
                                                                   axis.text = element_text(size = 15),
                                                                   axis.title = element_text(size = 20),
                                                                   plot.title = element_text(size=25),
                                                                   legend.direction = "horizontal") +
  labs(x="Weeks in 2019", y="", title = expression(paste(NO[X], " Emissions from EGUs, tons"))) +
  scale_size_manual("group", values=c(2,2,1,1,1), guide="none")+
  scale_color_manual(values = c("red", "black", "blue", "orange", "yellow"), 
                     labels = c("actual", "GSYNTH",
                                "3 year mean", "5 year mean",
                                "7 year mean"))
  

ggarrange(compare.so2,  compare.nox ,ncol = 2, nrow = 1) 

ggsave("compare_so2_nox_2019.png", path = "./plots/awma", width=18, height=6, units="in")


ggsave("compare_nox_2019.png", path = "./plots/", width=16, height=5, units="in")


all.nox.emission <- setDT(read.fst("data/all.nox.emission.2019.fst"))

all.nox.emission<- all.nox.emission %>% filter (SUM_OP_TIME>0)

groups <- c( "7 year mean", "5 year mean", "3 year mean", "gsynth")

# groups <- c("3 year mean", "gsynth")
all.nox.emission <- all.nox.emission %>%  filter (group %in% groups)

nox.3 <- all.nox.emission %>%  filter (group=="3 year mean" )
nox.gsynth <- all.nox.emission %>%  filter (group=="gsynth")

summary(nox.3)

summary(nox.gsynth)


nmge.nox <- all.nox.emission %>% ggplot(aes(x=group, y= NMGE)) + geom_boxplot() + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(trans='log10')+
  labs(x="", y="", title = expression(paste(NO[X] , " NMGE(%) in Log-scale"))) +
  theme_bw() +theme(legend.position = "none" ,axis.text = element_text(size = 16),
                    axis.title = element_text(size = 20),
                    title=element_text(size=20)) 

ggsave("NMGE_nox.png", path = "./plots/")


#combining NMGE plot and compare so2 plot 

plot_grid(nmge.so2, 
          compare.so2, 
          nmge.nox, compare.nox,
          labels = c('A', 'B', "C", "D"), 
          label_size = 15)

ggsave("combined_so2_2019.png", path = "./plots/", width=18, height=12, units="in")
