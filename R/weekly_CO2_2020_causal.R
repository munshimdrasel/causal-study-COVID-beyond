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
# ampd_daily_all_units<- read.fst ("data/ampd_daily_cleaned_facility.fst")
# 
# # ampd_daily_all_units <- ampd_daily_all_units %>% filter(STATE %in% c("VA", "TX"))
# 
# ampd_daily_select <- ampd_daily_all_units %>% 
#   dplyr::select(ORISPL_CODE, date, STATE, UNITID, ID, year, month, day, 
#                 SO2..tons., CO2..tons., pr, tmmx, rmax, vs, th, Fuel.Type..Primary. )
# 
# ampd_daily_select <- ampd_daily_select 
# 
# facility <- as.vector(unique(ampd_daily_select$ORISPL_CODE))
# 
# 
# ampd_daily_to_weekly <- setDT(ampd_daily_select)[, .(SO2..tons. = sum(SO2..tons., na.rm=TRUE),
#                                                      CO2..tons. = sum(CO2..tons., na.rm=TRUE),
#                                                      pr= sum(pr, na.rm=T),
#                                                      tmmx=mean(tmmx, na.rm=T),
#                                                      vs= mean(vs, na.rm=T),
#                                                      rmax= mean(rmax, na.rm=T),
#                                                      th= mean(th, na.rm=T)),
#                                                  by = .(STATE, ORISPL_CODE, ID, year,
#                                                         isoweek(date),Fuel.Type..Primary.)]
# names(ampd_daily_to_weekly)[names(ampd_daily_to_weekly) == 'isoweek'] <- 'week'
# 
# ampd_weekly <- ampd_daily_to_weekly %>% filter ( year <=2020 & th>0) #wind direction should not be negative
# 
# 
# 
# ampd_daily_all_units <- ampd_weekly
# facility <- as.vector(unique(ampd_daily_all_units$ORISPL_CODE))
# 
# save(facility, file="data/facility_co2_2020.RData")
# load ("data/facility_co2_2020.RData")
# # 
# fileConn<-file("output.txt")
# 
# gsynth.fn <- function(facility.name) {
#   ampd_daily_all_unit <- ampd_daily_all_units %>% filter (ORISPL_CODE %in% facility.name )
#   all_ampd <- ampd_daily_all_unit
#   id_selected <-  unique( ampd_daily_all_unit$ID)
#   all_ampd <-  all_ampd %>%  dplyr::select (STATE, year, week, ORISPL_CODE, ID, CO2..tons.,
#                                             pr, tmmx, rmax, vs, th)
#   # all_ampd$CO2..tons.[is.na(all_ampd$CO2..tons.)] <-  0
# 
#   all_ampd <- as.data.table(all_ampd)
# 
#   #setting intervention
#   all_ampd <- all_ampd[, inputed := 0]
#   all_ampd$inputed[all_ampd$year==2020 & all_ampd$week>8 &  all_ampd$ID %in% id_selected] <- 1
# 
#   all_ampd <- all_ampd [, id := paste(year, ID, sep = "_")]
# 
#   all_ampd_final <- all_ampd%>% dplyr::select (year, week, id, CO2..tons.,  inputed,
#                                                pr, tmmx, rmax, vs, th)
# 
# 
#   synth_eq <- CO2..tons. ~ inputed+ tmmx + rmax + pr + th + vs
# 
#   paste("Facility now running", facility.name, unique(ampd_daily_all_unit$STATE), sep= " ")
# 
#   # synth_eq <- CO2..tons. ~ inputed+  tmmx + rmax + pr + th + vs + day.id
#   tryCatch({
#     #Running gsynth
#     out <<- gsynth(synth_eq,
#                    data=all_ampd_final,
#                    index = c("id","week"), na.rm=T, force = "two-way", se=FALSE, nboots=1000,
#                    CV = TRUE, seed =  123, estimator = "mc", parallel=T, inference = "nonparametric")
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
#     # counterfactual_plots <- plot(out, type = "counterfactual", raw = "none",
#     #                              main=paste("Facility", facility.name,unique(ampd_daily_all_unit$STATE), sep= "-")) +
#     #   scale_x_continuous(breaks = seq(0, 52, by = 5))
#     # counterfactual_plots_att <- plot(out, main=paste(facility.name)) +
#     #   scale_x_continuous(breaks = seq(0, 120, by = 10)) +
#     #   theme(axis.text.x = element_text(angle = 90))
#     # all <- list(stats, out, counterfactual_plots, counterfactual_plots_att)
#     all <- list(stats, out)
#   }, error = function(e) {
#     writeLines(paste0("at facility ", facility.name, " something caused error "), fileConn)
#   })
#   return(all)
# }
# result_co2_weekly_2020 <- lapply(facility, gsynth.fn)
# close(fileConn)
# 
# 
# save(result_co2_weekly_2020, file="data/result_co2_weekly_2020.RData")
load ("data/result_co2_weekly_2020.RData")
load ("data/facility_co2_2020.RData")

# result_co2_weekly_2020_test <- list()
# facility_test <- list()
# 
# for (i in 1:length(result_co2_weekly_2020)){
#   sublist = result_co2_weekly_2020[[i]]
#   if (length(sublist) == 2)
#   {
#     result_co2_weekly_2020_test[[i]] <-result_co2_weekly_2020[[i]] # this does achieve the desired result
#     facility_test[[i]] <- facility[[i]]
#   }
#   else
#   {
#     result_co2_weekly_2020_test[[i]] <- NULL # this does achieve the desired result
#   }
# 
# }
# 
# result_co2_weekly_2020_test2 <- result_co2_weekly_2020_test[-which(sapply(result_co2_weekly_2020_test, is.null))]
# 
# result_co2_weekly_2020 <- result_co2_weekly_2020_test2
# facility <- unlist(facility_test, recursive = TRUE, use.names = TRUE)




#getting counterfactua plots for 2020 CO2 emissions
# counterfactual_plots <- list()
# for (i in 1: length(facility)) {
#   counterfactual_plots[[i]] <- result_co2_weekly_2020 [[i]][[3]]
# }
# layout <- rbind(c(1,2), c(3,4), c(5,6))
# pdf(file = 'plots/counterfactual_plots_co2_2020.pdf', onefile = TRUE, paper = 'A4',
#     width = 9, height = 9, pointsize = 1)
# marrangeGrob(grobs = counterfactual_plots, ncol = 2, nrow = 3, layout_matrix = layout)
# dev.off()



st = list()

for (i in 1:length(facility)) {
  fac.1 <- as.data.frame(facility[i])
  att.avg <-as.data.frame(result_co2_weekly_2020[[i]][[2]]$att)
  week <- 1:nrow(att.avg)
  st[[i]] <- cbind(fac.1,att.avg, week)
}

st = do.call(rbind, st)

names(st)[names(st) == 'facility[i]'] <- 'facility'
names(st)[names(st) == 'result_co2_weekly_2020[[i]][[2]]$att'] <- 'ATT'


ampd_facility<- read.fst ("data/ampd_daily_cleaned_facility.fst")

ampd_facility <- ampd_facility %>% filter (ORISPL_CODE %in% facility & year==2020)

ampd_facility <- ampd_facility %>%  dplyr::select ( STATE, ORISPL_CODE, Fuel.Type..Primary.,
                                                    Facility.Latitude, Facility.Longitude,
                                                    County, County.Code, FIPS.Code, Source.Category, Unit.Type,
                                                    SO2.Control.s., NOx.Control.s.,PM.Control.s., Hg.Control.s., EPA.Region,
                                                    NERC.Region, Associated.Stacks, Program.s., SO2.Phase, NOx.Phase,
                                                    Unit.Type)


ampd_facility <- distinct(ampd_facility, .keep_all = T)
names(ampd_facility)[names(ampd_facility) == 'ORISPL_CODE'] <- 'facility'

all.facility.co2.2020 <- merge (st, ampd_facility, by = "facility")


# getting each units actual emission into a dataframe====================================================
datalist = list()

for (i in 1:length(facility)) {
  co2.emis <- na.omit(as.data.frame(result_co2_weekly_2020[[i]][[2]]$Y.bar))
  co2.emis <- co2.emis %>% dplyr::select(-Y.co.bar)
  co2.emis$week <- c(1:nrow(co2.emis)) #leap year 2020
  fac.1 <- as.data.frame(facility[i])
  datalist[[i]] <- cbind(fac.1,co2.emis) # add it to your list
}

ac.ct.co2.emission = do.call(rbind, datalist)

names(ac.ct.co2.emission)[names(ac.ct.co2.emission) == 'facility[i]'] <- 'facility'

all.facility.co2.2020 <- merge (all.facility.co2.2020, ac.ct.co2.emission, by = c( "facility", "week" ))

write.fst(all.facility.co2.2020, "data/all.facility.co2.2020.fst")
