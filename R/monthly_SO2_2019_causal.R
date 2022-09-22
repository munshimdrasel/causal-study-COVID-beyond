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
                                                            year, month, day, NOx..tons., SO2..tons., pr, tmmx, rmax, vs, th,
                                                            Fuel.Type..Primary. )


ampd_daily_select <- ampd_daily_select %>%  filter (STATE=="TX")

facility <- as.vector(unique(ampd_daily_select$ORISPL_CODE))

#operation in 2020

operation.pct.facility <- function(facility.name) {
  ampd_daily_all_unit <- ampd_daily_select %>% filter (ORISPL_CODE %in% facility.name & year==2019)
  
  ampd_daily_all_unit.op <- ampd_daily_all_unit %>% mutate(op.pct =(length(ampd_daily_all_unit$SO2..tons. [ampd_daily_all_unit$SO2..tons. >0])/
                                                                      length(ampd_daily_all_unit$NOx..tons. ))*100)
  ampd_daily_all_unit <- ampd_daily_all_unit.op 
  return(ampd_daily_all_unit)
}

op <- lapply(facility, operation.pct.facility )

operation <- as.data.frame(matrix(nrow = 1048, ncol =17))

for (i in 1:length(facility)) {
  operation [i, ] <- as.data.frame((op[i]))
}

#taking facilities opearating more than 25% in 2019

operation <- operation %>%  filter (V17>25)
ID_operating_25pct <- as.vector(unique(operation$V5))

ampd_daily_select <- ampd_daily_select %>% filter (ID %in% ID_operating_25pct)

#facilities opearating in 2019 only

ampd_daily_all_units_2019 <- ampd_daily_select%>%  filter (year==2019)
facility_operating_2019 <- as.vector(unique(ampd_daily_all_units_2019$ORISPL_CODE))
ampd_facility <- ampd_daily_all_units %>%  filter (ORISPL_CODE %in% facility_operating_2019)

ampd_daily_to_monthly <- setDT(ampd_daily_select)[, .(SO2..tons. = sum(SO2..tons., na.rm=TRUE),
                                                     NOx..tons. = sum(NOx..tons., na.rm=TRUE),
                                                     pr= sum(pr, na.rm=T),
                                                     tmmx=mean(tmmx, na.rm=T),
                                                     vs= mean(vs, na.rm=T),
                                                     rmax= mean(rmax, na.rm=T),
                                                     th= mean(th, na.rm=T)),
                                                 by = .(STATE, ORISPL_CODE, ID, year,
                                                        month, Fuel.Type..Primary.)]


ampd_monthly <- ampd_daily_to_monthly %>% filter (STATE=="TX" & year <=2019 & th>0) #wind direction should not be negative



ampd_daily_all_units <- ampd_monthly
facility <- as.vector(unique(ampd_daily_all_units$ORISPL_CODE))

fileConn<-file("output.txt")

gsynth.fn <- function(facility.name) {
  ampd_daily_all_unit <- ampd_daily_all_units %>% filter (ORISPL_CODE %in% facility.name )
  all_ampd <- ampd_daily_all_unit
  id_selected <-  unique( ampd_daily_all_unit$ID)
  all_ampd <-  all_ampd %>%  dplyr::select (STATE, year, month, ORISPL_CODE, ID, SO2..tons., 
                                            pr, tmmx, rmax, vs, th)
  # all_ampd$SO2..tons.[is.na(all_ampd$SO2..tons.)] <-  0
  
  all_ampd <- as.data.table(all_ampd)
  
  #setting intervention
  all_ampd <- all_ampd[, inputed := 0]
  all_ampd$inputed[all_ampd$year==2019 & all_ampd$month>2 &  all_ampd$ID %in% id_selected] <- 1
  
  all_ampd <- all_ampd [, id := paste(year, ID, sep = "_")]
  
  all_ampd_final <- all_ampd%>% dplyr::select (year, month, id, SO2..tons.,  inputed,
                                               pr, tmmx, rmax, vs, th)
  
  all_ampd_final$month <- as.character(all_ampd_final$month)
  synth_eq <- SO2..tons. ~ inputed+ tmmx + rmax + pr + th + vs 
  
  paste("Facility now running", facility.name, unique(ampd_daily_all_unit$STATE), sep= " ")
  # synth_eq <- SO2..tons. ~ inputed+  tmmx + rmax + pr + th + vs + day.id
  
  tryCatch({
    #Running gsynth 
    out <<- gsynth(synth_eq,
                   data=all_ampd_final,
                   index = c("id","month"), na.rm=T, force = "two-way", se=FALSE,
                   CV = TRUE,  inference = "nonparametric")
    
    x <- as.data.frame(out$Y.bar)
    y <- as.data.frame(out$time)
    # m <- as.data.frame(rep((facility), times=30))
    z <- cbind(y,x)
    colnames(z)[1] <- "time"
    # somdate2 <- substr(somDate,7,10)
    # som.date <- as.numeric(gsub("-", "", as.character(somdate2)))
    xx <- z %>% filter(out$time <= "8")
    
    stats <- as.data.table(modStats(z, mod = "Y.ct.bar", obs = "Y.tr.bar"))
    # data_plots <- plot(out, type = "missing", theme.bw = TRUE,
    # main = paste(facility.name),)
    counterfactual_plots <- plot(out, type = "counterfactual", raw = "none", 
                                 main=paste("Facility", facility.name,unique(ampd_daily_all_unit$STATE), sep= "-")) + 
      scale_x_continuous(breaks = seq(0, 365, by = 30)) +
      theme(axis.text.x = element_text(angle = 90)) 
    # counterfactual_plots_att <- plot(out, main=paste(facility.name)) +
    #   scale_x_continuous(breaks = seq(0, 120, by = 10)) +
    #   theme(axis.text.x = element_text(angle = 90))
    
    # all <- list(stats, out, counterfactual_plots, counterfactual_plots_att)
    all <- list(stats, out, counterfactual_plots)
  }, error = function(e) {
    writeLines(paste0("at facility ", facility.name, " something caused error "), fileConn)
  })
  return(all)
}
result_so2_weekly_2019 <- lapply(facility, gsynth.fn)
close(fileConn)

# getting each units actual emission into a dataframe====================================================
datalist = list()

for (i in 1:length(facility)) {
  so2.emis <- na.omit(as.data.frame(result_so2_whole_2020[[i]][[2]]$Y.bar))
  so2.emis <- so2.emis %>% dplyr::select(-Y.co.bar)
  so2.emis$week <- c(1:52)
  so2.emis <- so2.emis %>% filter(week>=9)
  stats <- as.data.table(modStats(so2.emis, mod = "Y.ct.bar", obs = "Y.tr.bar"))
  fac.1 <- as.data.frame(facility[i])
  datalist[[i]] <- cbind(fac.1,stats) # add it to your list
}

ac.ct.so2.emission = do.call(rbind, datalist)

names(ac.ct.so2.emission)[names(ac.ct.so2.emission) == 'facility[i]'] <- 'facility'
# big_data<- big_data %>% filter (r !="NA")

ac.ct.so2.emission %>% 
  ggplot(aes(RMSE)) + 
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2", labels = scales::comma) +ylim(0,30)


#sensitivity analysis: replacing -ve value as 0
# datalist = list()
# 
# for (i in 1:length(facility)) {
#   so2.emis <- na.omit(as.data.frame(result[[i]][[2]]$Y.bar))
#   so2.emis <- so2.emis %>% dplyr::select(-Y.co.bar)
#   so2.emis$day <- c((101:131), (201:228), (301:331), (401:430))
#   so2.emis <- so2.emis %>% filter(day>=301)
#   so2.emis$Y.ct.bar <- with(so2.emis, ifelse(Y.ct.bar<0, 0,Y.ct.bar))
#   stats <- as.data.table(modStats(so2.emis, mod = "Y.ct.bar", obs = "Y.tr.bar"))
#   fac.1 <- as.data.frame(facility[i])
#   datalist[[i]] <- cbind(fac.1,stats) # add it to your list
# }
# 
# sensitivity.ac.ct.so2.emission = do.call(rbind, datalist) 
# 
# names(sensitivity.ac.ct.so2.emission)[names(sensitivity.ac.ct.so2.emission) == 'facility[i]'] <- 'facility'
# # big_data<- big_data %>% filter (r !="NA")
# 
# sensitivity.ac.ct.so2.emission %>% 
#   ggplot(aes(RMSE)) + 
#   geom_histogram(binwidth = 1, color = "black") +
#   scale_x_continuous(trans = "log2", labels = scales::comma) +ylim(0,150)

# ===============================average of 2016, 2017, 2018 vs 2019 emission======================
ampd_daily_emissions <- read.fst ("/Volumes/GoogleDrive/My Drive/R/ampd-raw-data-processing/data/ampd_daily_emission.fst")
ampd_daily_emissions_2016_2018_9_52 <- ampd_daily_all_units %>% 
  filter (year %in% c( 2016, 2017, 2018) &
            isoweek >=9) %>% dplyr::select(ORISPL_CODE, ID, isoweek, SO2..tons.)

ampd_daily_emissions_2016_2018_9_52 <- ampd_daily_emissions_2016_2018_9_52 %>% 
  filter (ORISPL_CODE %in% facility )


datalist = list()

for (i in 1:length(facility)) {
  ampd_daily_all_unit <- ampd_daily_emissions_2016_2018_9_52 %>% filter (ORISPL_CODE %in% facility[i] )
  names(ampd_daily_all_unit)[names(ampd_daily_all_unit) == 'SO2..tons.'] <- 'ac.so2.tons'
  avg.so2.emis <- ampd_daily_all_unit
  names(avg.so2.emis)[names(avg.so2.emis) == 'isoweek'] <- 'week'
  avg.so2.emis$week <- as.character(avg.so2.emis$week)
  
  so2.emis <- na.omit(as.data.frame(result_so2_whole_2020[[i]][[2]]$Y.bar))
  so2.emis <- so2.emis %>% dplyr::select(Y.ct.bar) #taking counterfactual emission data 
  so2.emis$week <- c(1:52)
  so2.emis <- so2.emis %>% filter(week>=9)
  so2.emis$week <- as.character(so2.emis$week)
  so2.emis$ORISPL_CODE <- facility[i]
  names(so2.emis)[names(so2.emis) == 'Y.ct.bar'] <- 'ct.so2.tons'
  
  so2.emis.linear <- merge(avg.so2.emis, so2.emis, by= c("ORISPL_CODE", "week"))
  stats <- as.data.table(modStats(so2.emis.linear, mod = "ct.so2.tons", obs = "ac.so2.tons"))
  fac.1 <- as.data.frame(facility[i])
  datalist[[i]] <- cbind(fac.1,stats) # add it to your list
}

sim.avg.ac.ct.so2.emission = do.call(rbind, datalist)

names(sim.avg.ac.ct.so2.emission)[names(sim.avg.ac.ct.so2.emission) == 'facility[i]'] <- 'facility'
# big_data<- big_data %>% filter (r !="NA")

sim.avg.ac.ct.so2.emission %>% 
  ggplot(aes(RMSE)) + 
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2", labels = scales::comma) + ylim(0,40)

sim.avg.ac.ct.so2.emission$group <- "3 year mean"
ac.ct.so2.emission$group <- "gsynth"
# sensitivity.ac.ct.so2.emission$group <- "sensitivity-gsynth"

all.so2.emission <- rbind(sim.avg.ac.ct.so2.emission,ac.ct.so2.emission)

all.so2.emission<- all.so2.emission %>% filter (r !="NA")

all.so2.emission %>% ggplot(aes(x=NMGE, fill=group)) + 
  geom_histogram(binwidth = 0.5, alpha=0.5, position = 'identity') +
  scale_x_continuous(trans = "log2", labels = scales::comma) + ylim(0,30) 

