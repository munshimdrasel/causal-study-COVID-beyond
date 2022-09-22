
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
library(panelView)
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

ampd_daily_all_units <- ampd_daily_all_units

ampd_daily_all_units$NOx..tons.[is.na(ampd_daily_all_units$NOx..tons.)] <-  0
ampd_daily_all_units$SO2..tons.[is.na(ampd_daily_all_units$SO2..tons.)] <-  0
ampd_daily_all_units$CO2..tons.[is.na(ampd_daily_all_units$CO2..tons.)] <-  0

facility <- as.vector(unique(ampd_daily_all_units$ORISPL_CODE))


# 
fileConn<-file("output.txt")


# ================================================ function and lapply
gsynth.fn <- function(facility.name) {
  ampd_daily_all_unit <- ampd_daily_all_units %>% filter (ORISPL_CODE %in% facility.name )
  all_ampd <- ampd_daily_all_unit %>% filter(year<=2020)
  id_selected <-  unique( ampd_daily_all_unit$ID)
  all_ampd <-  all_ampd %>%  dplyr::select (date, STATE, year, month, day, ORISPL_CODE, ID, NOx..tons., 
                                            pr, tmmx, rmax, vs, th, day.name)
  all_ampd$NOx..tons.[is.na(all_ampd$NOx..tons.)] <-  0
  
  all_ampd <- as.data.table(all_ampd)
  #adding season
  all_ampd <- all_ampd[, season := 0]
  all_ampd$season[all_ampd$month==5 ] <- 1
  all_ampd$season[all_ampd$month==6 ] <- 1
  all_ampd$season[all_ampd$month==7 ] <- 1
  all_ampd$season[all_ampd$month==8 ] <- 1
  all_ampd$season[all_ampd$month==9 ] <- 1
  all_ampd$season[all_ampd$month==10 ] <- 1
  
  # all_ampd$wind.direction <- as.factor(all_ampd$wind.direction)
  
  
  #Treatment period based on state of emergency
  # som <- read.csv ("data/state.of.emergency.csv")
  
  
  #making SO2 emissions as categorical variable: 2020 March and April as 1, all others as 0
  
  all_ampd <- all_ampd[, inputed := 0]
  
  
  # ampd.state <- all_ampd[16,]$state
  # somDate <- som[match(ampd.state, som$State),]$State.of.emergency
  # endDate <- som[match(ampd.state, som$State),]$Reopen.businesses
  # all_ampd$inputed <- ifelse(all_ampd$date >= somDate & 
  #                              all_ampd$date < endDate & 
  #                              all_ampd$year == 2020, 1, 0)
  
  all_ampd$inputed[all_ampd$year==2020 & all_ampd$month==3 &  all_ampd$ID %in% id_selected] <- 1
  all_ampd$inputed[all_ampd$year==2020 & all_ampd$month==4 & all_ampd$ID %in% id_selected ] <- 1
  all_ampd$inputed[all_ampd$year==2020 & all_ampd$month==5 & all_ampd$ID %in% id_selected ] <- 1
  all_ampd$inputed[all_ampd$year==2020 & all_ampd$month==6 & all_ampd$ID %in% id_selected ] <- 1
  all_ampd$inputed[all_ampd$year==2020 & all_ampd$month==7 & all_ampd$ID %in% id_selected ] <- 1
  all_ampd$inputed[all_ampd$year==2020 & all_ampd$month==8 & all_ampd$ID %in% id_selected ] <- 1
  all_ampd$inputed[all_ampd$year==2020 & all_ampd$month==9 & all_ampd$ID %in% id_selected ] <- 1
  all_ampd$inputed[all_ampd$year==2020 & all_ampd$month==10 & all_ampd$ID %in% id_selected ] <- 1
  all_ampd$inputed[all_ampd$year==2020 & all_ampd$month==11 & all_ampd$ID %in% id_selected ] <- 1
  all_ampd$inputed[all_ampd$year==2020 & all_ampd$month==12 & all_ampd$ID %in% id_selected ] <- 1
  
  
  #Adding February, March, April as covariates
  
  # all_ampd <- all_ampd[, feb := 0]
  # all_ampd$feb[all_ampd$month==2 ] <- 1
  # 
  # all_ampd <- all_ampd[, mar := 0]
  # all_ampd$mar[all_ampd$month==3 ] <- 1
  # 
  # all_ampd <- all_ampd[, apr := 0]
  # all_ampd$apr[all_ampd$month==4 ] <- 1
  
  #Adding weekdays and weekends as covariates
  
  all_ampd <- all_ampd[, weekdays := 0]
  all_ampd$weekdays[all_ampd$day.name=="Monday" ] <- 1
  all_ampd$weekdays[all_ampd$day.name=="Tuesday" ] <- 1
  all_ampd$weekdays[all_ampd$day.name=="Wednesday" ] <- 1
  all_ampd$weekdays[all_ampd$day.name=="Thursday" ] <- 1
  all_ampd$weekdays[all_ampd$day.name=="Friday" ] <- 1
  
  #formating day as 2 digits number
  all_ampd$day <- all_ampd[ , stri_pad_left(all_ampd$day, pad="0", width=2)]
  all_ampd$month <- all_ampd[ , stri_pad_left(all_ampd$month, pad="0", width=2)]
  
  
  #combining month and day together
  all_ampd <- all_ampd [, time := paste(month, day, sep = "")]
  
  all_ampd <- all_ampd [, id := paste(year, ID, sep = "_")]
  
  
  all_ampd_final <- all_ampd%>% dplyr::select (year, time, id, NOx..tons.,  inputed,
                                               pr, tmmx, rmax, vs, th,
                                               weekdays, inputed,season)
  
  #factor doesn't work with gsynth??
  #all_ampd$feb <- as.numeric(all_ampd$feb)
  synth_eq <- NOx..tons. ~ inputed+ tmmx + rmax + pr + th + vs +weekdays
  # synth_eq <- SO2..tons. ~ inputed+ weekdays
  paste("Facility now running", facility.name, unique(ampd_daily_all_unit$STATE), sep= " ")
  # synth_eq <- SO2..tons. ~ inputed+  tmmx + rmax + pr + th + vs + day.id
  #adding feb, mar, apr giving error time invariant
  
  tryCatch({
    #Running gsynth 
    out <<- gsynth(synth_eq,
                   data=all_ampd_final,
                   index = c("id","time"), na.rm=T, force = "two-way", se=FALSE,
                   CV = TRUE, seed =  123, estimator = "mc", min.T0 = 10, 
                   inference = "nonparametric",
                   parallel = TRUE, cores=1)
    
    x <- as.data.frame(out$Y.bar)
    y <- as.data.frame(out$time)
    # m <- as.data.frame(rep((facility), times=30))
    z <- cbind(y,x)
    colnames(z)[1] <- "time"
    # somdate2 <- substr(somDate,7,10)
    # som.date <- as.numeric(gsub("-", "", as.character(somdate2)))
    xx <- z %>% filter(out$time <= "0229")
    
    stats <- as.data.table(modStats(z, mod = "Y.ct.bar", obs = "Y.tr.bar"))
    # data_plots <- plot(out, type = "missing", theme.bw = TRUE,
                       # main = paste(facility.name),)
    counterfactual_plots <- plot(out, type = "counterfactual", raw = "none", 
                                 main=paste("Facility", facility.name,unique(ampd_daily_all_unit$state), sep= "-")) + 
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
result_nox_whole_2020 <- lapply(facility, gsynth.fn)
close(fileConn)

#saving file
save(result_nox_whole_2020, file="data/result_facility_nox_2020_whole.RData")

load ("data/result_facility_nox_2020_whole.RData")


#counter factual plots
counterfactual_plots <- list()

for (i in 1: length(facility)) {
  counterfactual_plots[[i]] <- result_nox_whole_2020 [[i]][[3]]
}


layout <- rbind(c(1,2,3), c(4,5,6), c(7,8,9))
pdf(file = 'data/counterfactual_plots_nox_whole_2020.pdf', onefile = TRUE, paper = 'A4',
    width = 9, height = 9, pointsize = 1)
marrangeGrob(grobs = counterfactual_plots, ncol = 3, nrow = 3, layout_matrix = layout)
dev.off()

#average treatment on the treated plot
# counterfactual_plots_att <- list()
# for (i in 1: length(facility)) {
#   counterfactual_plots_att[[i]] <- result [[i]][[4]]
# }
#
# layout <- rbind(c(1,2,3), c(4,5,6), c(7,8,9))
# pdf(file = 'counterfactual_plots_so2_att.pdf', onefile = TRUE, paper = 'A4',
#     width = 9, height = 9, pointsize = 1)
# marrangeGrob(grobs = counterfactual_plots_att, ncol = 3, nrow = 3, layout_matrix = layout)
# dev.off()

st <- as.data.frame(matrix(nrow = 1048, ncol = 15))

for (i in 1:length(facility)) {
  rs.1 <- as.data.frame((result_nox_whole_2020[[i]][[1]]))
  fac.1 <- as.data.frame(facility[i])
  att.avg <- as.data.frame(result_nox_whole_2020[[i]][[2]]$att.avg)
  cv <- as.data.frame(result_nox_whole_2020[[i]][[2]]$lambda.cv)
  mspe <- as.data.frame(result_nox_whole_2020[[i]][[2]]$MSPE)
  # att <- as.data.frame((result[[i]][[2]]$att))
  # time <- rownames(att)
  # att <- cbind(time,att)
  # som <- read.csv ("data/state.of.emergency.csv")
  # ampd_daily_all_units_state <- unique(ampd_daily_all_units %>% filter (ORISPL_CODE %in% fac.1))
  # somDate <- unique(som[match(ampd_daily_all_units_state$state, som$State),]$State.of.emergency)
  # endDate <- unique(som[match(ampd_daily_all_units_state$state, som$State),]$Reopen.businesses)
  # somdate2 <- substr(somDate,7,10)
  # som.date <- as.numeric(gsub("-", "", as.character(somdate2)))
  # qt <- att %>% filter(att$time >= som.date)
  # colnames(qt) [2] <- "att"
  # ci <- CI(na.omit(qt$att),0.95)
  # qt <- as.vector(qt %>% dplyr::select(att))
  # qt <- quantile(qt, probs = c(.025, .975), na.rm=T)
  st [i, ] <- cbind(fac.1,rs.1, att.avg, cv, mspe )#, qt[1], qt[2], ci[1], ci[3])
}
st <- st %>% select (-V2)

old.names <- c ("V1",  "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15")
new.names <- c ("facility", "n","FAC2", "MB", "MGE", "NMB", "NMGE", "RMSE", "r", "COE", "IOA", "ATT", "lambda.cv", "MSPE" )

st <- st%>% rename_at(vars(old.names), ~ new.names)

#calculating percentage operation in 2020
operation.pct.facility <- function(facility.name) {
  ampd_daily_all_unit <- ampd_daily_all_units %>% filter (ORISPL_CODE %in% facility.name & year==2020)
  
  ampd_daily_all_unit.op <- ampd_daily_all_unit %>% mutate(op.pct =(length(ampd_daily_all_unit$NOx..tons. [ampd_daily_all_unit$NOx..tons. >0])/
                                                                      length(ampd_daily_all_unit$NOx..tons. ))*100)
  ampd_daily_all_unit <- ampd_daily_all_unit.op %>% dplyr::select(STATE, ORISPL_CODE, Fuel.Type..Primary.,
                                                                  Facility.Latitude, Facility.Longitude, County, op.pct)
  return(ampd_daily_all_unit)
}

op <- lapply(facility, operation.pct.facility )

operation <- as.data.frame(matrix(nrow = 1048, ncol =7))

for (i in 1:length(facility)) {
  operation [i, ] <- as.data.frame((op[i]))
}

old.names <- c ("V1", "V2", "V3", "V4", "V5", "V6", "V7")
new.names <- c ("STATE","facility", "Fuel.Type..Primary.", "Facility.Latitude", "Facility.Longitude",
                "County", "op.pct" )

operation <- operation%>% rename_at(vars(old.names), ~ new.names)

all.facility <- merge (st, operation, by = "facility")

write.fst (all.facility, "data/metrics_facility_nox_whole_2020.fst")








