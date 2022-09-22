#Figure for papers

rm(list = ls())

library(fst)
library(data.table)
library(tidyverse)
library(viridis)
library(ggplot2)
library(stringi)
library(dslabs)
library(gridExtra)
library(ggmap)
library(Rmisc)
library(usmap)
library(ggpubr)
library(cowplot)

setwd ("/Volumes/GoogleDrive/My Drive/R/causal-study-COVID-beyond")

#Figure 1 plotting

ampd_raw <- as.data.table(read.fst ("data/ampd_monthly_all.fst"))

ampd_raw <- ampd_raw [, ID := paste(ORISPL_CODE, UNITID, sep = "-")]


#filtering electric utility as source category

# ampd_raw <- ampd_raw %>% filter (Source.Category== "Electric Utility")

#Glossary of ampd data
# glossary <- read.csv("data/datadefinition_09-01-2020_170204640.csv")

# Facility ID (ORISPL)	The unique six-digit facility identification number, also called an ORISPL, assigned by the Energy Information Administration.
# Unit ID	Unique identifier for each unit at a facility.
# Unit	A fossil fuel-fired combustion device.
# SO2 (tons)	Sulfur dioxide emissions in short tons.
# SO2 Phase	Title IV of the Clean Air Act SO2 Phase. Phase I started in 1995; Phase II started in 2000.
# Steam Load (1000lb)	Total steam pressure produced by a unit or source in any calendar year (or other specified time period) produced by combusting a given heat input of fuel.
# SO2 Control(s)	Method or equipment used by the combustion unit to minimize production or emission of sulfur dioxide (SO2).
# PM Control(s)	Method or equipment used by the combustion unit to minimize production or emission of particulate matter (PM).
# Operating Time	Any part of an hour in which a unit combusts any fuel.
# Operating Status	An indication of the present condition of a unit (planned, operating, shutdown, etc.).
# NOx Phase	Group 1 boilers are divided into two mutually exclusive groups: Phase I and Phase II. The Phase a Group 1 boiler is associated with determines (in part) the Acid Rain NOx limit a boiler is subject to. There is no Phase I-Phase II bifurcation of Group 2 boilers.
# NOx (tons)	Nitrogen oxide emissions in short tons.
# NOx Control(s)	Method or equipment used by the combustion unit to minimize production or emission of nitrogen oxides (NOx).
# Max Hourly HI Rate (MMBtu/hr)	The design heat input capacity (in MMBtu/hr) for the unit or the highest hourly heat input rate observed in the past five years, whichever is greater.
# Hg Control(s)	Method or equipment used by the combustion unit to minimize production or emission of mercury (Hg).
# Heat Input (MMBtu)	The measure of utilization that is calculated by multiplying the quantity of fuel by the fuels heat content.
# Gross Load (MW-h)	Total electrical generation of a unit or source in any calendar year (or other specified time period) produced by combusting a given heat input of fuel.
# Fuel Type (Primary)	The primary type of fuel combusted by the unit.
# Fuel Type (Secondary)	The secondary type of fuel combusted by the unit.

#ampd_pplants_va <- ampd_raw %>% filter (STATE== 'VA') 

#write.csv(ampd_pplants_va, "/Users/munshirasel/Google Drive/R/ampd-3/data/ampd_pplants_va.csv")



#sorting AMPD data by state, year and month

ampd_sym<-            aggregate(list (NOx..tons.=ampd_raw$NOx..tons., 
                                      SO2..tons.=ampd_raw$SO2..tons.,
                                      CO2..tons.= ampd_raw$CO2..tons.,
                                      Gross.Load..MW.h.=ampd_raw$Gross.Load..MW.h.,
                                      Steam.Load..1000lb.= ampd_raw$Steam.Load..1000lb.,
                                      HEAT.INPUT= ampd_raw$HEAT.INPUT,
                                      SUM_OP_TIME= ampd_raw$SUM_OP_TIME
), by=list(STATE=ampd_raw $ STATE, year=ampd_raw$year,
           month=ampd_raw $month), FUN=sum)


#sum of all states

ampd_ym<- aggregate(list (NOx..tons.=ampd_raw$NOx..tons., 
                          SO2..tons.=ampd_raw$SO2..tons.,
                          CO2..tons.= ampd_raw$CO2..tons.,
                          Gross.Load..MW.h.=ampd_raw$Gross.Load..MW.h.,
                          Steam.Load..1000lb.= ampd_raw$Steam.Load..1000lb.,
                          HEAT.INPUT= ampd_raw$HEAT.INPUT,
                          SUM_OP_TIME= ampd_raw$SUM_OP_TIME
), by=list( year=ampd_raw$year,
            month=ampd_raw $month), FUN=sum)


ampd_y<- aggregate(list (NOx..tons.=ampd_raw$NOx..tons., 
                         SO2..tons.=ampd_raw$SO2..tons.,
                         CO2..tons.= ampd_raw$CO2..tons.,
                         Gross.Load..MW.h.=ampd_raw$Gross.Load..MW.h.,
                         Steam.Load..1000lb.= ampd_raw$Steam.Load..1000lb.,
                         HEAT.INPUT= ampd_raw$HEAT.INPUT,
                         SUM_OP_TIME= ampd_raw$SUM_OP_TIME
), by=list( year=ampd_raw$year), FUN=sum)
# 
# ampd_y_97 <- ampd_y %>% filter (year >= 1997)
# 
# summary(ampd_y_97)
# 
# x <- ampd_y_97 %>%  filter (year >= 2015 & year < 2020) 
# 
# mean(x $SO2..tons.)


# names(ampd_harvard_sym)
names(ampd_sym)

ampd_y%>% filter (year >= 1997 ) %>%
  ggplot(aes( year, NOx..tons., color= year)) + geom_point() + geom_line()

ampd_ym$NOx..tons. <- (ampd_ym$NOx..tons.)/10^6
ampd_ym$SO2..tons. <- (ampd_ym$SO2..tons.)/10^6
ampd_ym$CO2..tons. <- (ampd_ym$CO2..tons.)/10^6

#Yearly NOx emission:
names(ampd_ym)
plot1<- ampd_ym%>% filter (year >= 2010 & year<=2020) %>%
  ggplot(aes( year, NOx..tons., group= year)) + geom_boxplot() + 
  scale_y_continuous(expand=c(0,0), limits = c(0, NA)) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) + theme_bw() +
  theme( axis.title.y = element_text(size=25),
         axis.text = element_text(size=20),
         title=element_text(size=20)) +
  labs(x= "", y="",  title = expression(paste( NO[X], " Emissions", ", ", 10^6," tons")) ) 

plot2 <- ampd_ym%>% filter (year >= 2010 & year<=2020) %>%
  ggplot(aes( year, SO2..tons., group= year)) + geom_boxplot() +scale_y_continuous(expand=c(0,0), limits = c(0, NA)) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) + theme_bw() +
  theme( axis.title.y = element_text(size=25),
         axis.text = element_text(size=20),
         title=element_text(size=20)) +
  labs(x= "", y="",  title = expression(paste( SO[2], " Emissions", ", ", 10^6," tons")) ) 

plot3 <- ampd_ym%>% filter (year >= 2010 & year<=2020) %>%
  ggplot(aes( year, CO2..tons., group= year)) + geom_boxplot() + scale_y_continuous(expand=c(0,0), limits = c(0, NA)) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) + theme_bw() +
  theme( axis.title.y = element_text(size=25),
         axis.text = element_text(size=20),
         title=element_text(size=20)) +
  labs(x= "", y="",  title = expression(paste( CO[2], " Emissions", ", ", 10^6," tons")) ) 

# plot_grid(plot1, 
#           plot2, plot3,
#           labels = c('A', 'B', 'C'), label_x = 0.9, label_y = 0.85,
#           label_size = 12)

ggarrange(plot1,  plot2, plot3,  ncol = 3, nrow = 1,
          common.legend = T, legend = "bottom") 

ggsave("nox_so2_co2_trend.png", path = "./plots/awma", width=16, height=5, units="in")



# ========================================================================================================
# Figure 2 plotting
# ========================================================================================================

#NOX lockdown


nox.facility.loc <- as.data.table(read.fst( "data/all.facility.nox.2020.fst"))

nox.facility.loc <- nox.facility.loc %>%  filter (week>8 & week <17)

#plotting US Map for all facility 
nox.facility.states.loc<- setDT(nox.facility.loc)[, .(ATT = sum(ATT, na.rm=TRUE),
                                                      Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                                      Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE),
                                                      Y.tr.bar=sum(Y.tr.bar,na.rm=T),
                                                      Y.ct.bar=sum(Y.ct.bar,na.rm=T)),
                                                  by = .(STATE)]

names(nox.facility.states.loc)[names(nox.facility.states.loc) == 'STATE'] <- 'state'


nox.loc <- plot_usmap(data = as.data.frame(nox.facility.states.loc), values = "ATT", color = "black", labels = F,  exclude = c("AK", "HI")) +
  scale_fill_gradient2(limits=c(-500, 1500), 
                       low="blue", mid= "white", high="red", name = expression(paste(TTT[lockdown], " ",NO[x] , " tons"))) +
  theme(legend.position = "top", legend.direction = "horizontal" ,
        legend.text = element_text(size = 15),text = element_text(size=20)) + 
  guides(fill = guide_colorbar( label.position = "top", label.theme = element_text(angle = 90),
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= 0.2,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1)) 

#percentage of actual emissions
nox.facility.states.loc$pct <- (nox.facility.states.loc$ATT*100)/(nox.facility.states.loc$Y.tr.bar)

nox.loc.pct <- plot_usmap(data = as.data.frame(nox.facility.states.loc), values = "pct", color = "black", 
                          labels = F,  exclude = c("AK", "HI")) +
  scale_fill_gradient2(limits=c(-100, 100), oob = scales::squish,
                       low="blue", mid= "white", high="red", name = expression(paste(TTT[lockdown], " ",NO[x] , " ,% change"))) +
  theme(legend.position = "top", legend.direction = "horizontal" ,
        legend.text = element_text(size = 15),text = element_text(size=20)) + 
  guides(fill = guide_colorbar( label.position = "top", label.theme = element_text(angle = 0),
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= 0.2,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1)) 

#SO2 lockdown

so2.facility.loc <- as.data.table(read.fst( "data/all.facility.so2.2020.fst"))

so2.facility.loc <- so2.facility.loc %>%  filter (week>8 & week <17)


#plotting US Map for all facility 
so2.facility.states.loc<- setDT(so2.facility.loc)[, .(ATT = sum(ATT, na.rm=TRUE),
                                                      Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                                      Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE),
                                                      Y.tr.bar=sum(Y.tr.bar,na.rm=T),
                                                      Y.ct.bar=sum(Y.ct.bar,na.rm=T)),
                                                  by = .(STATE)]


names(so2.facility.states.loc)[names(so2.facility.states.loc) == 'STATE'] <- 'state'


so2.loc <- plot_usmap(data = as.data.frame(so2.facility.states.loc), values = "ATT", color = "black", labels = F,  exclude = c("AK", "HI")) +
  scale_fill_gradient2(limits=c(-500, 4000),
                       low="blue", mid= "white", high="red", name = expression(paste(TTT[lockdown], " ", SO[2] , " tons"))) +
  theme(legend.position = "top", legend.direction = "horizontal" ,
        legend.text = element_text(size = 12),text = element_text(size=20)) + 
  guides(fill = guide_colorbar( label.position = "top", label.theme = element_text(angle = 90),
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= 0.2,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1)) 




#percentage of actual emissions
so2.facility.states.loc$pct <- (so2.facility.states.loc$ATT*100)/(so2.facility.states.loc$Y.tr.bar)

so2.loc.pct <- plot_usmap(data = as.data.frame(so2.facility.states.loc), values = "pct", color = "black", labels = F,  exclude = c("AK", "HI")) +
  scale_fill_gradient2(limits=c(-100, 100), oob = scales::squish,
                       low="blue", mid= "white", high="red", name = expression(paste(TTT[lockdown], " ",SO[2] , " ,% change"))) +
  theme(legend.position = "top", legend.direction = "horizontal" ,
        legend.text = element_text(size = 15),text = element_text(size=20)) + 
  guides(fill = guide_colorbar( label.position = "top", label.theme = element_text(angle = 0),
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= 0.2,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1)) 

#CO2 lockdown

co2.facility.loc <- as.data.table(read.fst( "data/all.facility.co2.2020.fst"))

co2.facility.loc <- co2.facility.loc %>%  filter (week>8 & week <17)

#plotting US Map for all facility 
co2.facility.states.loc<- setDT(co2.facility.loc)[, .(ATT = sum(ATT, na.rm=TRUE),
                                                      Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                                      Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE),
                                                      Y.tr.bar=sum(Y.tr.bar,na.rm=T),
                                                      Y.ct.bar=sum(Y.ct.bar,na.rm=T)),
                                                  by = .(STATE)]



names(co2.facility.states.loc)[names(co2.facility.states.loc) == 'STATE'] <- 'state'

co2.facility.states.loc$ATT <- co2.facility.states.loc$ATT/10e6

co2.loc <- plot_usmap(data = as.data.frame(co2.facility.states.loc), values = "ATT", color = "black", labels = F,  exclude = c("AK", "HI")) +
  scale_fill_gradient2(limits=c(-0.25, 0.25), 
                       low="blue", mid= "white", high="red", name = expression(paste(TTT[lockdown], " ",CO[2] , ",", 10^6, " tons"))) +
  theme(legend.position = c( 0.25, .9), legend.direction = "horizontal" ,
        legend.text = element_text(size = 15),text = element_text(size=20)) + 
  guides(fill = guide_colorbar( label.position = "top", label.hjust = 0, label.theme = element_text(angle = 90),
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= -0.1,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1)) 

#percentage of actual emissions
co2.facility.states.loc$pct <- (co2.facility.states.loc$ATT*100*10e6)/(co2.facility.states.loc$Y.tr.bar)

co2.loc.pct <- plot_usmap(data = as.data.frame(co2.facility.states.loc), values = "pct", color = "black", 
                          labels = F,  exclude = c("AK", "HI")) +
  scale_fill_gradient2(limits=c(-100, 100),  oob = scales::squish,
                       low="blue", mid= "white", high="red", name = expression(paste(TTT[lockdown], " ",CO[2] , " ,% change"))) +
  theme(legend.position = "top", legend.direction = "horizontal" ,
        legend.text = element_text(size = 15),text = element_text(size=20)) + 
  guides(fill = guide_colorbar( label.position = "top", label.theme = element_text(angle = 0),
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= 0.2,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1)) 
# ggarrange(nox,  so2, co2, labels = c("A", "B", "C"), ncol = 3, nrow = 1, label.x = 0.05, label.y = 0.9,
#           common.legend = F, legend = "bottom") 
# ggsave("loc_nox_so2_co2_2020.png", path = "./plots/", width=15, height=5, units="in")
# 
# 

#Longterm


nox.facility <- as.data.table(read.fst( "data/all.facility.nox.2020.fst"))

nox.facility <- nox.facility %>%  filter (week>8)


#plotting US Map for all facility 
nox.facility.states<- setDT(nox.facility)[, .(ATT = sum(ATT, na.rm=TRUE),
                                              Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                              Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE),
                                              Y.tr.bar=sum(Y.tr.bar,na.rm=T),
                                              Y.ct.bar=sum(Y.ct.bar,na.rm=T)),
                                          by = .(STATE)]


names(nox.facility.states)[names(nox.facility.states) == 'STATE'] <- 'state'


nox.long <- plot_usmap(data = as.data.frame(nox.facility.states), values = "ATT", color = "black", labels = F,  exclude = c("AK", "HI")) +
  scale_fill_gradient2(limits=c(-3000, 9000), 
                       low="blue", mid= "white", high="red", name = expression(paste(TTT[longterm], " ", NO[x] , " tons"))) +
  theme(legend.position = c( 0.25, .9), legend.direction = "horizontal" ,
        legend.text = element_text(size = 10),text = element_text(size=20)) + 
  guides(fill = guide_colorbar( label.position = "top", label.theme = element_text(angle = 90),
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= 0.3,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1)) 

#percentage of actual emissions
nox.facility.states$pct <- (nox.facility.states$ATT*100)/(nox.facility.states$Y.tr.bar)

nox.long.pct <- plot_usmap(data = as.data.frame(nox.facility.states), values = "pct", color = "black", 
                           labels = F,  exclude = c("AK", "HI")) +
  scale_fill_gradient2(limits=c(-100, 100), oob = scales::squish,
                       low="blue", mid= "white", high="red", name = expression(paste(TTT[longterm], " ",NO[x] , " ,% change"))) +
  theme(legend.position = "top", legend.direction = "horizontal" ,
        legend.text = element_text(size = 15),text = element_text(size=20)) + 
  guides(fill = guide_colorbar( label.position = "top", label.theme = element_text(angle = 0),
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= 0.2,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1)) 


#longterm SO2 
so2.facility <- as.data.table(read.fst( "data/all.facility.so2.2020.fst"))

so2.facility <- so2.facility %>%  filter (week>8)


#plotting US Map for all facility 
so2.facility.states<- setDT(so2.facility)[, .(ATT = sum(ATT, na.rm=TRUE),
                                              Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                              Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE),
                                              Y.tr.bar=sum(Y.tr.bar,na.rm=T),
                                              Y.ct.bar=sum(Y.ct.bar,na.rm=T)),
                                          by = .(STATE)]


names(so2.facility.states)[names(so2.facility.states) == 'STATE'] <- 'state'


so2.long <- plot_usmap(data = as.data.frame(so2.facility.states), values = "ATT", color = "black", labels = F,  exclude = c("AK", "HI")) +
  scale_fill_gradient2(limits=c(-5000, 25000),
                       low="blue", mid= "white", high="red", name = expression(paste(TTT[longterm]," ", SO[2] , " tons"))) +
  theme(legend.position = c( 0.25, .9), legend.direction = "horizontal" ,
        legend.text = element_text(size = 10),text = element_text(size=20)) + 
  guides(fill = guide_colorbar( label.position = "top", label.theme = element_text(angle = 90),
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= 0.3,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 15,
                                barheight = 1)) 



#percentage of actual emissions
so2.facility.states$pct <- (so2.facility.states$ATT*100)/(so2.facility.states$Y.tr.bar)

so2.long.pct <- plot_usmap(data = as.data.frame(so2.facility.states), values = "pct", color = "black", labels = F,  exclude = c("AK", "HI")) +
  scale_fill_gradient2(limits=c(-100, 100), oob = scales::squish,
                       low="blue", mid= "white", high="red", name = expression(paste(TTT[longterm], " ",SO[2] , " ,% change"))) +
  theme(legend.position = "top", legend.direction = "horizontal" ,
        legend.text = element_text(size = 15),text = element_text(size=20)) + 
  guides(fill = guide_colorbar( label.position = "top", label.theme = element_text(angle = 0),
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= 0.2,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1)) 
#CO2

co2.facility <- as.data.table(read.fst( "data/all.facility.co2.2020.fst"))

co2.facility <- co2.facility %>%  filter (week>8)


#plotting US Map for all facility 
co2.facility.states<- setDT(co2.facility)[, .(ATT = sum(ATT, na.rm=TRUE),
                                              Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                              Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE),
                                              Y.tr.bar=sum(Y.tr.bar,na.rm=T),
                                              Y.ct.bar=sum(Y.ct.bar,na.rm=T)),
                                          by = .(STATE)]


names(co2.facility.states)[names(co2.facility.states) == 'STATE'] <- 'state'

co2.facility.states$ATT <- co2.facility.states$ATT/10e6

co2.long <- plot_usmap(data = as.data.frame(co2.facility.states), values = "ATT", color = "black", labels = F,  exclude = c("AK", "HI")) +
  scale_fill_gradient2(limits=c(-0.5, 1.1),
                       low="blue", mid= "white", high="red", name = expression(paste(TTT[longterm], " ", CO[2] , ",", 10^6, " tons"))) +
  theme(legend.position = c( 0.25, .9), legend.direction = "horizontal" ,
        legend.text = element_text(size = 10),text = element_text(size=20)) + 
  guides(fill = guide_colorbar( label.position = "top", label.theme = element_text(angle = 90),
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= 0.3,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1)) 

#percentage of actual emissions
co2.facility.states$pct <- (co2.facility.states$ATT*100*10e6)/(co2.facility.states$Y.tr.bar)

co2.long.pct <- plot_usmap(data = as.data.frame(co2.facility.states), values = "pct", color = "black", 
                          labels = F,  exclude = c("AK", "HI")) +
  scale_fill_gradient2(limits=c(-100, 100),  oob = scales::squish,
                       low="blue", mid= "white", high="red", name = expression(paste(TTT[longterm], " ",CO[2] , " ,% change"))) +
  theme(legend.position = "top", legend.direction = "horizontal" ,
        legend.text = element_text(size = 15),text = element_text(size=20)) + 
  guides(fill = guide_colorbar( label.position = "top", label.theme = element_text(angle = 0),
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= 0.2,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1)) 
# ggarrange(nox.long,  so2.long, co2.long, labels = c("A", "B", "C"), ncol = 3, nrow = 1, label.x = 0.05, label.y = 0.9,
#           common.legend = F, legend = "bottom")
# 
# ggsave("long_nox_so2_co2_2020.png", path = "./plots/", width=15, height=5, units="in")






#actual vs counterfactual

#Figure 3 comparing nox, so2 and co2 actual vs counterfactual emissions

######actual counterfactual weekly


so2.2020 <- read.fst("data/all.facility.so2.2020.fst")

#GSYNTH  and actual

so2.gsynth.actual<- setDT(so2.2020)[, .(actual.so2.emis = sum(Y.tr.bar, na.rm=T),
                                        ct.so2.emis = sum(Y.ct.bar, na.rm=T)), 
                                    by = .(week)]

names(so2.gsynth.actual)[names(so2.gsynth.actual) == 'ct.so2.emis'] <- 'gsynth.counterfactual.2020'
names(so2.gsynth.actual)[names(so2.gsynth.actual) == 'actual.so2.emis'] <- 'actual.2020'

df <- melt(so2.gsynth.actual, id.vars = "week", measure.vars = c("actual.2020", "gsynth.counterfactual.2020"))


names(df)[names(df) == 'variable'] <- 'group'
names(df)[names(df) == 'value'] <- 'SO2.tons'

so2 <- df %>% mutate(group = recode(group, "actual.2020" = 'actual', "gsynth.counterfactual.2020" = "counterfactual"))



so2.2020.compare <- so2%>%  ggplot(aes(x=week, y=SO2.tons, color=group)) + geom_line(size=1)  +
  geom_vline(xintercept=8) +
  geom_vline(xintercept=16) + 
  annotate("rect", xmin = 8, xmax = 16, ymin = 0, ymax = Inf, fill = "green", alpha = .1, color = NA) +
  annotate("rect", xmin = 8, xmax = 52, ymin = 0, ymax = Inf, fill = "blue", alpha = .1, color = NA) +
  theme_bw() + scale_x_continuous(expand = c(0, 0), limits = c(0, 52)) +
  scale_color_manual( values = c("red", "blue"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 16000)) + theme (legend.position = "none",
                                                                      legend.title = element_blank(),
                                                                      legend.text = element_text(size = 15),
                                                                      axis.text.x = element_text(size=15),
                                                                      axis.text.y = element_text(size=15),
                                                                      axis.title = element_text(size=20)) +
  labs(x="Weeks in 2020", y="", title = expression(paste(SO[2], " emissions, tons"))) 

# ggsave("compare_so2_2020.png", path = "./plots/")

#nox. compare

#####actual counterfactual weekly


nox.2020 <- read.fst("data/all.facility.nox.2020.fst")

#GSYNTH  and actual

nox.gsynth.actual<- setDT(nox.2020)[, .(actual.nox.emis = sum(Y.tr.bar, na.rm=T),
                                        ct.nox.emis = sum(Y.ct.bar, na.rm=T)), 
                                    by = .(week)]

names(nox.gsynth.actual)[names(nox.gsynth.actual) == 'ct.nox.emis'] <- 'gsynth.counterfactual.2020'
names(nox.gsynth.actual)[names(nox.gsynth.actual) == 'actual.nox.emis'] <- 'actual.2020'

df <- melt(nox.gsynth.actual, id.vars = "week", measure.vars = c("actual.2020", "gsynth.counterfactual.2020"))


names(df)[names(df) == 'variable'] <- 'group'
names(df)[names(df) == 'value'] <- 'nox.tons'

df <- df [order(week), ]

nox <- df %>% mutate(group = recode(group, "actual.2020" = 'actual', "gsynth.counterfactual.2020" = "counterfactual"))


nox.2020.compare <-nox%>%  ggplot(aes(x=week, y=nox.tons, color=group)) +  geom_line(size=1)  +
  geom_vline(xintercept=8) +
  geom_vline(xintercept=16) + 
  annotate("rect", xmin = 8, xmax = 16, ymin = 0, ymax = Inf, fill = "green", alpha = .1, color = NA) +
  annotate("rect", xmin = 8, xmax = 52, ymin = 0, ymax = Inf, fill = "blue", alpha = .1, color = NA) +
  theme_bw() + scale_x_continuous(expand = c(0, 0), limits = c(0, 52)) +
  scale_color_manual( values = c("red", "blue"))+ 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 16000)) + theme (legend.position = c(0.63,0.2),
                                                                      legend.title = element_blank(),
                                                                      legend.text = element_text(size = 15),
                                                                      axis.text.x = element_text(size=15),
                                                                      axis.text.y = element_text(size=15),
                                                                      axis.title = element_text(size=20)) +
  labs(x="Weeks in 2020", y="", title = expression(paste(NO[x], " emissions, tons"))) 

# ggsave("compare_nox_2020.png", path = "./plots/")


#CO2 compare

co2.2020 <- read.fst("data/all.facility.co2.2020.fst")

#GSYNTH  and actual

co2.gsynth.actual<- setDT(co2.2020)[, .(actual.co2.emis = sum(Y.tr.bar, na.rm=T),
                                        ct.co2.emis = sum(Y.ct.bar, na.rm=T)), 
                                    by = .(week)]

names(co2.gsynth.actual)[names(co2.gsynth.actual) == 'ct.co2.emis'] <- 'counterfactual'
names(co2.gsynth.actual)[names(co2.gsynth.actual) == 'actual.co2.emis'] <- 'actual'

df <- melt(co2.gsynth.actual, id.vars = "week", measure.vars = c("actual", "counterfactual"))


names(df)[names(df) == 'variable'] <- 'group'
names(df)[names(df) == 'value'] <- 'co2.tons'

df <- df [order(week), ]

df$co2.tons <- df$co2.tons/10e6


co2.2020.compare <- df%>%  ggplot(aes(x=week, y=co2.tons, color=group))  + geom_line(size=1)  +
  geom_vline(xintercept=8) +
  geom_vline(xintercept=16) + 
  annotate("rect", xmin = 8, xmax = 16, ymin = 0, ymax = Inf, fill = "green", alpha = .1, color = NA) +
  annotate("rect", xmin = 8, xmax = 52, ymin = 0, ymax = Inf, fill = "blue", alpha = .1, color = NA) +
  theme_bw() + scale_x_continuous(expand = c(0, 0), limits = c(0, 52)) +
  scale_color_manual( values = c("red", "blue"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2.5)) + theme (legend.position = "none",
                                                                    legend.title = element_blank(),
                                                                    legend.text = element_text(size = 15),
                                                                    axis.text.x = element_text(size=15),
                                                                    axis.text.y = element_text(size=15),
                                                                    axis.title = element_text(size=20)) +
  labs(x="Weeks in 2020", y = "", title = expression(paste( CO[2], " Emissions", ", ", 10^6," tons"))) 

ggarrange(so2.2020.compare, nox.2020.compare,   co2.2020.compare, ncol = 3, nrow = 1) 


ggsave("compare_nox_so2_co2_2020.png", path = "./plots/awma", width=14, height=4, units="in")


ggarrange(so2.loc, nox.loc,   co2.loc, so2.long, nox.long, co2.long, 
           ncol = 3, nrow = 2,  common.legend = F, legend = "bottom") 

ggsave("spatial_maps_all.png", path = "./plots/awma/", width=22, height=9, units="in")


ggarrange(so2.loc.pct, nox.loc.pct,   co2.loc.pct, so2.long.pct, nox.long.pct, co2.long.pct, 
          ncol = 3, nrow = 2,  common.legend = F, legend = "bottom") 

ggsave("spatial_maps_all_pct.png", path = "./plots/awma/", width=22, height=9, units="in")

# =======================================================================================================
#Graphical Abstract===================================================================
# =======================================================================================================
plot_compare <- ggarrange(so2.2020.compare,nox.2020.compare,  co2.2020.compare, 
                          ncol = 3, nrow = 1) 

annotate_figure(plot_compare, top = text_grob("Temporal trend of power plant emissions in the U.S. in 2020", 
                                              color = "black", face = "bold", size = 22))

ggsave("actual_vs_counterfactual.png", path = "./plots/", width=14, height=4, units="in")


# =======================================================================================================
#Regression analysis for SO2 and NOx===================================================================
# =======================================================================================================


#so2 regression


so2.facility <- as.data.table(read.fst( "data/all.facility.so2.2020.fst"))

so2.facility <- so2.facility %>% filter (week>8)

so2.facility<- setDT(so2.facility)[, .(TTT = sum(ATT, na.rm=TRUE),
                                       actual.so2.emis = sum(Y.tr.bar, na.rm=T),
                                       ct.so2.emis = sum(Y.ct.bar, na.rm=T)), 
                                   by = .(facility, STATE,Fuel.Type..Primary.,Facility.Latitude,Facility.Longitude,
                                          Source.Category, Unit.Type, SO2.Control.s.,SO2.Control.s.,
                                          PM.Control.s., Hg.Control.s., EPA.Region, NERC.Region,
                                          Associated.Stacks,Program.s.,SO2.Phase,SO2.Phase)]

#subtracting mean ATT from total treatment on treated 
# so2.facility$ttt.mean <- so2.facility$TTT--mean(so2.facility$TTT)

#primary fuel
so2.facility[, Fuel1.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Primary.))]
so2.facility[Fuel.Type..Primary. == "", Fuel1.IsCoal := NA]

so2.facility[, Fuel1.IsNatGas := as.numeric(grepl("Natural Gas", Fuel.Type..Primary.))]
so2.facility[Fuel.Type..Primary. == "", Fuel1.IsNatGas  := NA]

other.fuel <- c("Other Gas", "Diesel Oil", "Wood", "Process Gas", "Residual Oil", "Petroleum Coke",
                "Other Oil") 
so2.facility[, Fuel1.IsOthers := as.numeric( Fuel.Type..Primary. %in% other.fuel)]
so2.facility[Fuel.Type..Primary. == "", Fuel1.IsOthers  := NA]

#so2 control
so2.facility[, Has.SO2.Control := "Has SO2 Control"]
so2.facility[SO2.Control.s. == "", Has.SO2.Control := "Has No SO2 Control"]

so2.facility$Has.SO2.Control <- as.factor(so2.facility$Has.SO2.Control)


so2.facility$Fuel <- "Others"
so2.facility$Fuel [ so2.facility$Fuel.Type..Primary.=="Coal"] <- "Coal"
so2.facility$Fuel [ grepl("Natural Gas", so2.facility$Fuel.Type..Primary.)] <- "Natural Gas"

so2.facility$Fuel <- as.factor(so2.facility$Fuel)


so2.facility$EPA.Region <- as.factor(so2.facility$EPA.Region)



#SO2
# eqn <- TTT ~ -1 +EPA.Region * Fuel  *Has.SO2.Control #+ Unit.Type +NOx.Phase + Source.Category #without intercept -1

eqn <- TTT ~ -1 + EPA.Region : Fuel  : Has.SO2.Control
#controls
r.lm <- lm(eqn, data = so2.facility)

res <- summary(r.lm)
conf <- confint(r.lm)

x <- as.data.frame(res$coefficients)
x <- setNames(cbind(rownames(x), x, row.names = NULL), 
              c("variable", "estimate", "Std. Error" ,"t value"  ,  "Pr(>|t|)"))

y <- as.data.frame(conf)
y <- setNames(cbind(rownames(y), y, row.names = NULL), 
              c("variable"  , "2.5 %" ,"97.5 %"))
y <- na.omit(y)

variable <- as.vector(x$variable)

lm_df <- data.frame("Variable" = variable,
                    "estimate" = x[,2], "ci0.025" = y[,2], "ci0.975" = y[,3])

so2_df <- lm_df

so2_df$fuel.group[grepl("Coal", so2_df$Variable)] <- "Coal"
so2_df$fuel.group[grepl("Natural Gas", so2_df$Variable)] <- "Natural Gas"
so2_df$fuel.group[grepl("Others", so2_df$Variable)] <- "Others"

so2_df$control.group[grepl("Has.SO2.ControlHas SO2 Control", so2_df$Variable)] <- "Has SO2 Control"
so2_df$control.group[grepl("Has.SO2.ControlHas No SO2 Control", so2_df$Variable)] <- "Has No SO2 Control"

so2_df$region.group <- sub("\\:.*", "", so2_df$Variable)


so2_df <- so2_df %>% 
  mutate(region.group = recode(region.group, "EPA.Region1"  = '1',
                               "EPA.Region2"  = '2',
                               "EPA.Region3"  = '3',
                               "EPA.Region4"  = '4',
                               "EPA.Region5"  = '5',
                               "EPA.Region6"  = '6',
                               "EPA.Region7"  = '7',
                               "EPA.Region8"  = '8',
                               "EPA.Region9"  = '9',
                               "EPA.Region10"  = '10'))

so2_df$region.group <- factor(so2_df$region.group, levels=c("1",  "2",
                                                            "3",  "4",  "5",  "6",  "7",
                                                            "8",  "9",   "10"))


so2.regression <- so2_df %>%  group_by (fuel.group) %>%   ggplot(aes(color=fuel.group)) +
  geom_pointrange( mapping = aes(x = fuel.group, y = estimate, 
                                 ymin = ci0.025, ymax = ci0.975),size=0.8) + coord_flip() +
  geom_hline(yintercept = 0) + theme( legend.title = element_blank(),
                                      legend.position = "none",
                                      axis.title.y = element_blank(),
                                      axis.title.x = element_text(size=15),
                                      axis.text = element_text(size=15,face="bold"),
                                      strip.text.x = element_text(size = 20),
                                      strip.text.y = element_text(size = 20)) +
  labs(y=expression(paste( "Average ",SO[2], " Emission change", ", "," tons"))) + 
  scale_color_brewer( palette = 'Dark2') +
  facet_grid(region.group~control.group)

ggsave("so2_regression.png", path = "./plots/awma", width=10, height=10, units="in")

#region Map
region_states <- ampd_raw  %>% dplyr::select(STATE,EPA.Region)
region_states <- region_states %>% na.omit() %>% distinct()
region_states$EPA.Region <- as.factor(region_states$EPA.Region)

names(region_states)[names(region_states) == 'STATE'] <- 'state'

plot_usmap(data = as.data.frame(region_states), values = "EPA.Region", color = "black", labels = F,  exclude = c("AK", "HI")) +
  theme(legend.position = "bottom", legend.direction = "horizontal" ,
        legend.text = element_text(size = 15),text = element_text(size=20)) + 
  scale_fill_discrete(name = "EPA Region")

ggsave("epa.region.png", path = "./plots/awma", width=10, height=5, units="in")

#NOx regression


nox.facility <- as.data.table(read.fst( "data/all.facility.nox.2020.fst"))

nox.facility <- nox.facility %>% filter (week>8)

nox.facility<- setDT(nox.facility)[, .(TTT = sum(ATT, na.rm=TRUE),
                                       actual.nox.emis = sum(Y.tr.bar, na.rm=T),
                                       ct.nox.emis = sum(Y.ct.bar, na.rm=T)), 
                                   by = .(facility, STATE,Fuel.Type..Primary.,Facility.Latitude,Facility.Longitude,
                                          Source.Category, Unit.Type, NOx.Control.s.,
                                          PM.Control.s., Hg.Control.s., EPA.Region, NERC.Region,
                                          Associated.Stacks,Program.s.,NOx.Phase)]

#subtracting mean ATT from total treatment on treated 
# nox.facility$ttt.mean <- nox.facility$TTT--mean(nox.facility$TTT)

#primary fuel
nox.facility[, Fuel1.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Primary.))]
nox.facility[Fuel.Type..Primary. == "", Fuel1.IsCoal := NA]

nox.facility[, Fuel1.IsNatGas := as.numeric(grepl("Natural Gas", Fuel.Type..Primary.))]
nox.facility[Fuel.Type..Primary. == "", Fuel1.IsNatGas  := NA]

other.fuel <- c("Other Gas", "Diesel Oil", "Wood", "Process Gas", "Residual Oil", "Petroleum Coke",
                "Other Oil") 
nox.facility[, Fuel1.IsOthers := as.numeric( Fuel.Type..Primary. %in% other.fuel)]
nox.facility[Fuel.Type..Primary. == "", Fuel1.IsOthers  := NA]

#nox control
nox.facility[, Has.NOx.Control := "Has NOx Control"]
nox.facility[NOx.Control.s. == "", Has.NOx.Control := "Has No NOx Control"]

nox.facility$Has.NOx.Control <- as.factor(nox.facility$Has.NOx.Control)


nox.facility$Fuel <- "Others"
nox.facility$Fuel [ nox.facility$Fuel.Type..Primary.=="Coal"] <- "Coal"
nox.facility$Fuel [ grepl("Natural Gas", nox.facility$Fuel.Type..Primary.)] <- "Natural Gas"

nox.facility$Fuel <- as.factor(nox.facility$Fuel)


nox.facility$EPA.Region <- as.factor(nox.facility$EPA.Region)

#nox
# eqn <- TTT ~ -1 +EPA.Region * Fuel  *Has.nox.Control #+ Unit.Type +NOx.Phase + Source.Category #without intercept -1

eqn <- TTT ~ -1 + EPA.Region : Fuel  : Has.NOx.Control
#controls
r.lm <- lm(eqn, data = nox.facility)

res <- summary(r.lm)
conf <- confint(r.lm)

x <- as.data.frame(res$coefficients)
x <- setNames(cbind(rownames(x), x, row.names = NULL), 
              c("variable", "estimate", "Std. Error" ,"t value"  ,  "Pr(>|t|)"))

y <- as.data.frame(conf)
y <- setNames(cbind(rownames(y), y, row.names = NULL), 
              c("variable"  , "2.5 %" ,"97.5 %"))
y <- na.omit(y)

variable <- as.vector(x$variable)

lm_df <- data.frame("Variable" = variable,
                    "estimate" = x[,2], "ci0.025" = y[,2], "ci0.975" = y[,3])

nox_df <- lm_df

nox_df$fuel.group[grepl("Coal", nox_df$Variable)] <- "Coal"
nox_df$fuel.group[grepl("Natural Gas", nox_df$Variable)] <- "Natural Gas"
nox_df$fuel.group[grepl("Others", nox_df$Variable)] <- "Others"

nox_df$control.group[grepl("Has.NOx.ControlHas NOx Control", nox_df$Variable)] <- "Has NOx Control"
nox_df$control.group[grepl("Has.NOx.ControlHas No NOx Control", nox_df$Variable)] <- "Has No NOx Control"

nox_df$control.group[grepl("Has.NOx.ControlHas No NOx Control", nox_df$Variable)] <- "Has No NOx Control"

nox_df$region.group <- sub("\\:.*", "", nox_df$Variable)



nox_df <- nox_df %>% 
  mutate(region.group = recode(region.group, "EPA.Region1"  = '1',
                               "EPA.Region2"  = '2',
                               "EPA.Region3"  = '3',
                               "EPA.Region4"  = '4',
                               "EPA.Region5"  = '5',
                               "EPA.Region6"  = '6',
                               "EPA.Region7"  = '7',
                               "EPA.Region8"  = '8',
                               "EPA.Region9"  = '9',
                               "EPA.Region10"  = '10'))

nox_df$region.group <- factor(nox_df$region.group, levels=c("1",  "2",
                                                            "3",  "4",  "5",  "6",  "7",
                                                            "8",  "9",   "10"))


nox.regression <- nox_df %>%  group_by (fuel.group) %>%   ggplot(aes(color=fuel.group)) +
  geom_pointrange( mapping = aes(x = fuel.group, y = estimate, 
                                 ymin = ci0.025, ymax = ci0.975),size=0.8) + coord_flip() +
  geom_hline(yintercept = 0) + theme( legend.title = element_blank(),
                                      legend.position = "none",
                                      axis.title.y = element_blank(),
                                      axis.title.x = element_text(size=15),
                                      axis.text = element_text(size=15,face="bold"),
                                      strip.text.x = element_text(size = 20),
                                      strip.text.y = element_text(size = 20)) +
  labs(y=expression(paste( "Average ",NO[X], " Emission change", ", "," tons"))) + 
  scale_color_brewer( palette = 'Dark2') +
  facet_grid(region.group~control.group)



ggsave("nox_regression.png", path = "./plots/awma", width=10, height=9, units="in")

#CO2 regression


co2.facility <- as.data.table(read.fst( "data/all.facility.co2.2020.fst"))

co2.facility <- co2.facility %>% filter (week>8)

co2.facility<- setDT(co2.facility)[, .(TTT = sum(ATT, na.rm=TRUE),
                                       actual.co2.emis = sum(Y.tr.bar, na.rm=T),
                                       ct.co2.emis = sum(Y.ct.bar, na.rm=T)), 
                                   by = .(facility, STATE,Fuel.Type..Primary.,Facility.Latitude,Facility.Longitude,
                                          Source.Category, Unit.Type, SO2.Control.s.,SO2.Control.s.,
                                          PM.Control.s., Hg.Control.s., EPA.Region, NERC.Region,
                                          Associated.Stacks,Program.s.,SO2.Phase,SO2.Phase)]

#subtracting mean ATT from total treatment on treated 
# so2.facility$ttt.mean <- so2.facility$TTT--mean(so2.facility$TTT)

#primary fuel
co2.facility[, Fuel1.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Primary.))]
co2.facility[Fuel.Type..Primary. == "", Fuel1.IsCoal := NA]

co2.facility[, Fuel1.IsNatGas := as.numeric(grepl("Natural Gas", Fuel.Type..Primary.))]
co2.facility[Fuel.Type..Primary. == "", Fuel1.IsNatGas  := NA]

other.fuel <- c("Other Gas", "Diesel Oil", "Wood", "Process Gas", "Residual Oil", "Petroleum Coke",
                "Other Oil") 
co2.facility[, Fuel1.IsOthers := as.numeric( Fuel.Type..Primary. %in% other.fuel)]
co2.facility[Fuel.Type..Primary. == "", Fuel1.IsOthers  := NA]

#co2 control
co2.facility[, Has.SO2.Control := "Has SO2 Control"]
co2.facility[SO2.Control.s. == "", Has.SO2.Control := "Has No SO2 Control"]

co2.facility$Has.SO2.Control <- as.factor(co2.facility$Has.SO2.Control)


co2.facility$Fuel <- "Others"
co2.facility$Fuel [ co2.facility$Fuel.Type..Primary.=="Coal"] <- "Coal"
co2.facility$Fuel [ grepl("Natural Gas", co2.facility$Fuel.Type..Primary.)] <- "Natural Gas"

co2.facility$Fuel <- as.factor(co2.facility$Fuel)


co2.facility$EPA.Region <- as.factor(co2.facility$EPA.Region)

co2.facility$TTT <- co2.facility$TTT/10e6
#SO2
# eqn <- TTT ~ -1 +EPA.Region * Fuel  *Has.SO2.Control #+ Unit.Type +NOx.Phase + Source.Category #without intercept -1

eqn <- TTT ~ -1 + EPA.Region : Fuel  #: Has.SO2.Control
#controls
r.lm <- lm(eqn, data = co2.facility)

res <- summary(r.lm)
conf <- confint(r.lm)

x <- as.data.frame(res$coefficients)
x <- setNames(cbind(rownames(x), x, row.names = NULL), 
              c("variable", "estimate", "Std. Error" ,"t value"  ,  "Pr(>|t|)"))

y <- as.data.frame(conf)
y <- setNames(cbind(rownames(y), y, row.names = NULL), 
              c("variable"  , "2.5 %" ,"97.5 %"))
y <- na.omit(y)

variable <- as.vector(x$variable)

lm_df <- data.frame("Variable" = variable,
                    "estimate" = x[,2], "ci0.025" = y[,2], "ci0.975" = y[,3])

co2_df <- lm_df

co2_df$fuel.group[grepl("Coal", co2_df$Variable)] <- "Coal"
co2_df$fuel.group[grepl("Natural Gas", co2_df$Variable)] <- "Natural Gas"
co2_df$fuel.group[grepl("Others", co2_df$Variable)] <- "Others"

# co2_df$control.group[grepl("Has.SO2.ControlHas SO2 Control", co2_df$Variable)] <- "Has SO2 Control"
# co2_df$control.group[grepl("Has.SO2.ControlHas No SO2 Control", co2_df$Variable)] <- "Has No SO2 Control"

co2_df$region.group <- sub("\\:.*", "", co2_df$Variable)


co2_df <- co2_df %>% 
  mutate(region.group = recode(region.group, "EPA.Region1"  = '1',
                               "EPA.Region2"  = '2',
                               "EPA.Region3"  = '3',
                               "EPA.Region4"  = '4',
                               "EPA.Region5"  = '5',
                               "EPA.Region6"  = '6',
                               "EPA.Region7"  = '7',
                               "EPA.Region8"  = '8',
                               "EPA.Region9"  = '9',
                               "EPA.Region10"  = '10'))

co2_df$region.group <- factor(co2_df$region.group, levels=c("1",  "2",
                                                            "3",  "4",  "5",  "6",  "7",
                                                            "8",  "9",   "10"))


co2.regression <- co2_df %>%  group_by (fuel.group) %>%   ggplot(aes(color=fuel.group)) +
  geom_pointrange( mapping = aes(x = fuel.group, y = estimate, 
                                 ymin = ci0.025, ymax = ci0.975),size=0.8) + coord_flip() +
  geom_hline(yintercept = 0) + theme( legend.title = element_blank(),
                                      legend.position = "none",
                                      axis.title.y = element_blank(),
                                      axis.title.x = element_text(size=15),
                                      axis.text = element_text(size=15,face="bold"),
                                      strip.text.x = element_text(size = 20),
                                      strip.text.y = element_text(size = 20)) +
  labs(y=expression(paste( "Average ",CO[2], " Emission change", ", "," tons"))) + 
  scale_color_brewer( palette = 'Dark2') +
  facet_grid(region.group~.)

ggsave("co2_regression.png", path = "./plots/awma", width=6, height=9, units="in")



#comparing coal SO2 emissions vs EPA AQS Sulfate and Sulfate emission during March-December 2020



aqs.data.mn <- read.fst("./data/aqs.1999.2020.monthly.fst")


#lockdown sulfate concentration March - April 2020


aqs.monthly <- aqs.data.mn%>% filter(  year==2020 & month>2 & month <=4 & chemicals=="Sulfate")

aqs.monthly <- setDT(aqs.monthly)[, .(mean = mean(mean, na.rm=TRUE)), 
                                  by = .(ID, Latitude, Longitude)]

aqs.monthly$group[aqs.monthly$mean <= 1] <- '<1'
aqs.monthly$group[aqs.monthly$mean > 1 & aqs.monthly$mean <= 2] <- '1-2'
aqs.monthly$group[aqs.monthly$mean > 2 ] <- '>2'




locs.sf <- st_as_sf( aqs.monthly, 
                     coords = c( 'Longitude', 'Latitude'),
                     crs = 'WGS84')


locs.sf$group <- factor(locs.sf$group, levels = c("<1", "1-2", ">2"))



# plot( locs.sf)


## let's load in some US boundary data
us_states <- USAboundaries::us_states()
us_counties <- USAboundaries::us_counties()

## if we plot the locations + US coundaries using ggplot, 
## they will automatically be converted to a consistent crs,
## but I think it's best to do this manually just to make sure
us_states.tran <- st_transform( us_states, crs = st_crs( locs.sf))
us_counties.tran <- st_transform( us_counties, crs = st_crs( locs.sf))

## now, a bigger better plot 
sulfate.loc <- ggplot( ) + 
  geom_sf( data = us_states.tran, color = 'black', fill = NA) + 
  geom_sf( data = locs.sf, aes( color = group)) + 
  # set the x and y limits
  coord_sf( xlim = c( -125, -67), ylim = c( 25, 50)) +
  theme_bw() +
  scale_color_manual( values = c("#E7B800", "#2E9FDF", "#FC4E07"),
                      name = expression(paste(SO[4]^-2, " ug/", m^3)))+
  theme(legend.position = c( 0.2, .12), legend.direction = "horizontal" ,
        legend.text = element_text(size = 10),text = element_text(size=10))

# ggsave("aqs_sulfate_lockdown.png", path = "./plots/")


#long term sulfate concentration March-Dec 2020
aqs.monthly <- aqs.data.mn%>% filter(  year==2020 & month>2 & month <=12 & chemicals=="Sulfate")

aqs.monthly <- setDT(aqs.monthly)[, .(mean = mean(mean, na.rm=TRUE)), 
                                  by = .(ID, Latitude, Longitude)]

aqs.monthly$group[aqs.monthly$mean <= 1] <- '<1'
aqs.monthly$group[aqs.monthly$mean > 1 & aqs.monthly$mean <= 2] <- '1-2'
aqs.monthly$group[aqs.monthly$mean > 2 ] <- '>2'




locs.sf <- st_as_sf( aqs.monthly, 
                     coords = c( 'Longitude', 'Latitude'),
                     crs = 'WGS84')

# plot( locs.sf)
locs.sf$group <- factor(locs.sf$group, levels = c("<1", "1-2", ">2"))

## let's load in some US boundary data
us_states <- USAboundaries::us_states()
us_counties <- USAboundaries::us_counties()

## if we plot the locations + US coundaries using ggplot, 
## they will automatically be converted to a consistent crs,
## but I think it's best to do this manually just to make sure
us_states.tran <- st_transform( us_states, crs = st_crs( locs.sf))
us_counties.tran <- st_transform( us_counties, crs = st_crs( locs.sf))

## now, a bigger better plot 
longterm.sulfate <- ggplot( ) + 
  geom_sf( data = us_states.tran, color = 'black', fill = NA) + 
  geom_sf( data = locs.sf, aes( color = group)) + 
  # set the x and y limits
  coord_sf( xlim = c( -125, -67), ylim = c( 25, 50)) +
  theme_bw()+
  scale_color_manual( values = c("#E7B800", "#2E9FDF", "#FC4E07"),
                      name = expression(paste(SO[4]^-2, " ug/", m^3)))+
  theme(legend.position = "none")

ggarrange(sulfate.loc,  longterm.sulfate, labels = c("A", "B"), ncol = 2, nrow = 1, 
          label.x = 0.9, label.y = 0.95) 


ggsave("aqs_sulfate_longterm.png", path = "./plots/", width=14, height=4, units="in")

#comparing coal SO2 emissions vs EPA AQS PM2.5 conc. during March-December 2020



aqs.data.mn <- read.fst("/Volumes/GoogleDrive/My Drive/R/spatial-temporal-PM2.5-chemicals/data/aqs.1999.2020.monthly.fst")

aqs.monthly <- aqs.data.mn%>% filter(  year==2020 & month>2 & month <=12 & chemicals=="PM2.5")

aqs.monthly <- setDT(aqs.monthly)[, .(mean = mean(mean, na.rm=TRUE)), 
                                  by = .(ID, Latitude, Longitude)]

aqs.monthly$group[aqs.monthly$mean < 8] <- '<8'
aqs.monthly$group[aqs.monthly$mean >=8 & aqs.monthly$mean <11] <- '8-11'
aqs.monthly$group[aqs.monthly$mean >=11 & aqs.monthly$mean <= 13] <- '11-13'
aqs.monthly$group[aqs.monthly$mean > 13 ] <- '>13'




locs.sf <- st_as_sf( aqs.monthly, 
                     coords = c( 'Longitude', 'Latitude'),
                     crs = 'WGS84')

plot( locs.sf)


## let's load in some US boundary data
us_states <- USAboundaries::us_states()
us_counties <- USAboundaries::us_counties()

## if we plot the locations + US coundaries using ggplot, 
## they will automatically be converted to a consistent crs,
## but I think it's best to do this manually just to make sure
us_states.tran <- st_transform( us_states, crs = st_crs( locs.sf))
us_counties.tran <- st_transform( us_counties, crs = st_crs( locs.sf))

## now, a bigger better plot 
ggplot( ) + 
  geom_sf( data = us_states.tran, color = 'black', fill = NA) + 
  geom_sf( data = locs.sf, aes( color = group)) + 
  # set the x and y limits
  coord_sf( xlim = c( -125, -67), ylim = c( 25, 50)) +
  theme_bw() 



