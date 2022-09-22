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
library(sf)

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



#monthly emission in 2020
years <- c (  2010:2020)
load ("data/facility_so2_2020.RData")
ampd_selected_facilities <- ampd_raw %>% filter (ORISPL_CODE %in% facility )

sum(ampd_selected_facilities$SO2..tons., na.rm=T)/10e6
sum(ampd_selected_facilities$NOx..tons., na.rm=T)/10e6
sum(ampd_selected_facilities$CO2..tons., na.rm=T)/10e6


ampd_ym<- aggregate(list (NOx..tons.=ampd_selected_facilities$NOx..tons., 
                          SO2..tons.=ampd_selected_facilities$SO2..tons.,
                          CO2..tons.= ampd_selected_facilities$CO2..tons.,
                          Gross.Load..MW.h.=ampd_selected_facilities$Gross.Load..MW.h.,
                          Steam.Load..1000lb.= ampd_selected_facilities$Steam.Load..1000lb.,
                          HEAT.INPUT= ampd_selected_facilities$HEAT.INPUT,
                          SUM_OP_TIME= ampd_selected_facilities$SUM_OP_TIME
), by=list( year=ampd_selected_facilities$year,
            month=ampd_selected_facilities $month), FUN=sum)

# ampd_ym %>% filter( year %in% years)  %>%
#   ggplot(aes(month, SO2..tons., color= year, group= year, )) +
#   geom_point() + geom_line(aes())+ labs(x= "Month", 
#                                         y = "SO2 Emission (tons)",
#                                         title = "") +
#   scale_x_continuous(breaks = seq(0, 12, by = 1))  +
#   theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
#          panel.background = element_rect(colour = "black", size=1,   fill=NA))


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

# ampd_y%>% filter (year >= 1997 ) %>%
#   ggplot(aes( year, NOx..tons., color= year)) + geom_point() + geom_line()

ampd_ym$NOx..tons. <- (ampd_ym$NOx..tons.)/10^6
ampd_ym$SO2..tons. <- (ampd_ym$SO2..tons.)/10^6
ampd_ym$CO2..tons. <- (ampd_ym$CO2..tons.)/10^6

#Yearly NOx emission:
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

ggarrange( plot2, plot1, plot3, labels = c("A", "B", "C"), ncol = 3, nrow = 1, label.x = 0.9, label.y = 0.85,
          common.legend = T, legend = "bottom") 

ggsave("nox_so2_co2_trend.png", path = "./plots/", width=16, height=5, units="in")


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
                       low="blue", mid= "white", high="red", name = expression(paste(ET[lockdown], " ",NO[x] , ", tons/week"))) +
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
                       low="blue", mid= "white", high="red", name = expression(paste(ET[lockdown], " ",NO[x] , " ,% change"))) +
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
                       low="blue", mid= "white", high="red", name = expression(paste(ET[lockdown], " ", SO[2] , ", tons/week"))) +
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
                       low="blue", mid= "white", high="red", name = expression(paste(ET[lockdown], " ",SO[2] , " ,% change"))) +
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
                       low="blue", mid= "white", high="red", name = expression(paste(ET[lockdown], " ",CO[2] , ",", 10^6, " tons/week"))) +
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
                       low="blue", mid= "white", high="red", name = expression(paste(ET[lockdown], " ",CO[2] , " ,% change"))) +
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
                       low="blue", mid= "white", high="red", name = expression(paste(ET[longterm], " ", NO[x] , ", tons/week"))) +
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
                       low="blue", mid= "white", high="red", name = expression(paste(ET[longterm], " ",NO[x] , " ,% change"))) +
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
  scale_fill_gradient2(limits=c(-5000, 25000), breaks= c(-5000,5000,15000,25000),
                       low="blue", mid= "white", high="red", name = expression(paste(ET[longterm]," ", SO[2] , ", tons/week"))) +
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
so2.facility.states$pct <- (so2.facility.states$ATT*100)/(so2.facility.states$Y.tr.bar)

so2.facility.states %>%  top_n (3,pct)
so2.facility.states %>%  top_n (3,ATT)

so2.long.pct <- plot_usmap(data = as.data.frame(so2.facility.states), values = "pct", color = "black", labels = F,  exclude = c("AK", "HI")) +
  scale_fill_gradient2(limits=c(-100, 100), oob = scales::squish,
                       low="blue", mid= "white", high="red", name = expression(paste(ET[longterm], " ",SO[2] , " ,% change"))) +
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
                       low="blue", mid= "white", high="red", name = expression(paste(ET[longterm], " ", CO[2] , ",", 10^6, " tons/week"))) +
  theme(legend.position = c( 0.25, .9), legend.direction = "horizontal" ,
        legend.text = element_text(size = 25),text = element_text(size=20)) + 
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
                       low="blue", mid= "white", high="red", name = expression(paste(ET[longterm], " ",CO[2] , " ,% change"))) +
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
                                                                      legend.text = element_text(size = 16),
                                                                      axis.text.x = element_text(size=16),
                                                                      axis.text.y = element_text(size=16),
                                                                      axis.title = element_text(size=20),
                                                                      title=element_text(size=20)) +
  labs(x="Weeks in 2020", y="", title = expression(paste(SO[2], " emissions, tons"))) 


wide.so2 <- dcast(so2, week~group)

wide.so2$pct <-(wide.so2$actual-wide.so2$counterfactual)*100/wide.so2$actual


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
                                                                      legend.text = element_text(size = 16),
                                                                      axis.text.x = element_text(size=16),
                                                                      axis.text.y = element_text(size=16),
                                                                      axis.title = element_text(size=20),
                                                                      title = element_text(size=20)) +
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
                                                                    legend.text = element_text(size = 16),
                                                                    axis.text.x = element_text(size=16),
                                                                    axis.text.y = element_text(size=16),
                                                                    axis.title = element_text(size=20),
                                                                    title = element_text(size=20)) +
  labs(x="Weeks in 2020", y = "", title = expression(paste( CO[2], " Emissions", ", ", 10^6," tons"))) 

p1 <- ggarrange(so2.2020.compare, nox.2020.compare, co2.2020.compare, labels = c("A","B","C"), font.label = list(size=15),
          label.x = 0.9, label.y = 0.85,  ncol = 3, nrow = 1) 


ggsave("compare_nox_so2_co2_2020.png", path = "./plots/", width=14, height=4, units="in")


p2 <- ggarrange(so2.loc, nox.loc,   co2.loc, so2.long, nox.long, co2.long, 
                labels = c("A","B","C", "D", "E", "F"),
               label.x = 0.95, label.y = 0.95,
          ncol = 3, nrow = 2,  common.legend = F, legend = "bottom") 

ggsave("spatial_maps_all.png", path = "./plots/", width=19, height=10, units="in")



p3 <- ggarrange(so2.loc.pct, nox.loc.pct,   co2.loc.pct, so2.long.pct, nox.long.pct, co2.long.pct, labels = c("D","E","F", "G", "H", "I"),
          label.x = 0.95, label.y = 0.91,  ncol = 3, nrow = 2,  common.legend = F, legend = "bottom") 

ggsave("spatial_maps_all_pct.png", path = "./plots/", width=19, height=11, units="in")

cowplot::plot_grid(p1, p3,
                   # 1 column and two rows - stacked on top of each other
                   ncol = 1,
                   nrow = 2,
                   # top plot is 2/3 as tall as second
                   rel_heights = c(0.3, 0.7))
ggsave("temporal_spatial_maps_all_pct.png", path = "./plots/", width=22, height=15, units="in")
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

so2.facility<- setDT(so2.facility)[, .(ET = sum(ATT, na.rm=TRUE),
                                       actual.so2.emis = sum(Y.tr.bar, na.rm=T),
                                       ct.so2.emis = sum(Y.ct.bar, na.rm=T)), 
                                   by = .(facility, STATE,Fuel.Type..Primary.,Facility.Latitude,Facility.Longitude,
                                          Source.Category, Unit.Type, SO2.Control.s.,SO2.Control.s.,
                                          PM.Control.s., Hg.Control.s., EPA.Region, NERC.Region,
                                          Associated.Stacks,Program.s.,SO2.Phase,SO2.Phase)]

#subtracting mean ATT from total treatment on treated 
# so2.facility$ET.mean <- so2.facility$ET--mean(so2.facility$ET)

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


so2.facility$Fuel <- "Other"
so2.facility$Fuel [ so2.facility$Fuel.Type..Primary.=="Coal"] <- "Coal"
so2.facility$Fuel [ grepl("Natural Gas", so2.facility$Fuel.Type..Primary.)] <- "Natural Gas"

so2.facility$Fuel <- as.factor(so2.facility$Fuel)


so2.facility$EPA.Region <- as.factor(so2.facility$EPA.Region)

#SO2
# eqn <- ET ~ -1 +EPA.Region * Fuel  *Has.SO2.Control #+ Unit.Type +NOx.Phase + Source.Category #without intercept -1

eqn <- ET ~ -1 + EPA.Region : Fuel  : Has.SO2.Control
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
so2_df$fuel.group[grepl("Other", so2_df$Variable)] <- "Other"

so2_df$control.group[grepl("Has.SO2.ControlHas SO2 Control", so2_df$Variable)] <- "with emission control"
so2_df$control.group[grepl("Has.SO2.ControlHas No SO2 Control", so2_df$Variable)] <- "without emission control"

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

so2.regression <- so2_df %>%  group_by (fuel.group) %>%   ggplot() +
  geom_pointrange( mapping = aes(x = fuel.group, y = estimate, 
                                 ymin = ci0.025, ymax = ci0.975, color=fuel.group),size=0.8) + coord_flip() +
  geom_hline(yintercept = 0) + theme( legend.title = element_blank(),
                                      legend.position = c(0.47,0.5),
                                      legend.text = element_text(size=14),
                                      axis.title.y = element_blank(),
                                      axis.title.x = element_text(size=20),
                                      axis.text.y = element_blank(),
                                      axis.ticks.y = element_blank(),
                                      axis.text.x = element_text(size=20),
                                      strip.text.x = element_text(size = 20),
                                      strip.text.y = element_text(size = 20, angle=-360),
                                      panel.grid.major = element_blank(), 
                                      panel.grid.minor = element_blank()) +
  labs(y=expression(paste( "Average ",SO[2], " emissions change", ", ","tons/week"))) + scale_color_brewer( palette = 'Dark2') +
  facet_grid(region.group~control.group)

so2.regression

ggsave("so2_regression.png", path = "./plots/", width=9, height=9, units="in")

#region 10
so2.facility.reg.10 <- so2.facility %>%  filter(EPA.Region==10)

so2.facility.reg.6 <- so2.facility %>%  filter(EPA.Region==6)
so2.facility.reg.control <- so2.facility %>%  filter(EPA.Region==6 & Has.SO2.Control== "Has No SO2 Control")

#NOx regression


nox.facility <- as.data.table(read.fst( "data/all.facility.nox.2020.fst"))

nox.facility <- nox.facility %>% filter (week>8)

nox.facility<- setDT(nox.facility)[, .(ET = sum(ATT, na.rm=TRUE),
                                       actual.nox.emis = sum(Y.tr.bar, na.rm=T),
                                       ct.nox.emis = sum(Y.ct.bar, na.rm=T)), 
                                   by = .(facility, STATE,Fuel.Type..Primary.,Facility.Latitude,Facility.Longitude,
                                          Source.Category, Unit.Type, NOx.Control.s.,
                                          PM.Control.s., Hg.Control.s., EPA.Region, NERC.Region,
                                          Associated.Stacks,Program.s.,NOx.Phase)]

#subtracting mean ATT from total treatment on treated 
# nox.facility$ET.mean <- nox.facility$ET--mean(nox.facility$ET)

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


nox.facility$Fuel <- "Other"
nox.facility$Fuel [ nox.facility$Fuel.Type..Primary.=="Coal"] <- "Coal"
nox.facility$Fuel [ grepl("Natural Gas", nox.facility$Fuel.Type..Primary.)] <- "Natural Gas"

nox.facility$Fuel <- as.factor(nox.facility$Fuel)


nox.facility$EPA.Region <- as.factor(nox.facility$EPA.Region)

#nox
# eqn <- ET ~ -1 +EPA.Region * Fuel  *Has.nox.Control #+ Unit.Type +NOx.Phase + Source.Category #without intercept -1

eqn <- ET ~ -1 + EPA.Region : Fuel  : Has.NOx.Control
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
nox_df$fuel.group[grepl("Other", nox_df$Variable)] <- "Other"

nox_df$control.group[grepl("Has.NOx.ControlHas NOx Control", nox_df$Variable)] <- "with emission control"
nox_df$control.group[grepl("Has.NOx.ControlHas No NOx Control", nox_df$Variable)] <- "without emission control"

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
nox.regression <- nox_df %>%  group_by (fuel.group) %>% ggplot() +
  geom_pointrange( mapping = aes(x = fuel.group, y = estimate, 
                                 ymin = ci0.025, ymax = ci0.975, color=fuel.group),size=0.8) + coord_flip() +
  geom_hline(yintercept = 0) + theme( legend.title = element_blank(),
                                      legend.position = c(0.1,0.3),
                                      legend.text = element_text(size=14),
                                      axis.title.y = element_blank(),
                                      axis.title.x = element_text(size=20),
                                      axis.text.y = element_blank(),
                                      axis.ticks.y = element_blank(),
                                      axis.text.x = element_text(size=15, angle = 45),
                                      strip.text.x = element_text(size = 20),
                                      strip.text.y = element_text(size = 20, angle=-360),
                                      panel.grid.major = element_blank(), 
                                      panel.grid.minor = element_blank())+
  labs(y=expression(paste( "Average ",NO[X], " emissions change", ", ","tons/week"))) + scale_color_brewer( palette = 'Dark2') +
  facet_grid(region.group~control.group)

nox.regression

ggsave("nox_regression.png", path = "./plots/", width=8, height=8, units="in")

#region 10
nox.facility.reg.10 <- nox.facility %>%  filter(EPA.Region==10)

nox.facility.reg.10 %>%  group_by(Fuel)%>% summarize(min = min(x),
                                                     q1 = quantile(x, 0.25),
                                                     median = median(x),
                                                     mean = mean(x),
                                                     q3 = quantile(x, 0.75),
                                                     max = max(x))
# plot_grid(so2.regression,nox.regression, ncol=2, nrow = 1, labels = c("A", "B"), label_x = 0.92, label_y=0.95 )
# ggsave("sox_nox_regression.png", path = "./plots/", width=17, height=10, units="in")

#CO2 regression


co2.facility <- as.data.table(read.fst( "data/all.facility.co2.2020.fst"))

co2.facility <- co2.facility %>% filter (week>8)

co2.facility<- setDT(co2.facility)[, .(ET = sum(ATT, na.rm=TRUE),
                                       actual.co2.emis = sum(Y.tr.bar, na.rm=T),
                                       ct.co2.emis = sum(Y.ct.bar, na.rm=T)), 
                                   by = .(facility, STATE,Fuel.Type..Primary.,Facility.Latitude,Facility.Longitude,
                                          Source.Category, Unit.Type, SO2.Control.s.,SO2.Control.s.,
                                          PM.Control.s., Hg.Control.s., EPA.Region, NERC.Region,
                                          Associated.Stacks,Program.s.,SO2.Phase,SO2.Phase)]

#subtracting mean ATT from total treatment on treated 
# so2.facility$ET.mean <- so2.facility$ET--mean(so2.facility$ET)

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


co2.facility$Fuel <- "Other"
co2.facility$Fuel [ co2.facility$Fuel.Type..Primary.=="Coal"] <- "Coal"
co2.facility$Fuel [ grepl("Natural Gas", co2.facility$Fuel.Type..Primary.)] <- "Natural Gas"

co2.facility$Fuel <- as.factor(co2.facility$Fuel)


co2.facility$EPA.Region <- as.factor(co2.facility$EPA.Region)

co2.facility$ET <- co2.facility$ET/10e6
#SO2
# eqn <- ET ~ -1 +EPA.Region * Fuel  *Has.SO2.Control #+ Unit.Type +NOx.Phase + Source.Category #without intercept -1

eqn <- ET ~ -1 + EPA.Region : Fuel  #: Has.SO2.Control
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
co2_df$fuel.group[grepl("Other", co2_df$Variable)] <- "Other"

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

# co2_df$estimate <- co2_df$estimate/10e6
# co2_df$ci0.025 <- co2_df$ci0.025/10e6
# co2_df$ci0.975  <- co2_df$ci0.975 /10e6

co2.regression <- co2_df %>%  group_by (fuel.group) %>%   ggplot() +
  geom_pointrange( mapping = aes(x = fuel.group, y = estimate, 
                                 ymin = ci0.025, ymax = ci0.975, color=fuel.group),size=0.8) + coord_flip() +
  geom_hline(yintercept = 0) + theme( legend.title = element_blank(),
                                      legend.position = c(0.15,0.3),
                                      legend.text = element_text(size=14),
                                      axis.title.y = element_blank(),
                                      axis.title.x = element_text(size=16),
                                      axis.text.y = element_blank(),
                                      axis.ticks.y = element_blank(),
                                      axis.text.x = element_text(size=20),
                                      strip.text.x = element_text(size = 20),
                                      strip.text.y = element_text(size = 20, angle=-360),
                                      panel.grid.major = element_blank(), 
                                      panel.grid.minor = element_blank()) +
  labs(y=expression(paste( "Average ",CO[2], " emissions change", ", ", 10^6,"tons/week"))) + scale_color_brewer( palette = 'Dark2') +
  facet_grid(region.group~.)

co2.regression 

ggsave("co2_regression.png", path = "./plots/", width=5, height=7, units="in")


#Heat Input regression


heat.input.facility <- as.data.table(read.fst( "data/all.facility.heat.input.2020.fst"))

heat.input.facility <- heat.input.facility %>% filter (week>8)

heat.input.facility<- setDT(heat.input.facility)[, .(ET = sum(ATT, na.rm=TRUE),
                                       actual.heat.input.emis = sum(Y.tr.bar, na.rm=T),
                                       ct.heat.input.emis = sum(Y.ct.bar, na.rm=T)), 
                                   by = .(facility, STATE,Fuel.Type..Primary.,Facility.Latitude,Facility.Longitude,
                                          Source.Category, Unit.Type, SO2.Control.s.,
                                          PM.Control.s., Hg.Control.s., EPA.Region, NERC.Region,
                                          Associated.Stacks,Program.s.,SO2.Phase,SO2.Phase)]

#subtracting mean ATT from total treatment on treated 
# so2.facility$ET.mean <- so2.facility$ET--mean(so2.facility$ET)

#primary fuel
heat.input.facility[, Fuel1.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Primary.))]
heat.input.facility[Fuel.Type..Primary. == "", Fuel1.IsCoal := NA]

heat.input.facility[, Fuel1.IsNatGas := as.numeric(grepl("Natural Gas", Fuel.Type..Primary.))]
heat.input.facility[Fuel.Type..Primary. == "", Fuel1.IsNatGas  := NA]

other.fuel <- c("Other Gas", "Diesel Oil", "Wood", "Process Gas", "Residual Oil", "Petroleum Coke",
                "Other Oil") 
heat.input.facility[, Fuel1.IsOthers := as.numeric( Fuel.Type..Primary. %in% other.fuel)]
heat.input.facility[Fuel.Type..Primary. == "", Fuel1.IsOthers  := NA]

#heat.input control
heat.input.facility[, Has.SO2.Control := "Has SO2 Control"]
heat.input.facility[SO2.Control.s. == "", Has.SO2.Control := "Has No SO2 Control"]

heat.input.facility$Has.SO2.Control <- as.factor(heat.input.facility$Has.SO2.Control)


heat.input.facility$Fuel <- "Other"
heat.input.facility$Fuel [ heat.input.facility$Fuel.Type..Primary.=="Coal"] <- "Coal"
heat.input.facility$Fuel [ grepl("Natural Gas", heat.input.facility$Fuel.Type..Primary.)] <- "Natural Gas"

heat.input.facility$Fuel <- as.factor(heat.input.facility$Fuel)


heat.input.facility$EPA.Region <- as.factor(heat.input.facility$EPA.Region)

heat.input.facility$ET <- heat.input.facility$ET/10e3
#SO2
# eqn <- ET ~ -1 +EPA.Region * Fuel  *Has.SO2.Control #+ Unit.Type +NOx.Phase + Source.Category #without intercept -1

eqn <- ET ~ -1 + EPA.Region : Fuel  #: Has.SO2.Control
#controls
r.lm <- lm(eqn, data = heat.input.facility)

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

heat.input_df <- lm_df

heat.input_df$fuel.group[grepl("Coal", heat.input_df$Variable)] <- "Coal"
heat.input_df$fuel.group[grepl("Natural Gas", heat.input_df$Variable)] <- "Natural Gas"
heat.input_df$fuel.group[grepl("Other", heat.input_df$Variable)] <- "Other"

# heat.input_df$control.group[grepl("Has.SO2.ControlHas SO2 Control", heat.input_df$Variable)] <- "Has SO2 Control"
# heat.input_df$control.group[grepl("Has.SO2.ControlHas No SO2 Control", heat.input_df$Variable)] <- "Has No SO2 Control"

heat.input_df$region.group <- sub("\\:.*", "", heat.input_df$Variable)


heat.input_df <- heat.input_df %>% 
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

heat.input_df$region.group <- factor(heat.input_df$region.group, levels=c("1",  "2",
                                                            "3",  "4",  "5",  "6",  "7",
                                                            "8",  "9",   "10"))

# heat.input_df$estimate <- heat.input_df$estimate/10e6
# heat.input_df$ci0.025 <- heat.input_df$ci0.025/10e6
# heat.input_df$ci0.975  <- heat.input_df$ci0.975 /10e6

heat.input.regression <- heat.input_df %>%  group_by (fuel.group) %>%  ggplot() +
  geom_pointrange( mapping = aes(x = fuel.group, y = estimate, 
                                 ymin = ci0.025, ymax = ci0.975, color=fuel.group),size=0.8) + coord_flip() +
  geom_hline(yintercept = 0) + theme( legend.title = element_blank(),
                                      legend.position = c(0.15,0.3),
                                      legend.text = element_text(size=14),
                                      axis.title.y = element_blank(),
                                      axis.title.x = element_text(size=16),
                                      axis.text.y = element_blank(),
                                      axis.ticks.y = element_blank(),
                                      axis.text.x = element_text(size=20),
                                      strip.text.x = element_text(size = 20),
                                      strip.text.y = element_text(size = 20, angle=-360),
                                      panel.grid.major = element_blank(), 
                                      panel.grid.minor = element_blank())+
  labs(y=expression(paste( "Average "," heat input change", ", ", 10^3," MMBtu/week"))) + scale_color_brewer( palette = 'Dark2') +
  facet_grid(region.group~.)

heat.input.regression

ggsave("heat.input_regression.png", path = "./plots/", width=5, height=7, units="in")


#Gross load regression


gross.load.facility <- as.data.table(read.fst( "data/all.facility.gross.load.2020.fst"))

gross.load.facility <- gross.load.facility %>% filter (week>8)

gross.load.facility<- setDT(gross.load.facility)[, .(ET = sum(ATT, na.rm=TRUE),
                                                     actual.gross.load.emis = sum(Y.tr.bar, na.rm=T),
                                                     ct.gross.load.emis = sum(Y.ct.bar, na.rm=T)), 
                                                 by = .(facility, STATE,Fuel.Type..Primary.,Facility.Latitude,Facility.Longitude,
                                                        Source.Category, Unit.Type, SO2.Control.s.,
                                                        PM.Control.s., Hg.Control.s., EPA.Region, NERC.Region,
                                                        Associated.Stacks,Program.s.,SO2.Phase,SO2.Phase)]

#subtracting mean ATT from total treatment on treated 
# so2.facility$ET.mean <- so2.facility$ET--mean(so2.facility$ET)

#primary fuel
gross.load.facility[, Fuel1.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Primary.))]
gross.load.facility[Fuel.Type..Primary. == "", Fuel1.IsCoal := NA]

gross.load.facility[, Fuel1.IsNatGas := as.numeric(grepl("Natural Gas", Fuel.Type..Primary.))]
gross.load.facility[Fuel.Type..Primary. == "", Fuel1.IsNatGas  := NA]

other.fuel <- c("Other Gas", "Diesel Oil", "Wood", "Process Gas", "Residual Oil", "Petroleum Coke",
                "Other Oil") 
gross.load.facility[, Fuel1.IsOthers := as.numeric( Fuel.Type..Primary. %in% other.fuel)]
gross.load.facility[Fuel.Type..Primary. == "", Fuel1.IsOthers  := NA]

#gross.load control
gross.load.facility[, Has.SO2.Control := "Has SO2 Control"]
gross.load.facility[SO2.Control.s. == "", Has.SO2.Control := "Has No SO2 Control"]

gross.load.facility$Has.SO2.Control <- as.factor(gross.load.facility$Has.SO2.Control)


gross.load.facility$Fuel <- "Other"
gross.load.facility$Fuel [ gross.load.facility$Fuel.Type..Primary.=="Coal"] <- "Coal"
gross.load.facility$Fuel [ grepl("Natural Gas", gross.load.facility$Fuel.Type..Primary.)] <- "Natural Gas"

gross.load.facility$Fuel <- as.factor(gross.load.facility$Fuel)


gross.load.facility$EPA.Region <- as.factor(gross.load.facility$EPA.Region)

gross.load.facility$ET <- gross.load.facility$ET/10e3
#SO2
# eqn <- ET ~ -1 +EPA.Region * Fuel  *Has.SO2.Control #+ Unit.Type +NOx.Phase + Source.Category #without intercept -1

eqn <- ET ~ -1 + EPA.Region : Fuel  #: Has.SO2.Control
#controls
r.lm <- lm(eqn, data = gross.load.facility)

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

gross.load_df <- lm_df

gross.load_df$fuel.group[grepl("Coal", gross.load_df$Variable)] <- "Coal"
gross.load_df$fuel.group[grepl("Natural Gas", gross.load_df$Variable)] <- "Natural Gas"
gross.load_df$fuel.group[grepl("Other", gross.load_df$Variable)] <- "Other"

# gross.load_df$control.group[grepl("Has.SO2.ControlHas SO2 Control", gross.load_df$Variable)] <- "Has SO2 Control"
# gross.load_df$control.group[grepl("Has.SO2.ControlHas No SO2 Control", gross.load_df$Variable)] <- "Has No SO2 Control"

gross.load_df$region.group <- sub("\\:.*", "", gross.load_df$Variable)


gross.load_df <- gross.load_df %>% 
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

gross.load_df$region.group <- factor(gross.load_df$region.group, levels=c("1",  "2",
                                                                          "3",  "4",  "5",  "6",  "7",
                                                                          "8",  "9",   "10"))

# gross.load_df$estimate <- gross.load_df$estimate/10e6
# gross.load_df$ci0.025 <- gross.load_df$ci0.025/10e6
# gross.load_df$ci0.975  <- gross.load_df$ci0.975 /10e6

gross.load.regression <- gross.load_df %>%  group_by (fuel.group) %>%   ggplot() +
  geom_pointrange( mapping = aes(x = fuel.group, y = estimate, 
                                 ymin = ci0.025, ymax = ci0.975, color=fuel.group),size=0.8) + coord_flip() +
  geom_hline(yintercept = 0) + theme( legend.title = element_blank(),
                                      legend.position = c(0.15,0.3),
                                      legend.text = element_text(size=14),
                                      axis.title.y = element_blank(),
                                      axis.title.x = element_text(size=16),
                                      axis.text.y = element_blank(),
                                      axis.ticks.y = element_blank(),
                                      axis.text.x = element_text(size=20),
                                      strip.text.x = element_text(size = 20),
                                      strip.text.y = element_text(size = 20, angle=-360),
                                      panel.grid.major = element_blank(), 
                                      panel.grid.minor = element_blank())+
  labs(y=expression(paste( "Average "," gross load change", ", ", 10^3," MWh/week"))) + scale_color_brewer( palette = 'Dark2') +
  facet_grid(region.group~.)

ggsave("gross.load_regression.png", path = "./plots/", width=5, height=7, units="in")

#Operation time regression


op.time.facility <- as.data.table(read.fst( "data/all.facility.op.time.2020.fst"))

op.time.facility <- op.time.facility %>% filter (week>8)

op.time.facility<- setDT(op.time.facility)[, .(ET = sum(ATT, na.rm=TRUE),
                                                     actual.op.time.emis = sum(Y.tr.bar, na.rm=T),
                                                     ct.op.time.emis = sum(Y.ct.bar, na.rm=T)), 
                                                 by = .(facility, STATE,Fuel.Type..Primary.,Facility.Latitude,Facility.Longitude,
                                                        Source.Category, Unit.Type, SO2.Control.s.,
                                                        PM.Control.s., Hg.Control.s., EPA.Region, NERC.Region,
                                                        Associated.Stacks,Program.s.,SO2.Phase,SO2.Phase)]

#subtracting mean ATT from total treatment on treated 
# so2.facility$ET.mean <- so2.facility$ET--mean(so2.facility$ET)

#primary fuel
op.time.facility[, Fuel1.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Primary.))]
op.time.facility[Fuel.Type..Primary. == "", Fuel1.IsCoal := NA]

op.time.facility[, Fuel1.IsNatGas := as.numeric(grepl("Natural Gas", Fuel.Type..Primary.))]
op.time.facility[Fuel.Type..Primary. == "", Fuel1.IsNatGas  := NA]

other.fuel <- c("Other Gas", "Diesel Oil", "Wood", "Process Gas", "Residual Oil", "Petroleum Coke",
                "Other Oil") 
op.time.facility[, Fuel1.IsOthers := as.numeric( Fuel.Type..Primary. %in% other.fuel)]
op.time.facility[Fuel.Type..Primary. == "", Fuel1.IsOthers  := NA]

#op.time control
op.time.facility[, Has.SO2.Control := "Has SO2 Control"]
op.time.facility[SO2.Control.s. == "", Has.SO2.Control := "Has No SO2 Control"]

op.time.facility$Has.SO2.Control <- as.factor(op.time.facility$Has.SO2.Control)


op.time.facility$Fuel <- "Other"
op.time.facility$Fuel [ op.time.facility$Fuel.Type..Primary.=="Coal"] <- "Coal"
op.time.facility$Fuel [ grepl("Natural Gas", op.time.facility$Fuel.Type..Primary.)] <- "Natural Gas"

op.time.facility$Fuel <- as.factor(op.time.facility$Fuel)


op.time.facility$EPA.Region <- as.factor(op.time.facility$EPA.Region)

op.time.facility$ET <- op.time.facility$ET
#SO2
# eqn <- ET ~ -1 +EPA.Region * Fuel  *Has.SO2.Control #+ Unit.Type +NOx.Phase + Source.Category #without intercept -1

eqn <- ET ~ -1 + EPA.Region : Fuel  #: Has.SO2.Control
#controls
r.lm <- lm(eqn, data = op.time.facility)

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

op.time_df <- lm_df

op.time_df$fuel.group[grepl("Coal", op.time_df$Variable)] <- "Coal"
op.time_df$fuel.group[grepl("Natural Gas", op.time_df$Variable)] <- "Natural Gas"
op.time_df$fuel.group[grepl("Other", op.time_df$Variable)] <- "Other"

# op.time_df$control.group[grepl("Has.SO2.ControlHas SO2 Control", op.time_df$Variable)] <- "Has SO2 Control"
# op.time_df$control.group[grepl("Has.SO2.ControlHas No SO2 Control", op.time_df$Variable)] <- "Has No SO2 Control"

op.time_df$region.group <- sub("\\:.*", "", op.time_df$Variable)


op.time_df <- op.time_df %>% 
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

op.time_df$region.group <- factor(op.time_df$region.group, levels=c("1",  "2",
                                                                          "3",  "4",  "5",  "6",  "7",
                                                                          "8",  "9",   "10"))

# op.time_df$estimate <- op.time_df$estimate/10e6
# op.time_df$ci0.025 <- op.time_df$ci0.025/10e6
# op.time_df$ci0.975  <- op.time_df$ci0.975 /10e6

op.time.regression <- op.time_df %>%  group_by (fuel.group) %>%   ggplot() +
  geom_pointrange( mapping = aes(x = fuel.group, y = estimate, 
                                 ymin = ci0.025, ymax = ci0.975, color=fuel.group),size=0.8) + coord_flip() +
  geom_hline(yintercept = 0) + theme( legend.title = element_blank(),
                                      legend.position = c(0.15,0.3),
                                      legend.text = element_text(size=14),
                                      axis.title.y = element_blank(),
                                      axis.title.x = element_text(size=14),
                                      axis.text.y = element_blank(),
                                      axis.ticks.y = element_blank(),
                                      axis.text.x = element_text(size=20),
                                      strip.text.x = element_text(size = 20),
                                      strip.text.y = element_text(size = 20, angle=-360),
                                      panel.grid.major = element_blank(), 
                                      panel.grid.minor = element_blank()) +
  labs(y=expression(paste( "Average ", " operating time change", ", ", " hr/week"))) + scale_color_brewer( palette = 'Dark2') +
  facet_grid(region.group~.)

op.time.regression


ggsave("op.time_regression.png", path = "./plots/", width=5, height=7, units="in")

# plot_grid(so2.regression,nox.regression, co2.regression, ncol=3, nrow = 1 )
# 
# ggsave("sox_nox_co2_regression.png", path = "./plots/", width=20, height=10, units="in")



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


so2.facility <- as.data.table(read.fst( "data/all.facility.so2.2020.fst"))

so2.facility[, Fuel1.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Primary.))]
so2.facility[Fuel.Type..Primary. == "", Fuel1.IsCoal := NA]

so2.coal.2020 <- so2.facility %>% filter (Fuel1.IsCoal==1)


##################################################################
## Supplemental US EPA Region map
##################################################################
us_states.tran$epa.region[us_states.tran$state_abbr %in% c("CT", "ME", "MA", "NH", "RI", "VT")] <- 1
us_states.tran$epa.region[us_states.tran$state_abbr %in% c("NJ", "NY")] <- 2
us_states.tran$epa.region[us_states.tran$state_abbr %in% c("DE", "DC", "MD", "PA", "VA", "WV")] <- 3
us_states.tran$epa.region[us_states.tran$state_abbr %in% c("AL", "FL", "GA", "KY", "MS", "NC", "SC","TN")] <- 4
us_states.tran$epa.region[us_states.tran$state_abbr %in% c("IL", "IN", "MI", "MN", "OH", "WI")] <- 5
us_states.tran$epa.region[us_states.tran$state_abbr %in% c("AR", "LA", "NM", "OK", "TX")] <- 6
us_states.tran$epa.region[us_states.tran$state_abbr %in% c("IA", "KS", "MO", "NE")] <- 7
us_states.tran$epa.region[us_states.tran$state_abbr %in% c("CO", "MT", "ND", "SD", "UT", "WY")] <- 8
us_states.tran$epa.region[us_states.tran$state_abbr %in% c("AZ", "CA", "HI", "NV")] <- 9
us_states.tran$epa.region[us_states.tran$state_abbr %in% c("AK", "ID", "OR", "WA" )] <- 10

us_states.tran$epa.reg.txt <- ifelse (us_states.tran$state_abbr %in% "ME", 1,
                                      ifelse (us_states.tran$state_abbr %in% "NY", 2,
                                              ifelse (us_states.tran$state_abbr %in% "VA", 3,
                                                      ifelse (us_states.tran$state_abbr %in% "GA", 4,
                                                              ifelse (us_states.tran$state_abbr %in% "WI", 5,
                                                                      ifelse (us_states.tran$state_abbr %in% "TX", 6,
                                                                              ifelse (us_states.tran$state_abbr %in% "KS", 7,
                                                                                      ifelse (us_states.tran$state_abbr %in% "WY", 8,
                                                                                              ifelse (us_states.tran$state_abbr %in% "CA", 9,
                                                                                                      ifelse (us_states.tran$state_abbr %in% "WA", 10,
                                                                                                              ""))))))))))





ggplot(data = us_states.tran, color = 'black' ) + 
  geom_sf(aes(fill = epa.region) ) + 
  # set the x and y limits
  coord_sf( xlim = c( -125, -67), ylim = c( 25, 50)) +
  theme_bw() + theme(legend.position = "none", axis.text = element_blank(),
                     axis.title = element_blank()) + geom_sf_text( aes(label=epa.reg.txt), size=6)

ggsave("epa_region.png", path = "./plots/", width=6, height=4, units="in")

#
##################################################################
## Spatial Interpolation for SO2 and sulfate ET
##################################################################

#longterm
so2.facility.long <- as.data.table(read.fst( "data/all.facility.so2.2020.fst"))
# so2.facility.loc <- so2.facility.loc %>%  filter (week>8 & week <17)
so2.facility.long <- so2.facility.long %>%  filter (week>8 )
so2.facility.long<- setDT(so2.facility.long)[, .(ET = sum(ATT, na.rm=TRUE),
                                                      Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                                      Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE)), 
                                                  by = .(facility)]


library(sp)
idw_longterm_so2 <- dplyr::select(so2.facility.long, ET, Facility.Latitude, Facility.Longitude)
sp::coordinates(idw_longterm_so2) <- ~ Facility.Longitude + Facility.Latitude
x.range <- as.integer(range(idw_longterm_so2@coords[,1]))
y.range <- as.integer(range(idw_longterm_so2@coords[,2]))
x.range <- as.integer(c(-126, -66))
y.range <- as.integer(c(23,51))

grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=0.5), 
                   y=seq(from=y.range[1], to=y.range[2], by=0.5))
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE

library(maptools)
# plot(grd, cex=1.5)
# points(lockdown, pch=1, col='red', cex=1)
# title("Interpolation Grid and Sample Points")
usmap <- readShapePoly("./data/cb_2019_us_nation_5m/cb_2019_us_nation_5m.shp")
usmapoutline <- fortify(usmap)


################## idw ##################
library(gstat)
idw <- idw(ET ~ 1, locations=idw_longterm_so2, newdata=grd, nmax=30)
idw.output <- as.data.frame(idw)
names(idw.output)[c(1:3)]<-c("lon","lat", "ET.pred")

xy <- idw.output[,c("lon","lat")]
proj4string(usmap) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
spdf <- SpatialPointsDataFrame(coords = xy, data = idw.output,
                               proj4string = CRS("+proj=longlat +datum=WGS84 
                                                 +ellps=WGS84 +towgs84=0,0,0"))
over <- over(spdf, usmap, fn=NULL)
points.so2 <- cbind(spdf@data, over)
points.so2$var1.var <- NULL
points.so2 <- na.omit(points.so2)
plot_idw <- ggplot(data=points.so2, aes(x=lon,y=lat))
layer1_idw <- c(geom_tile(data=points.so2, aes(fill=ET.pred)))
layer2_idw <- c(geom_path(data=usmap, aes(long, lat, group=group), colour = "grey40", size=1))

## idw smoothed ATT map
plot_idw+layer1_idw+borders("state")+layer2_idw+scale_fill_gradient2(midpoint = 0, mid="white", low="blue", high="red")+
  coord_equal()+xlim(-126,-66)+ylim(23,51) + xlab("Longitude") + ylab("Latitude") + labs(fill="ET") +
  ggtitle("IDW-Smoothed Total Treatment Effect Estimates")

#taking sulfate concentration from AQS data and doing inverse distance interpolation


#longterm



aqs.data.mn <- read.fst("./data/aqs.1999.2020.monthly.fst")


#longterm sulfate concentration March - April 2020


aqs.monthly.long <- aqs.data.mn%>% filter(  year==2020 & month>2 & chemicals=="Sulfate")

aqs.monthly.long <- setDT(aqs.monthly.long)[, .(mean = mean(mean, na.rm=TRUE)), 
                                  by = .(ID, Latitude, Longitude)]



idw_longterm_sulfate <- dplyr::select(aqs.monthly.long , mean, Latitude, Longitude)
sp::coordinates(idw_longterm_sulfate) <- ~ Longitude + Latitude
x.range <- as.integer(range(idw_longterm_sulfate@coords[,1]))
y.range <- as.integer(range(idw_longterm_sulfate@coords[,2]))
x.range <- as.integer(c(-126, -66))
y.range <- as.integer(c(23,51))

grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=0.5), 
                   y=seq(from=y.range[1], to=y.range[2], by=0.5))
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE


# plot(grd, cex=1.5)
# points(longterm, pch=1, col='red', cex=1)
# title("Interpolation Grid and Sample Points")
usmap <- readShapePoly("./data/cb_2019_us_nation_5m/cb_2019_us_nation_5m.shp")
usmapoutline <- fortify(usmap)


################## idw ##################
library(gstat)
idw <- idw(mean ~ 1, locations=idw_longterm_sulfate, newdata=grd, nmax=30)
idw.output <- as.data.frame(idw)
names(idw.output)[c(1:3)]<-c("lon","lat", "mean.pred")

xy <- idw.output[,c("lon","lat")]
proj4string(usmap) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
spdf <- SpatialPointsDataFrame(coords = xy, data = idw.output,
                               proj4string = CRS("+proj=longlat +datum=WGS84 
                                                 +ellps=WGS84 +towgs84=0,0,0"))
over <- over(spdf, usmap, fn=NULL)
points.sulfate <- cbind(spdf@data, over)
points.sulfate $var1.var <- NULL
points.sulfate  <- na.omit(points.sulfate )
plot_idw <- ggplot(data=points.sulfate , aes(x=lon,y=lat))
layer1_idw <- c(geom_tile(data=points.sulfate , aes(fill=mean.pred)))
layer2_idw <- c(geom_path(data=usmap, aes(long, lat, group=group), colour = "grey40", size=1))

## idw smoothed ATT map
plot_idw+layer1_idw+borders("state")+layer2_idw+scale_fill_viridis_c( option="plasma")+
  coord_equal()+xlim(-126,-66)+ylim(23,51) + xlab("Longitude") + ylab("Latitude") + labs(fill="mean_sulfate") +
  ggtitle("IDW-Smoothed mean sulfate Estimates")


points.so2.sulfate <- merge(points.so2, points.sulfate, by= c("lon", "lat", "AFFGEOID", "GEOID", "NAME"))

library(ggpmisc)

ggplot(data = points.so2.sulfate, aes(x = ET.pred, y = mean.pred))  +
  stat_poly_line() +
  stat_poly_eq() +
  geom_point() +
  labs(x= "ET SO2", 
       y = "Sulfate concentration",
       title = "") 


#kevins data

att_kevin <- load("./data/att_smooth.RData")
att_kevin <- points

library(ggplot2)
library(viridis)
library(raster)
r <-  rasterFromXYZ(att_kevin[,c(1,2, 3)])

ggplot() + geom_raster(data = att_kevin, aes(x=lon, y = lat, fill=att.pred)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1) +
  theme_bw() 


locs.sf <- st_as_sf( att_kevin, 
                     coords = c( 'lon', 'lat'),
                     crs = 'WGS84')


# we want to use an equal area projection, here's one I often use:
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"

link_locations.sf.trans <- st_transform( locs.sf , crs = p4s)


## =============================================================
# 2. create grid of receptors
#     I'll create this over the state of TX
## =============================================================
# get TX shapefile object, transform to our crs

# get USA dataset
states48 <- state.name[!(state.name %in% c( 'Alaska', 'Hawaii'))]
usa <- rnaturalearth::ne_states(#scale = 110, #type = "countries",
  country = "United States of America")
usa.sub <- spTransform( usa[usa$name %in% states48,], CRSobj = p4s)
usa.sub.sf <- st_as_sf( usa.sub)

all.states <- usa.sub.sf  %>%
  st_transform( crs = p4s)

# create a grid, use 4km resoulation
fishnet.sf <-
  st_bbox( all.states) %>%
  st_as_sfc() %>%
  st_make_grid( cellsize = 12000) %>%
  st_sf()

# define a receptor ID
fishnet.sf$ID_recept <- 1:nrow( fishnet.sf)

# make a plot with all data so far
ggplot( ) +
  geom_sf( data = all.states, fill = 'white', size = 2)  +
  geom_sf( data = fishnet.sf,
           mapping = aes( fill = ID_recept),
           color = 'grey50', size = .01, alpha = .9)


# =======================================================================================================
#Correlation plot of emissions vs meteorology===================================================================
# =======================================================================================================

ampd_daily_all_units<- read.fst ("data/ampd_daily_cleaned_facility.fst")

library(corrplot)


ampd_daily_select <- ampd_daily_all_units %>% 
  dplyr::select(ORISPL_CODE, date, STATE, UNITID, ID, year, month, day, SUM_OP_TIME, Gross.Load..MW.h.,HEAT.INPUT, NOx..tons.,
                SO2..tons., CO2..tons., pr, tmmx, rmax, vs, th, Fuel.Type..Primary. )  %>%  filter(th>=0)


ampd_daily_to_weekly <- setDT(ampd_daily_select)[, .(SO2..tons. = sum(SO2..tons., na.rm=TRUE),
                                                     NOx..tons. = sum(NOx..tons., na.rm=TRUE),
                                                     Gross.Load..MW.h. = sum(Gross.Load..MW.h., na.rm=TRUE),
                                                     SUM_OP_TIME = sum(SUM_OP_TIME,na.rm=TRUE),
                                                     HEAT.INPUT=sum(HEAT.INPUT, na.rm=TRUE),
                                                     pr= sum(pr, na.rm=T),
                                                     tmmx=mean(tmmx, na.rm=T),
                                                     vs= mean(vs, na.rm=T),
                                                     rmax= mean(rmax, na.rm=T),
                                                     th= mean(th, na.rm=T)),
                                                 by = .(STATE, ORISPL_CODE, ID, year,
                                                        isoweek(date),Fuel.Type..Primary.)]

ampd_daily_to_weekly <- ampd_daily_to_weekly %>%  dplyr::select(-STATE, -ORISPL_CODE, -ID, -Fuel.Type..Primary., -year, -isoweek)

head(ampd_daily_to_weekly)
M = cor(ampd_daily_to_weekly)

corrplot(M, method = 'ellipse', order = 'AOE', type = 'upper')

corrplot.mixed(M, order = 'AOE')





