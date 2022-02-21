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
library(usmap)
library(ggpubr)

# setwd ("/projects/HAQ_LAB/mrasel/R/causal-study-COVID-beyond")

setwd ("/Volumes/GoogleDrive/My Drive/R/causal-study-COVID-beyond")

so2.facility <- read.fst( "data/all.facility.so2.2020.fst")


so2.facility <- so2.facility %>%  filter (week>8)
sum(so2.facility$ATT, na.rm=T)





#plotting US Map for all facility 
so2.facility.states<- setDT(so2.facility)[, .(ATT = sum(ATT, na.rm=TRUE),
                                              S.E.=sum(S.E., na.rm=T),
                                              CI.lower=sum(CI.lower, na.rm = T),
                                              CI.upper=sum(CI.upper, na.rm=T),
                                              p.value=sum(p.value, na.rm=T),
                                              Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                              Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE),
                                              actual.so2.emis = sum(Y.tr.bar, na.rm=T),
                                              ct.so2.emis = sum(Y.ct.bar, na.rm=T)), 
                                          by = .(STATE)]

so2.facility.states %>% top_n (3, ATT )

x <- sum(so2.facility.states$actual.so2.emis)
y <- sum(so2.facility.states$ct.so2.emis)

pct.increase <- (x-y)*100/x

names(so2.facility.states)[names(so2.facility.states) == 'STATE'] <- 'state'

so2.facility.states <- so2.facility.states %>% mutate(pct=(so2.facility.states$actual.so2.emis-so2.facility.states$ct.so2.emis)*100/
                                                        so2.facility.states$actual.so2.emis)

plot_usmap(data = as.data.frame(so2.facility.states), values = "ATT", color = "black", labels = F,  exclude = c("AK", "HI")) +
  scale_fill_gradient2(low="blue", mid= "white", high="red", name = expression(paste("ATT " , SO[2] , " tons/day"))) +
  theme(legend.position = c( 0.25, .9), legend.direction = "horizontal" ,
        legend.text = element_text(size = 30),text = element_text(size=30)) + 
  guides(fill = guide_colorbar( label.position = "top",
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= -1.5,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1)) 

so2.facility.states %>% top_n (3, pct )



#######lockdown


so2.facility.loc <- read.fst( "data/all.facility.so2.2020.fst")


so2.facility.loc <- so2.facility.loc %>%  filter (week>8 & week <17)
sum(so2.facility.loc$ATT, na.rm=T)





#plotting US Map for all facility 
so2.facility.states.loc<- setDT(so2.facility.loc)[, .(ATT = sum(ATT, na.rm=TRUE),
                                                      S.E.=sum(S.E., na.rm=T),
                                                      CI.lower=sum(CI.lower, na.rm = T),
                                                      CI.upper=sum(CI.upper, na.rm=T),
                                                      p.value=sum(p.value, na.rm=T),
                                                      Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                                      Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE),
                                                      actual.so2.emis = sum(Y.tr.bar, na.rm=T),
                                                      ct.so2.emis = sum(Y.ct.bar, na.rm=T)), 
                                                  by = .(STATE)]

so2.facility.states.loc %>% top_n (3, ATT )

so2.facility.states %>% top_n (3, ATT )

a <- sum(so2.facility.states.loc$actual.so2.emis)
b <- sum(so2.facility.states.loc$ct.so2.emis)

pct.increase.loc <- (a-b)*100/a

names(so2.facility.states.loc)[names(so2.facility.states.loc) == 'STATE'] <- 'state'

so2.facility.states.loc <- so2.facility.states.loc %>% mutate(pct=(so2.facility.states.loc$actual.so2.emis-so2.facility.states.loc$ct.so2.emis)*100/
                                                                so2.facility.states.loc$actual.so2.emis)

plot_usmap(data = as.data.frame(so2.facility.states.loc), values = "ATT", color = "black", labels = F,  exclude = c("AK", "HI")) +
  scale_fill_gradient2(low="blue", mid= "white", high="red", name = expression(paste("ATT " , SO[2] , " tons/day"))) +
  theme(legend.position = c( 0.25, .9), legend.direction = "horizontal" ,
        legend.text = element_text(size = 30),text = element_text(size=30)) + 
  guides(fill = guide_colorbar( label.position = "top",
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= -1.5,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1)) 

so2.facility.states.loc %>% top_n (3, ATT )

