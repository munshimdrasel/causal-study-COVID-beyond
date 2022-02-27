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

nox.facility <- as.data.table(read.fst( "data/all.facility.nox.2020.fst"))

nox.facility[, Fuel1.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Primary.))]
nox.facility[Fuel.Type..Primary. == "", Fuel1.IsCoal := NA]


nox.facility <- nox.facility %>%  filter (week>8)
sum(nox.facility$ATT, na.rm=T)

nox.facility.coal <- nox.facility %>%  filter (Fuel1.IsCoal==1)
sum(nox.facility.coal$ATT, na.rm=T)


#coal vs




#plotting US Map for all facility 
nox.facility.states<- setDT(nox.facility)[, .(ATT = sum(ATT, na.rm=TRUE),
                                              S.E.=sum(S.E., na.rm=T),
                                              CI.lower=sum(CI.lower, na.rm = T),
                                              CI.upper=sum(CI.upper, na.rm=T),
                                              p.value=mean(p.value, na.rm=T),
                                               Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                              Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE),
                                              actual.nox.emis = sum(Y.tr.bar, na.rm=T),
                                              ct.nox.emis = sum(Y.ct.bar, na.rm=T)), 
                                                by = .(STATE)]




nox.facility.states %>% top_n (3, ATT )

x <- sum(nox.facility.states$actual.nox.emis)
y <- sum(nox.facility.states$ct.nox.emis)

pct.increase <- (x-y)*100/x

names(nox.facility.states)[names(nox.facility.states) == 'STATE'] <- 'state'

nox.facility.states <- nox.facility.states %>% mutate(pct=(nox.facility.states$actual.nox.emis-nox.facility.states$ct.nox.emis)*100/
                                                        nox.facility.states$actual.nox.emis)

plot_usmap(data = as.data.frame(nox.facility.states), values = "ATT", color = "black", labels = F,  exclude = c("AK", "HI")) +
  scale_fill_gradient2(low="blue", mid= "white", high="red", name = expression(paste("ATT " , NO[x] , " tons"))) +
  theme(legend.position = c( 0.25, .9), legend.direction = "horizontal" ,
        legend.text = element_text(size = 15),text = element_text(size=20)) + 
  guides(fill = guide_colorbar( label.position = "top",
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= -1.5,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1)) 

ggsave("longterm_nox_att.png", path = "./plots/")

nox.facility.states %>% top_n (3, pct )



#######lockdown


nox.facility.loc <- read.fst( "data/all.facility.nox.2020.fst")


nox.facility.loc <- nox.facility.loc %>%  filter (week>8 & week <17)
sum(nox.facility.loc$ATT, na.rm=T)





#plotting US Map for all facility 
nox.facility.states.loc<- setDT(nox.facility.loc)[, .(ATT = sum(ATT, na.rm=TRUE),
                                              S.E.=sum(S.E., na.rm=T),
                                              CI.lower=sum(CI.lower, na.rm = T),
                                              CI.upper=sum(CI.upper, na.rm=T),
                                              p.value=sum(p.value, na.rm=T),
                                              Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                              Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE),
                                              actual.nox.emis = sum(Y.tr.bar, na.rm=T),
                                              ct.nox.emis = sum(Y.ct.bar, na.rm=T)), 
                                          by = .(STATE)]

nox.facility.states.loc %>% top_n (3, ATT )

nox.facility.states %>% top_n (3, ATT )

a <- sum(nox.facility.states.loc$actual.nox.emis)
b <- sum(nox.facility.states.loc$ct.nox.emis)

pct.increase.loc <- (a-b)*100/a

names(nox.facility.states.loc)[names(nox.facility.states.loc) == 'STATE'] <- 'state'

nox.facility.states.loc <- nox.facility.states.loc %>% mutate(pct=(nox.facility.states.loc$actual.nox.emis-nox.facility.states.loc$ct.nox.emis)*100/
                                                        nox.facility.states.loc$actual.nox.emis)

plot_usmap(data = as.data.frame(nox.facility.states.loc), values = "ATT", color = "black", labels = F,  exclude = c("AK", "HI")) +
  scale_fill_gradient2(low="blue", mid= "white", high="red", name = expression(paste("ATT " , NO[x] , " tons"))) +
  theme(legend.position = c( 0.25, .9), legend.direction = "horizontal" ,
        legend.text = element_text(size = 15),text = element_text(size=20)) + 
  guides(fill = guide_colorbar( label.position = "top",
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= -1.5,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1)) 

ggsave("shortterm_nox_att.png", path = "./plots/")

nox.facility.states.loc %>% top_n (3, pct )




