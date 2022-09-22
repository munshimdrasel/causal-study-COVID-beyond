rm(list = ls())

library(fst)
library(data.table)
library(tidyverse)
library(viridis)
library(ggplot2)
library(stringi)
library(lubridate)
library(dslabs)
library(gridExtra)
library(ggmap)
library(Rmisc)
library(usmap)
library(ggpubr)

# setwd ("/projects/HAQ_LAB/mrasel/R/causal-study-COVID-beyond")

setwd ("/Volumes/GoogleDrive/My Drive/R/causal-study-COVID-beyond")

co2.facility <- as.data.table(read.fst( "data/all.facility.co2.2020.fst"))

co2.facility[, Fuel1.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Primary.))]
co2.facility[Fuel.Type..Primary. == "", Fuel1.IsCoal := NA]

co2.coal.2020 <- co2.facility %>% filter (Fuel1.IsCoal==1)

#percent contribution from coal facilities

sum(co2.coal.2020$Y.tr.bar)/sum(co2.facility$Y.tr.bar)
#
co2.weeks <- co2.facility 
co2.facility <- co2.facility %>%  filter (week>8)
sum(co2.facility$ATT, na.rm=T)

#week before 26
co2.facility.less26 <- co2.facility %>%  filter (week<26)

sum(co2.facility.less26$Y.ct.bar)
sum(co2.facility.less26$Y.tr.bar)
sum(co2.facility.less26$ATT)/sum(co2.facility.less26$Y.tr.bar)

co2.facility.less26.coal <- co2.facility.less26 %>%  filter (Fuel1.IsCoal==1)
# week after 26
co2.facility.more26 <- co2.facility %>%  filter (week>26)

sum(co2.facility.more26$Y.ct.bar)
sum(co2.facility.more26$Y.tr.bar)
sum(co2.facility.more26$ATT)/sum(co2.facility.more26$Y.tr.bar)

co2.facility.more26.coal <- co2.facility.more26 %>%  filter (Fuel1.IsCoal==1)

unique(co2.facility.more26.coal$facility)
# %increse with respect to counterfactual emissions
sum(co2.facility$ATT, na.rm=T)/sum(co2.facility$Y.tr.bar)

#rate
sum(co2.facility$ATT, na.rm=T)/ (52-8)

co2.facility$rate <- co2.facility$ATT/(52-8)

sum(co2.facility$rate)


sum(co2.facility$Y.tr.bar)
sum(co2.facility$Y.ct.bar)



co2.facility.coal <- co2.facility %>%  filter (Fuel1.IsCoal==1)
sum(co2.facility.coal$ATT, na.rm=T)
sum(co2.facility.coal$Y.tr.bar)
sum(co2.facility.coal$Y.ct.bar)


#coal vs




#plotting US Map for all facility 
co2.facility.states<- setDT(co2.facility)[, .(ATT = sum(ATT, na.rm=TRUE),
                                              Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                              Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE),
                                              actual.co2.emis = sum(Y.tr.bar, na.rm=T),
                                              ct.co2.emis = sum(Y.ct.bar, na.rm=T)), 
                                                by = .(STATE)]

co2.facility.week<- setDT(co2.weeks)[, .(ATT = sum(ATT, na.rm=TRUE),
                                              Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                              Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE),
                                              actual.co2.emis = sum(Y.tr.bar, na.rm=T),
                                              ct.co2.emis = sum(Y.ct.bar, na.rm=T)), 
                                          by = .(week)]

#weekly ATT overall

co2.facility.week %>% ggplot(aes(x=week)) + geom_line(aes(y=actual.co2.emis,  color="red")) +
  geom_line(aes(y=ct.co2.emis)) + geom_vline(xintercept=8)



co2.facility.yearly<- setDT(co2.facility)[, .(ATT = sum(ATT, na.rm=TRUE),
                                              Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                              Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE),
                                              actual.co2.emis = sum(Y.tr.bar, na.rm=T),
                                              ct.co2.emis = sum(Y.ct.bar, na.rm=T)), 
                                          by = .(facility)]
#facilities with increased ATT
co2.facility.yearly %>% filter (ATT>=0)






# write.csv(co2.facility.states, "./data/states.att.march.dec2020.co2.csv")




co2.facility.states %>% top_n (3, ATT )

x <- sum(co2.facility.states$actual.co2.emis)
y <- sum(co2.facility.states$ct.co2.emis)

pct.increase <- (x-y)*100/x

names(co2.facility.states)[names(co2.facility.states) == 'STATE'] <- 'state'

co2.facility.states <- co2.facility.states %>% mutate(pct=(co2.facility.states$actual.co2.emis-co2.facility.states$ct.co2.emis)*100/
                                                        co2.facility.states$actual.co2.emis)

co2.facility.states %>% filter(ATT>0)

plot_usmap(data = as.data.frame(co2.facility.states), values = "ATT", color = "black", labels = F,  exclude = c("AK", "HI")) +
  scale_fill_gradient2(limits=c(-1835000, 10902000),
                       low="blue", mid= "white", high="red", name = expression(paste("TTT " , CO[2] , " tons"))) +
  theme(legend.position = c( 0.25, .9), legend.direction = "horizontal" ,
        legend.text = element_text(size = 10),text = element_text(size=20)) + 
  guides(fill = guide_colorbar( label.position = "top",
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= -1.5,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1)) 

# ggsave("longterm_co2_att.png", path = "./plots/")

co2.facility.states %>% top_n (3, ATT )



#######lockdown


co2.facility.loc <- as.data.table(read.fst( "data/all.facility.co2.2020.fst"))

co2.facility.loc[, Fuel1.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Primary.))]
co2.facility.loc[Fuel.Type..Primary. == "", Fuel1.IsCoal := NA]

co2.facility.loc <- co2.facility.loc %>%  filter (week>8 & week <17)

#rate
sum(co2.facility.loc$ATT, na.rm=T)/ (17-8)


sum(co2.facility.loc$ATT, na.rm=T)
sum(co2.facility.loc$Y.tr.bar)
sum(co2.facility.loc$Y.ct.bar)

#% increase of CO2 in lockdown
sum(co2.facility.loc$ATT, na.rm=T)/sum(co2.facility.loc$Y.tr.bar)

co2.facility.coal.loc <- co2.facility.loc %>%  filter (Fuel1.IsCoal==1)
sum(co2.facility.coal.loc$ATT, na.rm=T)
sum(co2.facility.coal.loc$Y.tr.bar)
sum(co2.facility.coal.loc$Y.ct.bar)


#plotting US Map for all facility 
co2.facility.states.loc<- setDT(co2.facility.loc)[, .(ATT = sum(ATT, na.rm=TRUE),
                                              Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                              Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE),
                                              actual.co2.emis = sum(Y.tr.bar, na.rm=T),
                                              ct.co2.emis = sum(Y.ct.bar, na.rm=T)), 
                                          by = .(STATE)]

# write.csv(co2.facility.states.loc, "./data/states.att.march.april2020.co2.csv")

co2.facility.states.loc %>% top_n (3, ATT )

# co2.facility.states %>% top_n (3, ATT )

a <- sum(co2.facility.states.loc$actual.co2.emis)
b <- sum(co2.facility.states.loc$ct.co2.emis)

pct.increase.loc <- (a-b)*100/a

names(co2.facility.states.loc)[names(co2.facility.states.loc) == 'STATE'] <- 'state'

co2.facility.states.loc <- co2.facility.states.loc %>% mutate(pct=(co2.facility.states.loc$actual.co2.emis-co2.facility.states.loc$ct.co2.emis)*100/
                                                        co2.facility.states.loc$actual.co2.emis)

plot_usmap(data = as.data.frame(co2.facility.states.loc), values = "ATT", color = "black", labels = F,  exclude = c("AK", "HI")) +
  scale_fill_gradient2(limits=c(-735000, 2340000), 
                       low="blue", mid= "white", high="red", name = expression(paste("TTT " , NO[x] , " tons"))) +
  theme(legend.position = c( 0.25, .9), legend.direction = "horizontal" ,
        legend.text = element_text(size = 15),text = element_text(size=20)) + 
  guides(fill = guide_colorbar( label.position = "top",
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= -1.5,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1)) 

ggsave("shortterm_co2_att.png", path = "./plots/")

co2.facility.states.loc %>% top_n (-3, ATT )

summary(co2.facility.states.loc %>% filter (ATT>0))

co2.facility.states.loc %>% filter (ATT<0)

#ttest for both lockdown period and whole intervention period (March-December 2020)


#march-december 2020

co2.facility <- as.data.table(read.fst( "data/all.facility.co2.2020.fst"))

co2.facility <- co2.facility %>%  filter (week>8)


states <- unique(as.vector(co2.facility$STATE))

datalist=list()

for (i in 1: length(states) ) {
  state.ac.ct <- co2.facility %>% filter (STATE %in% states[i])
  
  ttest <- t.test(state.ac.ct$Y.tr.bar,state.ac.ct$Y.ct.bar, var.equal=T)
  
  df <- as.data.frame(ttest$p.value)
  names(df)[names(df) == 'ttest$p.value'] <- 'p.value'
  df$STATE <- states[i]
  datalist[[i]] <- df
  
}

states.ttest <- do.call(rbind, datalist)

states.ttest$sig <- ifelse(states.ttest$p.value<0.05, TRUE, FALSE)

states.ttest <- states.ttest[order(states.ttest$STATE),]

count(states.ttest$sig==TRUE)


#march-april lockdown 2020

co2.facility.loc <- read.fst( "data/all.facility.co2.2020.fst")


co2.facility.loc <- co2.facility.loc %>%  filter (week>8 & week <17)


states <- unique(as.vector(co2.facility.loc$STATE))

datalist=list()

for (i in 1: length(states) ) {
  state.ac.ct <- co2.facility.loc %>% filter (STATE %in% states[i])
  
  ttest <- t.test(state.ac.ct$Y.tr.bar,state.ac.ct$Y.ct.bar, var.equal=T)
  
  df <- as.data.frame(ttest$p.value)
  names(df)[names(df) == 'ttest$p.value'] <- 'p.value'
  df$STATE <- states[i]
  datalist[[i]] <- df
  
}

states.ttest <- do.call(rbind, datalist)

states.ttest$sig <- ifelse(states.ttest$p.value<0.05, TRUE, FALSE)

states.ttest <- states.ttest[order(states.ttest$STATE),]

count(states.ttest$sig==TRUE)

#####actual counterfactual weekly


co2.2020 <- read.fst("data/all.facility.co2.2020.fst")

#GSYNTH  and actual

co2.gsynth.actual<- setDT(co2.2020)[, .(actual.co2.emis = sum(Y.tr.bar, na.rm=T),
                                        ct.co2.emis = sum(Y.ct.bar, na.rm=T)), 
                                    by = .(week)]

names(co2.gsynth.actual)[names(co2.gsynth.actual) == 'ct.co2.emis'] <- 'gsynth.counterfactual.2020'
names(co2.gsynth.actual)[names(co2.gsynth.actual) == 'actual.co2.emis'] <- 'actual.2020'

df <- melt(co2.gsynth.actual, id.vars = "week", measure.vars = c("actual.2020", "gsynth.counterfactual.2020"))


names(df)[names(df) == 'variable'] <- 'group'
names(df)[names(df) == 'value'] <- 'co2.tons'

df <- df [order(week), ]


df%>%  ggplot(aes(x=week, y=co2.tons, color=group)) + geom_line()  + geom_vline(xintercept=8) +
  geom_vline(xintercept=16) + annotate("rect", xmin = 8, xmax = 52, ymin = 0, ymax = Inf, fill = "blue", alpha = .1, color = NA) +
  theme_bw() + scale_x_continuous(expand = c(0, 0), limits = c(0, 52)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) + theme (legend.position = c(0.6,0.25),
                                                                   legend.title = element_blank(),
                                                                   legend.text = element_text(size = 10)) +
  labs(x="Weeks", y=expression(paste(CO[2], " tons")), title = "") 

ggsave("compare_co2_2020.png", path = "./plots/")

