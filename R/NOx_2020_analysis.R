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

nox.facility <- as.data.table(read.fst( "data/all.facility.nox.2020.fst"))

nox.facility[, Fuel1.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Primary.))]
nox.facility[Fuel.Type..Primary. == "", Fuel1.IsCoal := NA]

nox.coal.2020 <- nox.facility %>% filter (Fuel1.IsCoal==1)

#percent contribution from coal facilities

sum(nox.coal.2020$Y.tr.bar)/sum(nox.facility$Y.tr.bar)

#
nox.weeks <- nox.facility 
nox.facility <- nox.facility %>%  filter (week>8)
sum(nox.facility$ATT, na.rm=T)

#rate
sum(nox.facility$ATT, na.rm=T)/ (52-8)

nox.facility$rate <- nox.facility$ATT/(52-8)

sum(nox.facility$rate)
# Calculate the mean and standard error
l.model <- lm(ATT ~ 1, nox.facility)

# Calculate the confidence interval
confint(l.model, level=0.95)

sum(nox.facility$Y.tr.bar)
sum(nox.facility$Y.ct.bar)



nox.facility.coal <- nox.facility %>%  filter (Fuel1.IsCoal==1)
sum(nox.facility.coal$ATT, na.rm=T)
sum(nox.facility.coal$Y.tr.bar)
sum(nox.facility.coal$Y.ct.bar)


#coal vs




#plotting US Map for all facility 
nox.facility.states<- setDT(nox.facility)[, .(ATT = sum(ATT, na.rm=TRUE),
                                               Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                              Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE),
                                              actual.nox.emis = sum(Y.tr.bar, na.rm=T),
                                              ct.nox.emis = sum(Y.ct.bar, na.rm=T)), 
                                                by = .(STATE)]

nox.facility.week<- setDT(nox.weeks)[, .(ATT = sum(ATT, na.rm=TRUE),
                                              Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                              Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE),
                                              actual.nox.emis = sum(Y.tr.bar, na.rm=T),
                                              ct.nox.emis = sum(Y.ct.bar, na.rm=T)), 
                                          by = .(week)]

#weekly ATT overall

nox.facility.week %>% ggplot(aes(x=week)) + geom_line(aes(y=actual.nox.emis,  color="red")) +
  geom_line(aes(y=ct.nox.emis)) + geom_vline(xintercept=8)



nox.facility.yearly<- setDT(nox.facility)[, .(ATT = sum(ATT, na.rm=TRUE),
                                              Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                              Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE),
                                              actual.nox.emis = sum(Y.tr.bar, na.rm=T),
                                              ct.nox.emis = sum(Y.ct.bar, na.rm=T)), 
                                          by = .(facility)]
#facilities with increased ATT
nox.facility.yearly %>% filter (ATT>=0)






# write.csv(nox.facility.states, "./data/states.att.march.dec2020.nox.csv")




nox.facility.states %>% top_n (3, ATT )

x <- sum(nox.facility.states$actual.nox.emis)
y <- sum(nox.facility.states$ct.nox.emis)

pct.increase <- (x-y)*100/x

names(nox.facility.states)[names(nox.facility.states) == 'STATE'] <- 'state'

nox.facility.states <- nox.facility.states %>% mutate(pct=(nox.facility.states$actual.nox.emis-nox.facility.states$ct.nox.emis)*100/
                                                        nox.facility.states$actual.nox.emis)

nox.facility.states %>% filter(ATT>0)

plot_usmap(data = as.data.frame(nox.facility.states), values = "ATT", color = "black", labels = F,  exclude = c("AK", "HI")) +
  scale_fill_gradient2(limits=c(-1000, 7000),
                       low="blue", mid= "white", high="red", name = expression(paste("TTT " , NO[x] , " tons"))) +
  theme(legend.position = c( 0.25, .9), legend.direction = "horizontal" ,
        legend.text = element_text(size = 10),text = element_text(size=20)) + 
  guides(fill = guide_colorbar( label.position = "top",
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= -1.5,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1)) 

# ggsave("longterm_nox_att.png", path = "./plots/")

nox.facility.states %>% top_n (3, ATT )



#######lockdown


nox.facility.loc <- as.data.table(read.fst( "data/all.facility.nox.2020.fst"))

nox.facility.loc[, Fuel1.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Primary.))]
nox.facility.loc[Fuel.Type..Primary. == "", Fuel1.IsCoal := NA]

nox.facility.loc <- nox.facility.loc %>%  filter (week>8 & week <17)

#rate
sum(nox.facility.loc$ATT, na.rm=T)/ (17-8)


sum(nox.facility.loc$ATT, na.rm=T)
sum(nox.facility.loc$Y.tr.bar)
sum(nox.facility.loc$Y.ct.bar)

# Calculate the mean and standard error
l.model <- lm(ATT ~ 1, nox.facility.loc)

# Calculate the confidence interval
confint(l.model, level=0.95)


nox.facility.coal.loc <- nox.facility.loc %>%  filter (Fuel1.IsCoal==1)
sum(nox.facility.coal.loc$ATT, na.rm=T)
sum(nox.facility.coal.loc$Y.tr.bar)
sum(nox.facility.coal.loc$Y.ct.bar)


#plotting US Map for all facility 
nox.facility.states.loc<- setDT(nox.facility.loc)[, .(ATT = sum(ATT, na.rm=TRUE),
                                              Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                              Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE),
                                              actual.nox.emis = sum(Y.tr.bar, na.rm=T),
                                              ct.nox.emis = sum(Y.ct.bar, na.rm=T)), 
                                          by = .(STATE)]

# write.csv(nox.facility.states.loc, "./data/states.att.march.april2020.nox.csv")

nox.facility.states.loc %>% top_n (-3, ATT )

# nox.facility.states %>% top_n (3, ATT )

a <- sum(nox.facility.states.loc$actual.nox.emis)
b <- sum(nox.facility.states.loc$ct.nox.emis)

pct.increase.loc <- (a-b)*100/a

names(nox.facility.states.loc)[names(nox.facility.states.loc) == 'STATE'] <- 'state'

nox.facility.states.loc <- nox.facility.states.loc %>% mutate(pct=(nox.facility.states.loc$actual.nox.emis-nox.facility.states.loc$ct.nox.emis)*100/
                                                        nox.facility.states.loc$actual.nox.emis)

plot_usmap(data = as.data.frame(nox.facility.states.loc), values = "ATT", color = "black", labels = F,  exclude = c("AK", "HI")) +
  scale_fill_gradient2(limits=c(-500, 1500), 
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

ggsave("shortterm_nox_att.png", path = "./plots/")

nox.facility.states.loc %>% top_n (-3, ATT )

summary(nox.facility.states.loc %>% filter (ATT>0))

nox.facility.states.loc %>% filter (ATT<0)

#ttest for both lockdown period and whole intervention period (March-December 2020)


#march-december 2020

nox.facility <- as.data.table(read.fst( "data/all.facility.nox.2020.fst"))

nox.facility <- nox.facility %>%  filter (week>8)


states <- unique(as.vector(nox.facility$STATE))

datalist=list()

for (i in 1: length(states) ) {
  state.ac.ct <- nox.facility %>% filter (STATE %in% states[i])
  
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

nox.facility.loc <- read.fst( "data/all.facility.nox.2020.fst")


nox.facility.loc <- nox.facility.loc %>%  filter (week>8 & week <17)


states <- unique(as.vector(nox.facility.loc$STATE))

datalist=list()

for (i in 1: length(states) ) {
  state.ac.ct <- nox.facility.loc %>% filter (STATE %in% states[i])
  
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


df%>%  ggplot(aes(x=week, y=nox.tons, color=group)) + geom_line()  + geom_vline(xintercept=8) +
  geom_vline(xintercept=16) + annotate("rect", xmin = 8, xmax = 52, ymin = 0, ymax = Inf, fill = "blue", alpha = .1, color = NA) +
  theme_bw() + scale_x_continuous(expand = c(0, 0), limits = c(0, 52)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) + theme (legend.position = c(0.6,0.25),
                                                                   legend.title = element_blank(),
                                                                   legend.text = element_text(size = 10)) +
  labs(x="Weeks", y=expression(paste(NO[x], " tons")), title = "") 

ggsave("compare_nox_2020.png", path = "./plots/")

