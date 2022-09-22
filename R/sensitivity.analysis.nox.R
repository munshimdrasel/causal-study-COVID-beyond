
rm(list = ls())

library(gsynth)
library(fst)
library(data.table)
library(tidyverse)
# library(parallel)
# library(sf)
library(viridis)
library(ggplot2)
library(lubridate)
library(dslabs)
library(stringi)
library(openair)
library(gridExtra)
library(ggmap)
# library(Rmisc)
library(usmap)
library(ggpubr)
# setwd ("/projects/HAQ_LAB/mrasel/R/causal-study")

setwd ("/Volumes/GoogleDrive/My Drive/R/causal-study-COVID-beyond")

# ==========================================No2 sensitivity===========================
#no2
# =======================================================================================

all.facility.nox <- read.fst("data/metrics_facility_nox_whole_2020.fst")

#taking 1048 facilities 0% to 100% operation during 2020
sum(all.facility.nox$ATT)

#is it right approach to sum up upper and lower confidence interval?

#facilities operating more than 0% during 2020
all.facility.nox <- all.facility.nox %>% filter(op.pct >0)
#we have 1018 facilities which were operating more than 0% of the time
sum(all.facility.nox$ATT)

#there could be some facilities which were emitting 0 nox emissions during 2020 (1866, 3494, 2104 etc.) but were operating more than 0% of time in 2020
#number of these facilities
sum(is.na(all.facility.nox$r))

facility_0op_0nox <- as.vector(unique(all.facility.nox$facility))
# in total there were 58 facilities which emitted 0 nox emissions during 2020 (may be due to Fuel types? Natural gas?)

all.facility.nox <- all.facility.nox %>% filter (r !="NA")

sum(all.facility.nox$ATT)

#plotting US Map for all facility 
all.facility.nox.updated<- setDT(all.facility.nox)[, .(ATT = sum(ATT, na.rm=TRUE),
                                                       Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                                       Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE)),
                                                   by = .(STATE)]

all.facility.nox.updated %>% top_n (-3, ATT )



colnames(all.facility.nox.updated) [1] <- "state"
plot_usmap(data = all.facility.nox.updated, values = "ATT", color = "black", labels = F,  exclude = c("AK", "HI")) +
  scale_fill_gradient2(low="blue", mid= "white", high="red", name = expression(paste("ATT " , NOx , " tons/day"))) +
  theme(legend.position = c( 0.25, .9), legend.direction = "horizontal" ,
        legend.text = element_text(size = 20),text = element_text(size=30)) + 
  guides(fill = guide_colorbar( label.position = "top",
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= -1.5,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1)) 


ggsave("US_plot_nox.png", width=10, units= "in", path = "/Volumes/GoogleDrive/My Drive/R/causal-study-COVID-beyond/plot")


all.facility2 <- all.facility.nox

all.facility2 %>% ggplot(aes(  op.pct, NMB, color= Fuel.Type..Primary. )) +
  geom_point() + labs(x= "facility operations in 2020 (%)", y = "NMB", title = " ")


#IOA, the Index of Agreement based on Willmott et al. (2011), 
# which spans between -1 and +1 with values approaching +1 representing better model performance.

all.facility.nox$fuel.types <- with(all.facility.nox, ifelse(grepl("Coal", Fuel.Type..Primary.), "Coal",
                                                             ifelse(grepl("Natural", Fuel.Type..Primary.), "Natural Gas", "Other Fuel")))

all.facility2 <- all.facility.nox # %>% filter(IOA >0 )
#609 facilities



facility_IOA_m0 <- all.facility2

sum(facility_IOA_m0$ATT)
# sum(facility_IOA_m0$upper.ci)
# sum(facility_IOA_m0$lower.ci)

plot1 <- facility_IOA_m0 %>% ggplot(aes(  op.pct, NMB, color= fuel.types )) +
  geom_point() + labs( x= "Facility operations in 2020 (%)", y = "Normalized Mean Bias", title = " ") +
  theme_bw() + labs(color="Fuel types")+theme(legend.text = element_text(size = 20), text = element_text(size=20), 
                                              axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1),
                                              legend.key.size = unit(1, 'lines')) +geom_hline(yintercept = 0) +
  scale_x_continuous(limits = c(0,100), expand = c(0, 0)) +
  guides(fill = guide_colorbar(title = "ATT nox tons/day")) +ylim (-5, 5)


plot2 <- facility_IOA_m0 %>% ggplot(aes(  op.pct, NMGE, color= fuel.types )) +
  geom_point() + labs(x= "Facility operations in 2020 (%)", y = "Normalized Mean Gross Error", title = " ") +
  labs(color="Fuel types")+
  theme_bw() + theme(legend.text = element_text(size = 20), text = element_text(size=20),
                     axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1),
                     legend.key.size = unit(1, 'lines')) +
  scale_x_continuous(limits = c(0,100), expand = c(0, 0)) + ylim (0, 5)



ggarrange (plot1, plot2, ncol=2, nrow=1, common.legend = TRUE, legend= "right")

ggsave("NMB.png", width= 15, height= 5, units= "in", path = "/Volumes/GoogleDrive/My Drive/R/causal-study-COVID-beyond/plot")

#facilities less than 25% operation
all.facility3 <- all.facility.nox %>% filter( op.pct<25)
facility_25 <- all.facility3

a <- sum(facility_25$ATT)
a.u <- sum(facility_25$upper.ci)
a.l <- sum(facility_25$lower.ci)

# all.facility2%>% ggplot(aes(  op.pct, NMB, color= Fuel.Type..Primary. )) +
#   geom_point() + labs(x= "facility operations in 2020 (%)", y = "NMB", title = " ")+
#   geom_vline(xintercept=25)
# 
# all.facility2 %>% ggplot(aes(  op.pct, NMGE, color= Fuel.Type..Primary. )) +
#   geom_point() + labs(x= "facility operations in 2020 (%)", y = "NMGE", title = " ")+
#   geom_vline(xintercept=25)


dfy.nox<- setDT(facility_25)[, .(ATT = sum(ATT, na.rm=TRUE),
                                 Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                 Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE)),
                             by = .(STATE)]

dfy.nox %>% top_n (3, ATT )

sum(dfy.nox$ATT)

colnames(dfy.nox) [1] <- "state"
plot_usmap(data = dfy.nox, values = "ATT", color = "black", labels = F,  exclude = c("AK", "HI")) +
  scale_fill_continuous(type="viridis", name = "ATT nox tons/day",
                        label = scales::comma) + theme(legend.position = "bottom")

#facilities >25% & <50% operation
all.facility4 <- all.facility.nox %>% filter(op.pct>=25 & op.pct<50)
facility_25_50 <- all.facility4

b <- sum(facility_25_50$ATT)
b.u <- sum(facility_25_50$upper.ci)
b.l <- sum(facility_25_50$lower.ci)

# all.facility2%>% ggplot(aes(  op.pct, NMB, color= Fuel.Type..Primary. )) +
#   geom_point() + labs(x= "facility operations in 2020 (%)", y = "NMB", title = " ")+
#   geom_vline(xintercept=25) + geom_vline(xintercept=50)
# 
# all.facility2 %>% ggplot(aes(  op.pct, NMGE, color= Fuel.Type..Primary. )) +
#   geom_point() + labs(x= "facility operations in 2020 (%)", y = "NMGE", title = " ")+
#   geom_vline(xintercept=25) + geom_vline(xintercept=50)

dfy.nox<- setDT(facility_25_50)[, .(ATT = sum(ATT, na.rm=TRUE),
                                    Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                    Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE)),
                                by = .(STATE)]

dfy.nox %>% top_n (3, ATT )

sum(dfy.nox$ATT)
mean(dfy.nox$ATT, na.rm =T)


colnames(dfy.nox) [1] <- "state"
plot_usmap(data = dfy.nox, values = "ATT", color = "black", labels = F,  exclude = c("AK", "HI")) +
  scale_fill_continuous(type="viridis", name = "ATT nox tons/day",
                        label = scales::comma) + theme(legend.position = "bottom")

#facilities >50% & <75% operation
all.facility5 <- all.facility.nox %>% filter( op.pct>=50 & op.pct<75)
facility_50_75 <- all.facility5

c <- sum(facility_50_75$ATT)
c.u <- sum(facility_50_75$upper.ci)
c.l <- sum(facility_50_75$lower.ci)

# all.facility2%>% ggplot(aes(  op.pct, NMB, color= Fuel.Type..Primary. )) +
#   geom_point() + labs(x= "facility operations in 2020 (%)", y = "NMB", title = " ")+
#   geom_vline(xintercept=50) + geom_vline(xintercept=75)
# 
# all.facility2 %>% ggplot(aes(  op.pct, NMGE, color= Fuel.Type..Primary. )) +
#   geom_point() + labs(x= "facility operations in 2020 (%)", y = "NMGE", title = " ")+
#   geom_vline(xintercept=50) + geom_vline(xintercept=75)


dfy.nox<- setDT(facility_50_75)[, .(ATT = sum(ATT, na.rm=TRUE),
                                    Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                    Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE)),
                                by = .(STATE)]

dfy.nox %>% top_n (3, ATT )

sum(dfy.nox$ATT)


colnames(dfy.nox) [1] <- "state"
plot_usmap(data = dfy.nox, values = "ATT", color = "black", labels = F,  exclude = c("AK", "HI")) +
  scale_fill_continuous(type="viridis", name = "ATT nox tons/day",
                        label = scales::comma) + theme(legend.position = "bottom")

#facilities >75% operation
all.facility6 <- all.facility.nox %>% filter(op.pct>=75)
facility_75 <- all.facility6

d <- sum(facility_75$ATT)
d.u <- sum(facility_75$upper.ci)
d.l <- sum(facility_75$lower.ci)

# all.facility2%>% ggplot(aes(  op.pct, NMB, color= Fuel.Type..Primary. )) +
#   geom_point() + labs(x= "facility operations in 2020 (%)", y = "NMB", title = " ")+ 
#   geom_vline(xintercept=75)
# 
# all.facility2 %>% ggplot(aes(  op.pct, NMGE, color= Fuel.Type..Primary. )) +
#   geom_point() + labs(x= "facility operations in 2020 (%)", y = "NMGE", title = " ")+
#   geom_vline(xintercept=75)


dfy.nox<- setDT(facility_75)[, .(ATT = sum(ATT, na.rm=TRUE),
                                 Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                 Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE)),
                             by = .(STATE)]

dfy.nox %>% top_n (3, ATT )

sum(dfy.nox$ATT)
mean(dfy.nox$ATT, na.rm =T)


colnames(dfy.nox) [1] <- "state"
plot_usmap(data = dfy.nox, values = "ATT", color = "black", labels = F,  exclude = c("AK", "HI")) +
  scale_fill_continuous(name = "ATT nox tons/day",type = "viridis",
                        label = scales::comma) + theme(legend.position = "bottom")



operation <- c ("0-25", "25-50", "50-75", "75-100")
att <- c (a, b, c, d)
att.u <- c(a.u, b.u, c.u, d.u)
att.l <- c (a.l, b.l, c.l, d.l)
facility_no <- c (nrow(all.facility3), nrow(all.facility4), nrow(all.facility5), nrow(all.facility6))

df <- data.frame(operation, att, att.u, att.l, facility_no)

#didn't calculate confidence interval (needed to use gsynth model se=T, nonparametric)
ggplot(df, aes(x=operation, y=att)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=att.l, ymax=att.u), width=.2,
                position=position_dodge(0.05)) + geom_label(label=facility_no, hjust = 0, nudge_x = 0.05) +
  labs(x= "facility operations in 2020 (%)", y = "Average Treatment on Treated (ATT)", title = " ")



all.facility.nox$operation.grp <- with(all.facility.nox, ifelse(op.pct <25, "0-25",
                                                                ifelse(op.pct>=25 & op.pct<50, "25-50",
                                                                       ifelse(op.pct>=50 & op.pct<75, "50-75", "75-100"))))

all.facility.nox %>% ggplot(aes(operation.grp, ATT, fill=operation.grp)) + geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) + 
  labs(x= "Facility operations in 2020 (%)",
       y = "Average Treatment on Treated (ATT) nox tons/day", title = " ")

#need facility information
dates <-seq(lubridate::ymd("2020-01-01"), lubridate::ymd("2020-12-31"), by = "days")
load ("data/result_facility_nox_2020_whole.RData")

#taking out counterfactual data from gsynth output
y.ct <- lapply(result_nox_whole_2020, function(x)  x[[2]]$Y.ct) 

for (i in seq_along(y.ct)) {
  y.ct[[i]] <- as.data.frame(y.ct[[i]])
  y.ct[[i]] <- reshape2::melt(y.ct[[i]])
  # y.ct[[i]]$ID <- rep(facility[i],nrow(y.ct[[i]]))
  y.ct[[i]]$date <- dates
}

y.bar_all <- y.ct %>% rbindlist

data.counterfactual.nox<- data.frame(lapply(y.bar_all, function(x) {gsub("2020_","", x) }))

data.counterfactual.nox$value <- as.numeric(data.counterfactual.nox$value)
data.counterfactual.nox$date <- as.Date(data.counterfactual.nox$date)

colnames(data.counterfactual.nox)[1] <- "ID"
colnames(data.counterfactual.nox)[2] <- "NOx..tons.ct"


#how about replacing negative counterfactual values with 0
# data.counterfactual.nox$so2..tons.ct2 <- data.counterfactual.so2$SO2..tons.ct
# 
# data.counterfactual.so2$so2..tons.ct2 <- with(data.counterfactual.so2, ifelse(SO2..tons.ct<0, 0, SO2..tons.ct))
# 
# data.counterfactual.so2$SO2..tons.ct <- data.counterfactual.so2$so2..tons.ct2

####################


ampd_daily_emissions <- read.fst ("/Volumes/GoogleDrive/My Drive/R/ampd-raw-data-processing/data/ampd_daily_emission.fst")
ampd_daily_emissions <- ampd_daily_emissions %>% filter (year ==2020 & month >=3 & month <=12)

ampd_merged <- ampd_daily_emissions %>% left_join(data.counterfactual.nox, by =c ("ID", "date"))


# ampd_merged$SO2..tons.[ampd_merged$year==2020 & ampd_merged$ID=="7897-2A"] <- ampd_merged$SO2..tons.ct

#total emissions
facility.id.25 <- as.vector(unique(facility_25$facility))
facility.id.25.50 <- as.vector(unique(facility_25_50$facility))
facility.id.50.75 <- as.vector(unique(facility_50_75$facility))
facility.id.75.100 <- as.vector(unique(facility_75$facility))



ampd_merged_25 <- ampd_merged %>% filter (ORISPL_CODE %in% facility.id.25)
e.ct <- sum(ampd_merged_25$NOx..tons.ct, na.rm=T)
e.at <- sum(ampd_merged_25$NOx..tons., na.rm=T)

ampd_merged_25_50 <- ampd_merged %>% filter (ORISPL_CODE %in% facility.id.25.50)
f.ct <- sum(ampd_merged_25_50$NOx..tons.ct, na.rm=T)
f.at <- sum(ampd_merged_25_50$NOx..tons., na.rm=T)

ampd_merged_50_75 <- ampd_merged %>% filter (ORISPL_CODE %in% facility.id.50.75)
g.ct <- sum(ampd_merged_50_75$NOx..tons.ct, na.rm=T)
g.at <- sum(ampd_merged_50_75$NOx..tons., na.rm=T)

ampd_merged_75_100 <- ampd_merged %>% filter (ORISPL_CODE %in% facility.id.75.100)
h.ct <- sum(ampd_merged_75_100$NOx..tons.ct, na.rm=T)
h.at <- sum(ampd_merged_75_100$NOx..tons., na.rm=T)

actual.emission <- c (e.at, f.at, g.at, h.at)
ct.emission <- c (e.ct, f.ct, g.ct, h.ct)
df <- data.frame(operation, att, att.u, att.l, facility_no, actual.emission, ct.emission)

#actual emissions vs counterfactual emissions (feb-march, 2020)
# sum(df$actual.emission)/sum(df$ct.emission)
#~16% increased in emissions of SO2 during lockdown period
#2.5% increase if negative counterfactual values replaced by 0

df %>% ggplot(aes(x=operation)) +geom_point(aes(y=actual.emission), color= "red") +
  geom_point(aes(y=ct.emission), color= "blue") +geom_line(aes(y=actual.emission)) +
  labs(x= "facility operations in 2020 (%)", y = "nox emissions (tons)", title = " ")



#comparing actual and counterfactual data

ampd_daily_emissions <- read.fst ("/Volumes/GoogleDrive/My Drive/R/ampd-raw-data-processing/data/ampd_daily_emission.fst")

ampd_daily_emissions_1_4 <- ampd_daily_emissions %>% filter (year >=2015 & month >=1 & month <=12)
ampd_daily_emissions_1_4 <- ampd_daily_emissions_1_4 %>% filter (ORISPL_CODE %in% facility_0op_0nox)

ampd_merged <- ampd_daily_emissions_1_4 %>% left_join(data.counterfactual.nox, by =c ("ID", "date"))

names(ampd_merged)

ampd_merged_small <- ampd_merged %>% dplyr::select(STATE, ID, year, month, day, NOx..tons., NOx..tons.ct)

# ampd_merged_small$SO2 <- as.data.table(ampd_merged_small$SO2..tons.ct)

dfx<- setDT(ampd_merged_small)[, .(NOx..tons. = sum(NOx..tons., na.rm=TRUE),
                                   NOx..tons.ct = sum(NOx..tons.ct, na.rm=TRUE)), 
                               by = .(STATE, ID, year, month)]
ampd_ym<- aggregate(list (NOx..tons.=dfx$NOx..tons.), by=list( year=dfx$year,
                                                               month=dfx$month), FUN=sum)


years <- c ( 2017, 2018, 2019, 2020)

ampd_ym %>% filter( year %in% years) %>%
  ggplot(aes(month, NOx..tons., color= year, group= year )) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "NOx Emission (tons)",
                                   title = " ") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) 


#comparing 2020 actual vs counterfactual emission
ampd_daily_emissions_2020 <- ampd_daily_emissions %>% filter (year ==2020 & month >=1 & month <=12)
ampd_daily_emissions_2020 <- ampd_daily_emissions_2020 %>% filter (ORISPL_CODE %in% facility_0op_0nox)
ampd_merged_2020 <- ampd_daily_emissions_2020 %>% left_join(data.counterfactual.nox, by =c ("ID", "date"))

# names(ampd_merged)

ampd_merged_2020 <- ampd_merged_2020 %>% dplyr::select(STATE, ID, year, month, day, NOx..tons., NOx..tons.ct)



dfx_2020<- setDT(ampd_merged_2020)[, .(NOx..tons. = sum(NOx..tons., na.rm=TRUE),
                                       NOx..tons.ct=sum(NOx..tons.ct, na.rm=TRUE)), 
                                   by = .(STATE, ID, year, month)]
ampd_ym<- as.data.table(aggregate(list (NOx..tons.=dfx_2020$NOx..tons.,
                                        NOx..tons.ct=dfx_2020$NOx..tons.ct), by=list( year=dfx_2020$year,
                                                                                      month=dfx_2020$month), FUN=sum))
ampd_ym$month2 <- paste0("0", ampd_ym$month)
ampd_ym<- ampd_ym [, yr.mn := paste(year, month2, sep = "-")]
ampd_ym <- ampd_ym%>% 
  as_tibble() %>% 
  mutate(Date = paste0(as.character(yr.mn), "-01")) %>% 
  mutate(Date = lubridate::as_date(Date, format = "%Y-%m-%d"))

years <- c (  2020)

ampd_ym %>% filter( year %in% years) %>%
  ggplot(aes(Date )) +
  geom_point(aes(y= NOx..tons.), color="red") + geom_point(aes(y= NOx..tons.ct),color="blue")+
  geom_line(aes(y= NOx..tons.), color="red")+ 
  geom_line(aes(y= NOx..tons.ct),color="blue")+
  labs(x= "", y = expression(paste("",NOx , " Emissions (tons)")),title = " ") +
  theme_bw() + expand_limits( y = 0)  +
  theme(text = element_text(size=30)) 

ggsave("NOx_monthly_all.png", path = "/Volumes/GoogleDrive/My Drive/R/causal-study-COVID-beyond/plot")

sum(ampd_ym$NOx..tons.)/sum(ampd_ym$NOx..tons.ct)



# =================================plotting NOx emissions vs month (counterfactual as it is vs
#                                                                   -ve Counterfactual replaced with 0)=======

#monthly plots quantile wise
ampd_daily_emissions <- read.fst ("/Volumes/GoogleDrive/My Drive/R/ampd-raw-data-processing/data/ampd_daily_emission.fst")
ampd_daily_emissions_2020 <- ampd_daily_emissions %>% filter (year ==2020 & month >=1 & month <=12)


#counterfactual values as it is
# facilities 0-25% operations monthly NOx

ampd_daily_emissions_2020_0_25 <- ampd_daily_emissions_2020 %>% filter (ORISPL_CODE %in% facility.id.25)
ampd_merged_2020_0_25 <- ampd_daily_emissions_2020_0_25 %>% left_join(data.counterfactual.nox, by =c ("ID", "date"))

ampd_merged_2020_0_25 <- ampd_merged_2020_0_25 %>% dplyr::select(STATE, ID, year, month, day, NOx..tons., NOx..tons.ct)

dfx_2020_0_25<- setDT(ampd_merged_2020_0_25)[, .(NOx..tons. = sum(NOx..tons., na.rm=TRUE),
                                                 NOx..tons.ct=sum(NOx..tons.ct, na.rm=TRUE)), 
                                             by = .(STATE, ID, year, month)]
ampd_ym_2020_0_25<- aggregate(list (NOx..tons.=dfx_2020_0_25$NOx..tons.,
                                    NOx..tons.ct=dfx_2020_0_25$NOx..tons.ct), by=list( year=dfx_2020_0_25$year,
                                                                                       month=dfx_2020_0_25$month), FUN=sum)

ampd_ym_2020_0_25$months <- month.abb[ampd_ym_2020_0_25$month]
years <- c (  2020)

ampd_ym_2020_0_25 %>% filter( year %in% years) %>%
  ggplot(aes(month )) +
  geom_point(aes(y= NOx..tons.), color="red") + 
  geom_point(aes(y= NOx..tons.ct),color="blue")+geom_line(aes(y= NOx..tons.), color="red")+ 
  geom_line(aes(y= NOx..tons.ct),color="blue")+
  labs(x= "Month", y = "NOx Emission (tons)",title = " ") + theme_bw() 

# facilities 25-50% operations monthly NOx
ampd_daily_emissions_2020_25_50 <- ampd_daily_emissions_2020 %>% filter (ORISPL_CODE %in% facility.id.25.50)
ampd_merged_2020_25_50 <- ampd_daily_emissions_2020_25_50 %>% left_join(data.counterfactual.nox, by =c ("ID", "date"))

ampd_merged_2020_25_50 <- ampd_merged_2020_25_50 %>% dplyr::select(STATE, ID, year, month, day, NOx..tons., NOx..tons.ct)

dfx_2020_25_50<- setDT(ampd_merged_2020_25_50)[, .(NOx..tons. = sum(NOx..tons., na.rm=TRUE),
                                                   NOx..tons.ct=sum(NOx..tons.ct, na.rm=TRUE)), 
                                               by = .(STATE, ID, year, month)]
ampd_ym_2020_25_50<- aggregate(list (NOx..tons.=dfx_2020_25_50$NOx..tons.,
                                     NOx..tons.ct=dfx_2020_25_50$NOx..tons.ct), by=list( year=dfx_2020_25_50$year,
                                                                                         month=dfx_2020_25_50$month), FUN=sum)

ampd_ym_2020_25_50$months <- month.abb[ampd_ym_2020_25_50$month]
years <- c (  2020)

ampd_ym_2020_25_50 %>% filter( year %in% years) %>%
  ggplot(aes(month )) +
  geom_point(aes(y= NOx..tons.), color="red") + 
  geom_point(aes(y= NOx..tons.ct),color="blue")+geom_line(aes(y= NOx..tons.), color="red")+ 
  geom_line(aes(y= NOx..tons.ct),color="blue")+
  labs(x= "Month", y = "NOx Emission (tons)",title = " ") + theme_bw()  

# facilities 50-75% operations monthly NOx
ampd_daily_emissions_2020_50_75 <- ampd_daily_emissions_2020 %>% filter (ORISPL_CODE %in% facility.id.50.75)
ampd_merged_2020_50_75 <- ampd_daily_emissions_2020_50_75 %>% left_join(data.counterfactual.nox, by =c ("ID", "date"))

ampd_merged_2020_50_75 <- ampd_merged_2020_50_75 %>% dplyr::select(STATE, ID, year, month, day, NOx..tons., NOx..tons.ct)

dfx_2020_50_75<- setDT(ampd_merged_2020_50_75)[, .(NOx..tons. = sum(NOx..tons., na.rm=TRUE),
                                                   NOx..tons.ct=sum(NOx..tons.ct, na.rm=TRUE)), 
                                               by = .(STATE, ID, year, month)]
ampd_ym_2020_50_75<- aggregate(list (NOx..tons.=dfx_2020_50_75$NOx..tons.,
                                     NOx..tons.ct=dfx_2020_50_75$NOx..tons.ct), by=list( year=dfx_2020_50_75$year,
                                                                                         month=dfx_2020_50_75$month), FUN=sum)

ampd_ym_2020_50_75$months <- month.abb[ampd_ym_2020_50_75$month]
years <- c (  2020)

ampd_ym_2020_50_75 %>% filter( year %in% years) %>%
  ggplot(aes(month )) +
  geom_point(aes(y= NOx..tons.), color="red") + 
  geom_point(aes(y= NOx..tons.ct),color="blue")+geom_line(aes(y= NOx..tons.), color="red")+ 
  geom_line(aes(y= NOx..tons.ct),color="blue")+
  labs(x= "Month", y = "NOx Emission (tons)",title = " ") + theme_bw()  +
  scale_x_continuous(limits = c(1,12)) + 
  scale_y_continuous(limits = c(0,25000)) 


# facilities with more than 75% operations result
ampd_daily_emissions_2020_75_100 <- ampd_daily_emissions_2020 %>% filter (ORISPL_CODE %in% facility.id.75.100)
ampd_merged_2020_75_100 <- ampd_daily_emissions_2020_75_100 %>% left_join(data.counterfactual.nox, by =c ("ID", "date"))

names(ampd_merged)

ampd_merged_2020_75_100 <- ampd_merged_2020_75_100 %>% dplyr::select(STATE, ID, year, month, day, NOx..tons., NOx..tons.ct)



dfx_2020_75_100<- setDT(ampd_merged_2020_75_100)[, .(NOx..tons. = sum(NOx..tons., na.rm=TRUE),
                                                     NOx..tons.ct=sum(NOx..tons.ct, na.rm=TRUE)), 
                                                 by = .(STATE, ID, year, month)]
ampd_ym_2020_75_100<- aggregate(list (NOx..tons.=dfx_2020_75_100$NOx..tons.,
                                      NOx..tons.ct=dfx_2020_75_100$NOx..tons.ct), by=list( year=dfx_2020_75_100$year,
                                                                                           month=dfx_2020_75_100$month), FUN=sum)
ampd_ym_2020_75_100$months <- month.abb[ampd_ym_2020_75_100$month]

ampd_ym_2020_75_100 %>% filter( year %in% years) %>%
  ggplot(aes(month )) +
  geom_point(aes(y= NOx..tons.), color="red") + geom_point(aes(y= NOx..tons.ct),
                                                           color="blue")+geom_line(aes(y= NOx..tons.), color="red")+ 
  geom_line(aes(y= NOx..tons.ct),color="blue")+
  labs(x= "Month", y = "NOx Emission (tons)",title = " ") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) + theme_bw() 

ampd_ym_2020_0_25$group <- "0-25"
ampd_ym_2020_25_50$group <- "25-50"
ampd_ym_2020_50_75$group <- "50-75"
ampd_ym_2020_75_100$group <- "75-100"

ampd_ym_2020_0_25$ct.type <- "-ve.as.it.is"
ampd_ym_2020_25_50$ct.type  <- "-ve.as.it.is"
ampd_ym_2020_50_75$ct.type  <-"-ve.as.it.is"
ampd_ym_2020_75_100$ct.type  <-"-ve.as.it.is"

#counterfactual values replaced with 0
data.counterfactual.nox$NOx..tons.ct.0 <- data.counterfactual.nox$NOx..tons.ct

data.counterfactual.nox$NOx..tons.ct.0 <- with(data.counterfactual.nox, ifelse(NOx..tons.ct<0, 0, NOx..tons.ct))


# facilities 0-25% operations monthly NOx
ampd_daily_emissions_2020_0_25_ct <- ampd_daily_emissions_2020 %>% filter (ORISPL_CODE %in% facility.id.25)
ampd_merged_2020_0_25_ct <- ampd_daily_emissions_2020_0_25_ct %>% left_join(data.counterfactual.nox, by =c ("ID", "date"))

ampd_merged_2020_0_25_ct <- ampd_merged_2020_0_25_ct %>% dplyr::select(STATE, ID, year, month, day,
                                                                       NOx..tons.,  NOx..tons.ct.0)

dfx_2020_0_25_ct<- setDT(ampd_merged_2020_0_25_ct)[, .(NOx..tons. = sum(NOx..tons., na.rm=TRUE),
                                                       NOx..tons.ct=sum(NOx..tons.ct.0, na.rm=TRUE)), 
                                                   by = .(STATE, ID, year, month)]
ampd_ym_2020_0_25_ct<- aggregate(list (NOx..tons.=dfx_2020_0_25_ct$NOx..tons.,
                                       NOx..tons.ct=dfx_2020_0_25_ct$NOx..tons.ct), by=list( year=dfx_2020_0_25_ct$year,
                                                                                             month=dfx_2020_0_25_ct$month), FUN=sum)

ampd_ym_2020_0_25_ct$months <- month.abb[ampd_ym_2020_0_25_ct$month]
years <- c (  2020)

ampd_ym_2020_0_25_ct %>% filter( year %in% years) %>%
  ggplot(aes(month )) +
  geom_point(aes(y= NOx..tons.), color="red") + 
  geom_point(aes(y= NOx..tons.ct),color="blue")+geom_line(aes(y= NOx..tons.), color="red")+ 
  geom_line(aes(y= NOx..tons.ct),color="blue")+
  labs(x= "Month", y = "NOx Emission (tons)",title = " ") + theme_bw() 

# facilities 25-50% operations monthly NOx
ampd_daily_emissions_2020_25_50_ct <- ampd_daily_emissions_2020 %>% filter (ORISPL_CODE %in% facility.id.25.50)
ampd_merged_2020_25_50_ct <- ampd_daily_emissions_2020_25_50_ct %>% left_join(data.counterfactual.nox, by =c ("ID", "date"))

ampd_merged_2020_25_50_ct <- ampd_merged_2020_25_50_ct %>% dplyr::select(STATE, ID, year, month, day,
                                                                         NOx..tons.,  NOx..tons.ct.0)

dfx_2020_25_50_ct<- setDT(ampd_merged_2020_25_50_ct)[, .(NOx..tons. = sum(NOx..tons., na.rm=TRUE),
                                                         NOx..tons.ct=sum(NOx..tons.ct.0, na.rm=TRUE)), 
                                                     by = .(STATE, ID, year, month)]
ampd_ym_2020_25_50_ct<- aggregate(list (NOx..tons.=dfx_2020_25_50_ct$NOx..tons.,
                                        NOx..tons.ct=dfx_2020_25_50_ct$NOx..tons.ct), by=list( year=dfx_2020_25_50_ct$year,
                                                                                               month=dfx_2020_25_50_ct$month), FUN=sum)

ampd_ym_2020_25_50_ct$months <- month.abb[ampd_ym_2020_25_50_ct$month]
years <- c (  2020)

ampd_ym_2020_25_50_ct %>% filter( year %in% years) %>%
  ggplot(aes(month )) +
  geom_point(aes(y= NOx..tons.), color="red") + 
  geom_point(aes(y= NOx..tons.ct),color="blue")+geom_line(aes(y= NOx..tons.), color="red")+ 
  geom_line(aes(y= NOx..tons.ct),color="blue")+
  labs(x= "Month", y = "NOx Emission (tons)",title = " ") + theme_bw()  

# facilities 50-75% operations monthly NOx
ampd_daily_emissions_2020_50_75_ct <- ampd_daily_emissions_2020 %>% filter (ORISPL_CODE %in% facility.id.50.75)
ampd_merged_2020_50_75_ct <- ampd_daily_emissions_2020_50_75_ct %>% left_join(data.counterfactual.nox, by =c ("ID", "date"))

ampd_merged_2020_50_75_ct <- ampd_merged_2020_50_75_ct %>% dplyr::select(STATE, ID, year, month, day, 
                                                                         NOx..tons., NOx..tons.ct.0)

dfx_2020_50_75_ct<- setDT(ampd_merged_2020_50_75_ct)[, .(NOx..tons. = sum(NOx..tons., na.rm=TRUE),
                                                         NOx..tons.ct=sum(NOx..tons.ct.0, na.rm=TRUE)), 
                                                     by = .(STATE, ID, year, month)]
ampd_ym_2020_50_75_ct<- aggregate(list (NOx..tons.=dfx_2020_50_75_ct$NOx..tons.,
                                        NOx..tons.ct=dfx_2020_50_75_ct$NOx..tons.ct), by=list( year=dfx_2020_50_75_ct$year,
                                                                                               month=dfx_2020_50_75_ct$month), FUN=sum)

ampd_ym_2020_50_75_ct$months <- month.abb[ampd_ym_2020_50_75_ct$month]
years <- c (  2020)

ampd_ym_2020_50_75_ct %>% filter( year %in% years) %>%
  ggplot(aes(month )) +
  geom_point(aes(y= NOx..tons.), color="red") + 
  geom_point(aes(y= NOx..tons.ct),color="blue")+geom_line(aes(y= NOx..tons.), color="red")+ 
  geom_line(aes(y= NOx..tons.ct),color="blue")+
  labs(x= "Month", y = "NOx Emission (tons)",title = " ") + theme_bw()  


# facilities with more than 75% operations result
ampd_daily_emissions_2020_75_100_ct <- ampd_daily_emissions_2020 %>% filter (ORISPL_CODE %in% facility.id.75.100)
ampd_merged_2020_75_100_ct <- ampd_daily_emissions_2020_75_100_ct %>% left_join(data.counterfactual.nox, by =c ("ID", "date"))



ampd_merged_2020_75_100_ct <- ampd_merged_2020_75_100_ct %>% dplyr::select(STATE, ID, year, month, day,
                                                                           NOx..tons., NOx..tons.ct.0)



dfx_2020_75_100_ct<- setDT(ampd_merged_2020_75_100_ct)[, .(NOx..tons. = sum(NOx..tons., na.rm=TRUE),
                                                           NOx..tons.ct=sum(NOx..tons.ct.0, na.rm=TRUE)), 
                                                       by = .(STATE, ID, year, month)]
ampd_ym_2020_75_100_ct<- aggregate(list (NOx..tons.=dfx_2020_75_100_ct$NOx..tons.,
                                         NOx..tons.ct=dfx_2020_75_100_ct$NOx..tons.ct), by=list( year=dfx_2020_75_100_ct$year,
                                                                                                 month=dfx_2020_75_100_ct$month), FUN=sum)
ampd_ym_2020_75_100_ct$months <- month.abb[ampd_ym_2020_75_100_ct$month]

ampd_ym_2020_75_100_ct %>% filter( year %in% years) %>%
  ggplot(aes(month )) +
  geom_point(aes(y= NOx..tons.), color="red") + geom_point(aes(y= NOx..tons.ct),
                                                           color="blue")+geom_line(aes(y= NOx..tons.), color="red")+ 
  geom_line(aes(y= NOx..tons.ct),color="blue")+
  labs(x= "Month", y = "NOx Emission (tons)",title = " ") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) + theme_bw()

ampd_ym_2020_0_25_ct$group <- "0-25"
ampd_ym_2020_25_50_ct$group <- "25-50"
ampd_ym_2020_50_75_ct$group <- "50-75"
ampd_ym_2020_75_100_ct$group <- "75-100"

ampd_ym_2020_0_25_ct$ct.type <- "0.replaced.-ve"
ampd_ym_2020_25_50_ct$ct.type  <- "0.replaced.-ve"
ampd_ym_2020_50_75_ct$ct.type  <- "0.replaced.-ve"
ampd_ym_2020_75_100_ct$ct.type  <- "0.replaced.-ve"

ampd_ym_nox <- rbind(ampd_ym_2020_0_25, ampd_ym_2020_25_50, ampd_ym_2020_50_75, ampd_ym_2020_75_100,
                     ampd_ym_2020_0_25_ct, ampd_ym_2020_25_50_ct, ampd_ym_2020_50_75_ct, ampd_ym_2020_75_100_ct)

# ampd_ym_nox$pct.increase <- (1-(ampd_ym_nox$SO2..tons./ampd_ym_so2$SO2..tons.ct)*100)

ampd_ym_nox %>%
  ggplot(aes(month )) +
  geom_point(aes(y= NOx..tons.), color="red") + geom_point(aes(y= NOx..tons.ct), color="blue")+
  geom_line(aes(y= NOx..tons.), color="red")+ 
  geom_line(aes(y= NOx..tons.ct),color="blue")+
  labs(x= "Month", y = "NOx Emission (tons)",title = " ") +theme_bw() +  scale_x_continuous(breaks = c(1:12))+
  facet_grid(group~ ct.type)


ggsave("NOx.png", path = "/Volumes/GoogleDrive/My Drive/R/causal-study/plots")








#+====================================Percentage ATT increased/decreased calculation=======================

#getting 2019 gsynth model data
ampd_daily_all_units<- read.fst ("data/ampd_daily_cleaned_facility.fst")

ampd_daily_all_units <- ampd_daily_all_units

ampd_daily_all_units$NOx..tons.[is.na(ampd_daily_all_units$NOx..tons.)] <-  0
ampd_daily_all_units$SO2..tons.[is.na(ampd_daily_all_units$SO2..tons.)] <-  0
ampd_daily_all_units$CO2..tons.[is.na(ampd_daily_all_units$CO2..tons.)] <-  0

facility <- as.vector(unique(ampd_daily_all_units$ORISPL_CODE))
rm(ampd_daily_all_units)
load ("data/result_facility_nox_2020_whole.RData")

# getting each units actual emission into a dataframe====================================================
datalist = list()

for (i in 1:length(facility)) {
  nox.emis <- na.omit(as.data.frame(result_nox_whole_2020[[i]][[2]]$Y.bar))
  nox.emis <- nox.emis %>% dplyr::select(-Y.co.bar)
  nox.emis$day <- seq(1: NROW(nox.emis$Y.tr.bar))
  
  nox.emis <- nox.emis %>% filter(day>=61)
  nox.emis <- nox.emis %>% dplyr::select(-day) 
  nox.emis.tr.sum <- sum(nox.emis$Y.tr.bar, na.rm=T)
  nox.emis.ct.sum <- sum(nox.emis$Y.ct.bar, na.rm=T)
  fac.1 <- as.data.frame(facility[i])
  datalist[[i]] <- cbind(fac.1,nox.emis.tr.sum, nox.emis.ct.sum) # add it to your list
}

ac.ct.nox.emission = do.call(rbind, datalist)

names(ac.ct.nox.emission)[names(ac.ct.nox.emission) == 'facility[i]'] <- 'facility'

all.facility.nox <- merge(all.facility.nox, ac.ct.nox.emission, by = "facility")


#is it right approach to sum up upper and lower confidence interval?

#facilities operating more than 0% during 2020
all.facility.nox <- all.facility.nox %>% filter(op.pct >0)

#there could be some facilities which were emitting 0 nox emissions during 2020 (1866, 3494, 2104 etc.) but were operating more than 0% of time in 2020
#number of these facilities
sum(is.na(all.facility.nox$r))

all.facility.nox <- all.facility.nox %>% filter (r !="NA")


#plotting US Map for all facility 
all.facility.nox.updated<- setDT(all.facility.nox)[, .(nox.emis.tr.sum = sum(nox.emis.tr.sum, na.rm=TRUE),
                                                       nox.emis.ct.sum = sum (nox.emis.ct.sum, na.rm=T),
                                                       Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                                       Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE)),
                                                   by = .(STATE)]

# all.facility.nox.updated$pct <- (all.facility.nox.updated$nox.emis.tr.sum- all.facility.nox.updated$nox.emis.ct.sum )/  all.facility.nox.updated$nox.emis.ct.sum
all.facility.nox.updated$pct <- (all.facility.nox.updated$nox.emis.tr.sum)/  all.facility.nox.updated$nox.emis.ct.sum

all.facility.nox.updated %>% top_n (-3, pct)

colnames(all.facility.nox.updated) [1] <- "state"
plot_usmap(data = all.facility.nox.updated, values = "pct", color = "black", labels = F,  exclude = c("AK", "HI")) +
  scale_fill_gradient2(low="blue", mid= "white", high="red", name = expression(paste("ATT " , NOx , " tons/day"))) +
  theme(legend.position = c( 0.25, .9), legend.direction = "horizontal" ,
        legend.text = element_text(size = 20),text = element_text(size=30)) + 
  guides(fill = guide_colorbar( label.position = "top",
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= -1.5,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1)) 
