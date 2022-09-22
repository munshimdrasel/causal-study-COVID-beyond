rm(list = ls())

library(fst)
library(data.table)
library(tidyverse)
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
library( sf)
library( USAboundaries)
library(gstat)
library(maptools)

# setwd ("/projects/HAQ_LAB/mrasel/R/causal-study-COVID-beyond")

setwd ("/Volumes/GoogleDrive/My Drive/R/causal-study-COVID-beyond")

so2.facility <- as.data.table(read.fst( "data/all.facility.so2.2020.fst"))



so2.facility[, Fuel1.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Primary.))]
so2.facility[Fuel.Type..Primary. == "", Fuel1.IsCoal := NA]

so2.coal.2020 <- so2.facility %>% filter (Fuel1.IsCoal==1)

#percent contribution from coal facilities

sum(so2.coal.2020$Y.tr.bar)/sum(so2.facility$Y.tr.bar)

so2.weeks <- so2.facility 

so2.facility <- so2.facility %>%  filter (week>8)



sum(so2.facility$ATT, na.rm=T)

#rate
sum(so2.facility$ATT, na.rm=T)/(52-8)

so2.facility.coal <- so2.facility %>%  filter (Fuel1.IsCoal==1)
sum(so2.facility.coal$ATT, na.rm=T)
sum(so2.facility.coal$Y.tr.bar)

sum(so2.facility.coal$Y.tr.bar)/sum(so2.facility$Y.tr.bar)
#plotting US Map for all facility 
so2.facility.states<- setDT(so2.facility)[, .(ATT = sum(ATT, na.rm=TRUE),
                                              Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                              Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE),
                                              actual.so2.emis = sum(Y.tr.bar, na.rm=T),
                                              ct.so2.emis = sum(Y.ct.bar, na.rm=T)), 
                                          by = .(STATE)]
so2.facility.week<- setDT(so2.weeks)[, .(ATT = sum(ATT, na.rm=TRUE),
                                         Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                         Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE),
                                         actual.so2.emis = sum(Y.tr.bar, na.rm=T),
                                         ct.so2.emis = sum(Y.ct.bar, na.rm=T)), 
                                     by = .(week)]

#confidence interval
# Calculate the mean and standard error
l.model <- lm(ATT ~ 1, so2.facility.states)

# Calculate the confidence interval
confint(l.model, level=0.95)



so2.facility.week %>% ggplot(aes(x=week)) + geom_line(aes(y=actual.so2.emis,  color="red")) +
  geom_line(aes(y=ct.so2.emis)) + geom_vline(xintercept=8)


so2.facility.yearly<- setDT(so2.facility)[, .(ATT = sum(ATT, na.rm=TRUE),
                                              Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                              Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE),
                                              actual.so2.emis = sum(Y.tr.bar, na.rm=T),
                                              ct.so2.emis = sum(Y.ct.bar, na.rm=T)), 
                                          by = .(facility)]
#facilities with increased ATT
so2.facility.yearly %>% filter (ATT>=0)

so2.facility.yearly$group <- "increased"
so2.facility.yearly$group <- ifelse(so2.facility.yearly$ATT<0, "decreased", "increased")

so2.facility.yearly %>% ggplot(aes(x=Facility.Longitude, y=Facility.Latitude, color=group)) + geom_point()

# write.csv(so2.facility.states, "./data/states.att.march.dec2020.so2.csv")


so2.facility.states %>% top_n (3, ATT )

x <- sum(so2.facility.states$actual.so2.emis)
y <- sum(so2.facility.states$ct.so2.emis)

pct.increase <- (x-y)*100/x

names(so2.facility.states)[names(so2.facility.states) == 'STATE'] <- 'state'

so2.facility.states <- so2.facility.states %>% mutate(pct=(so2.facility.states$actual.so2.emis-so2.facility.states$ct.so2.emis)*100/
                                                        so2.facility.states$actual.so2.emis)

plot_usmap(data = as.data.frame(so2.facility.states), values = "ATT", color = "black", labels = F,  exclude = c("AK", "HI")) +
  scale_fill_gradient2(low="blue", mid= "white", high="red", name = expression(paste("TTT " , SO[2] , " tons"))) +
  theme(legend.position = c( 0.25, .9), legend.direction = "horizontal" ,
        legend.text = element_text(size = 12),text = element_text(size=20)) + 
  guides(fill = guide_colorbar( label.position = "top",
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= -1.5,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1)) 

ggsave("longterm_so2_att.png", path = "./plots/")

#graphical abstract

plot_usmap(data = as.data.frame(so2.facility.states), values = "ATT", color = "black", labels = F,  exclude = c("AK", "HI")) +
  scale_fill_gradient2(low="blue", mid= "white", high="red", name = expression(paste("" , SO[2] , " change (tons)"))) +
  theme(legend.position = c( 0.25, .9), legend.direction = "horizontal" ,
        legend.text = element_text(size = 12),text = element_text(size=20)) + 
  guides(fill = guide_colorbar( label.position = "top",
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= -1.5,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1)) + labs (title='Longterm COVID-19 impact from power plant emissions')

ggsave("graphical_abstract.png", path = "./plots/")


so2.facility.states %>% top_n (3, ATT )
so2.facility.states %>% top_n (3, pct )

so2.facility.states %>%  filter (ATT<0)

#######lockdown


so2.facility.loc <- as.data.table(read.fst( "data/all.facility.so2.2020.fst"))


so2.facility.loc[, Fuel1.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Primary.))]
so2.facility.loc[Fuel.Type..Primary. == "", Fuel1.IsCoal := NA]

so2.facility.loc <- so2.facility.loc %>%  filter (week>8 & week <17)
sum(so2.facility.loc$ATT, na.rm=T)

#rate
sum(so2.facility.loc$ATT, na.rm=T)/ (17-8)

so2.facility.coal.loc <- so2.facility.loc %>%  filter (Fuel1.IsCoal==1)
sum(so2.facility.coal.loc$ATT, na.rm=T)


sum(so2.facility.coal.loc$Y.tr.bar)/sum(so2.facility.loc$Y.tr.bar)

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

# write.csv(so2.facility.states.loc, "./data/states.att.march.april2020.s02.csv")

so2.facility.states.loc %>% top_n (3, ATT )

so2.facility.states %>% top_n (3, ATT )

a <- sum(so2.facility.states.loc$actual.so2.emis)
b <- sum(so2.facility.states.loc$ct.so2.emis)

pct.increase.loc <- (a-b)*100/a

names(so2.facility.states.loc)[names(so2.facility.states.loc) == 'STATE'] <- 'state'

so2.facility.states.loc <- so2.facility.states.loc %>% mutate(pct=(so2.facility.states.loc$actual.so2.emis-so2.facility.states.loc$ct.so2.emis)*100/
                                                                so2.facility.states.loc$actual.so2.emis)

plot_usmap(data = as.data.frame(so2.facility.states.loc), values = "ATT", color = "black", labels = F,  exclude = c("AK", "HI")) +
  scale_fill_gradient2(low="blue", mid= "white", high="red", name = expression(paste("TTT " , SO[2] , " tons"))) +
  theme(legend.position = c( 0.25, .9), legend.direction = "horizontal" ,
        legend.text = element_text(size = 12),text = element_text(size=20)) + 
  guides(fill = guide_colorbar( label.position = "top",
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= -1.5,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1)) 
ggsave("shortterm_so2_att.png", path = "./plots/")

so2.facility.states.loc %>% top_n (-3, ATT )



#ttest for both lockdown period and whole intervention period (March-December 2020)


#march-december 2020

so2.facility <- as.data.table(read.fst( "data/all.facility.so2.2020.fst"))

so2.facility <- so2.facility %>%  filter (week>8)


states <- unique(as.vector(so2.facility$STATE))

datalist=list()

for (i in 1: length(states) ) {
  state.ac.ct <- so2.facility %>% filter (STATE %in% states[i])
  
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

so2.facility.loc <- read.fst( "data/all.facility.so2.2020.fst")


so2.facility.loc <- so2.facility.loc %>%  filter (week>8 & week <17)


states <- unique(as.vector(so2.facility.loc$STATE))

datalist=list()

for (i in 1: length(states) ) {
  state.ac.ct <- so2.facility.loc %>% filter (STATE %in% states[i])
  
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

#ttest

states <- unique(as.vector(so2.facility$STATE))

datalist=list()

for (i in 1: length(states) ) {
  state.ac.ct <- so2.facility %>% filter (STATE %in% states[i])
  
  ttest <- t.test(state.ac.ct$Y.tr.bar,state.ac.ct$Y.ct.bar, var.equal=T)
  
  df <- as.data.frame(ttest$p.value)
  names(df)[names(df) == 'ttest$p.value'] <- 'p.value'
  df$STATE <- states[i]
  datalist[[i]] <- df
  
}

states.ttest <- do.call(rbind, datalist)

states.ttest$sig <- ifelse(states.ttest$p.value<0.05, TRUE, FALSE)

count(states.ttest$sig==TRUE)


######coal SO2 spatial plot during March-Dec 2020
so2.facility <- as.data.table(read.fst( "data/all.facility.so2.2020.fst"))

so2.facility[, Fuel1.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Primary.))]
so2.facility[Fuel.Type..Primary. == "", Fuel1.IsCoal := NA]

so2.facility.coal <- so2.facility %>%  filter (Fuel1.IsCoal==1 & week>8)

so2.facility.coal.yearly <- setDT(so2.facility.coal)[, .(ATT = sum(ATT, na.rm=TRUE),
                                                         Facility.Longitude = mean(Facility.Longitude, na.rm=TRUE),
                                                        Facility.Latitude = mean(Facility.Latitude, na.rm=TRUE),
                                                        actual.so2.emis = sum(Y.tr.bar, na.rm=T),
                                                        ct.so2.emis = sum(Y.ct.bar, na.rm=T)), 
                                                    by = .(facility)]


locs.sf <- st_as_sf( so2.facility.coal.yearly, 
                     coords = c( 'Facility.Longitude', 'Facility.Latitude'),
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
  geom_sf( data = locs.sf, aes( color = ATT)) + 
  # set the x and y limits
  coord_sf( xlim = c( -125, -67), ylim = c( 25, 50)) + 
  # change the color scale
  scale_color_gradient2(limits=c(-4000, 8000), low = 'blue' ,high = 'red') +
  # extend the color scale to include 0
  expand_limits( color = 0) +
  theme_bw()


##################################################################
## Spatial Interpolation
##################################################################

idw_lockdown <- so2.facility.coal %>% dplyr::select(ATT, Facility.Latitude, Facility.Longitude)
sp::coordinates(idw_lockdown) <- ~ Facility.Longitude + Facility.Latitude
x.range <- as.integer(range(idw_lockdown@coords[,1]))
y.range <- as.integer(range(idw_lockdown@coords[,2]))
x.range <- as.integer(c(-126, -66))
y.range <- as.integer(c(23,51))

grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=0.5), 
                   y=seq(from=y.range[1], to=y.range[2], by=0.5))
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE


# plot(grd, cex=1.5)
# points(lockdown, pch=1, col='red', cex=1)
# title("Interpolation Grid and Sample Points")
usmap <- readShapePoly("./data/cb_2019_us_nation_5m/cb_2019_us_nation_5m.shp")
usmapoutline <- fortify(usmap)



################## idw ##################

idw <- idw(ATT ~ 1, locations=idw_lockdown, newdata=grd, nmax=30)
idw.output <- as.data.frame(idw)
names(idw.output)[c(1:3)]<-c("lon","lat", "att.pred")

xy <- idw.output[,c("lon","lat")]
proj4string(usmap) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
spdf <- SpatialPointsDataFrame(coords = xy, data = idw.output,
                               proj4string = CRS("+proj=longlat +datum=WGS84 
                                                 +ellps=WGS84 +towgs84=0,0,0"))
over <- over(spdf, usmap, fn=NULL)
points <- cbind(spdf@data, over)
points$var1.var <- NULL
points <- na.omit(points)
plot_idw <- ggplot(data=points, aes(x=lon,y=lat))
layer1_idw <- c(geom_tile(data=points, aes(fill=att.pred)))
layer2_idw <- c(geom_path(data=usmap, aes(long, lat, group=group), colour = "grey40", size=1))

ggplot( grid.dat.sf.m[year %in% c( 'X2020')]) + 
  geom_sf( aes( fill = so2.facility.coal, 
                geometry = geometry),
           color = NA) + labs(title="Counterfactual Emissions 2020") 

## idw smoothed ATT map
plot_idw+layer1_idw+borders("state")+
  layer2_idw+scale_fill_gradient2(midpoint = 0, mid="white", low="blue", high="red")+coord_equal()+xlim(-126,-66)+ylim(23,51) + xlab("Longitude") + ylab("Latitude") + labs(fill="ATT") + ggtitle("IDW-Smoothed Average Treatment Effect Estimates")

## point-level ATT map
blank_layer <- c(geom_tile(data=points, fill="grey80"))
plot_idw+blank_layer+borders("state")+layer2_idw+geom_point(data = so2.facility.coal, aes(x=Facility.Longitude, y=Facility.Latitude, color=ATT)) + scale_color_gradient2(midpoint = 0, mid="#fff650", high="#fa7b05", low="#18b342")+coord_equal()+xlim(-126,-66)+ylim(23,51) + xlab("Longitude") + ylab("Latitude") + labs(fill="ATT", color="ATT") + ggtitle("Point-Level Average Treatment Effect Estimates")



##################################################################
## Spatial Interpolation
##################################################################

idw_lockdown <- aqs.sulfate %>% dplyr::select(mean, Latitude, Longitude)
sp::coordinates(idw_lockdown) <- ~ Longitude + Latitude
x.range <- as.integer(range(idw_lockdown@coords[,1]))
y.range <- as.integer(range(idw_lockdown@coords[,2]))
x.range <- as.integer(c(-126, -66))
y.range <- as.integer(c(23,51))

grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=0.5), 
                   y=seq(from=y.range[1], to=y.range[2], by=0.5))
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE


# plot(grd, cex=1.5)
# points(lockdown, pch=1, col='red', cex=1)
# title("Interpolation Grid and Sample Points")
usmap <- readShapePoly("./data/cb_2019_us_nation_5m/cb_2019_us_nation_5m.shp")
usmapoutline <- fortify(usmap)



################## idw ##################

idw <- idw(mean ~ 1, locations=idw_lockdown, newdata=grd, nmax=30)
idw.output <- as.data.frame(idw)
names(idw.output)[c(1:3)]<-c("lon","lat", "mean.sulfate")

xy <- idw.output[,c("lon","lat")]
proj4string(usmap) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
spdf <- SpatialPointsDataFrame(coords = xy, data = idw.output,
                               proj4string = CRS("+proj=longlat +datum=WGS84 
                                                 +ellps=WGS84 +towgs84=0,0,0"))
over <- over(spdf, usmap, fn=NULL)
points <- cbind(spdf@data, over)
points$var1.var <- NULL
points <- na.omit(points)
plot_idw <- ggplot(data=points, aes(x=lon,y=lat))
layer1_idw <- c(geom_tile(data=points, aes(fill=mean.sulfate)))
layer2_idw <- c(geom_path(data=usmap, aes(long, lat, group=group), colour = "grey40", size=1))



## idw smoothed ATT map
plot_idw+layer1_idw+borders("state")+
  layer2_idw+scale_fill_gradient2( low="blue", high="red")+coord_equal()+xlim(-126,-66)+ylim(23,51) + xlab("Longitude") + ylab("Latitude") + labs(fill="Sulfate") + ggtitle("Sulfate 2020")

## point-level ATT map
blank_layer <- c(geom_tile(data=points, fill="grey80"))
plot_idw+blank_layer+borders("state")+layer2_idw+geom_point(data = so2.facility.coal, aes(x=Facility.Longitude, y=Facility.Latitude, color=ATT)) + scale_color_gradient2(midpoint = 0, mid="#fff650", high="#fa7b05", low="#18b342")+coord_equal()+xlim(-126,-66)+ylim(23,51) + xlab("Longitude") + ylab("Latitude") + labs(fill="ATT", color="ATT") + ggtitle("Point-Level Average Treatment Effect Estimates")



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
                          
                                   
                                   
so2.2020.compare <- so2%>%  ggplot(aes(x=week, y=SO2.tons, color=group)) + geom_line()  + geom_vline(xintercept=8) +
  geom_vline(xintercept=16) + annotate("rect", xmin = 8, xmax = 52, ymin = 0, ymax = Inf, fill = "blue", alpha = .1, color = NA) +
  theme_bw() + scale_x_continuous(expand = c(0, 0), limits = c(0, 52)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 16000)) + theme (legend.position = c(0.6,0.2),
                                                                   legend.title = element_blank(),
                                                                   legend.text = element_text(size = 10)) +
  labs(x="Weeks", y=expression(paste(SO[2], " tons")), title = "") 

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


nox.2020.compare <-nox%>%  ggplot(aes(x=week, y=nox.tons, color=group)) + geom_line()  + geom_vline(xintercept=8) +
  geom_vline(xintercept=16) + annotate("rect", xmin = 8, xmax = 52, ymin = 0, ymax = Inf, fill = "blue", alpha = .1, color = NA) +
  theme_bw() + scale_x_continuous(expand = c(0, 0), limits = c(0, 52)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 16000)) + theme (legend.position = c(0.6,0.2),
                                                                   legend.title = element_blank(),
                                                                   legend.text = element_text(size = 10)) +
  labs(x="Weeks", y=expression(paste(NO[x], " tons")), title = "") 

# ggsave("compare_nox_2020.png", path = "./plots/")


plot_grid(so2.2020.compare, 
          nox.2020.compare, 
          labels = c('A', 'B'), 
          label_size = 12)

ggsave("compare_nox_so2_2020.png", path = "./plots/", width=10, height=4, units="in")
