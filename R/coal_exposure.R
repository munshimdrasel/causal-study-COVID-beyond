rm(list = ls())

library( fst)
library( data.table)
library( magrittr)
library( raster)
library( sf)
library( ggplot2)
library(dplyr)
library(cowplot)

setwd ("/Volumes/GoogleDrive/My Drive/R/causal-study-COVID-beyond")

ct_source_impacts_dir <- './data/counterfactual_disperseR/disperseR/hyads_to_PM'

#coordinate reference system projection string for spatial data
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"

# get the names of the gridded HyADS output files from the data directory
grid.files.yr <- list.files( ct_source_impacts_dir,
                             pattern = 'grids_pm25_total_\\d{4}\\.fst',
                             full.names = TRUE)

 # x <- read.fst("/Volumes/GoogleDrive/My Drive/R/causal-study-COVID-beyond/data/counterfactual_disperseR/disperseR/hyads_to_PM/grids_pm25_total_2020_03.fst")
# read select files and combine into single data.table
grid.dat <- lapply( grid.files.yr,
                    function( f){
                      year.f <- gsub( '^.*_|\\.fst', '', f)
                      
                      in.f <- read.fst( f, as.data.table = T)
                      setnames( in.f, 'vals.out', 'counterfactual_coal_pm25')
                      in.f[, year := year.f]
                    }) %>% rbindlist

# summarize the data
summary( grid.dat)


# get the names of the gridded HyADS output files
# grid.files.unit.yr <- list.files( ct_source_impacts_dir,
#                                   pattern = 'grids_pm25_byunit_\\d{4}\\.fst',
#                                   full.names = TRUE)

# read select files
# grid.unit.dat <- lapply( grid.files.unit.yr,
#                          function( f){
#                            print( f)
#                            year.f <- gsub( '^.*_|\\.fst', '', f) %>%
#                              as( 'integer')
#                            
#                            in.f <- read.fst( f, as.data.table = T)
#                            in.f[, year := year.f]
#                            
#                            in.f[ is.na( in.f)] <- 0
#                            return (in.f)
#                          }) %>% rbindlist( fill = T)
# 
# 
# # check the dimensions
# dim( grid.unit.dat)



# first, use dcast to get year columns
grid.dat.c <- dcast( grid.dat, x + y ~ year, value.var = 'counterfactual_coal_pm25')

# convert to raster
# grid.dat.r <- rasterFromXYZ( grid.dat.c, crs = p4s)

# plot, which will show only ~16 of the years contained in grid.dat.r
# plot( grid.dat.r)


# create sf object
# grid.dat.sf <- rasterToPolygons( grid.dat.r) %>%
#   st_as_sf()

# convert object to data.table and melt across years
grid.dat.c <- data.table( grid.dat.c)
# grid.dat.sf.m <- melt( grid.dat.sf.dt, 
#                        id.vars = 'geometry',
#                        variable.name = 'year',
#                        value.name = 'coal_pm25')

counterfactual <- grid.dat.c

names(counterfactual)[names(counterfactual) == '2020'] <- 'counterfactual_coal_pm25'


summary(counterfactual)
# plot 4 years of data
# x <- ggplot( grid.dat.sf.m[year %in% c( 'X2020')]) + 
#   geom_sf( aes( fill = coal_pm25, 
#                 geometry = geometry),
#            color = NA) + labs(title="Counterfactual Emissions 2020") 



ac_source_impacts_dir <- '/Volumes/GoogleDrive/My Drive/R/causal-study-COVID-beyond/data/actual_disperseR/disperseR/hyads_to_PM'

#coordinate reference system projection string for spatial data
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"

# get the names of the gridded HyADS output files from the data directory
grid.files.yr <- list.files( ac_source_impacts_dir,
                             pattern = 'grids_pm25_total_\\d{4}\\.fst',
                             full.names = TRUE)

# read select files and combine into single data.table
grid.dat <- lapply( grid.files.yr,
                    function( f){
                      year.f <- gsub( '^.*_|\\.fst', '', f)
                      
                      in.f <- read.fst( f, as.data.table = T)
                      setnames( in.f, 'vals.out', 'coal_pm25')
                      in.f[, year := year.f]
                    }) %>% rbindlist

# summarize the data
summary( grid.dat)


# get the names of the gridded HyADS output files
# grid.files.unit.yr <- list.files( ac_source_impacts_dir,
#                                   pattern = 'grids_pm25_byunit_\\d{4}\\.fst',
#                                   full.names = TRUE)
# 
# # read select files
# grid.unit.dat <- lapply( grid.files.unit.yr,
#                          function( f){
#                            print( f)
#                            year.f <- gsub( '^.*_|\\.fst', '', f) %>%
#                              as( 'integer')
#                            
#                            in.f <- read.fst( f, as.data.table = T)
#                            in.f[, year := year.f]
#                            
#                            in.f[ is.na( in.f)] <- 0
#                            return (in.f)
#                          }) %>% rbindlist( fill = T)


# check the dimensions
# dim( grid.unit.dat)



# first, use dcast to get year columns
grid.dat.c <- dcast( grid.dat, x + y ~ year, value.var = 'coal_pm25')

# convert to raster
# grid.dat.r <- rasterFromXYZ( grid.dat.c, crs = p4s)

# plot, which will show only ~16 of the years contained in grid.dat.r
# plot( grid.dat.r)


# create sf object
# grid.dat.sf <- rasterToPolygons( grid.dat.r) %>%
#   st_as_sf()

# convert object to data.table and melt across years
grid.dat.c  <- data.table( grid.dat.c )
# grid.dat.sf.m <- melt( grid.dat.sf.dt, 
#                        id.vars = 'geometry',
#                        variable.name = 'year',
#                        value.name = 'coal_pm25')

actual <- grid.dat.c 

names(actual)[names(actual) == '2020'] <- 'actual_coal_pm25'

summary(actual)
summary(counterfactual)

# plot 4 years of data
# y <- ggplot( grid.dat.sf.m[year %in% c( 'X2020')]) + 
#   geom_sf( aes( fill = coal_pm25, 
#                 geometry = geometry),
#            color = NA) + labs(title="Actual Emissions 2020")

# library(ggpubr)
# ggarrange (y, x, ncol=2, nrow=1, common.legend = TRUE)

# names(actual)[names(actual) == 'coal_pm25'] <- 'actual_coal_pm25'
# names(counterfactual)[names(counterfactual) == 'coal_pm25'] <- 'counterfactual_coal_pm25'

bind <- cbind(actual, counterfactual)

bind$diff <- bind$actual_coal_pm25-bind$counterfactual_coal_pm25

bind <- bind [ , c(1,2,3,6,7)]

bind$pct <- bind$diff*100/bind$actual_coal_pm25

# get USA dataset
states48 <- state.name[!(state.name %in% c( 'Alaska', 'Hawaii'))]
usa <- rnaturalearth::ne_states(#scale = 110, #type = "countries",
  country = "United States of America")
usa.sub <- spTransform( usa[usa$name %in% states48,], CRSobj = p4s)
usa.sub.sf <- st_as_sf( usa.sub)


longterm <- bind %>% ggplot(aes( x = x, y = y, fill = diff))+
  geom_tile() +
  scale_fill_gradient2(high = "red",
                      low = "blue",
                      breaks = c( -0.03, 0.00, 0.03, 0.06, 0.09),
                      labels = c( '-0.03', '0.00', '0.03', '0.06', '0.09'),
                      na.value = NA,
                      oob = scales::squish,
                      limits = c( -0.03, 0.09)) +
  geom_sf( data = usa.sub.sf,
           inherit.aes = FALSE,
           fill = NA, color = 'black', size = .3) +
  labs( fill = expression(paste( 'Change in Coal ', PM["2.5"], ', µg ', m^{"-3"}))) +
  scale_x_continuous( expand = c( 0, 0)) +
  scale_y_continuous( expand = c( 0, 0)) +
  theme_bw() +
  theme( axis.text = element_blank(),
         axis.title = element_blank(),
         axis.ticks = element_blank(),
         legend.direction = 'horizontal',
         legend.position = c( .1, 0.1),
         legend.text = element_text( size = 8),
         legend.title = element_text( size = 14),
         panel.background = element_rect( fill = 'white'),
         panel.grid = element_blank(),
         panel.border = element_blank())+
  guides(fill = guide_colorbar( label.position = "top",
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= -1.5,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1))



# ggsave("pm2.5.change.2020.longterm.png", path = "./plots/")

summary(bind)

#ttest between actual and counterfactual PM2.5 during March-Dec 2020
t.test(bind$actual_coal_pm25, bind$counterfactual_coal_pm25, var.equal=T)

#comparing lockdown effect march-april 2020



ac_source_impacts_dir <- '/Volumes/GoogleDrive/My Drive/R/causal-study-COVID-beyond/data/actual_disperseR/disperseR/hyads_to_PM'

#coordinate reference system projection string for spatial data
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"

# get the names of the gridded HyADS output files from the data directory
grid.files.mn.3 <- list.files( ac_source_impacts_dir,
                             pattern = c("grids_pm25_total_2020_03.fst"),
                             full.names = TRUE)

grid.files.mn.4 <- list.files( ac_source_impacts_dir,
                               pattern = c("grids_pm25_total_2020_04.fst"),
                               full.names = TRUE)

# read select files and combine into single data.table
grid.dat.3 <- lapply( grid.files.mn.3,
                    function( f){
                      year.f <- gsub( '^.*_|\\.fst', '', f)
                      
                      in.f <- read.fst( f, as.data.table = T)
                      setnames( in.f, 'vals.out', 'coal_pm25')
                      in.f[, month := year.f]
                    }) %>% rbindlist

grid.dat.4 <- lapply( grid.files.mn.4,
                      function( f){
                        year.f <- gsub( '^.*_|\\.fst', '', f)
                        
                        in.f <- read.fst( f, as.data.table = T)
                        setnames( in.f, 'vals.out', 'coal_pm25')
                        in.f[, month := year.f]
                      }) %>% rbindlist

grid.dat.3.4.ac <- rbind(grid.dat.3, grid.dat.4)

grid.dat.3.4.ac <- setDT(grid.dat.3.4.ac) [ , .(coal_pm25=mean(coal_pm25, na.rm=T)), by=.(x,y)]

grid.dat.3.4.ac$group <- "actual.coal.pm2.5"
# summarize the data
summary( grid.dat.3.4.ac)



# first, use dcast to get year columns
grid.dat.3.4.ac <- dcast( grid.dat.3.4.ac, x + y ~ group, value.var = 'coal_pm25')

# convert to raster
#grid.dat.r.3.4.ac <- rasterFromXYZ( grid.dat.3.4.ac , crs = p4s)

# plot, which will show only ~16 of the years contained in grid.dat.r
# plot( grid.dat.r.3.4.ac)


# create sf object
# grid.dat.sf.3.4.ac <- rasterToPolygons( grid.dat.r.3.4.ac) %>%
  # st_as_sf()

# convert object to data.table and melt across years
# grid.dat.sf.dt.3.4.ac <- data.table( grid.dat.sf.3.4.ac)
grid.dat.3.4.ac <- data.table( grid.dat.3.4.ac)

# grid.dat.sf.m.3.4.ac <- melt( grid.dat.3.4.ac, 
#                        id.vars = 'geometry',
#                        variable.name = 'group',
#                        value.name = 'coal_pm25')



# setDT(grid.dat.sf.m.3.4)[, .(coal_pm25 = mean(coal_pm25, na.rm=TRUE)), 
#                          by = .(month)]

actual.lockdown <- grid.dat.3.4.ac

summary(actual.lockdown)

# plot 4 years of data
# actual.lockdown.plot <- ggplot( actual.lockdown) + 
#   geom_sf( aes( x=x,y=y,fill = actual.coal.pm2.5),
#            color = NA) + labs(title="Actual Emissions during lockdown 2020")


#counterfactual COAL PM2.5 during lockdown period

ct_source_impacts_dir <- '/Volumes/GoogleDrive/My Drive/R/causal-study-COVID-beyond/data/counterfactual_disperseR/disperseR/hyads_to_PM'

#coordinate reference system projection string for spatial data
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"

# get the names of the gridded HyADS output files from the data directory
grid.files.mn.3.ct <- list.files( ct_source_impacts_dir,
                               pattern = c("grids_pm25_total_2020_03.fst"),
                               full.names = TRUE)

grid.files.mn.4.ct <- list.files( ct_source_impacts_dir,
                               pattern = c("grids_pm25_total_2020_04.fst"),
                               full.names = TRUE)

# read select files and combine into single data.table
grid.dat.3.ct <- lapply( grid.files.mn.3.ct,
                      function( f){
                        year.f <- gsub( '^.*_|\\.fst', '', f)
                        
                        in.f <- read.fst( f, as.data.table = T)
                        setnames( in.f, 'vals.out', 'coal_pm25')
                        in.f[, month := year.f]
                      }) %>% rbindlist

grid.dat.4.ct <- lapply( grid.files.mn.4.ct,
                      function( f){
                        year.f <- gsub( '^.*_|\\.fst', '', f)
                        
                        in.f <- read.fst( f, as.data.table = T)
                        setnames( in.f, 'vals.out', 'coal_pm25')
                        in.f[, month := year.f]
                      }) %>% rbindlist

grid.dat.3.4.ct <- rbind(grid.dat.3.ct, grid.dat.4.ct)

grid.dat.3.4.ct <- setDT(grid.dat.3.4.ct) [ , .(coal_pm25=mean(coal_pm25, na.rm=T)), by=.(x,y)]

grid.dat.3.4.ct$group <- "counterfactual.coal.pm2.5"
# summarize the data
summary( grid.dat.3.4.ct)



# first, use dcast to get year columns
grid.dat.3.4.ct <- dcast( grid.dat.3.4.ct, x + y ~ group, value.var = 'coal_pm25')

# convert to raster
# grid.dat.r.3.4.ct <- rasterFromXYZ( grid.dat.3.4.ct , crs = p4s)

# plot, which will show only ~16 of the years contained in grid.dat.r
# plot( grid.dat.r.3.4.ct)


# create sf object
# grid.dat.sf.3.4.ct <- rasterToPolygons( grid.dat.r.3.4.ct) %>%
  # st_as_sf()

# convert object to data.table and melt across years
grid.dat.3.4.ct <- data.table( grid.dat.3.4.ct)

# grid.dat.sf.m.3.4.ct <- melt( grid.dat.sf.dt.3.4.ct, 
#                               id.vars = 'geometry',
#                               variable.name = 'group',
#                               value.name = 'coal_pm25')



# setDT(grid.dat.sf.m.3.4)[, .(coal_pm25 = mean(coal_pm25, na.rm=TRUE)), 
#                          by = .(month)]

counterfactual.lockdown <- grid.dat.3.4.ct

summary(counterfactual.lockdown)

# plot 4 years of data
# counterfactual.lockdown.plot <- ggplot( actual.lockdown[group %in% c( 'actual.coal.pm2.5')]) + 
#   geom_sf( aes( fill = coal_pm25, 
#                 geometry = geometry),
#            color = NA) + labs(title="Actual Emissions during lockdown 2020")
# 
# summary(actual.lockdown)


# library(ggpubr)
# ggarrange (y, x, ncol=2, nrow=1, common.legend = TRUE)

# names(actual.lockdown)[names(actual.lockdown) == 'coal_pm25'] <- 'actual_coal_pm25'
# names(counterfactual.lockdown)[names(counterfactual.lockdown) == 'coal_pm25'] <- 'counterfactual_coal_pm25'

bind.lockdown <- cbind(actual.lockdown, counterfactual.lockdown)

bind.lockdown$change.pm2.5 <- bind.lockdown$actual.coal.pm2.5-bind.lockdown$counterfactual.coal.pm2.5

bind.lockdown <- bind.lockdown [ , c(1,2,3,6,7)]

bind.lockdown$pct <- bind.lockdown$change.pm2.5*100/bind.lockdown$actual.coal.pm2.5


# get USA dataset
states48 <- state.name[!(state.name %in% c( 'Alaska', 'Hawaii'))]
usa <- rnaturalearth::ne_states(#scale = 110, #type = "countries",
  country = "United States of America")
usa.sub <- spTransform( usa[usa$name %in% states48,], CRSobj = p4s)
usa.sub.sf <- st_as_sf( usa.sub)


lockdown <- bind.lockdown %>% ggplot(aes( x = x, y = y, fill = change.pm2.5))+
  geom_tile() +
  scale_fill_gradient2(high = "red",
                      low = "blue",
                      breaks = c( -0.03, 0.00, 0.03, 0.06, 0.09),
                      labels = c( '-0.03', '0.00', '0.03', '0.06', '0.09'),
                      na.value = NA,
                      oob = scales::squish,
                      limits = c( -0.03, 0.09)) +
  geom_sf( data = usa.sub.sf,
           inherit.aes = FALSE,
           fill = NA, color = 'black', size = 0.3) +
  labs( fill = expression(paste( 'Change in Coal ', PM["2.5"], ', µg ', m^{"-3"}))) +
  scale_x_continuous( expand = c( 0, 0)) +
  scale_y_continuous( expand = c( 0, 0)) +
  theme_bw() +
  theme( axis.text = element_blank(),
         axis.title = element_blank(),
         axis.ticks = element_blank(),
         legend.direction = 'horizontal',
         legend.position = c(0.1,0.2),
         legend.text = element_text( size = 10),
         legend.title = element_text( size = 14),
         panel.background = element_rect( fill = 'white'),
         panel.grid = element_blank(),
         panel.border = element_blank()) +
  guides(fill = guide_colorbar( label.position = "top",
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= -1.5,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1))


names(bind)[names(bind) == 'actual_coal_pm25'] <- 'actual.coal.pm2.5'
names(bind)[names(bind) == 'counterfactual_coal_pm25'] <- 'counterfactual.coal.pm2.5'
names(bind)[names(bind) == 'diff'] <- 'change.pm2.5'

bind$group <- "Long-term"
bind.lockdown$group <- "Lockdown"

combined <- full_join(bind,bind.lockdown)

combined %>% ggplot(aes( x = x, y = y, fill = change.pm2.5))+
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       low = "blue",
                       breaks = c( -0.03, 0.00, 0.03, 0.06, 0.09),
                       labels = c( '-0.03', '0.00', '0.03', '0.06', '0.09'),
                       na.value = NA,
                       oob = scales::squish,
                       limits = c( -0.03, 0.09)) +
  geom_sf( data = usa.sub.sf,
           inherit.aes = FALSE,
           fill = NA, color = 'black', size = 0.3) +
  labs( fill = expression(paste( 'Change in Coal ', PM["2.5"], ', µg ', m^{"-3"}))) +
  scale_x_continuous( expand = c( 0, 0)) +
  scale_y_continuous( expand = c( 0, 0)) +
  theme_bw() +
  theme( axis.text = element_blank(),
         axis.title = element_blank(),
         axis.ticks = element_blank(),
         legend.direction = 'horizontal',
         legend.position = c(0.5,-0.1),
         legend.text = element_text( size = 10),
         legend.title = element_text( size = 14),
         panel.background = element_rect( fill = 'white'),
         panel.grid = element_blank(),
         panel.border = element_blank()) +
  guides(fill = guide_colorbar( label.position = "top",
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= -1.5,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1)) + facet_wrap(~group)



ggsave("coal_exposure_2020.png", path = "./plots/", width=10, height=5, units="in")

combined %>% ggplot(aes( x = x, y = y, fill = pct))+
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       low = "blue",
                       breaks = c( -100, -50, 0, 50, 100),
                       labels = c( '-100', '-50', '0', '50', '100'),
                       na.value = NA,
                       oob = scales::squish,
                       limits = c( -100, 100)) +
  geom_sf( data = usa.sub.sf,
           inherit.aes = FALSE,
           fill = NA, color = 'black', size = 0.3) +
  labs( fill = expression(paste( '% Change in Coal ', PM["2.5"]))) +
  scale_x_continuous( expand = c( 0, 0)) +
  scale_y_continuous( expand = c( 0, 0)) +
  theme_bw() +
  theme( axis.text = element_blank(),
         axis.title = element_blank(),
         axis.ticks = element_blank(),
         legend.direction = 'horizontal',
         legend.position = c(0.5,-0.1),
         legend.text = element_text( size = 10),
         legend.title = element_text( size = 14),
         panel.background = element_rect( fill = 'white'),
         panel.grid = element_blank(),
         panel.border = element_blank()) +
  guides(fill = guide_colorbar( label.position = "top",
                                title.position = "left", title.vjust = 0, title.hjust = -15,
                                label.vjust= -1.5,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 12,
                                barheight = 1)) + facet_wrap(~group)


ggsave("coal_exposure_2020_pct.png", path = "./plots/", width=10, height=5, units="in")

summary(bind.lockdown)

summary(bind)

#ttest
t.test(bind.lockdown$actual_coal_pm25, bind.lockdown$counterfactual_coal_pm25, var.equal=T)



#TTT vs sulfate concentration
