rm(list = ls())

library( fst)
library( data.table)
library( magrittr)
library( raster)
library( sf)
library( ggplot2)

setwd ("/Volumes/GoogleDrive/My Drive/R/long-term-coal/coal_unit_PM25")

ct_source_impacts_dir <- '/Volumes/GoogleDrive/My Drive/R/causal-study-COVID-beyond/data/counterfactual_disperseR/disperseR/hyads_to_PM'

#coordinate reference system projection string for spatial data
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"

# get the names of the gridded HyADS output files from the data directory
grid.files.yr <- list.files( ct_source_impacts_dir,
                             pattern = 'grids_pm25_total_\\d{4}\\.fst',
                             full.names = TRUE)

x <- read.fst("/Volumes/GoogleDrive/My Drive/R/causal-study-COVID-beyond/data/counterfactual_disperseR/disperseR/hyads_to_PM/grids_pm25_total_2020.fst")
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
grid.files.unit.yr <- list.files( ct_source_impacts_dir,
                                  pattern = 'grids_pm25_byunit_\\d{4}\\.fst',
                                  full.names = TRUE)

# read select files
grid.unit.dat <- lapply( grid.files.unit.yr,
                         function( f){
                           print( f)
                           year.f <- gsub( '^.*_|\\.fst', '', f) %>%
                             as( 'integer')
                           
                           in.f <- read.fst( f, as.data.table = T)
                           in.f[, year := year.f]
                           
                           in.f[ is.na( in.f)] <- 0
                           return (in.f)
                         }) %>% rbindlist( fill = T)


# check the dimensions
dim( grid.unit.dat)



# first, use dcast to get year columns
grid.dat.c <- dcast( grid.dat, x + y ~ year, value.var = 'coal_pm25')

# convert to raster
grid.dat.r <- rasterFromXYZ( grid.dat.c, crs = p4s)

# plot, which will show only ~16 of the years contained in grid.dat.r
plot( grid.dat.r)


# create sf object
grid.dat.sf <- rasterToPolygons( grid.dat.r) %>%
  st_as_sf()

# convert object to data.table and melt across years
grid.dat.sf.dt <- data.table( grid.dat.sf)
grid.dat.sf.m <- melt( grid.dat.sf.dt, 
                       id.vars = 'geometry',
                       variable.name = 'year',
                       value.name = 'coal_pm25')

counterfactual <- grid.dat.sf.m 

summary(counterfactual)
# plot 4 years of data
x <- ggplot( grid.dat.sf.m[year %in% c( 'X2020')]) + 
  geom_sf( aes( fill = coal_pm25, 
                geometry = geometry),
           color = NA) + labs(title="Counterfactual Emissions 2020") 



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
grid.files.unit.yr <- list.files( ac_source_impacts_dir,
                                  pattern = 'grids_pm25_byunit_\\d{4}\\.fst',
                                  full.names = TRUE)

# read select files
grid.unit.dat <- lapply( grid.files.unit.yr,
                         function( f){
                           print( f)
                           year.f <- gsub( '^.*_|\\.fst', '', f) %>%
                             as( 'integer')
                           
                           in.f <- read.fst( f, as.data.table = T)
                           in.f[, year := year.f]
                           
                           in.f[ is.na( in.f)] <- 0
                           return (in.f)
                         }) %>% rbindlist( fill = T)


# check the dimensions
dim( grid.unit.dat)



# first, use dcast to get year columns
grid.dat.c <- dcast( grid.dat, x + y ~ year, value.var = 'coal_pm25')

# convert to raster
grid.dat.r <- rasterFromXYZ( grid.dat.c, crs = p4s)

# plot, which will show only ~16 of the years contained in grid.dat.r
plot( grid.dat.r)


# create sf object
grid.dat.sf <- rasterToPolygons( grid.dat.r) %>%
  st_as_sf()

# convert object to data.table and melt across years
grid.dat.sf.dt <- data.table( grid.dat.sf)
grid.dat.sf.m <- melt( grid.dat.sf.dt, 
                       id.vars = 'geometry',
                       variable.name = 'year',
                       value.name = 'coal_pm25')

actual <- grid.dat.sf.m

summary(actual)

# plot 4 years of data
y <- ggplot( grid.dat.sf.m[year %in% c( 'X2020')]) + 
  geom_sf( aes( fill = coal_pm25, 
                geometry = geometry),
           color = NA) + labs(title="Actual Emissions 2020")

library(ggpubr)
ggarrange (y, x, ncol=2, nrow=1, common.legend = TRUE)
