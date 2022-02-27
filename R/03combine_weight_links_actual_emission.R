#this script is after getting monthly actual and counterfactual data
#after script disperseR_data_ready.R

# srun -p test --mem 50g -t 0-04:00 -c 1 -N 1 --pty /bin/bash
# srun --partition=normal --mail-type=BEGIN,END,FAIL --mail-user=mrasel@gmu.edu --mem=50G --time=0-6:00 --ntasks=16 --nodes=1 --pty /bin/bash -l
# devtools::install_github( 'lhenneman/disperseR@dev')

# select years to run. I suggest 5 years at a time
years.run <- 2020

library("ncdf4", lib.loc="/home/mrasel/R-orig/x86_64-pc-linux-gnu-library/4.0") #use this only for running on cluster
library(disperseR) # our package

# set directory structure
# disperseR::create_dirs('/projects/HAQ_LAB/mrasel/disperseR')

disperseR::create_dirs('/projects/HAQ_LAB/lhennem/disperseR')


# we want proc_dir and hysp_dir to be in scratch
proc_dir <- '/scratch/mrasel/disperseR/main/process'
dir.create(proc_dir, showWarnings = FALSE)


# read in emissions data
# load( '/projects/HAQ_LAB/mrasel/R/ampd-raw-data-processing/data/units_coal_1997_2021.rda')
# load( '/projects/HAQ_LAB/mrasel/R/ampd-raw-data-processing/data/units_monthly.rda')



# download data
disperseR::get_data(data = "all",
                    start.year = "2020",
                    start.month = "01",
                    end.year = "2020",
                    end.month = "12")

# pick out units to run -- load updated coal dataset
load( '/projects/HAQ_LAB/mrasel/R/causal-study-COVID-beyond/data/units_ct_emission_2020.rda')

units_monthly <- units_ct_emission_2020
#converting monthly to yearly emission
units_ct_emission_2020<- setDT(units_ct_emission_2020)[, .(SO2..tons. = sum(SO2..tons., na.rm=TRUE),
                                                           SO2.tons.ct = sum(SO2.tons.ct, na.rm=TRUE)), 
                                          by = .(FacID, Unit.ID, Latitude, Longitude, year, uID
                                                 )]



# limit to specific unitsi
units_updated <- units_ct_emission_2020 # %>%
  #mutate(uID=gsub("-", ".", ID))
units_updated[, ID := paste(FacID, Unit.ID, sep = "-")]

#adding Heights information
height <- disperseR::units %>%  filter (year==2018) %>% dplyr::select(ID, Height)

ID_units_updated <- as.vector(unique(units_updated$ID))

height <- height %>% filter (ID %in% ID_units_updated)

units_updated <- merge (units_updated, height, by = "ID")

# limit to specific units
unitsrun <- units_updated %>%
  dplyr::filter(year %in% years.run) %>% # only get data for called years
  dplyr::filter( SO2..tons. > 0) %>%
  data.table()


# find unique combos of Latitude, Longitude, and Height
unitslatlonh <- unique( unitsrun[ ,.( Latitude, Longitude, year)] )
unitslatlonh[, unit_run_ref:=1:nrow( unitslatlonh)]
unitsrun_notrim <- merge( unitsrun, unitslatlonh, by = c("Latitude", "Longitude", "year"))
unitsrun_trim <- merge( unitsrun, unitslatlonh, by = c("Latitude", "Longitude", "year"))[ !duplicated( unit_run_ref)]

# define data.table with all emissions events
# units<- aggregate(list (SO2..tons.=units_ct_emission_2020$SO2..tons.
# ), by=list( year=units_ct_emission_2020$year, uID=units_ct_emission_2020$uID,
#             FacID=units_ct_emission_2020$FacID, Unit.ID=units_ct_emission_2020$Unit.ID,
#             Latitude=units_ct_emission_2020$Latitude, Longitude=units_ct_emission_2020$Longitude), FUN=sum)
# 

#not working

# input_refs <- disperseR::define_inputs(units = unitsrun_trim,
#                                        startday = paste0( years.run[1], '-01-01'),
#                                        endday = paste0( years.run[length( years.run)], '-12-31'),
#                                        start.hours =  c(0, 6, 12, 18),
#                                        duration = 24*7)


# # select specific 
# array_num <- as.numeric( Sys.getenv("SLURM_ARRAY_TASK_ID"))
# array_num <- ifelse( array_num == '' | is.na( array_num), 1, array_num)
# set.seed( array_num^2 + 51)
# refs.use <- sample( 1:nrow( input_refs))
# 
# # run disperser
# hysp_raw <- disperseR::run_disperser_parallel(input.refs = input_refs[refs.use],
#                                               pbl.height = pblheight,
#                                               species = 'so2',
#                                               proc_dir = proc_dir,
#                                               mc.cores = 1)

# units<- aggregate(list (SO2..tons.=units_ct_emission_2020$SO2..tons.
# ), by=list( year=units_ct_emission_2020$year, uID=units_ct_emission_2020$uID,
#             FacID=units_ct_emission_2020$FacID, Unit.ID=units_ct_emission_2020$Unit.ID,
#             Latitude=units_ct_emission_2020$Latitude, Longitude=units_ct_emission_2020$Longitude), FUN=sum)
# 
years.run <- 2020

# define yearmons for link to zips/grids/counties
yearmons <- disperseR::get_yearmon(start.year = as( years.run[1], 'character'),
                                    start.month = "01",
                                   end.year = as( years.run[length( years.run)], 'character'),
                                   end.month = "12")

# name the rdata file
rdata_file <- 'disperseR_grids_2019-2020.RData'

# need to:
#  read in combined_gridlinks
#  add in missing units (copy identical ones)
# save updated gridlinks
load( file.path( rdata_dir, rdata_file))
edits <- lapply( yearmons, 
  function( ym){
    print( ym)
    year.ym <- as( substr( ym, 1, 4), 'numeric')
    mont.ym <- substr( ym, 5, 6)
    name.ym <- paste0( "MAP", mont.ym, ".", year.ym)

    map.ym <- get( name.ym)

    unitsrun_use <- unitsrun_notrim[year == year.ym]
    units.map <- names( map.ym)[ !(names( map.ym) %in% c( 'ZIP', 'x', 'y'))]

    changes <- lapply( units.map, 
      function( u){
        u_tmp <- unitsrun_use[ID == u]
        if( nrow( u_tmp) == 0) {
          map.ym[, (u) := NULL]
          return( -1)
        }
        u_use <- unitsrun_use[ID != u & unit_run_ref == u_tmp$unit_run_ref]$ID
        #added unitsrun_trim$ (not sure what's going on)
        if( length( u_use) == 0) return(0)
        map.ym[, (u_use) := lapply( u_use, function(u.) unlist( map.ym[, u, with = F]))]
        return( 1)
      })
    assign( name.ym, map.ym, envir = .GlobalEnv)
    return( changes)
})

# weight by emissions
month.locs.u <- lapply( years.run, function( y) 
  disperseR::calculate_exposure( year.E = y,
                                 year.D = y,
                                 link.to = 'grids',
                                 pollutant = 'SO2..tons.', #changed here
                                 rda_file = "loaded",
                                 exp_dir ="/projects/HAQ_LAB/mrasel/R/causal-study-COVID-beyond/data/disperseR", # changed here
                                 units.mo = units_monthly, #changed here
                                 source.agg = 'unit',
                                 time.agg = 'month',
                                 return.monthly.data = F)
)


month.locs <- lapply( years.run, function( y)
  disperseR::calculate_exposure( year.E = y,
                                 year.D = y,
                                 link.to = 'grids',
                                 pollutant = 'SO2..tons.', #changed here
                                 rda_file = "loaded",
                                 exp_dir="/projects/HAQ_LAB/mrasel/R/causal-study-COVID-beyond/data/disperseR", #changed here
                                 units.mo = units_monthly, #changed here
                                 source.agg = 'total',
                                 time.agg = 'month',
                                 return.monthly.data = T)
)

year.locs <- lapply( years.run, function( y)
  disperseR::calculate_exposure( year.E = y,
                                 year.D = y,
                                 link.to = 'grids',
                                 pollutant = 'SO2..tons.', #changed here
                                 rda_file = "loaded",
                                 exp_dir = "/projects/HAQ_LAB/mrasel/R/causal-study-COVID-beyond/data/disperseR", #changed here
                                 units.mo = units_monthly, #changed here
                                 source.agg = 'total',
                                 time.agg = 'year',
                                 return.monthly.data = F)
)

year.locs.u <- lapply( years.run, function( y)
  disperseR::calculate_exposure( year.E = y,
                                 year.D = y,
                                 link.to = 'grids',
                                 pollutant = 'SO2..tons.', #changed here
                                 rda_file = "loaded",
                                 exp_dir ="/projects/HAQ_LAB/mrasel/R/causal-study-COVID-beyond/data/disperseR", #changed here
                                 units.mo = units_monthly, #changed here
                                 source.agg = 'unit',
                                 time.agg = 'year',
                                 return.monthly.data = F)
)








