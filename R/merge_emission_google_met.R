
rm(list = ls())

library(fst)
library(data.table)
library(tidyverse)
library(parallel)
#library(sf) #issues loading on cluster
library(viridis)
library(ggplot2)
library(stringi)
library(panelView)
library(lubridate)
library(dslabs)
library(stringi)
library(openair)
library(ggmap)


# setwd ("/Volumes/GoogleDrive/My Drive/R/causal-study")



setwd ("/projects/HAQ_LAB/mrasel/R/causal-study-COVID-beyond")


#daily emission data and meteorological data read

ampd_daily <- read.fst ("data/ampd_daily_emission.fst")

met <- read.fst ("data/met_data.fst")


