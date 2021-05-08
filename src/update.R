#!/usr/bin/env Rscript

# you need to install the R package "needs" the
# first time you run this.
# install.packages("needs")
# library(needs)
#
needs(readr, rvest, dplyr, tidyr)

args = commandArgs(trailingOnly=TRUE)

dir.create('zip', showWarnings = F)
dir.create('data', showWarnings = F)
dir.create('out', showWarnings = F)
dir.create('out/stations', showWarnings = F)

source('src/download.R')
source('src/parse.R')
source('src/forecast.R')

url.stationen <- 'https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/KL_Tageswerte_Beschreibung_Stationen.txt'

stationen <- read_fwf(url.stationen,
                      skip = 3,
                      col_positions = fwf_widths(
                        c(5,9,9,15,12,10,42,22),
                        c('id','from','to','altitude','lat','lon','name','state')),
                      trim_ws = T,
                      locale = locale(encoding = 'latin1')) %>%
  mutate(from=as.Date(as.character(from), '%Y%m%d'), to=as.Date(as.character(to), '%Y%m%d')) %>% 
  filter(to > Sys.Date()-3)

if ('--historical' %in% args) {
	download('historical')
}

if (!('--no-download' %in% args)) {
  download('recent')
  
  sapply(stationen$id, parse)
}


# get forecast
sapply(stationen$id, add_forecast_brightsky)

stationen %>% write_csv('out/stations.csv')

f <- file('out/last-update.txt')
writeLines(as.character(Sys.time()), f)
close(f)