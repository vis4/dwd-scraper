# needs('remotes')
# remotes::install_github("retostauffer/Rmosmix")
needs(aiRthermo, ggplot2, tidyverse, readxl)
library("mosmix")

# get list of stations and their dwd ids
station_id_map <- read_excel('ha_messnetz.xls') %>% 
  select(dwdid=ID, name=`Stations-Name`, id=`WMO-Kennung`) %>% 
  bind_rows(read_excel('na_messnetz.xlsx') %>% 
              select(id=STATIONSKENNUNG, name=STATIONSNAME, dwdid=STATIONS_ID))


add_forecast <- function(dwd_station_id) {
  print(dwd_station_id)
  stationen$forecast[stationen$id == dwd_station_id] <<- F
  datafile <- paste0('out/stations/', dwd_station_id, '.csv')
  if (file.exists(datafile)) {
    data <- read_csv(datafile) %>% mutate(forecast=F)
    if (nrow(data) < 1) {
      return();
    }
    last_date <- as.Date(max(levels(data$date)))
    s <- station_id_map %>% filter(dwdid == as.numeric(dwd_station_id))
    if (nrow(s) == 1) {
      wmo <- s$id[1]
      print(paste0('  found wmo station ', wmo))
      while (last_date < Sys.Date()) {
        print(paste0('  loading forecast from ',last_date))
        fc <- tryCatch({
          mosmix_forecast(wmo, format(last_date, '%Y%m%d'))
        }, error=function(cond){
          F
        })
        if (fc != F) {
          all = bind_rows(all, fc)
        }
        last_date <- last_date+1
      }
      forecasts <- all %>% 
        group_by(date) %>% 
        top_n(-1, err) %>% 
        arrange(date) %>% 
        select(-err)
      
      if (nrow(forecasts) > 0) {
        bind_rows(data, forecasts) %>%
          write_csv(paste0('out/stations/', dwd_station_id, '-fc.csv'))  
        stationen$forecast[stationen$id == dwd_station_id] <<- T
      }
    }  
  }
}

mosmix_forecast <- function(station_id, after_date) {
  url <- paste0('https://opendata.dwd.de/weather/local_forecasts/mos/MOSMIX_L/single_stations/', station_id, '/kml/MOSMIX_L_',after_date,'21_',station_id,'.kmz')
  
  kmz   <- tempfile("mosmix_demo_", fileext = ".kmz")
  check <- download.file(url, kmz, quiet=T)
  if ( inherits(check, "try-error") ) stop("Problems downloading the file!")
  kml   <- unzip(kmz)
  
  doc <- XML::xmlParse(kml)
  datetime <- get_datetime(doc)
  meta     <- get_meta_info(doc)
  
  # fcst1 <- get_forecasts(station_id, doc, datetime, meta, as.zoo = T)
  # plot(fcst1)
  
  fcst2 <- get_forecasts(station_id, doc, datetime, meta, as.zoo = F)
  
  out <- fcst2 %>%
    mutate(date=format(datetime, '%Y-%m-%d')) %>%
    filter(date>as.Date(after_date, '%Y%m%d')) %>% 
    group_by(date) %>% 
    summarise(
      TNK=min(K2C(TTT)), 
      TMK=mean(K2C(TTT)), 
      TXK=max(K2C(TTT)), 
      RSK=sum(RR1c),
      SDK=round(sum(SunD1/3600),2), 
      err=mean(E_TTT), 
      forecast=TRUE)
  
  # fcst2 %>% ggplot(aes(x=datetime)) +
  #   geom_ribbon(aes(ymin=K2C(TTT-E_TTT), ymax=K2C(TTT+E_TTT)), fill='red', alpha=0.1) +
  #   geom_line(aes(y=K2C(TTT)), color='red') +
  #   geom_ribbon(aes(ymin=K2C(Td-E_Td), ymax=K2C(Td+E_Td)), fill='green', alpha=0.1) +
  #   geom_line(aes(y=K2C(Td)), color='green')
  # 
  # fcst2 %>% ggplot(aes(x=datetime)) +
  #   geom_col(aes(y=RR1c), color='blue')
  
  out
}

# mosmix_forecast('10382', '20210505')
