# needs('remotes')
# remotes::install_github("retostauffer/Rmosmix")
needs(ggplot2, tidyverse, jsonlite)
## library("mosmix")

# get list of stations and their dwd ids
# station_id_map <- read_excel('ha_messnetz.xls') %>% 
#   select(dwdid=ID, name=`Stations-Name`, id=`WMO-Kennung`) %>% 
#   bind_rows(read_excel('na_messnetz.xlsx') %>% 
#               select(id=STATIONSKENNUNG, name=STATIONSNAME, dwdid=STATIONS_ID))
# 
station_id_map <- read.csv('station-ids.csv')

add_forecast_brightsky <- function(dwd_station_id) {
  datafile <- paste0('out/stations/', dwd_station_id, '.csv')
  print(dwd_station_id)
  if (file.exists(datafile)) {
    data <- read_csv(datafile, col_types = 'Ddddddd')
    if (nrow(data) < 1) {
      return()
    }
    last_date <- as.Date(max(data$date))
    if (is.na(last_date) | Sys.Date() - last_date > 4) return()
    last_date <- last_date+1
    weather <- tibble()
    while (last_date <= Sys.Date()+5) {
      url <- paste0('https://api.brightsky.dev/weather?dwd_station_id=', dwd_station_id, '&date=', last_date)
      df <- tryCatch({ fromJSON(url) }, error=function(cond){ 
        print(paste('error loading', url))
        NULL
      })
      if (is.null(df)) break
      df <- df$weather %>% select(timestamp, temperature, precipitation, sunshine)
      if (nrow(df) > 0) {
        weather <- bind_rows(weather, df)
      }
      last_date <- last_date+1
    }
    if (nrow(weather) == 0) return()
    
    weather <- weather %>% 
      unique() %>% 
      mutate(datetime=as.POSIXct(timestamp, format='%Y-%m-%dT%H:%M:%S', tz='UTC')) %>% 
      select(-timestamp)
    
    # load current weather
    s <- station_id_map %>% filter(dwd_id == as.numeric(dwd_station_id))
    if (nrow(s) == 1) {
      wmo <- s$wmo_id[1]
      current <- tryCatch({read_csv2(paste0('https://opendata.dwd.de/weather/weather_reports/poi/',
                                          str_replace(sprintf('%-5s', wmo), ' ', '_'), '-BEOB.csv'),
                                   skip = 2,
                                   na = c('', '---'),
                                   col_types = 'ccddddddddddddddddddddddddddddddddddddddddd',
                                   locale = locale(decimal_mark =',',
                                                   grouping_mark = '.')) %>% 
        select(date=Datum,
               time=`Uhrzeit (UTC)`,
               temperature=`Temperatur (2m)`,
               precipitation=`Niederschlag (letzte Stunde)`,
               sunshine=`Sonnenscheindauer (letzte Stunde)`) %>%
        mutate(datetime=as.POSIXct(paste(date, time), format='%d.%m.%y %H:%M')) %>% 
        select(datetime, temperature, precipitation, sunshine) %>% 
        filter(!is.na(temperature) & !is.na(precipitation))}, error=function(cond) {
          tibble()
        })
      f <- paste0('data/', dwd_station_id, '-current.csv')
      current_a <- tibble()
      if (file.exists(f)) {
        current_a <- read_csv(f, col_types = 'Dddd')
      }
      current <- bind_rows(current_a, current) %>% 
        unique()
      
      if (nrow(current) > 0) {
        # keep data for next time
        current %>%
          select(datetime, temperature, precipitation, sunshine) %>% 
          filter(as.Date(datetime) > Sys.Date()-14) %>% 
          arrange(desc(datetime)) %>% 
          write_csv(f)
        
        na_weather <- weather %>% filter(is.na(temperature))
        
        if (nrow(na_weather) > 0) {
          repair <- na_weather %>% 
            select(datetime) %>% 
            left_join(current, by=c("datetime"="datetime")) %>% 
            unique()
          
          if (nrow(repair) > 0) {
            weather <- weather %>%
              filter(!is.na(temperature)) %>% 
              bind_rows(repair)
          }
        }
      }
    }
    
    forecasts <- weather %>% 
      mutate(date=as.Date(datetime, tz='UTC')) %>%
      group_by(date) %>%
      summarise(c=n(),
                TNK=round(min(temperature), 2), 
                TMK=round(mean(temperature), 2),
                TXK=round(max(temperature), 2),
                RSK=round(sum(precipitation), 2),
                SDK=round(sum(sunshine/60),2)) %>% 
      filter(c == 24) %>% 
      select(-c)
    
    if (nrow(forecasts) > 0) {
      bind_rows(data, forecasts) %>%
        arrange(desc(date)) %>% 
        write_csv(paste0('out/stations/', dwd_station_id, '-fc.csv'))  
      stationen$forecast[stationen$id == dwd_station_id] <<- T
    }
  }
}
# 
# add_forecast <- function(dwd_station_id) {
#   print(dwd_station_id)
#   # stationen$forecast[stationen$id == dwd_station_id] <<- F
#   datafile <- paste0('out/stations/', dwd_station_id, '.csv')
#   if (file.exists(datafile)) {
#     data <- read_csv(datafile) %>% mutate(forecast=0)
#     if (nrow(data) < 1) {
#       return()
#     }
#     last_date <- as.Date(max(data$date))
#     if (is.na(last_date) | Sys.Date() - last_date > 4) return()
#     s <- station_id_map %>% filter(dwdid == as.numeric(dwd_station_id))
#     if (nrow(s) == 1) {
#       wmo <- s$id[1]
#       print(paste0('  found wmo station ', wmo))
#       
#       # get current weather
#       current <- tryCatch({
#         read_csv2(paste0('https://opendata.dwd.de/weather/weather_reports/poi/',
#                                             str_replace(sprintf('%-5s', wmo), ' ', '_'), '-BEOB.csv'),
#                                      skip = 2,
#                                      na = c('', '---'),
#                                      locale = locale(decimal_mark =',',
#                                                      grouping_mark = '.')) %>%
#           filter(row_number() > 2) %>% 
#           select(date=Datum,
#                  time=`Uhrzeit (UTC)`,
#                  temp=`Temperatur (2m)`,
#                  precip=`Niederschlag (letzte Stunde)`,
#                  sunshine=`Sonnenscheindauer (letzte Stunde)`) %>% 
#           mutate(datetime=as.POSIXct(paste(date, time), format='%d.%m.%y %H:%M:%S'),
#                  error=0) %>% 
#           select(-date, -time) %>% 
#           filter(!is.na(temp) & !is.na(precip))}, error=function(cond) {
#             tibble()
#           })
#       
#       
#       # complement with
#       all <- tibble()
#       if (nrow(current) > 0) {
#         all <- current  
#       }
#       
#       while (last_date < Sys.Date()) {
#         print(paste0('  loading forecast from ',last_date))
#         fc <- tryCatch({
#           mosmix_forecast(wmo, format(last_date, '%Y%m%d'))
#         }, error=function(cond){
#           NULL
#         })
#         if (!is.null(fc)) {
#           all = bind_rows(all, fc)
#         }
#         last_date <- last_date+1
#       }
#       
#       forecasts.g <- all %>% 
#         group_by(datetime) %>% 
#         top_n(-1, error) %>% 
#         mutate(date=as.Date(datetime)) %>% 
#         group_by(date) %>%
#         filter(n() == 24)
#       
#       if (nrow(forecasts.g) > 0) {
#         forecasts <- forecasts.g %>% 
#           summarise(
#             TNK=round(min(temp), 2), 
#             TMK=round(mean(temp), 2),
#             TXK=round(max(temp), 2),
#             RSK=round(sum(precip), 2),
#             SDK=round(sum(sunshine),2)
#           ) %>% 
#           mutate(forecast=1)
#         
#         if (nrow(forecasts) > 0) {
#           bind_rows(data, forecasts) %>%
#             arrange(desc(date)) %>% 
#             write_csv(paste0('out/stations/', dwd_station_id, '-fc.csv'))  
#           stationen$forecast[stationen$id == dwd_station_id] <<- T
#         }
#       }
#     }  
#   }
# }
# 
# mosmix_forecast <- function(station_id, after_date) {
#   url <- paste0('https://opendata.dwd.de/weather/local_forecasts/mos/MOSMIX_L/single_stations/', station_id, '/kml/MOSMIX_L_',after_date,'21_',station_id,'.kmz')
#   
#   kmz   <- tempfile("mosmix_demo_", fileext = ".kmz")
#   check <- download.file(url, kmz, quiet=T)
#   if ( inherits(check, "try-error") ) stop("Problems downloading the file!")
#   kml   <- unzip(kmz)
#   
#   doc <- XML::xmlParse(kml)
#   datetime <- get_datetime(doc)
#   meta     <- get_meta_info(doc)
#   
#   
#   fcst2 <- mosmix::get_forecasts(station_id, doc, datetime, meta, as.zoo = F)
#   
#   if (!is.null(fcst2)) {
#     out <- fcst2 %>%
#       mutate(date=format(datetime, '%Y-%m-%d')) %>%
#       filter(date>as.Date(after_date, '%Y%m%d')) %>% 
#       transmute(datetime,
#                 temp=K2C(TTT),
#                 precip=RR1c,
#                 sunshine=SunD1/3600,
#                 error=E_TTT)
#       
#       # group_by(date) %>% 
#       # summarise(
#       #   TNK=min(K2C(TTT)), 
#       #   TMK=mean(K2C(TTT)), 
#       #   TXK=max(K2C(TTT)), 
#       #   RSK=sum(RR1c),
#       #   SDK=round(sum(SunD1/3600),2), 
#       #   err=mean(E_TTT)) %>% 
#       # mutate(forecast=TRUE)
#     # fcst2 %>% ggplot(aes(x=datetime)) +
#     #   geom_ribbon(aes(ymin=K2C(TTT-E_TTT), ymax=K2C(TTT+E_TTT)), fill='red', alpha=0.1) +
#     #   geom_line(aes(y=K2C(TTT)), color='red') +
#     #   geom_ribbon(aes(ymin=K2C(Td-E_Td), ymax=K2C(Td+E_Td)), fill='green', alpha=0.1) +
#     #   geom_line(aes(y=K2C(Td)), color='green')
#     # 
#     # fcst2 %>% ggplot(aes(x=datetime)) +
#     #   geom_col(aes(y=RR1c), color='blue')
#     
#     return(out)
#   }
#   return(NULL)
# }

# mosmix_forecast('10382', '20210505')
