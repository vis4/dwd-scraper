parse <- function(station) {
  print(station)
  fn.historical <- paste0('data/', station, '-historical.txt')
  fn.recent <- paste0('data/', station, '-recent.txt')
  if (file.exists(fn.historical)) {
    data <- read_delim(fn.historical, delim = ';', trim_ws = T, col_types = cols())
  } else if (file.exists(fn.recent)) {
    data <- read_delim(fn.recent, delim = ';', trim_ws = T, col_types = cols())
  } else {
    return(F);
  }

  if (file.exists(fn.historical) && file.exists(fn.recent)) {
    data <- data %>% bind_rows(read_delim(fn.recent, delim = ';', trim_ws = T, col_types = cols()))
  }

  if (file.exists(fn.recent)) {
    data <- data %>% bind_rows(read_delim(fn.recent, delim = ';', trim_ws = T, col_types = cols()))
  }
  both <- data %>%
    filter(TMK > -999 & TNK > -999 & TXK > -999) %>%
    mutate(date=as.Date(as.character(MESS_DATUM), '%Y%m%d')) %>%
    group_by(date) %>%
    summarise(TMK=first(TMK), TNK=first(TNK), TXK=first(TXK), RSK=first(RSK), SDK=first(SDK)) %>%
    arrange(desc(date)) %>%
    select(date, TMK, TNK, TXK, RSK, SDK)

  # fix start and end date in stationen list (<<- assigns to global var)
  stationen$to[stationen$id == station] <<- both$date[1]
  stationen$from[stationen$id == station] <<- both$date[nrow(both)]

  both %>% write_csv(paste0('out/stations/', station, '.csv'))
  return(T)
}
