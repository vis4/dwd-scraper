download <- function(type) {
  url.index <- paste0('https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/', type, '/')
  index <- read_html(url.index)

  urls <- index %>%
    html_nodes('a') %>%
    html_attr('href') %>%
    tibble(url = .) %>%
    extract(url, c('station'),  'KL_([0-9]{5})_', remove=F) %>%
    filter(station %in% stationen$id) %>%
    mutate(url = paste0(url.index, url))

  mapply(function(station, url) {
    path.archive <- paste0('zip/', station, '-', type, '.zip')
    download.file(url, dest=path.archive)
    df <- unzip(path.archive, list=T) %>%
      filter(grepl('produkt_', Name))
    unzip(path.archive, files=df$Name, exdir = 'zip')
    file.rename(paste0('zip/', df$Name), paste0('data/', station,'-',type,'.txt'))
  }, station=urls$station, url=urls$url)
}