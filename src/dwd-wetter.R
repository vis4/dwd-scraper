needs(dplyr, readr, tidyr, ggplot2)

# setwd("~/projects/dwd-wetter")

akt <- read_delim('data/akt/produkt_klima_tag_20180624_20191225_00430.txt', delim = ';', trim_ws = T)
his <- read_delim('data/his/produkt_klima_tag_19630101_20181231_00430.txt', delim=';', trim_ws = T)

both <- akt %>% 
  bind_rows(his) %>% 
  mutate(date=as.Date(as.character(MESS_DATUM), '%Y%m%d')) %>%
  group_by(date) %>% 
  summarise(TMK=first(TMK), TNK=first(TNK), TXK=first(TXK)) %>% 
arrange(desc(date)) %>% 
  select(date, TMK, TNK, TXK)  

both %>% write_csv('app/public/data/430-temp.csv')

t2019 <- both %>% 
  filter(format(date, '%Y') == '1963')

context <- both %>%
  filter(date>as.Date('1969/12/31') & date<as.Date('2000/01/01')) %>% 
  mutate(date=as.Date(format(date, '1963/%m/%d'))) %>%
  group_by(date) %>% 
  summarise(tmin=min(TNK),
            tmin.1=quantile(TNK, c(0.01)),
            tmin.5=quantile(TNK, c(0.05)),
            tmin.10=quantile(TNK, c(0.1)),
            tmin.25=quantile(TNK, c(0.25)),
            tmed=median(TMK),
            tmax.75=quantile(TXK, c(0.75)),
            tmax.90=quantile(TXK, c(0.90)),
            tmax.95=quantile(TXK, c(0.95)),
            tmax.99=quantile(TXK, c(0.99)),
            tmax=max(TXK))

all <- context %>% 
  left_join(t2019)
  
all %>% 
  ggplot(aes(x=date)) +
  geom_ribbon(aes(ymin=tmin, ymax=tmax), fill='#dddddd') +
  geom_ribbon(aes(ymin=tmin.25, ymax=tmax.75), fill='#aaaaaa') +
  geom_linerange(aes(ymin=TNK, ymax=TXK), color='purple') +
  geom_point(aes(y=TMK), size=0.4, color='purple') +
  geom_hline(aes(yintercept=0)) +
  theme_minimal()

format(both$date, '2019/%m/%d')

both %>% ggplot(aes(x=date)) +
  geom_line(aes(y=TMK))
