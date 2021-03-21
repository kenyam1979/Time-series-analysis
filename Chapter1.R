library(tidyverse)
library(tsibble)
library(forecast)

## 1.1
## 相対的にkの差があるのは正負変わらないので、定常過程においては同じ


## 1.2
## E(y_t)は常にmuで時間に依存せず、
## Cov(y_t,y_t-k)は常にkがゼロでsigma2、それ以外で0で時間に依存せず、
## したがって、少なくとも弱定常過程


## 1.3
## iid N(0, 4)
bind_cols(t=1:100, e=rnorm(length(t), 0, 2)) %>%
  ggplot(aes(x=t, y=e)) +
  geom_line()


## 1.4
## ??


## 1.5
economicdata <- read_excel("Data/economicdata.xls", 
                           col_types = c("date", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric"))
economicdata <- economicdata %>% rename(date=...1) %>% mutate(date=yearmonth(date)) #日付型にも関わらずインターバルが月次のため年月型に変換
economicdata <- as_tsibble(economicdata)


## 1) 
economicdata %>%
  pivot_longer(cols=-date) %>%
  ggplot(aes(x=date, y=value)) + 
  geom_line() + 
  facet_wrap(~name, scales='free')
  
  
## 2)
economicdata <- economicdata %>% 
  mutate(topix_log=log(topix), exrate_log=log(exrate), indprod_log=log(indprod)) %>%
  mutate(topix_d_log=topix_log-lag(topix_log), exrate_d_log=exrate_log-lag(exrate_log), indprod_d_log=indprod_log-lag(indprod_log))

## 3)
economicdata %>%
  select(date, topix_d_log, exrate_d_log, indprod_d_log) %>%
  pivot_longer(cols=-date) %>%
  ggplot(aes(x=date, y=value)) + 
  geom_line() + 
  facet_wrap(~name, scales='free')

## 4)
Acf(economicdata$indprod_d_log)
Box.test(economicdata$indprod_d_log, lag=log(25), type='Ljung-Box')


## 5)
Acf(economicdata$exrate_d_log)
Box.test(economicdata$exrate_d_log, lag=log(25), type='Ljung-Box')

Acf(economicdata$topix_d_log)
Box.test(economicdata$topix_d_log, lag=log(25), type='Ljung-Box')
