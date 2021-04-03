library(tidyverse)
library(tsibble)
library(forecast)

## 5.1 
## 省略

## 5.2
## 株価:          場合2　短期的にはある値を上下するものである(中期的には成長を前提として場合3)
## 為替レート:    場合2　おおよそ105円前後の範囲で動く
## GDP:           場合3　基本的には成長トレンドがある
## CPI:           場合2　基準年を1として上下するものであるため
## コールレート:  場合2　ある値のあたりを上下するものであるため
## 失業率:        場合2　ある値のあたりを上下するものであるため
## 消費:          場合3　基本的には成長トレンドがあるため


## 5.3
## 5%水準とすると、失業率の帰無仮説は棄却できず、差分系列の帰無仮説は棄却され、
## 単位根過程である
## 差分系列において、次数やパラメタを推定していく、必要に応じて予測

## 5.4
## ?


## 5.5
library(readxl)
economicdata <- read_excel("Data/economicdata.xls")
economicdata <- economicdata %>% rename(date=...1) %>% mutate(date=yearmonth(date)) #日付型にも関わらずインターバルが月次のため年月型に変換
economicdata <- as_tsibble(economicdata)

economicdata <- economicdata %>% 
  mutate(topix_log=log(topix), 
         exrate_log=log(exrate), 
         indprod_log=log(indprod), 
         cpi_log=log(cpi)) %>%
  mutate(topix_log_d=topix_log-lag(topix_log), 
         exrate_log_d=exrate_log-lag(exrate_log), 
         indprod_log_d=indprod_log-lag(indprod_log), 
         cpi_log_d=cpi_log-lag(cpi_log)) %>%
  filter(!is.na(topix_log_d))

economicdata %>% 
  dplyr::select(date, topix_log, exrate_log, indprod_log, cpi_log) %>%
  pivot_longer(cols=-date) %>%
  ggplot(aes(x=date, y=value)) + 
  geom_line() + 
  facet_wrap(~name, scales='free')

economicdata %>% 
  dplyr::select(date, topix_log_d, exrate_log_d, indprod_log_d, cpi_log_d) %>%
  pivot_longer(cols=-date) %>%
  ggplot(aes(x=date, y=value)) + 
  geom_line() + 
  facet_wrap(~name, scales='free')


### 1)
library(tseries)
adf.test(economicdata$topix_log)
pp.test(economicdata$topix_log)

adf.test(economicdata$exrate_log)
pp.test(economicdata$exrate_log)

adf.test(economicdata$indprod_log)
pp.test(economicdata$indprod_log)

adf.test(economicdata$cpi_log)
pp.test(economicdata$cpi_log)

### 2)
adf.test(economicdata$topix_log_d)
pp.test(economicdata$topix_log_d)

adf.test(economicdata$exrate_log_d)
pp.test(economicdata$exrate_log_d)

adf.test(economicdata$indprod_log_d)
pp.test(economicdata$indprod_log_d)

r<-adf.test(economicdata$cpi_log_d)
pp.test(economicdata$cpi_log_d)
