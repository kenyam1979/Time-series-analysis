library(tidyverse)
library(tsibble)
library(readxl)
library(forecast)


## 7.1 
### 図7.1の確認
msci_day <- read_excel("Data/msci_day.xls")
msci_day <- tsibble(msci_day, regular=FALSE)
msci_day <- msci_day %>% 
  mutate(jp_log=log(jp)) %>%
  mutate(jp_log_d=jp_log-dplyr::lag(jp_log))

jp_log_d <- na.omit(msci_day$jp_log_d)

ts.plot(jp_log_d)
mu <- mean(jp_log_d)
u <- jp_log_d - mu

Acf(u, lag.max=20)
Acf(u*u, lag.max=20)

### 図7.2の確認
library(fGarch)
spec <- garchSpec(model=list(alpha=0, beta=0)) # alpha=0
ts <- garchSim(spec=spec, n=1000)
ts.plot(ts)

spec <- garchSpec(model=list(alpha=0.5, beta=0)) # alpha=0.5
ts <- garchSim(spec=spec, n=1000)
ts.plot(ts)

spec <- garchSpec(model=list(alpha=0.9, beta=0)) # alpha=0.9
ts <- garchSim(spec=spec, n=1000)
ts.plot(ts)



## 7.2 
### (省略)

## 7.3
### (省略)



## 7.4
### 1) アメリカ株式収益率のコレログラム
msci_day <- msci_day %>% 
  mutate(us_log=log(us)) %>% 
  mutate(us_log_d=us_log-dplyr::lag(us_log))
us_log_d <- na.omit(msci_day$us_log_d)

ts.plot(us_log_d)
mu <- mean(jp_log_d)
u <- us_log_d - mu
Acf(u, lag.max=20)

### 2) u^2のコレログラム
Acf(u*u, lag.max=20)


### 3) AR(1) + GARCH((1,1)
library(fGarch)
m <- garchFit(formula=~arma(1,0) + garch(1,1),
              data=us_log_d,
              cond.dist='std')
summary(m)
plot(m)

library(rugarch)
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
  mean.model=list(armaOrder=c(1,0), include.mean=TRUE),
  distribution.model = "std")
m <- ugarchfit(spec=spec, 
               data=us_log_d)
m
plot(m)


### 4) AR(1) + GJR-GARCH((1,1)
library(rugarch)
spec <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)), 
  mean.model=list(armaOrder=c(1,0), include.mean=TRUE),
  distribution.model = "std")
m <- ugarchfit(spec=spec, 
               data=us_log_d)
m
plot(m)

### 参考: https://www1.doshisha.ac.jp/~mjin/R/Chap_35/35.html
### 参考: https://github.com/logics-of-blue/book-tsa-ssm-foundation/blob/master/book-data/3-3-ARCH%E3%83%BBGARCH%E3%83%A2%E3%83%87%E3%83%AB%E3%81%A8%E3%81%9D%E3%81%AE%E5%91%A8%E8%BE%BA.R

### 5)
#### プロット残差のAcfを確認

### 6) AR(1) + E-GARCH(1,1)
spec <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1)), 
  mean.model=list(armaOrder=c(1,0), include.mean=TRUE),
  distribution.model = "std")
m <- ugarchfit(spec=spec, 
               data=us_log_d)
m
plot(m)

### 7)
#### プロットの残差のAcfを確認

### 8)
#### (省略)



## 7.5
### (省略)
