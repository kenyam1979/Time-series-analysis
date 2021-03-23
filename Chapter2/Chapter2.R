library(tidyverse)
library(readxl)
library(tsibble)
library(forecast)

## 2.1
## ??

## 2.2
## a) MA    定常    反転不可
## b) MA    定常    反転可
## c) MA    定常    (省略)
## d) AR    定常    
## e) AR    非定常 
## f) ARMA  定常    反転可


## 2.3
## (省略)


## 2.4
## (省略)


## 2.5
economicdata <- read_excel("Data/economicdata.xls", 
                           col_types = c("date", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric"))
economicdata <- economicdata %>% rename(date=...1) %>% mutate(date=yearmonth(date)) #日付型にも関わらずインターバルが月次のため年月型に変換
economicdata <- as_tsibble(economicdata)
economicdata <- economicdata %>% 
  mutate(topix_log=log(topix), exrate_log=log(exrate), indprod_log=log(indprod)) %>%
  mutate(topix_d_log=topix_log-lag(topix_log), exrate_d_log=exrate_log-lag(exrate_log), indprod_d_log=indprod_log-lag(indprod_log))


### 1) 
#### 図2.5の確認
Acf(economicdata$indprod_d_log)
Pacf(economicdata$indprod_d_log)

model_ar4 <- arima(economicdata$indprod_d_log, order=c(4,0,0), method='ML')  #ARIMAのモデルフィッティング
Acf(model1$residuals)

model_arma12 <- arima(economicdata$indprod_d_log, order=c(1,0,2), method='ML')
Acf(model2$residuals)

#### 表2.3の確認
model_ma3 <- arima(economicdata$indprod_d_log, order=c(0,0,3), method='ML')
model_arma11 <- arima(economicdata$indprod_d_log, order=c(1,0,1), method='ML')
model_arma21 <- arima(economicdata$indprod_d_log, order=c(2,0,1), method='ML')
model_arma22 <- arima(economicdata$indprod_d_log, order=c(2,0,2), method='ML')

ic <- tibble(
  aic=c(
    AIC(model_ar4),
    AIC(model_ma3),
    AIC(model_arma11),
    AIC(model_arma21),
    AIC(model_arma12),
    AIC(model_arma22)),
  bic=c(
    BIC(model_ar4),
    BIC(model_ma3),
    BIC(model_arma11),
    BIC(model_arma21),
    BIC(model_arma12),
    BIC(model_arma22)))

### 2)
Box.test(model_ar4$residuals, lag=10, type='Ljung-Box')
Box.test(model_arma12$residuals, lag=10, type='Ljung-Box')





## 2.6
arma <- read_excel("Data/arma.xls")

### 1) ACFとPACF描画
Acf(arma$y1)
Pacf(arma$y1)

### 2) 可能性のあるモデル
### AR(3)
### MA(5)
### ARMA(2, 5)
model_ar3 <- arima(arma$y1, order=c(3,0,0), method='ML')
model_ma5 <- arima(arma$y1, order=c(0,0,5), method='ML')
model_arma25 <- arima(arma$y1, order=c(2,0,5), method='ML')

### 3) AICとSICの比較
ic <- tibble(
  aic=c(
    AIC(model_ar3),
    AIC(model_ma5),
    AIC(model_arma25)),
  bic=c(
    BIC(model_ar3),
    BIC(model_ma5),
    BIC(model_arma25)))
ic

### 検定
Box.test(model_ar3$residuals, lag=10, type='Ljung-Box')
Box.test(model_ma5$residuals, lag=10, type='Ljung-Box')
Box.test(model_arma25$residuals, lag=10, type='Ljung-Box')

### 補足:auto.arimaでの自動選定
auto.arima(arma$y1, trace=TRUE)

### 5) y2とy3
Acf(arma$y2)
Pacf(arma$y2)
model_auto <- auto.arima(arma$y2, trace=TRUE)
Acf(model_auto$residuals)
Box.test(model_auto$residuals, lag=10, type='Ljung-Box')

Acf(arma$y3)
Pacf(arma$y3)
model_auto <- auto.arima(arma$y3, trace=TRUE)
Acf(model_auto$residuals)
Box.test(model_auto$residuals, lag=10, type='Ljung-Box')


rm(list=ls(all.names=TRUE))
