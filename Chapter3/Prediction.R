library(tidyverse)
library(tsibble)
library(forecast)

## 予測に関する実験
set.seed(123)

## AR (1)
y <- arima.sim(
  n=500,
  model=list(order=c(1,0,0), ar=c(0.5)),
  sd=sqrt(4))
plot(y)
model <- arima(y, order=c(1,0,0), method='ML')
model
f <- forecast(model, level=c(95), h=50)
f
plot(f)  + abline(h=mean(y))
plot(f$upper - f$lower, type='o') #区間推定が収束していく 



## MA (2)
y <- arima.sim(
  n=500,
  model=list(order=c(0,0,2), ma=c(0.5, 0.4)),
  sd=sqrt(4))
plot(y)
model <- arima(y, order=c(0,0,2), method='ML')
f <- forecast(model, level=c(95), h=30)
f
plot(f)  + abline(h=mean(y))
plot(f$upper - f$lower, type='o') # 区間推定が2次以降一定になる



## ARMA (1, 1)
y <- arima.sim(
  n=500,
  model=list(order=c(1,0,1), ar=c(0.5), ma=c(0.5)),
  sd=sqrt(4))
plot(y)
model <- arima(y, order=c(1,0,1), method='ML')
f <- forecast(model, level=c(95), h=30)
f
plot(f)  + abline(h=mean(y))
plot(f$upper - f$lower, type='o')


