library(forecast)

## ランダムウォーク
rw1 <- cumsum(rnorm(n=500))
ts.plot(rw1)
m1 <- arima(rw1, order=c(0,1,0))
summary(m1)
f1 <- forecast(m1, h=20)
plot(f1)

m1 <- auto.arima(rw1)
summary(m1)
f1 <- forecast(m1, h=20)
plot(f1)

adf.test(rw1)
adf.test(na.omit(rw1-lag(rw1)))


## ドリフトありのランダムウォーク
rw2 <- cumsum(rnorm(n=500) + 0.1)
ts.plot(rw2)

m2 <- auto.arima(rw2)
summary(m2)
f2 <- forecast(m2, h=20)
plot(f2)

adf.test(rw1)
adf.test(na.omit(rw1-lag(rw1)))
