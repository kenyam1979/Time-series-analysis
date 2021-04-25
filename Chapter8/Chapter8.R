library(tidyverse)
library(tsibble)
library(readxl)
library(forecast)
library(tseries)


## 8.1
## (省略)

## 8.2
## (省略)

## 8.3
## (省略)


## 8.4
library(tsDyn)
ppp <- read_excel("Data/ppp.xls")
ppp <- ppp %>% mutate(lcpijp=log(cpijp), lcpius=log(cpius), lexjp=log(exjp))
ppp <- ppp %>% mutate(lrexjp=lcpijp - lcpius - lexjp)
ts.plot(ppp$lrexjp)

### 1) SEATR
m <- setar(ppp$lrexjp, mL=1, mH=1, nthresh=1)
summary(m)
plot(m)
ts.plot(regime(m))
#### 各レジーム内ではAR(1)で定常過程になっているのでPPP仮設を支持する


### (参考) LSTAR
m <- lstar(ppp$lrexjp, m=1)
summary(m)
plot(m)
ts.plot(regime(m))


### 2) ESTAR
### (省略)  tsDynにESTARの実装がない

### 3) 
### (省略)


## 8.5
## (省略)




## 参考
## https://tjo.hatenablog.com/entry/2013/08/22/204451
