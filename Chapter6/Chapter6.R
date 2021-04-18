library(tidyverse)
library(tsibble)
library(forecast)

## 6.1
### 1) Engle-Granger検定 (回帰して残差を単位根検定)
### 2) 共和分出ない場合は「見せかけの回帰」
### 3) ラグ変数を入れる、差分系列で回帰をする

## 6.2
## 省略

## 6.3
## 省略


## 6.4
library(readxl)
msci_day <- read_excel("Data/msci_day.xls")
msci_day <- tsibble(msci_day, regular=FALSE)
log_jp <- msci_day %>% mutate(log_jp = log(jp)) %>% pull(log_jp)

### 1) ホワイトノイズ生成
wn <- rnorm(1391, mean=0, sd=1)


### 2) ホワイトノイズとの回帰
df <- bind_cols(log_jp=log_jp, wn=wn)
m <- lm(log_jp~wn, df)
summary(m) # p値大きく相関がない


### 3) ランダムウォーク生成
rw <- cumsum(wn)


### 4) ランダムウォークとの回帰
df <- bind_cols(log_jp=log_jp, rw=rw)
m <- lm(log_jp~rw, df)
summary(m) # 見せかけの回帰が生じる




## 6.5
ppp <- read_excel("Data/ppp.xls")

### 1) 対数系列生成
ppp <- ppp %>% mutate(lcpijp=log(cpijp), lcpius=log(cpius), lexjp=log(exjp))

### 2) ADF検定
library(tseries)
adf.test(ppp$lcpijp) # 定常?
adf.test(ppp$lcpius) # 単位根
adf.test(ppp$lexjp) # 単位根

### 3) 対数実質為替レート
ppp <- ppp %>% mutate(lrexjp=lcpijp - lcpius - lexjp)

### 4) lrexjpが示唆するもの
#### PPP仮説が成立していれば定常過程となっているはず

### 5) ADF検定
adf.test(ppp$lrexjp) # 単位根

### 6) VECMでの検定
library(urca)
df <- ppp %>% select(lcpijp, lcpius, lexjp)
m <- ca.jo(df, K=6)
summary(m)
#### r=0 (h=0)ということになっている
#### PPP仮説がなりたってないということ？


### 7) 
#### カナダとアメリカ
ppp <- ppp %>% mutate(lcpica=log(cpica), lexca=log(exca))
ppp <- ppp %>% mutate(lrexca=lcpica - lcpius - lexca)
adf.test(ppp$lcpica)
adf.test(ppp$lexca)
adf.test(ppp$lrexca) # 単位根
df <- ppp %>% select(lcpica, lcpius, lexca)
m <- ca.jo(df, K=6)
summary(m)

#### イギリスとアメリカ
ppp <- ppp %>% mutate(lcpiuk=log(cpiuk), lexuk=log(exuk))
ppp <- ppp %>% mutate(lrexuk=lcpiuk - lcpius - lexuk)
adf.test(ppp$lcpiuk)
adf.test(ppp$lexuk)
adf.test(ppp$lrexuk) # 単位根
df <- ppp %>% select(lcpiuk, lcpius, lexuk)
m <- ca.jo(df, K=6)
summary(m)

