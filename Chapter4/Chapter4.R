library(readxl)
library(tidyverse)
library(tsibble)
library(vars)


## 4.1
## 省略

## 4.2
## 省略

## 4.3
## 省略

## 4.4
## 省略


## 4.5 
msci_day <- read_excel("Data/msci_day.xls")
msci_day <- tsibble(msci_day, regular=FALSE)

msci_day %>% ggplot() + 
  geom_line(aes(x=Date, y=jp)) +
  geom_line(aes(x=Date, y=uk)) +
  geom_line(aes(x=Date, y=us))

data <- msci_day %>% 
  mutate(jp_log=log(jp), 
         uk_log=log(uk), 
         us_log=log(us)) %>%
  mutate(jp_log_d=jp_log - lag(jp_log),
         uk_log_d=uk_log - lag(uk_log),
         us_log_d=us_log - lag(us_log)) %>%
  dplyr::select(Date, jp_log_d, uk_log_d, us_log_d) %>%
  filter(!is.na(jp_log_d))


model <- VAR(data[,-1], lag.max=10, ic='AIC')
summary(model)

### Granger因果の検定
causality(model, cause=c('jp_log_d'))
causality(model, cause=c('uk_log_d'))
causality(model, cause=c('us_log_d'))

### インパルス応答関数
irf <- irf(model, ci=0.95)
plot(irf)

### 分散分解分析
fevd <- fevd(model)
plot(fevd)




## 4.6
### 1) 対数差分
data2 <- msci_day %>% 
  mutate(jp_log=log(jp), 
         fr_log=log(fr), 
         ca_log=log(ca)) %>%
  mutate(jp_log_d=jp_log - lag(jp_log),
         fr_log_d=fr_log - lag(fr_log),
         ca_log_d=ca_log - lag(ca_log)) %>%
  dplyr::select(Date, jp_log_d, fr_log_d, ca_log_d) %>%
  filter(!is.na(jp_log_d))


### 2) 並べ替えの前提
### 日本がより外生性が高い前提(カナダの市場に時差で影響を受けない)
### フランスは日本が終わると始まる
### カナダはフランスが終わると始まる

### 3) 週次、月次
### 時差による仮定はなく、他の連関性を考える
### 例えばカナダとフランスは近いので影響し合う。そうすると日本がやはり外生性が高い


### 4) VARモデル
VARselect(data2[,-1], lag.max=10)  # 次数2となる
model2 <- VAR(data2[,-1], p=2)
summary(model2)

### 5) グレンジャー検定
causality(model2, cause=c('jp_log_d'))
causality(model2, cause=c('ca_log_d'))

### p値は0.05より小さく帰無仮説は棄却
### 2の前提の通り日本の影響が小さい

### 6) インパルス応答関数
irf2 <- irf(model2)
plot(irf2)
### 反応があって１日で影響が消える
### フランスは日本に1日遅れで影響
### カナダは日本とフランスに1日遅れで影響

### 7) 分散分解分析
fevd2 <- fevd(model2)
plot(fevd2)
### 日本では20％弱がフランス数％がカナダで説明できる
### フランスはほぼ他市場の影響なく、数％日本で説明できる
### カナダは20％強をフランスで説明できる

