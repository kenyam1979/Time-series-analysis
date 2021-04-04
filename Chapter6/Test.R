library(forecast)

## 二つのランダムウォーク
rw1 <- cumsum(rnorm(n=500))
rw2 <- cumsum(rnorm(n=500))
df <- bind_cols(rw1=rw1, rw2=rw2)
ts.plot(df)


## 見せかけの回帰
m <- lm(rw2~rw1, df)
summary(m)

df %>%
  ggplot(aes(x=rw1, y=rw2)) +
  geom_point() +
  geom_smooth(method='lm')


## 変数の単位根検定
adf.test(rw1)
adf.test(rw2)

## 回帰残差の単位根検定
resid <- m$residuals
resid_d <- na.omit(resid - lag(resid))

adf.test(resid)  # 残差は単位根過程
adf.test(resid_d) # 残差の差分系列は単位根ではない
