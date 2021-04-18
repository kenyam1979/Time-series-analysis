library(urca)
library(tseries)

## Generate random walk
set.seed(123)
z <- rep(0, 10000)
for (i in 2:10000) z[i] <- z[i-1] + rnorm(1)
ts.plot(z)


### VAR
p <- q <- r <- rep(0, 10000)
p <- 0.3*z + rnorm(10000)
q <- 0.6*z + rnorm(10000)
r <- 0.2*z + rnorm(10000)


## Johansen procedure
jotest=ca.jo(data.frame(p,q,r), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest) 

s = 1.000*p + 0.7064995*q -3.6156063*r
ts.plot(s)

adf.test(s)

## Reference
## https://www.quantstart.com/articles/Johansen-Test-for-Cointegrating-Time-Series-Analysis-in-R/

