
library(dCovTS)
data(MortTempPart)
MortTempPart[1:10,] # the first ten observations
## cmort tempr part
## 1 97.85 72.38 72.72
## 2 104.64 67.19 49.60
## 3 94.36 62.94 55.68
## 4 98.05 72.49 55.16
## 5 95.85 74.25 66.02
## 6 95.98 67.88 44.01
## 7 88.63 74.20 47.83
## 8 90.85 74.88 43.60
## 9 92.06 64.17 24.99
## 10 88.75 67.09 40.41
attach(MortTempPart)
temp <- tempr - mean(tempr) # center temperature
temp2 <- temp^2
trend <- time(cmort)
fit <- lm(cmort ~ trend + temp + temp2 + part, na.action = NULL)
Residuals <- as.numeric(resid(fit))
##Correlation plots
acf(Residuals, lag.max = 18,main = "")
pacf(Residuals, lag.max = 18,main = "")
ADCFplot(Residuals, MaxLag = 18, main = "Wild Bootstrap", method = "Wild")
ADCFplot(Residuals, MaxLag = 18, main = "Subsampling", method = "Subsampling")
fit2 <- arima(cmort, order =c(2, 0, 0), xreg = cbind(trend, temp, temp2, part))
Residuals2 <- as.numeric(residuals(fit2))
##Correlation plots
acf(Residuals2, lag.max = 18, main = "")
pacf(Residuals2, lag.max = 18, main = "")
ADCFplot(Residuals2, MaxLag = 18, main = "Wild Bootstrap", method = "Wild")
ADCFplot(Residuals2, MaxLag = 18, main = "Subsampling", method = "Subsampling")
UnivTest(Residuals2, type = "bartlett", p = 6, b = 499, parallel = TRUE)
## Univariate test of independence based on distance covariance
##
## data: Residuals2, kernel type: bartlett, bandwidth=6, boot replicates 499
## Tn = 67.7344, p-value = 0.118
UnivTest(Residuals2, type = "bartlett", p = 11, b = 499, parallel = TRUE)
## Univariate test of independence based on distance covariance
##
## data: Residuals2, kernel type: bartlett, bandwidth=11, boot replicates 499
## Tn = 125.6674, p-value = 0.170
UnivTest(Residuals2, type = "bartlett", p = 20, b = 499, parallel = TRUE)
## Univariate test of independence based on distance covariance
##
## data: Residuals2, kernel type: bartlett, bandwidth=20, boot replicates 499
## Tn = 225.9266, p-value = 0.208
box1 <- Box.test(Residuals2, lag = 6)
box2 <- Box.test(Residuals2, lag = 11)
box3 <- Box.test(Residuals2, lag = 20)
ljung1 <- Box.test(Residuals2, lag = 6, type = "Ljung")
ljung2 <- Box.test(Residuals2, lag = 11, type = "Ljung")
ljung3 <- Box.test(Residuals2, lag = 20, type = "Ljung")
data(ibmSp500)
new_data <- tail(ibmSp500[,2:3], 700)
series <- log(new_data + 1)
t=scale(series, center = TRUE, scale = FALSE)
t2 <- at^2
olnames(at) <- c("IBM", "SP")
olnames(at2) <- c("IBM_sq", "SP_sq")
cf(at, lag.max = 18)
cf(at2, lag.max = 18)
ADCFplot(at, MaxLag = 18, ylim = c(0, 0.2))
ADCFplot(at2, MaxLag = 18, ylim = c(0, 0.2))
λ
mADCFtest(at, "bartlett", p = 6, b = 499, parallel = TRUE)
## Multivariate test of independence based on distance correlation
##
## data: at, kernel type: bartlett, bandwidth=6, boot replicates 499
## Tnbar = 34.1743, p-value = 0.022
mADCFtest(at, "bartlett", p = 12, b = 499, parallel = TRUE)
## Multivariate test of independence based on distance correlation
##
## data: at, kernel type: bartlett, bandwidth=12, boot replicates 499
## Tnbar = 71.1713, p-value = 0.014
mADCFtest(at, "bartlett", p = 22, b = 499, parallel = TRUE)
## Multivariate test of independence based on distance correlation
##
## data: at, kernel type: bartlett, bandwidth=22, boot replicates 499
## Tnbar = 122.9424, p-value = 0.02
library(portes)
LjungBox(at, c(6, 12, 22))
library(MTS)
model <- VAR(at, 2)
resids <- residuals(model)
colnames(resids) <- c("IBM_res", "SP_res")
windows(9, 6)
acf(resids, lag.max = 18)
mADCFplot(resids, MaxLag = 18, ylim = c(0, 0.13))
## Tests of independence based on \overline{T}_n
mADCFtest(resids, "bartlett", p = 6, b = 499, parallel = TRUE)
## Multivariate test of independence based on distance correlation
##
## data: resids, kernel type: bartlett, bandwidth=6, boot replicates 499
## Tnbar = 29.9114, p-value = 0.036
mADCFtest(resids, "bartlett", p = 12, b = 499, parallel = TRUE)
## Multivariate test of independence based on distance correlation
##
## data: resids, kernel type: bartlett, bandwidth=12, boot replicates 499
## Tnbar = 64.7754, p-value = 0.018
mADCFtest(resids, "bartlett", p = 22, b = 499, parallel = TRUE)
## Multivariate test of independence based on distance correlation
##
## data: resids, kernel type: bartlett, bandwidth=22, boot replicates 499
## Tnbar = 115.3462, p-value = 0.034
## Tests of independence based on mLB
LjungBox(resids, c(6, 12, 22))
