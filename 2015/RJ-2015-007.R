# eblupFH(formula, vardir, method = "REML", MAXITER = 100, PRECISION = 0.0001, data)
# mseFH(formula, vardir, method = "REML", MAXITER = 100, PRECISION = 0.0001, data)
data("milk")
attach(milk)
FH <- mseFH(yi ~ as.factor(MajorArea), SD^2)
cv.FH <- 100 * sqrt(FH$mse) / FH$est$eblup
results <- data.frame(Area = SmallArea, SampleSize = ni, DIR = yi,
cv.DIR = 100 * CV, eblup.FH = FH$est$eblup, cv.FH)
detach(milk)
results <- results[order(results$SampleSize, decreasing = TRUE), ]
# Figure 1 left
plot(results$DIR, type = "n", ylab = "Estimate", ylim = c(0.4, 1.6),
xlab = "area (sorted by decreasing sample size)", cex.axis = 1.5,
cex.lab = 1.5)
points(results$DIR, type = "b", col = 1, lwd = 2, pch = 1, lty = 1)
points(results$eblup.FH, type = "b", col = 4, lwd = 2, pch = 4, lty = 2)
legend("top", legend = c("Direct", "EBLUP FH"), ncol = 2, col = c(1, 4), lwd = 2,
pch = c(1, 4), lty = c(1, 2), cex = 1.3)
plot(results$cv.DIR, type = "n", ylab = "CV", ylim = c(5, 40),
xlab = "area (sorted by decreasing sample size)", cex.axis = 1.5,
cex.lab = 1.5)
points(results$cv.DIR, type = "b", col = 1, lwd = 2, pch = 1, lty = 1)
points(results$cv.FH, type = "b", col = 4, lwd = 2, pch = 4, lty = 2)
legend("top", legend = c("Direct", "EBLUP FH"), ncol = 2, col = c(1, 4), lwd = 2,
pch = c(1, 4), lty = c(1, 2), cex = 1.3)
data("grapes")
data("grapesprox")
SFH <- mseSFH(grapehect ~ area + workdays - 1, var, grapesprox, data = grapes)
# eblupSFH(formula, vardir, proxmat, method = "REML", MAXITER = 100, PRECISION = 0.0001,
# data)
# mseSFH(formula, vardir, proxmat, method = "REML", MAXITER = 100, PRECISION = 0.0001,
# data)
# pbmseSFH(formula, vardir, proxmat, B = 100, method = "REML", MAXITER = 100,
# PRECISION = 0.0001, data)
# npbmseSFH(formula, vardir, proxmat, B = 100, method = "REML", MAXITER = 100,
# PRECISION = 0.0001, data)
cv.SFH <- 100 * sqrt(SFH$mse) / SFH$est$eblup
results <- data.frame(DIR = grapes$grapehect,
cv.DIR = 100 * abs(sqrt(grapes$var) / grapes$grapehect),
eblup.SFH = SFH$est$eblup, cv.SFH)
# Sort results by increasing CV of direct estimators
results <- results[order(results$cv.DIR), ]
# Figure 2 left
plot(results$DIR, type = "n", ylab = "Estimate", ylim = c(0, 400),
xlab = "area (sorted by increasing CVs of direct estimators)", cex.axis = 1.5,
cex.lab = 1.5)
points(results$DIR, type = "p", col = 1, lwd = 2, pch = 1)
points(results$eblup.SFH, type = "p", col = 4, lwd = 2, pch = 4)
legend("top", legend = c("Direct", "EBLUP SFH"), ncol = 2, col = c(1, 4), lwd = 2,
pch = c(1, 4), cex = 1.3)
# Figure 2 right
plot(results$cv.DIR, type = "n", ylab = "CV", ylim = c(0, 400),
xlab = "area (sorted by increasing CVs of direct estimators)", cex.axis = 1.5,
cex.lab = 1.5)
points(results$cv.DIR, type = "p", col = 1, lwd = 2, pch = 1)
points(results$cv.SFH, type = "p", col = 4, lwd = 2, pch = 4)
legend("top", legend = c("Direct", "EBLUP SFH"), ncol = 2, col = c(1, 4), lwd = 2,
pch = c(1, 4), cex = 1.3)
# eblupSTFH(formula, D, T, vardir, proxmat, model = "ST", MAXITER = 100,
# PRECISION = 0.0001, data)
# pbmseSTFH(formula, D, T, vardir, proxmat, B = 100, model = "ST", MAXITER = 100,
# PRECISION = 0.0001, data)
data("spacetime")
data("spacetimeprox")
D <- nrow(spacetimeprox) # number of areas
T <- length(unique(spacetime$Time)) # number of time periods
set.seed(123)
STFH <- pbmseSTFH(Y ~ X1 + X2, D, T, vardir = Var, spacetimeprox, data = spacetime)
# Bootstrap procedure with B = 100 iterations starts.
# b = 1
# ...
# b = 100
# Compute CVs for the EBLUPs based on the STFH model and for the direct estimators
cv.STFH <- 100 * sqrt(STFH$mse) / STFH$est$eblup
cv.DIR <- 100 * sqrt(spacetime$Var) / spacetime$Y
results <- data.frame(Area = spacetime$Area, Time = spacetime$Time,
DIR = spacetime$Y, eblup.STFH = STFH$est$eblup,
cv.DIR, cv.STFH)
results.lasttime <- results[results$Time == 3, ]
print(results.lasttime, row.names = FALSE)
# Area Time DIR eblup.STFH cv.DIR cv.STFH
# 2 3 0.261484 0.27343181 10.944523 7.653997
# 3 3 0.175358 0.17722992 7.777336 7.026746
# 8 3 0.096230 0.09653879 6.059391 5.567674
# 12 3 0.122160 0.13740348 21.904205 14.798918
# 13 3 0.294176 0.29129477 8.812059 6.657347
# 16 3 0.412106 0.31887378 13.584403 9.224897
# 17 3 0.057924 0.06912566 25.195980 20.314774
# 25 3 0.209146 0.17377084 15.411972 12.225196
# 43 3 0.148671 0.14398844 15.788815 14.700855
# 45 3 0.234361 0.22810227 9.550663 8.303303
# 46 3 0.137869 0.14354272 8.853735 8.355827
results.lasttime <- results.lasttime[order(results.lasttime$cv.DIR), ]
# Figure 3 left
plot(results.lasttime$DIR, type = "n", xlab = "area (time=3)", ylab = "Estimate",
ylim = c(0.05, 0.45), cex.axis = 1.5, cex.lab = 1.5, xaxt = "n")
axis(1, 1:11, results.lasttime$Area, cex.axis = 1.5)
points(results.lasttime$DIR, type = "b", col = 1, lwd = 2, pch = 1, lty = 1)
points(results.lasttime$eblup.STFH, type = "b", col = 4, lwd = 2, pch = 4, lty = 2)
legend("top", legend = c("Direct", "EBLUP STFH"), ncol = 2, col = c(1, 4), lwd = 2,
pch = c(1, 4), lty = c(1, 2), cex = 1.3)
# Figure 3 right
plot(results.lasttime$cv.DIR, type = "n", xlab = "area (time=3)", ylab = "CV",
cex.axis = 1.5, cex.lab = 1.5, xaxt = "n")
axis(1, 1:11, results.lasttime$Area, cex.axis = 1.5)
points(results.lasttime$cv.DIR, type = "b", col = 1, lwd = 2, pch = 1, lty = 1)
points(results.lasttime$cv.STFH, type = "b", col = 4, lwd = 2, pch = 4, lty = 2)
legend("top", legend = c("Direct", "EBLUP STFH"), ncol = 2, col = c(1, 4), lwd = 2,
pch = c(1, 4), lty = c(1, 2), cex = 1.3)
# eblupBHF(formula, dom, selectdom, meanxpop, popnsize, method = "REML", data)
# pbmseBHF(formula, dom, selectdom, meanxpop, popnsize, B = 200, method = "REML", data)
data("cornsoybeanmeans")
Xmean <- data.frame(cornsoybeanmeans[, c("CountyIndex", "MeanCornPixPerSeg",
"MeanSoyBeansPixPerSeg")])
Popn <- data.frame(cornsoybeanmeans[, c("CountyIndex", "PopnSegments")])
data("cornsoybean")
cornsoybean <- cornsoybean[-33, ]
set.seed(123)
BHF <- pbmseBHF(CornHec ~ CornPix + SoyBeansPix, dom = County, meanxpop = Xmean,
popnsize = Popn, B = 200, data = cornsoybean)
# Bootstrap procedure with B = 200 iterations starts.
# b = 1
# ...
# b = 200
cv.BHF <- 100 * sqrt(BHF$mse$mse) / BHF$est$eblup$eblup
results <- data.frame(CountyIndex = BHF$est$eblup$domain,
CountyName = cornsoybeanmeans$CountyName,
SampleSize = BHF$est$eblup$sampsize,
eblup.BHF = BHF$est$eblup$eblup, cv.BHF)
print(results, row.names = FALSE)
# CountyIndex CountyName SampleSize eblup.BHF cv.BHF
# 1 CerroGordo 1 122.1954 8.066110
# 2 Hamilton 1 126.2280 7.825271
# 3 Worth 1 106.6638 9.333344
# 4 Humboldt 2 108.4222 7.598736
# 5 Franklin 3 144.3072 4.875002
# 6 Pocahontas 3 112.1586 6.020232
# 7 Winnebago 3 112.7801 5.951520
# 8 Wright 3 122.0020 5.700670
# 9 Webster 4 115.3438 4.808813
# 10 Hancock 5 124.4144 4.495448
# 11 Kossuth 5 106.8883 4.532518
# 12 Hardin 5 143.0312 3.504340
# ebBHF(formula, dom, selectdom, Xnonsample, MC = 100, data, transform = "BoxCox",
# lambda = 0, constant = 0, indicator)
# pbmseebBHF(formula, dom, selectdom, Xnonsample, B = 100, MC = 100, data,
# transform = "BoxCox", lambda = 0, constant = 0, indicator)
# Xslabor2 -0.079 0.039 -0.039 0.168
# Xslabor1 -0.199 0.128 -0.228
# Xs(In) Xsedc1 Xsedc3 Xslbr1
# Xseduc1 -0.212
# Xseduc3 -0.070 0.206
povertyincidence <- function(y) {
result <- mean(y < 6557.143)
return (result)
}
data("incomedata")
data("Xoutsamp")
provincecodes <- unique(Xoutsamp$domain)
provincelabels <- unique(incomedata$provlab)[provincecodes]
Xoutsamp_AuxVar <- Xoutsamp[ ,c("domain", "educ1", "educ3", "labor1", "labor2")]
set.seed(123)
EB <- ebBHF(income ~ educ1 + educ3 + labor1 + labor2, dom = prov,
selectdom = provincecodes, Xnonsample = Xoutsamp_AuxVar, MC = 50,
constant = 3500, indicator = povertyincidence, data = incomedata)
EB$fit$summary
# Linear mixed-effects model fit by REML
# Data: NULL
# AIC BIC logLik
# 18980.72 19034.99 -9483.361
# Random effects:
# Formula: ~1 | as.factor(dom)
# (Intercept) Residual
# StdDev: 0.09436138 0.4179426
# Fixed effects: ys ~ -1 + Xs
# Value Std.Error DF t-value p-value
# Xs(Intercept) 9.505176 0.014384770 17143 660.7805 0
# Xseduc1 -0.124043 0.007281270 17143 -17.0359 0
# Xseduc3 0.291927 0.010366323 17143 28.1611 0
# Xslabor1 0.145985 0.006915979 17143 21.1084 0
# Xslabor2 -0.081624 0.017082634 17143 -4.7782 0
# Correlation:Xs(In) Xsedc1 Xsedc3 Xslbr1
# Xseduc1 -0.212
# Xseduc3 -0.070 0.206
# Xslabor1 -0.199 0.128 -0.228
# Xslabor2 -0.079 0.039 -0.039 0.168
# Standardized Within-Group Residuals:
# Min Q1 Med Q3 Max
# -4.2201202 -0.6617181 0.0203607 0.6881828 3.5797393
# Number of Observations: 17199
# Number of Groups: 52
# # Figure 4 left
plot(EB$fit$residuals, xlab = "Index", ylab = "Residuals", cex.axis = 1.5,
cex.lab = 1.5, ylim = c(-2, 2), col = 4)
abline(h = 0)
# Figure 4 right
hist(EB$fit$residuals, prob = TRUE, xlab = "Residuals", ylab = "", main = "",
cex.axis = 1.5, cex.lab = 1.5, xlim = c(-2, 2), ylim = c(0, 1))
set.seed(123)
pbmse.EB <- pbmseebBHF(income ~ educ1 + educ3 + labor1 + labor2, dom = prov,
selectdom = provincecodes, Xnonsample = Xoutsamp_AuxVar,
B = 200, MC = 50, constant = 3500,
indicator = povertyincidence, data = incomedata)
# Bootstrap procedure with B = 200 iterations starts.
# b = 1
# ...
# b = 200
pbcv.EB <- 100 * sqrt(pbmse.EB$mse$mse) / abs(pbmse.EB$est$eb$eb) # compute CV
results.EB <- data.frame(ProvinceIndex = pbmse.EB$est$eb$domain,
ProvinceName = provincelabels,
SampleSize = pbmse.EB$est$eb$sampsize,
EB = pbmse.EB$est$eb$eb, cv.EB = pbcv.EB)
results.EB
# ProvinceIndex ProvinceName SampleSize EB cv.EB
# 1 42 Soria 20 0.2104329 21.06776
# 2 5 Avila 58 0.1749877 19.49466
# 3 34 Palencia 72 0.2329916 11.57829
# 4 44 Teruel 72 0.2786618 11.89621
# 5 40 Segovia 58 0.2627178 13.21378
