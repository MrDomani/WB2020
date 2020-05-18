library("Qtools")
set.seed(467)
y <- rpois(1000, 4)
pmid <- midecdf(y)
xmid <- midquantile(y, probs = pmid$y)
pmid
# Empirical mid-ECDF
# Call:
# midecdf(x = y)
xmid
# Empirical mid-ECDF
# Call:
# midquantile(x = y, probs = pmid$y)
xmid <- midquantile(y, probs = 1:3/4)
x <- confint(xmid, level = 0.95)
x
# midquantile lower upper
# 25% 2.540000 2.416462 2.663538
# 50% 3.822816 3.693724 3.951907
# 75% 5.254902 5.072858 5.436946
attr(x, "stderr")
# [1] 0.06295447 0.06578432 0.09276875
par(mfrow = c(1,2))
plot(pmid, xlab = "y", ylab = "CDF", jumps = TRUE)
points(pmid$x, pmid$y, pch = 15)
plot(xmid, xlab = "p", ylab = "Quantile", jumps = TRUE)
points(xmid$x, xmid$y, pch = 15)
qlss(fun = "qnorm", probs = 0.1)
# call:
# qlss.default(fun = "qnorm", probs = 0.1)
# Unconditional Quantile-Based Location, Scale, and Shape
# ** Location **
# Median
# [1] 0
# ** Scale **
# Inter-quartile range (IQR)
# [1] 1.34898
# Inter-quantile range (IPR)
# 0.1
# 2.563103
# ** Shape **
# Skewness index
# 0.1
# 0
# Shape index
# 0.1
# 1.900031
y <- faithful$waiting
par(mfrow = c(1,2))
plot(density(y))
plot(midquantile(y, probs = p), jumps = FALSE)
plot(midquantile(y, probs = 0.1), jumps = FALSE)
qlss(y, probs = c(0.05, 0.1, 0.25), type = 7)
# call:
# qlss.numeric(x = y, probs = c(0.05, 0.1, 0.25), type = 7)
# Unconditional Quantile-Based Location, Scale, and Shape
# ** Location **
# Median
# [1] 76
# ** Scale **
# Inter-quartile range (IQR)
# [1] 24
# Inter-quantile range (IPR)
# 0.05 0.1 0.25
# 41 35 24
# ** Shape **
# Skewness index
# 0.05 0.1 0.25
# -0.3658537 -0.4285714 -0.5000000
# Shape index
# 0.05 0.1 0.25
# 1.708333 1.458333 1.000000

require("quantreg")
y <- faithful$waiting
x <- as.numeric(faithful$eruptions >= 3)
fit <- rq(formula = y ~ x, tau = c(0.1, 0.25, 0.5, 0.75, 0.9))
fit
# Call:
# rq(formula = y ~ x, tau = c(0.1, 0.25, 0.5, 0.75, 0.9))
# Coefficients:
# tau= 0.10 tau= 0.25 tau= 0.50 tau= 0.75 tau= 0.90
# (Intercept) 47 50 54 59 63
# x 26 26 26 25 25
# Degrees of freedom: 272 total; 270 residual
kt <- KhmaladzeTest(formula = y ~ x, taus = seq(.05, .95, by = .01))
KhmaladzeFormat(kt, 0.05)
# Khmaladze test for the location-shift hypothesis
# Joint test is not significant at 10% level
# Test(s) for individual slopes:
# not significant at 10% level

# )

dd <- airquality[complete.cases(airquality), ]
dd <- dd[order(dd$Solar.R), ]
fit.rq <- rq(Ozone ~ Solar.R, tau = c(.1, .5, .9), data = dd)
x <- seq(min(dd$Solar.R), max(dd$Solar.R), length = 200)
yhat <- predict(fit.rq, newdata = data.frame(Solar.R = x))
plot(Ozone ~ Solar.R, data = dd)
apply(yhat, 2, function(y, x) lines(x, y), x = x)
gof.rq <- GOFTest(fit.rq, alpha = 0.05, B = 1000, seed = 987)
gof.rq
# Goodness-of-fit test for quantile regression based on the cusum process
# Quantile 0.1: Test statistic = 0.1057; p-value = 0.001
# Quantile 0.5: Test statistic = 0.2191; p-value = 0
# Quantile 0.9: Test statistic = 0.0457; p-value = 0.018
# "boot"
system.time(fit.rqt <- tsrq(Ozone ~ Solar.R, data = dd, tsf = "mcjI",
symm = TRUE, dbounded = FALSE, lambda = seq(1, 3, by = 0.005),
conditional = FALSE, tau = c(.1, .5, .9)))
# user system elapsed
# 0.5 0.0 0.5
fit.rqt
# call:
# tsrq(formula = Ozone ~ Solar.R, data = dd, tsf = "mcjI", symm = TRUE,
# dbounded = FALSE, lambda = seq(1, 3, by = 0.005), conditional = FALSE,
# tau = c(0.1, 0.5, 0.9))
# Proposal I symmetric transformation (singly bounded response)
# Optimal transformation parameter:
# tau = 0.1 tau = 0.5 tau = 0.9
# 2.210 2.475 1.500
# Coefficients linear model (transformed scale):
# tau = 0.1 tau = 0.5 tau = 0.9
# (Intercept) -3.3357578 -48.737341 16.557327
# Solar.R 0.4169697 6.092168 1.443407
# Degrees of freedom: 111 total; 109 residual
x <- seq(9, 334, length = 200)
qhat <- predict(fit.rqt, newdata = data.frame(Solar.R = x),
type = "response")
dqhat <- predict(fit.rqt, newdata = data.frame(Solar.R = x),
type = "maref", namevec = "Solar.R")
# The linear component of the marginal effect is calculated as derivative of
# Ozone ~ beta1 * Solar.R
# with respect to Solar.R
par(mfrow = c(1, 2))
plot(Ozone ~ Solar.R, data = dd, xlab = "Solar radiation (lang)",
ylab = "Ozone (ppb)")
for(i in 1:3) lines(x, qhat[ ,i], lty = c(1, 2, 4)[i], lwd = 2)
plot(range(x), range(dqhat), type = "n", xlab = "Solar radiation (lang)",
ylab = "Marginal effect")
for(i in 1:3) lines(x, dqhat[ ,i], lty = c(1, 2, 4)[i], lwd = 2)
GOFTest(fit.rqt, alpha = 0.05, B = 1000, seed = 416)
# Goodness-of-fit test for quantile regression based on the cusum process
# Quantile 0.1: Test statistic = 0.0393; p-value = 0.025
# Quantile 0.5: Test statistic = 0.1465; p-value = 0.005
# Quantile 0.9: Test statistic = 0.0212; p-value = 0.127
system.time(fit.rqt <- rcrq(Ozone ~ Solar.R, data = dd, tsf = "mcjI",
symm = TRUE, dbounded = FALSE, lambda = seq(1, 3, by = 0.005),
tau = c(.1, .5, .9)))
# user system elapsed
# 36.88 0.03 37.64
data(Chemistry)
fit.rqt <- tsrq(score ~ gcse, data = Chemistry, tsf = "ao", symm = FALSE,
lambda = seq(0, 2, by = 0.01), tau = 0.9)
summary(fit.rqt, conditional = FALSE, se = "nid")
# call:
# summary.rqt(object = fit.rqt, se = "nid", conditional = FALSE)
# Aranda-Ordaz asymmetric transformation (doubly bounded response)
# Summary for unconditional inference
# tau = 0.9
# Optimal transformation parameter:
# Value Std. Error Lower bound Upper bound
# 0.000000000 0.001364422 -0.002674218 0.002674218
# Coefficients linear model (transformed scale):
# Value Std. Error Lower bound Upper bound
# (Intercept) -4.3520060 0.015414540 -4.3822179 -4.3217941
# gcse 0.8978072 0.002917142 0.8920898 0.9035247
# Degrees of freedom: 31022 total; 31020 residual
coef(tsrq2(score ~ gcse, data = chemsub, dbounded = TRUE,
lambda = seq(0, 2, by = 0.1), delta = seq(0, 2, by = 0.1),
tau = 0.9), all = TRUE)
# (Intercept) gcse lambda delta
# -4.1442274 0.8681246 0.0000000 0.0000000
fit.qlss <- qlss(formula = Ozone ~ Solar.R, data = airquality, type =
"rqt", tsf = "mcjI", symm = TRUE, dbounded = FALSE, lambda =
seq(1, 3, by = 0.005), probs = c(0.05, 0.1))
fit.qlss
# call:
# qlss.formula(formula = Ozone ~ Solar.R, probs = c(0.05, 0.1),
# data = airquality, type = "rqt", tsf = "mcjI", symm = TRUE,
# dbounded = FALSE, lambda = seq(1, 3, by = 0.005))
# Conditional Quantile-Based Location, Scale, and Shape
# -- Values are averaged over observations --
# ** Location **
# Median
# [1] 30.2258
# ** Scale **
# Inter-quartile range (IQR)
# [1] 43.40648
# Inter-quantile range (IPR)
# 0.05 0.1
# 88.02909 73.93430
# **Shape**
# Skewness index
# 0.05 0.1
# 0.5497365 0.5180108
# Shape index
# 0.05 0.1
# 1.960315 1.661648
set.seed(567)
x <- seq(9, 334, length = 200)
qhat <- predict(fit.qlss, newdata = data.frame(Solar.R = x),
interval = TRUE, level = 0.90, R = 500)
plot(qhat, z = x, whichp = 0.1, interval = TRUE, type = "l",
xlab = "Solar radiation (lang)", lwd = 2)


data("esterase")
taus <- c(.1, .25, .5, .75, .9)
fit.rq <- rq(Count ~ Esterase, data = esterase, tau = taus)
yhat1 <- fitted(fit.rq)
fit.rrq <- rrq(Count ~ Esterase, data = esterase, tau = taus)
yhat2 <- fitted(fit.rrq)
kt <- KhmaladzeTest(formula = Count ~ Esterase, data = esterase,
taus = seq(.05,.95,by = .01), nullH = "location-scale")
KhmaladzeFormat(kt, 0.05)
# Khmaladze test for the location-shift hypothesis
# Joint test is not significant at 10% level
# Test(s) for individual slopes:
# not significant at 10% level
# 

# 
# (
# )

# (
# )
# ∗

# (
# ∗
# )
# ∗
# ∗
