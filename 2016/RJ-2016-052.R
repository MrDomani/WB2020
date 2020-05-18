library(quantreg.nonpar)
data("india")
data <- india
faccsex <- factor(data$csex)
mbmisq <- data$mbmi^2
form.par <- cheight ~ mbmi + mbmisq + breastfeeding + breastfeedingsq + mage +
magesq + medu + edupartner + faccsex + facctwin + faccbirthorder +
facmunemployed + facmreligion + facmresidence + facwealth + facelectricity +
facradio + factelevision + facrefrigerator + facbicycle + facmotorcycle + faccar
basis.bsp <- create.bspline.basis(breaks = quantile(data$cage, c(0:10)/10))
B <- 500
B.boot <- 100
taus <- c(1:24)/25
print.taus <- c(1:4)/5
alpha <- 0.05
piv.bsp <- npqr(formula = form.par, basis = basis.bsp, var = "cage", taus = taus,
nderivs = 1, average = 1, print.taus = print.taus, B = B, uniform = TRUE)
gaus.bsp <- update(piv.bsp, process = "gaussian", printOutput = FALSE)
wboot.bsp <- update(gaus.bsp, process = "wbootstrap", B = B.boot)
gboot.bsp <- update(wboot.bsp, process = "gbootstrap")
par(mfrow = c(2, 2))
yrange <- c(.65, .95)
xrange <- c(0, 1)
plot(xrange, yrange, type = "n", xlab = "Quantile Index",
ylab = "Average Growth (cm/month)", ylim = yrange)
lines(piv.bsp$taus, piv.bsp$point.est)
lines(piv.bsp$taus, piv.bsp$CI[1, , 1], col = "blue")
lines(piv.bsp$taus, piv.bsp$CI[1, , 2], col = "blue")
title("Pivotal")
plot(xrange, yrange, type = "n", xlab = "Quantile Index", ylab = "", ylim = yrange)
lines(gaus.bsp$taus, gaus.bsp$point.est)
lines(gaus.bsp$taus, gaus.bsp$CI[1, ,1], col="blue")
lines(gaus.bsp$taus, gaus.bsp$CI[1, ,2], col="blue")
title("Gaussian")
plot(xrange, yrange, type = "n", xlab = "Quantile Index",
ylab = "Average Growth (cm/month)", ylim = yrange)
lines(wboot.bsp$taus, wboot.bsp$point.est)
lines(wboot.bsp$taus, wboot.bsp$CI[1, , 1], col = "blue")
lines(wboot.bsp$taus, wboot.bsp$CI[1, , 2], col = "blue")
title("Weighted Bootstrap")
plot(xrange, yrange, type = "n", xlab = "Quantile Index", ylab = "", ylim = yrange)
lines(gboot.bsp$taus, gboot.bsp$point.est)
lines(gboot.bsp$taus, gboot.bsp$CI[1, , 1], col = "blue")
lines(gboot.bsp$taus, gboot.bsp$CI[1, , 2], col = "blue")
title("Gradient Bootstrap")
title("Average Growth Rate with 95% CI", outer = TRUE)
pval.dimnames <- vector("list", 2)
pval.dimnames[[1]] <- c("Pivotal", "Gaussian", "Weighted Bootstrap",
"Gradient Bootstrap")
pval.dimnames[[2]] <- c("H0: Growth Rate <= 0", "H0: Growth Rate >= 0",
"H0: Growth Rate = 0", "Computation Minutes")
pvals <- matrix(NA, nrow = 4, ncol = 4, dimnames = pval.dimnames)
pvals[1, ] <- c(round(piv.bsp$pvalues, digits = 4), round(piv.time, digits = 0))
pvals[2, ] <- c(round(gaus.bsp$pvalues, digits = 4), round(gaus.time, digits = 0))
pvals[3, ] <- c(round(wboot.bsp$pvalues, digits = 4), round(wboot.time, digits = 0))
pvals[4, ] <- c(round(gboot.bsp$pvalues, digits = 4), round(gboot.time, digits = 0))
pvals
# H0: Growth Rate <= 0 H0: Growth Rate >= 0 H0: Growth Rate = 0
# Pivotal 0 1 0.0237
# Gaussian 0 1 0.0234
# Weighted Bootstrap 0 1 0.0221
# Gradient Bootstrap 0 1 0.0221
# Computation Minutes
# Pivotal 0.9
# Gaussian 0.6
# Weighted Bootstrap 30.0
# Gradient Bootstrap 346.0
basis.poly <- poly(cage, degree = 12)
basis.four <- create.fourier.basis(rangeval = range(data$cage), nbasis = 9,
period = 200)
piv.poly <- update(piv.bsp, basis = basis.poly)
piv.four <- update(piv.bsp, basis = basis.four)
pval2.dimnames <- vector("list", 2)
pval2.dimnames[[1]] <- c("B-spline", "Polynomial", "Fourier")
pval2.dimnames[[2]] <- c("H0: Growth Rate <= 0", "H0: Growth Rate >= 0",
"H0: Growth Rate = 0")
pvals2 <- matrix(NA, nrow = 3, ncol = 3, dimnames = pval2.dimnames)
pvals2[1, ] <- round(piv.bsp$pvalues, digits = 4)
pvals2[2, ] <- round(piv.poly$pvalues, digits = 4)
pvals2[3, ] <- round(piv.four$pvalues, digits = 4)
pvals2
# H0: Growth Rate <= 0 H0: Growth Rate >= 0 H0: Growth Rate = 0
# B-spline 0 1 0.0239
# Polynomial 0 1 0.0334
# Fourier 0 1 0.0386
piv.bsp <- npqr(formula = form.par, basis = basis.bsp, var = "cage", taus = taus,
B = B, nderivs = 1, average = 1, alpha = alpha, process = "pivotal",
uniform = TRUE, se = "unconditional", printOutput = FALSE)
piv.bsp.cond <- update(piv.bsp, se = "conditional")
piv.bsp.point <- update(piv.bsp, uniform = FALSE, se = "unconditional")
piv.bsp.point.cond <- update(piv.bsp, uniform = FALSE, se = "conditional")
piv.bsp.med <- npqr(formula = form.par, basis = basis.bsp, var = "cage", taus = 0.5,
B = B, nderivs = 1, average = 1, alpha = alpha, process = "pivotal", uniform = TRUE,
se = "unconditional", printOutput = FALSE)
piv.bsp.cond.med <- update(piv.bsp.med, se = "conditional")
stderr.dimnames <- vector("list", 2)
stderr.dimnames[[1]] <- c("Unconditional", "Conditional")
stderr.dimnames[[2]] <- c("Standard Error")
stderr <- matrix(NA, nrow = 2, ncol = 1, dimnames = stderr.dimnames)
stderr[1, ] <- piv.bsp.med$std.error[1]
stderr[2, ] <- piv.bsp.cond.med$std.error[1]
stderr
# Standard Error
# Unconditional 0.008104
# Conditional 0.007663
pval3.dimnames <- vector("list", 2)
pval3.dimnames[[1]] <- c("Uniform, Unconditional", "Uniform, Conditional",
"Pointwise, Unconditional", "Pointwise, Conditional")
pval3.dimnames[[2]] <- c("H0: Growth Rate <= 0", "H0: Growth Rate >= 0",
"H0: Growth Rate = 0")
pvals3 <- matrix(NA, nrow = 4, ncol = 3, dimnames = pval3.dimnames)
pvals3[1, ] <- round(piv.bsp$pvalues, digits = 4)
pvals3[2, ] <- round(piv.bsp.cond$pvalues, digits = 4)
pvals3[3, ] <- round(piv.bsp.point$pvalues, digits = 4)
pvals3[4, ] <- round(piv.bsp.point.cond$pvalues, digits = 4)
pvals3
# H0: Growth Rate <= 0 H0: Growth Rate >= 0 H0: Growth Rate = 0
# Uniform, Unconditional 0 1 0.0239
# Uniform, Conditional 0 1 0.0243
# Pointwise, Unconditional 0 1 0.0267
# Pointwise, Conditional 0 1 0.0222
piv.bsp.firstderiv <- npqr(formula = form.par, basis = basis.bsp, var = "cage",
taus = taus, nderivs = 1, average = 0, print.taus = print.taus, B = B,
process = "none", printOutput = FALSE)
piv.bsp.secondderiv <- update(piv.bsp.firstderiv, nderivs = 2)
xsurf1 <- as.vector(piv.bsp.firstderiv$taus)
ysurf1 <- as.vector(piv.bsp.firstderiv$var.unique)
zsurf1 <- t(piv.bsp.firstderiv$point.est)
xsurf2 <- as.vector(piv.bsp.secondderiv$taus)
ysurf2 <- as.vector(piv.bsp.secondderiv$var.unique)
zsurf2 <- t(piv.bsp.secondderiv$point.est)
# (
# )
# (
# )
# (
# )
par(mfrow = c(1, 2))
persp(xsurf1, ysurf1, zsurf1, xlab = "Quantile Index", ylab = "Age (months)",
zlab = "Growth Rate", ticktype = "detailed", phi = 30, theta = 120, d = 5,
col = "green", shade = 0.75, main = "Growth Rate (B-splines)")
persp(xsurf2, ysurf2, zsurf2, xlab = "Quantile Index", ylab = "Age (months)",
zlab = "Growth Acceleration", ticktype = "detailed", phi = 30, theta = 120,
d = 5, col = "green", shade = 0.75, main = "Growth Acceleration (B-splines)")
# Order of Derivative H0: Growth Rate <= 0 H0: Growth Rate >= 0 H0: Growth Rate = 0
# First Deriviative 0 1 0.042
# Second Derivative 0 1 0.061
data.subset <- data[1:1000, ]
detach(data)
attach(data.subset)
faccage <- factor(cage)
piv.fac.fun <- npqr(formula = form.par, basis = faccage, var = "cage", taus = taus,
print.taus = print.taus, B = B, nderivs = 0, average = 0, alpha = alpha,
process = "none", rearrange = FALSE, rearrange.vars = "both", se = "conditional",
printOutput = FALSE, method = "fn")
piv.fac.fun.re <- update(piv.fac.fun, rearrange.vars = "both")
# (
# )
# (
# )
# (
# )
# γ
# (
# τ
# )
# (
# )
xsurf <- as.vector(piv.fac.fun$taus)
ysurf <- as.vector(piv.fac.fun$var.unique)
zsurf.fac <- t(piv.fac.fun$point.est)
zsurf.fac.re <- t(piv.fac.fun.re$point.est)
par(mfrow = c(1, 2))
persp(xsurf, ysurf, zsurf.fac, xlab = "Quantile Index", ylab = "Age (months)",
zlab = "Height", ticktype = "detailed", phi = 30, theta = 40, d = 5,
col = "green", shade = 0.75, main = "Growth Chart (Indicators)")
persp(xsurf, ysurf, zsurf.fac.re, xlab = "Quantile Index", ylab = "Age (months)",
zlab = "Height", ticktype = "detailed", phi = 30, theta = 40, d = 5,
col = "green", shade = 0.75, main = "Growth Chart (Indicators, Rearranged)")
# lipsitzm@bu.edu
