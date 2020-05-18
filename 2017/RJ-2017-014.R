# chartgeneric <- new("Charttype", model = Datamodel(Delta = x),...)
# SPCproperty(data, nrep, property = "specifyproperty",chart = chartgeneric,
# params = list(specifyparameters), covprob=0.9, parallel=1,...)
library(spcadjust)
chart <- new("SPCCUSUM", model = SPCModelNormal(Delta = 1))
X <- rnorm(100)
xihat <- xiofdata(chart, X)
str(xihat)
# List of 3
# $ mu: num -0.0284
# $ sd: num 0.921
# $ m : int 100
cal <- SPCproperty(data = X, nrep = 50, property = "calARL",
chart = chart, params = list(target = 500), covprob = 0.9,quiet = TRUE)
cal
# 90 % CI: A threshold of 5.513 gives an in-control ARL of at least 500.
# Unadjusted result: 4.101
# Based on 50 bootstrap repetitions.
SPCproperty(data = X, nrep = 50, property = "calhitprob",
chart = chart, params = list(target = 0.05, nsteps=100), covprob = 0.9,
quiet = TRUE)
# 90 % CI: A threshold of 7.137 gives an in-control false alarm probability
# of at most 0.05 within 100 steps.
# Unadjusted result: 5.285
# Based on 50 bootstrap repetitions.
newX <- rnorm(100)
S <- runchart(chart, newdata = newX, xi = xihat)
par(mfrow = c(1, 2), mar = c(4, 5, 0, 0))
plot(newX, xlab = "t")
plot(S, ylab = expression(S[t]), xlab = "t", type = "b",
ylim = range(S, cal@res+2, cal@raw))
lines(c(0,100), rep(cal@res, 2), col = "red")
lines(c(0,100), rep(cal@raw, 2), col = "blue", lty = 2)
legend("topleft", c("Adjusted threshold","Unadjusted threshold"),
col = c("red", "blue"), lty = 1:2)
chartShew <- new("SPCShew", model = SPCModelNormal(), twosided = TRUE)
X <- rnorm(250)
xihat <- xiofdata(chartShew, X)
str(xihat)
# List of 3
# $ mu: num 0.0251
# $ sd: num 1.05
# $ m : int 250
SPCproperty(data = X, nrep = 50, property = "ARL", chart = chartShew,
params = list(threshold = 3), quiet = TRUE)
# 90 % CI: A threshold of 3 gives an in-control ARL of at least 213.1.
# Unadjusted result: 370.4
# Based on 50 bootstrap repetitions.
cal <- SPCproperty(data = X, nrep = 50, property = "calARL", chart = chartShew,
params = list(target = 370), quiet = TRUE)
cal
# 90 % CI: A threshold of 3.209 gives an in-control ARL of at least 370.
# Unadjusted result: 3
# Based on 50 bootstrap repetitions.
newX <- rnorm(150, mean = c(rep(0, 100), rep(2, 50)))
S <- runchart(chartShew, newdata = newX, xi = xihat)
n <- 500
Xlinreg <- data.frame(x1 = rbinom(n, 1, 0.4), x2 = runif(n, 0, 1), x3 = rnorm(n))
Xlinreg$y <- 2 + Xlinreg$x1 + Xlinreg$x2 + Xlinreg$x3 + rnorm(n)
chartlinregCUSUM <-
new("SPCCUSUM", model = SPCModellm(Delta = 1, formula = "y~x1+x2+x3"))
xihat <- xiofdata(chartlinregCUSUM, Xlinreg)
xihat
# Call:
# lm(formula = formula, data = P)
# Coefficients:
# (Intercept) x1 x2 x3
# 2.0222 1.0360 1.0350 0.9711
cal <- SPCproperty(data = Xlinreg, nrep = 50, property = "calARL",
chart = chartlinregCUSUM, params = list(target = 100), quiet = TRUE)
cal
# 90 % CI: A threshold of 3.138 gives an in-control ARL of at least 100.
# Unadjusted result: 2.745
# Based on 50 bootstrap repetitions.

n <- 120
newXlinreg <- data.frame(x1 = rbinom(n, 1, 0.4), x2 = runif(n, 0, 1),
x3 = rnorm(n))
outind <- c(rep(0, 100), rep(1, n-100))
newXlinreg$y <-
2 + newXlinreg$x1 + newXlinreg$x2 + newXlinreg$x3 + rnorm(n) + outind

S <- runchart(chartlinregCUSUM, newdata = newXlinreg, xi = xihat)
chartlinregEWMA <- new("SPCEWMA", model = SPCModellm(Delta = 0,
formula = "y~x1+x2+x3"), lambda = 0.1)
calEWMA <- SPCproperty(data = Xlinreg, nrep = 50, property = "calARL",
chart = chartlinregEWMA, params = list(target = 100), quiet = TRUE)
calEWMA
# 90 % CI: A threshold of +/- 0.5337 gives an in-control ARL of at least 100.
# Unadjusted result: 0.496
# Based on 50 bootstrap repetitions.
xihat <- xiofdata(chartlinregEWMA, Xlinreg)
M <- runchart(chartlinregEWMA, newdata = newXlinreg, xi = xihat)
model <- SPCModelNormal(Delta = 1)
model$Pofdata
# function (data)
# {
# list(mu = mean(data), sd = sd(data), m = length(data))
# }
model$Pofdata <- function(data){
  list(mu = median(data), sd = mad(data), m = length(data))
}


function(x)
X <- rnorm(100)
chartrobust <- new("SPCCUSUM", model = model)
SPCproperty(data = X, nrep = 50, property = "calARL",
chart = chartrobust, params = list(target = 100), quiet = TRUE)
# 90 % CI: A threshold of 4.162 gives an in-control ARL of at least 100.
# Unadjusted result: 2.987
# Based on 50 bootstrap repetitions.
SPCModelExponential = function(Delta = 1.25){
structure(list(
Pofdata = function(data){
list(lambda = 1/mean(data), n = length(data))
},
xiofP = function(P) P,
resample = function(P) rexp(P$n, rate = P$lambda),
getcdfupdates = function(P, xi) {
  if (Delta<1)
    function(x)
      pmax(0, 1-exp(-P$lambda*(x-log(Delta))/(xi$lambda*(1-Delta))))
  else
    function(x)
      pmin(1, exp(-P$lambda*(log(Delta)-x)/(xi$lambda*(Delta-1))))
},
updates = function(xi, data) log(Delta)-xi$lambda*(Delta-1)*data
), class = "SPCDataModel")
}
  
ExpCUSUMchart <- new("SPCCUSUM", model = SPCModelExponential(Delta = 1.25))
X <- rexp(500)
cal <- SPCproperty(data = X, nrep = 50, property = "calARL", chart = ExpCUSUMchart,
params = list(target = 1000), covprob = 0.9, quiet = TRUE)
cal
# 90 % CI: A threshold of 4.054 gives an in-control ARL of at least 1000.
# Unadjusted result: 3.165
# Based on 50 bootstrap repetitions.
setClass("SPCShewAsym", contains = c("SPCchart"))
setMethod("getq", signature = "SPCShewAsym", function(chart, property, params){
if (property == "calARL"){
list(
q = function(P, xi){
pobs <- function(alpha)(
getcdfupdates(chart, xi = xi, P = P)(xi$quant(alpha/2))
+(1-getcdfupdates(chart, xi = xi, P = P)(xi$quant(1-alpha/2))))
res <- uniroot(function(x) params$target-(1/pobs(x)),
lower = 1e-7,upper = 0.4)$root
as.double(log(res/(1-res)))
},
trafo = function(x) exp(x)/(1+exp(x)),
lowerconf = FALSE,
format = function(res)
paste("A threshold of alpha=", format(res, digits = 4),
" gives an in-control ARL of at least ",
params$target, ".", sep = "", collapse = "")
)
}else if (property == "ARL"){
list(
q = function(P, xi){
-log(getcdfupdates(chart, xi = xi, P = P)(xi$quant(params$alpha/2))
+(1-getcdfupdates(chart, xi = xi, P = P)(xi$quant(1-params$alpha/2)))
)},
trafo = function(x) exp(x),
lowerconf = FALSE,
format = function(res)
paste("A threshold defined by alpha=", params$alpha,
" gives an in-control ARL of at least ",
format(res, digits = 4), ".", sep = "",collapse = "")
)
}else stop("property ", property, " not implemented.")
})
# [1] "getq"
X <- rgamma(100, scale = 3, shape = 2)
modGammaBasic = structure(
list(
Pofdata = function(data){
list(scale = var(data)/mean(data),
shape = mean(data)^2/var(data),
n = length(data))
},
xiofP = function(P){
res <- P;
res$quant <- function(alpha)
qgamma(alpha, shape = P$shape, scale = P$scale);
res
},
resample = function(P) {
rgamma(P$n, shape = P$shape, scale = P$scale)
},
getcdfupdates = function(P, xi) {
# 90 % CI: A threshold of alpha=0.002869 gives an in-control ARL of at least
# 100.
function(x) pgamma(x, shape = P$shape, scale = P$scale)
},
updates = function(xi, data) data
),
class = "SPCDataModel")
chartAsym <- new("SPCShewAsym", model = modGammaBasic)
SPCproperty(data = X, nrep = 50, chart = chartAsym,
property = "ARL", params = list(alpha = 0.01), quiet = TRUE)
# 90 % CI: A threshold defined by alpha=0.01 gives an in-control ARL of at
# least 34.54.
# Unadjusted result: 100
# Based on 50 bootstrap repetitions.
SPCproperty(data = X, nrep = 50,
property = "calARL", chart = chartAsym,
params = list(target = 100), quiet = TRUE)
# Unadjusted result: 0.009998
# Based on 50 bootstrap repetitions.
modExp = modGammaBasic
modExp$Pofdata <- function(data){
list(scale = mean(data),
shape = 1,
n = length(data))
}
chartAsymExp <- new("SPCShewAsym", model = modExp)
X <- rexp(100)
SPCproperty(data = X, nrep = 50, chart = chartAsymExp,
property = "ARL", params = list(alpha = 0.01), quiet = TRUE)
# 90 % CI: A threshold defined by alpha=0.01 gives an in-control ARL of at
# least 84.08.
# Unadjusted result: 100
# Based on 50 bootstrap repetitions.
SPCproperty(data = X, nrep = 50,
property = "calARL", chart = chartAsymExp,
params = list(target = 100), quiet = TRUE)
# 90 % CI: A threshold of alpha=0.007553 gives an in-control ARL of at least
# 100.
# Unadjusted result: 0.009998
# Based on 50 bootstrap repetitions.
data(cardiacsurgery)
#Use dead within 30 days as response
dead30 <- as.numeric(cardiacsurgery$time <= 30)
#Use the two first years of data as phase I sample
phaseone <- cardiacsurgery$date <= 730
estdata <-data.frame(y = dead30[phaseone],
x = sqrt(cardiacsurgery$Parsonnet[phaseone]))
#Use the five last years of data as phase II sample
phasetwo <- !phaseone
rundata <- data.frame(y = dead30[phasetwo],
x = sqrt(cardiacsurgery$Parsonnet[phasetwo]),
z = cardiacsurgery$surgeon[phasetwo],
year = (cardiacsurgery$date[phasetwo]-730)/365)
chartlogregd <-
new("SPCCUSUM", model = SPCModellogregLikRatio(Delta = 0.75, formula = "y~x"))
chartlogregh <-
new("SPCCUSUM", model = SPCModellogregLikRatio(Delta = -0.75, formula = "y~x"))
cald <- SPCproperty(data = estdata, chart = chartlogregd, property = "calARL",
nrep = 50, params = list(target = 10000, gridpoints = 250),
parallel = Inf)
cald
# 90 % CI: A threshold of 6.157 gives an in-control ARL of at least 10000.
# Unadjusted result: 5.065
# Based on 50 bootstrap repetitions.
calh <- SPCproperty(data = estdata, chart = chartlogregh, property = "calARL",
nrep = 50, params = list(target = 10000, gridpoints = 250),
parallel = Inf)
calh
# 90 % CI: A threshold of 6.271 gives an in-control ARL of at least 10000.
# Unadjusted result: 4.469
# Based on 50 bootstrap repetitions.
