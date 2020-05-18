Rastrigin <- function(x) {
fn.call <<- fn.call + 1
sum(x^2 - 10 * cos(2 * pi * x)) + 10 * length(x)
}
options(digits = 10)
dimension <- 2
lower <- rep(-5.12, dimension)
upper <- rep(5.12, dimension)
set.seed(1234)
sink("tmp.txt")
fn.call <<- 0
library(DEoptim)
out.DEoptim <- DEoptim(fn = Rastrigin, lower = lower, upper = upper,
control = list(storepopfrom = 1))
sink(NULL)
out.DEoptim$optim[c(1, 2, 3)]
# $bestmem
# par1 par2
# -4.775211422e-07 6.390004611e-08
# $bestval
# [1] 4.60502747e-11
# $nfeval
# [1] 402
cat("DEoptim call functions", fn.call, "times.\n")
# DEoptim call functions 4020 times.
set.seed(1234)
x.ini <- lower + runif(length(lower)) * (upper - lower)
fn.call <- 0
out.sann <- optim(par = x.ini, fn = Rastrigin, method = "SANN")
out.sann[c("value","par","counts")]
# $value
# [1] 3.980605068
# $par
# [1] -1.98836973902 -0.00123560529
# $counts
# function gradient
set.seed(1234)
lower <- rep(-5.12, dimension)
# 10000 NA
set.seed(1234)
library(GenSA)
expected.val <- 0
absTol <- 1e-13
fn.call <- 0
out.GenSA <- GenSA(par = NULL, lower = lower, upper = upper, fn = Rastrigin,
control = list(threshold.stop = expected.val + absTol))
out.GenSA[c("value", "par", "counts")]
# $value
# [1] 0
# $par
# [1] 2.750668687e-12 -2.889218652e-12
# $counts
# [1] 196
cat("GenSA call functions", fn.call, "times.\n")
# GenSA call functions 196 times.
out.DEoptim_BFGS <- optim(par = out.DEoptim$optim$bestmem, fn = Rastrigin,
lower = lower, upper = upper, method = "L-BFGS-B")
out.DEoptim_BFGS[c("value","par")]
# $value
# [1] 0
# $par
# par1 par2
# -9.362433236e-12 1.250185258e-12
dimension <- 30
upper <- rep(5.12, dimension)
fn.call <- 0
out.GenSA <- GenSA(lower = lower, upper = upper, fn = Rastrigin,
control = list(max.time=1.9, verbose=TRUE))
out.GenSA[c("value")]
# $value
# [1] 0
# [1] "GE" "IBM" "JPM" "MSFT" "WMT"
P <- NULL
for(ticker in tickers) {
tmp <- Cl(to.monthly(eval(parse(text = ticker))))
P <- cbind(P, tmp)
}
colnames(P) <- tickers
R <- diff(log(P))
R <- R[-1,]
library("quantmod")
tickers <- c("GE", "IBM", "JPM", "MSFT", "WMT")
getSymbols(tickers, from = "2000-12-01", to = "2010-12-31")
mu <- colMeans(R)
sigma <- cov(R)
library("PerformanceAnalytics")
pContribCVaR <- ES(weights = rep(0.2, 5),
method = "gaussian", portfolio_method = "component",
mu = mu, sigma = sigma)$pct_contrib_ES
obj <- function(w) {
fn.call <<- fn.call + 1
if (sum(w) == 0) { w <- w + 1e-2 }
w <- w / sum(w)
CVaR <- ES(weights = w,
method = "gaussian", portfolio_method = "component",
mu = mu, sigma = sigma)
tmp1 <- CVaR$ES
tmp2 <- max(CVaR$pct_contrib_ES - 0.225, 0)
out <- tmp1 + 1e3 * tmp2
return(out)
}
set.seed(1234)
fn.call <- 0
sink("tmp.txt")
out.DEoptim <- DEoptim(fn = obj, lower = rep(0, 5), upper = rep(1, 5))
sink(NULL)
fn.call.DEoptim <- fn.call
out.DEoptim$optim$bestval
# [1] 0.1142884416
out.DEoptim$optim$nfeval
# [1] 402
cat("DEoptim call functions", fn.call.DEoptim, "times.\n")
# DEoptim call functions 10050 times.
out.DEoptim.fur <- optim(par = out.DEoptim$optim$bestmem, fn = obj, method = "Nelder-Mead")
out.DEoptim.fur$value
# [1] 0.1141564043
set.seed(1234)
fn.call <<- 0
out.GenSA <- GenSA(fn = obj, lower = rep(0, 5), upper = rep(1, 5),
control = list(smooth = FALSE, max.call = 3000))
fn.call.GenSA <- fn.call
out.GenSA$value
# [1] 0.1141484884
out.GenSA$counts
# [1] 3000
cat("GenSA call functions", fn.call.GenSA, "times.\n")
# GenSA call functions 3000 times.
wstar.GenSA <- out.GenSA$par
wstar.GenSA <- wstar.GenSA / sum(wstar.GenSA)
rbind(tickers, round(100 * wstar.GenSA, 2))
# [,1] [,2] [,3] [,4] [,5]
# tickers "GE" "IBM" "JPM" "MSFT" "WMT"
# "18.92" "21.23" "8.33" "15.92" "35.6"
100 * (sum(wstar.GenSA * mu) - mean(mu))
# [1] 0.03790568876
Thomson.fn <- function(x) {
fn.call <<- fn.call + 1
x <- matrix(x, ncol = 2)
y <- t(apply(x, 1, function(z) {
c(sin(z[1]) * cos(z[2]),
sin(z[1]) * sin(z[2]), cos(z[1]))}))
n <- nrow(x)
tmp <- matrix(NA, nrow = n, ncol = n)
index <- cbind(as.vector(row(tmp)), as.vector(col(tmp)))
index <- index[index[, 1] < index[, 2], , drop=F]
rdist <- apply(index, 1, function(z) {
tmp <- 1/sqrt(sum((y[z[1], ] - y[z[2], ])^2))
})
res <- sum(rdist)
return(res)
}
n.particles <- 12
lower.T <- rep(0, 2 * n.particles)
upper.T <- c(rep(pi, n.particles), rep(2 * pi, n.particles))

options(digits = 10)
set.seed(1234)
sink("tmp.txt")
fn.call <<- 0
out.DEoptim <- DEoptim(fn = Thomson.fn, lower = lower.T, upper = upper.T)
sink(NULL)
fn.call.DEoptim <- fn.call
out.DEoptim$optim[c(2, 3)]
# $bestval
# [1] 49.59590424
# $nfeval
# [1] 402
out.DEoptim_BFGS <- optim(par = out.DEoptim$optim$bestmem, fn = Thomson.fn,
lower = lower.T, upper = upper.T, method = "L-BFGS-B")
out.DEoptim_BFGS[c("value")]
# $value
# [1] 49.16525309
cat("GenSA call functions", fn.call, "times.\n")
# GenSA call functions 48240 times.
cat("DEoptim call functions", fn.call.DEoptim, "times.\n")
# DEoptim call functions 48240 times.
set.seed(1234)
fn.call <<- 0
out.GenSA <- GenSA(par = NULL, lower = lower.T, upper = upper.T,
fn = Thomson.fn, control = list(max.call = fn.call.DEoptim))
out.GenSA[c("value", "counts")]
# $value
# [1] 49.16525306
# $counts
# [1] 48240
# 1e−05 1e−07 1e−08 1e−09
# Absolute tolerance
# Successful runs%
# 0
# 20
# 40
# 60
# 80
# 100
# 120</div>
# GenSA
# DEoptim
# DEoptim_BFGS
# 88
# 57
# 35
# 22

# ∑
# −
