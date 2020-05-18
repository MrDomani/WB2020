install.packages("GA")
library(GA)
# optimArgs = list(method = "L-BFGS-B",
# poptim = 0.05,
# pressel = 0.5,
# control = list(fnscale = -1, maxit = 100))
library(quantmod)
myStocks <- c("AAPL", "XOM", "GOOGL", "MSFT", "GE", "JNJ", "WMT", "CVX", "PG", "WFC")
getSymbols(myStocks, src = "yahoo")
returns <- lapply(myStocks, function(s)
  monthlyReturn(eval(parse(text = s)),
  subset = "2013::2014"))
returns <- do.call(cbind,returns)
colnames(returns) <- myStocks
library(timeSeries)
plot(as.timeSeries(returns), at = "chic", minor.ticks="month",
  mar.multi = c(0.2, 5.1, 0.2, 1.1), oma.multi = c(4, 0, 4, 0),
  col = .colorwheelPalette(10), cex.lab = 0.8, cex.axis = 0.8)
title("Portfolio Returns")
nStocks <- ncol(returns) # number of portfolio assets
R <- colMeans(returns) # average monthly returns
S <- cov(returns) # covariance matrix of monthly returns
s <- sqrt(diag(S)) # volatility of monthly returns
plot(s, R, type = "n", panel.first = grid(),
  xlab = "Std. dev. monthly returns", ylab = "Average monthly returns")
text(s, R, names(R), col = .colorwheelPalette(10), font = 2)
weights <- function(w) # normalised weights
  { drop(w/sum(w)) }
ExpReturn <- function(w) # expected return
  { sum(weights(w)*R) }
VarPortfolio <- function(w) # objective function
{
w <- weights(w)
drop(w %*% S %*% w)
}
fitness <- function(w) # fitness function
{
ER <- ExpReturn(w)-0.01
penalty <- if(ER < 0) 100*ER^2 else 0
-(VarPortfolio(w) + penalty)
}
GA <- ga(type = "real-valued", fitness = fitness,
min = rep(0, nStocks), max = rep(1, nStocks), names = myStocks,
maxiter = 1000, run = 200, optim = TRUE)
summary(GA)
-----------------------------------+
# | Genetic Algorithm |
-----------------------------------+
# GA settings:
# Type = real-valued
# Population size = 50
# Number of generations = 1000
# Elitism = 2
# Crossover probability = 0.8
# Mutation probability = 0.1
# Search domain =
# AAPL XOM GOOGL MSFT GE JNJ WMT CVX PG WFC
# Min 0 0 0 0 0 0 0 0 0 0
# Max 1 1 1 1 1 1 1 1 1 1
# GA results:
# Iterations = 216
# Fitness function value = -0.00049345
# Solution =
# AAPL XOM GOOGL MSFT GE JNJ WMT CVX PG WFC
# [1,] 0.030918 0.11534 0.034683 0.52062 0 0 0.17201 0.26144 0.18096 0.98719
plot(GA)
(w <- weights(GA@solution))
# AAPL XOM GOOGL MSFT GE JNJ WMT CVX
# 0.013424 0.050081 0.015059 0.226047 0.000000 0.000000 0.074685 0.113512
# PG WFC
# 0.078572 0.428621
ExpReturn(w)
# [1] 0.016178
VarPortfolio(w)
# [1] 0.00049345
barplot(w, xlab = "Stocks", ylab = "Portfolio weights",
  cex.names = 0.7, col = .colorwheelPalette(10))
data <- data.frame(
y = c(4, 5, 4, 1, 0, 4, 3, 4, 0, 6, 3, 3, 4, 0, 2, 6, 3, 3, 5, 4, 5, 3, 1,
4, 4, 1, 5, 5, 3, 4, 2, 5, 2, 2, 3, 4, 2, 1, 3, 2, 2, 1, 1, 1, 1, 3,
0, 0, 1, 0, 1, 1, 0, 0, 3, 1, 0, 3, 2, 2, 0, 1, 1, 1, 0, 1, 0, 1, 0,
0, 0, 2, 1, 0, 0, 0, 1, 1, 0, 2, 3, 3, 1, 1, 2, 1, 1, 1, 1, 2, 4, 2,
0, 0, 0, 1, 4, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1),
year = 1851:1962,
t = 1:112)
plot(y ~ year, data = data, ylab = "Number of mine accidents/yr")
plot(cumsum(y) ~ year, data = data, type = "s",
  ylab = "Cumsum number of mine accidents/yr")
loglik1 <- function(th, data)
{
mu <- exp(th) # Poisson mean
sum(dpois(data$y, mu, log = TRUE))
}
GA1 <- ga(type = "real-valued",
fitness = loglik1, data = data,
min = log(1e-5), max = log(6), names = "th",
maxiter = 200, run = 50,
optim = TRUE)
exp(GA1@solution[1,])
# 1.7054
mean(data$y)
# [1] 1.7054
meanFun <- function(th, t)
{
tau <- th[3] # change-point parameter
th <- th[1:2] # mean-related parameters
X <- cbind(1, t >= tau) # design matrix
exp(drop(X %*% th))
}
loglik2 <- function(th, data)
{
mu <- meanFun(th, data$t) # vector of Poisson means
sum(dpois(data$y, mu, log = TRUE))
}
GA2 <- ga(type = "real-valued",
fitness = loglik2, data = data,
min = c(log(1e-5), log(1e-5), min(data$t)),
max = c(log(6), log(6), max(data$t)+1),
names = c("th1", "th2", "tau"),
maxiter = 1000, run = 200,
optim = TRUE)
summary(GA2)
-----------------------------------+
# | Genetic Algorithm |
-----------------------------------+
# GA settings:
# Type = real-valued
# Population size = 50
# Number of generations = 1000
# Elitism = 2
# Crossover probability = 0.8
# Mutation probability = 0.1
# Search domain =
# th1 th2 tau
# Min -11.5129 -11.5129 1
# Max 1.7918 1.7918 113
# GA results:
# Iterations = 318
# Fitness function value = -168.86
# Solution =
# th1 th2 tau
# [1,] 1.1306 -1.2344 41.446
(mean <- exp(cumsum(GA2@solution[1,1:2]))) # mean function parameters
# th1 th2
# 3.09756 0.90141
(tau <- GA2@solution[1,3]) # change-point
# tau
# 41.446
(tab <- data.frame(
loglik = c(GA1@fitnessValue, GA2@fitnessValue),
df = c(ncol(GA1@solution), ncol(GA2@solution)),
BIC = c(2*GA1@fitnessValue - log(nrow(data))*ncol(GA1@solution),
2*GA2@fitnessValue - log(nrow(data))*ncol(GA2@solution))))
# loglik df BIC
# 1 -203.86 1 -412.43
# 2 -168.86 3 -351.88
mu <- meanFun(GA2@solution, data$t)
col <- c("red3", "dodgerblue2")
with(data,
{ plot(t, y)
abline(v = tau, lty = 2)
lines(t[t < tau], mu[t < tau], col = col[1], lwd = 2)
lines(t[t >= tau], mu[t >= tau], col = col[2], lwd = 2)
par(new=TRUE)
plot(year, cumsum(y), type = "n", axes = FALSE, xlab = NA, ylab = NA)
axis(side = 3); mtext("Year", side = 3, line = 2.5)
})
with(data,
{ plot(t, cumsum(y), type = "s", ylab = "Cumsum number of mine accidents/yr")
abline(v = tau, lty = 2)
lines(t[t < tau], cumsum(mu)[t < tau], col = col[1], lwd = 2)
lines(t[t >= tau], cumsum(mu)[t >= tau], col = col[2], lwd = 2)
par(new=TRUE)
plot(year, cumsum(y), type = "n", axes = FALSE, xlab = NA, ylab = NA)
axis(side = 3); mtext("Year", side = 3, line = 2.5)
})
fitness <- function(x, pause = 0.1)
{
Sys.sleep(pause)
x*runif(1)
}
ncores <- c(1, 2, 4, 8, 16) # number of cores/processors
pause <- c(0.01, 0.1, 1, 2) # pause during fitness evaluation
nrep <- 10 # number of simulation replications
data(gnp, package="astsa")
plot(gnp)
decode <- function(string, bitOrders)
{
string <- split(string, rep.int(seq.int(bitOrders), times = bitOrders))
orders <- sapply(string, function(x) { binary2decimal(gray2binary(x)) })
return(unname(orders))
}
decode(c(0,1,0, 0,1, 0,0,1), bitOrders = c(3,2,3))
# [1] 3 1 1
fitness <- function(string, data, bitOrders)
{
orders <- decode(string, bitOrders)
mod <- try(Arima(data, order = orders, include.constant = TRUE, method = "ML"),
silent = TRUE)
if(inherits(mod, "try-error")) NA else -mod$bic
}
# Type = binary
# Number of islands = 4
GA <- gaisl(type = "binary", nBits = 8,
fitness = fitness, data = gnp, bitOrders = c(3,2,3),
maxiter = 1000, run = 100, popSize = 50,
numIslands = 4, migrationInterval = 20)
plot(GA)
summary(GA)
-----------------------------------+
# | Genetic Algorithm |
# | Islands Model |
-----------------------------------+
# GA settings:
# Islands pop. size = 12
# Migration rate = 0.1
# Migration interval = 20
# Elitism = 1
# Crossover probability = 0.8
# Mutation probability = 0.1
# GA results:
# Iterations = 280
# Epochs = 14
# Fitness function values = -2259.615 -2259.615 -2259.615 -2259.615
# Solutions =
# x1 x2 x3 x4 x5 x6 x7 x8
# [1,] 0 1 1 1 1 0 0 1
# [2,] 0 1 1 1 1 0 0 1
# [3,] 0 1 1 1 1 0 0 1
# [4,] 0 1 1 1 1 0 0 1
(orders <- decode(GA@solution[1,], c(3,2,3)))
# [1] 2 2 1
mod <- Arima(gnp, order = orders, include.constant = TRUE, method = "ML")
mod
# Series: gnp
# ARIMA(2,2,1)
# Coefficients:
# ar1 ar2 ma1
# 0.2799 0.1592 -0.9735
# s.e. 0.0682 0.0682 0.0143
# sigma^2 estimated as 1451: log likelihood=-1119.01
# AIC=2246.02 AICc=2246.21 BIC=2259.62
mod1 <- auto.arima(gnp, ic = "bic")
print(mod1)
# Series: gnp
# ARIMA(1,2,1)
# Coefficients:
# ar1 ma1
# 0.3243 -0.9671
# s.e. 0.0665 0.0162
# sigma^2 estimated as 1486: log likelihood=-1121.71
# AIC=2249.43 AICc=2249.54 BIC=2259.62
mod1$bic
# [1] 2259.622
mod$bic
# [1] 2259.615
# −0.8 −0.4 0.0
# AAPL
# −0.05 0.00 0.05
# XOM
# −0.5 −0.3 −0.1 0.1
# GOOGL
# −0.05 0.05 0.15
# MSFT
# −0.10 0.00 0.05 0.10
# GE
# −0.05 0.00 0.05
# JNJ
# −0.05 0.05 0.15
# WMT
# −0.10 0.00 0.05
# CVX
# −0.05 0.00 0.05 0.10
# PG
# −0.06 −0.02 0.02 0.06
# WFC</div>
# 0.00
# 0.25
# 0.50
# 0.75
# 1.00
# 2.5 5.0 7.5 10.0
# Fitness function
# Probability of selection
# pressel
# 0
# 0.2
# 0.5
# 0.9
# 1</div>
# AAPL
# XOM
# GOOGL
# MSFT
# GE
# JNJ
# WMT
# CVX
# PG
# WFC</div>
# ∑
# ∑
# ∑
# 0
# √
# ● ● ● ● ● ● ●</div>
# ●
# −0.02 −0.01 0.00 0.01 0.02
# Average monthly returns</div>
