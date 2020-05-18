library(zoo)
library(xts)
library(highfrequency)
library(TTR)
library(timeDate)
library(quantmod)
library(PIN)
#Load data samples
data(sample_tdata)
data(sample_qdata)
#Match the trade and quote data
tqdata <- matchTradesQuotes(sample_tdata, sample_qdata)
x <- getTradeDirection(tqdata)
tradeDirection <- matrix(x)
buy_side <- which(tradeDirection > 0)
num_buy_side <- length(matrix(buy_side))
num_sell_side <- length(tradeDirection) - length(matrix(buy_side))
ntrades <- cbind(num_buy_side, num_sell_side)
initparams <- cbind(0.15, 0.05, 0.5, 0.5)
options(warn = -1)
param_optim <- optim(initparams, pin_likelihood, gr = NULL, ntrades)
epsi <- param_optim$par[1]
miu <- param_optim$par[2]
alph <- param_optim$par[3]
delt <- param_optim$par[4]
pin <- (alph*miu)/(alph*miu + 2*epsi)
∏
∑
∑
This content downloaded  on Tue, 12 Mar 2013 07:49:27 AM
All use subject to JSTOR Terms and Conditions</div>
αµ
