library("fanplot")
fan0(data = th.mcmc)
library("zoo")
library("tsbugs")
# create irregular multiple time series object
th.mcmc2 <- zoo(th.mcmc, order.by = svpdx$date) #$
# plot
fan0(data = th.mcmc2, type = "interval", ln = c(0.5, 0.8, 0.95),
llab = TRUE, rcex = 0.6)
# spaghetti lines
fan(th.mcmc2, style = "spaghetti", n.spag = 20, alpha = 0.3)
# transparent fan with visible lines
fan0(th.mcmc2, ln = c(5, 20, 50, 80, 95), alpha = 0, ln.col = "darkorange", llab = TRUE)
> head(boe)
time0 time mode uncertainty skew
1 2004 2004.00 1.34 0.2249 0
2 2004 2004.25 1.60 0.3149 0
3 2004 2004.50 1.60 0.3824 0
4 2004 2004.75 1.71 0.4274 0
5 2004 2005.00 1.77 0.4499 0
6 2004 2005.25 1.68 0.4761 0
# select relevant data
y0 <- 2013
boe0 <- subset(boe, time0 == y0)
k <- nrow(boe0)
# guess work to set percentiles the boe are plotting
p <- seq(0.05, 0.95, 0.05)
p <- c(0.01, p, 0.99)
# quantiles of split-normal distribution for each probability (row) at each future
# time point (column)
cpival <- matrix(NA, nrow = length(p), ncol = k)
for (i in 1:k)
cpival[, i] <- qsplitnorm(p, mode = boe0$mode[i],
sd = boe0$uncertainty[i],
skew = boe0$skew[i]) #$
# past data
plot(cpi, type = "l", col = "tomato", lwd = 2,
xlim = c(y0 - 5, y0 + 3), ylim = c(-2, 7),
xaxt = "n", yaxt = "n", ylab = "")
# background shading during forecast period
rect(y0 - 0.25, par("usr")[3] - 1, y0 + 3, par("usr")[4] + 1,
border = "gray90", col = "gray90")
# add fan
fan(data = cpival, data.type = "values", probs = p,
start = y0, frequency = 4, anchor = cpi[time(cpi) == y0 - 0.25],
fan.col = colorRampPalette(c("tomato", "gray90")), ln = NULL, rlab = NULL)
# boe aesthetics
axis(2, at = -2:7, las = 2, tcl = 0.5, labels = FALSE)
axis(4, at = -2:7, las = 2, tcl = 0.5)
axis(1, at = 2008:2016, tcl = 0.5)
axis(1, at = seq(2008, 2016, 0.25), labels = FALSE, tcl = 0.2)
abline(h = 2) # boe cpi target
abline(v = y0 + 1.75, lty = 2) # 2 year line
# simulate future values
cpisim <- matrix(NA, nrow = 10000, ncol = k)
for (i in 1:k)
cpisim[, i] <- rsplitnorm(n = 10000, mode = boe0$mode[i],
sd = boe0$uncertainty[i],
skew = boe0$skew[i])
# truncate cpi series and plot
cpi0 <- ts(cpi[time(cpi) < 2013], start = start(cpi), frequency = frequency(cpi))
plot(cpi0, type = "l", lwd = 2, las = 1, ylab = "",
xlim = c(y0 - 5, y0 + 3.5), ylim = c(-2, 7))
# add fan
library("RColorBrewer")
fan(data = cpisim, type = "interval", probs = seq(0.01,0.99,0.01),
start = y0, frequency = 4, ln = c(50,80,95), med.ln = FALSE,
fan.col = colorRampPalette(colors = rev(brewer.pal(9, "Oranges"))))
# plot past data
plot(cpi0, type = "l", xlim = c(y0-5, y0+3), ylim = c(-2, 7), lwd = 2)
# box plots
fan(cpisim, style = "boxplot", start = y0, frequency = 4, outline = FALSE)
1982 1983 1984 1985
−2 −1 0 1</div>
L95%
L80%
L50%
U50%
U80%
U95%
L95%
L80%
L50%
U50%
U80%
U95%
M
M</div>
1982 1983 1984 1985
−2 −1 0 1</div>
5%
20%
50%
80%
95%
5%
20%
50%
80%
95%</div>
2008 2009 2010 2011 2012 2013 2014 2015 2016</div>
−2
−1
0
1
2
3
4
5
6
7





√
√
√
√
0 200 400 600 800 1000
−2 −1 0 1</div>
