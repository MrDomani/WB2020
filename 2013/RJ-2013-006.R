# load the package used throughout this article
library("ftsa")
# Fit and plot functional principal components
# order specifies the number of principal components
# h specifies the forecast horizon
plot(forecast(ftsm(Australiasmoothfertility, order=2), h=20), "components")
# Plot the historical data in gray
plot(Australiasmoothfertility, col = gray(0.8), xlab = "Age",
ylab = "Count of live birth (per 1,000 females)",
main = "Forecasted fertility rates (2007-2026)")
# Plot the forecasts in rainbow color for Fig. 4(a)
plot(forecast(ftsm(Australiasmoothfertility, order = 2), h = 20), add = TRUE)
legend("topright", c("2007", "2026"), col = c("red", "blue"), lty = 1)
plot(Australiasmoothfertility, col = gray(0.8), xlab = "Age",
ylab = "Count of live birth (per 1,000 females)",
main = "Forecasted fertility rates (2007-2026)")
# Plot the forecasts in rainbow color for Fig. 4(b)
plot(ftsmiterativeforecasts(Australiasmoothfertility, components = 2, iteration = 20),
add = TRUE)
legend("topright", c("2007", "2026"), col = c("red", "blue"), lty = 1)
# Plot the point forecast
aus = forecast(ftsm(Australiasmoothfertility, order = 2), h = 1)
plot(aus, ylim = c(0, 140))
# Plot the lower and upper bounds
lines(aus$lower, col = 2); lines(aus$upper, col = 2)
# Add a legend to the plot
legend("topright", c("Point forecasts", "Interval forecasts"), col = c(1, 2), lty = 1,
cex = 0.9)
# Name obs to represent partially observed data,
obs <- ElNino2011smooth$y[1:5,62]
# Name history to represent historical data,
history <- ElNino2011smooth
# Name fore to represent the forecasting period
fore <- ElNino2011smooth$y[6:12,62]
int <- dynupdate(data = history, newdata = obs, holdoutdata = fore,
method = "block", interval = TRUE, level = 80)
bmupdate <- dynupdate(data = history, newdata = obs, holdoutdata = fore,
method = "block", value = TRUE)
plot(6:12, fore, type = "l", ylim = c(19, 26), xlab = "Month",
ylab = "Sea surface temperature")
lines(6:12, bmupdate, col = 4)
lines(6:12, int$low$y, col = 2); lines(6:12, int$up$y, col = 2)
legend("topright", c("True observations", "Point forecasts", "Interval forecasts"),
col=c(1, 4, 2), lty = 1, cex = 0.8)
history <- ElNino2011smooth
obs <- ElNino2011smooth$y[1:5, 62]
fore <- ElNino2011smooth$y[6:12, 62]
# Implement the ridge and PLS regressions,
# The tuning parameter lambda=100 as an
# illustration
rrmethod <- dynupdate(history, newdata = obs, holdoutdata = fore, method = "ridge",
value = TRUE, lambda = 100, level = 80)
plsmethod <- dynupdate(history, newdata = obs, holdoutdata = fore, method = "pls",
value = TRUE, lambda = 100, level = 80)
plsmethodint <- dynupdate(history, newdata = obs, holdoutdata = fore, method = "pls",
interval = TRUE, lambda = 100, level = 80)
# Plot the true observations for forecasting period
plot(6:12, fore, type = "l", ylim = c(19, 26), xlab = "Month",
ylab = "Sea surface temperature")
# Plot point forecasts obtained by RR and PLS
lines(6:12, plsmethod, col = 4); lines(6:12, rrmethod, col = "purple")
# Plot interval forecasts obtained by PLS
lines(6:12, plsmethodint$low$y, col = 2); lines(6:12, plsmethodint$up$y, col = 2)
legend("topright",c("True observations", "PLS point forecasts", "RR point forecasts",
"PLS interval forecasts"), col = c(1, 4, "purple", 2), lty = 1, cex = 0.8)
H.Shang@soton.ac.uk
b
b
b
b
b
b
b
R
b
b
b
b
b
b
b
b
b
b
p
b
b
b
b
S
b
b
b
b
b
b
b
b
b
b
b
b
b
∞
∑
∑
∑
∑
∑
∑
∑
∑
∑
∑
>
>
>
>
∗
∗
>
>
0 50 100 150</div>
Mean</div>
0.0 0.1 0.2 0.3 0.4</div>
−400 −200 0 200
−0.1 0.0 0.1 0.2 0.3</div>
−100 0 50 150 250</div>
µ
