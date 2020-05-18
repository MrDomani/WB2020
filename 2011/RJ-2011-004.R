library(digitize)
MonodGrowth <- function(params, M) {
with(params, rK*(M/(M1+M)))
}
MonodError <- function(params, M, y) {
with(params,
sum((MonodGrowth(params, M)-y)^2))
}
cal <- ReadAndCal('monod.jpg')
growth <- DigitData(col = 'red')
data <- Calibrate(growth, cal, 1, 8, 0.5, 1)
plot(data$x, data$y, pch=20, col='grey',
xlab = 'Nutrients concentration',
ylab = 'Divisions per hour')
points(xcal, MonodGrowth(out$set, xcal),
type = 'l', lty = 1, lwd = 2)
points(xcal, MonodGrowth(paper, xcal),
type = 'l', lty = 2)
lty = c(NA, 1, 2),
