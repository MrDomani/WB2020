library(Ake)
x <- 5
h <- 0.1
y <- 0:10
k_b <- kern.fun(x, y, h, "discrete", "bino")
data("faithful", package = "datasets")
x <- faithful$waiting
f <- dke.fun(x, ker = "GA", 0.1)
f$C_n
# [1] 0.9888231
data("faithful", package = "datasets")
x <- faithful$waiting
f1 <- dke.fun(x, 0.1, "continuous", ker = "GA")
f2 <- dke.fun(x, 0.036, "continuous", ker = "LN")
f3 <- dke.fun(x, 0.098, "continuous", ker = "RIG")
f4 <- dke.fun(x, 0.01, "continuous", ker = "BE", a0 = 40, a1 = 100)
t <- seq(min(x), max(x), length.out = 100)

hist(x, probability = TRUE, xlab = "Waiting times (in min.)",
ylab = "Frequency", main = "", border = "gray")
lines(t, f1$fn, lty = 2, lwd = 2, col = "blue")
lines(t, f2$fn, lty = 5, lwd = 2, col = "black")
lines(t, f3$fn, lty = 1, lwd = 2, col = "green")
lines(t, f4$fn, lty = 4, lwd = 2, col = "grey")
lines(density(x, width = 12), lty = 8, lwd = 2, col = "red")
legend("topleft", c("Gamma", "Lognormal", "Reciprocal inverse Gaussian",
"Extended beta", "Gaussian"), col = c("blue", "black", "green", "grey", "red"),
lwd = 2, lty = c(2, 5, 1, 4, 8), inset = .0)
data("milk", package = "Ake")
x <- milk$week
y <- milk$yield
h <- reg.fun(x, y, "discrete", "bino", 0.1)
h
# Bandwidth h:0.1 Coef_det=0.9726
# Number of points: 35; Kernel = Binomial
# data y
# Min. : 1.0 Min. :0.0100
# 1st Qu.: 9.5 1st Qu.:0.2750
# Median :18.0 Median :0.3600
# Mean :18.0 Mean :0.3986
# 3rd Qu.:26.5 3rd Qu.:0.6150
# Max. :35.0 Max. :0.7200
# eval.points m_n
# Min. : 1.0 Min. :0.01542
# 1st Qu.: 9.5 1st Qu.:0.27681
# Median :18.0 Median :0.35065
# Mean :18.0 Mean :0.39777
# 3rd Qu.:26.5 3rd Qu.:0.60942
# Max. :35.0 Max. :0.70064

data("milk", package = "Ake")
x <- milk$week
y <- milk$yield
f <- hcvreg.fun(x, y, type_data = "discrete", ker = "triang", a = 1)
f$hcv
# [1] 1.141073
