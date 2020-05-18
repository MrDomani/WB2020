library("SimCorMultRes")
library("multgee")
# Loading required package: gnm
# Loading required package: VGAM
# Loading required package: stats4
# Loading required package: splines
set.seed(1)
N <- 100
clsize <- 4
ncategories <- 5
betas <- c(2, 1, 1, 2, 1.5, 1.5, 2.5, 0.5, 0, 0)
x <- rnorm(N * clsize)
cor.matrix <- toeplitz(0.56^seq(0, clsize * ncategories - 1))
for (i in 1:clsize) {
diag.index <- 1:ncategories + (i - 1) * ncategories
cor.matrix[diag.index, diag.index] <- diag(1, ncategories)
}
B <- 1000
indeGEEcoefs <- matrix(NA_real_, B, 8)
RCGEEcoefs <- matrix(NA_real_, B, 8)
for (b in 1:B) {
SimNomRes <- rmult.bcl(clsize = clsize, ncategories = ncategories,
betas = betas, xformula = ~x, cor.matrix = cor.matrix)
fitRC <- try(nomLORgee(y ~ x, id = id, repeated = time, data = SimNomRes$simdata,
LORstr = "RC", add = 0.05), silent = TRUE)
if (!inherits(fitRC, "try-error")) {
if (fitRC$convergence$conv)
RCGEEcoefs[b, ] <- coef(fitRC)
}
fitinde <- try(nomLORgee(y ~ x, id = id, repeated = time, data = SimNomRes$simdata,
LORstr = "independence"), silent = TRUE)
if (!inherits(fitinde, "try-error")) {
if (fitinde$convergence$conv)
indeGEEcoefs[b, ] <- coef(fitinde)
}
}
convergence <- c(mean(!is.na(indeGEEcoefs)), mean(!is.na(RCGEEcoefs))) * 100
convergence
# [1] 100.0 99.7
simindemean <- colMeans(indeGEEcoefs, na.rm = TRUE)
simindesd <- apply(indeGEEcoefs, 2, function(x) sd(x, na.rm = TRUE))
simRCmean <- colMeans(RCGEEcoefs, na.rm = TRUE)
simRCsd <- apply(RCGEEcoefs, 2, function(x) sd(x, na.rm = TRUE))
simindesmse <- (betas[-c(9:10)] - simindemean)^2 + simindesd^2
simRCsmse <- (betas[-c(9:10)] - simRCmean)^2 + simRCsd^2
SRE <- simindesmse/simRCsmse
rbind(simindemean, simindesd, simRCmean, simRCsd, SRE)
library("SimCorMultRes")
set.seed(123)
commonlogoddsratio <- function(N, rho, intercepts, B) {
cor.matrix <- toeplitz(c(1, rho))
x <- rep(0, 2 * N)
ans <- rep(0, B)
for (b in 1:B) {
CorOrdRes <- rmult.clm(clsize = 2, intercepts = intercepts, betas = 0,
xformula = ~x, link = "probit", cor.matrix = cor.matrix)
simdata <- data.frame(table(CorOrdRes$Ysim[, 1], CorOrdRes$Ysim[, 2]))
if (any(simdata[, 3] == 0))
simdata[, 3] <- simdata[, 3] + 0.001
colnames(simdata) <- c("x", "y", "Freq")
fit <- glm(Freq ~ x + y + as.numeric(x):as.numeric(y), family = poisson(),
data = simdata)
ans[b] <- as.numeric(coef(fit)[length(coef(fit))])
}
ans
}
N <- 1000
intercepts <- c(-3, -2, -1, 0, 1, 2, 3)
B <- 10000
rho <- seq(0.05, 0.95, 0.05)
logoddsratio <- rep(0, length(rho))
for (i in seq_along(rho)) {
simdata <- commonlogoddsratio(N, rho[i], intercepts, B)
logoddsratio[i] <- mean(simdata)
}
# There were 50 or more warnings (use warnings() to see the first 50)
eta <- 1/(2 * logoddsratio)
rhophi <- (sqrt(1 + eta^2) - eta) * 13/12
# φ
absdif <- abs(rhophi - rho)
plot(rho, absdif, xlab = expression(rho), ylab = expression(abs(rho[hat(phi)] -
rho)), xaxt = "n")
axis(1, at = seq(0.05, 0.95, 0.1), labels = seq(0.05, 0.95, 0.1))
# φ
# φ
# =
# (
# )
# 0
# (
# )
# (
# )
# (
# )
# (
# )
# (
# )
# (
# )
# (
# )
# (
# )
# (
# )
# (
# )
# (
# )
# (
# )
# φ
# 0
# =
# (
# 0
# 0
# )
# (
# 0
# 0
# )
# (
# 0
# 0
# )
# (
# 0
# 0
# )
# 0
# 0
# [
# (
# )]
# (
# )
# (
# )
library(SimCorMultRes)
set.seed(123)
N <- 5000
clsize <- 3
mprobs_1 <- c(0.1, 0.3, 0.4, 0.2)
mprobs_2 <- c(0.2, 0.2, 0.2, 0.4)
mprobs_3 <- c(0.2, 0.4, 0.3, 0.1)
cprobs_1 <- cumsum(mprobs_1[-4])
cprobs_2 <- cumsum(mprobs_2[-4])
cprobs_3 <- cumsum(mprobs_3[-4])
intercepts <- qnorm(rbind(cprobs_1, cprobs_2, cprobs_3))
x <- rep(0, clsize * N)
# =
# φ
# = φ
CommomLOR <- log(2)
eta <- 1/(2 * CommomLOR)
rhophi <- (sqrt(1 + eta^2) - eta) * 13/12
rhophi
# [1] 0.5543136
cor.matrix <- toeplitz(c(1, rhophi, rhophi))
simdata <- rmult.clm(clsize = clsize, intercepts = intercepts, betas = 0,
xformula = ~x, link = "probit", cor.matrix = cor.matrix)
t(apply(simdata$Ysim, 2, function(x) table(x)/N))
# 1 2 3 4
# [1,] 0.0970 0.2986 0.4068 0.1976
# [2,] 0.1996 0.2046 0.1978 0.3980
# [3,] 0.2068 0.3868 0.3068 0.0996
cor(simdata$rlatent)
# [,1] [,2] [,3]
# [1,] 1.0000000 0.5476759 0.5527712
# [2,] 0.5476759 1.0000000 0.5534464
# [3,] 0.5527712 0.5534464 1.0000000
