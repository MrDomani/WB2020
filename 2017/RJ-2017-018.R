install.packages("coxphMIC")
# coxphMIC(formula = Surv(time, status) ~ ., data, method.beta0 = "MPLE",
# beta0 = NULL, theta0 = 1, method = "BIC", lambda0 = NULL, a0 = NULL,
# scale.x = TRUE, maxit.global = 300, maxit.local = 100,
# rounding.digits = 4, zero = sqrt(.Machine$double.eps),
# compute.se.gamma = TRUE, compute.se.beta = TRUE,
# CI.gamma = TRUE, conf.level = 0.95,
# details = FALSE)
library(survival); data(pbc);
dat <- pbc; dim(dat);
# [1] 418 20
dat$status <- ifelse(pbc$status == 2, 1, 0)
dat$sex <- ifelse(pbc$sex == "f", 1, 0)
dat <- na.omit(dat);
dim(dat);
# [1] 276 20
head(dat)
# id time status trt age sex ascites hepato spiders edema bili chol
# 1 1 400 1 1 58.76523 1 1 1 1 1.0 14.5 261
# 2 2 4500 0 1 56.44627 1 0 1 1 0.0 1.1 302
# 3 3 1012 1 1 70.07255 0 0 0 0 0.5 1.4 176
# 4 4 1925 1 1 54.74059 1 0 1 1 0.5 1.8 244
# 5 5 1504 0 2 38.10541 1 0 1 1 0.0 3.4 279
# 7 7 1832 0 2 55.53457 1 0 1 0 0.0 1.0 322
# albumin copper alk.phos ast trig platelet protime stage
# 1 2.60 156 1718.0 137.95 172 190 12.2 4
# 2 4.14 54 7394.8 113.52 88 221 10.6 3
# 3 3.48 210 516.0 96.10 55 151 12.0 4
# 4 2.54 64 6121.8 60.63 92 183 10.3 4
# 5 3.53 143 671.0 113.15 72 136 10.9 3
# 7 4.09 52 824.0 60.45 213 204 9.7 3
fit.mic <- coxphMIC(formula = Surv(time, status)~.-id, data = dat, CI.gamma = FALSE)
names(fit.mic)
# [1] "opt.global" "opt.local" "min.Q" "gamma" "beta" "VCOV.gamma"
# [7] "se.gamma" "se.beta" "BIC" "result" "call"
print(fit.mic)
# beta0 gamma se.gamma z.stat p.value beta.MIC se.beta.MIC
# trt -0.0622 0.0000 0.1071 0.0000 1.0000 0.0000 NA
# age 0.3041 0.3309 0.1219 2.7138 0.0067 0.3309 0.1074
# sex -0.1204 0.0000 0.1086 -0.0002 0.9998 0.0000 NA
# ascites 0.0224 0.0000 0.0991 0.0000 1.0000 0.0000 NA
# hepato 0.0128 0.0000 0.1259 0.0000 1.0000 0.0000 NA
# spiders 0.0460 0.0000 0.1118 -0.0001 1.0000 0.0000 NA
# edema 0.2733 0.2224 0.1066 2.0861 0.0370 0.2224 0.0939
# bili 0.3681 0.3909 0.1142 3.4237 0.0006 0.3909 0.0890
# chol 0.1155 0.0000 0.1181 0.0002 0.9999 0.0000 NA
# albumin -0.2999 -0.2901 0.1248 -2.3239 0.0201 -0.2901 0.1103
# copper 0.2198 0.2518 0.1050 2.3986 0.0165 0.2518 0.0868
# alk.phos 0.0022 0.0000 0.0837 0.0000 1.0000 0.0000 NA
# ast 0.2308 0.2484 0.1128 2.2023 0.0276 0.2484 0.1025
# trig -0.0637 0.0000 0.0858 0.0000 1.0000 0.0000 NA
# platelet 0.0840 0.0000 0.1129 0.0000 1.0000 0.0000 NA
# protime 0.2344 0.2293 0.1046 2.1917 0.0284 0.2293 0.1022
# stage 0.3881 0.3692 0.1476 2.5007 0.0124 0.3692 0.1243
plot(fit.mic, conf.level = 0.95)
fit0.mic <- coxphMIC(formula = Surv(time, status)~.-id, data = dat,
method = "BIC", scale.x = TRUE, method.beta0 = "zero")
c(fit.mic$min.Q, fit0.mic$min.Q)
# [1] 974.3340 978.1232
beta.MPLE <- fit.mic$result[, 1]
beta0 <- sign(beta.MPLE)*sign(abs(beta.MPLE) > .06);
cbind(beta.MPLE, beta0)
# beta.MPLE beta0
# [1,] -0.0622 -1
# [2,] 0.3041 1
# [3,] -0.1204 -1
# [4,] 0.0224 0
# [5,] 0.0128 0
# [6,] 0.0460 0
# [7,] 0.2733 1
# [8,] 0.3681 1
# [9,] 0.1155 1
# [10,] -0.2999 -1
# [11,] 0.2198 1
# [12,] 0.0022 0
# [13,] 0.2308 1
# [14,] -0.0637 -1
# [15,] 0.0840 1
# [16,] 0.2344 1
# [17,] 0.3881 1
fit1.mic <- coxphMIC(formula = Surv(time, status)~.-id, data = dat,
method = "BIC", scale.x = TRUE, method.beta0 = "user-supplied", beta0 = beta0)
c(fit.mic$min.Q, fit0.mic$min.Q, fit1.mic$min.Q)
# [1] 974.3340 978.1232 979.6826
set.seed(818)
n <- NROW(dat); n0 <- sum(dat$status == 1)
A0 <- 10:200
p <- NCOL(dat)-3
BETA <- matrix(0, nrow = length(A0), ncol = p) # USE ARRAY
for (j in 1:length(A0)){
su.fit <- coxphMIC(formula = Surv(time, status)~.-id, data = dat, a0 = A0[j],
method = "BIC", scale.x = TRUE)
BETA[j, ] <- su.fit$beta
}
BETA <- as.data.frame(BETA)
colnames(BETA) <- colnames(dat)[-(1:3)]
row.names(BETA) <- A0
head(BETA, n = 5)
# trt age sex ascites hepato spiders edema bili chol albumin copper alk.phos
# 10 0 0.2983 0 0 0 0 0.2024 0.4135 0 -0.2799 0.2495 0
# 11 0 0.2987 0 0 0 0 0.2015 0.4159 0 -0.2799 0.2491 0
# 12 0 0.2992 0 0 0 0 0.2006 0.4181 0 -0.2799 0.2487 0
# 13 0 0.3000 0 0 0 0 0.1998 0.4200 0 -0.2801 0.2482 0
# 14 0 0.3009 0 0 0 0 0.1992 0.4216 0 -0.2804 0.2478 0
# ast trig platelet protime stage
# 10 0.1937 0 0 0.1912 0.3583
# 11 0.1924 0 0 0.1895 0.3612
# 12 0.1914 0 0 0.1878 0.3642
# 13 0.1906 0 0 0.1862 0.3672
# 14 0.1901 0 0 0.1847 0.3701
par(mar = rep(5,4), mfrow = c(1,1))
x.min <- min(A0); x.max <- max(A0)
plot(x = c(x.min, x.max), y = c(min(BETA), max(BETA)), type = "n",
xlab = "a", cex.lab = 1.2, las = 1, ylab = expression(tilde(beta)))
for (j in 1:ncol(BETA)){
lines(x = A0, y = BETA[,j], col = "red", lty = 1, lwd = 1)
points(x = A0, y = BETA[,j], col = "red", pch = j, cex = .3)
vname <- colnames(BETA)[j]
if (abs(BETA[nrow(BETA),j]) > .00001) {
# # text(x.max+5, BETA[nrow(BETA),j], labels = vname, cex = 1, col = "blue")
mtext(text = vname, side = 4, line = 0.5, at = BETA[nrow(BETA),j], las = 1,
cex = 1, col = "blue", font = 1)
}
}
abline(h = 0, col = "gray25", lwd = 2)
abline(v = n0, col = "gray45", lwd = 1.5)
text(n0+5, -0.2, expression(paste("a = ", n[0], " = ", 111, sep = "")), cex = 1.2,
col = "gray35")
# −2 −1 0 1 2
# 0.0 0.2 0.4 0.6 0.8 1.0</div>
# w(β)</div>
# a=1</div>
# −1.5 −1.0 −0.5 0.0 0.5 1.0 1.5
# −1.5 −0.5 0.0 0.5 1.0 1.5</div>
# a=1</div>
# −2 −1 0 1 2
# 0.0 0.2 0.4 0.6 0.8 1.0</div>
# w(γ)</div>
# a=1</div>
# ∑
# ∑
# ∑
# ∑
# ∑
# ∑
# ∑
# b
# e
# e
# e
# b
# e
# b
# 0
# 0
# 0
# β
# γ
# β
# γ
# β</div>
