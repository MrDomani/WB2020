set.seed(1)
library("cmvnorm", quietly = TRUE)
cm <- c(1, 1i)
cv <- matrix(c(2, 1i, -1i, 2), 2, 2)
(z <- rcmvnorm(6, mean = cm, sigma = cv))
# [,1] [,2]
# [1,] 0.9680986+0.5525419i 0.0165969+2.9770976i
# [2,] 0.2044744-1.4994889i 1.8320765+0.8271259i
# [3,] 1.0739973+0.2279914i -0.7967020+0.1736071i
# [4,] 1.3171073-0.9843313i 0.9257146+0.5524913i
# [5,] 1.3537303-0.8086236i -0.0571337+0.3935375i
# [6,] 2.9751506-0.1729231i 0.3958585+3.3128439i
dcmvnorm(z, cm, cv)
# [1] 5.103754e-04 1.809636e-05 2.981718e-03 1.172242e-03 4.466836e-03 6.803356e-07
helper <- function(x) c(x[1] + 1i * x[2], x[3] + 1i * x[4])
objective <- function(x, cv)
-sum(dcmvnorm(z, mean = helper(x), sigma = cv, log = TRUE))
helper(optim(c(1, 0, 1, 0), objective, cv = cv)$par)
# [1] 1.315409-0.447863i 0.385704+1.372762i
colMeans(z)
# [1] 1.315426-0.447472i 0.386068+1.372784i
val <- latin.hypercube(40, 2, names = c("a", "b"), complex = TRUE)
head(val)
# a b
# [1,] 0.7375+0.2375i 0.2375+0.7125i
# [2,] 0.6875+0.5875i 0.1375+0.3375i
# [3,] 0.4625+0.5375i 0.9875+0.5875i
# [4,] 0.7875+0.0625i 0.0625+0.7875i
# [5,] 0.3875+0.0375i 0.5875+0.7625i
# [6,] 0.2125+0.5625i 0.7625+0.9625i
true_scales <- c(1, 2)
true_means <- c(1, 1i)
A <- corr_complex(val, means = true_means, scales = true_scales)
round(A[1:4, 1:4], 2)
# [,1] [,2] [,3] [,4]
# [1,] 1.00+0.00i 0.59-0.27i 0.25-0.10i 0.89+0.11i
# [2,] 0.59+0.27i 1.00+0.00i 0.20+0.00i 0.42+0.26i
# [3,] 0.25+0.10i 0.20+0.00i 1.00+0.00i 0.10+0.06i
# [4,] 0.89-0.11i 0.42-0.26i 0.10-0.06i 1.00+0.00i
all(eigen(A)$values > 0)
# [1] TRUE
true_beta <- c(1, 1+1i, 1-2i)
d <- drop(rcmvnorm(n = 1, mean = regressor.multi(val) %*% true_beta, sigma = A))
head(d)
# [1] 3.212719+1.594901i 1.874278+0.345517i 3.008503-0.767618i 3.766526+2.071882i
# [5] 3.712913+0.800983i 3.944167+0.924833i
betahat.fun(val, solve(A), d)
# const a b
# 0.593632-0.0128655i 0.843608+1.0920437i 1.140372-2.5053751i
interpolant.quick.complex(rbind(c(0.5, 0.3+0.1i)), d, val, solve(A),
scales = true_scales, means = true_means, give.Z = TRUE)
# $mstar.star
# [1] 1.706402-1.008601i
# $Z
# [1] 0.203295
# $prior
# [1] 1.608085-0.104419i
library("elliptic")
valsigma <- 2 + 1i + round(latin.hypercube(30, 3,
names = c("z", "g1", "g2"), complex = TRUE)/4, 2)
head(valsigma)
# z g1 g2
# [1,] 2.17+1.15i 2.09+1.22i 2.21+1.09i
# [2,] 2.11+1.01i 2.04+1.03i 2.25+1.15i
# [3,] 2.10+1.04i 2.15+1.00i 2.22+1.20i
# [4,] 2.13+1.10i 2.24+1.21i 2.01+1.16i
# [5,] 2.20+1.00i 2.20+1.06i 2.08+1.08i
# [6,] 2.05+1.10i 2.19+1.04i 2.11+1.03i
dsigma <- apply(valsigma, 1, function(u) sigma(u[1], g = u[2:3]))
scales.likelihood.complex(scales = c(1, 1, 2), means = c(1, 1+1i, 1-2i),
zold = valsigma, z = dsigma, give_log = TRUE)
# [1] 144.5415
scales <- function(x) exp(x[c(1, 2, 2)])
means <- function(x) x[c(3, 4, 4)] + 1i * x[c(5, 6, 6)]
objective <- function(x, valsigma, dsigma)
-scales.likelihood.complex(scales = scales(x), means = means(x),
zold = valsigma, z = dsigma)
start <- c(-0.538, -5.668, 0.6633, -0.0084, -1.73, -0.028)
jj <- optim(start, objective, valsigma = valsigma, dsigma = dsigma,
method = "SANN", control = list(maxit = 100))
(u <- jj$par)
# [1] -0.5380 -5.6680 0.6633 -0.0084 -1.7300 -0.0280
Asigma <- corr_complex(z1 = valsigma, scales = scales(u), means = means(u))
(D <- 2 * (LC - LR))
interpolant.quick.complex(rbind(c(2+1i, 2+1i, 2+1i)), zold = valsigma,
d = dsigma, Ainv = solve(Asigma), scales = scales(u), means = means(u))
# [1] 3.078956+1.259993i
sigma(2 + 1i, g = c(2 + 1i, 2 + 1i))
# [1] 3.078255+1.257819i
ob2 <- function(x, valsigma, dsigma)
-scales.likelihood.complex(scales = scales(x), means = c(0, 0, 0),
zold = valsigma, z = dsigma)
jjr <- optim(u[1:2], ob2, method = "SANN", control = list(maxit = 1000),
valsigma = valsigma, dsigma = dsigma)
(ur <- jjr$par)
# [1] 0.2136577 -4.2640825
LR <- scales.likelihood.complex(scales = scales(ur), means = c(0, 0, 0),
zold = valsigma, z = dsigma)
LC <- scales.likelihood.complex(scales = scales(u), means = means(u),
zold = valsigma, z = dsigma)
(D <- 2 * (LC - LR))
# [1] 22.17611
# hankin.robin@gmail.com
# ∗
# ∗
# |
# ∗
# ∗
# ∗
# ∗
# ∗
# ∗
# ∗
# 0
# −
# ∑
# ∑
# ∑
# ∑
# ∑
# ∑
# p
# Z
# Z
# Z
# Z
# R
# R
# R
# πΓ
