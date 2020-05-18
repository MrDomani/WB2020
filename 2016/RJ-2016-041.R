# 2013 2014 2015 2016
# 0 10 20 30 40 50 60
# Date
# Number of Daily Downloads</div>
install.packages("tolerance")
library(tolerance)
# (
# [
# ]
# )
# (
# [
# ]
# )
# (
# [
# ]
# )
# (
# {
# [
# ]
# }
# ∩
# {
# [
# ]
# }
# )
# √
# (
# √
# )
# −

# √
# √
# =
# √
milk <- c(0.968, 0.982, 1.030, 1.003, 1.046,
1.020, 0.997, 1.010, 1.027, 1.010,
0.973, 1.000, 1.044, 0.995, 1.020,
0.993, 0.984, 0.981, 0.997, 0.992)
shapiro.test(milk)
# Shapiro-Wilk normality test
# data: milk
# W = 0.96364, p-value = 0.6188
normtol.int(x = milk, alpha = 0.05, P = 0.90, side = 1)
# alpha P x.bar 1-sided.lower 1-sided.upper
# 1 0.05 0.9 1.0036 0.9610333 1.046167
normtol.int(x = milk, alpha = 0.05, P = 0.90, side = 2, method = "EXACT", m = 50)
# alpha P x.bar 2-sided.lower 2-sided.upper
# 1 0.05 0.9 1.0036 0.9523519 1.054848
normtol.int(x = milk, alpha = 0.05, P = 0.90, side = 2, method = "OCT", m = 50)
# alpha P x.bar 2-sided.lower 2-sided.upper
# 1 0.05 0.9 1.0036 0.9471414 1.060059
K.table(n = c(10, 20), alpha = c(0.01, 0.05), P = c(0.95, 0.99),
side = 1, by.arg = "P")
# 10 20
# 0.99 3.738315 2.807866
# 0.95 2.910963 2.396002
# 10 20
# 0.99 5.073725 3.831558
# 0.95 3.981118 3.295157
# Θ
# Θ
# (
# [
# ]
# )
# Θ
# (
# [
# ]
# )
# Θ
# (
# [
# ]
# )
# Θ
# (
# {
# [
# ]
# }
# ∩
# {
# [
# ]
# }|
# Θ
# ∼ N
# =
# ∼ N
# =
# (
# )

# (
# )
# =
# (
# )
# =
# (
# )
# (
# )
# =
# √
# √
# (
# )
# (
# )
# (
# )
# −
# (
# )
# −

# √
# √
bayesnormtol.int(x = milk, alpha = 0.05, P = 0.90, side = 1,
hyper.par = list(mu.0 = 1.000, sig2.0 = 0.001,
m.0 = 20, n.0 = 20))
# alpha P 1-sided.lower 1-sided.upper
# 1 0.05 0.9 0.9551936 1.048406
bayesnormtol.int(x = milk, alpha = 0.05, P = 0.90, side = 2, method = "EXACT",
m = 50, hyper.par = list(mu.0 = 1.000, sig2.0 = 0.001,
m.0 = 20, n.0 = 20))
# alpha P 2-sided.lower 2-sided.upper
# 1 0.05 0.9 0.9453603 1.05824
bayesnormtol.int(x = milk, alpha = 0.05, P = 0.90, side = 2, method = "OCT",
m = 50, hyper.par = list(mu.0 = 1.000, sig2.0 = 0.001,
m.0 = 20, n.0 = 20))
# alpha P 2-sided.lower 2-sided.upper
# 1 0.05 0.9 0.9407625 1.062838
# (
# )
# √
# (
# )
# √
# √
# √
# (
# )
# √
# (
# )
# √
# √
lm.out <- lm(breaks ~ wool + tension, data = warpbreaks)
out <- anovatol.int(lm.out, data = warpbreaks, alpha = 0.10,
P = 0.85, side = 2, method = "OCT")
# These are 90%/85% 2-sided tolerance intervals.
out
# $wool
# mean n k 2-sided.lower 2-sided.upper
# A 31.03704 27 1.886857 9.117165 52.95691
# B 25.25926 27 1.886857 3.339387 47.17913
# $tension
# mean n k 2-sided.lower 2-sided.upper
# L 36.38889 18 1.948567 13.7521219 59.02566
# M 26.38889 18 1.948567 3.7521219 49.02566
# H 21.66667 18 1.948567 -0.9701003 44.30343
plottol(out, x = warpbreaks)
# 0
# ∗
# 0
# 0
# ∗
norm.ss(alpha = 0.05, P = 0.90, delta = 0.10, P.prime = 0.97,
side = 2, m = 50, method = "FW")
# alpha P delta P.prime n
# 1 0.05 0.9 0.1 0.97 60
# ∗
# 0
# ∗
norm.ss(alpha = 0.05, P = 0.90, side = 2, spec = c(0.990, 1.010),
method = "DIR", hyper.par = list(mu.0 = 1.004, sig2.0 = 0.001))
# alpha P delta P.prime n
# 1 0.05 0.9 5
# ∗
# 0
# 0
# 0
norm.ss(x = milk, alpha = 0.05, P = 0.90, side = 2, spec = c(0.900, 1.100),
method = "YGZO", hyper.par = list(mu.0 = 0.994, sig2.0 = 0.002))
# alpha P delta P.prime n
# 1 0.05 0.9 0.1807489 0.9733307 42
# ∗
# 0
# norm.OC(k = 4, alpha = NULL, P = c(0.90, 0.95, 0.99), n = 10:20,
# side = 2, method = "EXACT", m = 25)
# norm.OC(k = 4, alpha = c(0.01, 0.05, 0.10), P = NULL, n = 10:20,
# side = 2, method = "EXACT", m = 25)
# 0.900
# 0.950
# 0.990</div>
# 10 12 14 16 18 20
# 0.93 0.95 0.97 0.99
# Normal Tolerance Interval OC Curve for P (k=4)
# n
# P
#      1-α
# 0.900
# 0.950
# 0.990</div>
# norm.OC(k = NULL, P = c(0.90, 0.95, 0.99), alpha=c(0.01, 0.05, 0.10),
# n = 10:20, side = 2, method = "EXACT", m = 25)
# 10 12 14 16 18 20
# (0.990,0.990)
# (0.950,0.990)
# (0.900,0.990)</div>
