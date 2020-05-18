names(iris)
# [1] "Sepal.Length" "Sepal.Width" "Petal.Length" "Petal.Width" "Species"
names(iris) <- c("SL", "SW", "PL", "PW", "SPP")
library(car)
some(iris, 3) # 3 random rows
# SL SW PL PW SPP
# 44 5.0 3.5 1.6 0.6 setosa
# 61 5.0 2.0 3.5 1.0 versicolor
# 118 7.7 3.8 6.7 2.2 virginica
mod.iris <- lm(cbind(SL, SW, PL, PW) ~ SPP, data=iris)
class(mod.iris)
# [1] "mlm" "lm"
manova.iris <- Anova(mod.iris)
manova.iris
# Type II MANOVA Tests: Pillai test statistic
# Df test stat approx F num Df den Df Pr(>F)
# SPP 2 1.19 53.5 8 290 <2e-16
class(manova.iris)
# [1] "Anova.mlm"
summary(manova.iris)
# Type II MANOVA Tests:
# Sum of squares and products for error:
# SL SW PL PW
# SL 38.956 13.630 24.625 5.645
# SW 13.630 16.962 8.121 4.808
# PL 24.625 8.121 27.223 6.272
# PW 5.645 4.808 6.272 6.157
# ------------------------------------------
# Term: SPP
# Sum of squares and products for the hypothesis:
# SL SW PL PW
# SL 63.21 -19.95 165.25 71.28
# SW -19.95 11.34 -57.24 -22.93
# PL 165.25 -57.24 437.10 186.77
# PW 71.28 -22.93 186.77 80.41
# Multivariate Tests: SPP
# Df test stat approx F num Df den Df Pr(>F)
# Pillai 2 1.19 53.5 8 290 <2e-16
# Wilks 2 0.02 199.1 8 288 <2e-16
# Hotelling-Lawley 2 32.48 580.5 8 286 <2e-16
# Roy 2 32.19 1167.0 4 145 <2e-16
anova(mod.iris)
linearHypothesis(mod.iris, "0.5*SPPversicolor + 0.5*SPPvirginica")
# . . .
# Multivariate Tests:
# Df test stat approx F num Df den Df Pr(>F)
# Pillai 1 0.967 1064 4 144 <2e-16
# Wilks 1 0.033 1064 4 144 <2e-16
# Hotelling-Lawley 1 29.552 1064 4 144 <2e-16
# Roy 1 29.552 1064 4 144 <2e-16
linearHypothesis(mod.iris, "SPPversicolor = SPPvirginica")
# . . .
# Multivariate Tests:
# Df test stat approx F num Df den Df Pr(>F)
# Pillai 1 0.7452 105.3 4 144 <2e-16
# Wilks 1 0.2548 105.3 4 144 <2e-16
# Hotelling-Lawley 1 2.9254 105.3 4 144 <2e-16
# Roy 1 2.9254 105.3 4 144 <2e-16
mod.iris.2 <- update(mod.iris)
coef(mod.iris.2)
# SL SW PL PW
# (Intercept) 5.8433 3.0573 3.758 1.1993
# SPPS:VV -0.8373 0.3707 -2.296 -0.9533
# SPPV:V -0.3260 -0.1020 -0.646 -0.3500
# Multivariate Tests:
# Df test stat approx F num Df den Df Pr(>F)
# Pillai 1 0.7452 105.3 4 144 <2e-16
# Wilks 1 0.2548 105.3 4 144 <2e-16
# Hotelling-Lawley 1 2.9254 105.3 4 144 <2e-16
# Roy 1 2.9254 105.3 4 144 <2e-16
C <- matrix(c(1, -0.5, -0.5, 0, 1, -1), 3, 2)
colnames(C) <- c("S:VV", "V:V")
rownames(C) <- unique(iris$SPP)
contrasts(iris$SPP) <- C
contrasts(iris$SPP)
# S:VV V:V
# setosa 1.0 0
# versicolor -0.5 1
# virginica -0.5 -1
linearHypothesis(mod.iris.2, c(0, 1, 0)) # setosa vs. versicolor & virginica
# . . .
# Multivariate Tests:
# Df test stat approx F num Df den Df Pr(>F)
# Pillai 1 0.967 1064 4 144 <2e-16
# Wilks 1 0.033 1064 4 144 <2e-16
# Hotelling-Lawley 1 29.552 1064 4 144 <2e-16
# Roy 1 29.552 1064 4 144 <2e-16
linearHypothesis(mod.iris.2, c(0, 0, 1)) # versicolor vs. virginica
# . . .
library(heplots)
hyp <- list("V:V"="SPPV:V", "S:VV"="SPPS:VV")
heplot(mod.iris.2, hypotheses=hyp, fill=c(TRUE, FALSE), col=c("red", "blue"))
Anova(mod.iris, imatrix=list(Sepal.Length=matrix(c(1, 0, 0, 0))))
# Type II Repeated Measures MANOVA Tests: Pillai test statistic
# Df test stat approx F num Df den Df Pr(>F)
# Sepal.Length 1 0.992 19327 1 147 <2e-16
# SPP:Sepal.Length 2 0.619 119 2 147 <2e-16
linearHypothesis(mod.iris, c("SPPversicolor = 0", "SPPvirginica = 0"),
P=matrix(c(1, 0, 0, 0))) # equivalent
# . . .
# Multivariate Tests:
# Df test stat approx F num Df den Df Pr(>F)
# Pillai 2 0.6187 119.3 2 147 <2e-16
# Wilks 2 0.3813 119.3 2 147 <2e-16
# Hotelling-Lawley 2 1.6226 119.3 2 147 <2e-16
# Roy 2 1.6226 119.3 2 147 <2e-16
Anova(lm(SL ~ SPP, data=iris))
# Anova Table (Type II tests)
# Response: SL
# Sum Sq Df F value Pr(>F)
# SPP 63.2 2 119 <2e-16
# Residuals 39.0 147
some(OBrienKaiser, 4)
# treatment gender pre.1 pre.2 pre.3 pre.4 pre.5 post.1 post.2 post.3 post.4
# 11 B M 3 3 4 2 3 5 4 7 5
# 12 B M 6 7 8 6 3 9 10 11 9
# 14 B F 2 2 3 1 2 5 6 7 5
# 16 B F 4 5 7 5 4 7 7 8 6
# post.5 fup.1 fup.2 fup.3 fup.4 fup.5
# 11 4 5 6 8 6 5
# 12 6 8 7 10 8 7
# 14 2 6 7 8 6 3
# 16 7 7 8 10 8 7
contrasts(OBrienKaiser$treatment)
# [,1] [,2]
# control -2 0
# A 1 -1
# B 1 1
contrasts(OBrienKaiser$gender)
# [,1]
# F 1
# M -1
xtabs(~ treatment + gender, data=OBrienKaiser)
# gender
# treatment F M
# control 2 3
# A 2 2
# B 4 3
phase <- factor(rep(c("pretest", "posttest", "followup"), each=5),
levels=c("pretest", "posttest", "followup"))
hour <- ordered(rep(1:5, 3))
idata <- data.frame(phase, hour)
idata
# phase hour
# 1 pretest 1
# 2 pretest 2
# 3 pretest 3
# . . .
# 14 followup 4
# 15 followup 5
mod.ok <- lm(cbind(pre.1, pre.2, pre.3, pre.4, pre.5,
post.1, post.2, post.3, post.4, post.5,
fup.1, fup.2, fup.3, fup.4, fup.5)
~ treatment*gender, data=OBrienKaiser)
av.ok <- Anova(mod.ok, idata=idata, idesign=~phase*hour, type=3)
av.ok

# Type III Repeated Measures MANOVA Tests: Pillai test statistic
# Df test stat approx F num Df den Df Pr(>F)
# (Intercept) 1 0.967 296.4 1 10 9.2e-09
# gender 1 0.268 3.7 1 10 0.08480
# treatment:gender 2 0.364 2.9 2 10 0.10447
# phase 1 0.814 19.6 2 9 0.00052
# treatment:phase 2 0.696 2.7 4 20 0.06211
# gender:phase 1 0.066 0.3 2 9 0.73497
# treatment:gender:phase 2 0.311 0.9 4 20 0.47215

# treatment 2 0.441 3.9 2 10 0.05471
# hour 1 0.933 24.3 4 7 0.00033
# treatment:hour 2 0.316 0.4 8 16 0.91833
# gender:hour 1 0.339 0.9 4 7 0.51298
# treatment:gender:hour 2 0.570 0.8 8 16 0.61319
# phase:hour 1 0.560 0.5 8 3 0.82027
# treatment:phase:hour 2 0.662 0.2 16 8 0.99155
# gender:phase:hour 1 0.712 0.9 8 3 0.58949
# treatment:gender:phase:hour 2 0.793 0.3 16 8 0.97237
summary(av.ok, multivariate=FALSE)
linearHypothesis(mod.ok, "(Intercept) = 0", idata=idata,
idesign=~phase*hour, iterms="hour")
# Response transformation matrix:
# hour.L hour.Q hour.C hour^4
# pre.1 -0.6325 0.5345 -3.162e-01 0.1195
# pre.2 -0.3162 -0.2673 6.325e-01 -0.4781
# . . .
# fup.5 0.6325 0.5345 3.162e-01 0.1195
# . . .
# Multivariate Tests:
# Df test stat approx F num Df den Df Pr(>F)
# Pillai 1 0.933 24.32 4 7 0.000334
# Wilks 1 0.067 24.32 4 7 0.000334
# Hotelling-Lawley 1 13.894 24.32 4 7 0.000334
# Roy 1 13.894 24.32 4 7 0.000334
Hour <- model.matrix(~ hour, data=idata)
dim(Hour)
# [1] 15 5
head(Hour, 5)
# (Intercept) hour.L hour.Q hour.C hour^4
# 1 1 -0.6325 0.5345 -3.162e-01 0.1195
# 2 1 -0.3162 -0.2673 6.325e-01 -0.4781
# 3 1 0.0000 -0.5345 -4.096e-16 0.7171
# 4 1 0.3162 -0.2673 -6.325e-01 -0.4781
# 5 1 0.6325 0.5345 3.162e-01 0.1195
linearHypothesis(mod.ok, "(Intercept) = 0", P=Hour[ , c(2:5)])
# Roy 1 5.019 50.19 1 10 0.0000336
# fup.5 0.6325
# . . .
# Multivariate Tests:
# Df test stat approx F num Df den Df Pr(>F)
# Pillai 1 0.834 50.19 1 10 0.0000336
# Wilks 1 0.166 50.19 1 10 0.0000336
# Hotelling-Lawley 1 5.019 50.19 1 10 0.0000336
# Roy 1 0.0001 0.001153 1 10 0.974
# pre.2 -0.3162 -0.2673 6.325e-01 -0.4781
# . . .
# Response transformation matrix:
# hour.L hour.Q hour.C hour^4
# pre.1 -0.6325 0.5345 -3.162e-01 0.1195
# fup.5 0.6325 0.5345 3.162e-01 0.1195
# Sum of squares and products for the hypothesis:
# hour.L hour.Q hour.C hour^4
# hour.L 0.01034 1.556 0.3672 -0.8244
# hour.Q 1.55625 234.118 55.2469 -124.0137
# hour.C 0.36724 55.247 13.0371 -29.2646
# hour^4 -0.82435 -124.014 -29.2646 65.6907
# . . .
# Multivariate Tests:
# Df test stat approx F num Df den Df Pr(>F)
# Pillai 1 0.933 24.32 4 7 0.000334
# Wilks 1 0.067 24.32 4 7 0.000334
# Hotelling-Lawley 1 13.894 24.32 4 7 0.000334
# Roy 1 13.894 24.32 4 7 0.000334
linearHypothesis(mod.ok, "(Intercept) = 0", P=Hour[ , 2, drop=FALSE]) # linear
# Response transformation matrix:
# hour.L
# pre.1 -0.6325
# pre.2 -0.3162
# . . .
# . . .
# Multivariate Tests:
# Df test stat approx F num Df den Df Pr(>F)
# Pillai 1 0.0001 0.001153 1 10 0.974
# Wilks 1 0.9999 0.001153 1 10 0.974
# Hotelling-Lawley 1 0.0001 0.001153 1 10 0.974
linearHypothesis(mod.ok, "(Intercept) = 0", P=Hour[ , 3, drop=FALSE]) # quadratic
# Response transformation matrix:
# hour.Q
# pre.1 0.5345
# pre.2 -0.2673
# . . .
# fup.5 0.5345
linearHypothesis(mod.ok, "(Intercept) = 0", P=Hour[ , c(2, 4:5)]) # all non-quadratic
# Response transformation matrix:
# hour.L hour.C hour^4
# pre.1 -0.6325 -3.162e-01 0.1195
# pre.2 -0.3162 6.325e-01 -0.4781
# . . .
# fup.5 0.6325 3.162e-01 0.1195
# . . .
# Multivariate Tests:
# Pillai 1 0.896 23.05 3 8 0.000272
# Hotelling-Lawley 1 8.644 23.05 3 8 0.000272
# Roy 1 8.644 23.05 3 8 0.000272
# Df test stat approx F num Df den Df Pr(>F)
# Wilks 1 0.104 23.05 3 8 0.000272
# 4.5 5.5 6.5 7.5
# 42</div>
# 2.0 2.5 3.0 3.5 4.0
# 23
# 99</div>
# 1 2 3 4 5 6 7
# 44</div>
# 0.5 1.0 1.5 2.0 2.5
# b
# b
# b
# b
# b
# 0
# 0
# λ
