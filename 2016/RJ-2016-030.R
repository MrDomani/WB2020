library(multipleNCC)
fit <- wpl(Surv(agestart, agestop, dead24) ~ factor(smoking3gr) + bmi + sex,
data = CVD_Accidents, m = 1, CVD_Accidents$samplestat, weight.method = "glm",
match.var = cbind(CVD_Accidents$BMI, CVD_Accidents$sex),
match.int = c(-2, 2, 0, 0))
summary(fit)
# Endpoint 1 :
# Call:
# wpl.formula(formula = Surv(agestart, agestop, dead24) ~ factor(smoking3gr) +
# bmi + factor(sex), data = CVD_Accidents,
# samplestat = CVD_Accidents$samplestat,
# match.var = cbind(CVD_Accidents$bmi, CVD_Accidents$sex),
# match.int = c(-2, 2, 0, 0), weight.method = "glm")
# n= 566, number of events= 236
# coef exp(coef) se(coef) robust se z Pr(>|z|)
# factor(smoking3gr)2 0.47107 1.60171 0.22099 0.26057 1.808 0.07062 .
# factor(smoking3gr)3 1.34245 3.82842 0.19400 0.23424 5.731 9.98e-09 ***
# bmi 0.08051 1.08384 0.01799 0.02562 3.143 0.00167 **
# factor(sex)2 -1.22307 0.29433 0.15980 0.22475 -5.442 5.27e-08 ***
# ---
# exp(coef) exp(-coef) lower .95 upper .95
# factor(smoking3gr)2 1.6017 0.6243 0.9611 2.6692
# factor(smoking3gr)3 3.8284 0.2612 2.4190 6.0591
# bmi 1.0838 0.9226 1.0308 1.1396
# factor(sex)2 0.2943 3.3976 0.1895 0.4572
# Endpoint 2 :
# Call:
# coxph(formula = Surv(left.time.ncc, survtime.ncc, status.ncc ==
# i) ~ x + cluster(ind.no.ncc), weights = 1/p)
# n= 566, number of events= 60
# coef exp(coef) se(coef) robust se z Pr(>|z|)
# factor(smoking3gr)2 -0.61629 0.53994 0.45484 0.48347 -1.275 0.202413
# factor(smoking3gr)3 0.92343 2.51792 0.32202 0.34402 2.684 0.007270 **
# bmi 0.08383 1.08744 0.03813 0.04587 1.828 0.067619 .
# factor(sex)2 -1.42549 0.24039 0.32702 0.36888 -3.864 0.000111 ***
# ---
# exp(coef) exp(-coef) lower .95 upper .95
# factor(smoking3gr)2 0.5399 1.8520 0.2093 1.3928
# factor(smoking3gr)3 2.5179 0.3972 1.2829 4.9417
# bmi 1.0874 0.9196 0.9939 1.1897
# factor(sex)2 0.2404 4.1599 0.1167 0.4954
glmp <- GLMprob(CVD_Accidents$agestop, CVD_Accidents$samplestat,
left.time = CVD_Accidents$agestart, match.var = cbind(CVD_Accidents$sex,
CVD_Accidents$bmi), match.int = c(0, 0, -2, 2))
kmp <- KMprob(CVD_Accidents$agestop, CVD_Accidents$samplestat, 1,
left.time = CVD_Accidents$agestart, match.var = cbind(CVD_Accidents$sex,
CVD_Accidents$bmi), match.int = c(0, 0, -2, 2))
summary(1/glmp[CVD_Accidents$samplestat == 1])
# Min. 1st Qu. Median Mean 3rd Qu. Max.
# 4.575 6.545 9.201 13.240 15.080 73.180
summary(1/kmp[CVD_Accidents$samplestat == 1])
# Min. 1st Qu. Median Mean 3rd Qu. Max.
# 2.481 6.839 8.772 13.150 15.020 95.810
