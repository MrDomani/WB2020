library(pseval)
set.seed(1492)
fakedata <- generate_example_data(n = 800)
head(fakedata)
## Z BIP CPV BSM S.obs time.obs
## 1 0 0.3353179 1.4851399 0.45961614 0.35268095 0.3301972
## 2 0 1.4536863 2.6379400 1.39591042 1.46688905 0.1195136
## 3 0 -0.7243934 NA -0.62723499 -0.73190763 0.2631222
## 4 0 -0.1183592 0.9421504 0.07738308 -0.01833409 0.1373458
## 5 0 -0.2352566 NA -0.14971448 -0.18470242 0.8543703
## 6 0 -0.7782851 0.1159434 -0.65721609 -0.66313714 0.2200481
## event.obs Y.obs S.obs.cat BIP.cat
## 1 0 0 (-0.198,0.503] (0.0574,0.766]
## 2 1 0 (1.36, Inf] (0.766, Inf]
## 3 1 1 (-Inf,-0.198] (-Inf,-0.678]
## 4 1 0 (-0.198,0.503] (-0.678,0.0574]
## 5 1 1 (-0.198,0.503] (-0.678,0.0574]
## 6 1 0 (-Inf,-0.198] (-Inf,-0.678]
binary.ps <- psdesign(data = fakedata, Z = Z, Y = Y.obs, S = S.obs, BIP = BIP)
binary.ps
## Augmented data frame: 800 obs. by 6 variables.
## Z Y S.1 S.0 cdfweights BIP
## 1 0 0 NA 0.3527 1 0.335
## 2 0 0 NA 1.4669 1 1.454
## 3 0 1 NA -0.7319 1 -0.724
## 4 0 0 NA -0.0183 1 -0.118
## 5 0 1 NA -0.1847 1 -0.235
## 6 0 0 NA -0.6631 1 -0.778
##
## Empirical TE: 0.526
##
## Mapped variables:
## Z -> Z
## Y -> Y.obs
## S -> S.obs
## BIP -> BIP
##
## Integration models:
## None present, see ?add_integration for information on integration models.
##
## Risk models:
## None present, see ?add_riskmodel for information on risk models.
## No estimates present, see ?ps_estimate.
## No bootstraps present, see ?ps_bootstrap.
fakedata.cc <- fakedata
missdex <- sample((1:nrow(fakedata.cc))[fakedata.cc$Y.obs == 0],
size = floor(sum(fakedata.cc$Y.obs == 0) * .8))
fakedata.cc[missdex, ]$S.obs <- NA
fakedata.cc$weights <- ifelse(fakedata.cc$Y.obs == 1, 1, .2)
binary.cc <- psdesign(data = fakedata.cc, Z = Z, Y = Y.obs, S = S.obs,
BIP = BIP, weights = weights)
surv.ps <- psdesign(data = fakedata, Z = Z, Y = Surv(time.obs, event.obs), S = S.obs,
BIP = BIP, CPV = CPV, BSM = BSM)
## Warning in psdesign(data = fakedata, Z = Z, Y = Surv(time.obs,
## event.obs), : tau missing in psdesign: assuming that the
## surrogate S was measured at time 0.
binary.ps <- binary.ps + integrate_parametric(S.1 ~ BIP)
binary.ps
## Augmented data frame: 800 obs. by 6 variables.
## Z Y S.1 S.0 cdfweights BIP
## 1 0 0 NA 0.3527 1 0.335
## 2 0 0 NA 1.4669 1 1.454
## 3 0 1 NA -0.7319 1 -0.724
## 4 0 0 NA -0.0183 1 -0.118
## 5 0 1 NA -0.1847 1 -0.235
## 6 0 0 NA -0.6631 1 -0.778
##
## Empirical TE: 0.526
##
## Mapped variables:
## Z -> Z
## Y -> Y.obs
## S -> S.obs
## BIP -> BIP
##
## Integration models:
## integration model for S.1 :
## integrate_parametric(formula = S.1 ~ BIP )
##
## Risk models:
## None present, see ?add_riskmodel for information on risk models.
## No estimates present, see ?ps_estimate.
## No bootstraps present, see ?ps_bootstrap.
binary.ps + integrate_parametric(S.0 ~ BIP)
## Augmented data frame: 800 obs. by 6 variables.
## Z Y S.1 S.0 cdfweights BIP
## 1 0 0 NA 0.3527 1 0.335
## 2 0 0 NA 1.4669 1 1.454
## 3 0 1 NA -0.7319 1 -0.724
## 4 0 0 NA -0.0183 1 -0.118
## 5 0 1 NA -0.1847 1 -0.235
## 6 0 0 NA -0.6631 1 -0.778
##
## Empirical TE: 0.526
##
## Mapped variables:
## Z -> Z
## Y -> Y.obs
## S -> S.obs
## BIP -> BIP
##
## Integration models:
## integration model for S.1 :
## integrate_parametric(formula = S.1 ~ BIP )
## integration model for S.0 :
## integrate_parametric(formula = S.0 ~ BIP )
##
## Risk models:
## None present, see ?add_riskmodel for information on risk models.
## No estimates present, see ?ps_estimate.
## No bootstraps present, see ?ps_bootstrap.
library(splines)
binary.ps + integrate_parametric(S.1 ~ BIP^2)
binary.ps + integrate_parametric(S.1 ~ bs(BIP, df = 3))
binary.ps + integrate_parametric(S.1 ~ BIP + BSM + BSM^2)
binary.ps <- psdesign(data = fakedata, Z = Z, Y = Y.obs, S = S.obs, BIP = BIP,
BSM = BSM, age = age)
binary.ps + integrate_parametric(S.1 ~ BIP + age)
binary.ps <- binary.ps + risk_binary(model = Y ~ S.1 * Z, D = 50, risk = risk.logit)
binary.ps
## Augmented data frame: 800 obs. by 6 variables.
## Z Y S.1 S.0 cdfweights BIP
## 1 0 0 NA 0.3527 1 0.335
## 2 0 0 NA 1.4669 1 1.454
## 3 0 1 NA -0.7319 1 -0.724
## 4 0 0 NA -0.0183 1 -0.118
## 5 0 1 NA -0.1847 1 -0.235
## 6 0 0 NA -0.6631 1 -0.778
##
## Empirical TE: 0.526
##
## Mapped variables:
## Z -> Z
## Y -> Y.obs
## S -> S.obs
## BIP -> BIP
##
## Integration models:
## integration model for S.1 :
## integrate_parametric(formula = S.1 ~ BIP )
##
## Risk models:
## risk_binary(model = Y ~ S.1 * Z, D = 50, risk = risk.logit )
##
## No estimates present, see ?ps_estimate.
## No bootstraps present, see ?ps_bootstrap.
binary.est <- binary.ps + ps_estimate(method = "BFGS")
binary.boot <- binary.est + ps_bootstrap(n.boots = 500, progress.bar = FALSE,
start = binary.est$estimates$par, method = "BFGS")
binary.boot
## Augmented data frame: 800 obs. by 6 variables.
## Z Y S.1 S.0 cdfweights BIP
## 1 0 0 NA 0.3527 1 0.335
## 2 0 0 NA 1.4669 1 1.454
## 3 0 1 NA -0.7319 1 -0.724
## 4 0 0 NA -0.0183 1 -0.118
## 5 0 1 NA -0.1847 1 -0.235
## 6 0 0 NA -0.6631 1 -0.778
##
## Empirical TE: 0.526
##
## Mapped variables:
## Z -> Z
## Y -> Y.obs
## S -> S.obs
## BIP -> BIP
##
## Integration models:
## integration model for S.1 :
## integrate_parametric(formula = S.1 ~ BIP )
##
## Risk models:
## risk_binary(model = Y ~ S.1 * Z, D = 50, risk = risk.logit )
##
## Estimated parameters:
## (Intercept) S.1 Z S.1:Z
## -0.920 -0.028 -0.220 -1.133
## Convergence: TRUE
##
## Bootstrap replicates:
## Estimate boot.se lower.CL.2.5. upper.CL.97.5.
## (Intercept) -0.920 0.182 -1.286 -0.580
## S.1 -0.028 0.128 -0.276 0.220
## Z -0.220 0.250 -0.697 0.277
## S.1:Z -1.133 0.214 -1.581 -0.780
## p.value
## (Intercept) 4.02e-07
## S.1 8.27e-01
## Z 3.80e-01
## S.1:Z 1.29e-07
##
## Out of 500 bootstraps, 500 converged ( 100 %)
##
## Test for wide effect modification on 1 degree of freedom. 2-sided p value < .0001
binary.est <- psdesign(data = fakedata, Z = Z, Y = Y.obs, S = S.obs, BIP = BIP) +
integrate_parametric(S.1 ~ BIP) +
risk_binary(model = Y ~ S.1 * Z, D = 50, risk = risk.logit) +
ps_estimate(method = "BFGS")
calc_STG(binary.boot, progress.bar = FALSE)
## $obsSTG
## [1] 0.3397774
##
## $bootstraps
## STG.boot.se STG.lower.CL.2.5 STG.upper.CL.97.5
## V1 0.1243311 0.1573031 0.6382418
smary <- summary(binary.boot)
## Augmented data frame: 800 obs. by 6 variables.
## Z Y S.1 S.0 cdfweights BIP
## 1 0 0 NA 0.3527 1 0.335
## 2 0 0 NA 1.4669 1 1.454
## 3 0 1 NA -0.7319 1 -0.724
## 4 0 0 NA -0.0183 1 -0.118
## 5 0 1 NA -0.1847 1 -0.235
## 6 0 0 NA -0.6631 1 -0.778
##
## Empirical TE: 0.526
##
## Mapped variables:
## Z -> Z
## Y -> Y.obs
## S -> S.obs
## BIP -> BIP
##
## Integration models:
## integration model for S.1 :
## integrate_parametric(formula = S.1 ~ BIP )
##
## Risk models:
## risk_binary(model = Y ~ S.1 * Z, D = 50, risk = risk.logit )
##
## Estimated parameters:
## (Intercept) S.1 Z S.1:Z
## -0.920 -0.028 -0.220 -1.133
## Convergence: TRUE
##
## Bootstrap replicates:
## Estimate boot.se lower.CL.2.5. upper.CL.97.5.
## (Intercept) -0.920 0.182 -1.286 -0.580
## S.1 -0.028 0.128 -0.276 0.220
## Z -0.220 0.250 -0.697 0.277
## S.1:Z -1.133 0.214 -1.581 -0.780
## p.value
## (Intercept) 4.02e-07
## S.1 8.27e-01
## Z 3.80e-01
## S.1:Z 1.29e-07
##
## Out of 500 bootstraps, 500 converged ( 100 %)
##
## Test for wide effect modification on 1 degree of freedom. 2-sided p value < .0001
##
## Treatment Efficacy:
## empirical marginal model
## 0.526 0.526 0.539
## Model-based average TE is 2.3 % different from the empirical and 2.3 % different
## from the marginal.
head(calc_risk(binary.boot, contrast = "TE", n.samps = 20), 3)
## S.1 Y R0 R1 Y.boot.se
## V1 -2.2756987 -1.7437221 0.2980453 0.8177536 1.1622104
## V2 -1.4262708 -1.1360482 0.2930970 0.6260692 0.6957994
## V3 -0.5973759 -0.3532827 0.2883149 0.3901715 0.3328793
## Y.upper.CL.0.95 Y.lower.CL.0.95 R0.boot.se R0.upper.CL.0.95
## V1 -0.30455106 -3.780389 0.08994238 0.4766278
## V2 -0.05541741 -2.556452 0.06901664 0.4331237
## V3 0.29675970 -1.275280 0.04941685 0.4007098
## R0.lower.CL.0.95 R1.boot.se R1.upper.CL.0.95 R1.lower.CL.0.95
## V1 0.1188411 0.06911306 0.9368827 0.6720409
## V2 0.1468385 0.07592515 0.7762517 0.4768403
## V3 0.1734875 0.05079824 0.5248493 0.2834909
head(calc_risk(binary.boot, contrast = function(R0, R1) 1 - R1/R0, n.samps = 20), 3)
## S.1 Y R0 R1 Y.boot.se
## V1 -0.97417991 -0.71327775 0.2904830 0.4976780 0.4840781
## V2 -0.11875337 0.05966359 0.2855748 0.2685364 0.1882398
## V3 -0.09236484 0.08009338 0.2854242 0.2625636 0.1820450
## Y.upper.CL.0.95 Y.lower.CL.0.95 R0.boot.se R0.upper.CL.0.95
## V1 0.1395560 -1.6775172 0.05815888 0.4084405
## V2 0.4746753 -0.4357974 0.03903555 0.3909614
## V3 0.4835707 -0.4036444 0.03849822 0.3904263
## R0.lower.CL.0.95 R1.boot.se R1.upper.CL.0.95 R1.lower.CL.0.95
## V1 0.1763127 0.06531273 0.6258814 0.3695242
## V2 0.2043944 0.03260224 0.3454742 0.1923057
## V3 0.2053113 0.03176977 0.3379838 0.1879937
plot(binary.boot, contrast = "TE", lwd = 2)
abline(h = smary$TE.estimates[2], lty = 3)
expit <- function(x) exp(x)/(1 + exp(x))
trueTE <- function(s){
r0 <- expit(-1 - 0 * s)
r1 <- expit(-1 - 1.25 * s)
1 - r1/r0
}
rug(binary.boot$augdata$S.1)
curve(trueTE(x), add = TRUE, col = "red")
legend("bottomright", legend = c("estimated TE", "95\\% CB",
"marginal TE", "true TE"),
col = c("black", "black", "black", "red"),
lty = c(1, 2, 3, 1), lwd = c(2, 2, 1, 1))
plot(binary.boot, contrast = "logRR", lwd = 2,
col = c("black", "grey75", "grey75"))
plot(binary.boot, contrast = "RR", log = "y", lwd = 2,
col = c("black", "grey75", "grey75"))
plot(binary.boot, contrast = "RD", lwd = 2,
col = c("black", "grey75", "grey75"))
plot(binary.boot, contrast = "risk", lwd = 2, lty = c(1, 0, 0, 2, 0, 0))
legend("topright", legend = c("R0", "R1"), lty = c(1, 2), lwd = 2)
te.est <- calc_risk(binary.boot, CI.type = "pointwise", n.samps = 200)
head(te.est, 3)
## S.1 Y R0 R1 Y.boot.se
## V1 -2.328509 -1.770899 0.2983546 0.8267105 1.1943792
## V2 -2.275699 -1.743722 0.2980453 0.8177536 1.1622104
## V3 -1.694556 -1.360959 0.2946547 0.6956675 0.8334835
## Y.lower.CL.2.5 Y.upper.CL.97.5 R0.boot.se R0.lower.CL.2.5
## V1 -4.768551 -0.6276106 0.09125321 0.1460172
## V2 -4.671015 -0.6184582 0.08994238 0.1475266
## V3 -3.456249 -0.4524787 0.07557692 0.1648504
## R0.upper.CL.97.5 R1.boot.se R1.lower.CL.2.5 R1.upper.CL.97.5
## V1 0.4890998 0.06786962 0.6663453 0.9237321
## V2 0.4856823 0.06911306 0.6556765 0.9183559
## V3 0.4513863 0.07721039 0.5287710 0.8307616
plot(binary.boot, contrast = "TE", lwd = 2, CI.type = "band")
sbs <- calc_risk(binary.boot, CI.type = "pointwise", n.samps = 200)
lines(Y.lower.CL.2.5 ~ S.1, data = sbs, lty = 3, lwd = 2)
lines(Y.upper.CL.97.5 ~ S.1, data = sbs, lty = 3, lwd = 2)
legend("bottomright", lwd = 2, lty = 1:3,
legend = c("estimate", "simultaneous CI", "pointwise CI"))
library(ggplot2)
TE.est <- calc_risk(binary.boot, n.samps = 200)
ggplot(TE.est,
aes(x = S.1, y = Y, ymin = Y.lower.CL.0.95, ymax = Y.upper.CL.0.95)) +
geom_line() + geom_ribbon(alpha = .2) + ylab(attr(TE.est, "Y.function"))
cc.fit <- binary.cc + integrate_parametric(S.1 ~ BIP) +
risk_binary(D = 10) + ps_estimate()
cc.fit
surv.fit <- psdesign(fakedata, Z = Z, Y = Surv(time.obs, event.obs),
S = S.obs, BIP = BIP, CPV = CPV) +
integrate_semiparametric(formula.location = S.1 ~ BIP, formula.scale = S.1 ~ 1) +
risk_exponential(D = 10) + ps_estimate(method = "BFGS") + ps_bootstrap(n.boots = 20)
surv.fit
plot(surv.fit)
fakedata$Y.cont <- log(fakedata$time.obs + 0.01)
cont.fit <- psdesign(fakedata, Z = Z, Y = Y.cont,
S = S.obs, BIP = BIP, CPV = CPV) +
integrate_semiparametric(formula.location = S.1 ~ BIP, formula.scale = S.1 ~ 1) +
risk_continuous(D = 10) + ps_estimate(method = "BFGS") + ps_bootstrap(n.boots = 20)
cont.fit
plot(cont.fit, contrast = "risk")
with(fakedata, table(S.obs.cat, BIP.cat))
cat.fit <- psdesign(fakedata, Z = Z, Y = Y.obs,
S = S.obs.cat, BIP = BIP.cat) +
integrate_nonparametric(formula = S.1 ~ BIP) +
risk_binary(Y ~ S.1 * Z, D = 10, risk = risk.probit) + ps_estimate(method = "BFGS")
cat.fit
plot(cat.fit)
cat.fit.ps <- psdesign(fakedata, Z = Z, Y = Y.obs,
S = S.obs, BIP = BIP.cat) +
integrate_nonparametric(formula = S.1 ~ BIP) +
risk_binary(Y ~ S.1 * Z, D = 10, risk = risk.logit) +
ps_estimate(method = "pseudo-score") +
ps_bootstrap(n.boots = 20, method = "pseudo-score")
summary(cat.fit.ps)
plot(cat.fit.ps)
