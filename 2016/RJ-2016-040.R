set.seed(12345)
n <- 100 #sample size
p <- 100 # number of variables
s <- 3 # number of variables with non-zero coefficients
X <- matrix(rnorm(n*p), ncol=p)
beta <- c(rep(5,s), rep(0,p-s))
Y <- X%*%beta + rnorm(n)
lasso.reg <- rlasso(Y~X, post=FALSE, intercept=TRUE) # use Lasso, not-post-Lasso
# lasso.reg <- rlasso(X,Y, post=FALSE, intercept=TRUE) # alternative use
summary(lasso.reg, all=FALSE)
# Call:
# rlasso.formula(formula = Y ~ X, post = FALSE, intercept = TRUE)
# Post-Lasso Estimation: FALSE
# Total number of variables: 100
# Number of selected variables: 11
# Residuals:
# Min 1Q Median 3Q Max
# -2.09008 -0.45801 -0.01237 0.50291 2.25098
# Estimate
# (Intercept) 0.057
# 1 4.771
# 2 4.693
# 3 4.766
# 13 -0.045
# 15 -0.047
# 16 -0.005
# 19 -0.092
# 22 -0.027
# 40 -0.011
# 61 0.114
# 100 -0.025
# Residual standard error: 0.8039
# Multiple R-squared: 0.9913
# Adjusted R-squared: 0.9902
# Joint significance test:
# the sup score statistic for joint significance test is 64.02 with a p-value of 0
yhat.lasso <- predict(lasso.reg) #in-sample prediction
Xnew <- matrix(rnorm(n*p), ncol=p) # new X
Ynew <- Xnew%*%beta + rnorm(n) #new Y
yhat.lasso.new <- predict(lasso.reg, newdata=Xnew) #out-of-sample prediction
post.lasso.reg <- rlasso(Y~X, post=TRUE, intercept=TRUE)
summary(post.lasso.reg, all=FALSE)
# Call:
# rlasso.formula(formula = Y ~ X, post = TRUE, intercept = TRUE)
# Post-Lasso Estimation: TRUE
# Total number of variables: 100
# Number of selected variables: 3
# Residuals:
# Min 1Q Median 3Q Max
# -2.83981 -0.80339 0.02063 0.75573 2.30421
# Estimate
# (Intercept) 0.000
# 1 5.150
# 2 4.905
# 3 4.912
# Residual standard error: 1.059
# Multiple R-squared: 0.9855
# Adjusted R-squared: 0.9851
# Joint significance test:
# the sup score statistic for joint significance test is 66.87 with a p-value of 0
yhat.postlasso <- predict(post.lasso.reg) #in-sample prediction
yhat.postlasso.new <- predict(post.lasso.reg, newdata=Xnew) #out-of-sample prediction
lasso.effect <- rlassoEffects(x=X, y=Y, index=c(1,2,3,50))
print(lasso.effect)
# Call:
# rlassoEffects.default(x = X, y = Y, index = c(1, 2, 3, 50))
# Coefficients:
# V1 V2 V3 V50
# 5.0890 4.7781 4.8292 0.1384
summary(lasso.effect)
# [1] "Estimates and significance testing of the effect of target variables"
# Estimate. Std. Error t value Pr(>|t|)
# V1 5.0890 0.1112 45.781 <2e-16 ***
# V2 4.7781 0.1318 36.264 <2e-16 ***
# V3 4.8292 0.1314 36.752 <2e-16 ***
# V50 0.1384 0.1122 1.234 0.217
# ---
# Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
confint(lasso.effect)
# 2.5 % 97.5 %
confint(lasso.effect)
# 2.5 % 97.5 %
# V1 4.8710981 5.306834
# V2 4.5198436 5.036332
# V3 4.5716439 5.086726
# V50 -0.0814167 0.358284
confint(lasso.effect, joint = TRUE)
# 2.5 % 97.5 %
# V1 4.8464078 5.3315239
# V2 4.4315649 5.1246107
# V3 4.4864563 5.1719131
# V50 -0.1720149 0.4488822
plot(lasso.effect, main="Confidence Intervals")
Y <- X%*%beta + rnorm(n, sd=sin(X%*%beta)^2)
Y <- X%*%beta + rt(n, df=5)
lasso.reg = rlasso(Y~X, post=FALSE, intercept=TRUE) # use Lasso, not-post-Lasso
summary(lasso.reg, all=FALSE)
# Call:
# rlasso.formula(formula = Y ~ X, post = FALSE, intercept = TRUE)
# Post-Lasso Estimation: FALSE
# Total number of variables: 100
# Number of selected variables: 8
# Residuals:
# Min 1Q Median 3Q Max
# -2.51449 -0.76567 0.09798 0.80007 2.11780
# Estimate
# (Intercept) 0.032
# 1 4.959
# 2 4.686
# 3 4.671
# 8 0.083
# 21 0.010
# 36 0.047
# 58 -0.070
# 100 -0.133
# Residual standard error: 1.051
# Multiple R-squared: 0.9858
# Adjusted R-squared: 0.9845
# Joint significance test:
# the sup score statistic for joint significance test is 66.87 with a p-value of 0
library(hdm)
data(cps2012)
X <- model.matrix( ~ -1 + female + female:(widowed + divorced + separated +
nevermarried + hsd08 + hsd911+ hsg + cg + ad + mw + so + we + exp1 + exp2 + exp3) +
(widowed + divorced + separated + nevermarried +
hsd08 + hsd911 + hsg + cg + ad + mw + so + we + exp1 + exp2 + exp3)^2, data=cps2012)
dim(X)
# [1] 29217 136
X <- X[,which(apply(X, 2, var)!=0)] # exclude all constant variables
dim(X)
# [1] 29217 116
index.gender <- grep("female", colnames(X))
y <- cps2012$lnw
effects.female <- rlassoEffects(x=X, y=y, index=index.gender)
summary(effects.female)
# [1] "Estimates and significance testing of the effect of target variables"
# Estimate. Std. Error t value Pr(>|t|)
# female -0.154923 0.050162 -3.088 0.002012 **
# female:widowed 0.136095 0.090663 1.501 0.133325
# female:divorced 0.136939 0.022182 6.174 6.68e-10 ***
# female:separated 0.023303 0.053212 0.438 0.661441
# female:nevermarried 0.186853 0.019942 9.370 < 2e-16 ***
# female:hsd08 0.027810 0.120914 0.230 0.818092
# female:hsd911 -0.119335 0.051880 -2.300 0.021435 *
# female:hsg -0.012890 0.019223 -0.671 0.502518
# female:cg 0.010139 0.018327 0.553 0.580114
# female:ad -0.030464 0.021806 -1.397 0.162405
# female:mw -0.001063 0.019192 -0.055 0.955811
# female:so -0.008183 0.019357 -0.423 0.672468
# female:we -0.004226 0.021168 -0.200 0.841760
# female:exp1 0.004935 0.007804 0.632 0.527139
# female:exp2 -0.159519 0.045300 -3.521 0.000429 ***
# female:exp3 0.038451 0.007861 4.891 1.00e-06 ***
# ---
# Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
joint.CI <- confint(effects.female, level = 0.95, joint = TRUE)
joint.CI
# 2.5 % 97.5 %
# female -0.244219905 -0.06562666
# female:widowed -0.036833704 0.30902467
# female:divorced 0.097104065 0.17677471
# female:separated -0.066454223 0.11305975
# female:nevermarried 0.149932792 0.22377417
# female:hsd08 -0.230085134 0.28570576
# female:hsd911 -0.215291089 -0.02337899
# female:hsg -0.046383541 0.02060398
# female:cg -0.023079946 0.04335705
# female:ad -0.072370082 0.01144259
# female:mw -0.035451308 0.03332443
# female:so -0.043143587 0.02677690
# female:we -0.043587197 0.03513494
# female:exp1 -0.008802488 0.01867301
# female:exp2 -0.239408202 -0.07963045
# female:exp3 0.024602478 0.05229868
plot(effects.female, joint=TRUE, level=0.95)
effects.female <- rlassoEffects(lnw ~ female + female:(widowed + divorced + separated +
nevermarried + hsd08 + hsd911 + hsg + cg + ad + mw + so + we + exp1 + exp2 + exp3) +
(widowed + divorced + separated + nevermarried +
hsd08 + hsd911 + hsg + cg + ad + mw + so + we + exp1 + exp2 + exp3)^2, data=cps2012,
I = ~ female + female:(widowed + divorced + separated + nevermarried +
hsd08 + hsd911 + hsg + cg + ad + mw + so + we + exp1 + exp2 + exp3))
data(EminentDomain)
z <- EminentDomain$logGDP$z
x <- EminentDomain$logGDP$x
y <- EminentDomain$logGDP$y
d <- EminentDomain$logGDP$d
lasso.IV.Z <- rlassoIV(x=x, d=d, y=y, z=z, select.X=FALSE, select.Z=TRUE)
# or lasso.IV.Z <- rlassoIVselectZ(x=X, d=d, y=y, z=Z)
summary(lasso.IV.Z)
# [1] "Estimates and significance testing of the effect of target variables in the
# IV regression model"
# coeff. se. t-value p-value
# d1 0.01543 0.01926 0.801 0.423
lasso.IV.XZ <- rlassoIV(x=x, d=d, y=y, z=z, select.X=TRUE, select.Z=TRUE)
summary(lasso.IV.XZ)
# Estimates and Significance Testing of the effect of target variables in the
# IV regression model
# coeff. se. t-value p-value
# d1 -0.03488 0.15881 -0.22 0.826
confint(lasso.IV.XZ)
# 2.5 % 97.5 %
# d1 -0.3461475 0.2763868
data(pension)
y <- pension$tw; d = pension$p401; z = pension$e401
X <- pension[,c("i2", "i3", "i4", "i5", "i6", "i7", "a2", "a3", "a4", "a5",
"fsize", "hs", "smcol", "col", "marr", "twoearn", "db", "pira", "hown")]
# # simple model
# xvar <- c("i2", "i3", "i4", "i5", "i6", "i7", "a2", "a3", "a4", "a5",
# "fsize", "hs", "smcol", "col", "marr", "twoearn", "db", "pira", "hown")
# xpart <- paste(xvar, collapse = "+")
# form <- as.formula(paste("tw ~ ", paste(c("p401", xvar), collapse ="+"), "|",
# paste(xvar, collapse = "+")))
# formZ <- as.formula(paste("tw ~ ", paste(c("p401", xvar), collapse ="+"), "|",
# paste(c("e401", xvar), collapse = "+")))
#pension.ate = rlassoATE(X,d,y)
pension.ate = rlassoATE(form, data = pension)
summary(pension.ate)
# Estimation and significance testing of the treatment effect
# Type: ATE
# Bootstrap: not applicable
# coeff. se. t-value p-value
# TE 10490 1920 5.464 4.67e-08 ***
# ---
# Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#pension.atet <- rlassoATET(X,d,y)
pension.atet <- rlassoATET(form, data = pension)
summary(pension.atet)
# Estimation and significance testing of the treatment effect
# Type: ATET
# Bootstrap: not applicable
# coeff. se. t-value p-value
# TE 11810 2844 4.152 3.29e-05 ***
# ---
# Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
pension.late <- rlassoLATE(X,d,y,z)
#pension.late <- rlassoLATE(formZ, data=pension)
summary(pension.late)
# Estimation and significance testing of the treatment effect
# Type: LATE
# Bootstrap: not applicable
# coeff. se. t-value p-value
# TE 12189 2734 4.458 8.27e-06 ***
# ---
# Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
pension.latet <- rlassoLATET(X,d,y,z)
#pension.latet <- rlassoLATET(formZ, data=pension)
summary(pension.latet)
# Estimation and significance testing of the treatment effect
# Type: LATET
# Bootstrap: not applicable
# coeff. se. t-value p-value
# TE 12687 3590 3.534 0.00041 ***
# ---
# Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 0
# β
# 0
# δ
# 0
# γ
# }
# data(BLP)
# BLPData <- BLP$BLP
# attach(BLPData)
# X <- as.matrix(cbind(1, BLPData[,c("hpwt", "air", "mpd", "space")]))
# Z <- BLP$Z
# #Z <- hdm:::constructIV(firm.id, cdid, id, X)
# ols.reg <- lm(y~ price + hpwt + air + mpd + space, data =BLPData)
# tsls.reg <- tsls(x=X, d=price, y=y, z=Z, intercept =FALSE, homoscedastic = FALSE)
# lasso.reg <- rlassoIV(x=X[,-1], y=y, z=Z, d=price, select.X=TRUE, select.Z=TRUE,
# intercept = TRUE)
# augZ <- BLP$augZ
# tu <- trend/19
# mpdu <- mpd/7
# spaceu <- space/2
# augX = cbind(1, hpwt, air, mpdu, spaceu, tu, hpwt^2, hpwt^3, mpdu^2, mpdu^3,
# spaceu^2, spaceu^3, tu^2, tu^3, hpwt*air, mpdu*air, spaceu*air, tu*air,
# hpwt*mpdu, hpwt*spaceu, hpwt*tu, mpdu*spaceu, mpdu*tu, spaceu*tu)
# colnames(augX) <- c("constant", "hpwt", "air", "mpdu", "spaceu", "tu", "hpwt^2", "hpwt^3",
# "mpdu^2", "mpdu^3", "spaceu^2", "spaceu^3", "tu^2", "tu^3", "hpwt*air", "mpdu*air",
# "spaceu*air", "tu*air", "hpwt*mpdu", "hpwt*spaceu", "hpwt*tu", "mpdu*spaceu", "mpdu*tu",
# "spaceu*tu")
# # augZ <- hdm:::constructIV(firm.id, cdid, id, augX) # construction of augmented set of IVs
# ols.reg <- lm(y~ -1 + cbind(price, augX))
# tsls.reg <- tsls(x=augX, d=price, y=y, z=augZ, intercept =FALSE, homoscedastic = FALSE)
# lasso.reg <- rlassoIV(x=augX[,-1], y=y, z=augZ, d=price, select.X=TRUE, select.Z=TRUE,
# intercept = TRUE)
