library(MLCIRTwithin)
(multi1 <- rbind(c(1,2,0,0), c(3,4,5,6)))
# [,1] [,2] [,3] [,4]
# [1,] 1 2 0 0
# [2,] 3 4 5 6
(multi2 <- c(7, 2:3, 5:6))
# [1] 7 2 3 5 6
Zth1 <- matrix(0,2*2,0)
zth1 <- c(rep(-1, times=2), rep(1, times = 2))
Zth2 <- matrix(0,2,0)
zth2 <- c(-0.5,0.5)
(Zga1 <- diag(6)[, -c(1,3)])
# [,1] [,2] [,3] [,4] [,5]
# [1,] 0 0 0 0
# [2,] 1 0 0 0
# [3,] 0 0 0 0
# [4,] 0 1 0 0
# [5,] 0 0 1 0
# [6,] 0 0 0 1
(Zga2 <- diag(5)[, -5])
# [,1] [,2] [,3] [,4]
# [1,] 1 0 0 0
# [2,] 0 1 0 0
# [3,] 0 0 1 0
# [4,] 0 0 0 1
# [5,] 0 0 0 0
(Zbe <- diag(7)[, -c(1,3,7)])
# [,1] [,2] [,3] [,4]
# [1,] 0 0 0 0
# [2,] 1 0 0 0
# [3,] 0 0 0 0
# [4,] 0 1 0 0
# [5,] 0 0 1 0
# [6,] 0 0 0 1
# [7,] 0 0 0 0
Zga1 <- diag(6)[ , -c(1, 3, 5)]; Zga1[5, 2] <- 1
Zga1
# [,1] [,2] [,3]
# [1,] 0 0 0
# [2,] 1 0 0
# [3,] 0 0 0
# [4,] 0 1 0
# [5,] 0 1 0
# [6,] 0 0 1
zga1 <- c(1, 0, 1, 0, 0, 0)
zga2 <- c(0, 0, 0, 0, 1)
zbe <- rep(0, times = 7)
Zbe <- diag(7)[, -c(1,3,4,7)]
Zbe
# [,1] [,2] [,3]
# [1,] 0 0 0
# [2,] 1 0 0
# [3,] 0 0 0
# [4,] 0 0 0
# [5,] 0 1 0
# [6,] 0 0 1
# [7,] 0 0 0
zbe <- c(0, 0, 0, 2, 0, 0, 0)
# out1 <- est_multi_poly_between(S, k = k0, link = "global", multi = rbind(1:3, 4:6))
# out2 <- est_multi_poly_within(S, k1 = k0, k2 = k0, link = "global", multi1 = c(1:3),
# multi2 = c(4:6)).
library(MLCIRTwithin)
data(SF12_nomiss)
head(SF12_nomiss)
# Y1 Y2 Y3 Y4 Y5 Y6 Y7 Y8 Y9 Y10 Y11 Y12 age
# 1 1 0 1 1 0 2 2 2 1 1 1 0 74.94593
# 2 0 0 1 1 2 1 2 1 0 2 1 1 84.49829
# 3 1 1 1 2 1 0 0 2 1 1 1 1 77.44285
# 4 3 2 2 4 4 4 4 3 4 4 4 4 80.55305
# 6 1 1 2 2 2 2 2 2 2 2 2 2 81.68104
# 7 1 0 1 3 2 2 1 2 1 2 1 3 78.55168
str(SF12_nomiss)
# $ Y1 : num 1 0 1 3 1 1 1 1 2 2 ...
# $ Y2 : num 0 0 1 2 1 0 0 2 1 1 ...
# $ Y3 : num 1 1 1 2 2 1 1 2 1 2 ...
# $ Y4 : num 1 1 2 4 2 3 1 3 2 3 ...
# $ Y5 : num 0 2 1 4 2 2 2 2 2 3 ...
# $ Y6 : num 2 1 0 4 2 2 2 2 3 3 ...
# $ Y7 : num 2 2 0 4 2 1 1 2 3 3 ...
# $ Y8 : num 2 1 2 3 2 2 2 1 3 3 ...
# $ Y9 : num 1 0 1 4 2 1 2 3 3 3 ...
# $ Y10: num 1 2 1 4 2 2 1 2 2 2 ...
# $ Y11: num 1 1 1 4 2 1 2 1 4 3 ...
# $ Y12: num 0 1 1 4 2 3 1 2 3 3 ...
# $ age: num 74.9 84.5 77.4 80.6 81.7 ...
# For the description of each item see the online documentation
?SF_nomiss
(multi1_dim1 <- c(1:5, 8))
# [1] 1 2 3 4 5 8
(multi1_dim2 <- c(6:7, 9:12))
# [1] 6 7 9 10 11 12
(multi2_dim1 <- c(1:5, 8:12))
# [1] 1 2 3 4 5 8 9 10 11 12
(multi2_dim2 <- c(6:12, 1))
# [1] 6 7 8 9 10 11 12 1
(multi3_dim1 <- c(1:5, 8, 12))
# [1] 1 2 3 4 5 8 12
(multi3_dim2 <- c(6:12, 1))
# [1] 6 7 8 9 10 11 12 1
(multi4_dim1 <- c(1:5, 8, 12))
# [1] 1 2 3 4 5 8 12
(multi4_dim2 <- c(6:7, 9:12, 1))
# [1] 6 7 9 10 11 12 1
# Item responses and covariates
S <- SF12_nomiss[ , 1:12]
X <- SF12_nomiss[ , 13]
### Model selection
maxk1 <- 6
maxk2 <- 6
tol1 <- 10^-3
tol2 <- 10^-6
# Multidimensional structure of Type 1
set.seed(0)
out1 <- search.model_within(S, kv1 = 1:maxk1, kv2 = 1:maxk2, X = X, link = "global",
disc = TRUE, multi1 = multi1_dim1, multi2 = multi1_dim2,
fort = TRUE, tol1 = tol1, tol2 = tol2, nrep = 1)
# Multidimensional structure of Type 2
set.seed(0)
out2 <- search.model_within(S, kv1 = 1:maxk1, kv2 = 1:maxk2, X = X, link = "global",
disc = TRUE, multi1 = multi2_dim1, multi2 = multi2_dim2,
fort = TRUE, tol1 = tol1, tol2 = tol2, nrep = 1)
# Multidimensional structure of Type 3
set.seed(0)
out3 <- search.model_within(S, kv1 = 1:maxk1, kv2 = 1:maxk2, X = X, link = "global",
disc = TRUE, multi1 = multi3_dim1, multi2 = multi3_dim2,
fort = TRUE, tol1 = tol1, tol2 = tol2, nrep = 1)
# Multidimensional structure of Type 4
set.seed(0)
out4 <- search.model_within(S, kv1 = 1:maxk1, kv2 = 1:maxk2, X = X, link = "global",
disc = TRUE, multi1 = multi4_dim1, multi2 = multi4_dim2,
fort = TRUE, tol1 = tol1, tol2 = tol2, nrep = 1)
# BIC indices
BIC <- cbind(out1$bicv, out2$bicv, out3$bicv, out4$bicv)
colnames(BIC) <- c("Type 1", "Type 2", "Type 3", "Type 4")
k1 <- rep(1:6, times = 1, each = 6)
k2 <- rep(1:6, times = 6)
BIC <- cbind(k1, k2, BIC)
BIC
k1 k2 Type 1 Type 2 Type 3 Type 4
[1,] 1 1 16041.95 16041.95 16041.95 16041.95
[2,] 1 2 14952.57 14758.90 14758.90 14857.40
[3,] 1 3 14637.43 14397.05 14397.05 14523.30
[4,] 1 4 14457.90 14186.90 14186.90 14323.75
[5,] 1 5 14457.16 14182.29 14182.29 14322.70
[6,] 1 6 14456.62 14181.76 14181.76 14323.05
[7,] 2 1 15245.74 14755.76 15106.47 15106.47
[8,] 2 2 14180.93 14010.29 14071.65 14097.89
[9,] 2 3 13865.85 13721.04 13750.05 13784.53
[10,] 2 4 13686.27 13543.49 13554.24 13595.26
[11,] 2 5 13685.48 13543.77 13553.30 13599.42
[12,] 2 6 13684.91 13559.40 13563.59 13604.91
[13,] 3 1 14945.39 14292.43 14748.77 14748.77
[14,] 3 2 13880.61 13674.91 13783.16 13791.13
[15,] 3 3 13565.50 13435.72 13482.17 13495.04
[16,] 3 4 13385.96 13291.34 13303.73 13319.80
[17,] 3 5 13385.83 13294.18 13306.15 13324.50
[18,] 3 6 13392.96 13308.42 13318.83 13334.18
[19,] 4 1 14897.97 14183.55 14678.01 14678.01
[20,] 4 2 13837.48 13606.69 13728.08 13732.33
[21,] 4 3 13522.40 13369.83 13441.26 13451.80
[22,] 4 4 13342.82 13245.49 13262.09 13271.06
[23,] 4 5 13342.45 13251.35 13269.85 13277.34
[24,] 4 6 13341.56 13265.77 13271.07 13293.80
[25,] 5 1 14858.93 14122.61 14610.54 14610.54
[26,] 5 2 13851.00 13549.28 13744.75 13674.28
[27,] 5 3 13538.57 13353.08 13383.57 13387.67
[28,] 5 4 13356.25 13196.32 13215.00 13219.66
[29,] 5 5 13298.70 13207.16 13216.58 13226.12
[30,] 5 6 13298.20 13212.85 13237.74 13242.02
[31,] 6 1 14932.45 14110.19 14613.70 14613.70
[32,] 6 2 13805.78 13557.76 13683.30 13683.43
[33,] 6 3 13495.08 13339.91 13393.67 13410.61
[34,] 6 4 13311.17 13205.40 13231.35 13230.88
[35,] 6 5 13312.05 13214.68 13237.86 13240.88
[36,] 6 6 13310.90 13222.19 13241.50 13243.07
# Minimum BIC
min(BIC[ , 3:6])
[1] 13196.32
# Detect model with the minimum BIC
arrayInd(which.min(BIC[ , -c(1:2)]), .dim = dim(BIC))
[,1] [,2]
[1,] 28 2
# Number of support points for the best model
out2$out.single[[28]]$k1
[1] 5
out2$out.single[[28]]$k2
[1] 4
# Selected model
outsel = out2$out.single[[28]]
# Minimum BIC and selected model for multidimensional structures of types 1, 3, 4
min(BIC[, "Type 1"])
[1] 13298.2
which.min(BIC[ ,"Type 1"])
[1] 30
min(BIC[, "Type 3"])
[1] 13215
which.min(BIC[ ,"Type 3"])
[1] 28
min(BIC[, "Type 4"])
[1] 13219.66
which.min(BIC[ ,"Type 4"])
[1] 28
# Re-estimate model to compute standard errors
out <- est_multi_poly_within(S = S, k1 = outsel$k1, k2 = outsel$k2, X = X,
start = "external",
multi1 = multi2_dim1, multi2 = multi2_dim2,
Phi = outsel$Phi, ga1t = outsel$ga1t, ga2t = outsel$ga2t,
De1 = outsel$De1, De2 = outsel$De2,
link = "global", disc = TRUE, fort = TRUE, output = TRUE,
out_se = TRUE, disp = TRUE)
### Display output of the estimated model
# summary(out)
# coef(out)
# confint(out)
# Estimates of support points and average mass probabilities for physical HQOL
lv1 <- rbind(out$Th1, t(out$piv1))
rownames(lv1) <- c("Physical HQOL", "Average prob.")
round(lv1, 3)
1 2 3 4 5
Physical HQOL 0.823 1.684 2.854 0.093 -2.024
Average prob. 0.199 0.387 0.310 0.083 0.021
# Estimates of support points and average mass probabilities for emotional HQOL
lv2 <- rbind(out$Th2, t(out$piv2))
rownames(lv2) <- c("Emotional HQOL", "Average prob.")
round(lv2, 3)
1 2 3 4
Emotional HQOL 0.471 11.526 7.585 4.151
Average prob. 0.149 0.141 0.390 0.321
# Standardized support points of physical HQOL
lv1s <- rbind(out$Th1s, t(out$piv1s))
rownames(lv1s) <- c("Stand. Physical HQOL", "Average prob.")
round(lv1s, 3)
5 4 1 2 3
Stand. Physical HQOL -3.561 -1.517 -0.812 0.019 1.149
Average prob. 0.021 0.083 0.199 0.387 0.310
# Standardized support points of emotional HQOL
lv2s <- rbind(out$Th2s, t(out$piv2s))
rownames(lv2s) <- c("Stand. Emotional HQOL", "Average prob.")
round(lv2s, 3)
1 4 3 2
Stand. Emotional HQOL -1.667 -0.553 0.486 1.679
Average prob. 0.149 0.321 0.390 0.141
# Item discriminating parameters and related standard errors
gamma1 <- cbind(out$ga1c, out$sega1c)
colnames(gamma1) <- c("gamma1", "st.err.")
round(gamma1, 3)
gamma1 st.err.
[1,] 1.000 0.000
[2,] 1.988 0.329
[3,] 1.193 0.199
[4,] 3.519 0.570
[5,] 3.394 0.582
[6,] NA NA
[7,] NA NA
[8,] 1.579 0.255
[9,] -0.006 0.117
[10,] 0.732 0.151
[11,] 0.145 0.125
[12,] 1.002 0.184
gamma2 <-cbind(out$ga2c, out$sega2c)
colnames(gamma2) <- c("gamma2", "st.err.")
round(gamma2, 3)
gamma2 st.err.
[1,] 0.212 0.042
[2,] NA NA
[3,] NA NA
[4,] NA NA
[5,] NA NA
[6,] 1.000 0.000
[7,] 0.809 0.076
[8,] 0.128 0.037
[9,] 0.599 0.079
[10,] 0.429 0.059
[11,] 0.793 0.093
[12,] 0.606 0.074
# Confidence intervals at 95% of item discriminating parameters (columns 1-4)
confint(out)
[...] Output omitted
Confidence interval for the item parameters:
gamma1_1 gamma1_2 gamma2_1 gamma2_2 beta1_1 beta1_2 beta2_1 beta2_2 beta3_1 beta3_2 beta4_1 beta4_2
1 1.0000 1.0000 0.1293 0.2939 0.0000 0.0000 2.2927 3.0763 5.4703 6.6560 7.0976 8.9132
2 1.3425 2.6326 NA NA -0.2428 1.8308 3.6945 6.1378 NA NA NA NA
3 0.8036 1.5825 NA NA -1.4885 -0.0974 1.1539 2.5872 NA NA NA NA
4 2.4010 4.6368 NA NA -1.3097 2.1941 1.2833 4.9066 5.2791 9.4358 8.5939 13.4278
5 2.2542 4.5348 NA NA -0.8427 2.5507 1.9437 5.5126 5.3510 9.4330 8.2434 12.8277
6 NA NA 1.0000 1.0000 0.0000 0.0000 2.4866 3.9477 5.4657 7.6420 8.4771 11.2498
7 NA NA 0.6612 0.9574 -0.4397 0.7189 1.7730 3.0777 4.1602 5.8143 7.1608 9.3353
8 1.0790 2.0796 0.0556 0.2010 -2.8325 -0.8067 0.3711 2.0693 3.0574 4.9355 4.5989 6.6225
9 -0.2348 0.2235 0.4451 0.7529 -1.9089 -0.6523 1.0767 2.2309 3.5534 5.0154 6.9565 9.1180
10 0.4366 1.0274 0.3135 0.5451 -0.7823 0.3940 1.9615 3.1901 4.6207 6.1860 7.3341 9.3617
11 -0.0999 0.3905 0.6100 0.9762 -1.1600 0.1005 1.6032 2.9576 4.8700 6.7528 7.4612 9.8398
12 0.6411 1.3637 0.4611 0.7503 -1.5644 0.0124 1.4481 2.8845 4.6337 6.4619 6.9167 9.0782
[...] Output omitted
γ
γ
γ
γ
∗
γ
∗
# Standardized discriminating parameters
gammas <- rbind(out$ga1cs, out$ga2cs)
rownames(gammas) <- c("Physical HQOL", "Emotional HQOL")
round(gammas, 3)
[,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
Physical HQOL 1.036 2.059 1.236 3.645 3.516 NA NA 1.636 -0.006 0.758 0.151 1.038
Emotional HQOL 0.699 NA NA NA NA 3.304 2.674 0.424 1.979 1.419 2.621 2.001
γ
∗
γ
∗
γ
∗
γ
∗
# Standardized difficulty parameters
round(out$Becs, 3)
cutoff
item 1 2 3 4
1 -2.929 -0.245 3.134 5.076
2 -2.514 1.608 NA NA
3 -2.778 -0.115 NA NA
4 -5.414 -2.761 1.501 5.155
5 -4.795 -1.921 1.743 4.887
6 -5.979 -2.762 0.575 3.884
7 -4.700 -2.414 0.148 3.409
8 -5.215 -2.175 0.601 2.215
9 -4.853 -1.918 0.712 4.465
10 -3.979 -1.209 1.618 4.563
11 -5.514 -2.704 0.827 3.667
12 -6.066 -3.124 0.258 2.707
δ
# Effect of covariate age on physical HQOL
De1 <- cbind(out$De1, out$seDe1)
colnames(De1) <- c("delta12", "delta13", "delta14", "delta15", "se(delta12)",
"se(delta13)", "se(delta14)", "se(delta15)")
round(De1, 3)
delta12 delta13 delta14 delta15 se(delta12) se(delta13) se(delta14) se(delta15)
intercept 2.960 3.021 -0.997 -4.773 1.035 0.937 1.862 2.453
X1 -0.038 -0.043 0.002 0.039 0.017 0.015 0.026 0.035
# Effect of covariate age on emotional HQOL
De2 <- cbind(out$De2, out$seDe2)
colnames(De2) <- c("delta22", "delta23", "delta24", "se(delta22)", "se(delta23)",
"se(delta24)")
round(De2, 3)
delta22 delta23 delta24 se(delta22) se(delta23) se(delta24)
intercept 0.743 2.581 1.905 0.951 0.791 0.850
X1 -0.013 -0.027 -0.019 0.015 0.013 0.014
# Confidence intervals at 95% of regression coeffic. for physical and emotional HQOL
confint(out)
[...] Output omitted
Confidence interval for the regression coefficients for the 1st latent variable:
logit
2_1 2_2 3_1 3_2 4_1 4_2 5_1 5_2
intercept 0.9301 4.9890 1.1833 4.8581 -4.6451 2.6520 -9.5808 0.0355
X1 -0.0709 -0.0055 -0.0717 -0.0146 -0.0489 0.0528 -0.0295 0.1075
Confidence interval for the regression coefficients for the 2nd latent variable:
logit
2_1 2_2 3_1 3_2 4_1 4_2
intercept -1.1204 2.606 1.0306 4.1313 0.2383 3.5707
X1 -0.0434 0.017 -0.0521 -0.0021 -0.0458 0.0081
# Model with global logit parametrization for covariates
outgl <- est_multi_poly_within(S = S, k1 = 5, k2 = 4, X = X,
multi1 = multi2_dim1, multi2 = multi2_dim2, link = "global",
disc = TRUE, fort = TRUE, tol = 10^-8, disp = TRUE, output = TRUE,
out_se = TRUE, glob = TRUE)
# Effect of covariate age on physical HQOL
De1glob <- cbind(outgl$De1, outgl$seDe1)
colnames(De1glob) <- c("coef", "se")
round(De1glob, 3)
coef se
cutoff1 5.093 0.544
cutoff2 2.827 0.467
cutoff3 1.050 0.443
cutoff4 -0.777 0.477
X1 -0.028 0.007
# Effect of covariate age on emotional HQOL
De2glob <- cbind(outgl$De2, outgl$seDe2)
colnames(De2glob) <- c("coef", "se")
round(De2glob, 3)
coef se
cutoff1 2.306 0.463
cutoff2 0.664 0.453
cutoff3 -1.312 0.461
X1 -0.010 0.007
# Confidence intervals at 95% of regression coeffic. for physical and emotional HQOL
confint(outgl)
[...] Output omitted
Confidence interval for the regression coefficients for the 1st latent variable:
logit
_1 _2
cutoff1 4.0265 6.1586
cutoff2 1.9112 3.7427
cutoff3 0.1816 1.9187
cutoff4 -1.7110 0.1575
X1 -0.0422 -0.0132
Confidence interval for the regression coefficients for the 2nd latent variable:
logit
_1 _2
cutoff1 1.3987 3.2124
cutoff2 -0.2229 1.5509
cutoff3 -2.2153 -0.4088
X1 -0.0240 0.0049
library(MLCIRTwithin)
### Load and prepare data
library(LMest)
data(data_criminal_sim)
data_criminal_sim[1:12,]
id sex time y1 y2 y3 y4 y5 y6 y7 y8 y9 y10
[1,] 1 1 1 0 0 0 0 0 0 0 0 0 0
[2,] 1 1 2 0 0 0 0 0 0 0 0 0 0
[3,] 1 1 3 0 0 0 0 0 0 0 0 0 0
[4,] 1 1 4 0 0 0 0 0 0 0 0 0 0
[5,] 1 1 5 0 0 0 0 0 0 0 0 0 0
[6,] 1 1 6 0 0 0 0 0 0 0 0 0 0
[7,] 2 1 1 0 0 0 0 0 0 0 0 0 0
[8,] 2 1 2 0 0 0 0 0 0 0 0 0 0
[9,] 2 1 3 0 0 0 0 0 0 0 0 0 0
[10,] 2 1 4 0 0 0 0 0 0 0 0 0 0
[11,] 2 1 5 0 0 0 0 0 0 0 0 0 0
[12,] 2 1 6 0 0 0 0 0 0 0 0 0 0
# Keep items: y1,y3,y5,y7,y10; keep occasions: 1, 2
criminal_red <- data_criminal_sim[(data_criminal_sim[,3]==1 | data_criminal_sim[,3]==2),
c(1:3,4,6,8,10,13)]
# Data reshape in wide format
criminal_red <- data.frame(criminal_red)
crim_wide <- reshape(criminal_red, v.names = c("y1", "y3", "y5", "y7", "y10"),
timevar = "time", idvar = "id", direction = "wide")
head(crim_wide)
id sex y1.1 y3.1 y5.1 y7.1 y10.1 y1.2 y3.2 y5.2 y7.2 y10.2
1 1 1 0 0 0 0 0 0 0 0 0 0
3 2 1 0 0 0 0 0 0 0 0 0 0
5 3 1 0 0 0 0 0 0 0 0 0 0
7 4 1 0 0 0 0 0 0 0 0 0 0
9 5 1 0 0 0 0 0 0 0 0 0 0
11 6 1 0 0 0 0 0 0 0 0 0 0
dim(crim_wide)
[1] 10000 12
# Aggregate records with the same pattern
crim_wide <- as.matrix(crim_wide)
crim_wide2 <- aggr_data(crim_wide[, -1])
# Item responses, covariates, and vector of weights
S <- crim_wide2$data_dis[,-1]
X <- crim_wide2$data_dis[,1]; X <- X - 1
yv <- crim_wide2$freq
0
0
### Define the multidimensional structure
multi1 <- matrix(0, nrow=5, ncol=2)
multi2 <- matrix(0, nrow=2, ncol=5)
multi1[1,] <- c(6, 1)
multi1[2,] <- c(2, 7)
multi1[3,] <- c(3, 8)
multi1[4,] <- c(4, 9)
multi1[5,] <- c(5, 10)
multi2[1,] <- c(1:5)
multi2[2,] <- c(7, 6, 8:10)
multi1
[,1] [,2]
[1,] 6 1
[2,] 2 7
[3,] 3 8
[4,] 4 9
[5,] 5 10
multi2
[,1] [,2] [,3] [,4] [,5]
[1,] 1 2 3 4 5
[2,] 7 6 8 9 10
# Number of latent classes
k1 <- 2
k2 <- 2
0
0
### Specification of model constraints
# Fix support points on latent variable U
# Fix support points on the first dimension of latent variable V
# Free support points on the second dimension of latent variable V
(Zth1 <- matrix(0, nrow(multi1)*k1, 0))
[1,]
[2,]
[3,]
[4,]
[5,]
[6,]
[7,]
[8,]
[9,]
[10,]
(zth1 <- c(rep(-1, times = nrow(multi1)), rep(1, times = nrow(multi1))))
[1] -1 -1 -1 -1 -1 1 1 1 1 1
(Zth2 <- diag(nrow(multi2)*k2)[ , -c(1,3)])
[,1] [,2]
[1,] 0 0
[2,] 1 0
[3,] 0 0
[4,] 0 1
(zth2 <- c(-1, 0, 1, 0))
[1] -1 0 1 0
# Equality constraints on difficulties and discriminating indices to account
# for the longitudinal data structure
(Zbe <- matrix(1, nrow(multi2), 1) %x% diag(nrow(multi1)))
[,1] [,2] [,3] [,4] [,5]
[1,] 1 0 0 0 0
[2,] 0 1 0 0 0
[3,] 0 0 1 0 0
[4,] 0 0 0 1 0
[5,] 0 0 0 0 1
[6,] 1 0 0 0 0
[7,] 0 1 0 0 0
[8,] 0 0 1 0 0
[9,] 0 0 0 1 0
[10,] 0 0 0 0 1
Zga2 <- Zbe
Zga1 <- Zbe
out <- est_multi_poly_within(S = S, yv = yv, k1 = k1, k2 = k2, X = X, link = "global",
disc = TRUE, multi1 = multi1, multi2 = multi2, disp = TRUE,
output = TRUE, out_se = TRUE, Zth1 = Zth1, zth1 = zth1, Zth2 = Zth2,
zth2 = zth2, Zbe = Zbe, Zga1 = Zga1, Zga2 = Zga2)
### Display output
# summary(out)
# coef(out)
# confint(out)
# Support points and average weights of latent variable V
out$Th2
class
dimension 1 2
1 -1.0000000 1.000000
2 -0.6925198 1.679367
out$piv2
[,1]
[1,] 0.91757919
[2,] 0.08242081
# Conditional response probabilities for latent variable V
# Note that the latent classes are numbered following an increasing order,
# starting from latent variable U
round(out$Phi[,, 3:4], 3)
, , class = 3
item
category 1 2 3 4 5 6 7 8 9 10
0 0.994 0.982 0.944 0.986 0.998 0.991 0.974 0.919 0.981 0.997
1 0.006 0.018 0.056 0.014 0.002 0.009 0.026 0.081 0.019 0.003
, , class = 4
item
category 1 2 3 4 5 6 7 8 9 10
0 0.897 0.799 0.551 0.874 0.882 0.758 0.618 0.335 0.758 0.628
1 0.103 0.201 0.449 0.126 0.118 0.242 0.382 0.665 0.242 0.372
# Estimates of item parameters
beta <- cbind(out$Bec[1:5], out$Bec[6:10])
colnames(beta) <- c("time 1", "time 2")
gamma1 <- cbind(out$ga1c[1:5], out$ga1c[6:10])
colnames(gamma1) <- c("time 1", "time 2")
gamma2 <- cbind(out$ga2c[1:5], out$ga2c[6:10])
colnames(gamma2) <- c("time 1", "time 2")
beta
time 1 time 2
[1,] 4.994018 4.994018
[2,] 4.891019 4.891019
[3,] 2.700745 2.700745
[4,] 5.991825 5.991825
[5,] 5.544927 5.544927
gamma1
time 1 time 2
[1,] 1.318531 1.318531
[2,] 2.195804 2.195804
[3,] 1.184268 1.184268
[4,] 2.883623 2.883623
[5,] 1.353772 1.353772
gamma2
time 1 time 2
[1,] 1.510292 1.510292
[2,] 1.317424 1.317424
[3,] 1.311341 1.311341
[4,] 1.169848 1.169848
[5,] 2.183823 2.183823
