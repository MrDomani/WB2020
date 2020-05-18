library(Frames2)
data(DatA)
head(DatA, 3)
# Domain Feed Clo Lei Inc Tax M2 Size ProbA ProbB Stratum
# 1 a 194.48 38.79 23.66 2452.07 112.90 0.00 0 0.02063274 0.0000000 1
# 2 a 250.23 16.92 22.68 2052.37 106.99 0.00 0 0.02063274 0.0000000 1
# 3 ab 199.95 24.50 23.24 2138.24 121.16 127.41 2 0.02063274 0.1133501 1
data(PiklA)
PiklA[1:6, 1:6]
# [,1] [,2] [,3] [,4] [,5] [,6]
# [1,] 0.020632737 0.000397876 0.000397876 0.000397876 0.000397876 0.000397876
# [2,] 0.000397876 0.020632737 0.000397876 0.000397876 0.000397876 0.000397876
# [3,] 0.000397876 0.000397876 0.020632737 0.000397876 0.000397876 0.000397876
# [4,] 0.000397876 0.000397876 0.000397876 0.020632737 0.000397876 0.000397876
# [5,] 0.000397876 0.000397876 0.000397876 0.000397876 0.020632737 0.000397876
# [6,] 0.000397876 0.000397876 0.000397876 0.000397876 0.000397876 0.020632737
library(Frames2)

data(DatA)
data(DatB)
data(PiklA)
data(PiklB)

yA <- with(DatA, data.frame(Feed, Clo))
yB <- with(DatB, data.frame(Feed, Clo))

## Estimation for variables Feeding and Clothing using Hartley and
## Fuller-Burmeister estimators with first and second order probabilities known
Hartley(yA, yB, PiklA, PiklB, DatA$Domain, DatB$Domain)
# Estimation:
# Feed Clo
# Total 586959.9820 71967.62214
# Mean 246.0429 30.16751
FB(yA, yB, PiklA, PiklB, DatA$Domain, DatB$Domain)
# Estimation:
# Feed Clo
# Total 591665.5078 72064.99223
# Mean 248.0153 30.20832
## This is how estimates change when only first order probabilities are considered
Hartley(yA, yB, DatA$ProbA, DatB$ProbB, DatA$Domain, DatB$Domain)
# Estimation:
# Feed Clo
# Total 570867.8042 69473.86532
# Mean 247.9484 30.17499
FB(yA, yB, DatA$ProbA, DatB$ProbB, DatA$Domain, DatB$Domain)
# Estimation:
# Feed Clo
# Total 571971.9511 69500.11448
# Mean 248.4279 30.18639
summary(Hartley(yA, yB, PiklA, PiklB, DatA$Domain, DatB$Domain))
# Call:
# Hartley(ysA = yA, ysB = yB, pi_A = PiklA, pi_B = PiklB, domains_A = DatA$Domain,
# domains_B = DatB$Domain)
# Estimation:
# Feed Clo
# Total 586959.9820 71967.62214
# Mean 246.0429 30.16751
# Variance Estimation:
# Feed Clo
# Var. Total 2.437952e+08 4.728875e+06
# Var. Mean 4.283804e+01 8.309261e-01
# Total Domain Estimations:
# Feed Clo
# Total dom. a 263233.1 31476.84
# Total dom. ab 166651.7 21494.96
# Total dom. b 164559.2 20451.85
# Total dom. ba 128704.7 15547.49
# Mean Domain Estimations:
# Feed Clo
# Mean dom. a 251.8133 30.11129
# Mean dom. ab 241.6468 31.16792
# Mean dom. b 242.2443 30.10675
# Mean dom. ba 251.5291 30.38466
# Parameters:
# Feed Clo
# theta 0.8027766 0.7551851
Hartley(yA, yB, DatA$ProbA, DatB$ProbB, DatA$Domain, DatB$Domain, 0.95)
# Estimation and 95 % Confidence Intervals:
# Feed Clo
# Total 570867.8042 69473.86532
# Lower Bound 511904.6588 61756.37677
# Upper Bound 629830.9496 77191.35387
# Mean 247.9484 30.17499
# Lower Bound 222.3386 26.82301
# Upper Bound 273.5582 33.52697
FB(yA, yB, DatA$ProbA, DatB$ProbB, DatA$Domain, DatB$Domain, 0.95)
# Estimation and 95 % Confidence Intervals:
# Feed Clo
# Total 571971.9511 69500.11448
# Lower Bound 513045.7170 61802.57411
# Upper Bound 630898.1852 77197.65484
# Mean 248.4279 30.18639
# Lower Bound 222.8342 26.84307
# Upper Bound 274.0217 33.52971
yA <- with(DatA, data.frame(Feed, Clo, Lei))
yB <- with(DatB, data.frame(Feed, Clo, Lei))

## Bankier-Kalton-Anderson estimation and a 95% confidence
## interval for the three main variables
BKA(yA, yB, DatA$ProbA, DatB$ProbB, DatA$ProbB, DatB$ProbA, DatA$Domain,
DatB$Domain, 0.95)
# Estimation and 95 % Confidence Intervals:
# Feed Clo Lei
# Total 566434.3200 68959.26705 50953.07583
# Lower Bound 624569.2139 76538.11015 56036.23578
# Upper Bound 508299.4262 61380.42395 45869.91588
# Mean 247.8845 30.17814 22.29822
# Lower Bound 273.3257 33.49482 24.52273
# Upper Bound 222.4434 26.86147 20.07372
Compare(yA, yB, DatA$ProbA, DatB$ProbB, DatA$Domain, DatB$Domain)
# $Hartley
# Estimation:
# Feed Clo Lei
# Total 570867.8042 69473.86532 51284.2727
# $FullerBurmeister
# Estimation:
# Feed Clo Lei
# Mean 248.4279 30.18639 22.24236
# $PEL
indA <- as.integer(DatA$Domain == "ab")
indB <- as.integer(DatB$Domain == "ba")
Hartley(indA, indB, DatA$ProbA, DatB$ProbB, DatA$Domain, DatB$Domain)
# Estimation:
# [,1]
# Mean 0.2320545
BKA(indA, indB, DatA$ProbA, DatB$ProbB, DatA$ProbB, DatB$ProbA, DatA$Domain,
DatB$Domain)
# Estimation:
# [,1]
# Mean 0.2452491


# Mean 247.9484 30.17499 22.2746
# Total 571971.9511 69500.11448 51210.03819
# Estimation:
# Feed Clo Lei
# Total 1.791588e+08 2.663164e+06 1.455533e+06
# Mean 2.479314e+02 3.011373e+01 2.235969e+01
# $Calibration_DF
# Estimation:
# Feed Clo Lei
# Total 595162.2604 72214.13351 53108.5059
# Mean 248.8422 30.19332 22.2051

# Total 534.2743208
# Total 560.4121771
## SFRR estimator and CalSF estimator with frame sizes as auxiliary information
## using method "raking" for the calibration for the three main variables
SFRR (yA, yB, DatA$ProbA, DatB$ProbB, DatA$ProbB, DatB$ProbA, DatA$Domain,
DatB$Domain, N_A = 1735, N_B = 1191)
# Estimation:
# Feed Clo Lei
# Total 584713.4070 71086.18669 52423.74035
# Mean 248.2219 30.17743 22.25487
CalSF(yA, yB, DatA$ProbA, DatB$ProbB, DatA$ProbB, DatB$ProbA, DatA$Domain,
DatB$Domain, N_A = 1735, N_B = 1191, met = "raking")
# Estimation:
# Feed Clo Lei
# Mean 248.2219 30.17743 22.25487
# Total 584713.4070 71086.18669 52423.74035
## Estimates for the three main variables using PML, PEL and CalDF
## with frame sizes as auxiliary information in PEL and CalDF
PML(yA, yB, PiklA, PiklB, DatA$Domain, DatB$Domain, N_A = 1735, N_B = 1191)
# Estimation:
# Feed Clo Lei
# Total 593085.4467 72272.73759 53287.68044
# Mean 248.0966 30.23277 22.29104
PEL(yA, yB, PiklA, PiklB, DatA$Domain, DatB$Domain, N_A = 1735, N_B = 1191)
# Estimation:
# Feed Clo Lei
# Total 590425.4843 72211.61334 53258.38286
# Mean 247.4958 30.26982 22.32497
CalDF(yA, yB, PiklA, PiklB, DatA$Domain, DatB$Domain, N_A = 1735, N_B = 1191)
# Estimation:
# Feed Clo Lei
# Total 587502.4374 71368.45308 52490.98852
# Mean 248.7193 30.21385 22.22207
## Estimates for the three main variables using PEL estimator
## with frame sizes and overlap domain size as auxiliary information
PEL(yA, yB, PiklA, PiklB, DatA$Domain, DatB$Domain, N_A = 1735, N_B = 1191,
N_ab = 601)
# Estimation:
# Feed Clo Lei
# Total 575289.2186 70429.95642 51894.32490
# Mean 247.4362 30.29245 22.32014
## Calibration estimators with the same auxiliary information
## Estimates do not change when raking method is used for the calibration
CalSF(yA, yB, PiklA, PiklB, DatA$ProbB, DatB$ProbA, DatA$Domain, DatB$Domain,
N_A = 1735, N_B = 1191, N_ab = 601)
# Estimation:
# Feed Clo Lei
# Total 577163.6066 70173.20412 51726.19862
# Mean 248.2424 30.18202 22.24783
CalSF(yA, yB, PiklA, PiklB, DatA$ProbB, DatB$ProbA, DatA$Domain, DatB$Domain,
# Estimation:
# Feed Clo Lei
# Total 577163.6067 70173.20414 51726.19863
# Mean 248.2424 30.18202 22.24783
N_ab = 601)
# Estimation:
# Feed Clo Lei
N_A = 1735, N_B = 1191, N_ab = 601, met = "raking")
CalDF(yA, yB, PiklA, PiklB, DatA$Domain, DatB$Domain, N_A = 1735, N_B = 1191,
# Total 578691.1756 70246.32319 51600.78973
# Mean 248.8994 30.21347 22.19389
CalDF(yA, yB, PiklA, PiklB, DatA$Domain, DatB$Domain, N_A = 1735, N_B = 1191,
N_ab = 601, met = "raking")
# Estimation:
# Total 578691.1763 70246.32328 51600.78979
# Mean 248.8994 30.21347 22.19389
xsAFrameA = DatA$Inc, xsBFrameA = DatB$Inc, xsAFrameB = DatA$M2,
xsBFrameB = DatB$M2, XA = 4300260, XB = 176553)
# Estimation:
# Feed Clo Lei
## PEL, CalSF and CalDF estimators for the three main variables
## using Income as auxiliary variable in frame A and Metres2 as auxiliary
## variable in frame B assuming frame sizes known
PEL(yA, yB, PiklA, PiklB, DatA$Domain, DatB$Domain, N_A = 1735, N_B = 1191,
# Feed Clo Lei
# Total 587742.7193 71809.56826 53094.20112
# Mean 246.3713 30.10129 22.25614
CalSF(yA, yB, PiklA, PiklB, DatA$ProbB, DatB$ProbA, DatA$Domain, DatB$Domain,
N_A = 1735, N_B = 1191, xsAFrameA = DatA$Inc, xsBFrameA = DatB$Inc,
xsAFrameB = DatA$M2, xsBFrameB = DatB$M2, XA = 4300260, XB = 176553)
# Estimation:
# Feed Clo Lei
# Total 582398.3181 70897.88438 52252.24741
# Mean 247.5819 30.13922 22.21282
CalDF(yA, yB, PiklA, PiklB, DatA$Domain, DatB$Domain, N_A = 1735, N_B = 1191,
xsAFrameA = DatA$Inc, xsBFrameA = DatB$Inc, xsAFrameB = DatA$M2,
xsBFrameB = DatB$M2, XA = 4300260, XB = 176553)
# Feed Clo Lei
# Total 585185.4497 71194.61148 52346.43878
# Mean 247.8075 30.14866 22.16705
## Now, assume that overlap domain size is also known
PEL(yA, yB, PiklA, PiklB, DatA$Domain, DatB$Domain, N_A = 1735, N_B = 1191,
xsAFrameB = DatA$M2, xsBFrameB = DatB$M2, XA = 4300260, XB = 176553)
# Estimation:
# Feed Clo Lei
# Estimation:
N_ab = 601, xsAFrameA = DatA$Inc, xsBFrameA = DatB$Inc,
# Total 572611.6997 69991.74803 51737.56089
# Mean 246.2846 30.10398 22.25271
CalSF(yA, yB, PiklA, PiklB, DatA$ProbB, DatB$ProbA, DatA$Domain, DatB$Domain,
N_A = 1735, N_B = 1191, N_ab = 601, xsAFrameA = DatA$Inc, xsBFrameA = DatB$Inc,
# Feed Clo Lei
# Total 575636.7876 70076.78485 51628.27583
# Mean 247.5857 30.14055 22.20571
CalDF(yA, yB, PiklA, PiklB, DatA$Domain, DatB$Domain, N_A = 1735, N_B = 1191,
# Estimation:
xsAFrameB = DatA$M2, xsBFrameB = DatB$M2, XA = 4300260, XB = 176553)
N_ab = 601, xsAFrameA = DatA$Inc, xsBFrameA = DatB$Inc,
xsAFrameB = DatA$M2, xsBFrameB = DatB$M2, XA = 4300260, XB = 176553)
# Estimation:
# Feed Clo Lei
# Total 576630.7609 70102.0037 51477.16737
# Mean 248.0132 30.1514 22.14072
## Confidence intervals through jackknife for the three main variables
## for estimators defined under the so called single frame approach with
## a stratified random sampling in frame A and a simple random sampling
## without replacement in frame B. Finite population correction factor
## is required for frame A
JackBKA (yA, yB, DatA$ProbA, DatB$ProbB, DatA$ProbB, DatB$ProbA, DatA$Domain,
DatB$Domain, conf_level = 0.95, sdA = "str", strA = DatA$Stratum, fcpA = TRUE)
# Feed Clo Lei
# Total 566434.3200 68959.26705 50953.07583
# Jack Upper End 610992.1346 74715.89841 54717.32664
# Jack Lower End 521876.5055 63202.63570 47188.82502
# Mean 247.8845 30.17814 22.29822
# Jack Upper End 267.3840 32.69738 23.94555
# Jack Lower End 228.3850 27.65891 20.65090
JackSFRR(yA, yB, DatA$ProbA, DatB$ProbB, DatA$ProbB, DatB$ProbA, DatA$Domain,
DatB$Domain, N_A = 1735, N_B = 1191, conf_level = 0.95, sdA = "str",
strA = DatA$Stratum, fcpA = TRUE)
# Feed Clo Lei
# Total 584713.4070 71086.18669 52423.74035
# Jack Lower End 549467.7802 65595.62751 49642.80309
# Mean 248.2219 30.17743 22.25487
# Jack Upper End 263.1843 32.50828 23.43543
# Jack Lower End 233.2595 27.84659 21.07431
JackCalSF(yA, yB, DatA$ProbA, DatB$ProbB, DatA$ProbB, DatB$ProbA, DatA$Domain,
strA = DatA$Stratum, fcpA = TRUE)
# Feed Clo Lei
# Total 577163.6066 70173.20412 51726.19862
# Jack Upper End 599105.4275 73516.53187 53165.97439
# Jack Upper End 619959.0338 76576.74587 55204.67760
DatB$Domain, N_A = 1735, N_B = 1191, N_ab = 601, conf_level = 0.95, sdA = "str",
# Jack Lower End 555221.7858 66829.87636 50286.42285
# Mean 248.2424 30.18202 22.24783
# Jack Upper End 257.6798 31.62001 22.86709
# Jack Lower End 238.8051 28.74403 21.62857
JackHartley (yA, yB, DatA$ProbA, DatB$ProbB, DatA$Domain, DatB$Domain,
# Feed Clo Lei
# Total 570867.8042 69473.86532 51284.27265
# Jack Upper End 610664.7131 74907.33129 54782.33083
# Jack Lower End 531070.8954 64040.39934 47786.21447
conf_level = 0.95, sdA = "str", strA = DatA$Stratum, fcpA = TRUE)
## Same for a selection of dual frame estimators
# Mean 247.9484 30.17499 22.27460
# Jack Upper End 265.2336 32.53494 23.79393
JackPML(yA, yB, DatA$ProbA, DatB$ProbB, DatA$Domain, DatB$Domain,
N_A = 1735, N_B = 1191, conf_level = 0.95, sdA = "str", strA = DatA$Stratum,
fcpA = TRUE)
# Feed Clo Lei
# Jack Lower End 230.6631 27.81504 20.75527
# Total 594400.6320 72430.05834 53408.30337
# Jack Upper End 626443.7529 76885.06491 56003.77592
# Jack Lower End 562357.5111 67975.05176 50812.83082
# Mean 248.0934 30.23115 22.29178
# Jack Upper End 261.4677 32.09060 23.37509
# Jack Lower End 234.7191 28.37171 21.20847
JackCalDF(yA, yB, DatA$ProbA, DatB$ProbB, DatA$Domain, DatB$Domain, N_A = 1735,
N_B = 1191, N_ab = 601, conf_level = 0.95, sdA = "str", strA = DatA$Stratum,
fcpA = TRUE)
# Feed Clo Lei
# Total 578895.6961 70230.11306 51570.55683
# Jack Upper End 601626.7000 73614.66702 53037.42260
# Jack Lower End 556164.6921 66845.55910 50103.69107
# Mean 248.9874 30.20650 22.18088
# Jack Upper End 258.7642 31.66222 22.81179
# Jack Lower End 239.2106 28.75078 21.54997
data(Dat)
head(Dat, 3)
# Drawnby Stratum Opinion Landline Cell ProbLandline ProbCell
# 1 1 2 0 1 1 0.000673623 8.49e-05
## "ab" or "ba"

# 2 1 5 1 1 1 0.002193297 5.86e-05
# 3 1 1 0 1 1 0.001831489 7.81e-05
attach(Dat)
## We can split the original dataset in four new different
## datasets, each one corresponding to one domain.

DomainOnlyLandline <- Dat[Landline == 1 & Cell == 0,]
DomainBothLandline <- Dat[Drawnby == 1 & Landline == 1 & Cell == 1,]
DomainOnlyCell <- Dat[Landline == 0 & Cell == 1,]
DomainBothCell <- Dat[Drawnby == 2 & Landline == 1 & Cell == 1,]

## From the domain datasets, we can build frame datasets

FrameLandline <- rbind(DomainOnlyLandline, DomainBothLandline)
FrameCell <- rbind(DomainOnlyCell, DomainBothCell)

## Finally, we only need to label domain of each unit using "a", "b",
Domain <- c(rep("a", nrow(DomainOnlyLandline)), rep("ab", nrow(DomainBothLandline)))
FrameLandline <- cbind(FrameLandline, Domain)

Domain <- c(rep("b", nrow(DomainOnlyCell)), rep("ba", nrow(DomainBothCell)))
FrameCell <- cbind(FrameCell, Domain)
summary(PML(FrameLandline$Opinion, FrameCell$Opinion, FrameLandline$ProbLandline,
FrameCell$ProbCell, FrameLandline$Domain, FrameCell$Domain, N_A = 4982920,
N_B = 5707655))
# Call:
# PML(ysA = FrameLandline$Opinion, ysB = FrameCell$Opinion,
# pi_A = FrameLandline$ProbLandline, pi_B = FrameCell$ProbCell,
# domains_A = FrameLandline$Domain, domains_B = FrameCell$Domain,
# N_A = 4982920, N_B = 5707655)
# Estimation:
# [,1]
# Total 3.231325e+06
# Mean 4.635634e-01
# Variance Estimation:
# [,1]
# Var. Total 1.784362e+10
# Var. Mean 3.672317e-04
# [,1]
# Total dom. a 219145.1
# Total dom. ab 2318841.9
# Total dom. b 1346646.1
# Total dom. ba 1457501.0
# Mean Domain Estimations:
# [,1]
# Mean dom. a 0.4438149
# Mean dom. ab 0.4990548
# Total Domain Estimations:
# Total 2.986787e+06
# Mean 4.702923e-01
# Mean dom. b 0.4172797
# Mean dom. ba 0.4674919
# Parameters:
# gamma 0.3211534
summary(CalDF(FrameLandline$Opinion, FrameCell$Opinion, FrameLandline$ProbLandline,
FrameCell$ProbCell, FrameLandline$Domain, FrameCell$Domain, N_A = 4982920,
N_B = 5707655, N_ab = 4339659))
# Call:
# CalDF(ysA = FrameLandline$Opinion, ysB = FrameCell$Opinion,
# pi_A = FrameLandline$ProbLandline, pi_B = FrameCell$ProbCell,
# domains_A = FrameLandline$Domain, domains_B = FrameCell$Domain,
# N_A = 4982920, N_B = 5707655, N_ab = 4339659)
# Estimation:
# [,1]
# Total 2.985028e+06
# Mean 4.700153e-01
# Variance Estimation:
# [,1]
# Var. Total 1.478990e+10
# Var. Mean 3.666844e-04
# Parameters:
# eta 0.7296841

summary(CalSF(FrameLandline$Opinion, FrameCell$Opinion, FrameLandline$ProbLandline,
FrameCell$ProbCell, FrameLandline$ProbCell, FrameCell$ProbLandline,
FrameLandline$Domain, FrameCell$Domain, N_A = 4982920, N_B = 5707655,
N_ab = 4339659))
# Call:
# CalSF(ysA = FrameLandline$Opinion, ysB = FrameCell$Opinion,
# pi_A = FrameLandline$ProbLandline, pi_B = FrameCell$ProbCell,
# pik_ab_B = FrameLandline$ProbCell, pik_ba_A = FrameCell$ProbLandline,
# domains_A = FrameLandline$Domain, domains_B = FrameCell$Domain, N_A = 4982920,
# N_B = 5707655, N_ab = 4339659)
# Estimation:
# [,1]
# Variance Estimation:
# [,1]
# Var. Total 1.442969e+10
# Var. Mean 3.577539e-04
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# T
# T
# e
# e
# e
# e
# e
# e
# q
# ∑
# ∑
# ∑
# ∑
# ∑
# ∑
# ∑
# ∑
# ?
# ?
# ?
# ×
# √
# β
# β
# = −
