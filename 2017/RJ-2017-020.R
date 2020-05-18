library(MCI)
data(shopping1)
# The survey dataset
data(shopping2)
# Dataset with distances and travel times
# 3 20-30 283.00 7.566339 478.0 5.662568
# 4 Other 58.00 1.550698 1011.0 11.976686
shopping1_adj <- shopping1[(shopping1$weekday != 3) & (shopping1$holiday != 1)
& (shopping1$survey != "pretest"),]
# Removing every case from tuesday, holidays and the ones belonging to the pretest
ijmatrix <- ijmatrix.create(shopping1_adj, "resid_code", "POS", "POS_expen")
# Creates an interaction matrix based on the observed frequencies (automatically)
# and the POS expenditures (Variable "POS_expen" separately stated)
ijmatrix
# interaction resid_code POS freq_ij_abs freq_i_total p_ij_obs
# 1 resid1-POS1 resid1 POS1 91 113 0.8053097
# 2 resid1-POS2 resid1 POS2 22 113 0.1946903
# 3 resid10-POS1 resid10 POS1 3 7 0.4285714
# ...
# freq_ij_abs_POS_expen freq_i_total_POS_expen p_ij_obs_POS_expen
# 1 2318.25 3245.25 0.71435174
# 2 927.00 3245.25 0.28564826
# 3 30.00 328.00 0.09146341
# ...
ijmatrix_dist <- merge(ijmatrix, shopping2, by.x = "interaction", by.y = "route",
all.x = TRUE)
# Adding the distances and travel times
visit <- shares.segm(ijmatrix_dist, "resid_code", "POS", "d_time", "freq_ij_abs",
0, 10, 20, 30)
# Segmentation by travel time using the number of customers/visitors
# Parameters: interaction matrix (data frame), columns with origins and destinations,
# variable to divide in classes, absolute frequencies/expenditures, class segments
expen <- shares.segm(ijmatrix_dist, "resid_code", "POS", "d_time",
"freq_ij_abs_POS_expen", 0, 10, 20, 30)
# Segmentation by travel time using the POS expenditures
visit
# d_time_class POS1_abs POS1_rel POS2_abs POS2_rel
# 1 0-10 108 72.483221 58 40.277778
# 2 10-20 24 16.107383 62 43.055556
# 3 20-30 10 6.711409 12 8.333333
# 4 Other 7 4.697987 12 8.333333
expen
# d_time_class POS1_abs POS1_rel POS2_abs POS2_rel
# 1 0-10 2858.25 76.418689 2900.0 34.354491
# 2 10-20 541.00 14.464274 4052.4 48.006255
ijmatrix_dist$freq_ij_abs_cor <- var.correct(ijmatrix_dist$freq_ij_abs,
corr.mode = "inc", incby = 0.1)
# Correcting the absolute values (frequencies) by increasing by 0.1
data(shopping3)
ijmatrix_alldata <- merge(ijmatrix_dist, shopping3)
# Adding the information about the origins (places of residence) stored in shopping3
ijmatrix_alldata$visitper1000 <- (ijmatrix_alldata$freq_ij_abs_cor /
ijmatrix_alldata$resid_pop2015) * 1000
# Calculating the dependent variable
# visitper1000: surveyed customers per 1,000 inhabitants of the origin
ijmatrix_alldata <- ijmatrix_alldata[(!is.na(ijmatrix_alldata$visitper1000))
& (!is.na(ijmatrix_alldata$d_time)),]
# Removing NAs (data for some outlier origins and routes not available)
POS1 <- ijmatrix_alldata[ijmatrix_alldata$POS == "POS1",]
# Dataset for POS1 (town centre)
POS2 <- ijmatrix_alldata[ijmatrix_alldata$POS == "POS2",]
# Dataset for POS2 (out-of-town shopping centre)
huff.decay(POS1, "d_km", "visitper1000")
# Model type Intercept p Intercept Slope p Slope R-Squared Adj. R-squared
# 1 Linear 0.7354 2e-04 -0.0455 0.0038 0.216 0.1936
# 2 Power 1.9121 0.4434 -1.5267 2e-04 0.333 0.3139
# 3 Exponential 0.2788 0.0222 -0.1353 0.0044 0.2092 0.1866
# 4 Logistic 1.6999 0.0164 0.1823 0.0026 0.2311 0.2092
huff.decay(POS1, "d_time", "visitper1000")
# Model type Intercept p Intercept Slope p Slope R-Squared Adj. R-squared
# 1 Linear 1.2112 0 -0.0585 1e-04 0.3516 0.3331
# 2 Power 34.6019 0.0289 -2.2968 3e-04 0.3211 0.3017
# 3 Exponential 0.691 0.6322 -0.1432 0.0027 0.23 0.208
# 4 Logistic 0.2659 0.7795 0.2058 6e-04 0.2893 0.2689
huff.decay(POS2, "d_km", "visitper1000")
# Model type Intercept p Intercept Slope p Slope R-Squared Adj. R-squared
# 1 Linear 0.6734 0 -0.0316 7e-04 0.2812 0.2606
# 2 Power 5.7372 0.0348 -1.5978 0 0.3863 0.3688
# 3 Exponential 0.9161 0.8318 -0.1605 0 0.4086 0.3917
# 4 Logistic -0.4868 0.4212 0.2043 1e-04 0.3444 0.3256
huff.decay(POS2, "d_time", "visitper1000")
# Model type Intercept p Intercept Slope p Slope R-Squared Adj. R-squared
# 1 Linear 0.9353 0 -0.0411 1e-04 0.3706 0.3526
# 2 Power 213.9932 7e-04 -2.7363 0 0.4213 0.4048
# 3 Exponential 2.3946 0.14 -0.184 0 0.4191 0.4025
# 4 Logistic -1.8572 0.031 0.2441 0 0.3835 0.3659
# 3 0.0000000000
# ...
ijmatrix <- ijmatrix.create(shopping1_KAeast, "resid_code", "gro_purchase_code",
                            "gro_purchase_expen")
ijmatrix
# interaction resid_code gro_purchase_code freq_ij_abs freq_i_total p_ij_obs
# 2 resid1-ALDI11 resid1 ALDI11 0 186 0.000000000
# 3 resid1-ALDI2 resid1 ALDI2 0 186 0.000000000
# ...
# freq_ij_abs_gro_purchase_expen freq_i_total_gro_purchase_expen
# 1 420.0 5270.0
# 2 0.0 5270.0
# ...
# p_ij_obs_gro_purchase_expen
# 1 0.0796963947
# 2 0.0000000000
data(shopping1)
# Survey dataset
data(shopping3)
# Dataset containing information about the city districts
data(shopping4)
# Dataset containing the grocery stores
shopping1_KAeast <- shopping1[shopping1$resid_code %in%
shopping3$resid_code[shopping3$KA_east == 1],]
# Extracting only inhabitants of the eastern districts of Karlsruhe

# 1 resid1-ALDI1 resid1 ALDI1 10 186 0.053763441
# 3 0.0 5270.0
shares.total(ijmatrix, "resid_code", "gro_purchase_code", "p_ij_obs",
"freq_i_total")
# Total values for the shopping trips
# suppliers_single sum_E_j share_j
# 1 ALDI1 11 0.039426523
# 2 ALDI11 1 0.003584229
# 3 ALDI2 3 0.010752688
# 4 ALDI4 1 0.003584229
# ...
shares.total(ijmatrix, "resid_code", "gro_purchase_code",
"p_ij_obs_gro_purchase_expen", "freq_i_total_gro_purchase_expen")
# Total values for the shopping expenditures
# suppliers_single sum_E_j share_j
# 1 ALDI1 470.0 0.0492378608
# 2 ALDI11 60.0 0.0062856844
# 3 ALDI2 87.0 0.0091142423
# 4 ALDI4 80.0 0.0083809125
# ...
ijmatrix_adj <- ijmatrix.create(shopping1_KAeast, "resid_code",
"gro_purchase_code", "gro_purchase_expen",
remSing = TRUE, remSing.val = 1,
remSingSupp.val = 2, correctVar = TRUE,
correctVar.val = 0.1)
# Removing singular instances/outliers (remSing = TRUE) incorporating
# only suppliers which are at least obtained three times (remSingSupp.val = 2)
# Correcting the values (correctVar = TRUE)
# by adding 0.1 to the absolute values (correctVar.val = 0.1)
ijmatrix_adj <- ijmatrix_adj[(ijmatrix_adj$gro_purchase_code != "REFORMHAUSBOESER") &
(ijmatrix_adj$gro_purchase_code != "WMARKT_DURLACH") &
(ijmatrix_adj$gro_purchase_code != "X_INCOMPLETE_STORE"),]
# Remove non-regarded observations
ijmatrix_adj
# interaction resid_code gro_purchase_code freq_ij_abs freq_i_total
# 1 resid1-ALDI1 resid1 ALDI1 10.1 172.4
# 2 resid1-ALDI2 resid1 ALDI2 0.1 172.4
# 3 resid1-CAP1 resid1 CAP1 18.1 172.4
# ...
# p_ij_obs freq_ij_abs_gro_purchase_expen freq_i_total_gro_purchase_expen
# 1 0.0585846868 420.1 4745.4
# 2 0.0005800464 0.1 4745.4
# 3 0.1049883991 224.1 4745.4
# ...
# p_ij_obs_gro_purchase_expen
# 1 8.852784e-02
# 2 2.107304e-05
# 3 4.722468e-02
# ...
ijmatrix_dist <- merge (ijmatrix_adj, shopping2, by.x="interaction", by.y="route")
# Include the distances and travel times (shopping2)
ijmatrix_alldata <- merge (ijmatrix_dist, shopping4, by.x = "gro_purchase_code",
by.y = "location_code")
# Adding the store information (shopping4)
ijmatrix_transf <- mci.transmat(ijmatrix_alldata, "resid_code", "gro_purchase_code",
"p_ij_obs", "d_time", "salesarea_qm")
ijmatrix_transf
# resid_code gro_purchase_code p_ij_obs_t d_time_t salesarea_qm_t
# 1 resid1 ALDI1 0.5586409 0.060847455 -0.193400118
# 14 resid1 ALDI2 -1.4456805 -0.008788473 -0.193400118
# 23 resid1 CAP1 0.8119981 -0.091762709 -0.545582636
# ...
# 109 resid29 REWE1 -0.1893441 -0.060611802 -0.001514591
# 119 resid29 TREFF1 -0.1893441 -0.027694956 -0.169919022
# 6 resid3 ALDI1 -0.2768831 0.058442499 -0.193400118
# ...
# Call:
# lm(formula = mci_formula, data = mciworkfile)
# Residuals:
# Min 1Q Median 3Q Max
# -1.27457 -0.28725 -0.02391 0.32163 1.29351
# summary(mci_expen)
# Call:
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)
# d_time_t -1.2443 0.2319 -5.367 4.02e-07 ***
# salesarea_qm_t 0.9413 0.1158 8.132 4.59e-13 ***
# ---
# Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 0.4458 on 119 degrees of freedom
# Multiple R-squared: 0.4603, Adjusted R-squared: 0.4512
# F-statistic: 50.74 on 2 and 119 DF, p-value: < 2.2e-16
mci_trips <- mci.fit(ijmatrix_alldata, "resid_code", "gro_purchase_code", "p_ij_obs",
                     "d_time", "salesarea_qm")

summary(mci_trips)

# shares: "p_ij_obs", explanatory variables: "d_time", "salesarea_qm"

mci_expen <- mci.fit(ijmatrix_alldata, "resid_code", "gro_purchase_code",
"p_ij_obs_gro_purchase_expen", "d_time", "salesarea_qm")

# Residuals:
# Min 1Q Median 3Q Max
# -2.07495 -0.61794 -0.07452 0.70263 2.60085
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)
# d_time_t -2.3788 0.4517 -5.267 6.26e-07 ***
# salesarea_qm_t 2.0409 0.2255 9.051 3.30e-15 ***
# ---
# Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 0.8683 on 119 degrees of freedom
# Multiple R-squared: 0.4954, Adjusted R-squared: 0.487
# F-statistic: 58.43 on 2 and 119 DF, p-value: < 2.2e-16
expen <- mci.shares(ijmatrix_alldata, "resid_code", "gro_purchase_code",
"salesarea_qm", 2.0409, "d_time", -2.3788)
# MCI market share prediction with two variables
# salesarea_qm (weighting power function with exponent equal to 2.0409)
# d_time (weighting power function with exponent equal to -2.3788)
expen
# gro_purchase_code interaction resid_code
# 1 ALDI1 resid1-ALDI1 resid1
# 14 ALDI2 resid1-ALDI2 resid1
# 23 CAP1 resid1-CAP1 resid1
# ...
# d_time salesarea_qm storetype_dc store_chain
# 1 5.4 900 1 Aldi
# 14 4.6 900 1 Aldi
# 23 3.8 400 0 Edeka
# ...
# U_ij sum_U_ij p_ij
# 1 1.936847e+04 9682538 2.000350e-03
# 14 2.836255e+04 9682538 2.929247e-03
# 23 8.537978e+03 9682538 8.817913e-04
# ...
chain <- var.asdummy(ijmatrix_alldata$store_chain)
# Converting the character vector (column store_chain) to dummy variables
# and storing in a new data frame
chain
# Aldi_DUMMY Edeka_DUMMY Lidl_DUMMY Netto_DUMMY Real_DUMMY Rewe_DUMMY Treff 3000_DUMMY
# 1 1 0 0 0 0 0 0
# 2 1 0 0 0 0 0 0
# ...
# 66 0 1 0 0 0 0 0
# 67 0 0 1 0 0 0 0
# ...
ijmatrix_alldata <- cbind(ijmatrix_alldata, chain)
# Add dummy dataset to interaction matrix
mci_expen2 <- mci.fit(ijmatrix_alldata, "resid_code", "gro_purchase_code",
"p_ij_obs_gro_purchase_expen", "d_time", "salesarea_qm",
"Aldi_DUMMY", "Edeka_DUMMY", "Lidl_DUMMY", "Netto_DUMMY",
"Real_DUMMY", "Rewe_DUMMY")
# Same model as above with additional dummy variables
summary(mci_expen2)
# Call:
# lm(formula = mci_formula, data = mciworkfile)
# Residuals:
# Min 1Q Median 3Q Max
# -2.1601 -0.4338 -0.1041 0.2561 2.5342
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)
# d_time_t -2.56243 0.41753 -6.137 1.28e-08 ***
# salesarea_qm_t 1.31622 0.30754 4.280 3.94e-05 ***
# Aldi_DUMMY -0.05658 0.17995 -0.314 0.753763
# Edeka_DUMMY 0.11873 0.12051 0.985 0.326637
# Lidl_DUMMY -0.59177 0.24441 -2.421 0.017060 *
# Netto_DUMMY -0.19785 0.25719 -0.769 0.443330
# Real_DUMMY 1.32882 0.33093 4.015 0.000107 ***
# Rewe_DUMMY -0.38429 0.24012 -1.600 0.112299
# ---
# Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 0.7955 on 113 degrees of freedom
# Multiple R-squared: 0.5978, Adjusted R-squared: 0.5694
# F-statistic: 21 on 8 and 113 DF, p-value: < 2.2e-16
expen2 <- mci.shares(ijmatrix_alldata, "resid_code", "gro_purchase_code",
"salesarea_qm", 1.31622, "d_time", -2.56243, "Lidl_DUMMY", -0.59177,
"Real_DUMMY", 1.32882, mcitrans = "ilc")
# MCI market share prediction with four variables
# (ratio-scaled variables are log-centering transformed)
# salesarea_qm (multiplicative weighting with factor equal to 1.31622)
# d_time (multiplicative weighting with factor equal to -2.56243)
# Lidl_DUMMY (multiplicative weighting with factor equal to -0.59177)
# Real_DUMMY (multiplicative weighting with factor equal to 1.32882)
shares.total(expen2, "resid_code", "gro_purchase_code", "p_ij",
"freq_ij_abs_gro_purchase_expen")
# Expected total sales and shares based on the observed local
# market potential (sum of all obtained expenditures for each origin)
# suppliers_single sum_E_j share_j
# 1 ALDI1 16.029089 0.008058517
# 2 ALDI2 2.157771 0.001084805
# 3 CAP1 7.101650 0.003570307
# 4 EDEKA1 790.385662 0.397361072
# 5 EDEKA2 34.432270 0.017310592
# 6 EDEKA3 25.098495 0.012618099
# 7 LIDL1 40.925354 0.020574946
# 8 NETTO1 2.063759 0.001037541
# 9 REAL1 1019.935563 0.512765741
# 10 REWE1 35.451053 0.017822778
# 11 TREFF1 15.506130 0.007795602
# Market area estimation using the Huff Model with standard parameters
# (gamma = 1, lambda = -2)
data("Freiburg1")
huff_mat <- huff.shares (Freiburg1, "district", "store", "salesarea", "distance")
ijmatrix_alldata[ijmatrix_alldata$gro_purchase_code == "REAL1",]$salesarea_qm <- 8525
# Replacing the sales area of REAL1 with a new value: 8525 sqm (increase of 10 %)
expen2_new <- mci.shares(ijmatrix_alldata, "resid_code",
"gro_purchase_code", "salesarea_qm", 1.31622,
"d_time", -2.56243,
"Lidl_DUMMY", -0.59177, "Real_DUMMY", 1.32882,
mcitrans = "ilc")
shares.total(expen2_new, "resid_code", "gro_purchase_code", "p_ij",
"freq_ij_abs_gro_purchase_expen")
# suppliers_single sum_E_j share_j
# 1 ALDI1 15.787692 0.007886640
# 2 ALDI2 2.097968 0.001048026
# 3 CAP1 6.995287 0.003494451
# 4 EDEKA1 778.019361 0.388654556
# 5 EDEKA2 33.873811 0.016921444
# 6 EDEKA3 24.365708 0.012171732
# 7 LIDL1 40.262537 0.020112891
# 8 NETTO1 2.032720 0.001015432
# 9 REAL1 1048.206517 0.523624808
# 10 REWE1 34.910285 0.017439208
# 11 TREFF1 15.275568 0.007630811

# Distance matrix and sales area
data(Freiburg2)
# Grocery purchasing power on the city district level
huff_mat_pp <- merge (huff_mat, Freiburg2)
# Adding the purchasing power data for the city districts
huff_total <- shares.total (huff_mat_pp, "district", "store", "p_ij", "ppower")
# Total expected sales and shares
huff_total
# suppliers_single sum_E_j share_j
# 1 1 4057591 0.010759819
# 2 10 5809861 0.015406444
# 3 11 1289847 0.003420383
# 4 12 7103210 0.018836115
# 5 13 3476313 0.009218400
# ...
data(Freiburg3)
# Annual sales of the grocery stores
huff_total_control <- merge(huff_total, Freiburg3, by.x = "suppliers_single",
by.y = "store")
model.fit(huff_total_control$annualsales, huff_total_control$sum_E_j, plotVal = TRUE)
# $resids_sq_sum
# [1] 2.125162e+15
# $pseudorsq
# [1] 0.5128422
# $globerr
# [1] 0.5210329
# $mape
# [1] 0.6383766
huff_total_opt1 <- huff.attrac(Freiburg1, "district", "store", "salesarea", "distance",
                               lambda = -2, dtype= "pow", lambda2 = NULL,
                               
Freiburg2, "district", "ppower",
Freiburg3, "store", "annualsales",
tolerance = 5, output = "total")
# One-time optimization (one iteration) with an accepted difference of +/- 5 %
# Output of total sales/shares
model.fit(huff_total_opt2$total_obs, huff_total_opt2$sum_E_j, plotVal = TRUE)
huff_total_opt1
# suppliers_single sum_E_j share_j total_obs diff attrac_new_opt
# 1 1 7097226 0.018820246 7210720 113494.39 1329.26622
# 2 10 5043058 0.013373057 5600000 556941.50 1400.00000
# 3 11 1136322 0.003013270 1000000 -136321.73 193.82148
# 4 12 2849360 0.007555862 2776000 -73360.04 271.22161
...
model.fit(huff_total_opt1$total_obs, huff_total_opt1$sum_E_j, plotVal = TRUE)
# total_obs = observed total values, originally from dataset Freiburg3
# sum_E_j = expected total values
# $resids_sq_sum
# [1] 2.901841e+14
# $pseudorsq
# [1] 0.9334801
# $globerr
# [1] 0.1564878
# $mape
# [1] 0.1620126
huff_total_opt2 <- huff.fit(Freiburg1, "district", "store", "salesarea", "distance",
lambda = -2, dtype= "pow", lambda2 = NULL,
Freiburg2, "district", "ppower",
Freiburg3, "store", "annualsales",
tolerance = 1, iterations = 2, output = "total",
show_proc = TRUE)
# 2 iterations of the optimization algorithm with an accepted difference of +/- 1 %
# Output of total sales/shares, stored in dataset huff_total_opt2
# printing of status messages:
# Iteration 1 of 2 ...
# Processing location 1 ...
# ...
# iterations_count resids_sq_sum pseudorsq globerr mape
# 1 1 2.886177e+14 0.9338392 0.155826392 0.162228371
# # total_obs = observed total values, originally from dataset Freiburg3
# # sum_E_j = expected total values
# $resids_sq_sum
# [1] 4.806282e+13
# $pseudorsq
# [1] 0.9889824
huff.fit(Freiburg1, "district", "store", "salesarea", "distance", lambda = -2,
Freiburg2, "district", "ppower",
Freiburg3, "store", "annualsales",
tolerance = 1, iterations = 10, output = "diag", show_proc = TRUE)
dtype= "pow", lambda2 = NULL,
$globerr
[1] 0.05946104
$mape
[1] 0.0618133
huff_total_opt10 <- huff.fit(Freiburg1, "district", "store", "salesarea", "distance",
lambda = -2, dtype= "pow", lambda2 = NULL,
Freiburg2, "district", "ppower",
Freiburg3, "store", "annualsales",
tolerance = 1, iterations = 10, output = "total",
show_proc = TRUE)
# 10 iterations of the optimization algorithm with an accepted difference of +/- 1 %
# Output of total sales/shares, stored in dataset huff_total_opt10
# with printing of status messages
model.fit(huff_total_opt10$total_obs, huff_total_opt10$sum_E_j, plotVal = TRUE)
# $resids_sq_sum
# [1] 185646134781
# $pseudorsq
# [1] 0.9999574
# $globerr
# [1] 0.004508996
# $mape
# [1] 0.004405252
# 2 2 4.806282e+13 0.9889824 0.059461039 0.061813302
# 3 3 9.936652e+12 0.9977222 0.031229899 0.032170472
# 4 4 2.318130e+12 0.9994686 0.018162793 0.019737125
# 5 5 6.378060e+11 0.9998538 0.010563610 0.011844831
# 6 6 5.567110e+11 0.9998724 0.007654992 0.007507305
# 7 7 1.776295e+11 0.9999593 0.005357527 0.005444095
# 8 8 1.778885e+11 0.9999592 0.004689937 0.004669445
# 9 9 1.828774e+11 0.9999581 0.004587815 0.004541543
# 10 10 1.856461e+11 0.9999574 0.004508996 0.004405252
