# gsym.point(methods, data, marker, status, tag.healthy,
# categorical.cov = NULL, control = control.gsym.point(),
# CFN = 1, CFP = 1, confidence.level = 0.95,
# trace = FALSE, seed = FALSE, value.seed = 3, verbose = TRUE)
# summary(object, ...)
# plot(x, legend = TRUE, ...)
library("GsymPoint")
data("melanoma")
summary(melanoma)
# X group
# Min. :-5.88100 Min. :0.0000
# 1st Qu.:-3.22100 1st Qu.:0.0000
# Median :-1.69550 Median :0.0000
# Mean :-1.55642 Mean :0.2917
# 3rd Qu.: 0.00675 3rd Qu.:1.0000
# Max. : 3.03200 Max. :1.0000
melanoma.cutpoint1 <- gsym.point(methods = "GPQ", data = melanoma,
marker = "X", status = "group", tag.healthy = 0,
categorical.cov = NULL, CFN = 2, CFP = 1,
control = control.gsym.point(), confidence.level = 0.95,
trace = FALSE, seed = TRUE, value.seed = 3, verbose = TRUE)
names(melanoma.cutpoint1)
# [1] "GPQ" "methods" "call" "data"
names(melanoma.cutpoint1$GPQ)
# [1] "Global"
names(melanoma.cutpoint1$GPQ$Global)
# [1] "optimal.result" "AUC" "rho"
# [4] "pvalue.healthy" "pvalue.diseased"
melanoma.cutpoint1$GPQ
# $Global
# $Global$optimal.result
# $Global$optimal.result$cutoff
# Value ll ul
# 1 -1.213237 -1.792908 -0.6283236
# $Global$optimal.result$Specificity
# Value ll ul
# 1 0.75465 0.6249716 0.8485824
# $Global$optimal.result$Sensitivity
# Value ll ul
# 1 0.877325 0.8124858 0.9242912
# $Global$AUC
# [1] 0.9056956
# $Global$rho
# [1] 0.5
# $Global$pvalue.healthy
# [1] 0.4719117
# $Global$pvalue.diseased
# [1] 0.9084176
melanoma.cutpoint1$GPQ$Global$AUC
# [1] 0.9056956
summary(melanoma.cutpoint1)
# *************************************************
# OPTIMAL CUTOFF: GENERALIZED SYMMETRY POINT
# *************************************************
# Call:
# gsym.point(methods = "GPQ", data = melanoma, marker = "X", status = "group",
# tag.healthy = 0, categorical.cov = NULL, CFN = 2, CFP = 1,
# control = control.gsym.point(), confidence.level = 0.95,
# trace = FALSE, seed = TRUE, value.seed = 3, verbose = TRUE)
# According to the Shapiro-Wilk normality test, the marker can be
# considered normally distributed in both groups.
# Shapiro-Wilk test p-values
# Group 0 Group 1
# Original marker 0.4719 0.9084
# Area under the ROC curve (AUC): 0.906
# METHOD: GPQ
# Estimate 95% CI lower limit 95% CI upper limit
# cutoff -1.213237 -1.7929079 -0.6283236
# Specificity 0.754650 0.6249716 0.8485824
# Sensitivity 0.877325 0.8124858 0.9242912
melanoma.cutpoint2 <- gsym.point(methods = "EL", data = melanoma,
marker = "X", status = "group", tag.healthy = 0,
categorical.cov = NULL, CFN = 2, CFP = 1,
control = control.gsym.point(), confidence.level = 0.95,
trace = FALSE, seed = TRUE, value.seed = 3, verbose = TRUE)
# According to the Shapiro-Wilk normality test, the marker can be
# considered normally distributed in both groups.
# Therefore the GPQ method would be more suitable for this dataset.
# Shapiro-Wilk test p-values
# Group 0 Group 1
# Original marker 0.4719 0.9084
summary(melanoma.cutpoint2)
# *************************************************
# OPTIMAL CUTOFF: GENERALIZED SYMMETRY POINT
# *************************************************
# Call:
# gsym.point(methods = "EL", data = melanoma, marker = "X", status = "group",
# tag.healthy = 0, categorical.cov = NULL, CFN = 2, CFP = 1,
# control = control.gsym.point(), confidence.level = 0.95,
# trace = FALSE, seed = TRUE, value.seed = 3, verbose = TRUE)
# According to the Shapiro-Wilk normality test, the marker can be
# considered normally distributed in both groups.
# Therefore the GPQ method would be more suitable for this dataset.
# Shapiro-Wilk test p-values
# Group 0 Group 1
# Original marker 0.4719 0.9084
# Area under the ROC curve (AUC): 0.906
# METHOD: EL
# Estimate 95% CI lower limit 95% CI upper limit
# cutoff -1.2382325 -1.8403497 -0.4565671
# Specificity 0.7901833 0.6326184 0.8973174
# Sensitivity 0.8950916 0.8163092 0.9486587
# According to the Shapiro-Wilk normality test, the marker can be
# considered normally distributed in both groups.
melanoma.cutpoint3 <- gsym.point(methods = c("EL","GPQ"),
data = melanoma, marker = "X", status = "group",
tag.healthy = 0, categorical.cov = NULL, CFN = 2, CFP = 1,
control = control.gsym.point(), confidence.level = 0.95,
trace = FALSE, seed = TRUE, value.seed = 3, verbose = TRUE)
# Therefore, although the results of both methods will be shown,
# the GPQ method would be more suitable for this dataset.
# Shapiro-Wilk test p-values
# Group 0 Group 1
# Original marker 0.4719 0.9084
plot(melanoma.cutpoint1)
library("GsymPoint")
data("prostate")
prostate.cutpoint1 <- gsym.point(methods = "GPQ", data = prostate,
marker = "marker", status = "status", tag.healthy = 0,
categorical.cov = NULL, CFN = 10, CFP = 1,
control = control.gsym.point(I = 1500), confidence.level = 0.95,
trace = FALSE, seed = TRUE, value.seed = 3, verbose = TRUE)
summary(prostate.cutpoint1)
# *************************************************
# OPTIMAL CUTOFF: GENERALIZED SYMMETRY POINT
# *************************************************
# Call:
# gsym.point(methods = "GPQ", data = prostate, marker = "marker",
# status = "status", tag.healthy = 0, categorical.cov = NULL,
# CFN = 10, CFP = 1, control = control.gsym.point(I = 1500),
# confidence.level = 0.95, trace = FALSE, seed = TRUE, value.seed = 3,
# verbose = TRUE)
# According to the Shapiro-Wilk normality test, the marker can not
# be considered normally distributed in both groups.
# However, after transforming the marker using the Box-Cox
# transformation estimate, the Shapiro-Wilk normality test
# indicates that the transformed marker can be considered
# normally distributed in both groups.
# Box-Cox lambda estimate = -1.2494
# Shapiro-Wilk test p-values
# Group 0 Group 1
# Original marker 0.0000 0.0232
# Box-Cox transformed marker 0.3641 0.2118
# Area under the ROC curve (AUC): 0.725
# METHOD: GPQ
# Estimate 95% CI lower limit 95% CI upper limit
# cutoff 51.9522523 46.8013315 57.3009307
# Specificity 0.3233012 0.1420636 0.5191686
# Sensitivity 0.9323301 0.9142064 0.9519169
names(prostate.cutpoint1)
# [1] "GPQ" "methods" "call" "data"
# [1] "Global"
# [1] "GPQ"
prostate.cutpoint1$call
# gsym.point(methods = "GPQ", data = prostate, marker = "marker",
# status = "status", tag.healthy = 0, categorical.cov = NULL,
# confidence.level = 0.95, trace = FALSE,
# seed = TRUE, value.seed = 3, verbose = TRUE)
prostate.cutpoint1$data
# marker status
# 2 40 0
# 3 46 0
names(prostate.cutpoint1$GPQ)
names(prostate.cutpoint1$GPQ$Global)
# [1] "optimal.result" "AUC"
# [3] "rho" "lambda"
# [5] "normality.transformed" "pvalue.healthy"
# [7] "pvalue.diseased" "pvalue.healthy.transformed"
# [9] "pvalue.diseased.transformed"
prostate.cutpoint1$methods
# CFN = 10, CFP = 1, control = control.gsym.point(I = 1500),
# 1 40 0
# [...]
# 51 99 1
# 52 126 1
# 53 136 1
prostate.cutpoint1$GPQ$Global$optimal.result
# $cutoff
# $Sensitivity
prostate.cutpoint1$GPQ$Global$rho
# [1] 0.1
prostate.cutpoint1$GPQ$Global$lambda
# Value ll ul
# 1 51.95225 46.80133 57.30093
# $Specificity
# Value ll ul
# 1 0.3233012 0.1420636 0.5191686
# Value ll ul
# 1 0.9323301 0.9142064 0.9519169
prostate.cutpoint1$GPQ$Global$AUC
# [1] 0.725
# [1] -1.249428
prostate.cutpoint1$GPQ$Global$normality.transformed
# [1] "yes"
prostate.cutpoint1$GPQ$Global$pvalue.healthy
# [1] 3.276498e-07
prostate.cutpoint1$GPQ$Global$pvalue.diseased
# [1] 0.02323895
prostate.cutpoint1$GPQ$Global$pvalue.healthy.transformed
# [1] 0.3640662
prostate.cutpoint1$GPQ$Global$pvalue.diseased.transformed
# [1] 0.2118137
prostate.cutpoint2 <- gsym.point(methods = "EL", data = prostate,
marker = "marker", status = "status", tag.healthy = 0,
categorical.cov = NULL, CFN = 10, CFP = 1,
control = control.gsym.point(B = 999), confidence.level = 0.95,
trace = FALSE, seed = TRUE, value.seed = 3, verbose = TRUE)
# According to the Shapiro-Wilk normality test, the marker can not
# be considered normally distributed in both groups.
# However, after transforming the marker using the Box-Cox
# transformation estimate, the Shapiro-Wilk normality test
# indicates that the transformed marker can be considered
# normally distributed in both groups.
# Therefore the GPQ method would be more suitable for this dataset.
# Box-Cox lambda estimate = -1.2494
# Shapiro-Wilk test p-values
# Group 0 Group 1
# Original marker 0.0000 0.0232
# Box-Cox transformed marker 0.3641 0.2118
summary(prostate.cutpoint2)
# *************************************************
# OPTIMAL CUTOFF: GENERALIZED SYMMETRY POINT
# *************************************************
# Call:
# gsym.point(methods = "EL", data = prostate, marker = "marker",
# status = "status", tag.healthy = 0, categorical.cov = NULL,
# CFN = 10, CFP = 1, control = control.gsym.point(B = 999),
# confidence.level = 0.95, trace = FALSE, seed = TRUE, value.seed = 3,
# verbose = TRUE)
# According to the Shapiro-Wilk normality test, the marker can not
# be considered normally distributed in both groups.
# However, after transforming the marker using the Box-Cox
# transformation estimate, the Shapiro-Wilk normality test
# indicates that the transformed marker can be considered
# normally distributed in both groups.
# Therefore the GPQ method would be more suitable for this dataset.
# Box-Cox lambda estimate = -1.2494
# Shapiro-Wilk test p-values
# Group 0 Group 1
# Original marker 0.0000 0.0232
# Box-Cox transformed marker 0.3641 0.2118
# Area under the ROC curve (AUC): 0.725
# METHOD: EL
# Estimate 95% CI lower limit 95% CI upper limit
# cutoff 49.2249839 45.39058266 58.7032623
# Specificity 0.2451690 0.09891113 0.5269153
# Sensitivity 0.9245169 0.90989111 0.9526915
plot(prostate.cutpoint1)
library("GsymPoint")
data("elastase")
elastase.gender.cutpoint1 <- gsym.point(methods = "GPQ",
data = elastase, marker = "elas", status = "status",
tag.healthy = 0, categorical.cov = "gender", CFN = 10, CFP = 1,
control = control.gsym.point(), confidence.level = 0.95,
trace = FALSE, seed = TRUE, value.seed = 3, verbose = TRUE)
names(elastase.gender.cutpoint1)
# [1] "GPQ" "methods" "levels.cat" "call"
# [5] "data"
names(elastase.gender.cutpoint1$GPQ)
# [1] "Female" "Male"
names(elastase.gender.cutpoint1$GPQ$Male)
# [1] "optimal.result" "AUC"
# [3] "rho" "lambda"
# [5] "normality.transformed" "pvalue.healthy"
# [7] "pvalue.diseased" "pvalue.healthy.transformed"
# [9] "pvalue.diseased.transformed"
elastase.gender.cutpoint1$GPQ$Male$optimal.result
# $cutoff
# Value ll ul
# 1 20.72776 18.08961 23.49228
# $Specificity
# Value ll ul
# 1 0.2739826 0.1345484 0.4326794
# $Sensitivity
# Value ll ul
# 1 0.9273983 0.9134548 0.9432679
elastase.gender.cutpoint1$GPQ$Male$AUC
# [1] 0.7216855
elastase.gender.cutpoint1$GPQ$Male$rho
# [1] 0.1
elastase.gender.cutpoint1$GPQ$Male$lambda
# [1] -0.04277911
elastase.gender.cutpoint1$GPQ$Male$normality.transformed
# [1] "yes"
summary(elastase.gender.cutpoint1)
elastase.gender.cutpoint1$GPQ$Male$pvalue.healthy
# [1] 0.5866506
elastase.gender.cutpoint1$GPQ$Male$pvalue.healthy.transformed
# [1] 0.06656483
# Shapiro-Wilk test p-values
# Group 0 Group 1
elastase.gender.cutpoint1$GPQ$Male$pvalue.diseased
# [1] 5.44323e-09
elastase.gender.cutpoint1$GPQ$Male$pvalue.diseased.transformed
# [1] 0.2147409
# *************************************************
# OPTIMAL CUTOFF: GENERALIZED SYMMETRY POINT
# *************************************************
# Call:
# gsym.point(methods = "GPQ", data = elastase, marker = "elas",
# status = "status", tag.healthy = 0, categorical.cov = "gender",
# CFN = 10, CFP = 1, control = control.gsym.point(), confidence.level = 0.95,
# trace = FALSE, seed = TRUE, value.seed = 3, verbose = TRUE)
# *************************************************
# Female
# *************************************************
# According to the Shapiro-Wilk normality test, the marker can be
# considered normally distributed in both groups.
# Original marker 0.0837 0.9077
# Area under the ROC curve (AUC): 0.818
# METHOD: GPQ
# Estimate 95% CI lower limit 95% CI upper limit
# cutoff 25.0929510 12.3370641 34.0526540
# Specificity 0.4246091 0.1460251 0.6618634
# Sensitivity 0.9424609 0.9146025 0.9661863
# *************************************************
# Male
# *************************************************
# According to the Shapiro-Wilk normality test, the marker can not
# be considered normally distributed in both groups.
# However, after transforming the marker using the Box-Cox
# transformation estimate, the Shapiro-Wilk normality test
# indicates that the transformed marker can be considered
# normally distributed in both groups.
# Box-Cox lambda estimate = -0.0428
# Shapiro-Wilk test p-values
# Group 0 Group 1
# Box-Cox transformed marker 0.0666 0.2147
# METHOD: GPQ
# Estimate 95% CI lower limit 95% CI upper limit
# cutoff 20.7277556 18.0896126 23.4922813
# Sensitivity 0.9273983 0.9134548 0.9432679
# Shapiro-Wilk test p-values
# Original marker 0.5867 0.0000
# Area under the ROC curve (AUC): 0.722
# Specificity 0.2739826 0.1345484 0.4326794
elastase.gender.cutpoint2 <- gsym.point (methods = "EL", data = elastase,
marker = "elas", status = "status", tag.healthy = 0,
categorical.cov = "gender", CFN = 10, CFP = 1,
control = control.gsym.point(), confidence.level = 0.95,
trace = FALSE, seed = TRUE, value.seed = 3, verbose = TRUE)
# Female :
# According to the Shapiro-Wilk normality test, the marker can be
# considered normally distributed in both groups.
# Therefore the GPQ method would be more suitable for this dataset.
# Group 0 Group 1
# Original marker 0.0837 0.9077
# Male :
# According to the Shapiro-Wilk normality test, the marker can not
# be considered normally distributed in both groups.
# However, after transforming the marker using the Box-Cox
# transformation estimate, the Shapiro-Wilk normality test
# indicates that the transformed marker can be considered
# normally distributed in both groups.
# Therefore the GPQ method would be more suitable for this dataset.
# Box-Cox lambda estimate = -0.0428
# Shapiro-Wilk test p-values
# Group 0 Group 1
# Original marker 0.5867 0.0000
# Box-Cox transformed marker 0.0666 0.2147
plot(elastase.gender.cutpoint1)
# 0.0 0.2 0.4 0.6 0.8 1.0
# 0.0 0.2 0.4 0.6 0.8 1.0
# False Positive Rate
# True Positive Rate</div>
# GPQ: (0.245, 0.877)
# AUC:  0.906</div>
# ρ
# σ
# Empirical ROC Curve and line y = 1−0.5x
#  
# −∞
# R
# ∑
