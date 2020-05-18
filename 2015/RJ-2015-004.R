# bc
# bc
# bc
# bc
# rdrobust(y = y,x = x)
# rdrobust(y = y,x = x,deriv = 1)
# rdrobust(y = y,x = x,fuzzy = t)
# rdrobust(y = y,x = x,fuzzy = t,deriv = 1)
library(rdrobust)
data(rdrobust_RDsenate)
vote <- rdrobust_RDsenate$vote
margin <- rdrobust_RDsenate$margin
summary(vote)
# 0.00 42.67 50.55 52.67 61.35 100.00 93
summary(margin)
# Min. 1st Qu. Median Mean 3rd Qu. Max.
# -100.000 -12.210 2.166 7.171 22.770 100.000
(rdplot(y = vote, x = margin, title = "RD Plot - Senate Elections Data",
y.label = "Vote Share in Election at time t+1",
x.label = "Vote Share in Election at time t"))
# Call:
# rdplot(y = vote, x = margin, title = "RD Plot - Senate Elections Data",
# x.label = "Vote Share in Election at time t",
# y.label = "Vote Share in Election at time t+1")
# Method: mimicking variance evenly-spaced method using spacings estimators
# Left Right
# Number of Obs. 595 702
# Polynomial Order 4 4
# Scale 1 1
# Selected Bins 15 35
# Bin Length 6.6614 2.8561
# IMSE-optimal bins 8 9
# Mimicking Variance bins 15 35
# Relative to IMSE-optimal:
# Implied scale 1.8750 3.8889
# WIMSE variance weight 0.1317 0.0167
# WIMSE bias weight 0.8683 0.9833
(rdplot(y = vote, x = margin, binselect = "es",
title = "RD Plot - Senate Elections Data",
y.label = "Vote Share in Election at time t+1",
x.label = "Vote Share in Election at time t"))
# Call:
# rdplot(y = vote, x = margin, binselect = "es", x
# title = "RD Plot - Senate Elections Data",
# x.label = "Vote Share in Election at time t",
# y.label = "Vote Share in Election at time t+1")
# Method: IMSE-optimal evenly-spaced method using spacings estimators
# Left Right
# Number of Obs. 595 702
# Polynomial Order 4 4
# Scale 1 1
# Selected Bins 8 9
# Bin Length 12.4901 11.1071
# IMSE-optimal bins 8 9
# Mimicking Variance bins 15 35
# Relative to IMSE-optimal:
# Implied scale 1.0000 1.0000
# WIMSE variance weight 0.5000 0.5000
# WIMSE bias weight 0.5000 0.5000
rdrobust(y = vote, x = margin)
# Call:
# rdrobust(y = vote, x = margin)
(rdplot(y = vote, x = margin, binselect = "es", scale = 5,
title = "RD Plot - Senate Elections Data",
y.label = "Vote Share in Election at time t+1",
x.label = "Vote Share in Election at time t"))
# Call:
# rdplot(y = vote, x = margin, binselect = "es", scale = 5,
# title = "RD Plot - Senate Elections Data",
# x.label = "Vote Share in Election at time t",
# y.label = "Vote Share in Election at time t+1")
# Method: IMSE-optimal evenly-spaced method using spacings estimators
# Left Right
# Number of Obs. 595 702
# Polynomial Order 4 4
# Scale 5 5
# Selected Bins 40 45
# Bin Length 2.4980 2.2214
# IMSE-optimal bins 8 9
# Mimicking Variance bins 15 35
# Relative to IMSE-optimal:
# Implied scale 5.0000 5.0000
# WIMSE variance weight 0.0079 0.0079
# WIMSE bias weight 0.9921 0.9921
# Summary:
# Number of Obs 1297
# NN Matches 3
# BW Type CCT
# Kernel Type Triangular
# Left Right
# Number of Obs 343 310
# Order Loc Poly (p) 1 1
# Order Bias (q) 2 2
# BW Loc Poly (h) 16.7936 16.7936
# BW Bias (b) 27.4372 27.4372
# rho (h/b) 0.6121 0.6121
# Estimates:
# Coef Std. Err. z P>|z| CI Lower CI Upper
# Conventional 7.4253 1.4954 4.9656 0.0000 4.4944 10.3562
# Robust 0.0000 4.0697 10.9833
rdrobust(y = vote, x = margin, all = TRUE)
# Call:
# rdrobust(y = vote, x = margin, all = TRUE)
# Summary:
# Number of Obs 1297
# cvgrid_max = 80, cvplot = TRUE)
# BW Selector CV
# Number of Obs 1297
# rdbwselect(y = vote, x = margin, bwselect = "CV", cvgrid_min = 10,
# NN Matches 3
# BW Type CCT
# Kernel Type Triangular
# Left Right
# Number of Obs 343 310
# Order Loc Poly (p) 1 1
# Order Bias (q) 2 2
# BW Loc Poly (h) 16.7936 16.7936
# BW Bias (b) 27.4372 27.4372
# rho (h/b) 0.6121 0.6121
# Estimates:
# Coef Std. Err. z P>|z| CI Lower CI Upper
# Conventional 7.4253 1.4954 4.9656 0.0000 4.4944 10.3562
# Bias-Corrected 7.5265 1.4954 5.0333 0.0000 4.5957 10.4574
# Robust 7.5265 1.7637 4.2675 0.0000 4.0697 10.9833
rdbwselect(y = vote, x = margin, all = TRUE)
# Call:
# rdbwselect(y = vote, x = margin, all = TRUE)
# BW Selector All
# Number of Obs 1297
# NN Matches 3
# Kernel Type Triangular
# Left Right
# Number of Obs 595 702
# Order Loc Poly (p) 1 1
# Order Bias (q) 2 2
# h b
# CCT 16.79357 27.43722
# IK 15.66761 16.48524
# CV 35.42113 NA
rdbwselect(y = vote, x = margin, bwselect = "CV",
cvgrid_min = 10, cvgrid_max = 80, cvplot = TRUE)
# Call:
# Kernel Type Triangular
# Left Right
# Number of Obs 595 702
# Order Loc Poly (p) 1 1
# Order Bias (q) 2 2
# h b
# 34.5 34.5
# NN Matches 3
# rdrobust(y = vote,x = margin,kernel = "uniform")
# rdrobust(y = vote,x = margin,bwselect = "IK")
# rdrobust(y = vote,x = margin,bwselect = "CV")
# rdrobust(y = vote,x = margin,h = 15,rho = 0.8)
# rdrobust(y = vote,x = margin,p = 2,q = 4)
# rdrobust(y = vote,x = margin,vce = "resid")
# ∂
# ν
# ν
# ν
# ν
# ∂
# ν
# ν
# ∂
# ν
# ν
# τ
# τ
# ν
# ν
# ν
# ν
# ν
# ν
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# (
# q
# s
# s
# q
# l
# 0
# −
# 0
# 0


# β∈R
# β∈R
# V
# V
