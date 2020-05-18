library(QCA)
# base variable and vector of thresholds
b <- sort(rnorm(15)); th <- quantile(b, c(0.1, 0.5, 0.9))
# create bivalent crisp set
calibrate(b, thresholds = th[2])
# [1] 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1
# create trivalent crisp set using thresholds derived from cluster analysis
calibrate(b, thresholds = findTh(b, groups = 3))
# [1] 0 0 0 1 1 1 1 1 1 1 1 2 2 2 2
# fuzzification using Equation (1)
round(calibrate(b, type = "fuzzy", thresholds = th), 2)
# [1] 0.00 0.00 0.04 0.32 0.42 0.47 0.48 0.50 0.59 0.72 0.77 0.93 0.94 1.00 1.00
# negation of previous result
round(calibrate(b, type = "fuzzy", thresholds = rev(th)), 2)
# [1] 1.00 1.00 0.96 0.68 0.58 0.53 0.52 0.50 0.41 0.28 0.23 0.07 0.06 0.00 0.00
# fuzzification using piecewise logistic
round(calibrate(b, type = "fuzzy", thresholds = th, logistic = TRUE), 2)
# [1] 0.02 0.04 0.06 0.25 0.38 0.45 0.46 0.50 0.64 0.79 0.83 0.93 0.93 0.96 0.97
data(Krook)
Krook
# ES QU WS WM LP WNP
# SE 1 1 1 0 0 1
# FI 1 0 1 0 0 1
# NO 1 1 1 1 1 1
# .. . . . . . .
# <rest omitted>
superSubset(Krook, outcome = "WNP", cov.cut = 0.52)
# incl PRI cov.r
# --------------------------------
# 1 ES+LP 1.000 1.000 0.733
# 2 ES+WM 1.000 1.000 0.524
# 3 WS+WM+LP 1.000 1.000 0.611
# 4 QU+wm+LP 1.000 1.000 0.550
# 5 QU+WM+lp 1.000 1.000 0.524
# 6 QU+WS+LP 1.000 1.000 0.550
# 7 QU+WS+WM 1.000 1.000 0.524
# 8 es+QU+WS 1.000 1.000 0.524
# --------------------------------
KrookTT <- truthTable(Krook, outcome = "WNP")
KrookTT
# OUT: outcome value
# n: number of cases in configuration
# incl: sufficiency inclusion score
# PRI: proportional reduction in inconsistency
# ES QU WS WM LP OUT n incl PRI
# 3 0 0 0 1 0 0 2 0.000 0.000
# 4 0 0 0 1 1 1 1 1.000 1.000
# 9 0 1 0 0 0 0 1 0.000 0.000
# 11 0 1 0 1 0 0 4 0.000 0.000
# 12 0 1 0 1 1 1 1 1.000 1.000
# 18 1 0 0 0 1 0 1 0.000 0.000
# 21 1 0 1 0 0 1 1 1.000 1.000
# 24 1 0 1 1 1 1 1 1.000 1.000
# 25 1 1 0 0 0 0 3 0.000 0.000
# 26 1 1 0 0 1 1 1 1.000 1.000
# 27 1 1 0 1 0 1 1 1.000 1.000
# 28 1 1 0 1 1 1 2 1.000 1.000
# 29 1 1 1 0 0 1 1 1.000 1.000
# 32 1 1 1 1 1 1 2 1.000 1.000
KrookSC <- eqmcc(KrookTT, details = TRUE)
KrookSC
# n OUT = 1/0/C: 11/11/0
# Total : 22
# S1: ES*QU*ws*LP + ES*QU*ws*WM + es*ws*WM*LP + ES*WS*wm*lp + ES*WS*WM*LP
# incl PRI cov.r cov.u
# ---------------------------------------
# ES*QU*ws*LP 1.000 1.000 0.273 0.091
# ES*QU*ws*WM 1.000 1.000 0.273 0.091
# es*ws*WM*LP 1.000 1.000 0.182 0.182
# ES*WS*wm*lp 1.000 1.000 0.182 0.182
# ES*WS*WM*LP 1.000 1.000 0.273 0.273
# ---------------------------------------
# S1 1.000 1.000 1.000
KrookSP <- eqmcc(KrookTT, include = "?", rowdom = FALSE, details = TRUE)
KrookSP
# n OUT = 1/0/C: 11/11/0
# Total : 22
# S1: WS + ES*WM + QU*LP + (es*LP)
# S2: WS + ES*WM + QU*LP + (WM*LP)
# -------------------
# incl PRI cov.r cov.u (S1) (S2)
# -----------------------------------------------
# WS 1.000 1.000 0.455 0.182 0.182 0.182
# ES*WM 1.000 1.000 0.545 0.091 0.091 0.091
# QU*LP 1.000 1.000 0.545 0.091 0.091 0.091
# -----------------------------------------------
# es*LP 1.000 1.000 0.182 0.000 0.091
# WM*LP 1.000 1.000 0.636 0.000 0.091
# -----------------------------------------------
# S1 1.000 1.000 1.000
# S2 1.000 1.000 1.000
KrookSI <- eqmcc(KrookTT, include = "?", direxp = c(1,1,1,1,1), details = TRUE)
KrookSI
# n OUT = 1/0/C: 11/11/0
# Total : 22
# p.sol: WS + ES*WM + QU*LP + WM*LP
# S1: ES*WS + WM*LP + ES*QU*LP + ES*QU*WM
# incl PRI cov.r cov.u
# ------------------------------------
# ES*WS 1.000 1.000 0.455 0.182
# WM*LP 1.000 1.000 0.636 0.182
# ES*QU*LP 1.000 1.000 0.455 0.091
# ES*QU*WM 1.000 1.000 0.455 0.091
# ------------------------------------
# S1 1.000 1.000 1.000
KrookSI$PIchart$i.sol$C1P1
# 4 12 21 24 26 27 28 29 32
# ES*WS - - x x - - - x x
# WM*LP x x - x - - x - x
# ES*QU*LP - - - - x - x - x
# ES*QU*WM - - - - - x x - x
KrookSI$pims$i.sol$C1P1
# ES*WS WM*LP ES*QU*LP ES*QU*WM
# SE 1 0 0 0
# FI 1 0 0 0
# NO 1 1 1 1
# DK 1 1 0 0
# NL 0 1 1 1
# ES 0 0 0 1
# .. . . . .
# <rest omitted>
# 1984
# 1985
# 1986
# 1987
# 1988
# 1989
# 1990
# 1991
# 1992
# 1993
# 1994
# 1995
# 1996
# 1997
# 1998
# 1999
# 2000
# 2001
# 2002
# 2003
# 2004
# 2005
# 2006
# 2007
# 2008
# 2009
# 2010
# 2011
# 2012
# Year
# Number of QCA Applications
# 0 5 10 15 20 25 30 35 40
# csQCA
# mvQCA
# fsQCA
# Applications Total = 280
# Articles Total = 276</div>
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# fs/QCA (81.9%)
# fuzzy (1.2%)
# Tosmana (14.4%)
# other (2.5%)</div>
# C
# C
