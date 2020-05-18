# the origin of the plot.
# from the origin of the plot.
res <- CAvariants(shopdataM, catype = "SONSCA")
names(res)
# [1] "Xtable" "rows" "cols" "r" "rowlabels"
# [6] "collabels" "Rprinccoord" "Cprinccoord" "Rstdcoord" "Cstdcoord"
# [11] "tauden" "tau" "inertiasum" "inertias" "inertias2"
# [16] "comps" "catype" "mj" "mi" "pcc"
# [21] "Jmass" "Imass" "Trend" "Z" "ellcomp"
# [26] "risell" "Mell"
print(res)
summary(res)
res <- CAvariants(shopdataM, catype = "SONSCA")
# RESULTS for SONSCA Correspondence Analysis
# Data Table:
# M12< M13 M16 M19 M25 M35 M45 M57 M65+
# clothing 81 138 304 384 942 359 178 137 45
# accessories 66 204 193 149 297 109 53 68 28
# tobacco 150 340 229 151 313 136 121 171 145
# stationary 667 1409 527 84 92 36 36 37 17
# books 67 259 258 146 251 96 48 56 41
# records 24 272 368 141 167 67 29 27 7
# household 47 117 98 61 193 75 50 55 29
# candy 430 637 246 40 30 11 5 17 28
# toys 743 684 116 13 16 16 6 3 8
# jewelry 132 408 298 71 130 31 14 11 10
# perfumes 32 57 61 52 111 54 41 50 28
# hobby 197 547 402 138 280 200 152 211 111
# other 209 550 454 252 624 195 88 90 34
# Row Weights: Imass
# clothing accessories tobacco stationary books records household
# clothing 1 0 0 0 0 0 0
# accessories 0 1 0 0 0 0 0
# tobacco 0 0 1 0 0 0 0
# stationary 0 0 0 1 0 0 0
# books 0 0 0 0 1 0 0
# records 0 0 0 0 0 1 0
# household 0 0 0 0 0 0 1
# candy 0 0 0 0 0 0 0
# toys 0 0 0 0 0 0 0
# jewelry 0 0 0 0 0 0 0
# perfumes 0 0 0 0 0 0 0
# hobby 0 0 0 0 0 0 0
# other 0 0 0 0 0 0 0
# candy toys jewelry perfumes hobby other
# clothing 0 0 0 0 0 0
# accessories 0 0 0 0 0 0
# tobacco 0 0 0 0 0 0
# stationary 0 0 0 0 0 0
# books 0 0 0 0 0 0
# records 0 0 0 0 0 0
# household 0 0 0 0 0 0
# candy 1 0 0 0 0 0
# toys 0 1 0 0 0 0
# jewelry 0 0 1 0 0 0
# perfumes 0 0 0 1 0 0
# hobby 0 0 0 0 1 0
# other 0 0 0 0 0 1
# Column Weights: Jmass
# 12< 13 16 19 25 35 45 57 65+
# 12< 0.137 0.00 0.000 0.0000 0.000 0.0000 0.0000 0.0000 0.0000
# 13 0.000 0.27 0.000 0.0000 0.000 0.0000 0.0000 0.0000 0.0000
# 16 0.000 0.00 0.171 0.0000 0.000 0.0000 0.0000 0.0000 0.0000
# 19 0.000 0.00 0.000 0.0808 0.000 0.0000 0.0000 0.0000 0.0000
# 25 0.000 0.00 0.000 0.0000 0.166 0.0000 0.0000 0.0000 0.0000
# 35 0.000 0.00 0.000 0.0000 0.000 0.0665 0.0000 0.0000 0.0000
# 45 0.000 0.00 0.000 0.0000 0.000 0.0000 0.0394 0.0000 0.0000
# 57 0.000 0.00 0.000 0.0000 0.000 0.0000 0.0000 0.0448 0.0000
# 65+ 0.000 0.00 0.000 0.0000 0.000 0.0000 0.0000 0.0000 0.0255
# Total inertia 0.038
# Inertias, percent inertias and cumulative percent inertias of the row space
# inertia inertiapc cuminertiapc
# 1 0.0300 79.88 79.88
# 2 0.0037 9.86 89.74
# 3 0.0032 8.44 98.18
# 4 0.0003 0.92 99.10
# 5 0.0003 0.67 99.77
# 6 0.0001 0.17 99.94
# 7 0.0000 0.05 99.99
# 8 0.0000 0.01 100.00
# Inertias, percent inertias and cumulative percent inertias of the column space
# inertia2 inertiapc2 cuminertiapc2
# 1 0.0225 59.83 59.83
# 2 0.0096 25.58 85.41
# 3 0.0028 7.33 92.74
# 4 0.0019 5.18 97.92
# 5 0.0003 0.82 98.74
# 6 0.0003 0.74 99.48
# 7 0.0001 0.35 99.83
# 8 0.0001 0.17 100.00
# Predictability Index for Variants of Non symmetrical Correspondence Analysis:
# Numerator of Tau Index predicting the rows given the column categories
# [1] 0.038
# Tau Index predicting the rows given the column categories
# [1] 0.041
# C-statistic 10331.51 and p-value 0
# Polynomial Components of Inertia
# ** Column Components **
# Component Value P-value
# Location 6181.536 0
# Dispersion 2642.363 0
# Cubic 757.192 0
# Error 750.418 0
# ** C-Statistic ** 10331.509 0
# Generalized correlation matrix of Hybrid Decomposition
# v1 v2 v3 v4 v5 v6 v7 v8
# m1 -0.147 0.084 0.018 -0.030 0.011 0.005 -0.005 0.003
# m2 -0.028 -0.034 -0.032 0.024 0.005 -0.010 0.003 0.001
# m3 -0.013 -0.037 0.036 -0.016 -0.004 0.006 -0.006 0.002
# m4 -0.001 0.002 0.006 0.014 -0.010 0.005 -0.001 -0.001
# m5 -0.001 -0.001 -0.007 -0.006 -0.007 0.009 -0.004 -0.004
# m6 0.000 0.000 -0.001 0.000 0.000 -0.001 -0.006 0.005
# m7 0.000 0.000 0.000 -0.001 -0.002 -0.003 -0.001 -0.002
# m8 0.000 0.000 0.000 0.000 -0.001 0.000 0.001 0.001
# m9 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
# Column standard polynomial coordinates = column polynomial axes
# Axis 1 Axis 2
# M12< -1.232 1.352
# M13 -0.759 0.142
# M16 -0.285 -0.652
# M19 0.188 -1.029
# M25 0.661 -0.991
# M35 1.135 -0.536
# M45 1.608 0.336
# M57 2.081 1.624
# M65+ 2.554 3.328
# Row principal polynomial coordinates
# Axis 1 Axis 2
# clothing 0.072 -0.056
# accessories 0.017 -0.014
# tobacco 0.039 0.017
# stationary -0.084 0.033
# books 0.012 -0.012
# records 0.000 -0.021
# household 0.015 -0.004
# candy -0.045 0.027
# toys -0.067 0.049
# jewelry -0.017 -0.006
# perfumes 0.014 0.000
# hobby 0.030 0.015
# other 0.014 -0.030
# Column distances from the origin of the plot
# Axis 1 Axis 2
# M12< 0.057 0.002
# M13 0.027 0.000
# M16 0.000 0.000
# M19 0.027 0.002
# M25 0.046 0.004
# M35 0.041 0.000
# M45 0.031 0.005
# M57 0.021 0.022
# M65+ 0.010 0.047
# Row distances from the origin of the plot
# Axis 1 Axis 2
# clothing 0.005 0.003
# accessories 0.000 0.000
# tobacco 0.001 0.000
# stationary 0.007 0.001
# books 0.000 0.000
# records 0.000 0.000
# household 0.000 0.000
# candy 0.002 0.001
# toys 0.005 0.002
# jewelry 0.000 0.000
# perfumes 0.000 0.000
# hobby 0.001 0.000
# other 0.000 0.001
# M12< M13 M16 M19 M25 M35 M45 M57 M65+
# clothing 0.111 0.097 0.014 -0.112 -0.150 -0.118 -0.065 -0.012 0.044
# accessories 0.029 0.022 0.002 -0.024 -0.031 -0.027 -0.019 -0.011 -0.002
# tobacco 0.057 0.017 -0.010 -0.003 0.004 -0.023 -0.059 -0.094 -0.122
# stationary -0.132 -0.089 -0.003 0.089 0.114 0.110 0.098 0.085 0.064
# books 0.023 0.015 0.000 -0.014 -0.018 -0.018 -0.018 -0.017 -0.015
# records 0.011 0.008 0.000 -0.008 -0.010 -0.010 -0.008 -0.006 -0.004
# household 0.023 0.014 0.000 -0.013 -0.016 -0.018 -0.018 -0.018 -0.017
# candy -0.074 -0.049 -0.001 0.048 0.061 0.061 0.055 0.049 0.039
# toys -0.122 -0.070 0.004 0.061 0.074 0.088 0.101 0.113 0.115
# jewelry -0.021 -0.013 0.000 0.012 0.015 0.016 0.016 0.016 0.015
# perfumes 0.021 0.010 -0.001 -0.008 -0.009 -0.013 -0.018 -0.023 -0.026
# hobby 0.048 0.007 -0.012 0.010 0.021 -0.011 -0.055 -0.098 -0.135
# other 0.026 0.030 0.007 -0.039 -0.054 -0.036 -0.010 0.017 0.043
# Eccentricity of ellipses
# [1] 0.757
# Ellipse axes, Area, p-values of rows
# HL Axis 1 HL Axis 2 Area P-value
# clothing 0.013 0.009 0 0.000
# accessories 0.010 0.007 0 0.000
# tobacco 0.011 0.007 0 0.000
# stationary 0.010 0.007 0 0.000
# books 0.012 0.008 0 0.000
# records 0.008 0.005 0 0.000
# household 0.015 0.010 0 0.000
# candy 0.013 0.008 0 0.000
# toys 0.011 0.007 0 0.000
# jewelry 0.013 0.008 0 0.000
# perfumes 0.015 0.010 0 0.297
# hobby 0.011 0.007 0 0.000
# other 0.011 0.007 0 0.000
# Ellipse axes, Area, p-values of columns
# HL Axis 1 HL Axis 2 Area P-value
# M12< 0.034 0.022 0.002 0
# M13 0.020 0.013 0.001 0
# M16 0.020 0.013 0.001 0
# M19 0.023 0.015 0.001 0
# M25 0.025 0.016 0.001 0
# M35 0.026 0.017 0.001 0
# M45 0.031 0.020 0.002 0
# M57 0.046 0.030 0.004 0
# M65+ 0.070 0.046 0.010 0
plot(res, plottype = "biplot", biptype = "row", scaleplot = 5, pos = 1)
# 10 20 30 40 50 60 70
# −0.2 −0.1 0.0 0.1 0.2 0.3 0.4
# Age group scores
# Increase in Predictability</div>
# clothing
# accessories
# tobacco
# stationary
# toys
# other</div>
plot(res, scaleplot = 1, ell = TRUE, alpha = 0.05)
plot(res, scaleplot = 1, ell = TRUE, alpha = 0.05, prop = 60)
