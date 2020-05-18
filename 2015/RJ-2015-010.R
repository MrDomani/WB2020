# CaGalt(Y, X, type = "s", conf.ellip = FALSE, nb.ellip = 100, level.ventil = 0,
# sx = NULL, graph = TRUE, axes = c(1, 2))
# 7 "$freq$cos2" "cos2 for the frequencies"
# 8 "$freq$contrib" "contributions of the frequencies"
# 9 "$quali.var" "results for the categorical variables"
# 10 "$quali.var$coord" "coordinates for the categories"
# 11 "$quali.var$cos2" "cos2 for the categories"
data(health)
res.cagalt <- CaGalt(Y = health[, 1:115], X = health[, 116:118], type = "n")
res.cagalt
# **Results for the Correspondence Analysis on Generalised Aggregated Lexical
# Tables (CaGalt)**
# *The results are available in the following entries:
# name description
# 1 "$eig" "eigenvalues"
# 2 "$ind" "results for the individuals"
# 3 "$ind$coord" "coordinates for the individuals"
# 4 "$ind$cos2" "cos2 for the individuals"
# 5 "$freq" "results for the frequencies"
# 6 "$freq$coord" "coordinates for the frequencies"
# 12 "$ellip" "coordinates to construct confidence ellipses"
# 13 "$ellip$freq" "coordinates of the ellipses for the frequencies"
# 14 "$ellip$var" "coordinates of the ellipses for the variables"
summary(res.cagalt)
# Eigenvalues
# Dim.1 Dim.2 Dim.3 Dim.4 Dim.5 Dim.6 Dim.7
# Variance 0.057 0.036 0.026 0.024 0.020 0.013 0.012
# % of var. 30.207 19.024 13.776 12.953 10.847 6.819 6.374
# Cumulative % of var. 30.207 49.230 63.007 75.960 86.807 93.626 100.000
# Individuals (the 10 first individuals)
# Dim.1 cos2 Dim.2 cos2 Dim.3 cos2
# 6 | 0.120 0.037 | -0.551 0.781 | -0.065 0.011 |
# 7 | -0.134 0.019 | -0.788 0.649 | -0.166 0.029 |
# 9 | 0.056 0.002 | 0.272 0.047 | -0.211 0.028 |
# 10 | 0.015 0.001 | -0.262 0.342 | -0.084 0.035 |
# 11 | -1.131 0.293 | 0.775 0.138 | -0.613 0.086 |
# 13 | -0.909 0.231 | -0.340 0.032 | 0.464 0.060 |
# 14 | 0.097 0.026 | 0.070 0.014 | 0.236 0.154 |
# 15 | -0.718 0.117 | -1.524 0.526 | -0.717 0.116 |
# 17 | -0.924 0.372 | 0.074 0.002 | 0.954 0.397 |
# 18 | -0.202 0.050 | 0.563 0.389 | 0.404 0.200 |
# Frequencies (the 10 first most contributed frequencies on the first principal plane)
# Dim.1 ctr cos2 Dim.2 ctr cos2 Dim.3 ctr cos2
# physically | -0.508 6.062 0.941 | -0.036 0.050 0.005 | -0.045 0.102 0.007 |
# to have | 0.241 5.654 0.727 | 0.054 0.451 0.037 | -0.027 0.157 0.009 |
# well | -0.124 0.790 0.168 | -0.255 5.254 0.703 | -0.073 0.593 0.057 |
# to feel | -0.217 1.174 0.222 | -0.321 4.059 0.484 | -0.158 1.353 0.117 |
# hungry | 0.548 1.504 0.254 | -0.630 3.158 0.336 | -0.367 1.478 0.114 |
# I | 0.360 2.927 0.537 | -0.213 1.623 0.188 | -0.114 0.639 0.053 |
# one | 0.246 0.964 0.241 | 0.369 3.449 0.542 | 0.120 0.500 0.057 |
# something | -0.826 2.959 0.431 | 0.444 1.353 0.124 | -0.734 5.121 0.340 |
# best | 0.669 4.182 0.602 | 0.091 0.123 0.011 | 0.225 1.041 0.068 |
# psychologically | -0.369 0.560 0.155 | -0.727 3.444 0.601 | 0.190 0.323 0.041 |
# Categorical variables
# Dim.1 cos2 Dim.2 cos2 Dim.3 cos2
# 21-35 | -0.148 0.347 | -0.063 0.063 | 0.148 0.347 |
# 36-50 | 0.089 0.108 | -0.037 0.019 | 0.120 0.199 |
# over 50 | 0.330 0.788 | 0.020 0.003 | -0.028 0.006 |
# under 21 | -0.271 0.484 | 0.080 0.042 | -0.240 0.382 |
# Man | -0.054 0.081 | 0.172 0.826 | 0.018 0.009 |
# Woman | 0.054 0.081 | -0.172 0.826 | -0.018 0.009 |
# fair | 0.042 0.029 | -0.008 0.001 | -0.144 0.342 |
# good | -0.007 0.001 | -0.119 0.185 | -0.077 0.077 |
# poor | -0.027 0.002 | 0.138 0.061 | 0.193 0.120 |
# very good | -0.007 0.000 | -0.011 0.001 | 0.027 0.005 |
plot(res.cagalt, choix = "quali.var", conf.ellip = TRUE, axes = c(1, 4))
plot(res.cagalt, choix = "freq", cex = 1.5, col.freq = "darkgreen",
select = "contrib 10")
# i 
# I 
# K 
# j 
# k 
# Age 
# Gender 
# Health 
# Words </div>
# Age 
# Gender 
# Health </div>
# 1               
# 1 
# ∑
