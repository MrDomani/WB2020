library(wec)
data(PUMS)
PUMS$race.effect <- factor(PUMS$race)
contrasts(PUMS$race.effect) <- contr.sum(4)
contrasts(PUMS$race.effect)
# [,1] [,2] [,3]
# Hispanic 1 0 0
# Black 0 1 0
# Asian 0 0 1
# White -1 -1 -1
m.effect <- lm(wage ~ race.effect, data=PUMS)
summary(m.effect)$coefficients
# Estimate Std. Error t value Pr(>|t|)
# (Intercept) 49762 954 52.2 0.0e+00
# race.effect1 -8724 1649 -5.3 1.3e-07
# race.effect2 -12096 1702 -7.1 1.3e-12
# race.effect3 16135 2042 7.9 3.0e-15
PUMS$race.wec <- factor(PUMS$race)
contrasts(PUMS$race.wec) <- contr.wec(PUMS$race.wec, "White")
contrasts(PUMS$race.wec)
# Hispanic Black Asian
# Hispanic 1.00 0.00 0.000
# Black 0.00 1.00 0.000
# Asian 0.00 0.00 1.000
# White -0.12 -0.11 -0.069
m.wec <- lm(wage ~ race.wec, data=PUMS)
summary(m.wec)$coefficients
# Estimate Std. Error t value Pr(>|t|)
# (Intercept) 52320 587 89.1 0.0e+00
# race.wecHispanic -11282 1810 -6.2 4.8e-10
# race.wecBlack -14654 1905 -7.7 1.6e-14
# race.wecAsian 13577 2484 5.5 4.7e-08
PUMS$race.wec.b <- PUMS$race.wec
contrasts(PUMS$race.wec.b) <- contr.wec(PUMS$race.wec, "Black")
m.wec.b <- lm(wage ~ race.wec.b, data=PUMS)
summary(m.wec.b)$coefficients
# Estimate Std. Error t value Pr(>|t|)
# (Intercept) 52320 587 89.1 0.0e+00
# race.wec.bHispanic -11282 1810 -6.2 4.8e-10
# race.wec.bAsian 13577 2484 5.5 4.7e-08
# race.wec.bWhite 2128 325 6.5 6.5e-11
m.wec2 <- lm(wage ~ race.wec + education.int, data=PUMS)
summary(m.wec2)$coefficients
# Estimate Std. Error t value Pr(>|t|)
# (Intercept) 52320 560 93.4 0.0e+00
# race.wecHispanic -4955 1738 -2.9 4.4e-03
# race.wecBlack -11276 1820 -6.2 6.0e-10
# race.wecAsian 5151 2385 2.2 3.1e-02
# education.int 9048 287 31.5 7.9e-208
contrasts(PUMS$education.wec) <- contr.wec(PUMS$education.cat, "High school")
PUMS$education.wec <- PUMS$education.cat
wec.interact(PUMS$race.wec, PUMS$education.int, output.contrasts = TRUE)
# [,1] [,2] [,3]
# 1 1.0 0.000 0.000
# 2 0.0 1.000 0.000
# 3 0.0 0.000 1.000
# 4 -0.1 -0.098 -0.066
PUMS$race.educint <- wec.interact(PUMS$race.wec, PUMS$education.int)
m.wec.educ <- lm(wage ~ race.wec + education.int + race.educint, data=PUMS)
summary(m.wec.educ)$coefficients
# Estimate Std. Error t value Pr(>|t|)
# (Intercept) 52320 559 93.5 0.0e+00
# race.wecHispanic -4955 1736 -2.9 4.3e-03
# race.wecBlack -11276 1817 -6.2 5.7e-10
# race.wecAsian 5151 2381 2.2 3.1e-02
# education.int 9048 287 31.6 2.3e-208
# race.educintinteractHispanic -3266 977 -3.3 8.3e-04
# race.educintinteractBlack -3293 990 -3.3 8.8e-04
# race.educintinteractAsian 3575 1217 2.9 3.3e-03
PUMS$race.educat <- wec.interact(PUMS$race.wec, PUMS$education.wec)
m.wec.educwec <- lm(wage ~ race.wec + education.wec + race.educat, data=PUMS)
summary(m.wec.educwec)$coefficients
# Estimate Std. Error t value Pr(>|t|)
# (Intercept) 52320 569 92.0 0.0e+00
# race.wecHispanic -6645 1764 -3.8 1.7e-04
# race.wecBlack -11738 1849 -6.3 2.3e-10
# race.wecAsian 7528 2419 3.1 1.9e-03
# education.wecDegree 14343 572 25.1 1.6e-134
# race.educatx1Hispanic:x2Degree -7674 2441 -3.1 1.7e-03
# race.educatx1Black:x2Degree -6682 2252 -3.0 3.0e-03
# race.educatx1Asian:x2Degree 4022 1536 2.6 8.8e-03
# rense.nieuwenhuis@sofi.su.se
# m.tegrotenhuis@maw.ru.nl
# b.pelzer@maw.ru.nl
# 30000
# 40000
# 50000
# 60000
# 70000
# 80000
# −2 −1 0 1 2 3
# Education (mean centered)
# Income in $
# Effect Education
# 1. White
# 2. Black
# 3. Unweighted Average
# 4. Weighted Average</div>
# Grand MeanGrand MeanGrand MeanGrand Mean
# Weighted MeanWeighted MeanWeighted MeanWeighted Mean</div>
# 40000
# 50000
# 60000
# 70000
# Hispanic White Black Asian
# Income in $
# Effect Treatment Weighted Effect</div>
# ●
# ●
# ●
# ●
# ●
# ●</div>
# ∑
