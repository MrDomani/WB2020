data(film)
anova.result <- EMSanova(thickness ~ Gate + Operator + Day, data = film,
type = c("F", "R", "R"))
anova.result
# Df SS MS Fvalue Pvalue Sig
# Gate 2 1.573172222 0.786586111
# Operator 2 0.112072222 0.056036111 18.7656 0.0506 .
# Gate:Operator 4 0.042844444 0.010711111 4.3229 0.0926 .
# Day 1 0.001002778 0.001002778 0.3358 0.6208
# Gate:Day 2 0.011338889 0.005669444 2.2881 0.2175
# Operator:Day 2 0.005972222 0.002986111 9.188 0.0018 **
# Gate:Operator:Day 4 0.009911111 0.002477778 7.6239 9e-04 ***
# Residuals 18 0.005850000 0.000325000
# EMS
# Gate Error+2Gate:Operator:Day+6Gate:Day+4Gate:Operator+12Gate
# Operator Error+6Operator:Day+12Operator
# Gate:Operator Error+2Gate:Operator:Day+4Gate:Operator
# Day Error+6Operator:Day+18Day
# Gate:Day Error+2Gate:Operator:Day+6Gate:Day
# Operator:Day Error+6Operator:Day
# Gate:Operator:Day Error+2Gate:Operator:Day
# Residuals Error
ApproxF(SS.table = anova.result, approx.name = "Gate")
# $Appr.F
# [1] 48.17076
# $df1
# [1] 2.01261
# $df2
# [1] 5.995597
# $Appr.Pvalue
# [1] 0.0002010433
del.ID <- c("Gate:Day", "Residuals")
PooledANOVA(anova.result, del.ID)
# Df SS MS Fvalue Pvalue Sig
# Gate 2 1.5732 0.7866 73.4365 7e-04 ***
# Operator 2 0.1121 0.0560 18.7656 0.0506 .
# Gate:Operator 4 0.0428 0.0107 4.3229 0.0926 .
# Day 1 0.0010 0.0010 0.3358 0.6208
# Operator:Day 2 0.0060 0.0030 3.4745 0.0507 .
# Gate:Operator:Day 4 0.0099 0.0025 2.883 0.0491 *
# Residuals 20 0.0172 0.0009
# EMS
# Gate Error+2Gate:Operator:Day+4Gate:Operator+12Gate
# Operator Error+6Operator:Day+12Operator
# Gate:Operator Error+2Gate:Operator:Day+4Gate:Operator
# Day Error+6Operator:Day+18Day
# Operator:Day Error+6Operator:Day
# Gate:Operator:Day Error+2Gate:Operator:Day
# Residuals Error
data(baseball)
anova.result <- EMSanova(velocity ~ Group + Subject + test,
data = baseball,
type = c("F", "R", "F"),
nested = c(NA, "Group", NA),
level = c(1, 1, 2))
anova.result
# Df SS MS Fvalue Pvalue Sig Model.Level
# Group 2 28.139200 14.0696000 1.0426 0.3729 1
# Subject(Group) 18 242.905914 13.4947730 115.517 <0.0001 *** 1
# test 1 21.257486 21.2574857 181.9669 <0.0001 *** 2
# Group:test 2 12.381943 6.1909714 52.9955 <0.0001 *** 2
# Residuals 18 2.102771 0.1168206 2
# EMS
# Group Error+2Subject(Group)+14Group
# Subject(Group) Error+2Subject(Group)
# test Error+21test
# Group:test Error+7Group:test
# Residuals Error
data(rubber)
anova.result <- EMSanova(cure ~ Rep + Mix + Lap + Temp,
data = rubber,
type = c("R", "F", "F", "F"),
level = c(1, 3, 1, 2))
anova.result
# Df SS MS Fvalue Pvalue Sig Model.Level
# Rep 3 5.581019 1.8603395 2.2311 0.1105 1
# Lap 2 51.496852 25.7484259 4.9395 0.0539 . 1
# Rep:Lap 6 31.276481 5.2127469 6.2517 5e-04 *** 1
# Temp 2 2978.564630 1489.2823148 2167.7859 <0.0001 *** 2
# Rep:Temp 6 4.122037 0.6870062 0.8239 0.5626 2
# Lap:Temp 4 5.603148 1.4007870 0.5948 0.6732 2
# Rep:Lap:Temp 12 28.261296 2.3551080 2.8245 0.0146 * 2
# Mix 2 149.217963 74.6089815 53.0128 2e-04 *** 3
# Rep:Mix 6 8.444259 1.4073765 1.6879 0.1672 3
# Lap:Mix 4 4.894815 1.2237037 1.4772 0.2697 3
# Rep:Lap:Mix 12 9.940741 0.8283951 0.9935 0.4828 3
# Temp:Mix 4 47.853704 11.9634259 15.6467 1e-04 *** 3
# Rep:Temp:Mix 12 9.175185 0.7645988 0.917 0.5454 3
# Lap:Temp:Mix 8 10.688519 1.3360648 1.6024 0.1765 3
# Residuals 24 20.011481 0.8338117 3
# EMS
# Rep Error+27Rep
# Lap Error+9Rep:Lap+36Lap
# Rep:Lap Error+9Rep:Lap
# Temp Error+9Rep:Temp+36Temp
# Rep:Temp Error+9Rep:Temp
# Lap:Temp Error+3Rep:Lap:Temp+12Lap:Temp
# Rep:Lap:Temp Error+3Rep:Lap:Temp
# Mix Error+9Rep:Mix+36Mix
# Rep:Mix Error+9Rep:Mix
# Lap:Mix Error+3Rep:Lap:Mix+12Lap:Mix
# Rep:Lap:Mix Error+3Rep:Lap:Mix
# Temp:Mix Error+3Rep:Temp:Mix+12Temp:Mix
# Rep:Temp:Mix Error+3Rep:Temp:Mix
# Lap:Temp:Mix Error+4Lap:Temp:Mix
# Residuals Error
# lee.eunk@ewha.ac.kr
# ε
# ε
# ε
# ε
