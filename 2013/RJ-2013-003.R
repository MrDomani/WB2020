library(FactoMineR)
data(mortality)
mfa <- MFA(mortality, group=c(9, 9), type=c("f", "f"), name.group=c("1979", "2006"))
round(mfa$eig,3) [1:4,]
# eigenvalue percentage cumulative \%
# of variance of variance
# comp 1 1.790 52.420 52.420
# comp 2 0.999 29.269 81.689
# comp 3 0.262 7.659 89.348
# comp 4 0.149 4.367 93.715
plot(mfa, choix="freq", invisible="ind", habillage="group")
lines(mfa$freq$coord[1:9, 1], mfa$freq$coord[1:9, 2], col="red")
lines(mfa$freq$coord[10:18, 1], mfa$freq$coord[10:18, 2], col="green")
# 3439 1666 1195 1328 966 1117 757 135 4
mortality[58, 10:18] # road accidents in 2006
sel <- c(2, 8:10, 15, 38, 58)
plot(mfa, lab.ind=FALSE, habillage="group")
text(mfa$ind$coord[sel, 1], mfa$ind$coord[sel, 2], rownames(mortality)[sel],
 pos=c(2, 2, 2, 2, 4, 2, 4))
round(mfa$ind$contr[c(2, 8:10, 15, 38, 58), 1:2], 3)
# Addiction to prescription medication 0.998 0.448
# Complications in pregnancy & childb. 0.685 0.527
# Congenital defects circulatory system 0.692 0.176
# Congenital defects nervous system 0.179 0.070
# Homicides 1.802 0.657
# Meningococcal disease 0.105 0.084
# Road accidents 34.295 23.364
mortality[58, 1:9] # road accidents in 1979
# 15-24 25-34 35-44 45-54 55-64 65-74 75-84 85-94 95 and more
# 15-24 25-34 35-44 45-54 55-64 65-74 75-84 85-94 95 and more
# 1214 785 646 599 443 362 454 137 8
sel <- c(2, 10, 41, 58)
plot(mfa, lab.ind=FALSE, habillage="group", partial=rownames(mortality)[sel])
text(mfa$ind$coord[sel,1], mfa$ind$coord[sel,2], rownames(mortality)[sel],pos=4)
# column-
# margin
# 1              
# margin
# 1 J
# margin
# 1
# 1               
# column-
# margin
# 1979 2006</div>
# ∑
# ∑
# ∑
# t
# i
# I=62
# ×
