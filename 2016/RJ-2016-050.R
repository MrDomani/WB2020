install.packages("comf_0.1.6.tar.gz", repos=NULL, type="source")
library(comf)
lsCond <- createCond()
lsCond$ta <- 21:30
lsCond$rh <- 51:60
lsCond$met <- 1.0
ta <- 21:30
rh <- 51:60
met <- 1.0
dfCond <- data.frame(ta, rh, met)
lsCond2 <- as.list(dfCond)
# using lsCond from above does not produce a warning
calcComfInd(lsCond, request="all")
# using lsCond2 from above displays 31 warnings which report
# the corresponding standard values used
calcComfInd(lsCond2, request="all")
warnings()
# the results however are identical
ta <- c(20,22,24)
tr <- ta
vel <- rep(0.15,3)
rh <- rep(50,3)
maxLength <- max(sapply(list(ta, tr, vel, rh), length))
SET <- sapply(seq(maxLength),
function(x) { calcSET(ta[x], tr[x], vel[x], rh[x]) } )
library(comf)
library(psych)
data(dfField)
describe(dfField)
# creating a list with standard values
lsField <- createCond()
# assigning the variables included in the data set to the list
variables <- c("ta", "tr", "vel", "rh", "clo", "met", "trm", "asv", "tao")
for(i in 1:length(variables)) {
lsField[[variables[i]]] <- dfField[[variables[i]]]
}
lsField$epCoeff <- calcepCoeff(lsField)$epCoeff
lsField$apCoeff <- calcapCoeff(lsField)$apCoeff
lsField$esCoeff <- calcesCoeff(lsField)$esCoeff
lsField$asCoeff <- calcasCoeff(lsField)$asCoeff
indices <- c('pmv', 'pmvadj', 'apmv', 'epmv', 'ATHBpmv',
'pts', 'ptsa', 'ptse', 'ATHBpts')
results <- calcComfInd(lsField, request = indices)
asv.cat <- cutTSV(dfField$asv)
results.cat <- lapply(seq(length(indices)), function(i) {cutTSV(results[,i])})
names(results.cat) <- indices
# calculating mean value of bias between predicted and actual sensation vote
# for each comfort index
meanBias <- sapply(indices, function(i) {
calcBias(asv.cat, results.cat[[i]])$meanBias
})
# calculating standard error of bias between predicted and actual sensation vote
# for each comfort index
seBias <- sapply(indices, function(i) {
calcBias(asv.cat, results.cat[[i]])$seBias
})
# calculating the true positive rate for each comfort index
TPR <- sapply(indices, function(i) {
calcTPRTSV(asv.cat, results.cat[[i]])
})
library(ggplot2)
library(plyr)
library(reshape2)
group <- c("PMV", "PMVadj", "aPMV", "ePMV", "ATHB pmv", "PTS", "PTSa",
"PTSe", "ATHB pts")
lower <- meanBias - seBias
upper <- meanBias + seBias
fig4Win <- data.frame(meanBias, TPR, group, lower, upper)
fig4Win$variable <- rep(2,9)
fig4Win$group <- factor(fig4Win$group, levels = fig4Win$group)
addline_format <- function(x,...){
gsub('\\s','\n',x)
}
means.barplot <- qplot(x=group, y=meanBias, data=fig4Win, geom="point",
stat="identity", position="dodge", ymax=.5) +
scale_x_discrete(breaks=unique(fig4Win$group),
labels=addline_format(c("PMV","PMVadj","aPMV","ePMV",
"ATHB pmv","PTS","PTSa","PTSe","ATHB pts")))
means.barplot + geom_errorbar(aes(ymax=upper,ymin=lower),
position=position_dodge(0.9), data=fig4Win) +
theme_bw() +
xlab("Comfort indices") +
ylab("bias PSV - ASV") +
ylim(c(-1,.5))
## uncomment next line to save file to current working directory
#ggsave("Fig1_MeanBias.png")
means.barplot <- qplot(x=group, y=TPR*100, data=fig4Win, geom="point",
stat="identity", position="dodge", ymax=100) +
scale_x_discrete(breaks=unique(fig4Win$group),
labels=addline_format(c("PMV", "PMVadj", "aPMV", "ePMV", "ATHB pmv",
"PTS", "PTSa", "PTSe", "ATHB pts")))
means.barplot +
theme_bw() +
xlab("Comfort indices") +
ylab("True positive rate") +
ylim(c(0,100))
## uncomment next line to save file to current working directory
#ggsave("Fig1_TPR.png")
