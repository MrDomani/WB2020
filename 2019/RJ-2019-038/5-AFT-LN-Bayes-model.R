
options(digits=2)
# Bayesian approach

# Outcomes for AFT model
simCIBMTR$LT <- rep(0,dim(simCIBMTR)[1])
simCIBMTR$y1L <- simCIBMTR$y1U <- simCIBMTR[,1]
simCIBMTR$y1U[which(simCIBMTR[,2]==0)] <- Inf
simCIBMTR$y2L <- simCIBMTR$y2U <- simCIBMTR[,3]
simCIBMTR$y2U[which(simCIBMTR[,4]==0)] <- Inf

formAFT <- Formula(LT | y1L + y1U | y2L + y2U ~ dTypeALL + dTypeCML + dTypeMDS +
           sexP | dTypeALL + dTypeCML + dTypeMDS | dTypeALL + dTypeCML + dTypeMDS)

#####################
##      Model      ##
#####################
myModel <- "LN"

#####################
##  Initial values ##
#####################
startValues <- initiate.startValues_AFT(formAFT, simCIBMTR, model=myModel, nChain=3)

#####################
## Hyperparameters ##
#####################
hyperParams <- list(theta=c(0.5,0.05), LN=list(LN.ab1=c(0.5,0.05),
               LN.ab2=c(0.5,0.05), LN.ab3=c(0.5,0.05)))

###################
## MCMC SETTINGS ##
###################
mcmcParams <- list(run=list(numReps=numReps, thin=thin, burninPerc=burninPerc),
              storage=list(nGam_save=0, nY1_save=0, nY2_save=0, nY1.NA_save=0),
              tuning=list(betag.prop.var=rep(0.01,3), mug.prop.var=rep(0.01,3),
              zetag.prop.var=rep(0.01,3), gamma.prop.var=0.01))

fitBayesAFT <- BayesID_AFT(formAFT, simCIBMTR, model=myModel, hyperParams, startValues, mcmcParams)

# Summary
print(fitBayesAFT,digits=2)
print(summary(fitBayesAFT))
