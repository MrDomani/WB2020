
options(digits=2)
# Bayesian approach

#####################
##      Model      ##
#####################
myModel=c("semi-Markov","PEM")

#####################
##  Initial values ##
#####################
startValues <- initiate.startValues_HReg(form, simCIBMTR, model=myModel, nChain=3)

#####################
## Hyperparameters ##
#####################
hyperParams <- list(theta=c(0.5, 0.05),
               PEM=list(PEM.ab1=c(0.5, 0.05), PEM.ab2=c(0.5, 0.05), PEM.ab3=c(0.5, 0.05),
               PEM.alpha1=10, PEM.alpha2=10, PEM.alpha3=10))

###################
## MCMC SETTINGS ##
###################
sg_max <- c(max(simCIBMTR$time1[simCIBMTR$event1==1]),
            max(simCIBMTR$time2[simCIBMTR$event1==0 & simCIBMTR$event2==1]),
            max(simCIBMTR$time2[simCIBMTR$event1==1 & simCIBMTR$event2==1]))

mcmcParams <- list(run=list(numReps=numReps, thin=thin, burninPerc=burninPerc),
              storage=list(nGam_save=0, storeV=rep(FALSE,3)),
              tuning=list(mhProp_theta_var=0.05, Cg=rep(0.2,3), delPertg=rep(0.5,3),
              rj.scheme=1, Kg_max=rep(50,3), sg_max=sg_max, time_lambda1=seq(1,sg_max[1],1),
              time_lambda2=seq(1,sg_max[2],1), time_lambda3=seq(1,sg_max[3],1)))

fitBayesPHR <- BayesID_HReg(form, data=simCIBMTR, model=myModel, hyperParams=hyperParams, startValues=startValues, mcmc=mcmcParams)

# Summary
print(fitBayesPHR,digits=2)
print(summary(fitBayesPHR))

# Convergence diagnostic via trace plot of multiple chains (Figure 3)
ymin <- min(fitBayesPHR$chain1$theta.p,fitBayesPHR$chain2$theta.p,fitBayesPHR$chain3$theta.p)
ymax <- max(fitBayesPHR$chain1$theta.p,fitBayesPHR$chain2$theta.p,fitBayesPHR$chain3$theta.p)

par(mfrow=c(1,1))
plot(fitBayesPHR$chain1$theta.p,type="l",col="red",ylim=c(ymin,ymax), xlab="iteration", ylab=expression(theta))
lines(fitBayesPHR$chain2$theta.p,type="l",col="green")
lines(fitBayesPHR$chain3$theta.p,type="l",col="blue")
