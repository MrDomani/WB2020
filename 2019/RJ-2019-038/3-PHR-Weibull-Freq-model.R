
options(digits=2)
# Frequentist approach
fitFreqPHR <- FreqID_HReg(form, data=simCIBMTR, model="semi-Markov")

# Summary
print(summary(fitFreqPHR))

# Estimated survival functions (Figure 2.1)
pred <- predict(fitFreqPHR, time=seq(0,365,1), tseq=seq(from=0,to=365,by=30))
plot(pred, plot.est="Surv")

# Estimated hazard functions (Figure 2.2)
plot(pred, plot.est="Haz")
