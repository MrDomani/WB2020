library(OrthoPanels)
head(BES_panel)
# n t Econ Clegg Brown Cameron Approve NHS Terror PID Tax
# 1 1 1 3 0 9 0 7 0 0 1 6
# 2 2 1 4 0 10 0 8 0 0 1 8
# 3 3 1 3 0 5 4 7 0 0 0 6
# 4 4 1 2 0 7 3 4 0 0 1 6
# 5 5 1 2 0 0 0 0 0 0 0 5
# 6 6 1 2 0 7 0 8 0 0 1 4
BES.opm.model<-opm(Approve ~ Econ + Clegg + Brown + Cameron + NHS + Terror + PID + Tax,
                   data = BES_panel, index = c('n','t'), n.samp = 10000, add.time.indicators = TRUE)
# data = BES_panel, index = c('n','t'), n.samp = 10000, add.time.indicators = TRUE)
summary(BES.opm.model)
# Call:
# opm(x = Approve ~ Econ + Clegg + Brown + Cameron + NHS + Terror +
# PID + Tax, data = BES_panel, index = c("n", "t"), n.samp = 10000,
# add.time.indicators = TRUE)
# Parameter estimates:
# <--95CI <--68CI med 68CI--> 95CI-->
# rho 0.128000 0.15800000 0.189000 0.224000 0.25800000
# sig2 1.611477 1.68193076 1.759734 1.846594 1.93906618
# beta.Econ -0.038086 0.00077932 0.041545 0.081118 0.12235574
# beta.Clegg -0.020535 0.00060795 0.022996 0.045020 0.06678845
# beta.Brown 0.297306 0.31959250 0.343490 0.367214 0.39072453
# beta.Cameron -0.124476 -0.10195910 -0.077744 -0.054332 -0.03195979
# beta.NHS -0.235810 -0.18692841 -0.136087 -0.085502 -0.03647700
# beta.Terror -0.153162 -0.11566089 -0.077034 -0.037453 -0.00036214
# beta.PID 0.488818 0.64660493 0.804710 0.959541 1.10995704
# beta.Tax 0.017598 0.04022762 0.063176 0.086574 0.10903010
# beta.tind.2 0.217953 0.27900281 0.342908 0.406506 0.46890678
confint(BES.opm.model, level = 0.90)
# 5% 95%
# rho 0.13800000 0.24700000
# sig2 1.63217694 1.90996203
# beta.Econ -0.02625512 0.10794038
# beta.Clegg -0.01312387 0.06009414
# beta.Brown 0.30435125 0.38292909
# beta.Cameron -0.11665040 -0.03883916
# beta.NHS -0.21928907 -0.05272413
# beta.Terror -0.14110536 -0.01271258
# beta.PID 0.53904739 1.05947755
# beta.Tax 0.02570175 0.10189246
# beta.tind.2 0.23669235 0.4483367
caterplot(BES.opm.model)
abline(v=0)
plot(BES.opm.model, "rho")
quantile(BES.opm.model$samples$beta[, 1] / (1 - BES.opm.model$samples$rho),
probs = c(0.025, 0.5, 0.975))
# 2.5% 50% 97.5%
# -0.04723233 0.05098789 0.15025695
# ∏
# ∏
# ∑
# ∑
# ∑
# 0
# 0
