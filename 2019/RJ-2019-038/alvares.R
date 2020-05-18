
install.packages("SemiCompRisks")
require(SemiCompRisks)

# Simulating outcomes using CIBMTR covariates (Appendix C)
source("1-Simulated-data.R")

# Descriptive analysis (Table 1)
source("2-Descriptive-analysis.R")

# Independent semi-Markov PHR model with Weibull baseline hazards (Section 6.1)
set.seed(1405); source("3-PHR-Weibull-Freq-model.R")

# MCMC setting (number of scans; extent of thinning; proportion of burn-in)

# Setting in Section 6.2
# WARNING: It takes a long time to run
# numReps=5e6; thin=1e3; burninPerc=0.5

# Setting in Appendix C
numReps=5e4; thin=5e1; burninPerc=0.5

# Independent semi-Markov PHR model with PEM baseline hazards
set.seed(1405); source("4-PHR-PEM-Bayes-model.R")

# Independent AFT model with log-Normal baseline survival distribution
set.seed(1405); source("5-AFT-LN-Bayes-model.R")
