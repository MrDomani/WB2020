##############################################
# jomo: tutorial with single level data      #
##############################################
library(mice) 
library(jomo)
summary(JSPmiss)
class(JSPmiss$ravens)

load("ExamScores.RData")
load("JSPmiss.RData")

md.pattern(JSPmiss)

class(JSPmiss$english)
JSPmiss <- within(JSPmiss, english <- as.numeric(english))
Y <- JSPmiss[, c("english", "ravens")]
is.numeric(Y)
JSPmiss$cons <- 1
X <- JSPmiss[, c("cons", "sex")]
set.seed(1569)
imp <- jomo(Y = Y, X = X, nburn = 1000, nbetween = 1000, nimp = 5)

class(imp)
names(imp)
dim(imp)

head(imp)
head(imp[imp$Imputation == 5,])

beta.start <- matrix(1, 2, 2)
l1cov.start <- diag(2, 2)
l1cov.prior <- diag(2, 2);
set.seed(1569)
imp2 <- jomo(Y, X = X, beta.start = beta.start, l1cov.start = l1cov.start,
             l1cov.prior = l1cov.prior, nburn = 200,
             nbetween = 200, nimp = 5)

library(mitools) 
imp.list <- imputationList(split(imp, imp$Imputation)[-1])
fit.imp <- with(data = imp.list, lm(english ~ ravens + sex))
coefs <- MIextract(fit.imp, fun = coef)
vars <- MIextract(fit.imp, fun = function(x) diag(vcov(x)))
results <- MIcombine(coefs, vars)
summary(results)

JSPmiss <- within(JSPmiss, fluent <- factor(fluent))
Y <- JSPmiss[, c("english", "ravens", "fluent")]
X <- JSPmiss[, c("cons", "sex")]
set.seed(1569)
imp <- jomo(Y = Y, X = X)

beta.start <- matrix(0, 2, 4) 
l1cov.start <- diag(2, 4)
set.seed(1569)
imp <- jomo(Y = Y, X = X, beta.start = beta.start, l1cov.start = l1cov.start)


############################################
# jomo: tutorial with multilevel data      #
############################################

clus <- JSPmiss$school
Y <- JSPmiss[, c("english", "ravens", "fluent")]
JSPmiss$cons <- 1
X <- JSPmiss[, c("cons", "sex")]
set.seed(1569)
imp <- jomo(Y = Y, X = X, clus = clus, nburn = 2000, nbetween = 1000, nimp = 5)

library(lme4)
imp.list <- imputationList(split(imp, imp$Imputation)[-1])
fit.imp <- with(data = imp.list, lmer(english ~ ravens + sex + factor(fluent) + (1|clus)))
coefs <- MIextract(fit.imp, fun = fixef)
vars <- MIextract(fit.imp, fun = function(x) diag(vcov(x)))
results <- MIcombine(coefs, vars)
summary(results)

Z <- JSPmiss[, c("cons", "sex")] 
imp <- jomo(Y = Y, X = X, Z = Z, clus = clus)

beta.start <- matrix(1, 2, 4) # initialise fixed effects to zero
u.start <- matrix(0.5, nlevels(JSPmiss$school), 4) # initialise all level-2 random effects to 0.5
l1cov.start <- diag(2, 4)     # initialise diagonal covariance matrix of 2's for level 1
l2cov.start <- diag(2, 4)     # initialise diagonal covariance matrix of 2's for level 1
l2cov.prior <- diag(2, 4);    # set scale matrix inverse-Wishart prior for level 2 covariance matrix

set.seed(1569)
imp <- jomo(Y = Y, X = X, clus = clus, beta.start = beta.start, u.start = u.start,
            l1cov.start = l1cov.start, l2cov.start = l2cov.start, l2cov.prior = l2cov.prior,
            nburn = 2000, nbetween = 1000, nimp = 5)


Y <- JSPmiss[, c("english", "ravens", "fluent")]
X <- JSPmiss[, c("cons", "sex")]
clus <- data.frame(JSPmiss$school)

imp2 <- jomo(Y = Y, X = X, clus = clus, meth = "fixed")

imp3 <- jomo(Y = Y, X = X, clus = clus, meth = "random")

l1cov.start.1 <- diag(2, 4)
l1cov.start <- matrix(l1cov.start.1, nrow = 4 * nlevels(JSPmiss$school), ncol = 4, byrow = TRUE)
a.start <- 7
imp <- jomo(Y = Y, X = X, clus = clus, l1cov.start = l1cov.start, a = a.start,
            meth = "random")

###################################
# Imputing level 2 variables      #
###################################

Y <- ExamScores[, c("normexam", "standlrt")]
Y2 <- ExamScores[, "avslrt", drop = FALSE]
clus <- data.frame(ExamScores$school)
set.seed(1569)
imp <- jomo(Y = Y, Y2 = Y2, clus = clus)

#####################################
# Checking convergence of MCMC      #
#####################################

# Define data.frames with outcomes and covariates of imputation model
Y <- JSPmiss[, c("english", "ravens")]
X <- JSPmiss[, c("cons", "sex")]
# Run jomo.MCMCchain
imp <- jomo.MCMCchain(Y = Y, X = X, nburn = 5000)

plot(imp$collectbeta[1, 1, 1:5000], type = "l",
     ylab = expression(beta["m,0"]),
     xlab = "Iteration number" )

JSPmiss <- within(JSPmiss, fluent <- factor(fluent))
Y <- JSPmiss[, c("english", "ravens", "fluent")]
X <- JSPmiss[, c("cons", "sex")]
clus <- data.frame(JSPmiss$school)
imp2 <- jomo.MCMCchain(Y = Y, X = X, clus=clus, nburn = 5000)
plot(imp2$collectomega[1, 1, 1:5000], type = "l",
     ylab = expression(omega[e,1,1]^2),
     xlab = "Iteration number" , ylim= c(300,500))

###############################
# Using jomo in practice      #
###############################


JSPmiss <- within(JSPmiss, fluent <- factor(fluent))
Y <- JSPmiss[, c("english", "ravens", "fluent")]
X <- JSPmiss[, c("cons", "sex")]
set.seed(1569)
imp <- jomo(Y = Y, X = X, nimp = 2)
set.seed(1569)
imp1 <- jomo.MCMCchain(Y = Y, X = X)
beta.start <- imp1$collectbeta[,,1000] 
l1cov.start <- imp1$collectomega[,,1000] 
start.imp <- imp1$finimp.latnorm 
imp2 <- jomo.MCMCchain(Y = Y, X = X, beta.start = beta.start, l1cov.start = l1cov.start,
                       start.imp = start.imp, nburn = 1000)

JSPmiss <- within(JSPmiss, fluent <- factor(fluent))
Y <- JSPmiss[, c("english", "ravens", "fluent")]
X <- JSPmiss[, c("cons", "sex")]
imp1 <- jomo.MCMCchain(Y = Y, X = X)
l1cov.guess <- apply(imp1$collectomega, c(1, 2), mean)
l1cov.prior <- l1cov.guess*4
imp <- jomo(Y = Y, X = X, l1cov.prior = l1cov.prior)

################################################
# mitml: an alternative interface to jomo      #
################################################

library(mitml)

fml <- english + ravens +fluent  ~ sex +  (1|school)

imp <- jomoImpute(data = JSPmiss, formula = fml, n.burn = 1000, n.iter = 1000, m = 5,
                  seed = 1569)

summary(imp)
plot(imp, trace = "all")

imp.list <- mitmlComplete(imp, print = "all")
fit.imp <- with(imp.list, lmer(english ~ ravens +sex + fluent +  (1|school)))
testEstimates(fit.imp, var.comp = TRUE)
