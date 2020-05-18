###############################################################################################
################################ Real Data Analysis  ##########################################
###############################################################################################
library(gee)
library(swgee)

##load the dataset BMI
data(BMI)
bmidata <- BMI

##set up the covariance matrix of the measurement error 
rho <- 0
sigma1 <- 0.5
sigma2 <- 0.5
sigma <- matrix(0,2,2)
sigma[1,1] <- sigma1*sigma1
sigma[1,2] <- rho*sigma1*sigma2
sigma[2,1] <- sigma[1,2]
sigma[2,2] <- sigma2*sigma2

##naive method, ignore missingness and measurement error
output1 <- gee(bbmi~sbp+chol+age, id = id, data = bmidata,
            family=binomial(link="logit"), corstr="independence")

##swgee method ##########
set.seed(1000)
output2 <- swgee(bbmi~sbp+chol+age, data=bmidata, id=id,
            family=binomial(link="logit"),corstr="independence",
            missingmodel=O~bbmi+sbp+chol+age, SIMEXvariable=c("sbp","chol"),
            SIMEX.err=sigma, repeated=FALSE, B=100, lambda=seq(0, 2, 0.5))

summary(output2)


####plot the swgee estimates ########
plot(output2,"sbp")



###############################################################################################
################################ Simulation Studies  ##########################################
###############################################################################################
library(mvtnorm)
library(swgee)
library(geepack)
library(gee)

n <- 200
m <- 3
beta1 <- log(1.5)
beta2 <- log(1.5)
betaz <- log(0.75)

#sigma1 sigma2 =0.25, =0.5 , =0.75
sigma1 <- 0.25
sigma2 <- 0.25
alpha0 <- 0.5
alpha1 <- 0.5
alpha2 <- 0.1
alpha3 <- 0.1
alphaz <- 0.2
phox <- 0.5
pho <- 0.5


sigmae <- matrix(c(sigma1^2, pho*sigma1*sigma2, pho*sigma1*sigma2, sigma2^2), ncol=2)
sigma <- matrix(c(sigma1^2, pho*sigma1*sigma2, pho*sigma1*sigma2, sigma2^2), ncol=2)
mux <- c(0.5, 0.5)
sigmax <- matrix(c(1, phox, phox, 1), ncol=2)

###generate id and visit
id <- rep(1:n, each=m)
visit <- rep(1:m,n)

###generate x, z and y
set.seed(1000)
swgee.coef <- c()
swgee.se <- c()
gee.coef <- c()
gee.se <- c()
K <- 500
for(k in 1:K){
    nm <- n*m
    z <- rbinom(nm, 1, 0.5)
    x <- rmvnorm(nm, mux, sigmax)
    logitp <- beta1*x[,1]+beta2*x[,2]+betaz*z
    p <- 1/(1+exp(-logitp))
    y <- ifelse(runif(nm)<=p, 1, 0)

    ###################################### create the covariate with ME: w=x+ME ##########################
    w <- x
    for(i in 1:nm){
        w[i,]=rmvnorm(1, mean=x[i,], sigma=sigmae)
    }
    colnames(w) <- c("w1","w2")
    indata_pre <- data.frame(cbind(id, visit, y, x, z))
    colnames(indata_pre) <- c("id","visit","y","x1","x2","z")
    indata_pre.lag <- indata_pre[,3:6]
    colnames(indata_pre.lag) <- c("y.lag","x1.lag","x2.lag","z.lag")
    ###########################callate the lag values and fit the logsistic model for missing process##################
    indata_pre.lag <- rbind(matrix(0, ncol = 4, nrow = 1, dimnames = list(c("0"), c("y.lag","x1.lag","x2.lag","z.lag"))), indata_pre.lag)
    indata_pre.lag <- indata_pre.lag[1:nrow(indata_pre),]
    indata_tmp <- cbind(indata_pre,indata_pre.lag)
    indata_tmp[indata_tmp$visit==1, c("y.lag","x1.lag","x2.lag","z.lag")] <- 0
    ###missing indicator
    miss.logitp <- alpha0 + as.matrix(indata_tmp[,7:10])%*%matrix(c(alpha1, alpha2, alpha3, alphaz),ncol=1)
    miss.p <- 1/(1+exp(-miss.logitp))
    tmp <- miss.p
    indata_pre <- cbind(indata_pre, tmp)
    indata_pre <- data.frame(indata_pre)
    indata_pre$tmp[indata_pre$visit==1]=1
    pi <- ave(indata_pre$tmp, indata_pre$id, FUN=cumprod)
    indata_pre$R_tmp <- ifelse(runif(nm)<=pi, 1,0)
    indata_pre$R <- ave(indata_pre$R_tmp, indata_pre$id, FUN=cumprod)
    ###################################### set the missing observations to NA ####################################
    indata <- cbind(indata_pre[,c(1,2,3,6,9)],w)
    indata$y <- ifelse(indata$R==0, NA, indata$y)
    indata$w1 <- ifelse(indata$R==0, NA, indata$w1)
    indata$w2 <- ifelse(indata$R==0, NA, indata$w2)
    indata$z <- ifelse(indata$R==0, NA, indata$z)
    indata <- as.data.frame(indata)

    #####################naive method, ignore missingness and measurement error ##################################
    output1 <- gee(y~w1+w2+z, id = id, data = indata, family = binomial(link="logit"), corstr = "independence")
    gee.coef<-rbind(gee.coef, summary(output1)$coefficients[,"Estimate"])
    gee.se <- rbind(gee.se, summary(output1)$coefficients[,"Robust S.E."])
    output2 <- swgee(y~w1+w2+z, data = indata, id = id, family = binomial(link="logit"),corstr = "independence",
            missingmodel = R~y+w1+w2+z, SIMEXvariable = c("w1","w2"),
            SIMEX.err = sigmae, repeated = FALSE, B = 100, lambda = seq(0, 2, 0.5))
    swgee.coef<-rbind(swgee.coef, output2$beta[1:4])
    swgee.se<-rbind(swgee.se, sqrt(output2$beta[5:8]))
    print(k)
}

coef <- c(0, beta1, beta2, betaz)
gee.bias <- apply(gee.coef, 2, mean)-coef
gee.se1 <- apply(gee.coef, 2, sd)
gee.se2 <- apply(gee.se, 2, mean)
gee.upper <- gee.coef+1.96*gee.se
gee.lower <- gee.coef-1.96*gee.se
gee.CR <- c()
for (i in 1:K){
    gee.CR <- rbind(gee.CR, as.vector((coef>gee.lower[i, ])*(coef<gee.upper[i,])))
}
gee.CR <- round(apply(gee.CR,2,sum)/K*100, 2)

gee<- cbind(gee.bias, gee.se1, gee.CR)

######################################################

swgee.bias <- apply(swgee.coef, 2, mean)-coef
swgee.se1 <- apply(swgee.coef, 2, sd)
swgee.se2 <- apply(swgee.se, 2, mean)
swgee.upper <- swgee.coef+1.96*swgee.se
swgee.lower <- swgee.coef-1.96*swgee.se
swgee.CR <- c()
for (i in 1:K){
    swgee.CR <- rbind(swgee.CR, as.vector((coef>swgee.lower[i, ])*(coef<swgee.upper[i,])))
}
swgee.CR <- round(apply(swgee.CR,2,sum)/K*100, 2)

swgee<- cbind(swgee.bias, swgee.se1, swgee.CR)
