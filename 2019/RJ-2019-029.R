###############replication code for the submitted paper regarding R package ipwErrorY###############
library("ipwErrorY")

###############Section 4.1 example of estimation with known misclassification probabilities###############
##create a dataset with sensitivity=0.95 and specificity=0.85
set.seed(100)
X1 = rnorm(2000) 
A = rbinom(2000, 1, 1/(1 + exp(-0.2 - X1)))
Y = rbinom(2000, 1, 1/(1 + exp(-0.2 - A - X1)))
y1 = which(Y == 1)
y0 = which(Y == 0) 
Yast = Y
Yast[y1] = rbinom(length(y1), 1, 0.95)
Yast[y0] = rbinom(length(y0), 1, 0.15)
da = data.frame(X1 = X1, A = A,Yast = Yast)
#display the data structure
head(da)
#apply the correction method with sensitivity=0.95 and specificity=0.85
KnownError(data = da, indA = "A", indYerror = "Yast", indX = "X1",
           sensitivity = 0.95, specificity = 0.85, confidence=0.95)



###############Section 4.2 example of using validation data###############
#create main data and validation data with sensitivity=0.95 and specificity=0.85
set.seed(100)
X1= rnorm(1200)   
A = rbinom(1200, 1, 1/(1 + exp(-0.2 - X1)))
Y= rbinom(1200, 1, 1/(1 + exp(-0.2 - A - X1)))
y1 = which(Y == 1)
y0 = which(Y==0) 
Yast = Y
Yast[y1] = rbinom(length(y1), 1, 0.95)
Yast[y0] = rbinom(length(y0), 1, 0.15)
mainda = data.frame(A = A, X1 = X1, Yast = Yast)
X1 = rnorm(800)   
A = rbinom(800, 1, 1/(1 + exp(-0.2 - X1)))
Y = rbinom(800, 1, 1/(1 + exp(-0.2 - A - X1)))
y1 = which(Y == 1)
y0 = which(Y == 0) 
Yast = Y
Yast[y1] = rbinom(length(y1), 1, 0.95)
Yast[y0] = rbinom(length(y0), 1, 0.15)
validationda = data.frame(A = A, X1 = X1, Y = Y, Yast = Yast)
#display the data structure
head(mainda)
head(validationda)
#apply the optimal linear combination correction method
EstValidation(maindata = mainda, validationdata = validationda, indA = "A", 
              indYerror = "Yast",  indX = "X1",  indY = "Y", confidence=0.95)



###############Section 4.3 example of using replicates###############
#create a dataset with sensitivity=0.95 and specificity=0.85
set.seed(100) 
X1 = rnorm(2000)   
A = rbinom(2000, 1, 1/(1 + exp(-0.2 - X1)))
Y = rbinom(2000, 1, 1/(1 + exp(-0.2 - A - X1)))
y1 = which(Y == 1)
y0 = which(Y == 0) 
Yast1 = Y
Yast1[y1] = rbinom(length(y1), 1, 0.95)
Yast1[y0] = rbinom(length(y0), 1, 0.15)
Yast2 = Y
Yast2[y1] = rbinom(length(y1), 1, 0.95)  
Yast2[y0] = rbinom(length(y0), 1, 0.15)
da = data.frame(A = A, X1 = X1, Yast1 = Yast1, Yast2 = Yast2)
#display the data structure
head(da)
#apply the correction method assuming specificity=0.85
Est2Replicates(data = da,  indA = "A", indYerror = c("Yast1", "Yast2"),
               indX = "X1", constraint = "known specificity", sensitivity = NULL, 
               specificity = 0.85, prevalence = NULL, confidence=0.95)



###############Section 4.4 example of doubly robust estimation###############
#create a dataset with sensitivity=0.95 and specificity=0.85
set.seed(100)
X = rnorm(2000)   
xx = X^2
A = rbinom(2000, 1, 1/(1 + exp(-0.1 - X - 0.2*xx)))
Y = rbinom(2000, 1, 1/(1 + exp(1 - A - 0.5*X - xx)))
y1 = which(Y == 1)
y0 = which(Y == 0) 
Y[y1] = rbinom(length(y1), 1, 0.95)
Y[y0] = rbinom(length(y0), 1, 0.15)
Yast = Y
da = data.frame(A = A, X = X, xx = xx, Yast = Yast)
#display the data structure
head(da)
#apply the doubly robust correction method with sensitivity=0.95 and specificity=0.85
KnownErrorDR(data = da, indA = "A",  indYerror = "Yast", indXtrt = c("X", "xx"), 
             indXout = c("X", "xx"), sensitivity = 0.95, specificity = 0.85,  
             sharePara = FALSE, confidence=0.95)

 

###############Section 5 bootstrap confidence intervals###############
drFUN<-function(dt) {
  KnownErrorDR(data = dt, indA = "A",  indYerror = "Yast", indXtrt = c("X", "xx"), 
               indXout = c("X", "xx"), sensitivity = 0.95, specificity = 0.85,  
               sharePara = FALSE, confidence=0.95)$`Estimate`
}
#obtain estimate
EST=drFUN(dt=da)
#bootstrapping with 200 replications 
set.seed(100)
resultsBoot=replicate(200,drFUN(dt=da[sample(1:nrow(da),replace=TRUE),]))
#obtain 95% bootstrap confidence interval 
#calculate normality-based confidence interval 
STD=sd(resultsBoot)
lowN=EST-qnorm(1-(1-0.95)/2)*STD
upN=EST+qnorm(1-(1-0.95)/2)*STD
CIn=c(lowN,upN)
#calculate percentile-based confidence interval 
lowP=as.numeric(quantile(resultsBoot,probs=0.025))
upP=as.numeric(quantile(resultsBoot,probs=0.975))
CIp=c(lowP,upP)
#print normality-based confidence interval 
CIn
#print percentile-based confidence interval
CIp



###############Section 5 error message for missing data###############
#modify dataset da to produce missing data
da[1,1]=NA
#an error message 
KnownErrorDR(data = da, indA = "A",  indYerror = "Yast", indXtrt = c("X", "xx"), 
             indXout = c("X", "xx"), sensitivity = 0.95, specificity = 0.85,  
             sharePara = FALSE, confidence=0.95)


