milr(y, x, bag, lambda, numLambda, lambdaCriterion, nfold, maxit)
softmax(y, x, bag, alpha, ...)
fitted(object, type)
predict(object, newdata, bag_newdata, type)
library(magrittr)
library(milr)
set.seed(99)
# set the size of dataset
numOfBag <- 50
numOfInstsInBag <- 3
# set true coefficients: beta_0, beta_1, beta_2, beta_3
trueCoefs <- c(-2, -2, -1, 1, 2, 0.5, 0, 0, 0, 0, 0)
trainData <- DGP(numOfBag, numOfInstsInBag, trueCoefs)
trainData$X %<>% set_colnames(paste0("X", 1:ncol(.)))
tapply(trainData$Z, trainData$ID, function(x) sum(x) > 0) %>% as.numeric
## [36] 1 1 0 0 0 1 1 0 0 0 1 1 0 1 1
## [1] 1 1 1 1 1 1 0 0 1 0 1 1 0 0 1 1 0 1 1 1 1 0 0 0 1 0 1 0 1 0 1 0 1 0 0
# fit milr model
milrFit_EST <- milr(trainData$Z, trainData$X, trainData$ID, lambda = 0)
# call the Wald's test result
summary(milrFit_EST)
## Log-Likelihood: -14.005.
## Estimates:
## Estimate Std.Err Z value Pr(>z)
## intercept -3.28671 1.16695 -2.8165 0.004855 **
## X1 -2.45529 0.92227 -2.6622 0.007762 **
## X2 -1.26351 0.67621 -1.8685 0.061689 .
## X3 0.94016 0.75173 1.2507 0.211054
## X4 3.84173 1.47862 2.5982 0.009372 **
## X5 0.22000 0.66579 0.3304 0.741073
## X6 -1.00740 0.73288 -1.3746 0.169262
## X7 -0.53063 0.59871 -0.8863 0.375463
## X8 0.25334 0.71596 0.3538 0.723451
## X9 -1.92753 0.92437 -2.0852 0.037047 *
## X10 0.12249 0.63054 0.1943 0.845972
## ---
## Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# call the regression coefficients
coef(milrFit_EST)
## intercept X1 X2 X3 X4 X5
## -3.2867082 -2.4552903 -1.2635149 0.9401636 3.8417318 0.2199982
## X6 X7 X8 X9 X10
## -1.0074012 -0.5306309 0.2533409 -1.9275338 0.1224893
fitted(milrFit_EST, type = "bag")
## 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
## 1 1 0 1 1 1 0 1 1 0 1 1 0 0 1 1 0 1 1 1 1 0 0 1 0
## 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
## 0 1 0 1 0 1 1 1 0 0 1 1 0 0 0 1 1 1 0 0 0 1 0 1 1
table(DATA = tapply(trainData$Z, trainData$ID, function(x) sum(x) > 0) %>% as.numeric,
FITTED = fitted(milrFit_EST, type = "bag"))
## FITTED
## DATA 0 1
## 0 18 4
## 1 3 25
# predict for testing data
testData <- DGP(numOfBag, numOfInstsInBag, trueCoefs)
testData$X %<>% set_colnames(paste0("X", 1:ncol(.)))
pred_EST <- predict(milrFit_EST, testData$X, testData$ID, type = "bag")
#predict(milrFit_EST, testData$X, testData$ID,
# type = "instance") # instance-level prediction
table(DATA = tapply(testData$Z, testData$ID, function(x) sum(x) > 0) %>% as.numeric,
PRED = pred_EST)
## PRED
## DATA 0 1
## 0 13 6
## 1 8 23
set.seed(99)
# Set the new coefficient vector (large p)
trueCoefs_Lp <- c(-2, -2, -1, 1, 2, 0.5, rep(0, 95))
# Generate the new training data with large p
trainData_Lp <- DGP(numOfBag, numOfInstsInBag, trueCoefs_Lp)
trainData_Lp$X %<>% set_colnames(paste0("X", 1:ncol(.)))
# variable selection by user-defined tuning set
lambdaSet <- exp(seq(log(0.01), log(50), length = 50))
milrFit_VS <- milr(trainData_Lp$Z, trainData_Lp$X, trainData_Lp$ID,
lambda = lambdaSet)
# grep the active factors and their corresponding coefficients
coef(milrFit_VS) %>% .[abs(.) > 0]
## intercept
## -0.9020893
# variable selection using auto-tuning
milrFit_auto_VS <- milr(trainData_Lp$Z, trainData_Lp$X, trainData_Lp$ID,
lambda = -1, numLambda = 50)
# the auto-selected lambda values
milrFit_auto_VS$lambda
## [1] 0.08041559 0.09259014 0.10660786 0.12274780 0.14133125
## [6] 0.16272815 0.18736444 0.21573056 0.24839117 0.28599645
## [11] 0.32929500 0.37914875 0.43655012 0.50264180 0.57873946
## [16] 0.66635795 0.76724148 0.88339831 1.01714075 1.17113118
## [21] 1.34843505 1.55258192 1.78763568 2.05827549 2.36988893
## [26] 2.72867921 3.14178869 3.61744105 4.16510498 4.79568271
## [31] 5.52172701 6.35769107 7.32021625 8.42846331 9.70449388
## [36] 11.17370961 12.86535784 14.81311383 17.05575111 19.63791336
## [41] 22.61100310 26.03420494 29.97566379 34.51384138 39.73907818
## [46] 45.75539179 52.68254760 60.65844293 69.84185212 80.41558721
# the values of BIC under each lambda value
milrFit_auto_VS$BIC
## [1] 196.54069 184.90606 161.51628 157.75005 118.76827 118.95214 115.27610
## [8] 115.55212 115.87195 112.32892 112.75439 113.24620 113.81466 114.48027
## [15] 119.19304 116.18877 121.15644 114.54700 112.02964 110.31200 103.92031
## [22] 101.90760 88.78977 83.57070 82.69910 82.13240 78.00261 74.25620
## [29] 74.19599 78.98716 77.39963 75.33387 78.16814 69.25384 69.25384
## [36] 69.25384 69.25384 69.25384 69.25384 69.25384 69.25384 69.25384
## [43] 69.25384 69.25384 69.25384 69.25384 69.25384 69.25384 69.25384
## [50] 69.25384
# grep the active factors and their corresponding coefficients
coef(milrFit_auto_VS) %>% .[abs(.) > 0]
## intercept
## -0.9020893
# variable selection using auto-tuning with cross validation
milrFit_auto_CV <- milr(trainData_Lp$Z, trainData_Lp$X, trainData_Lp$ID,
lambda = -1, numLambda = 50,
lambdaCriterion = "deviance", nfold = 10)
# the values of predictive deviance under each lambda value
milrFit_auto_CV$cv
## [1] 10.013948 3.754961 3.132322 2.933881 2.433803 2.346058 2.752407
## [8] 3.248528 3.858600 4.392568 4.781208 5.249175 5.727995 6.030227
## [15] 6.393522 6.432488 6.379543 6.339838 6.317661 6.329531 5.551296
## [22] 5.222904 5.113070 5.006837 5.078377 5.106067 5.242165 5.579102
## [29] 5.786248 6.178347 6.414204 6.648448 6.659413 6.573462 6.547737
## [36] 6.547737 6.547737 6.547737 6.547737 6.547737 6.547737 6.547737
## [43] 6.547737 6.547737 6.547737 6.547737 6.547737 6.547737 6.547737
## [50] 6.547737
# grep the active factors and their corresponding coefficients
coef(milrFit_auto_CV) %>% .[abs(.) > 0]
## intercept X1 X2 X3 X4
## -2.446119887 -0.362833108 -1.479388087 0.541861054 0.535400264
## X7 X11 X14 X15 X17
## 1.448461978 0.334921736 0.004238594 -0.755908930 0.017708059
## X18 X25 X26 X30 X32
## -0.586349577 -0.244962971 0.343205919 1.315468844 -0.845118964
## X33 X37 X48 X58 X61
## 0.370261921 -0.493144745 -0.523001848 -0.044975426 0.208521105
## X62 X71 X72 X74 X76
## 0.409946699 1.369814722 0.484713157 0.683531448 1.542186462
## X77 X79 X85 X95 X100
## -0.656669320 -1.685794976 -0.369189815 -0.912145167 -0.135461219
dataName <- "MIL-Data-2002-Musk-Corel-Trec9.tgz"
dataUrl <- "http://www.cs.columbia.edu/~andrews/mil/data/"
filePath <- file.path(getwd(), dataName)
# Download MIL data sets from the url
download.file(paste0(dataUrl, dataName), filePath)
for (it in 1:length(itSet)) {
# Extract MUSK1 data file
untar(filePath, files = "MilData/Musk/musk1norm.svm")
# Read and Preprocess MUSK1
library(reshape2)
tmp <- read.table(file.path(getwd(), "MilData/Musk/musk1norm.svm"),
sep = " ", colClasses = "character")
MUSK1 <- colsplit(tmp[,1], ":", names = c("obs", "bag", "label"))[,2:3]
MUSK1 <- cbind(MUSK1, Reduce(cbind,
lapply(2:ncol(tmp),
function(i) colsplit(tmp[,i], ":", names = paste0(c("num", "x"), i-1))[,2]
)))
MUSK1$bag <- MUSK1$bag + 1
MUSK1$label <- (MUSK1$label + 1)/2
MUSK1[,3:ncol(MUSK1)] <- scale(MUSK1[,3:ncol(MUSK1)])
Y <- tapply(MUSK1$label, MUSK1$bag, function(x) sum(x) > 0) %>% as.numeric
nc <- ncol(MUSK1)
# set the iterations from 5000 to 25000
itSet <- seq(5000, 25000, 2000)
runtime <- matrix(0, length(itSet), 3)
runacc <- matrix(0, length(itSet), 3)
# record the computation time
runtime[it,1] <- system.time(
softmaxFit_0 <- softmax(MUSK1$label, MUSK1[,3:nc], MUSK1$bag, alpha = 0,
control = list(maxit = itSet[it]))
)[3]
runtime[it,2] <- system.time(
softmaxFit_3 <- softmax(MUSK1$label, MUSK1[,3:nc], MUSK1$bag, alpha = 3,
control = list(maxit = itSet[it]))
)[3]
runtime[it,3] <- system.time(
# use a very small lambda so that milr can do the estimation
# without evaluating the Hessian matrix
milrFit <- milr(MUSK1$label, MUSK1[,3:nc], MUSK1$bag, lambda = 1e-7,
maxit = itSet[it])
)[3]
# calculate the accuracy
tmp <- table(DATA = Y, FIT_s0 = fitted(softmaxFit_0, type = "bag"))
runacc[it,1] <- sum(diag(tmp))/sum(tmp)
tmp <- table(DATA = Y, FIT_s3 = fitted(softmaxFit_3, type = "bag"))
runacc[it,2] <- sum(diag(tmp))/sum(tmp)
tmp <- table(DATA = Y, FIT_MILR = fitted(milrFit, type = "bag"))
runacc[it,3] <- sum(diag(tmp))/sum(tmp)
}
# MILR-LASSO
milrSV <- milr(MUSK1$label, MUSK1[,3:nc], MUSK1$bag,
lambda = -1, numLambda = 100,
lambdaCriterion = "deviance", maxit = 16000)
sv_ind <- which(coef(milrSV)[-1] != 0) + 2
# show the detected active covariates
names(MUSK1)[sv_ind]
## [1] "V31" "V36" "V37" "V76" "V83" "V105" "V106" "V108" "V109" "V116"
## [11] "V118" "V124" "V126" "V129" "V132" "V136" "V147" "V162" "V163"
# use a very small lambda so that milr can do the estimation
# without evaluating the Hessian matrix
milrREFit <- milr(MUSK1$label, MUSK1[,sv_ind], MUSK1$bag,
lambda = 1e-7, maxit = 16000)
table(DATA = Y, FIT_MILR = fitted(milrREFit, type = "bag"))
## FIT_MILR
## DATA 0 1
## 0 39 6
## 1 4 43
predY <- matrix(0, length(Y), 4); colnames(predY) <- c("s0", "s3", "milr", "milr_sv")
set.seed(99)
folds <- 10; foldSize <- floor(length(Y)/folds)
foldBag <- c(rep(1:folds, foldSize), sample(1:folds, length(Y) - folds*foldSize))
foldBag <- sample(foldBag, length(foldBag))
foldIns <- rep(foldBag, table(MUSK1$bag))
for (i in 1:folds) {
# prepare training and testing sets
ind <- which(foldIns == i)
training <- MUSK1[-ind,]; testing <- MUSK1[ind,]
# train models
fit_s0 <- softmax(training$label, training[,3:nc], training$bag,
alpha = 0, control = list(maxit = 25000))
fit_s3 <- softmax(training$label, training[,3:nc], training$bag,
alpha = 3, control = list(maxit = 25000))
# milr, use a very small lambda so that milr do the estimation
# without evaluating the Hessian matrix
fit_milr <- milr(training$label, training[,3:nc], training$bag,
lambda = 1e-7, maxit = 16000)
fit_milr_sv <- milr(training$label, training[,sv_ind], training$bag,
lambda = 1e-7, maxit = 16000)
# store the predicted labels
ind2 <- which(foldBag == i)
# predict function returns bag response in default
predY[ind2,1] <- predict(fit_s0, as.matrix(testing[,3:nc]), testing$bag)
predY[ind2,2] <- predict(fit_s3, as.matrix(testing[,3:nc]), testing$bag)
predY[ind2,3] <- predict(fit_milr, as.matrix(testing[,3:nc]), testing$bag)
predY[ind2,4] <- predict(fit_milr_sv, as.matrix(testing[,sv_ind]), testing$bag)
}
table(DATA = Y, PRED_s0 = predY[,1])
## PRED_s0
## DATA 0 1
## 0 36 9
## 1 6 41
table(DATA = Y, PRED_s3 = predY[,2])
## PRED_s3
## DATA 0 1
## 0 28 17
## 1 4 43
table(DATA = Y, PRED_MILR = predY[,3])
## PRED_MILR
## DATA 0 1
## 0 32 13
## 1 10 37
table(DATA = Y, PRED_MILR_SV = predY[,4])
## PRED_MILR_SV
## DATA 0 1
## 0 35 10
## 1 7 40
CPU time (sec)
5
10
15</div>
s_0
s_3
milr</div>
Accuracy (%)
90
92.5
95
97.5
100</div>
∏
∑
∏
∑
∏
∏
∑
∑
∑
∑
∑
∑








