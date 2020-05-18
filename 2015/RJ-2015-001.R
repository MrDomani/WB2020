install.packages("Peptides")
library(Peptides)
lengthpep(seq = "GLPRKILCAIAKKKGKCKGPLKLVCKC")
# [1] 27
mw(seq = "GLPRKILCAIAKKKGKCKGPLKLVCKC")
# [1] 2897.787
aacomp(seq = "GLPRKILCAIAKKKGKCKGPLKLVCKC")
# Number Mole%
# Tiny 9 33.333
# Small 12 44.444
# Aliphatic 9 33.333
# Aromatic 0 0.000
# NonPolar 18 66.667
# Polar 9 33.333
# Charged 9 33.333
# Basic 9 33.333
# Acidic 0 0.000
charge(seq = "GLPRKILCAIAKKKGKCKGPLKLVCKC", pH = 7, pKscale = "EMBOSS")
# [1] 8.85201
pI(seq = "GLPRKILCAIAKKKGKCKGPLKLVCKC", pKscale = "EMBOSS")
# [1] 10.801
aindex(seq = "GLPRKILCAIAKKKGKCKGPLKLVCKC")
# [1] 104.8148
instaindex(seq = "GLPRKILCAIAKKKGKCKGPLKLVCKC")
# [1] 2.237037
boman(seq = "GLPRKILCAIAKKKGKCKGPLKLVCKC")
# [1] 0.5259259
hydrophobicity(seq = "GLPRKILCAIAKKKGKCKGPLKLVCKC", scale = "Eisenberg")
# [1] -0.08777778
hmoment(seq = "GLPRKILCAIAKKKGKCKGPLKLVCKC", angle = 100, window = 11)
# [1] 0.6170697
hmoment(seq = "GLPRKILCAIAKKKGKCKGPLKLVCKC", angle = 160, window = 11)
# [1] 0.4617153
membpos(seq = "GLPRKILCAIAKKKGKCKGPLKLVCKC", angle = 100)
# Pep H uH MembPos
# 1 GLPRKILCAIA 0.271 0.469 Surface
# 2 LPRKILCAIAK 0.091 0.617 Surface
# 3 PRKILCAIAKK -0.142 0.520 Globular
# 4 RKILCAIAKKK -0.289 0.401 Globular
# 5 KILCAIAKKKG -0.015 0.325 Globular
# 6 ILCAIAKKKGK -0.015 0.319 Globular
# 7 LCAIAKKKGKC -0.115 0.339 Globular
# 8 CAIAKKKGKCK -0.347 0.115 Globular
# 9 AIAKKKGKCKG -0.330 0.096 Globular
# 10 IAKKKGKCKGP -0.375 0.141 Globular
# 11 AKKKGKCKGPL -0.405 0.161 Globular
# 12 KKKGKCKGPLK -0.597 0.110 Globular
# 13 KKGKCKGPLKL -0.365 0.156 Globular
# 14 KGKCKGPLKLV -0.130 0.310 Globular
# 15 GKCKGPLKLVC 0.033 0.257 Globular
# 16 KCKGPLKLVCK -0.147 0.426 Globular
# 17 CKGPLKLVCKC 0.015 0.487 Globular
file <- system.file(file = file.path("xvg-files", "POPG.xvg"),
package = "Peptides")
md <- read.xvg(file)
head(md)
# Time (ps) |d| d\\sx\\N d\\sy\\N d\\sz\\N
# 1 0 3.476546 -0.2250402 -0.3378360 -3.452766
# 2 3 3.447776 -0.2403412 -0.4272313 -3.412751
# 3 6 3.459584 -0.2213712 -0.3733103 -3.432252
# 4 9 3.391920 -0.2328529 -0.3787930 -3.362650
# 5 12 3.403695 -0.1856968 -0.2425249 -3.389962
file <- system.file(file.path("xvg-files", "POPG.xvg"), package = "Peptides")
plot.xvg(file)
install.packages("caret", dependencies = TRUE)
library(Peptides)
library(caret)
data(pepdata)
str(pepdata)
# $ sequence : chr "DAEFRHDSGYEVHHQKLVFFAEDVGSNK" "SLDRSSCFTGSLDSIRAQSGLGCNSFRY" ...
# $ group : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
# $ length : int 28 28 30 22 25 28 30 25 31 26 ...
# $ mw : num 3263 3028 3275 2747 2903 ...
# $ tinyAA : num 21.4 50 40 13.6 24 ...
# $ smallAA : num 46.4 60.7 56.7 27.3 44 ...
# $ aliphaticAA : num 21.4 17.9 16.7 36.4 28 ...
# $ aromaticAA : num 25 10.71 3.33 18.18 12 ...
# $ nonpolarAA : num 42.9 46.4 46.7 54.5 56 ...
# $ polarAA : num 57.1 53.6 53.3 45.5 44 ...
# $ chargedAA : num 42.9 17.9 26.7 27.3 28 ...
# $ basicAA : num 21.4 10.7 20 22.7 16 ...
# $ acidicAA : num 21.43 7.14 6.67 4.54 12 ...
# $ charge : num -2.97 0.742 3.741 1.936 0.762 ...
# $ pI : num 5.43 8.22 10.05 11.05 9.03 ...
# $ aindex : num 52.1 59.3 48.7 119.5 89.6 ...
# $ instaindex : num 26.9 58 45.5 52.8 45.1 ...
# $ boman : num 2.65 2.43 2.22 1.77 1.84 ...
# $ hydrophobicity: num -0.9 -0.286 -0.62 -0.145 -0.372 -0.004 -0.23 0.536 0.477 0.754 ...
# $ hmoment : num 0.392 0.407 0.46 0.533 0.373 0.603 0.483 0.643 0.264 0.38 ...
# $ transmembrane : num 0 0 0 0 0 0 0 0.067 0.095 0.188 ...
# $ globular : num 1 1 1 1 1 1 0.8 0.733 0.905 0.688 ...
# $ surface : num 0 0 0 0 0 0 0.2 0.2 0 0.125 ...
set.seed(2014)
inTrain <- createDataPartition(y = pepdata$group, p = 0.8, list = FALSE)
training <- pepdata[ inTrain, 2:23]
testing <- pepdata[-inTrain, 2:23]
folds <- 10
repeats <- 10
fitControl <- trainControl(method = "repeatedcv",
number = folds,
repeats = repeats,
classProbs = TRUE,
allowParallel = TRUE,
summaryFunction = twoClassSummary)
train.rpart <- train(group~., data = training,
method = "rpart",
metric = "ROC",
tuneLength = 10,
trControl = fitControl)
train.rpart
# CART
# 80 samples
# 21 predictors
# 2 classes: '0', '1'
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 10 times)
# Summary of sample sizes: 72, 72, 72, 72, 72, 72, ...
# Resampling results across tuning parameters:
# cp ROC Sens Spec ROC SD Sens SD Spec SD
# 0.00000000 0.8321875 0.8075 0.7525 0.1327056 0.1875379 0.2117001
# 0.07222222 0.7862500 0.7525 0.7700 0.1664132 0.2056494 0.2294041
# 0.14444444 0.7462500 0.6450 0.8850 0.1847590 0.2414707 0.2023224
# 0.21666667 0.7475000 0.6450 0.9000 0.1854731 0.2414707 0.1946247
# 0.28888889 0.7475000 0.6450 0.9000 0.1854731 0.2414707 0.1946247
# 0.36111111 0.7475000 0.6450 0.9000 0.1854731 0.2414707 0.1946247
# 0.43333333 0.7475000 0.6450 0.9000 0.1854731 0.2414707 0.1946247
# 0.50555556 0.7475000 0.6450 0.9000 0.1854731 0.2414707 0.1946247
# 0.57777778 0.7475000 0.6450 0.9000 0.1854731 0.2414707 0.1946247
# 0.65000000 0.5550000 0.7650 0.3800 0.1128152 0.2926283 0.4570989
# ROC was used to select the optimal model using the largest value.
# The final value used for the model was cp = 0.
plot(train.rpart)
pred.rpart <- predict(train.rpart, newdata = testing)
pred.rpart
# [1] 1 0 0 0 0 0 0 0 0 0 0 1 1 0 1 1 1 1 1 1
# Levels: 0 1
confusionMatrix(data = pred.rpart, testing$group, positive = "1")
# Confusion Matrix and Statistics
# Reference
# Prediction 0 1
# 0 9 2
# 1 1 8
# Accuracy : 0.85
# 95% CI : (0.6211, 0.9679)
# No Information Rate : 0.5
# P-Value [Acc > NIR] : 0.001288
# Kappa : 0.7
# Mcnemar's Test P-Value : 1.000000
# Sensitivity : 0.8000
# Specificity : 0.9000
# Pos Pred Value : 0.8889
# Neg Pred Value : 0.8182
# Prevalence : 0.5000
# Detection Rate : 0.4000
# Detection Prevalence : 0.4500
# Balanced Accuracy : 0.8500
tree <- train.rpart$finalModel
plot(tree, compress = TRUE, margin = c(0.1, 0.1, 0.1, 0.1), cex = 0.5)
text(tree)
train.lda <- train(group~., data = training,
method = "lda",
metric = "ROC",
tuneLength = 10,
trControl = fitControl)
train.lda
# Linear Discriminant Analysis
# 80 samples
# 21 predictors
# 2 classes: '0', '1'
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 10 times)
# Summary of sample sizes: 72, 72, 72, 72, 72, 72, ...
# Resampling results
# ROC Sens Spec ROC SD Sens SD Spec SD
# 0.821875 0.78 0.8 0.1534471 0.2169578 0.1978419
pred.lda <- predict(train.lda, newdata = testing)
pred.lda
# [1] 0 0 0 0 0 0 0 1 0 0 1 1 1 1 1 1 1 1 1 1
# Levels: 0 1
confusionMatrix(data = pred.lda, testing$group,positive = "1")
# Confusion Matrix and Statistics
# Reference
# Prediction 0 1
# 0 9 0
# 1 1 10
# Accuracy : 0.95
# 95% CI : (0.7513, 0.9987)
# No Information Rate : 0.5
# P-Value [Acc > NIR] : 2.003e-05
# Kappa : 0.9
# Mcnemar's Test P-Value : 1
# Sensitivity : 1.0000
# Specificity : 0.9000
# Pos Pred Value : 0.9091
# Neg Pred Value : 1.0000
# Prevalence : 0.5000
# Detection Rate : 0.5000
# Detection Prevalence : 0.5500
# Balanced Accuracy : 0.9500
# Complexity Parameter
# AUC (Repeated Cross−Validation
# 0.55
# 0.60
# 0.65
# 0.70
# 0.75
# 0.80
# 0.0 0.2 0.4 0.6</div>
# 0 10000 20000 30000 40000 50000
# 2.0 2.5 3.0 3.5
# Time (ps)
# Distance (nm)</div>
# |d|</div>
# ∑
# ∑
# ∑
# 'data.frame': 100 obs. of 23 variables:
# 'Positive' Class : 1
# 'Positive' Class : 1
