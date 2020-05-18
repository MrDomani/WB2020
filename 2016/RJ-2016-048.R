install.packages("SDEFSR")
devtools::install_github("SIMIDAT/SDEFSR")
library(SDEFSR)
irisFromKEEL <- read.dataset("iris.dat")
irisFromARFF <- read.dataset("iris.arff")
irisFromCSV <- read.dataset("iris.csv")
# = "
# ¨
df <- data.frame(matrix(data = runif(1000), ncol = 10))
# #Add class attribute (It must be the last attribute and it must be categorical)
df[,11] <- c("Class 0", "Class 1", "Class 2", "Class 3")
SDEFSR_DatasetObject <- SDEFSR_DatasetFromDataFrame(df, relation = "random")
summary(irisFromKEEL)
# Summary of the SDEFSR_Dataset object: 'irisFromKEEL'
# - relation: iris
# - nVars: 4
# - Ns: 150
# - attributeNames: SepalLength, SepalWidth, PetalLength, PetalWidth, Class
# - class_names: Iris-setosa, Iris-versicolor, Iris-virginica
# - examplesPerClass: 50, 50, 50
irisFromKEEL$nVars
# [1] 4
irisFromKEEL$attributeNames
# [1] "SepalLength" "SepalWidth" "PetalLength" "PetalWidth" "Class"
ruleSet <- MESDIF(paramFile = "param.txt")
# #or
ruleSet <- MESDIF(training = irisFromKEEL)
ruleSet <- MESDIF(paramFile = NULL, training = irisFromKEEL, test = NULL,
output = c("optionsFile.txt", "rulesFile.txt", "testQM.txt"),
seed = 0, nLabels = 3, nEval = 300, popLength = 100,
eliteLength = 2, crossProb = 0.6, mutProb = 0.01,
RulesRep = "can", Obj1 = "CSUP", Obj2 = "CCNF", Obj3 = "null",
Obj4 = "null", targetVariable = "Class",
targetClass = "Iris-virginica")
# --------------------------------
# Algorithm: MESDIF
# Relation: iris
# Training dataset: training
# Test dataset: test
# Rules Representation: CAN
# Number of evaluations: 300
# Number of fuzzy partitions: 3
# Population Length: 100
# Elite Population Length: 2
# Crossover Probability: 0.6
# Mutation Probability: 0.01
# Obj1: CSUP (Weigth: )
# Obj2: CCNF (Weigth: )
# Obj3: null (Weigth: )
# Obj4: null
# Number of examples in training: 150
# Number of examples in test: 150
# Target Variable: Class
# Target Class: Iris-virginica
# --------------------------------
# Searching rules for only one value of the target class...
# GENERATED RULE 1
# Variable SepalWidth = Label 1 ( 2 , 3.2 , 4.4 )
# THEN Iris-virginica
# GENERATED RULE 2
# Variable PetalWidth = Label 2 ( 1.3 , 2.5 , 3.7 )
# THEN Iris-virginica
# Testing rules...
# Rule 1 :
# - N_vars: 2
# - Coverage: 0.8
# - Significance: 0.602743
# - Unusualness: 0.02
# - Accuracy: 0.357724
# - CSupport: 0.286667
# - FSupport: 0.245
# - CConfidence: 0.358333
# - FConfidence: 0.351955
# - True Positive Rate: 0.86
# - False Positive Rate: 0.77
# Rule 2 :
# - N_vars: 2
# - Coverage: 0.193333
# - Significance: 27.673033
# - Unusualness: 0.128889
# - Accuracy: 0.9375
# - CSupport: 0.193333
# - FSupport: 0.201667
# - CConfidence: 1
# - FConfidence: 0.889706
# - True Positive Rate: 0.58
# - False Positive Rate: 0
# Global:
# - N_rules: 2
# - N_vars: 2
# - Coverage: 0.496666
# - Significance: 14.137888
# - Unusualness: 0.074444
# - Accuracy: 0.647612
# - CSupport: 0.3
# - FSupport: 0.223334
# - FConfidence: 0.620831
# - CConfidence: 0.679166
# - True Positive Rate: 0.72
# - False Positive Rate: 0.385
# rulesOrderedBySignificance <- sort(x = ruleSet, decreasing = TRUE, by = "Significance")
# Rule 1
# Rule 2
# 1 0.5 0 0.5 1
# value
# Rule
# qualityMeasure
# FPR
# TPR</div>
# the example.
filteredRules <- ruleSet[Unusualness > 0.05]
length(filteredRules)
# [1] 1
filteredRules <- ruleSet[Unusualness > 0.05 & TPr > 0.9 & nVars == 3]
length(filteredRules)
# [1] 0
SDEFSR_GUI()
SDEFSR_GUI()
# Package 'shiny' is not installed and must be installed to run this GUI.
# Do you want to install it? (Y/n): Y
