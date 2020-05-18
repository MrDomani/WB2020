library(nmfgpu4R)
nmfgpu4R.init()
result <- nmf(data, r, threshold=0.1, thresholdType="rmsd", maxiter=500)
# Set seed for reproducible results
set.seed(42)
# Split iris dataset into training and test data
idx <- sample(1:nrow(iris), 100, replace=F)
data.train <- iris[idx,-5]
data.test <- iris[-idx,-5]
# Compute model and retrieve encoding matrix H for both training and test data
library(nmfgpu4R)
nmfgpu4R.init()
model <- nmf(t(data.train), 2)
encoding.train <- t(predict(model)) # Identical: encoding.train <- t(model$H)
encoding.test <- t(predict(model, t(data.test)))
# Use encoding matrices to predict "Species"
library(e1071)
model.svm <- best.svm(x=encoding.train, y=iris$Species[idx])
prediction <- predict(model.svm, encoding.test)
table(iris[-idx,5], prediction)
# Plot encoding matrices
library(ggplot2)
data.plot <- data.frame(rbind(encoding.train, encoding.test),
class=unlist(list(iris[idx,5], iris[-idx,5])),
type=c(rep("Train", nrow(data.train)),
rep("Test", nrow(data.test))))
ggplot(data.plot, aes(x=r1, y=r2, color=class, shape=type)) + geom_point()
||
||
||
||
