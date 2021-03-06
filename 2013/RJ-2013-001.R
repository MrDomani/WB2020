
data(USCongress)
# CREATE THE DOCUMENT-TERM MATRIX
doc_matrix <- create_matrix(USCongress$text, language="english", removeNumbers=TRUE,
stemWords=TRUE, removeSparseTerms=.998)
container <- create_container(doc_matrix, USCongress$major, trainSize=1:4000,
testSize=4001:4449, virgin=FALSE)
SVM <- train_model(container,"SVM")
GLMNET <- train_model(container,"GLMNET")
MAXENT <- train_model(container,"MAXENT")
SLDA <- train_model(container,"SLDA")
BOOSTING <- train_model(container,"BOOSTING")
BAGGING <- train_model(container,"BAGGING")
RF <- train_model(container,"RF")
NNET <- train_model(container,"NNET")
TREE <- train_model(container,"TREE")
SVM_CLASSIFY <- classify_model(container, SVM)
GLMNET_CLASSIFY <- classify_model(container, GLMNET)
MAXENT_CLASSIFY <- classify_model(container, MAXENT)
SLDA_CLASSIFY <- classify_model(container, SLDA)
BOOSTING_CLASSIFY <- classify_model(container, BOOSTING)
BAGGING_CLASSIFY <- classify_model(container, BAGGING)
RF_CLASSIFY <- classify_model(container, RF)
NNET_CLASSIFY <- classify_model(container, NNET)
TREE_CLASSIFY <- classify_model(container, TREE)
analytics <- create_analytics(container,
cbind(SVM_CLASSIFY, SLDA_CLASSIFY,
BOOSTING_CLASSIFY, BAGGING_CLASSIFY,
RF_CLASSIFY, GLMNET_CLASSIFY,
NNET_CLASSIFY, TREE_CLASSIFY,
MAXENT_CLASSIFY))
summary(analytics)
# CREATE THE data.frame SUMMARIES
topic_summary <- analytics@label_summary
alg_summary <- analytics@algorithm_summary
ens_summary <-analytics@ensemble_summary
doc_summary <- analytics@document_summary
create_ensembleSummary(analytics@document_summary)
SVM <- cross_validate(container, 4, "SVM")
GLMNET <- cross_validate(container, 4, "GLMNET")
MAXENT <- cross_validate(container, 4, "MAXENT")
SLDA <- cross_validate(container, 4, "SLDA")
BAGGING <- cross_validate(container, 4, "BAGGING")
BOOSTING <- cross_validate(container, 4, "BOOSTING")
RF <- cross_validate(container, 4, "RF")
NNET <- cross_validate(container, 4, "NNET")
TREE <- cross_validate(container, 4, "TREE")
write.csv(analytics@document_summary, "DocumentSummary.csv")
