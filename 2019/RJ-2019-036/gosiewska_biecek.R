# Illustrations

library("auditor")
library("DALEX")
library("randomForest")

lm_model <- lm(m2.price ~ ., data = apartments)
set.seed(59)
rf_model <- randomForest(m2.price ~ ., data = apartments)


lm_audit <- audit(lm_model, label = "lm",
                  data = apartmentsTest, y = apartmentsTest$m2.price)
rf_audit <- audit(rf_model, label = "rf",
                  data = apartmentsTest, y = apartmentsTest$m2.price)


lm_res_fitted <- modelResiduals(lm_audit, variable = "Fitted values")
rf_res_fitted <- modelResiduals(rf_audit, variable = "Fitted values")

lm_res_observed <- modelResiduals(lm_audit, variable = "m2.price")
rf_res_observed <- modelResiduals(rf_audit, variable = "m2.price")



## Model Raning Plot

rf_mp <- modelPerformance(rf_audit)
lm_mp <- modelPerformance(lm_audit)
plot(rf_mp, lm_mp, type = "ModelRanking")



## REC CUrve Plot

plot(rf_res_fitted, lm_res_fitted, type = "REC")


scoreREC(lm_res_fitted)
scoreREC(rf_res_fitted)



## Residual Boxplot Plot

plot(lm_res_fitted, rf_res_fitted, type = "ResidualBoxplot")



## Residual Density Plot

plot(rf_res_observed, lm_res_observed, type = "ResidualDensity")


