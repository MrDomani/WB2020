
# Create a short example time series with missing values
x <- ts(c(1, 2, 3, 4, 5, 6, 7, 8, NA, NA, 11, 12))
# Impute the missing values with na.mean
na.mean(x)
# Impute the missing values with na.mean using option median
na.mean(x, option="median")
# Impute the missing values with na.interpolation
na.interpolation(x)
# Impute the missing values with na.kalman
# (tsAirgap is an example time series provided by the imputeTS package)
imp <- na.kalman(tsAirgap)
#Code for visualization
plotNA.imputations(tsAirgap, x.imp, tsAirgapComplete)
# Example Code 'plotNA.distribution'
# (tsAirgap is an example time series provided by the imputeTS package)
# Visualize the missing values in this time series
plotNA.distribution(tsAirgap)
# Example Code 'plotNA.distributionBar'
# (tsHeating is an example time series provided by the imputeTS package)
# Visualize the missing values in this time series
plotNA.distributionBar(tsHeating, breaks = 20)
# Example Code 'plotNA.gapsize'
# (tsNH4 is an example time series provided by the imputeTS package)
# Visualize the top gap sizes / NAs in a row
plotNA.gapsize(tsNH4)
# Example Code 'plotNA.imputations'
# (tsAirgap is an example time series provided by the imputeTS package)
# Step 1: Perform imputation for x using na.mean
tsAirgap.imp <- na.mean(tsAirgap)
# Step 2: Visualize the imputed values in the time series
plotNA.imputations(tsAirgap, tsAirgap.imp)
# Example Code 'statsNA'
# (tsNH4 is an example time series provided by the imputeTS package)
# Print stats about the missing data
statsNA(tsNH4)
# Example Code to use tsAirgap dataset
library("imputeTS")
tsAirgap
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
● ● ●
Value</div>
1950 1952 1954 1956 1958 1960
100 300 500</div>
imputed values real values known values</div>
Value
1950 1952 1954 1956 1958 1960
100 300 500</div>
Visualization Imputed Values
Distribution of NAs
