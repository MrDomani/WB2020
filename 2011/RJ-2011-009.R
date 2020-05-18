data(srft)
members <- c("CMCG", "ETA", "GASP", "GFS","JMA", "NGPS", "TCWB", "UKMO")
srftData <-ensembleData(forecasts = srft[,members],dates = srft$date,observations = srft$obs,latitude = srft$lat,longitude = srft$lon,forecastHour = 48)
srftFit <-ensembleBMA(srftData, dates = "2004013100",model = "normal", trainingDays = 25)
plot(srftFit, srftData, dates = "2004013100")
srftGridForc <- quantileForecast(srftFit,
srftGridData, quantiles = c( .1, .5, .9))
probFreeze <- cdf(srftFit, srftGridData,
date = "2004013100", value = 273.15)
data(prcpFit)
prcpGridForc <- quantileForecast(
prcpFit, prcpGridData, date = "20030115",
q = c(0.5, 0.9))
probPrecip <- 1 - cdf(prcpFit, prcpGridData,
date = "20030115", values = c(0, 25))
srftForc <- quantileForecast(srftFit,
srftData, quantiles = c( .1, .5, .9))
CRPS(srftFit, srftData)
# ensemble BMA
# 1.945544 1.490496
MAE(srftFit, srftData)
# ensemble BMA
# 2.164045 2.042603
use <- ensembleValidDates(srftData) >=
"2004013000"
srftPIT <- pit(srftFitALL, srftData)
hist(srftPIT, breaks = (0:(k+1))/(k+1),
main = "Probability Integral Transform")
axis(1, at = seq(0, to = 1, by = .2),
srftFitALL <- ensembleBMA(srftData,
trainingDays = 25)
abline(h=1/(ensembleSize(srftData)+1), lty=2)
xlab="", xaxt="n", prob = TRUE,
0.00 0.04 0.08 0.12
Probability Density</div>
0.0 0.1 0.2 0.3 0.4 0.5
Prob No Precip and Scaled PDF for Precip</div>
