data = read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = FALSE)
attach(data)

library(MASS)
library(tseries)
library(forecast)

dataVals = data[, 2]
logTrainingSet = log(dataVals[1:241])

acf(logTrainingSet, lag.max = 12)
pacf(logTrainingSet, lag.max = 12)
diffTrainingSet = diff(logTrainingSet, 1)
diffTrainingSet = diffTrainingSet[!is.na(diffTrainingSet)]
diffTrainingSet

adf.test(logTrainingSet)
adf.test(diffTrainingSet)

tsTrainingSet = ts(logTrainingSet, start = 1, end = numeric(), frequency = 1)
tsTrainingSet
fitTrainingSet = auto.arima(tsTrainingSet)

exp(logTrainingSet)

forecastedLog = forecast(fitTrainingSet, h = 60)
plot(forecastedLog, type = "l", main = "Forecast")

forecastedExtracted = as.numeric(forecastedLog$mean)
finalForecasted = exp(forecastedExtracted)

validationData = data.frame(dataVals[242:301], finalForecasted)
colHeads = c("CurrentData", "ForecastedData")
names(validationData) = colHeads
attach(validationData)
percError = ((validationData$`CurrentData` - validationData$`ForecastedData`) / validationData$`CurrentData`)
percError
mean(percError)

Box.test(fitTrainingSet$resid, lag = 6, type = "Ljung-Box")
Box.test(fitTrainingSet$resid, lag = 12, type = "Ljung-Box")
Box.test(fitTrainingSet$resid, lag = 18, type = "Ljung-Box")
