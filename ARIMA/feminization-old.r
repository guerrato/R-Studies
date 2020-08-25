data = read.csv('/Users/guerrato/Projects/studies/R/seriesClose.csv', header = TRUE, sep = '	', stringsAsFactors = FALSE)

library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)

getSymbols("SPY", from = '2019-07-01', to = '2020-04-20')
class(SPY)

SPY_Close_Prices = SPY[, 1]
plot(SPY_Close_Prices)
class(SPY)

Acf(SPY_Close_Prices, main = 'ACF for diff Series')
Pacf(SPY_Close_Prices, main = 'PACF for diff Series')
auto.arima(SPY_Close_Prices, seasonal = FALSE)

logs = diff(ln(SPY_Close_Prices), lag = 1)
logs = logs[!is.na(logs)]

plot(logs.type = '1', main = 'logs')
print(adf.test(logs))

auto.arima(logs, seasonal = FALSE)

str(logs)