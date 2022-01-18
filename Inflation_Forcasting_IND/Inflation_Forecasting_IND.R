
install.packages("tidyverse")
install.packages("urca")
install.packages("forecast")
install.packages("tseries")
install.packages("TSstudio")
install.packages("ggfortify")
install.packages("Rcpp")

library(tidyverse)
library(urca)
library(forecast)
library(tseries)
library(TSstudio)
library(ggfortify)
library(Rcpp)
library(ggplot2)

# Loading the Dataset

library(readxl)
InflationDataApr2012Dec2021 <- read_excel("InflationDataApr2012Dec2021.xlsx")
View(InflationDataApr2012Dec2021)

head(InflationDataApr2012Dec2021)
nrow(InflationDataApr2012Dec2021)

# Declaring a Time Series Object

inf <- ts(InflationDataApr2012Dec2021$WPI, start = c(2012,04,01), frequency = 12)

# Graphing the Time Series

autoplot(inf) + ggtitle("Inflation Rate (India), April 2012 to December 2021") +labs(x = "Time", y = "Inflation Rate")

# Forecasting Building Blocks

ggAcf(inf) + ggtitle("ACF of Inflation")
ggPacf(inf) + ggtitle("PACF of Inflation")

# Differencing the Series

dinf <- diff(inf)
ggAcf(dinf) + ggtitle("ACF Of Inflation (Differenced)")
ggPacf(dinf) + ggtitle("PACF of Inflation (Differenced)")

autoplot(dinf) + ggtitle("Inflation Rate (India), April 2012 to December 2021") +labs(x = "Time", y = "Inflation Rate (Differenced)")

# Decomposition of the Time Series

ts_decompose(inf, type = "additive", showline = TRUE)

# Tests for Non-stationarity

## Augmented Dickey-Fuller Test

adf.test(inf)
adf.test(inf, k = 1)
adf.test(inf, k = 2)
adf.test(dinf)

## Phillips-Perron Test

pp.test(inf)
pp.test(dinf)

## Kwiatkowski-Phillips-Schmidt-Shin Test

kpss.test(inf)
kpss.test(dinf)

# Forecasting Proper
# Splitting the Dataset into Training and Testing Sets

split_inf <- ts_split(inf, sample.out = 8)

training <- split_inf$train
testing <- split_inf$test

length(training)
length(testing)

# Diagnosing the Training Set

arima_diag(training)

# Building the Forecast Model

## Model 1

arima211 <- arima(training, order = c(2,1,1))
autoplot(arima221)
check_res(arima221)

## Model 2

sarima2111 <- arima(training, order = c(2,1,1), seasonal = list(order = c(1,0,0)))
autoplot(sarima2111)
check_res(sarima2111)

## Model 3

auto <- auto.arima(training, seasonal = TRUE)
auto
autoplot(auto)
check_res(auto)

# Generating the Forecasts and Forecast Evaluation

## For Model 1

fcast1 <- forecast(arima211, h = 8)
test_forecast(actual = inf, forecast.obj = fcast1, test = testing)
accuracy(fcast1, testing)

## For Model 2

fcast2 <- forecast(sarima2111, h = 8)
test_forecast(actual = inf, forecast.obj = fcast2, test = testing)
accuracy(fcast2,testing)

## For Model 3

fcasta <- forecast(auto, h = 8)
test_forecast(actual = inf, forecast.obj = fcasta, test = testing)
accuracy(fcasta,testing)

# Out of Sample Forecasting

# Generating the Optimal Fit

finalfit <- auto.arima(inf, seasonal = TRUE)
autoplot(finalfit)
check_res(finalfit)

# Generating the Out-of-Sample Forecast

fcastf <- forecast(inf, model = finalfit, h = 4)
plot_forecast(fcastf)
summary(fcastf)














