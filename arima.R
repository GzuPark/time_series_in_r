# --------------------------------------------------
# Time Series Analysis - ARIMA
# used to AirPassengers dataset
# 
# R version: 4.0.2
# Date: Nov 14, 2020
# Author: Jongmin Park (jijupax@gmail.com)
# --------------------------------------------------


# --------------------------------------------------
# Prepare
# --------------------------------------------------

# clear variables
rm(list=ls())

# required packages
if (!require("forecast")) install.packages("forecast")  # v8.13
source("eda.R")

# load AirPassengers data
# reference: AirPassengers
# https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/AirPassengers
data(AirPassengers)
class(AirPassengers)



# --------------------------------------------------
# EDA
# --------------------------------------------------

description(AirPassengers)

# plot raw data
airpassenger_plot(AirPassengers, title="Air Passengers per month from 1949 to 1960")

# plot decomposed data
ts_data <- stats::ts(AirPassengers, frequency=12)
# reference: decompose
# https://www.rdocumentation.org/packages/stats/versions/3.2.1/topics/decompose
decomposed_data <- decompose(ts_data, "multiplicative")
plot(decomposed_data)

# plot each component of decomposed data
airpassenger_plot(decomposed_data$trend, 
                title="trend",
                xrange=c(1:12),  # change x-axis interval, 2 -> 1
                yrange=seq(0, 500, by=50))
airpassenger_plot(decomposed_data$trend, 
                title="seasonal",
                xrange=c(1:12),  # change x-axis interval, 2 -> 1
                yrange=seq(0.5, 1.5, by=0.1))
airpassenger_plot(decomposed_data$trend, 
                title="random",
                xrange=c(1:12),  # change x-axis interval, 2 -> 1
                yrange=seq(0.5, 1.5, by=0.1))

# plot linear regression
airpassenger_plot(AirPassengers, title="Air Passengers per month from 1949 to 1960")
# reference: lm
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/lm
abline(reg=lm(AirPassengers~time(AirPassengers)))

# plot based on each month
boxplot(AirPassengers~cycle(AirPassengers),
        xlab="date",
        ylab="# of passengers",
        main="Monthly Air Passengers from 1949 to 1960")



# --------------------------------------------------
# Predict
# --------------------------------------------------

# ARIMA
# reference: arima
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/arima
model <- stats::arima(AirPassengers,  order=c(1, 1, 1)); model
model <- stats::arima(AirPassengers,  order=c(1, 1, 1), seasonal=c(1, 1, 1)); model
model <- stats::arima(AirPassengers,  order=c(2, 1, 1), seasonal=c(1, 1, 1)); model

# reference: auto.arima
# https://www.rdocumentation.org/packages/forecast/versions/8.13/topics/auto.arima
model <- forecast::auto.arima(AirPassengers, ic="aic", trace=TRUE)
model <- forecast::auto.arima(AirPassengers, ic="aic", stepwise=FALSE, trace=TRUE)
heuristic_arima <- stats::arima(AirPassengers,  order=c(2, 1, 1), seasonal=c(2, 1, 0)); heuristic_arima

# reference: plot.ts, plot a class of time series
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/plot.ts
stats::plot.ts(model$residuals)

# reference: acf, pacf
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/acf
stats::acf(stats::ts(model$residuals), main="ACF Residual")
stats::pacf(stats::ts(model$residuals), main="PACF Residual")

# reference: forecast
# https://www.rdocumentation.org/packages/forecast/versions/8.13/topics/forecast
predicted <- forecast::forecast(model, level=c(95), h=10*12)

airpassenger_plot(predicted, 
                title="Predict next 10 years of Air Passengers per month",
                xrange=c(1949:1971),
                yrange=seq(200, 1100, by=300))

# reference: Box.test
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/Box.test
stats::Box.test(model$resid, type="Ljung-Box")
stats::Box.test(heuristic_arima$resid, type="Ljung-Box")
