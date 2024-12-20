
library(stats)
library(dplyr)
library(ggplot2)
library(tseries)
library(forecast)


# ARIMA -------------------------------------------------------------------


df <- read.csv('BICCtemp.txt', sep = ';', dec = ",")
df<- df %>% subset(Rok!=2020)
month_names <- c("Styczeń", "Luty", "Marzec", "Kwiecień", "Maj", "Czerwiec", "Lipiec", "Sierpień", "Wrzesień", "Październik", "Listopad", "Grudzień")

## zamiana na time series
ts_data <- ts(df$Wskaznik, start=c(2000, 1), frequency=12)

plot(ts_data)
summary(ts_data)

## sprawdzenie zalozen ARIMA czyli stacjonarnosc (srednia, wariancja i autokorelacja sa stale w czasie)
adf.test(ts_data)

fit <- auto.arima(ts_data)

## fajne wykresy residuals, sprawdzanie czy nie ma autokorelacji?
checkresiduals(fit)

## prognoza na 24 miesiace do przodu
forecast_values <- forecast(fit, h=24)
plot(forecast_values)



# SARIMA ------------------------------------------------------------------

df <- read.csv('BICCtemp.txt', sep = ';', dec = ",")
df<- df %>% subset(Rok!=2020)
month_names <- c("Styczeń", "Luty", "Marzec", "Kwiecień", "Maj", "Czerwiec", "Lipiec", "Sierpień", "Wrzesień", "Październik", "Listopad", "Grudzień")

## zamiana na time series
ts_data <- ts(df$Wskaznik, start=c(2000, 1), frequency=12)


## sprawdzenie zalozen SARIMA czyli stacjonarnosc (srednia, wariancja i autokorelacja sa stale w czasie)
adf.test(ts_data)

fit_sarima <- auto.arima(ts_data, seasonal=TRUE)

## fajne wykresy residuals, sprawdzanie czy nie ma autokorelacji?
checkresiduals(fit_sarima)

## prognoza na 24 miesiace do przodu
forecast_values_sarima <- forecast(fit_sarima, h=24)
plot(forecast_values_sarima)

