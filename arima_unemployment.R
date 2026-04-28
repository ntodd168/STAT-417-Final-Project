# Reads unemployment data
unemployment_data <- read.csv("UNRATENSA.csv")

# Keeps observations before 2020
end_date <- as.Date("2020-01-01")
unemployment_data <- unemployment_data[as.Date(unemployment_data$observation_date) < end_date, ]

# Creates date and unemployment rate values
observation_dates <- as.Date(unemployment_data$observation_date)
unemployment_rate <- unemployment_data$UNRATENSA

# Creates monthly unemployment time series
start_year <- as.integer(format(min(observation_dates), "%Y"))
start_month <- as.integer(format(min(observation_dates), "%m"))
unemployment_series <- ts(unemployment_rate, start = c(start_year, start_month), frequency = 12)

# Defines model selection rule
information_criterion <- "aicc"

# Searches ARIMA orders and picks optimal model
library(forecast)
best_model <- auto.arima(unemployment_series, seasonal = TRUE, ic = information_criterion, stepwise = FALSE, approximation = FALSE)

# Prints model and model scores
print(best_model)
cat("AIC:", best_model$aic, "\n")
cat("AICc:", best_model$aicc, "\n")
cat("BIC:", best_model$bic, "\n")
