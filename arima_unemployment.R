# Reads unemployment data
unemployment_data <- read.csv("UNRATENSA.csv")

# Creates date and unemployment rate values
observation_dates <- as.Date(unemployment_data$observation_date)
unemployment_rate <- unemployment_data$UNRATENSA

# Creates monthly unemployment time series
start_year <- as.integer(format(min(observation_dates), "%Y"))
start_month <- as.integer(format(min(observation_dates), "%m"))
unemployment_series <- ts(unemployment_rate, start = c(start_year, start_month), frequency = 12)

library(forecast)

# Plots unemployment series
plot(unemployment_series, main = "Monthly Unemployment", ylab = "Unemployment Rate", xlab = "Year")
Acf(unemployment_series, main = "ACF of Monthly Unemployment")

# Defines model selection rule
model_selection_rule <- "aicc"

# Searches ARIMA orders and picks optimal model
best_model <- auto.arima(unemployment_series, seasonal = TRUE, ic = model_selection_rule, stepwise = FALSE, approximation = FALSE)

# Prints model and model scores
print(best_model)
cat("AIC:", best_model$aic, "\n")
cat("AICc:", best_model$aicc, "\n")
cat("BIC:", best_model$bic, "\n")

# Creates fitted values and residual values
fitted_values <- fitted(best_model)
residual_values <- residuals(best_model)

# Prints residual fit scores and accuracy metrics
print(accuracy(best_model))

# Plots observed values and fitted values
plot(unemployment_series, main = "Observed and Fitted Unemployment", ylab = "Unemployment Rate", xlab = "Year")
lines(fitted_values, col = "blue", lwd = 2)
legend("topleft", legend = c("Observed", "Fitted"), col = c("black", "blue"), lty = 1, bty = "n")

# Plots residual ACF
Acf(residual_values, main = "ACF of Residuals")

# Checks residuals
checkresiduals(best_model)
