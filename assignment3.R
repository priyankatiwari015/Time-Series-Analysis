# Clear workspace
rm(list = ls())
# Clear workspace
rm(list = ls())
# Set working directory
setwd("D:/Git/time_series_project")
# Load necessary libraries
library(readr)
library(tseries)
library(lmtest)
library(forecast)
library(TSA)
## Necessary functions ##
31
# function to check if a series follows normal distribution
check_normality <- function(series, plot_title) {
  par(mfrow = c(1, 1))
  # QQ plot
  qqnorm(series, main = plot_title)
  qqline(series, col = 4, lwd = 1.5, lty = 1)
  # Shapiro-Wilk test
  SWresult <- shapiro.test(series)
  print(SWresult)
}
# function for stationarity check using Dickey-Fuller and Phillips-Perron Unit Root test
test_stationarity <- function(ts_data) {
  adf_result <- adf.test(ts_data)
  print(adf_result)
  pp_result <- pp.test(ts_data)
  print(pp_result)
  kpss_result <- kpss.test(ts_data)
  print(kpss_result)
}
#function for sort AIC and BIC
sort.score <- function(x, score = c("bic", "aic")){
  if (score == "aic"){
    x[with(x, order(AIC)),]
  } else if (score == "bic") {
    x[with(x, order(BIC)),]
  } else {
    32
    warning('score = "x" only accepts valid arguments ("aic","bic")')
  }
}
# Check the data
check_data <- function(data) {
  print(class(data))
  print(any(is.na(data)))
  print(summary(data))
  print(head(data))
}
# function to plot different SARIMA models (p=0, d=1, q=0)(P=0, D=1, Q=0)_s
## purpose: (1) Check residuals of series (ts plot, acf plot, pacf plot)
## purpose: (2) Returns model
sarima_res_test <- function(ts_data, p = 0, d = 1, q = 0, P = 0, D = 1, Q = 0, s = 12) {
  model <- arima(ts_data, order = c(p, d, q),
                 seasonal = list(order = c(P, D, Q), period = s))
  res <- residuals(model)
  # ts plot, acf and pacf plots of residuals
  par(mfrow = c(3, 1))
  plot(res, xlab = 'Time', ylab = 'Residuals', main = 'Residuals Time Series', type = 'l')
  acf(res, lag.max = 50, main = 'ACF of Residuals')
  pacf(res, lag.max = 50, main = 'PACF of Residuals')
  par(mfrow = c(1, 1))
  return(model) # returns SARIMA model
}
33
#getwd()
# Set working directory
#setwd("/Users/priyankatiwari/Documents/RMIT/Sem4/time_series/Assignment3")
data =read_csv("DepartmentStoreretailing.csv")
check_data(data)
# Result: All data available
# checking for missing timepoints
time_str <- data$Time # e.g., "Apr-1982"
months <- substr(time_str, 1, 3)
years <- as.numeric(sub(".*-", "", time_str))
time_numeric <- years * 12 + match(months, month.abb)
full_range <- min(time_numeric):max(time_numeric)
full_months <- (full_range - 1) %% 12 + 1
full_years <- (full_range - 1) %/% 12
full_labels <- paste0(month.abb[full_months], "-", full_years)
actual_labels <- paste0(months, "-", years)
missing_months <- setdiff(full_labels, actual_labels)
if (length(missing_months) == 0) {
  cat("None, all timepoints available\n")
} else {
  cat("Missing timepoint(s):\n")
  print(missing_months)
}
# Result: All timepoints available
34
retail_data= ts(data$`Department store retailing`, start = c(1982,4), frequency = 12)
class(retail_data)
head(retail_data)
summary(retail_data)
plot(retail_data,cex.main = 0.8, type='o',ylab="Department stores retail", main="Figure
1.1: Time Series plot of total number of department stores in retail ")
plot(retail_data,cex.main = 0.8,,type='l', ylab = "Department stores retail", main="Figure
1.2: Time series plot of total number of department stores in retail.")
points(y=retail_data,x=time(retail_data), pch=as.vector(season(retail_data)))
# Sample every 6 months for plot clarity
idx <- seq(1, length(retail_data), by = 6)
plot(time(retail_data)[idx], retail_data[idx], type = "o",
     xlab = "Year", ylab = "Retail Sales (sampled every 6 months)",
     main = "Sampled Department Store Retail Sales",
     col = "black", pch = 16, lwd = 2)
# ACF & PACF plot to confirm frequency
acf(as.vector(retail_data), main = "Figure 1.3: ACF of number of department stores in
retail", xlab = "Lag", ylab = "ACF", lag.max=50)
pacf(as.vector(retail_data), main = "Figure 1.4: PACF of total number of department
stores in retail", xlab = "Lag", ylab = "PACF",ylim = c(-1, 1))
# first lag scatter plot
lag1_retail_data <- zlag(retail_data)
index = 2:length(retail_data)
plot(y=retail_data,x=lag1_retail_data, ylab='Number of stores', xlab='First lag', main=
       "Figure 1.5: Scatter plot of neighboring number of stores",cex.main = 0.8)
35
cor(retail_data[index], lag1_retail_data[index])
# second lag scatter plot
lag2_retail_data <- zlag(lag1_retail_data)
index2 = 3:length(lag2_retail_data)
plot(y=retail_data,x=lag2_retail_data, ylab='Number of stores', xlab='Second lag', main=
       "Figure 1.6: Scatter plot of number of stores and its second lag values",cex.main = 0.8)
cor(retail_data[index2], lag2_retail_data[index2])
## Changing variance, seasonality, non-stationary
# checking if the original series follows normal distribution
check_normality(retail_data, "Figure 2.1: QQ plot of number of department stores in
retail")
# Transformation
## trying different seq for BoxCox transformation
# BC1 <- BoxCox.ar(retail_data,lambda = seq(-1, 0, 0.01)) # Error
# BC2 <- BoxCox.ar(retail_data,lambda = seq(-1, 1, 0.01)) # Error
# BC3 <- BoxCox.ar(retail_data,lambda = seq(0, 1, 0.01)) # seq worked
BC4 <- BoxCox.ar(retail_data,lambda = seq(0, 2, 0.01)) # seq worked
# BC5 <- BoxCox.ar(retail_data,lambda = seq(0, 3, 0.01)) # Error
BC4$ci
lambda <- BC4$lambda[which(max(BC4$loglike) == BC4$loglike)]
lambda
rt_dataBC = (retail_data^lambda-1)/lambda #Box-cox transformed data
# check again --> plot and normality
plot(rt_dataBC, type = "o", ylab = "Box-cox transformed data", xlab = "Time (Months)",
     main = "Figure 2.2: Box-Cox transformed data of number of department stores in retail",
     cex.lab = 0.5, cex.main = 0.8)
# check noramality of transformed data
check_normality(rt_dataBC, "Figure 2.3: QQ plot of Box-cox transformed data")
36
# Trend & Seasonality
# checking for stationarity of transformed series
test_stationarity(rt_dataBC)
# 1st difference on transformed series
rt_dataBCDiff <- diff(rt_dataBC, differences = 1)
# time series plot of 1st difference on transformed series
plot(rt_dataBCDiff, type = "o", ylab = "Number of department stores", xlab = "Time
(Months)", main = "Figure 2.4: First Difference of Transformed data of number of
department stores.", cex.main = 0.8)
# ACF & PACF plot of 1st difference on transformed series
acf(rt_dataBCDiff, main = "Figure 2.5: ACF of first difference of transformed data", xlab =
      "Lag", ylab = "ACF", lag.max = 80, cex.main = 0.8)
pacf(rt_dataBCDiff, main = "Figure 2.6: PACF of first difference of transformed data", xlab
     = "Lag", ylab = "PACF",ylim = c(-1, 1), cex.main = 0.8)
# checking for stationarity in 1st difference of transformed series
test_stationarity(rt_dataBCDiff)
# seasonal difference on 1st difference on transformed series
rt_dataBCDiffSD <- diff(rt_dataBCDiff, lag = 12)
# time series plot of 1st & seasonal difference on transformed series
plot(rt_dataBCDiffSD, type = "o", ylab = "Number of department stores", xlab = "Time
(Months)", main = "Figure 2.7: First & seasonal difference of transformed data.", cex.main
     = 0.8)
37
# Residuals approach to finalize seasonal components of model
# --> applied on transformed data
## SARIMA (0,1,0)x(0,1,0)12 returns residual model
m1 = sarima_res_test(rt_dataBC)
# SARIMA(0,1,0) (1,1,2)_12 returns residual model
m2 = sarima_res_test(rt_dataBC, P=1, Q=2)
# nonseasonal parameter determinations
# --> applied on 1st and seasonal difference on transformed data
# ACF & PACF plot of 1st & seasonal difference on transformed series
acf(rt_dataBCDiffSD, main = "Figure 3.1: ACF of first & seasonal difference of
transformed data", xlab = "Lag", ylab = "ACF", lag.max = 80, cex.main = 0.8)
pacf(rt_dataBCDiffSD, main = "Figure 3.2: PACF of first & seasonal difference of
transformed data", xlab = "Lag", ylab = "PACF",ylim = c(-1, 1), cex.main = 0.8)
# EACF plot
eacf(rt_dataBCDiffSD, ar.max = 10, ma.max = 10)
# BIC based model selection
set.seed(92397)
res = armasubsets(y=rt_dataBCDiffSD, nar=10, nma=10, y.name='p',ar.method='ols')
plot(res, main = "")
title(main = "Figure 3.3: BIC selection plot for models", line = 6)
###p = 5, 6, 7
##q = 3, 4
38
###### MODEL_FITTING USING ML, CSS_ML, CSS(CSS some models failed and were
assigned NA)
library(forecast)
library(lmtest)
# Define seasonal component for SARIMA
seasonal_order <- list(order = c(1, 1, 2), period = 12)
# List of (p,d,q) orders to fit
orders_to_fit <- list(
  c(0, 0, 1), c(0, 0, 2), c(1, 0, 2), c(1, 0, 3), c(2, 0, 2),
  c(2, 0, 3), c(5, 0, 3), c(5, 0, 4), c(6, 0, 3) #c(6, 0, 4), c(7,0,3)c(7, 0, 4) not considered
  due to non convergence.
  # Function to fit SARIMA models with specified estimation method and print coeftest
  fit_sarima_models <- function(series, method) {
    model_list <- list()
    for (ord in orders_to_fit) {
      label <- paste0("SARIMA(", paste(ord, collapse = ","), ")x(1,1,2)[12]_Method-", method)
      cat("\nFitting:", label, "\n")
      fit <- Arima(series, order = ord, seasonal = seasonal_order, method = method)
      # Print coeftest output for the fitted model
      cat("Coefficient test for", label, ":\n")
      print(coeftest(fit))
      cat("\n-----------------------------\n")
      39
      model_list[[label]] <- fit
    }
    return(model_list)
  }
  # The rest of your code remains unchanged:
  # compare_models(), extract_accuracy_metrics(), and the loop running all three methods
  # Fit models with all three methods
  methods <- c("ML", "CSS-ML", "CSS")
  all_results <- list()
  for (m in methods) {
    cat("\n==================== Fitting models with method:", m,
        "====================\n")
    all_results[[m]] <- fit_sarima_models(rt_dataBCDiff, method = m)
  }
  # Compare and display results for each method
  for (m in methods) {
    cat("\n==== Model comparison for method:", m, "====\n")
    scores <- compare_models(all_results[[m]])
    cat("\nSorted by AIC:\n")
    print(scores[order(scores$AIC), ])
    cat("\nSorted by BIC:\n")
    40
    print(scores[order(scores$BIC), ])
    cat("\nAccuracy metrics:\n")
    acc <- extract_accuracy_metrics(all_results[[m]])
    print(acc)
  }
  #-------------------------------------------------------------------------
  # Diagnostic Checking
  #-------------------------------------------------------------------------
  helper <- function(class = c("acf", "pacf"), ...) {
    # Capture additional arguments
    params <- match.call(expand.dots = TRUE)
    params <- as.list(params)[-1]
    # Calculate ACF/PACF values
    if (class == "acf") {
      acf_values <- do.call(acf, c(params, list(plot = FALSE)))
    } else if (class == "pacf") {
      acf_values <- do.call(pacf, c(params, list(plot = FALSE)))
    }
    # Extract values and lags
    acf_data <- data.frame(
      41
      Lag = as.numeric(acf_values$lag),
      ACF = as.numeric(acf_values$acf)
    )
    # Identify seasonal lags to be highlighted
    seasonal_lags <- acf_data$Lag %% 1 == 0
    # Plot ACF/PACF values
    if (class == "acf") {
      do.call(acf, c(params, list(plot = TRUE)))
    } else if (class == "pacf") {
      do.call(pacf, c(params, list(plot = TRUE)))
    }
    # Add colored segments for seasonal lags
    for (i in which(seasonal_lags)) {
      segments(x0 = acf_data$Lag[i], y0 = 0, x1 = acf_data$Lag[i], y1 = acf_data$ACF[i], col
               = "red")
    }
  }
  # seasonal_acf ------------------------------------------------------------
  seasonal_acf <- function(...) {
    helper(class = "acf", ...)
  }
  42
  # seasonal_pacf -----------------------------------------------------------
  seasonal_pacf <- function(...) {
    helper(class = "pacf", ...)
  }
  #########################################
  #-----------ML513---------------------
  fit_ml_5_1_3 <- Arima(rt_dataBC, order = c(5,1,3), seasonal = c(1,1,2), method = "ML")
  res.513 <- rstandard(fit_ml_5_1_3)
  par(mfrow = c(1, 2))
  # Histogram
  hist(res.513,xlab='Standardized Residuals',main="Fig 5.4 (a) Residuals from the
SARIMA(5,1,3)x(1,1,2)_12 Model")
  #QQ plot
  qqnorm(res.513,main="Fig5.4 (b): Q-Q plot for Residuals: SARIMA(5,1,3)x(1,1,2)_12
Model.")
  qqline(res.513)
  #Shapiro wilk
  shapiro.test(res.513)
  par(mar=c(1,1,4,1))
  tsdiag(fit_ml_5_1_3,gof=15,omit.initial=F)
  43
  par(mfrow = c(2, 1))
  # Seasonal PACF
  seasonal_pacf(res.513,
                lag.max=36,
                main="Fig 5.6: PACF of Residuals from the SARIMA(5,1,3)x(1,1,2)_12 Model.")
  #-----------ML613---------------------
  fit_ml_6_1_3 <- Arima(rt_dataBC, order = c(6,1,3), seasonal = c(1,1,2), method = "ML")
  res.613 <- rstandard(fit_ml_6_1_3)
  par(mfrow = c(1, 2))
  # Histogram
  hist(res.613,xlab='Standardized Residuals',main="Fig 5.7 (a) Residuals from the
SARIMA(6,1,3)x(1,1,2)_12 Model")
  #QQ plot
  qqnorm(res.613,main="Fig 5.7 (b): Q-Q plot for Residuals: SARIMA(6,1,3)x(1,1,2)_12
Model.")
  qqline(res.613)
  #Shapiro wilk
  shapiro.test(res.613)
  44
  par(mar=c(1,1,4,1))
  tsdiag(fit_ml_6_1_3,gof=15,omit.initial=F)
  par(mfrow = c(2, 1))
  # Seasonal PACF
  seasonal_pacf(res.613,
                lag.max=36,
                main="Fig 5.9: PACF of Residuals from the SARIMA(6,1,3)x(1,1,2)_12 Model.")
  #-----------CSS213---------------------
  fit_css_ml_2_1_3 <- Arima(rt_dataBC, order = c(2,1,3), seasonal = c(1,1,2), method =
                              "CSS-ML")
  res.213 <- rstandard(fit_css_ml_2_1_3)
  par(mfrow = c(1, 2))
  # Histogram
  hist(res.213,xlab='Standardized Residuals',main="Fig 5.1 (a) Residuals from the
SARIMA(2,1,3)x(1,1,2)_12 Model")
  #QQ plot
  qqnorm(res.213,main="Fig 5.1 (b): Q-Q plot for Residuals: SARIMA(2,1,3)x(1,1,2)_12
Model.")
  qqline(res.213)
  45
  #Shapiro wilk
  shapiro.test(res.213)
  par(mar=c(1,1,4,1))
  tsdiag(fit_css_ml_2_1_3,gof=15,omit.initial=F)
  par(mfrow = c(1, 1))
  # Seasonal PACF
  seasonal_pacf(res.213,
                lag.max=36,
                main="Fig 5.3: PACF of Residuals from the SARIMA(2,1,3)x(1,1,2)_12 Model.")
  #--------------------------------------
  # Forecasting
  #----------------------------------------
  frc.513.ml <- forecast(fit_ml_5_1_3, h=10)
  print(frc.513.ml)
  plot(frc.513.ml)
  lines(fitted(frc.513.ml), col= "blue")
  legend("topleft", lty=1, pch=1, col=c("blue","black"), text.width = 2, c("Data", "Fitted "))
  #--------------------------------------
  # Forecasting
  46
  #----------------------------------------
  frc.513.ml <- forecast(fit_ml_5_1_3, h=10)
  print(frc.513.ml)
  plot(frc.513.ml, main = "Fig 6.1: Forecasts from SARIMA(5,1,3)(1,1,2)[12]")
  lines(fitted(frc.513.ml), col= "blue")
  legend("topleft", lty=1, pch=1, col=c("blue","black"), text.width = 2, c("Data", "Fitted "))