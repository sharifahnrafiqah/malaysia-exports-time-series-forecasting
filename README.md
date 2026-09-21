Malaysia Export Time Series Analysis & Forecasting
Overview

This project analyses Malaysia's historical export data using time series analysis and forecasting techniques in R.

The project was originally completed as part of a university group assignment for STA572. This GitHub version has been organised as a portfolio project to demonstrate my experience in statistical analysis, time series modelling, data visualisation, and forecasting.

Objectives
Explore the historical pattern of Malaysia's exports
Analyse the distribution and characteristics of the data
Examine autocorrelation and partial autocorrelation
Test whether the time series is stationary
Apply appropriate time series models
Compare model performance using forecasting error measures
Perform residual diagnostics
Generate future export forecasts
Tools & Technologies
R
RStudio
Time Series Analysis
Statistical Modelling
Data Visualisation
Methodology
1. Exploratory Data Analysis

The analysis begins with summary statistics and distribution analysis using measures such as:

Mean
Median
Standard deviation
Skewness
Minimum and maximum values

The project also uses boxplots and histograms to examine the distribution of Malaysia's export data.

2. Time Series Analysis

The autocorrelation function (ACF) was examined to identify relationships between observations at different time lags.

PACF analysis was subsequently used to help identify the autoregressive order for ARIMA modelling.

3. Stationarity Testing

The time series was examined for stationarity.

The Augmented Dickey-Fuller (ADF) test was used to test for a unit root. First differencing was subsequently applied to address the non-stationarity identified in the original series.

4. Forecasting Models

Several models were fitted and compared, including:

AR(1)
MA(0)
ARMA(1,0)
ARIMA(1,1,0)
ARIMA(0,1,0) with drift
Holt's method
Naïve with drift/trend
5. Model Evaluation

Models were compared using forecasting and information criteria including:

RMSE
MAE
MAPE
MASE
AIC
BIC
ACF1
6. Residual Diagnostics

Residuals were evaluated using graphical and statistical diagnostics, including:

Residual plots
Histogram
Q-Q plot
ACF
Anderson-Darling test
Breusch-Pagan test
Ljung-Box test
Durbin-Watson test
Outlier detection
7. Forecasting

The selected forecasting model was used to generate future export forecasts with 80% and 95% prediction intervals.

Key Findings

The original analysis identified an increasing trend in Malaysia's export data.

The ADF test indicated that the original time series was non-stationary. After first differencing, further stationarity testing was performed.

Several forecasting models were compared. The original analysis selected an ARIMA(0,1,0) model with drift based on the reported model comparison and forecasting error measures.

The forecasts indicated an overall increasing pattern, while the widening prediction intervals reflected greater uncertainty over longer forecasting horizons.
