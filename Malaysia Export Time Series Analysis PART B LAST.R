#PART B-------------------------------------------------------------------------
#STEP 1 IMPORTING TIME SERIES-------------------------------------------------
exportMalaysia<-read.csv(file.choose(), header = TRUE )
dataset=exportMalaysia
dataset
data1=dataset[,2] 
data1

#STEP 2 SUMMARY STATISTICS----------------------------------------------------
library(e1071)
summary(data1)
length(data1)
is.na(data1)
sd(data1)
skewness(data1)
head(data1)
tail(data1)

#STEP 3 SHAPE OF DISTRIBUTION-------------------------------------------------
par(mfrow=c(1,1))
boxplot(data1, main="Boxplot of Total Export in Malaysia")
plot(data1, main="Scatterplot of Total Export in Malaysia" )
hist(data1, main="Histogram of Total Export in Malaysia")
set.seed(1)
hist(data1, main = "Normal Distribution for Data1")
c <- seq(min(data1), max(data1), length = 27)
d <- dnorm(c, mean = mean(data1), sd = sd(data1)) 
d <- d * diff(hist(data1, main = "Histogram of Total Export in Malaysia")$mids[1:2]) * length(data1)
lines(c, d, lwd = 2, col = "red")
boxplot.stats(data1)$out

#STEP 4 TIME SERIES COMPONENTS------------------------------------------------
#TIME SERIES PLOT
#TURN DATA TO DATA FRAME
data1_df <- data.frame(
  year = 1996:2022,
  export = c(data1)
)

library(ggplot2)
plot_data <- data1_df %>%
  full_join(data1_df , by = "year") %>%
  rename(actual = export.x) %>%
  select(year, actual)

ggplot(plot_data, aes(x = year)) +
  geom_line(aes(y = actual, color = "Actual")) +
  labs(title = "Total Malaysia Exports",
       x = "Time(Year)",
       y = "Billions of US Dollars") +
  scale_color_manual(name = "Legend", values = c("Actual" = "black")) +
  theme_minimal()
#increasing trend, no seasonality component, cyclical component, low variablility, no irregularities

#ACF PLOT OF DATA1
acf(data1, main="ACF of Total Malaysia Exports")

#DETECT SEASONALITY
data1.ts=ts(data1,frequency=1, start=c(1996,1))
library(forecast)
ggseasonplot(data1.ts)

#STEP 5 IS THE TIME SERIES STATIONARY IN MEAN AND VARIANCE?---------------------
data1
library(MASS)
?boxcox
BXCX=boxcox(lm(data1~1))
lambda=BXCX$x[which.max(BXCX$y)]
lambda
#lambda = 1.030303 indicates power transformation with exponent 1.3.
#there is no need to continue transforming data as it is closer to 1 than 2
library(tseries)
adf.test(data1)
#AUGMENTED DICKY FULLER TEST
#H0: THE TIME SERIES IS NOT STATIONARY
#IF THE P-VALUE IS MORE THAN 0.05, FAIL TO REJECT H0
#SINCE THE P-VALUE IS MORE THAN 0.05, FAIL TO REJECT H0, THE TIME SERIES IS NOT STATIONARY

#PRACTICE AR(P), MA(P), ARMA(P,Q) USING DATA1 ----------------------------------
acf(data1) 
#q-order= 5
pacf(data1)
#p-order =1

#AR(1)
p=1
modelprac_1 = arima(data1, order = c(p,0,0))
modelprac_1
checkresiduals(modelprac_1)

#MA(5)
q=5
modelprac_2 = arima(data1, order = c(0,0,q))
modelprac_2
checkresiduals(modelprac_2)

#ARMA(1,5)
modelprac_3 = arima(data1, order = c(p,0,q))
modelprac_3
checkresiduals(modelprac_3)

#STEP 6 IF THE TIME SERIES IS NOT STATIONARY------------------------------------
#FIRST DIFFERENCING 
data1=dataset[,2]
data1
data1_diff=diff(data1, lag=1)
data1_diff
adf.test(data1_diff)
#AUGMENTED DICKY FULLER TEST
#H0: THE TIME SERIES IS NOT STATIONARY
#IF THE P-VALUE IS MORE THAN 0.05, FAIL TO REJECT H0
#SINCE THE P-VALUE IS MORE THAN 0.05, FAIL TO REJECT H0, THE TIME SERIES IS NOT STATIONARY
kpss.test(data1_diff)
#KPSS TEST
#H0: THE TIME SERIES IS TREND STATIONARY
#IF THE P-VALUE IS MORE THAN 0.05, FAIL TO REJECT H0
#SINCE THE P-VALUE IS MORE THAN 0.05, FAIL TO REJECT H0, THE TIME SERIES IS TREND STATIONARY
plot.ts(data1_diff, main= "First Differencing Plot") 

#STEP 7 FOR STATIONARY TIME SERIES (P ORDER, Q ORDER)---------------------------
#STATIONARY 
acf(data1_diff)
#q-order=0
pacf(data1_diff) 
#p-order=1


#STEP 8 FITTING AUTOREGRESSIVE TIME SERIES MODELS-----------------------------
#AR(1) MODEL
p=1
model1 = arima(data1, order = c(p,0,0))
model1
checkresiduals(model1)

#MA(0) MODEL
q=0
model2 = arima(data1, order = c(0,0,q))
model2
checkresiduals(model2)

#ARMA(1,0) MODEL
p=1
q=0
model3 = arima(data1, order = c(p, 0, q))
model3
checkresiduals(model3)

#ARIMA(1, 1, 0) MODEL
p=1
q=0
d= 1
model4 = arima(data1, order = c(p, d, q))
model4
checkresiduals(model4)

#AUTO ARIMA MODEL
model5 = auto.arima(data1, trace = TRUE)
model5
#WITH DRIFT MEANS THE MODEL DOES NOT HAVE INTERCEPT VALUE
checkresiduals(model5)

#STEP 9 ERROR MEASURES--------------------------------------------------------
accuracy(model1)
accuracy(model2)
accuracy(model3)
accuracy(model4)
accuracy(model5)
BIC(model1)
BIC(model2)
BIC(model3)
BIC(model4)
BIC(model5)
#best model= model 5 (AUTOARIMA : ARIMA(0,1,0)) WITH DRIFT
#AIC is from checkresiduals()

#STEP 10 RESIDUAL CHECKS------------------------------------------------------
residuals_model5=model5$residuals

#1. iid
par(mfrow=c(1,1))
plot(residuals_model5, type = "p", 
     main = "Scatterplot of Model 5 Residuals")
hist(model5$residuals, xlab = "Residuals", main = "Histogram of Model 5 Residuals")
a <- seq(min(model5$residuals), max(model5$residuals), length = 26)
b <- dnorm(a, mean = mean(model5$residuals), sd = sd(model5$residuals)) 
b <- b * diff(hist(model5$residuals, xlab = "Residuals", main = "Histogram of Model 5 Residuals")$mids[1:2]) * length(model5$residuals)
lines(a, b, lwd = 2, col = "red")

#2. Normality
library(nortest)
ad.test(residuals_model5)
#ANDERSON DARLING TEST
#H0:THE RESIDUALS ARE NORMALLY DISTRIBUTED
#SINCE THE P-VALUE IS MORE THAN 0.05, FAIL TO REJECT H0, THE RESIDUALS ARE NORMALLY DISTRIBUTED
par(mfrow=c(1,1))
qqnorm(residuals_model4, main="Normal qqplot for Model 5" )
qqline(residuals_model4, col = "red", lwd = 2)

#3. Homoscedasticity
library(lmtest)
library(forecast)
library(ggplot2)
bptest(residuals_model5 ~ model5$fitted)
#BREUSH-PAGAN TEST
#H0: HOMOSCEDASTICITY PRESENT
#IF THE P-VALUE IS LESS THAN 0.05, REJECT H0, HETEROSCEDASTICITY IS PRESENT

standardized_residuals_model5 <- residuals_model5/sd(residuals_model5)
standardized_residuals_model5
fittedmodel5<- fitted(model5)
fittedmodel5
par(mfrow= c(1,1))
data_for_plot <- data.frame(fittedmodel5, standardized_residuals_model5)
ggplot(data_for_plot, aes(x=fittedmodel5, y=standardized_residuals_model5))+
  geom_point(color="blue") + 
  geom_hline(yintercept=0, col="red", lwd=1.5)+
  labs(title="Standardized Residuals vs. Fitted Values (Model 5)",
       xlab= "Fitted values",
       ylab= "Standardized Residuals") + theme_minimal()


#4. Absence of serial correlation
acf(residuals_model5, main="ACF Residual plot for Model 5")
library(car)
dwtest(residuals_model5 ~ model5$fitted)
#DURBIN-WATSON TEST
#H0:NO SERIAL CORRELATION
#IF THE DW VALUE IS CLOSER TO 2, REJECT H0, THERE IS SERIAL/AUTO CORRELATION
#SINCE THE P-VALUE IS MORE THAN 0.05, FAIL TO REJECT H0, THERE IS NO SERIAL CORRELATION

#5. Absence of lag dependence
par(mfrow=c(1,1))
plot(c(residuals_model5), main="Residual Scatter plot for Model 5")
Box.test(residuals_model5, lag = 1, type = "Ljung-Box")
#LJUNG-BOX TEST
#H0:THE RESIDUALS ARE INDEPENDENTLY DISTRIBUTED
#IF THE P-VALUE IS LESS THAN 0.05, REJECT H0
#SINCE THE P-VALUE IS MORE THAN 0.05, FAIL TO REJECT H0, THE RESIDUALS ARE INDEPENDENTLY DISTRIBUTED

#6. Absence of influential observations
boxplot(residuals_model5, main="Residual Boxplot for Model 5")
boxplot.stats(residuals_model5)$out

#7. Absence of volatility
RES2_model5 = residuals_model4^2
RES2_model5
acf(RES2_model5, main="ACF Residual^2 plot for Model 5")

#8. Repeat assumptions for rnorm (for comparison)
data1_rnorm = rnorm(300)
data1_rnorm
#iid
plot((data1_rnorm), 
     type = "p", main = "Scatterplot of rnorm")
set.seed(1)
hist(data1_rnorm, main = "Normal Distribution for rnorm")
c <- seq(min(data1_rnorm), max(data1_rnorm), length = 300)
d <- dnorm(c, mean = mean(data1_rnorm), sd = sd(data1_rnorm)) 
d <- d * diff(hist(data1_rnorm, main = "Normal Distribution for rnorm")$mids[1:2]) * length(data1_rnorm)
lines(c, d, lwd = 2, col = "red")
#normality
ad.test(data1_rnorm)
qqnorm(data1_rnorm)
qqline((data1_rnorm), col = "red", lwd = 2)
#homoscedasticity
error_rnorm = data1_rnorm
xrnorm = rep(c(-1,1),300)
yrnorm = 1 + xrnorm + error_rnorm
bptest(yrnorm~xrnorm)
#serial correlation
acf(data1_rnorm)
dwtest(residuals(data1_rnorm)~data1_rnorm$fitted)
acf(data1_rnorm)
dwtest(yrnorm~xrnorm)
#lag dependence
plot(c(data1_rnorm),lag = 1,
     main = "Scatter Plot of rnorm", xlab = "Predicted Values")
Box.test((data1_rnorm), lag = 1, type = "Ljung-Box")
#absence of influential observations 
boxplot((data1_rnorm), main = "Boxplot of rnorm")
boxplot.stats(data1_rnorm)$out
#absence of volatility
acf(c(data1_rnorm^2))

#STEP 11 FORECASTING----------------------------------------------------------
# Forecast 12 steps ahead using the best model
library(forecast)
library(forecast)
library(ggplot2)
library(tidyverse)

model5 = auto.arima(data1, trace = TRUE)

data1.ts = ts(data1, frequency = 1, start=c(1996), end=c(2022))
data1_model5 <- auto.arima(data1.ts, trace=TRUE,)
summary(data1_model5)
fitted_values <- fitted(data1_model5)
accuracy_model5 <- accuracy(data1_model5)
accuracy_model5
plot(data1.ts, col="black", main="Holt's method", ylab="Observed/Fitted", xlab="Time(Year)")
lines(fitted_values, col="red", lty=1)

forecast(data1_model5, h=12)
plot(forecast(data1_model5, h=12), main="Forecasts from model 5", xlab="Time(Year)", ylab="Total Export Malaysia")
lines(data1_model5$fitted, col="red")
legend("topleft", legend=c("Observed Data", "Forecast"), col=c("black", "red"), lty=1)

