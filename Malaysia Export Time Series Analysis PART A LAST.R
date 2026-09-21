#PART A-------------------------------------------------------------------------
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
boxplot(data1, main="Boxplot of Total Export in Malaysia")
plot(data1, main="Scatterplot of Total Export in Malaysia" )
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
library(dplyr)
library(tidyr)
library(forecast)
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

#STEP 5 FIT A SUITABLE TIME SERIES MODEL--------------------------------------
#naive with trend model
naive_trend_model=rwf(data1,drift=TRUE)
summary(naive_trend_model)
fittednaive <-naive_trend_model$fitted
fittednaive

#average percent change model
avgpercent_func <- function(data, h=1) {
  if(!is.data.frame(data) || !"export" %in% colnames(data)) {
    stop("Input data must be a data frame with an 'export' column.")
    
  }
  
  data <- data %>%
    mutate(
      percent_change_t = (lag(export)+ lag(export,2))/2,
      average_percent_change = (percent_change_t/lag(export,2))*export,
      Ft_m = export + average_percent_change
    )
  
  data <- data %>% drop_na()
  
  for(i in 1:h){
    last_export <- tail(data$export,1)
    last_avg_change <- tail(data$average_percent_change, 1)
    new_forecast <- last_export + last_avg_change
    data <- rbind(data, data.frame(year = max(data$year) + 1, export = NA, percent_change_t = NA, average_percent_change = NA, Ft_m = new_forecast))
  }
  return(data)
}

avgpercent_model <- avgpercent_func(data1_df)
avgpercent_model
summary(avgpercent_model)
#error measure metrics for average percent model
actual_values <- tail(data1_df$export, nrow(avgpercent_model) -5)
predicted_values <- head(avgpercent_model$Ft_m, nrow(avgpercent_model) - 5)
RES_average <- data1[3:length(actual_values)]- predicted_values[3:length(predicted_values)]
scale <- mean(abs(diff(data1)))


mae <- mean(abs(predicted_values - actual_values))
mse <- mean((predicted_values - actual_values)^2)
mape <- mean(abs((predicted_values - actual_values) / actual_values)) * 100
rmse <- sqrt(mse)
me <- mean(RES_average)
mase <- mean(abs(RES_average))/scale
acf1 <- acf(RES_average, plot=FALSE)$acf[2]
mpe <- mean((actual_values- predicted_values)/ actual_values)*100


#Print the accuracy metrics#Priscalent the accuracy metrics
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "%\n")
cat("Mean Absolute Percentage Error (MAPE):", mape, "%\n")
cat("Mean Error (ME):", me, "\n")
cat("Mean Absolute Scaled Error (MASE):", mase, "\n")
cat("First Autocorrelation of Residuals (ACF1):", acf1, "\n")
cat("Mean Percentage Error (MPE):", mpe, "\n")


#holt's model
holt_model=holt(data1, h=12)
summary(holt_model)
fittedholt <-holt_model$fitted
fittedholt
#BEST MODEL IS HOLT'S METHOD

#STEP 6 HOW WELL MODEL FITS THE DATA------------------------------------------
#Naive with Trend
library(ggplot2)
plotdata_naive<- data.frame(
  year = 1996:2022,
  actual = as.numeric(data1),
  fitted = as.numeric(fittednaive)
)
ggplot(plotdata_naive, aes(x = year)) +
  geom_line(aes(y = actual, color = "Actual")) +
  geom_line(aes(y = fitted, color = "Fitted")) +
  labs(title = "Actual vs Fitted Values with Naive Trend Model",
       x = "Year",
       y = "Malaysia Exports") +
  scale_color_manual(name = "Legend", values = c("Actual" = "blue", "Fitted" = "red")) +
  theme_minimal()

#Average Percent Change
plotdata_holt <- data1_df %>%
  full_join(avgpercent_model, by = "year") %>%
  rename(actual = export.x, fitted = Ft_m) %>%
  mutate(fitted = ifelse(is.na(fitted), export.y, fitted)) %>%
  select(year, actual, fitted)

ggplot(plotdata_holt, aes(x = year)) +
  geom_line(aes(y = actual, color = "Actual")) +
  geom_line(aes(y = fitted, color = "Fitted")) +
  labs(title = "Actual vs Fitted Values with Average Percent Change model",
       x = "Year",
       y = "Export Values") +
  scale_color_manual(name = "Legend", values = c("Actual" = "blue", "Fitted" = "red")) +
  theme_minimal()

#Holt's method
library(ggplot2)
plotdata_holt<- data.frame(
  year = 1996:2022,
  actual = data1,
  fitted = fittedholt
)
ggplot(plotdata_holt, aes(x = year)) +
  geom_line(aes(y = actual, color = "Actual")) +
  geom_line(aes(y = fitted, color = "Fitted")) +
  labs(title = "Actual vs Fitted Values with Holt's Model",
       x = "Year",
       y = "Malaysia Exports") +
  scale_color_manual(name = "Legend", values = c("Actual" = "blue", "Fitted" = "red")) +
  theme_minimal()

#STEP 7 ERROR MEASURE---------------------------------------------------------
library(stats)
RES_naivetrend= residuals(naive_trend_model)
RES_average <- data1[3:length(actual_values)]- predicted_values[3:length(predicted_values)]
RES_holt= residuals(holt_model)
RES_naivetrend
RES_average
RES_holt
accuracy(naive_trend_model)
accuracy(holt_model)

#error measure metrics for average percent model
actual_values <- tail(data1_df$export, nrow(avgpercent_model) -5)
predicted_values <- head(avgpercent_model$Ft_m, nrow(avgpercent_model) - 5)
RES_average <- data1[3:length(actual_values)]- predicted_values[3:length(predicted_values)]
scale <- mean(abs(diff(data1)))


mae <- mean(abs(predicted_values - actual_values))
mse <- mean((predicted_values - actual_values)^2)
mape <- mean(abs((predicted_values - actual_values) / actual_values)) * 100
rmse <- sqrt(mse)
me <- mean(RES_average)
mase <- mean(abs(RES_average))/scale
acf1 <- acf(RES_average, plot=FALSE)$acf[2]
mpe <- mean((actual_values- predicted_values)/ actual_values)*100


#Print the accuracy metrics#Priscalent the accuracy metrics
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "%\n")
cat("Mean Absolute Percentage Error (MAPE):", mape, "%\n")
cat("Mean Error (ME):", me, "\n")
cat("Mean Absolute Scaled Error (MASE):", mase, "\n")
cat("First Autocorrelation of Residuals (ACF1):", acf1, "\n")
cat("Mean Percentage Error (MPE):", mpe, "\n")

accuracy(holt_model)

#STEP 8 RESIDUAL CHECKS-------------------------------------------------------
#1. iid
par(mfrow=c(1,1))
plot(RES_holt, type = "p", 
     main = "Scatterplot of Holt's model Residuals")
set.seed(1)
hist(RES_holt, main = "Normal Distribution for Data1")
c <- seq(min(RES_holt), max(RES_holt), length = 27)
d <- dnorm(c, mean = mean(RES_holt), sd = sd(RES_holt)) 
d <- d * diff(hist(RES_holt, main = "Histogram of Holt's Model Residuals")$mids[1:2]) * length(RES_holt)
lines(c, d, lwd = 2, col = "red")
#2. Normality
library(nortest)
ad.test(RES_holt)
#ANDERSON DARLING TEST
#H0:THE RESIDUALS ARE NORMALLY DISTRIBUTED
#IF THE P-VALUE IS LESS THAN 0.05, REJECT H0, THE RESIDUALS ARE NOT NORMALLY DISTRIBUTED
#SINCE THE P-VALUE IS MORE THAN 0.05, FAIL TO REJECT H0, THE RESIDUALS ARE NORMALLY DISTRIBUTED
qqnorm(RES_holt, main="Normal qqplot for Holt's model" )
qqline(RES_holt, col = "red", lwd = 2)

#3. Homoscedasticity
library(lmtest)
library(forecast)
library(ggplot2)
bptest(RES_holt ~ holt_model$fitted)
#BREUSH-PAGAN TEST
#H0: HOMOSCEDASTICITY PRESENT
#IF THE P-VALUE IS LESS THAN 0.05, REJECT H0
#IF THE P-VALUE IS MORE THAN 0.05, FAIL TO REJECT H0, THERE HOMOSCEDASTICITY
#SINCE THE P-VALUE IS LESS THAN 0.05, REJECT H0, HETEROSCEDASTICITY PRESENT
#EXPLAIN HOW TO HANDLE HETEROSCEDASTICITY

standardized_RESholt <- RES_holt/sd(RES_holt)
standardized_RESholt
fitted_holt <- fitted(holt_model)
fitted_holt
par(mfrow= c(1,1))
data_for_plot <- data.frame(Fittedholt=fitted_holt, StandardizedResiduals_holt = standardized_RESholt)
ggplot(data_for_plot, aes(x=Fittedholt, y=StandardizedResiduals_holt))+
  geom_point(color="blue") + 
  geom_hline(yintercept=0, col="red", lwd=1.5)+
  labs(title="Standardized Residuals vs. Fitted Values (Holt's model)",
       xlab= "Fitted values",
       ylab= "Standardized Residuals") + theme_minimal()

#4. Absence of serial correlation
acf(RES_holt, main="ACF plot for Holt's method residuals")
library(car)
dwtest(RES_holt ~ holt_model$fitted)
#DURBIN-WATSON TEST
#H0:NO SERIAL CORRELATION
#IF THE DW VALUE IS CLOSER TO 2, REJECT H0, THERE IS SERIAL/AUTO CORRELATION
#IF THE P-VALUE IS LESS THAN 0.5, REJECT H0, THERE IS SERIAL/AUTO CORRELATION

#5. Absence of lag dependence
plot(c(RES_holt), main="Residual Scatter plot for Holt's model")
Box.test(RES_holt, lag = 1, type = "Ljung-Box")
#LJUNG-BOX TEST
#H0:THE RESIDUALS ARE INDEPENDENTLY DISTRIBUTED
#IF THE P-VALUE IS LESS THAN 0.05, REJECT H0
#SINCE THE P-VALUE IS MORE THAN 0.05, FAIL TO REJECT H0, THE RESIDUALS ARE INDEPENDENTLY DISTRIBUTED

#6. Absence of influential observations
boxplot(RES_holt, main="Residual Boxplot for Holt's model")
boxplot.stats(RES_holt)$out

#7. Absence of volatility
RES2_holt = RES_holt^2
RES2_holt
acf(RES2_holt, main="ACF Residual^2 plot for Holt's method model")

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
qqnorm(data1_rnorm, main ="Normal Q-Q Plot for rnorm")
qqline((data1_rnorm), col = "red", lwd = 2)
#homoscedasticity
error_rnorm = data1_rnorm
xrnorm = rep(c(-1,1),300)
yrnorm = 1 + xrnorm + error_rnorm
bptest(yrnorm~xrnorm)
STD_residuals_rnorm=data1_rnorm/sd(data1_rnorm)
plot(STD_residuals_rnorm, data1_rnorm, xlab = "Standardized Residuals", ylab = "Fitted Values", main="Standardized Residuals vs. Fitted Values rnorm")
#at a horizontal line at 0
abline(h = 0, col = 2)
#serial correlation
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
data1_rnorm2 = (data1_rnorm)^2
data1_rnorm2
acf(c(data1_rnorm^2))

#STEP 9 THE BEST MODEL--------------------------------------------------------
#HOLT MODEL IS THE BEST MODEL
#EXPLAIN WHY THE CHOSEN MODEL IS THE BEST MODEL
#DATA
#ACCURACY

#STEP 10 FORECASTING----------------------------------------------------------
library(forecast)
library(ggplot2)
library(tidyverse)

data1.ts = ts(data1, frequency = 1, start=c(1996), end=c(2022))
data1_holt <- holt(data1.ts, h=12)
summary(data1_holt)
fitted_values <- fitted(data1_holt)
accuracy_holt <- accuracy(data1_holt)
accuracy_holt
plot(data1.ts, col="black", main="Holt's method", ylab="Observed/Fitted", xlab="Time(Year)")
lines(fitted_values, col="red", lty=1)

forecast(data1_holt, h=12)
plot(forecast(data1_holt, h=12), main="Forecasts from Holt's Method", xlab="Time(Year)", ylab="Total Export Malaysia")
lines(data1_holt$fitted, col="red")
legend("topleft", legend=c("Observed Data", "Forecast"), col=c("black", "red"), lty=1)
