install.packages("forecast")
library(forecast)
library(zoo)
setwd("C:/Users/Desktop/BAN673/case#1")
Grocery<- read.csv("673_case1_1.csv")
head(Grocery)
tail(Grocery)
Grocery.ts <- ts(Grocery$Sales, 
               start = c(2014,1), end = c(2018, 12), freq = 12)
#Develop the data partition 
nValid <- 12
nTrain <- length(Grocery.ts) - nValid
train.ts <- window(Grocery.ts, start = c(2014, 1), end = c(2014, nTrain))
valid.ts <- window(Grocery.ts, start = c(2014, nTrain + 1), 
                   end = c(2014, nTrain + nValid))
#Q1
#a)
ar1 <- Arima(Grocery.ts, order = c(1,0,0))
summary(ar1)

confint(ar1,level=0.95)
#b)
# Create differenced data using (lag-1)
diff.Grocery.ts <- diff(Grocery.ts, lag = 1)
diff.Grocery.ts

# Use Acf() function to identify autocorrealtion for the model 
# and plot autocorrrelation for different lags (up to maximum of 12).
Acf(diff.Grocery.ts, lag.max = 12, 
    main = "Autocorrelation for Differenced Grocery Sale Data")

#Q2
#a)
# Use tslm() function to create quadratic trend and seasonal model.
train.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(train.trend.season)
# Apply forecast() function to make predictions for ts with 
# trend and seasonal model in validation set.  
train.trend.season.pred <- forecast(train.trend.season, h = nValid, level = 0)
train.trend.season.pred

#b)
# Use Acf() function to identify autocorrealtion for the model residuals 

Acf(train.trend.season.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for Grocery Training Residuals")

#c)
## USE Arima() FUNCTION TO CREATE AR(1) MODEL FOR THE REGRESSION RESIDUALS.

res.ar1 <- Arima(train.trend.season$residuals, order = c(1,0,0))
summary(res.ar1)

# Use Acf() function to identify autocorrealtion for the training 
# residual of residuals and plot autocorrrelation for different lags 
# (up to maximum of 12).
Acf(res.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Grocery Training Residuals of Residuals")
#d)

# Create two-level modeling results, regression + AR(1) for validation period.
# Create data table with historical validation data, regression forecast
# for validation period, AR(1) for validation, and and two level model results. 
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
valid.two.level.pred <- train.trend.season.pred$mean + res.ar1.pred$mean

valid.df <- data.frame(valid.ts, train.trend.season.pred$mean, 
                       res.ar1.pred$mean, valid.two.level.pred)
names(valid.df) <- c("Grocery", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df

#e)
## FIT REGRESSION MODEL WITH linear TREND AND SEASONALITY 
## FOR ENTIRE DATASET. FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create linear trend and seasonality model.
trend.season <- tslm(Grocery.ts ~ trend + season)

# See summary of linear trend equation and asociated parameters.
summary(trend.season)

# Apply forecast() function to make predictions with linear trend and seasonal 
# model into the future 12 months.  
trend.season.pred <- forecast(trend.season, h = 12, level = 0)
trend.season.pred


# Use Arima() function to fit AR(1) model for regression residulas.
# The ARIMA model order of order = c(1,0,0) gives an AR(1) model.
# Use forecast() function to make prediction of residuals into the future 12 months.
residual.ar1 <- Arima(trend.season$residuals, order = c(1,0,0))
residual.ar1.pred <- forecast(residual.ar1, h = 12, level = 0)


# Use summary() to identify parameters of AR(1) model.
summary(residual.ar1)

# Use Acf() function to identify autocorrealtion for the residual of residuals 
# and plot autocorrrelation for different lags (up to maximum of 12).
Acf(residual.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Grocery Residuals of Residuals for Entire Data Set")


# Identify forecast for the future 12 periods as sum of linear trend and seasonal model
# and AR(1) model for residuals.
trend.season.ar1.pred <- trend.season.pred$mean + residual.ar1.pred$mean
trend.season.ar1.pred


# Create a data table with linear trend and seasonal forecast for 12 future periods,
# AR(1) model for residuals for 12 future periods, and combined two-level forecast for
# 12 future periods. 
table.df <- data.frame(trend.season.pred$mean, 
                       residual.ar1.pred$mean, trend.season.ar1.pred)
names(table.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
table.df

#Q3
#a)
## FIT AUTO ARIMA MODEL FOR ENTIRE DATASET. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use auto.arima() function to fit ARIMA model.
# use summary() to show auto ARIMA model and its parameters for entire dataset.
auto.arima <- auto.arima(Grocery.ts)
summary(auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 12 periods. 
auto.arima.pred <- forecast(auto.arima, h = 12, level = 0)
auto.arima.pred

#b)
round(accuracy(trend.season.pred$fitted, Grocery.ts), 3)

round(accuracy(trend.season.pred$fitted + residual.ar1.pred$fitted, Grocery.ts), 3)
round(accuracy(auto.arima.pred$fitted, Grocery.ts), 3)

round(accuracy(HW.ZZZ.pred$fitted, Grocery.ts),3) #case1
round(accuracy((snaive(Grocery.ts))$fitted, Grocery.ts), 3)
