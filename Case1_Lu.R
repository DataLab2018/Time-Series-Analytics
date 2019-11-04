install.packages("forecast")
library(forecast)
library(zoo)
setwd("C:/Users/Desktop/BAN673/case#1")
Grocery<- read.csv("673_case1_1.csv")
head(Grocery)
tail(Grocery)

#Q1
#a) USE ts() FUNCTION TO CREATE TIME SERIES DATA SET.
# With monthly data, frequency (freq) of periods in a season is 12. 
Grocery.ts <- ts(Grocery$Sales, 
                   start = c(2014,1), end = c(2018, 12), freq = 12)
ets(Grocery.ts)
#b)

# Use acf() function to identify autocorrealtion and plot autocorrrelation

autocor <- Acf(Grocery.ts, lag.max = 12, main = "Autocorrelation for Grocery")
# Display autocorrelatiion coefficients for varous lags
Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)

##c) Use plot() to plot time series data  
plot(Grocery.ts, 
     xlab = "Time", ylab = "Sales (in $)", 
     ylim = c(100, 500), main = "Grocery Sales", col = "blue")
Grocery.lin <- tslm(Grocery.ts ~ trend)
Grocery.quad <- tslm(Grocery.ts ~ trend + I(trend^2))
lines(Grocery.lin$fitted, lwd = 2)
lines(Grocery.quad$fitted, lwd = 2)


#Q2
#a)
# Fit a regression model with linear trend and seasonality.
reg.trend.seas <- tslm(Grocery.ts ~ trend  + season)
summary(reg.trend.seas)
reg.trend.seas1 <- tslm(Grocery.ts ~ trend + I(trend^2) + season)
summary(reg.trend.seas1)#not significant
# Create forecast for the 12 periods into the future.
reg.trend.seas.pred <- forecast(reg.trend.seas, h = 12, level = 0)
reg.trend.seas.pred

#b)

# Identify and display residulas for time series based on the regression
# (differences between actual and regression values in the same periods)
reg.trend.seas.res <- reg.trend.seas$residuals
reg.trend.seas.res
# Apply trailing MA with 12 periods in the window to residuals.
ma.trailing.res_12 <- rollmean(reg.trend.seas.res, k = 12, align = "right")
ma.trailing.res_12
# Create forecast for residuals for the 12 periods into the future.
ma.trailing.res_12.pred <- forecast(ma.trailing.res_12, h = 12, level = 0)
ma.trailing.res_12.pred
# To develop real forecast for 12 periods into the future, 
# combine regression forecast and trailing MA forecast for residuals.
ts.forecast.12 <- reg.trend.seas.pred$mean + ma.trailing.res_12.pred$mean
ts.forecast.12
# Create a table with regression forecast, trailing MA for residuals
# and total forecast for 12 months into the future
total.reg.ma.pred <- data.frame(reg.trend.seas.pred$mean, ma.trailing.res_12.pred$mean, 
                                ts.forecast.12)
total.reg.ma.pred

#c)
# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
Grocery.snaive<- snaive(Grocery.ts, h = 12,level = 0)
Grocery.snaive
round(accuracy(Grocery.snaive$fitted, Grocery.ts), 3)
round(accuracy(reg.trend.seas.pred$fitted, Grocery.ts), 3)
round(accuracy(reg.trend.seas.pred$fitted+ma.trailing.res_12, Grocery.ts), 3)

#Q3
#a)
Grocery.ts <- ts(Grocery$Sales, 
                 start = c(2014,1), end = c(2018, 12), freq = 12)

nValid <- 12
nTrain <- length(Grocery.ts) - nValid
train.ts <- window(Grocery.ts, start = c(2014, 1), end = c(2014, nTrain))
valid.ts <- window(Grocery.ts, start = c(2014, nTrain + 1), 
                   end = c(2014, nTrain + nValid))
#b)
hw.MMM <- ets(train.ts, model = "MMM")
hw.MMM 
hw.MMM.pred <- forecast(hw.MMM, h = nValid, level = 0)
hw.MMM.pred

#c)
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ 
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

#d)
HW.ZZZ <- ets(Grocery.ts, model = "ZZZ")
HW.ZZZ
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 0)
HW.ZZZ.pred

#e)
#compare two forecasts: seasonal naive forecast 
#and Holt-Winter
round(accuracy(HW.ZZZ.pred$fitted, Grocery.ts),3)
round(accuracy(Grocery.snaive$fitted, Grocery.ts), 3)

#f)
round(accuracy(HW.ZZZ.pred$fitted, Grocery.ts),3)
round(accuracy(reg.trend.seas.pred$fitted+ma.trailing.res_12, Grocery.ts), 3)


