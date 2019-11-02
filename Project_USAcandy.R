install.packages("forecast")
library(forecast)
install.packages("ggplot2")
library(ggplot2)
install.packages("zoo")
library(zoo)

## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("C:/Users/ludai/Desktop/BAN673/Project")

# Create data frame.
Candy.data <- read.csv("USAcandy.csv")

summary(Candy.data)

# Time series
Candy.ts <- ts(Candy.data$Volume_mkg, 
                   start = c(1972, 1), end = c(2017, 8), freq = 12)

# Use Acf() function to identify autocorrealtion and plot autocorrrelation
# for different lags (up to maximum of 12).
Candy <- Acf(Candy.ts, lag.max = 12, main = "Autocorrelation for USA Candy")

# time series components
Candy.stl <- stl(Candy.ts, s.window = "periodic")
autoplot(Candy.stl, main = "Candy Time Series Component")

# Employ the plot() function to create a data plot with the historical data 
plot(Candy.ts, 
     xlab = "Time", ylab = "Production volume (mkg)", 
     ylim = c(100, 1000), main = "Time plot for USA Candy Production From Jan.1972~ Aug.2017", col = "blue")


# Develop data partition with the validation partition of 12 periods and training partition of 48 periods. 
nValid <- 100
nTrain <- length(Candy.ts) - nValid
candytrain.ts <- window(Candy.ts, start = c(1972, 1), end = c(1972, nTrain))
candyvalid.ts <- window(Candy.ts, start = c(1972, nTrain + 1), 
                        end = c(1972, nTrain + nValid))


# Create Holt-Winter's exponenthial smoothing (HW) for candy training data.
# Use ets() function with model = "ZZZ"
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw.ZZZ <- ets(candytrain.ts, model = "ZZZ")
hw.ZZZ 


# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
# Show predictions 
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

## FORECAST WITH HOLT-WINTER'S MODEL USING ENTIRE DATA SET INTO
## the FUTURE FOR 12 PERIODS.

# Create Holt-Winter's exponenthial smoothing (hW) for full candy dataset. 
# Use ets() function with model = "ZZZ", to identify the best hw option
# and optimal alpha, beta, & gamma to fit hw for the entire data period.
HW.ZZZ <- ets(Candy.ts, model = "ZZZ")
HW.ZZZ 

# Use forecast() function to make predictions using this hw model for
# 12 month into the futurre.
# length <- length(ridership.ts)
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 0)
HW.ZZZ.pred


# Use auto.arima() function to fit ARIMA model.
# use summary() to show auto ARIMA model and its parameters for entire dataset.
auto.arima <- auto.arima(Candy.ts)
summary(auto.arima)



# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 12 periods. 
auto.arima.pred <- forecast(auto.arima, h = 12, level = 0)
auto.arima.pred
