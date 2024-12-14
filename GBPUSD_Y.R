library(readxl)
GBPUSD <- read_excel("GBPUSD_X.xls",
                     col_types = c("date", "numeric", "numeric", "numeric"))
View(GBPUSD)
# Save the data as data.frame 
GBPUSD <- as.data.frame(GBPUSD)
summary(GBPUSD)
#Regression Model with trend
dim(GBPUSD)
trend <- seq_along(GBPUSD$Date)
trend

GBPUSD_trend <- lm(formula = Adj_Close ~ trend, data = GBPUSD)
summary(GBPUSD_trend)
#time trend is significant

#Making monetary value log 
GBPUSD$Adj_close_l = log(GBPUSD$Adj_Close)

#Plotting
plot(GBPUSD$Date, GBPUSD$Adj_close_l, type = "l", 
     ylab ="Price_l", xlab="Time")
plot(GBPUSD$Date, GBPUSD$Adj_Close, type = "l",
     ylab="Price", xlab="Time")
#Plot shows that my data is not stationary

library(tseries)
adf.test(GBPUSD$Adj_Close)
# The p value is 0.45,we fail to reject the, so  data is not stationary (has the unit root)

pp.test(GBPUSD$Adj_Close)
# The p value is 0.59 which means that we reject the Ho, so data is not stationary (has the unit root)

kpss.test(GBPUSD$Adj_Close)




#We get rid of non-stationarity by differencing 
Adj_Close1=diff(log(GBPUSD$Adj_Close))
I_UK1 = diff(log(GBPUSD$I_UK))
I_USA1 = diff(log(GBPUSD$I_USA))

adf.test(Adj_Close1)
#Now p-value is 0.01, we got rid of non-sta heavily
pp.test(Adj_Close1)
#Same thing
kpss.test(Adj_Close1)

plot.ts(Adj_Close1, ylab = "Exchange Rate", xlab = "Years, monthly", main = "GBPUSD")
#Now it is stationary as plot showed

View(GBPUSD)

#Creating an AR(2) Model

#Create the data with the 1st difference; 
# 1st difference eliminates the 1st value: to keep the row numbers unchanged, we add NA to the top

GBPUSD$Adj_Close1 <- c(NA,Adj_Close1)
GBPUSD$I_UK1 <- c(NA,I_UK1)
GBPUSD$I_USA1 <- c(NA,I_USA1)

#Creating the lags
library(seastests)
GBPUSD$Adj_Close.1_lag_1<- .Lag(GBPUSD$Adj_Close1, 1)
GBPUSD$Adj_Close.1_lag_2<- .Lag(GBPUSD$Adj_Close1, 2)
GBPUSD$I_USA.1_lag_1 <- .Lag(GBPUSD$I_USA1, 1)
GBPUSD$I_USA.1_lag_2 <- .Lag(GBPUSD$I_USA1, 2)
GBPUSD$I_UK.1_lag_1 <- .Lag(GBPUSD$I_UK1, 1)
GBPUSD$I_UK.1_lag_2 <- .Lag(GBPUSD$I_UK1, 2)

#We estimate the model
m1 <- lm(formula = Adj_Close1 ~ Adj_Close.1_lag_1, data = GBPUSD)  
summary(m1)
m2 <- lm(formula = Adj_Close1 ~ Adj_Close.1_lag_1+Adj_Close.1_lag_2, data = GBPUSD)  
summary(m2)
m3 = lm(formula = Adj_Close1 ~ Adj_Close.1_lag_1+Adj_Close.1_lag_2+I_USA.1_lag_1+I_USA.1_lag_2+I_UK.1_lag_1+I_UK.1_lag_2  , data = GBPUSD)  
summary(m3)

# Akaike Information Criteria
AIC(m1)
AIC(m2)
AIC(m3)
# Bayesian Information Criteria
BIC(m1)
BIC(m2)
BIC(m3)
#Summary shows that m1 model is most appropriate one



#Checking the autocorrelation in residuals
# Checking the Durbin Watson Test
# H0: no serial correlation, i.e., 𝜌 = 0
library(lmtest)
dwtest(m1)
#p-value is 0.53, so we fail to reject the Ho. There is no autocorrelation
bgtest(m1)
#p-value is 0.61,we fail to reject the null, no autocorrelation

acf(m1$residuals, main = "Correlogram, FDL Model")

Box.test(m1$residuals, lag = 1, type = "Box-Pierce")
#p-value = 0.91, we fail to reject null and there is no autocorrelation

Box.test(m1$residuals, lag = 1, type = "Ljung-Box")
#same, no autocorrelation




# Check for Heteroskedasticity

library(lmtest)
library(skedastic)

# Breusch-Pagan test
# Ho: homoskedasticity
# Ha: heteroskedasticity
bptest(m1)
#as we see there is no hetero in our data, it is homo

# White Test
white_lm(m1)
#no hetero again, it is homo



# Now we want to make a prediction for unknown future of 2019, 2020, 2021
# We use one-step-ahead forecast, where future values are used in prediction as they are available
# Make a one.step.ahead prediction with the new data that starts in 2018 quarter 4

library(forecast)
f_multi.stp.ahead <- forecast(m1, h=9, newdata = GBPUSD, level = 95)
f_multi.stp.ahead


fcast_m1 <- predict(m1, newdata = GBPUSD[120:128,], interval = "confidence", level = 0.95)
fcast_m1

# Model 1 & 2 Prediction Values together with Original Values

# Actual
plot(GBPUSD$Date[120:128], GBPUSD$Adj_Close1[120:128], type ="l",
     ylab = "log diff", xlab = "years,quarterly", main = "Exchange Rate")

# Model 2 prediction with red color
lines(GBPUSD$Date[120:128],fcast_m1[,1], type = "l",  lty =1, col = 2)

# confidence bands
lines(GBPUSD$Date[120:128],fcast_m1[,2], type = "l",  lty =2, col = 4)
lines(GBPUSD$Date[120:128],fcast_m1[,3], type = "l",  lty =2, col = 4)























