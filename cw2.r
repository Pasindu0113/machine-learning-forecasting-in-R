#getting the readxl library
library(readxl)
data <- read_excel("ExchangeUSD.xlsx")
data <- subset (data, select = -Wdy)
str(data)
summary(data)
View(data)
Date <- as.Date(data$Date)
#plotting the time series data
plot(data$Rate~Date,type="l",col="red")

library(quantmod)
library(neuralnet)
library(forecast)

#setting the dtaset as a time series dataset 
timeser <- xts(data[,2],order.by = as.Date(data$Date))

# timeser1 <- ts (data[,2])
# View(timeser1)

#splitting the dataset into two parts as training and testing
rtimesertraining1 <- window(timeser, end = '2012-12-31')
rtimesertesting1 <- window(timeser, start = '2012-12-31')

#finding optimal lag
library(vars)
VARselect(timeser)

#getting the time series data ready to be inputted to the neural network
rtimeser <- dailyReturn(timeser)
rtimeser1 <- lag(rtimeser, k = 1)
rtimeserall <- cbind(rtimeser,rtimeser1)
colnames(rtimeserall) <- c('rtimeser', 'rtimeser1')
rtimeserall <- na.exclude(rtimeserall)

rtimesertraining <- window(rtimeserall, end = '2012-12-31')
rtimesertesting <- window(rtimeserall, start = '2012-12-31')

#creating various kinds of neural networks and plotting them with the results matrix
anntimeser1 <- neuralnet(rtimeser~rtimeser1,data = rtimesertraining, hidden=1, act.fct = function(x){x})
anntimeser1$result.matrix
plot(anntimeser1)

anntimeser2 <- neuralnet(rtimeser~rtimeser1,data = rtimesertraining, hidden=c(3,2), act.fct = function(x){x})
anntimeser2$result.matrix
plot(anntimeser2)

anntimeser3 <- neuralnet(rtimeser~rtimeser1,data = rtimesertraining, hidden=3, act.fct = function(x){x})
anntimeser3$result.matrix
plot(anntimeser3)

anntimeser4 <- neuralnet(rtimeser~rtimeser1,data = rtimesertraining, hidden=c(3,2,1), act.fct = function(x){x})
anntimeser4$result.matrix
plot(anntimeser4)

anntimeser5 <- neuralnet(rtimeser~rtimeser1,data = rtimesertraining, hidden=c(4,3,2,1), act.fct = function(x){x})
anntimeser5$result.matrix
plot(anntimeser5)

#fitting the time series data to the neural network auto regression model
fit <- nnetar(window(rtimesertraining1, start = '2012-11-03'), lambda=0.5)
foretest <- length(rtimesertesting)

#forcasting for testing values
fcast <- forecast(fit, PI=TRUE, h=25)
autoplot(fcast)

# fcast %>%
#   autoplot() +
#   geom_line(
#     aes(
#       x = as.numeric(date$Date(rtimesertesting)),
#       y = as.numeric(rtimesertesting)
#     ),
#     col = "red"
#   )

#evaluating the forcasting process
accuracy(fcast, rtimesertesting1)


