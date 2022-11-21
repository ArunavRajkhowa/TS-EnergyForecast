library(forecast)
library(fpp)
library(dplyr)

setwd("D:\\Coding_Projects\\TS-EnergyForecast\\")


energy=read.csv("train.csv")
test=read.csv('test.csv')


energy= energy %>%
  na.omit()
energy = energy %>%  select(c(-row_id,-datetime))

# converting to a time series data format
souvenirts=ts(energy,frequency=24,start=c(2008,3),end=c(2018,12))
plot(decompose(souvenirts))
plot(souvenirts)

#test for stationarity
library(tseries)
adf.test(souvenirts, alternative ="stationary", k=12)
# WE HAVE A SMALL p value so we can say we have stationary 

#souvenirts=log(souvenirts)
souvenirforecast=HoltWinters(souvenirts)

souvenirforecast

plot(souvenirforecast)
#ARIMA models have 3 parameters  and is generally written as ARIMA(p,d,q)
#######SARIMA################
auto.arima(souvenirts)
#OUTPUTS  
#ARIMA(2,0,2)(1,1,0)[24]
#p=2 ,d=0, q=2 | P=1, D=1 , Q=0 | m=24
arimafit=arima(souvenirts,order=c(2,0,2),seasonal=c(1,1,0))
#arimafit %>% 
 # forecast(h=48) %>% 
  #autoplot()+autolayer(test)

arimafuture=forecast:::forecast.Arima(arimafit,h=48) 
#write.csv(arimafit,"trial_001.csv")
plot(arimafuture,main='3 year Total Energy consumption Forecast')
test_pred=predict(arimafit,new_data = test)
adf.test(arimafuture$residuals, alternative ="stationary")
# From the above p-value, we can conclude that the residuals 
# of our ARIMA prediction model is stationary.
acf(arimafuture$residuals,lag.max=100) #lag 1 is good. after that acf becomes less
pacf(arimafuture$residuals,lag.max=100)
Box.test(arimafuture$residuals,lag = 5)





### Handling missing values and outliers in timeseries data

library(fpp)
#plot(melsyd[,3], main="Economy class passengers: Melbourne-Sydney")
plot(souvenirts)
# cleaned data

tsoutliers(souvenirts)

plot(souvenirts, main="after imputing missing values")
lines(tsclean(souvenirts), col='red')




