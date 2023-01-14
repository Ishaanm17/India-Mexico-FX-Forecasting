library(fpp2)
library(quantmod)
library(tseries)


### installing excel package
library(readxl)

##downloading Garch library 
library(fGarch)



## downloading India UIP data
IndiaUIP <- read_excel('/Users/ishaansm/Desktop/FX Time Series Models /India/IndiaUIP.xlsx')
IndiaUIP

#### making the dataframe to use to create ARIMA and GARCH models 
IndiaUIP_top <- IndiaUIP[(length(IndiaUIP)):length(IndiaUIP)]
IndiaUIP_top

###plotting data
plot(IndiaUIP, type='l',
     xlab='date',
     ylab='UIP')

### Testing Stationary


##getting moving average
IndiaUIP_ma <- ma(IndiaUIP, 1)
plot(IndiaUIP_ma)
#### Model shows time series is moving in an upward trend 


### Comparison White Noise
whiteNoise <- rnorm(100,sd=200)
plot(whiteNoise, type='l')
acf(whiteNoise)
acf(whiteNoise^2)


### creating GARCH and ARIMA models 


### creating ARIMA models
IndiaUIPARIMA <- auto.arima(IndiaUIP_top)

checkresiduals(IndiaUIPARIMA)
### India UIP dataset seems not to have a predictable pattern which suggest that the dataset is weakly white and makes the dataset worthy to use such ARIMA and GARCH models to forecast 

#### best ARIMA model, (1,1,1) with drift 


### ARIMA forecasting 
IndiaUIPforecast=forecast(IndiaUIPARIMA)

autoplot(IndiaUIPforecast1)
### graphing ARIMA plot 
autoplot(IndiaUIPforecast)

print(IndiaUIPforecast)


###Checking Residuals 
IndiaUIPtest <- auto.arima(IndiaUIP_top, trace=TRUE)
checkresiduals(IndiaUIPtest)


#### Plotting GARCH Model 

### finding best fit GARCH model 
garch_10 <- garchFit(~arma(0,0)+garch(1,0), data=IndiaUIP_top, trace=F)
garch_20 <- garchFit(~arma(0,0)+garch(2,0), data=IndiaUIP_top, trace=F)
garch_30 <- garchFit(~arma(0,0)+garch(3,0), data=IndiaUIP_top, trace=F)
garch_40 <- garchFit(~arma(0,0)+garch(4,0), data=IndiaUIP_top, trace=F)
garch_50 <- garchFit(~arma(0,0)+garch(5,0), data=IndiaUIP_top, trace=F)
garch_11 <- garchFit(~arma(0,0)+garch(1,1), data=IndiaUIP_top, trace=F)
garch_21 <- garchFit(~arma(0,0)+garch(2,1), data=IndiaUIP_top, trace=F)
garch_31 <- garchFit(~arma(0,0)+garch(3,1), data=IndiaUIP_top, trace=F)


### Finding values of AIC model
garch_10@fit$ics['AIC']
garch_20@fit$ics['AIC']
garch_30@fit$ics['AIC']
garch_40@fit$ics['AIC']
garch_50@fit$ics['AIC']
garch_11@fit$ics['AIC']
garch_21@fit$ics['AIC']
garch_31@fit$ics['AIC']

#### garch_11 is the best model

### plotting GARCH model

IndiaUIPprediction <- predict(garch_11, n.ahead=100, plot=TRUE)

#getting results of the GARCH model
summary(garch_11)
print(prediction)


### looking at both arima and garch plots to make sure there is no overfitting  


IndiaUIPARIMA <- auto.arima(IndiaUIP_top)


IndiaUIParima_res <- IndiaUIPARIMA$residuals 
IndiaUIParima_garch_stdres <- residuals(garch_11, standardize=TRUE)

### Plotting the qq plots 
par(mfrow=c(1,2))
qqnorm(IndiaUIParima_res, main="1-1 Plot of ARMA")
qqline(arima_res)

qqnorm(IndiaUIParima_garch_stdres, main="Q-Q Plot of ARMA-GARCH")
qqline(IndiaUIParima_garch_stdres)





##### downloading 2 year differential data 
India2yr <- read_excel('/Users/ishaansm/Desktop/FX Time Series Models /India/India2yBondDifferential.xlsx')
India2yr

India2yr_top <- India2yr[(length(India2yr)):length(India2yr)]


###plotting data
plot(India2yr, type='l',
     xlab='date',
     ylab='India-US 2yr Bond Differential')



##getting moving average
India2yr_ma <- ma(India2yr, 1)
plot(India2yr_ma)
#### moving in an upward trend


### Comparison White Noise
whiteNoise <- rnorm(100,sd=200)
plot(whiteNoise, type='l')
acf(whiteNoise)
acf(whiteNoise^2)

## comparing residuals
acf(India2yr)


### creating GARCH and ARIMA models 



### creating ARIMA models
India2yrARIMA <- auto.arima(India2yr_top)

checkresiduals(India2yrARIMA)
###### The 2yr model 

#### best ARIMA model, (0,1,1)

### ARIMA forecasting 
India2yrforecast=forecast(arima(India2yr_top, c(0,1,1)), h=100)
### graphing ARIMA plot 
autoplot(India2yrforecast)

print(India2yrforecast)


###Checking Residuals 
India2yrtest <- auto.arima(India2yr_top, trace=TRUE)
checkresiduals(India2yrtest)
### the 2yr dataset seems not to have a predictable pattern which suggest that the dataset is weakly white and makes the dataset worthy to use such ARIMA and GARCH models to forecast 


#### Plotting GARCH Model 

### finding best fit GARCH model 
garch_10 <- garchFit(~arma(0,0)+garch(1,0), data=India2yr_top, trace=F)
garch_20 <- garchFit(~arma(0,0)+garch(2,0), data=India2yr_top, trace=F)
garch_30 <- garchFit(~arma(0,0)+garch(3,0), data=India2yr_top, trace=F)
garch_40 <- garchFit(~arma(0,0)+garch(4,0), data=India2yr_top, trace=F)
garch_50 <- garchFit(~arma(0,0)+garch(5,0), data=India2yr_top, trace=F)
garch_11 <- garchFit(~arma(0,0)+garch(1,1), data=India2yr_top, trace=F)
garch_21 <- garchFit(~arma(0,0)+garch(2,1), data=India2yr_top, trace=F)
garch_31 <- garchFit(~arma(0,0)+garch(3,1), data=India2yr_top, trace=F)


### Finding values of AIC model
garch_10@fit$ics['AIC']
garch_20@fit$ics['AIC']
garch_30@fit$ics['AIC']
garch_40@fit$ics['AIC']
garch_50@fit$ics['AIC']
garch_11@fit$ics['AIC']
garch_21@fit$ics['AIC']
garch_31@fit$ics['AIC']

#### garch_20 is the best model

### plotting GARCH model


India2yrprediction <- predict(garch_20, n.ahead=100, plot=TRUE)
print(India2yrprediction)

### looking at both arima and garch plots to make sure not overfitting 


India2yrARIMA <- auto.arima(India2yr_top)


India2yrarima_res <- India2yrARIMA$residuals 
India2yrarima_garch_stdres <- residuals(garch_20, standardize=TRUE)

### Plotting the qq plots 
par(mfrow=c(1,2))
qqnorm(India2yrarima_res, main="1-1 Plot of ARMA")
qqline(India2yrarima_res)

qqnorm(India2yrarima_garch_stdres, main="Q-Q Plot of ARMA-GARCH")
qqline(India2yrarima_garch_stdres)



##### downloading 5yr differential 

India5yr <- read_excel('/Users/ishaansm/Desktop/FX Time Series Models /India/India5yBondDifferential.xlsx')
India5yr

India5yr_top <- India5yr[(length(India5yr)):length(India5yr)]


###plotting data
plot(India5yr, type='l',
     xlab='date',
     ylab='India-US 5yr Bond Differential')

### creating ARIMA models
India5yrARIMA <- auto.arima(India5yr_top)

checkresiduals(India5yrARIMA)
### The 5yr dataset seems not to have a predictable pattern which suggest that the dataset is weakly white and makes the dataset worthy to use such ARIMA and GARCH models to forecast 


#### best ARIMA model, (0,1,0)

### ARIMA forecasting 
India5yrforecast=forecast(arima(India5yr_top, c(0,1,0)), h=100)
### graphing ARIMA plot 
autoplot(India5yrforecast)

print(India5yrforecast)


###Checking Residuals 
India5yrtest <- auto.arima(India5yr_top, trace=TRUE)
checkresiduals(India5yrtest)



#### Plotting GARCH Model 

### finding best fit GARCH model 
garch_10 <- garchFit(~arma(0,0)+garch(1,0), data=India5yr_top, trace=F)
garch_20 <- garchFit(~arma(0,0)+garch(2,0), data=India5yr_top, trace=F)
garch_30 <- garchFit(~arma(0,0)+garch(3,0), data=India5yr_top, trace=F)
garch_40 <- garchFit(~arma(0,0)+garch(4,0), data=India5yr_top, trace=F)
garch_50 <- garchFit(~arma(0,0)+garch(5,0), data=India5yr_top, trace=F)
garch_11 <- garchFit(~arma(0,0)+garch(1,1), data=India5yr_top, trace=F)
garch_21 <- garchFit(~arma(0,0)+garch(2,1), data=India5yr_top, trace=F)
garch_31 <- garchFit(~arma(0,0)+garch(3,1), data=India5yr_top, trace=F)


### Finding values of AIC model
garch_10@fit$ics['AIC']
garch_20@fit$ics['AIC']
garch_30@fit$ics['AIC']
garch_40@fit$ics['AIC']
garch_50@fit$ics['AIC']
garch_11@fit$ics['AIC']
garch_21@fit$ics['AIC']
garch_31@fit$ics['AIC']

#### garch_10 is the best model

### plotting GARCH model


India5yrprediction <- predict(garch_10, n.ahead=100, plot=TRUE, legend.position='top')
print(prediction)


### looking at both arima and garch plots to make sure not overfitting 


India5yrARIMA <- auto.arima(India5yr_top)


India5yrarima_res <- India5yrARIMA$residuals 
India5yrarima_garch_stdres <- residuals(garch_10, standardize=TRUE)

### Plotting the qq plots 
par(mfrow=c(1,2))
qqnorm(arima_res, main="1-1 Plot of ARMA")
qqline(arima_res)

qqnorm(India5yrarima_garch_stdres, main="Q-Q Plot of ARMA-GARCH")
qqline(India5yrarima_garch_stdres)




###downloading 10yr data

India10yr <- read_excel('/Users/ishaansm/Desktop/FX Time Series Models /India/India10yBondDifferential.xlsx')
India10yr

India10yr_top <- India10yr[(length(India10yr)):length(India10yr)]


###plotting data
plot(India10yr, type='l',
     xlab='date',
     ylab='India-US 10yr Bond Differential')


### creating ARIMA models
India10yrARIMA <- auto.arima(India10yr_top)

checkresiduals(India10yrARIMA)
### The 10yr dataset seems not to have a predictable pattern which suggest that the dataset is weakly white and makes the dataset worthy to use such ARIMA and GARCH models to forecast 


#### best ARIMA model, (0,1,2)

### ARIMA forecasting 
India10yrforecast=forecast(arima(India10yr_top, c(0,1,2)), h=100)
### graphing ARIMA plot 
autoplot(India10yrforecast)

print(India10yrforecast)


###Checking Residuals 
India10yrtest <- auto.arima(India10yr_top, trace=TRUE)
checkresiduals(India10yrtest)






#### Plotting GARCH Model 

### finding best fit GARCH model 
garch_10 <- garchFit(~arma(0,0)+garch(1,0), data=India10yr_top, trace=F)
garch_20 <- garchFit(~arma(0,0)+garch(2,0), data=India10yr_top, trace=F)
garch_30 <- garchFit(~arma(0,0)+garch(3,0), data=India10yr_top, trace=F)
garch_40 <- garchFit(~arma(0,0)+garch(4,0), data=India10yr_top, trace=F)
garch_50 <- garchFit(~arma(0,0)+garch(5,0), data=India10yr_top, trace=F)
garch_11 <- garchFit(~arma(0,0)+garch(1,1), data=India10yr_top, trace=F)
garch_21 <- garchFit(~arma(0,0)+garch(2,1), data=India10yr_top, trace=F)
garch_31 <- garchFit(~arma(0,0)+garch(3,1), data=India10yr_top, trace=F)


### Finding values of AIC model
garch_10@fit$ics['AIC']
garch_20@fit$ics['AIC']
garch_30@fit$ics['AIC']
garch_40@fit$ics['AIC']
garch_50@fit$ics['AIC']
garch_11@fit$ics['AIC']
garch_21@fit$ics['AIC']
garch_31@fit$ics['AIC']

#### garch_10 is the best model

### plotting GARCH model


India10yrprediction <- predict(garch_10, n.ahead=100, plot=TRUE)
print(India10yrprediction)

### looking at both arima and garch plots to make sure not overfitting 


India10yrARIMA <- auto.arima(India10yr_top)


India10yrarima_res <- India10yrARIMA$residuals 
India10yrarima_garch_stdres <- residuals(garch_10, standardize=TRUE)

### Plotting the qq plots 
par(mfrow=c(1,2))
qqnorm(arima_res, main="1-1 Plot of ARMA")
qqline(arima_res)

qqnorm(arima_garch_stdres, main="Q-Q Plot of ARMA-GARCH")
qqline(arima_garch_stdres)

### Arima model is the best fit to forecast 

