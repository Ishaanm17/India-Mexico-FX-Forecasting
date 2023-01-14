library(fpp2)
library(quantmod)
library(tseries)
library(forecast)

### installing excel package
library(readxl)

## downloading Mexico UIP data
MexicoUIP <- read_excel('/Users/ishaansm/Desktop/FX Time Series Models /Mexico/MexicoUIP.xlsx')
MexicoUIP

MexicoUIP_top <- MexicoUIP[(length(MexicoUIP)):length(MexicoUIP)]


###plotting data
plot(MexicoUIP, type='l',
     xlab='date',
     ylab='UIP')
### Testing Stationary


##getting moving average
MexicoUIP_ma <- ma(MexicoUIP, 1)
plot(MexicoUIP_ma)


### Comparison White Noise
whiteNoise <- rnorm(100,sd=200)
plot(whiteNoise, type='l')
acf(whiteNoise)
acf(whiteNoise^2)


### creating GARCH and ARIMA models 
##downloading Garch library 
library(fGarch)


### creating ARIMA models
MexicoUIPARIMA <- auto.arima(MexicoUIP_top)

checkresiduals(MexicoUIPARIMA)


#### best ARIMA model, (0,1,0) with drift 

### ARIMA forecasting 
MexicoUIPforecast=forecast(MexicoUIPARIMA)
### graphing ARIMA plot 
autoplot(MexicoUIPforecast)

print(MexicoUIPforecast)


###Checking Residuals 
MexicoUIPtest <- auto.arima(MexicoUIP_top, trace=TRUE)
checkresiduals(MexicoUIPtest)
### The UIP dataset seems not to have a predictable pattern which suggest that the dataset is weakly white and makes the dataset worthy to use such ARIMA and GARCH models to forecast 


#### Plotting GARCH Model 

### finding best fit GARCH model 
garch_10 <- garchFit(~arma(0,0)+garch(1,0), data=MexicoUIP_top, trace=F)
garch_20 <- garchFit(~arma(0,0)+garch(2,0), data=MexicoUIP_top, trace=F)
garch_30 <- garchFit(~arma(0,0)+garch(3,0), data=MexicoUIP_top, trace=F)
garch_40 <- garchFit(~arma(0,0)+garch(4,0), data=MexicoUIP_top, trace=F)
garch_50 <- garchFit(~arma(0,0)+garch(5,0), data=MexicoUIP_top, trace=F)
garch_11 <- garchFit(~arma(0,0)+garch(1,1), data=MexicoUIP_top, trace=F)
garch_21 <- garchFit(~arma(0,0)+garch(2,1), data=MexicoUIP_top, trace=F)
garch_31 <- garchFit(~arma(0,0)+garch(3,1), data=MexicoUIP_top, trace=F)


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


MexicoUIPprediction <- predict(garch_10, n.ahead=100, plot=TRUE)

#getting results of the GARCH model
summary(garch_11)
print(prediction)


### looking at both arima and garch plots to make sure there is no overfitting  


MexicoUIPARIMA <- auto.arima(MexicoUIP_top)


MexicoUIParima_res <- MexicoUIPARIMA$residuals 
MexicoUIParima_garch_stdres <- residuals(garch_10, standardize=TRUE)

### Plotting the qq plots 
par(mfrow=c(1,2))
qqnorm(MexicoUIParima_res, main="1-1 Plot of ARMA")
qqline(arima_res)

qqnorm(MexicoUIParima_garch_stdres, main="Q-Q Plot of ARMA-GARCH")
qqline(MexicoUIParima_garch_stdres)

### Arima model is the best model to use 



##### downloading 5 year differential data 
Mexico5yr <- read_excel('/Users/ishaansm/Desktop/FX Time Series Models /Mexico/Mexico5yearBondDifferential.xlsx')
Mexico5yr

Mexico5yr_top <- Mexico2yr[(length(Mexico5yr)):length(Mexico5yr)]


###plotting data
plot(Mexico5yr, type='l',
     xlab='date',
     ylab='Mexico-US 5yr Bond Differential')



##getting moving average
Mexico2yr_ma <- ma(Mexico2yr, 1)
plot(Mexico2yr_ma)


### Comparison White Noise
whiteNoise <- rnorm(100,sd=200)
plot(whiteNoise, type='l')
acf(whiteNoise)
acf(whiteNoise^2)

## comparing residuals
acf(Mexico5yr)


### creating GARCH and ARIMA models 



### creating ARIMA models
Mexico5yrARIMA <- auto.arima(Mexico2yr_top)

checkresiduals(Mexico2yrARIMA)
## The 5yr dataset seems not to have a predictable pattern which suggest that the dataset is weakly white and makes the dataset worthy to use such ARIMA and GARCH models to forecast 


#### best ARIMA model, (0,1,1)

### ARIMA forecasting 
Mexico5yrforecast=forecast(arima(Mexico5yr_top, c(0,1,1)), h=100)
### graphing ARIMA plot 
autoplot(Mexico5yrforecast)

print(Mexico5yrforecast)


###Checking Residuals 
Mexico5yrtest <- auto.arima(Mexico5yr_top, trace=TRUE)
checkresiduals(Mexico5yrtest)

#### Plotting GARCH Model 

### finding best fit GARCH model 
garch_10 <- garchFit(~arma(0,0)+garch(1,0), data=Mexico5yr_top, trace=F)
garch_20 <- garchFit(~arma(0,0)+garch(2,0), data=Mexico5yr_top, trace=F)
garch_30 <- garchFit(~arma(0,0)+garch(3,0), data=Mexico5yr_top, trace=F)
garch_40 <- garchFit(~arma(0,0)+garch(4,0), data=Mexico5yr_top, trace=F)
garch_50 <- garchFit(~arma(0,0)+garch(5,0), data=Mexico5yr_top, trace=F)
garch_11 <- garchFit(~arma(0,0)+garch(1,1), data=Mexico5yr_top, trace=F)
garch_21 <- garchFit(~arma(0,0)+garch(2,1), data=Mexico5yr_top, trace=F)
garch_31 <- garchFit(~arma(0,0)+garch(3,1), data=Mexico5yr_top, trace=F)


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


Mexico5yrprediction <- predict(garch_11, n.ahead=100, plot=TRUE)
print(Mexico5yrprediction)

### looking at both arima and garch plots to make sure not overfitting 


Mexico5yrARIMA <- auto.arima(Mexico5yr_top)


Mexico5yrarima_res <- Mexico5yrARIMA$residuals 
Mexico5yrarima_garch_stdres <- residuals(garch_11, standardize=TRUE)

### Plotting the qq plots 
par(mfrow=c(1,2))
qqnorm(Mexico5yrarima_res, main="1-1 Plot of ARMA")
qqline(Mexico5yrarima_res)

qqnorm(Mexico2yrarima_garch_stdres, main="Q-Q Plot of ARMA-GARCH")
qqline(Mexico2yrarima_garch_stdres)
### ARIMA model is the best model, yet the GARCH model looks quite bit similar with regards to the qq plot 


###downloading 10yr data

Mexico10yr <- read_excel('/Users/ishaansm/Desktop/FX Time Series Models /Mexico/Mexico10yearBondDifferential.xlsx')
Mexico10yr

Mexico10yr_top <- Mexico10yr[(length(Mexico10yr)):length(Mexico10yr)]


###plotting data
plot(Mexico10yr, type='l',
     xlab='date',
     ylab='Mexico-US 10yr Bond Differential')


### creating ARIMA models
Mexico10yrARIMA <- auto.arima(Mexico10yr_top)

checkresiduals(Mexico10yrARIMA)
### The 10 yr dataset seems not to have a predictable pattern which suggest that the dataset is weakly white and makes the dataset worthy to use such ARIMA and GARCH models to forecast 


#### best ARIMA model, (5,1,4)

### ARIMA forecasting 
Mexico10yrforecast=forecast(arima(Mexico10yr_top, c(5,1,4)), h=100)
### graphing ARIMA plot 
autoplot(Mexico10yrforecast)

print(Mexico10yrforecast)


###Checking Residuals 
Mexico10yrtest <- auto.arima(Mexico10yr_top, trace=TRUE)
checkresiduals(Mexico10yrtest)
### The 10yr dataset seems not to have a predictable pattern which suggest that the dataset is weakly white and makes the dataset worthy to use such ARIMA and GARCH models to forecast 






#### Plotting GARCH Model 

### finding best fit GARCH model 
garch_10 <- garchFit(~arma(0,0)+garch(1,0), data=Mexico10yr_top, trace=F)
garch_20 <- garchFit(~arma(0,0)+garch(2,0), data=Mexico10yr_top, trace=F)
garch_30 <- garchFit(~arma(0,0)+garch(3,0), data=Mexico10yr_top, trace=F)
garch_40 <- garchFit(~arma(0,0)+garch(4,0), data=Mexico10yr_top, trace=F)
garch_50 <- garchFit(~arma(0,0)+garch(5,0), data=Mexico10yr_top, trace=F)
garch_11 <- garchFit(~arma(0,0)+garch(1,1), data=Mexico10yr_top, trace=F)
garch_21 <- garchFit(~arma(0,0)+garch(2,1), data=Mexico10yr_top, trace=F)
garch_31 <- garchFit(~arma(0,0)+garch(3,1), data=Mexico10yr_top, trace=F)


### Finding values of AIC model
garch_10@fit$ics['AIC']
garch_20@fit$ics['AIC']
garch_30@fit$ics['AIC']
garch_40@fit$ics['AIC']
garch_50@fit$ics['AIC']
garch_11@fit$ics['AIC']
garch_21@fit$ics['AIC']
garch_31@fit$ics['AIC']

#### garch_50 is the best model

### plotting GARCH model


Mexico10yrprediction <- predict(garch_50, n.ahead=30, plot=TRUE, fill=FALSE)
print(Mexico10yrprediction)

### looking at both arima and garch plots to make sure not overfitting 


Mexico10yrARIMA <- auto.arima(Mexico10yr_top)


Mexico10yrarima_res <- Mexico10yrARIMA$residuals 
Mexico10yrarima_garch_stdres <- residuals(garch_50, standardize=TRUE)

### Plotting the qq plots 
par(mfrow=c(1,2))
qqnorm(Mexico10yrarima_res, main="1-1 Plot of ARMA")
qqline(Mexico10yrarima_res)

qqnorm(Mexico10yrarima_garch_stdres, main="Q-Q Plot of ARMA-GARCH")
qqline(Mexico10yrarima_garch_stdres)

### Arima model is the best fit to forecast, yet only marginally as the GARCH qq plot resembles alot of resemblence to the ARIMA QQ plot 

