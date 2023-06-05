# loading packages "TSA" for rstandard function and "itsmr" for test function
library(TSA)
library(itsmr)

### ----- Determination of d -----

# reading in so2 data
so2_original <- read.table("so2.txt")

# mean correcting original so2 data
so2 <- so2_original - mean(so2_original$V1)

# plotting mean corrected so2 data
plot(so2$V1, type="o", xlab ="Time", ylab ="Atmospheric Sulfur Dioxide Levels",
     main = "Mean Corrected so2 Data")
# doesn't look stationary:
# - variance seems to be decreasing over time 
# - looks like there is a downward linear trend 

# Computing least squares estimate for cubic trend model
time=1:508
time2 = time^2
time3 = time^3
cubicfit = lm(so2$V1~time+time2+time3)
# Coefficients:
# (Intercept)         time        time2        time3  
#   6.717e-01    2.206e-04   -1.329e-05    1.274e-08  

# Plotting fit with original so2 data
plot(so2$V1,type="o",xlab="Time",ylab="Atmospheric Sulfur Dioxide Levels",
     main="Mean Corrected so2 Data with Estimated Cubic Trend Model")
lines(fitted(cubicfit),col="red")

# plotting the mean corrected differencecd so2 data
so2_diff <- diff(so2$V1)
plot(so2_diff, type="o", xlab ="Time", ylab ="Atmospheric Sulfur Dioxide Levels",
     main = "Mean Corrected Differenced so2 Data")


### ----- Determination of p and q for the mean corrected differenced data -----

# We plot the sample acf/pacf for the mean corrected differenced data
acf1 <- acf(so2_diff, lag.max=40, main="Sample ACF for Mean Corrected Differenced so2 Data")
pacf1 <- pacf(so2_diff, lag.max=40, main="Sample PACF for Mean Corrected Differenced so2 Data")
# suggests cut off at lag 4 or even lag 3 in the sample ACF
# sort of looks like exponential decay in the sample PACF 
# Suggests using an MA(4) or M(3) model 
# so we could start with an ARMA(p,4) or ARMA(p,3)

# Using MLE to fit an ARMA models to the mean corrected differenced so2 data
# for q = 2
armafit <- arima(so2_diff,order=c(3,0,2),method="ML")
armafit

armafit42 <- arima(so2_diff,order=c(4,0,2),method="ML") # aic = 1305.18
armafit42

armafit <- arima(so2_diff,order=c(5,0,2),method="ML")
armafit

# for q = 3

armafit23 <- arima(so2_diff,order=c(2,0,3),method="ML") # aic = 1319.47
armafit23

armafit <- arima(so2_diff,order=c(3,0,3),method="ML")
armafit

armafit43 <- arima(so2_diff,order=c(4,0,3),method="ML") #aic = 1307.03
armafit43

armafit <- arima(so2_diff,order=c(5,0,3),method="ML")
armafit

# for q = 4
armafit <- arima(so2_diff,order=c(2,0,4),method="ML")
armafit

armafit34 <- arima(so2_diff,order=c(3,0,4),method="ML") # aic = 1308.13 
armafit34

armafit <- arima(so2_diff,order=c(4,0,4),method="ML") 
armafit

armafit <- arima(so2_diff,order=c(5,0,4),method="ML")
armafit

armafit64 <- arima(so2_diff,order=c(6,0,4),method="ML") # aic = 1304.13
armafit64

armafit <- arima(so2_diff,order=c(7,0,4),method="ML") 
armafit

acf2 <- acf(so2_diff, lag.max=40, col="green", main="Sample ACF and ARMA(4,2) Model ACF")
lines(x=0:40, ARMAacf(ar=c(0.6954,0.1130,-0.1096,0.1320),
                      ma=c(-1.5940,0.594),lag.max=40),type="h", col="red")
pacf2 <- pacf(so2_diff,lag.max=40,col="green", main="Sample PACF and ARMA(4,2) Model PACF")
lines(ARMAacf(ar=c(0.6954,0.1130,-0.1096,0.1320),
              ma=c(-1.5940, 0.594),lag.max=40,pacf=TRUE),type="h",col="red")


# plotting the model ACF/PACF in red and sample ACF/PACF in green 
acf2 <- acf(so2_diff, lag.max=40, col="green", main="Sample ACF and ARMA(2,3) Model ACF")
lines(x=0:40, ARMAacf(ar=c(-1.2483,-0.7584),
                      ma=c(0.3932,-0.2266,-0.6816),lag.max=40),type="h", col="red")
pacf2 <- pacf(so2_diff,lag.max=40,col="green", main="Sample PACF and ARMA(2,3) Model PACF")
lines(ARMAacf(ar=c(-1.2483,-0.7584),
              ma=c(0.3932,-0.2266,-0.6816),lag.max=40,pacf=TRUE),type="h",col="red")


acf2 <- acf(so2_diff, lag.max=40, col="green", main="Sample ACF and ARMA(3,3) Model ACF")
lines(x=0:40, ARMAacf(ar=c(-1.2448,-0.7597,-0.0041),
                      ma=c(0.3883,-0.2227,-0.6783),lag.max=40),type="h", col="red")
pacf2 <- pacf(so2_diff,lag.max=40,col="green", main="Sample PACF and ARMA(3,3) Model PACF")
lines(ARMAacf(ar=c(-1.2448,-0.7597,-0.0041),
              ma=c(0.3883,-0.2227,-0.6783),lag.max=40,pacf=TRUE),type="h",col="red")


acf2 <- acf(so2_diff, lag.max=40, col="green", main="Sample ACF and ARMA(4,3) Model ACF")
lines(x=0:40, ARMAacf(ar=c(0.8421,-0.0164,-0.1255,0.1434),
                      ma=c(-1.7430,0.8604,-0.1175),lag.max=40),type="h", col="red")
pacf2 <- pacf(so2_diff,lag.max=40,col="green", main="Sample PACF and ARMA(4,3) Model PACF")
lines(ARMAacf(ar=c(0.8421,-0.0164,-0.1255,0.1434),
              ma=c(-1.7430,0.8604,-0.1175),lag.max=40,pacf=TRUE),type="h",col="red")


acf2 <- acf(so2_diff, lag.max=40, col="green", main="Sample ACF and ARMA(5,3) Model ACF")
lines(x=0:40, ARMAacf(ar=c(0.0342,0.5694,-0.0348,0.0594,0.089),
                      ma=c(-0.933,-0.4561,0.3892),lag.max=40),type="h", col="red")
pacf2 <- pacf(so2_diff,lag.max=40,col="green", main="Sample PACF and ARMA(5,3) Model PACF")
lines(ARMAacf(ar=c(0.0342,0.5694,-0.0348,0.0594,0.089),
              ma=c(-0.933,-0.4561,0.3892),lag.max=40,pacf=TRUE),type="h",col="red")


acf2 <- acf(so2_diff, lag.max=40, col="green", main="Sample ACF and ARMA(2,4) Model ACF")
lines(x=0:40, ARMAacf(ar=c(-1.1630,-0.6932),
                      ma=c(0.3059,-0.2182,-0.6379,0.0134),lag.max=40),type="h", col="red")
pacf2 <- pacf(so2_diff,lag.max=40,col="green", main="Sample PACF and ARMA(2,4) Model PACF")
lines(ARMAacf(ar=c(-1.1630,-0.6932),
              ma=c(0.3059,-0.2182,-0.6379,0.0134),lag.max=40,pacf=TRUE),type="h",col="red")


acf2 <- acf(so2_diff, lag.max=40, col="green", main="Sample ACF and ARMA(3,4) Model ACF")
lines(x=0:40, ARMAacf(ar=c(-0.4446,0.3334,0.7566),
                      ma=c(-0.4292,-0.6574,-0.5671,0.6538),lag.max=40),type="h", col="red")
pacf2 <- pacf(so2_diff,lag.max=40,col="green", main="Sample PACF and ARMA(3,4) Model PACF")
lines(ARMAacf(ar=c(-0.4446,0.3334,0.7566),
              ma=c(-0.4292,-0.6574,-0.5671,0.6538),lag.max=40,pacf=TRUE),type="h",col="red")


acf2 <- acf(so2_diff, lag.max=40, col="green", main="Sample ACF and ARMA(4,4) Model ACF")
lines(x=0:40, ARMAacf(ar=c(-0.3403,-0.726,-0.4911,0.073),
                      ma=c(-0.5164,0.5263,-0.2472,-0.3616),lag.max=40),type="h", col="red")
pacf2 <- pacf(so2_diff,lag.max=40,col="green", main="Sample PACF and ARMA(4,4) Model PACF")
lines(ARMAacf(ar=c(-0.3403,-0.726,-0.4911,0.073),
              ma=c(-0.5164,0.5263,-0.2472,-0.3616),lag.max=40,pacf=TRUE),type="h",col="red")


acf2 <- acf(so2_diff, lag.max=40, col="green", main="Sample ACF and ARMA(5,4) Model ACF")
lines(x=0:40, ARMAacf(ar=c(0.767,0.6566,-0.7227,0.0505,0.1055),
                      ma=c(-1.6788,0.0957,1.1819,-0.5988),lag.max=40),type="h", col="red")
pacf2 <- pacf(so2_diff,lag.max=40,col="green", main="Sample PACF and ARMA(5,4) Model PACF")
lines(ARMAacf(ar=c(0.767,0.6566,-0.7227,0.0505,0.1055),
              ma=c(-1.6788,0.0957,1.1819,-0.5988),lag.max=40,pacf=TRUE),type="h",col="red")


acf2 <- acf(so2_diff, lag.max=40, col="green", main="Sample ACF and ARMA(6,4) Model ACF")
lines(x=0:40, ARMAacf(ar=c(-0.7792,0.3541,0.7790,0.0049,0.083,0.1178),
                      ma=c(-0.0994,-0.9693,-0.5829,0.6516),lag.max=40),type="h", col="red")
pacf2 <- pacf(so2_diff,lag.max=40,col="green", main="Sample PACF and ARMA(6,4) Model PACF")
lines(ARMAacf(ar=c(-0.7792,0.3541,0.7790,0.0049,0.083,0.1178),
              ma=c(-0.0994,-0.9693,-0.5829,0.6516),lag.max=40,pacf=TRUE),type="h",col="red")


# standardizing the residuals and plotting it
zt = rstandard(armafit23) 
plot(zt,type="o",col="blue", main="Standardized Residuals for ARMA(2,3)",
     ylab = "Residuals")

zt1 = rstandard(armafit43) 
plot(zt1,type="o",col="blue", main="Standardized Residuals for ARMA(4,3)",
     ylab = "Residuals")

zt2 = rstandard(armafit34) 
plot(zt2,type="o",col="blue", main="Standardized Residuals for ARMA(3,4)",
     ylab = "Residuals")

zt3 = rstandard(armafit64) 
plot(zt3,type="o",col="blue", main="Standardized Residuals for ARMA(6,4)",
     ylab = "Residuals")

# plotting the standardized residuals and their sample acf/pacf 
acf(zt1,lag.max=40, main="Standardized Residuals Sample ACF")
pacf(zt1,lag.max=40, main="Standardized Residuals Sample PACF")

# Computing Ljung-Box and MCleod-Li statistics 
test(zt1)
# The Ljung-Box statistic QLB = 15.52 with p-value 0.746 > 0.05. This 
#   does not provide sufficient evidence to reject the iid hypothesis
# The Mcleod-Li statistic QML = 54.42 with p-value 1e-04 < 0.05. This
#   provides sufficient evidence to reject the iid hypothesis.


# Loading library forecast 
library(forecast)

# computing forecast for 10 time steps ahead using our arma(4,3) model
fc = forecast(so2_diff,model=armafit43,h=10) 

plot(fc,type="o",main="Forecasts from ARMA(4,3)", 
     xlab ="Time", ylab ="Atmospheric Sulfur Dioxide Levels")









