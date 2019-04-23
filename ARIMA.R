# Read Data
radio.csv = read.csv("lenex_mixers_csv.csv", header=T)
radio.csv
radio.csv$Sales
radio.sales <- radio.csv$Sales
radio.sales

#Define radioseries as a timeseries
radioseries=ts(radio.sales, start = c(2004,1))
radioseries
plot.ts(radioseries)

#install package (Forecast)

install.packages("forecast")
library("forecast")

#acf and pacf

acf(radioseries, type="correlation", lag.max = 50)
acf(radioseries, type="correlation", Lag = 50, plot=F)
pacf(radioseries, lag.max =20)

#Ljung.Box Test
Box.test(radioseries, lag = 20, type="Ljung-Box")

#Differencing
#Non-Seasonal
radioseries1<-diff(radioseries, differences = 1)
radioseries1

#Seasonal
radioseries12<-diff.Date(radioseries, lag=12)
radioseries12

#acf and pacf of de-seasonalized series

acf(radioseries12, type="correlation", lag.max = 50)
pacf(radioseries12, lag.max =50)

#Ljung.Box Test
Box.test(radioseries12, lag = 20, type="Ljung-Box")

#ARIMA Model
radioseries=ts(radio.sales, frequency=12, start = c(2004,1))
radioseriesarima<- Arima(radioseries,order=c(0,0,0),seasonal=c(0,1,1))
radioseriesarima

#acf and pacf of Residuals

acf(radioseriesarima$residuals, type="correlation", lag.max = 20)
pacf(radioseriesarima$residuals, lag.max =20)

#Ljung.Box Test
Box.test(radioseriesarima$residuals, lag = 20, type="Ljung-Box")


# Forecast


radiofuture=forecast(radioseriesarima,h=12)
forecast(radiofuture)
plot(radiofuture)
coef(radioseriesarima)

# Exponential Smoothing

radioforecasts<-ets(radioseries,model="AAA", damped=FALSE)
radioforecasts
radioforecasts$fitted
plot(radioforecasts)
accuracy(radioforecasts)
residuals(radioforecasts)
summary(radioforecasts)
coef(radioforecasts)
simulate(radioforecasts)

# Level Model

radioforecasts<-HoltWinters(radioseries,beta=FALSE, gamma=F)
radioforecasts
radioforecasts$fitted
plot(radioforecasts)

# Try Additive Seasonal Model

radioforecasts2<-HoltWinters(radioseries,beta=FALSE)
radioforecasts2
radioforecasts2$fitted
plot(radioforecasts2)
radioforecasts2$SSE

# Forecast


radiofuture=forecast(radioforecasts2,h=12)
forecast(radioforecasts)
forecast(radiofuture)

