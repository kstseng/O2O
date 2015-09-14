###############################################################################################################
##
## Use data: booking number by month
##
###############################################################################################################
booking.ts <- ts(as.numeric(bookingNumberByMonth), frequency = 12, start = c(2012,1))
plot.ts(booking.ts)

library("TTR")
booking.ts.ma <- SMA(booking.ts,n=3)
plot.ts(booking.ts.ma)

#
booking.ts.comp <- decompose(booking.ts)
plot(booking.ts.comp)

# fcst
bookingforecasts <- HoltWinters(booking.ts, beta=FALSE, gamma=FALSE)
plot(bookingforecasts)
bookingforecasts <- HoltWinters(booking.ts, beta=FALSE)
bookingforecasts <- HoltWinters(booking.ts)

library(forecast)
bookingforecasts2 <- forecast.HoltWinters(bookingforecasts, h=8)
plot.forecast(bookingforecasts2)
acf(bookingforecasts2$residuals, lag.max=20)
Box.test(bookingforecasts2$residuals, lag=20, type="Ljung-Box")
plot.ts(bookingforecasts2$residuals)
#
plotForecastErrors <- function(forecasterrors){
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
plotForecastErrors(bookingforecasts2$residuals)
####
bookingarima <- arima(booking.ts, order=c(2,0,0))
bookingarimaforecasts <- forecast.Arima(bookingarima, h=31)
acf(bookingarimaforecasts$residuals, lag.max=20)
Box.test(bookingarimaforecasts$residuals, lag=20, type="Ljung-Box")

####
fit1 <- HoltWinters(booking.ts, beta=FALSE, gamma=FALSE)
# double exponential - models level and trend
fit2 <- HoltWinters(booking.ts, gamma=FALSE)
# triple exponential - models level, trend, and seasonal components
fit3 <- HoltWinters(booking.ts)

###############################################################################################################
##
## auto.arima
##
###############################################################################################################
range <- c(1:30)
test <- as.numeric(bookingNumberByMonth)[range]
test.ts <- ts(test, frequency = 12, start = c(2012,1))
test.autoArima <- auto.arima(test.ts)
fcst.test <- forecast(test.autoArima, h = 44 - max(range))
as.numeric(fcst.test$mean)/as.numeric(bookingNumberByMonth)[(max(range) + 1):44]
plot(c(test, as.numeric(fcst.test$mean)), col = "red", type = "l")
lines(as.numeric(bookingNumberByMonth), col = "blue")
#
booking.autoArima <- auto.arima(booking.ts)
plot(forecast(booking.autoArima, h = 12))
forecast(booking.autoArima, h = 10)

