sqlcommand <- "select substring(convert(char,order_DATE,112),1,6),sum(us_amt) from 
shipmentBacklog aa  (nolock)  where  order_date between '2010/1/1' and 
'2015/8/31'  and fact_1234 = '1'  and bomseq >= 0  and breakdown >= 0  and 
itp_find <> 2  and itp_find <> 9   and ( qty <> 0 or us_amt <> 0 or 
cancel_flag = ' ' ) group by substring(convert(char,order_DATE,112),1,6) 
order by 1"
library(RODBC)
conn <- odbcConnect(dsn = "ACL_EAI_ACL", uid = "david79", pwd = "dtG79")
rawData <- sqlQuery(conn, sqlcommand)
colnames(rawData) <- c("date", "us.amt")
plot(rawData$us.amt, type = "l")
##
range <- c(1:68)
test <- as.numeric(rawData$us.amt)[range]
test.ts <- ts(test, frequency = 12, start = c(2010,1))
test.autoArima <- auto.arima(test.ts)
fcst.test <- forecast(test.autoArima, h = nrow(rawData) - max(range))
#fcst.test <- forecast(test.autoArima, h = 1)

as.numeric(fcst.test$mean)/as.numeric(rawData$us.amt)[(max(range) + 1):nrow(rawData)]
plot(c(test, as.numeric(fcst.test$mean)), col = "red", type = "l")
lines(as.numeric(rawData$us.amt), col = "blue")
#
booking.autoArima <- auto.arima(booking.ts)
plot(forecast(booking.autoArima, h = 12))
forecast(booking.autoArima, h = 10)