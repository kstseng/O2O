###############################################################################################################
##
## Read data (number of booking by PG, Sector, Month)
##
###############################################################################################################
bookingNumber <- read.csv("bookingNumber.csv", header = T)

###############################################################################################################
##
## Change date form.
##
###############################################################################################################
bookingYM <- bookingNumber$OrderYM
bookingDT <- 0
for (i in 1:length(bookingYM)){
  print(i/length(bookingYM))
  tmp <- paste(c(substring(bookingYM[i], 1, 4), substring(bookingYM[i], 5, 6), "01"), collapse = "-")
  bookingDT[i] <- tmp
}
bookingNumber$OrderYM <- bookingDT

###############################################################################################################
##
## Summary all region, PG and sector by month from booking number.
##
###############################################################################################################
uniDT <- unique(bookingDT)
bookingNumberByMonth <- 0
for (j in 1:length(uniDT)){
  print(j/length(uniDT))
  bookingNumberByMonth[j] <- sum(bookingNumber[which(bookingNumber$OrderYM == uniDT[j]), "US.Amt"])
}
bookingNumberByMonth <- matrix(bookingNumberByMonth, nrow = 1)
colnames(bookingNumberByMonth) <- uniDT
# YearForm <- sapply(1:length(uniDT), function(k)strsplit(uniDT[k], "-")[[1]][1])
# bookingNumberByYear

###############################################################################################################
##
## Summary all region, PG and sector from opty data.
##
###############################################################################################################
head(opty2013Use)
valData <- rbind(opty2012Use, opty2013Use, opty2014Use)

opty2orderNumber <- 0
for (i in 1:length(bookingNumberByMonth)){
  print(i/length(bookingNumberByMonth))
  startDT <- colnames(bookingNumberByMonth)[i]
  endDT <- seq(as.Date(startDT), length = 2, by = "months")[2]
  valDataInRange <- valData[which(valData$ActualCloseDate >= startDT & valData$ActualCloseDate < endDT) ,]
  successOpty <- valDataInRange[which(valDataInRange$Opportunity == 100), ]
  opty2orderNumber[i] <- sum(successOpty$TotalRevenueUSD)
}
plot(opty2orderNumber, type = "l", ylim = c(min(opty2orderNumber, bookingNumberByMonth), max(opty2orderNumber, bookingNumberByMonth)), col = "blue")
lines(as.numeric(bookingNumberByMonth), col = "red")

opty2orderNumberM <- matrix(opty2orderNumber, nrow = 1)
colnames(opty2orderNumberM) <- colnames(bookingNumberByMonth)
out <- rbind(bookingNumberByMonth, opty2orderNumberM)
rownames(out) <- c("bookingFromPoki", "bookingByOpty")
# write.csv(t(out), "bookingValidation.csv")

