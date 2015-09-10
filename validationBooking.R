##
## read data (number of booking by PG, Sector, Month)
##
bookingNumber <- read.csv("bookingNumber.csv", header = T)

bookingYM <- bookingNumber$OrderYM
bookingDT <- 0
for (i in 1:length(bookingYM)){
  print(i/length(bookingYM))
  tmp <- paste(c(substring(bookingYM[i], 1, 4), substring(bookingYM[i], 5, 6), "01"), collapse = "-")
  bookingDT[i] <- tmp
}
bookingNumber$OrderYM <- bookingDT

uniDT <- unique(bookingDT)
bookingNumberByMonth <- 0
for (j in 1:length(uniDT)){
  print(j/length(uniDT))
  bookingNumberByMonth[j] <- sum(bookingNumber[which(bookingNumber$OrderYM == uniDT[j]), "US.Amt"])
}
bookingNumberByMonth <- matrix(bookingNumberByMonth, nrow = 1)
colnames(bookingNumberByMonth) <- uniDT


opty2013Use