#########################################################################################################################
##                                                                                                                     ##
## read data                                                                                                           ##
##                                                                                                                     ##
#########################################################################################################################
opty2012 <- read.csv("C:\\Users\\David79.Tseng\\Dropbox\\HomeOffice\\O2O\\2012Opty.csv", header = T)
opty2013 <- read.csv("C:\\Users\\David79.Tseng\\Dropbox\\HomeOffice\\O2O\\2013Opty.csv", header = T)
opty2014 <- read.csv("C:\\Users\\David79.Tseng\\Dropbox\\HomeOffice\\O2O\\2014Opty.csv", header = T)
opty2015 <- read.csv("C:\\Users\\David79.Tseng\\Dropbox\\HomeOffice\\O2O\\2015Opty.csv", header = T)
newColnames <- c("RBU", "GlobalSector", "Sales", "SalesForce", "Account", "TotalRevenueUSD", 
                 "TotalRevenue", "Currency", "ProjectName", "OpportunityMethodology", "OpportunityStatus", "Opportunity", 
                 "OpportunityID", "ReasonOfWL", "CreateDate", "ActualCloseDate", "ActualClose.Create_diff", "ExpectedCloseDate")
colnames(opty2012) <- colnames(opty2013) <- colnames(opty2014) <- colnames(opty2015) <- newColnames

#########################################################################################################################
##                                                                                                                     ##
## processing data                                                                                                     ##
##                                                                                                                     ##  
#########################################################################################################################

## extract the columns
UseCol <- c("RBU", "GlobalSector", "SalesForce", "TotalRevenueUSD", "Currency", 
            "OpportunityMethodology", "Opportunity", 
            "CreateDate", "ActualCloseDate", "ActualClose.Create_diff", "ExpectedCloseDate")
opty2012Use <- opty2012[, UseCol]
opty2013Use <- opty2013[, UseCol]
opty2014Use <- opty2014[, UseCol]
opty2015Use <- opty2015[, UseCol]

## Turn TotalRevenueUSD to numeric
opty2012Use$TotalRevenueUSD <- as.numeric(opty2012Use$TotalRevenueUSD)
opty2013Use$TotalRevenueUSD <- as.numeric(opty2013Use$TotalRevenueUSD)
opty2014Use$TotalRevenueUSD <- as.numeric(opty2014Use$TotalRevenueUSD)
opty2015Use$TotalRevenueUSD <- as.numeric(opty2015Use$TotalRevenueUSD)

## date form transform
opty2012Use$CreateDate <- as.Date(opty2012Use$CreateDate)
opty2012Use$ActualCloseDate <- as.Date(opty2012Use$ActualCloseDate)
opty2012Use$ExpectedCloseDate <- as.Date(opty2012Use$ExpectedCloseDate)
opty2013Use$CreateDate <- as.Date(opty2013Use$CreateDate)
opty2013Use$ActualCloseDate <- as.Date(opty2013Use$ActualCloseDate)
opty2013Use$ExpectedCloseDate <- as.Date(opty2013Use$ExpectedCloseDate)
opty2014Use$CreateDate <- as.Date(opty2014Use$CreateDate)
opty2014Use$ActualCloseDate <- as.Date(opty2014Use$ActualCloseDate)
opty2014Use$ExpectedCloseDate <- as.Date(opty2014Use$ExpectedCloseDate)
opty2015Use$CreateDate <- as.Date(opty2015Use$CreateDate)
opty2015Use$ActualCloseDate <- as.Date(opty2015Use$ActualCloseDate)
opty2015Use$ExpectedCloseDate <- as.Date(opty2015Use$ExpectedCloseDate)

## add new variable to opty201xUse
dataForUse <- rbind(opty2012Use, opty2013Use)
ExpectedClose.Create_diff <- as.numeric(dataForUse$ExpectedCloseDate - dataForUse$CreateDate)
dataForUseAdd <- cbind(dataForUse, ExpectedClose.Create_diff = ExpectedClose.Create_diff)
dataForUseAddP <- dataForUseAdd[-which(dataForUseAdd$ExpectedClose.Create_diff < 0), ]
WonLose <- 0
for(i in 1:nrow(dataForUseAddP)){
  print(i/nrow(dataForUseAddP))
  if (dataForUseAddP$Opportunity[i] == 100){
    WonLose[i] <- 1
  }else{
    WonLose[i] <- 0
  }
}
dataForUseFinal <- cbind(dataForUseAddP, WonLose = as.factor(WonLose))
useX <- c("RBU", "GlobalSector", "SalesForce", "TotalRevenueUSD", "OpportunityMethodology", "ExpectedClose.Create_diff", "WonLose")
dataForUseFinalForModel <- dataForUseFinal[, useX]
#########################################################################################################################
##                                                                                                                     ##
## Cross Validation data
##                                                                                                                     ##  
#########################################################################################################################
datCV <- opty2014Use
ExpectedClose.Create_diff <- as.numeric(datCV$ExpectedCloseDate - datCV$CreateDate)
datCVAdd <- cbind(datCV, ExpectedClose.Create_diff = ExpectedClose.Create_diff)
datCVAddP <- datCVAdd[-which(datCVAdd$ExpectedClose.Create_diff < 0), ]
WonLose <- 0
for(i in 1:nrow(datCVAddP)){
  print(i/nrow(datCVAddP))
  if (datCVAddP$Opportunity[i] == 100){
    WonLose[i] <- 1
  }else{
    WonLose[i] <- 0
  }
}
datCVAddPFinal <- cbind(datCVAddP, WonLose = as.factor(WonLose))
datCVAddPFinalForModel <- datCVAddPFinal[, useX]
#########################################################################################################################
##                                                                                                                     ##
## Compare training data and cv data
##                                                                                                                     ##  
#########################################################################################################################
# str(dataForUseFinal)
# str(datCVAddPFinal)
# table(dataForUseFinal$RBU)
# table(datCVAddPFinal$RBU)
# table(dataForUseFinal$GlobalSector)
# table(datCVAddPFinal$GlobalSector)

remove.ind <- which(datCVAddPFinalForModel$RBU == "iConnectivity")
datCVAddPFinalForModel.remind <- datCVAddPFinalForModel[-remove.ind, ]
datCVAddPFinalForModel.remind$RBU <- as.factor(as.character(datCVAddPFinalForModel.remind$RBU))
datCVAddPFinalForModel.remind$GlobalSector <- as.factor(as.character(datCVAddPFinalForModel.remind$GlobalSector))

levels(datCVAddPFinalForModel.remind$RBU) <- levels(dataForUseFinalForModel$RBU)
levels(datCVAddPFinalForModel.remind$GlobalSector) <- levels(dataForUseFinalForModel$GlobalSector)
####
####
####
####
####
testdata <- opty2012Use
tmp <- which(testdata$ExpectedCloseDate - testdata$CreateDate < 0)
testdata[tmp, "Opportunity"]
head(testdata[tmp, ], 10)
length(which(testdata[tmp, "Opportunity"] == 100))/length(testdata[tmp, "Opportunity"])
length(tmp)


##
str(opty2012Use)
tmp <- (which(opty2012Use$ActualCloseDate - opty2012Use$CreateDate == 0))
opty2012Use[tmp[1:10], ]

opty2012Use$OpportunityMethodology

length((which(opty2012Use$ExpectedCloseDate - opty2012Use$CreateDate < 0)))
length((which(opty2013Use$ExpectedCloseDate - opty2013Use$CreateDate < 0)))
length((which(opty2014Use$ExpectedCloseDate - opty2014Use$CreateDate < 0)))
length((which(opty2015Use$ExpectedCloseDate - opty2015Use$CreateDate < 0)))

length(which(opty2012Use$ExpectedCloseDate == opty2012Use$ActualCloseDate))




