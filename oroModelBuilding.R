## Random Forest
library(randomForest)
rf <- randomForest(WonLose ~ RBU + GlobalSector + TotalRevenueUSD + OpportunityMethodology + ExpectedClose.Create_diff, data = opty2012Final)
plot(rf)


## SVM
library(e1071)
# data(iris)
# model <- svm(Species ~ ., data = iris)
A <- model.matrix(WonLose ~ RBU + GlobalSector + TotalRevenueUSD + OpportunityMethodology + ExpectedClose.Create_diff, data = opty2012Final)   
model <- svm(WonLose ~ ., data = A)

## Decision Tree
# Classification Tree with rpart
library(rpart)
# grow tree 
fit <- rpart(WonLose ~ RBU + GlobalSector + TotalRevenueUSD + OpportunityMethodology + ExpectedClose.Create_diff,
             method="class", data=opty2012Final)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree 
post(fit, file = "c:/tree.ps", 
     title = "Classification Tree for Kyphosis")
