####################################################################################################################
##
## Random Forest
##
####################################################################################################################
library(randomForest)
# set.seed(111)
# ind <- sample(2, nrow(dataForUseFinalForModel), replace = TRUE, prob=c(0.8, 0.2))
# rf1 <- randomForest(WonLose ~ ., data = dataForUseFinalForModel[ind == 1,])
# rf1
# pred. <- predict(rf1, dataForUseFinalForModel[ind == 2,])
# table(predicted = as.character(pred), observed = dataForUseFinalForModel[ind==2, "WonLose"])
##
myrf <- randomForest(WonLose ~ ., data = dataForUseFinalForModel)
myrf
pred.rf <- predict(myrf, datCVAddPFinalForModel.remind)
table(predicted = as.character(pred.rf), observed = datCVAddPFinalForModel.remind[, "WonLose"])

####################################################################################################################
##
## Logistic Regression
##
####################################################################################################################
# dataForUseFinalForModel
# datCVAddPFinalForModel.remind
# mylogit <- glm(WonLose ~ RBU + GlobalSector + SalesForce + TotalRevenueUSD + OpportunityMethodology + ExpectedClose.Create_diff + 
#                  RBU*GlobalSector + GlobalSector*SalesForce + SalesForce*TotalRevenueUSD + TotalRevenueUSD*OpportunityMethodology + OpportunityMethodology*ExpectedClose.Create_diff + ExpectedClose.Create_diff*RBU, data = dataForUseFinalForModel, family = "binomial")
mylogit <- glm(WonLose ~ RBU + GlobalSector + SalesForce + TotalRevenueUSD + OpportunityMethodology + ExpectedClose.Create_diff + 
                 SalesForce*OpportunityMethodology + RBU*GlobalSector, data = dataForUseFinalForModel, family = "binomial")
summary(mylogit)
pred.logit <- predict(mylogit, newdata=datCVAddPFinalForModel.remind, type="response")
  response.logit <- 0
  for (i in 1:length(pred.logit)){
    print(i/length(pred.logit))
    if(pred.logit[i] >= 0.5){
      response.logit[i] <- 1
    }else{
      response.logit[i] <- 0
    }
  }
  table(predicted = response.logit, observed = datCVAddPFinalForModel.remind[, "WonLose"])
  
####################################################################################################################
##
## Regularized Logistic Regression
##
####################################################################################################################
# train <- dataForUseFinalForModel
# test <- datCVAddPFinalForModel.remind
library(LiblineaR)
x <- dataForUseFinalForModel[, 1:6]
y <- factor(dataForUseFinalForModel[, 7])
train <- sample(1:dim(dataForUseFinalForModel)[1],round(dim(dataForUseFinalForModel)[1]*0.8))
xTrain <- x[train,]
xTest <- x[-train,]
yTrain <- y[train]
yTest <- y[-train]
# Center and scale data
# s <- scale(xTrain,center=TRUE,scale=TRUE)
# Sparse Logistic Regression
t=6
# co=heuristicC(s)
m <- LiblineaR(data=xTrain,labels=yTrain,type=t,bias=TRUE,verbose=FALSE)
# Scale the test data
s2=scale(xTest,attr(s,"scaled:center"),attr(s,"scaled:scale"))
# Make prediction
p=predict(m,s2)
# Display confusion matrix
res=table(p$predictions,yTest)
print(res)
# Compute Balanced Classification Rate
BCR=mean(c(res[1,1]/sum(res[,1]),res[2,2]/sum(res[,2]),res[3,3]/sum(res[,3])))
print(BCR)


####################################################################################################################
##
## SVM
##
####################################################################################################################
t1 <- proc.time()
mysvm <- svm(WonLose ~ ., data = dataForUseFinalForModel)
t2 <- proc.time()
pred.svm <- predict(mysvm, subset(datCVAddPFinalForModel.remind, select = -WonLose)) 
table(predicted = pred.svm, observed = datCVAddPFinalForModel.remind[, "WonLose"])

####################################################################################################################
##
## xgboost
##
####################################################################################################################
library(xgboost)
library(Matrix)
train <- dataForUseFinalForModel
test <- datCVAddPFinalForModel.remind
train.mx <- sparse.model.matrix(WonLose~., train)
test.mx <- sparse.model.matrix(WonLose~., test)
dtrain <- xgb.DMatrix(train.mx, label=train$WonLose)
dtest <- xgb.DMatrix(test.mx, label=test$WonLose)
train.gdbt <- xgb.train(params=list(objective="multi:softmax", num_class=10, eval_metric="mlogloss", eta=0.2, max_depth=5, subsample=1, colsample_bytree=0.5), data=dtrain, nrounds=150, watchlist=list(eval=dtest, train=dtrain))
pred.xgboost <- predict(train.gdbt,newdata=dtest)
table(predicted = pred.xgboost, observed = datCVAddPFinalForModel.remind[, "WonLose"])
sum(diag(table(test$WonLose,pred)))/nrow(test)
######
######
######






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
