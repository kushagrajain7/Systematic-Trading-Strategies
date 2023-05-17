library(rstudioapi)
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())
options(scipen=999)
cat("\014")

library(caret)
library(gbm)
library(e1071)
library('MLmetrics')
library(neuralnet)


load("universe (2).rdata")
universe = stock

load("stock_1.rdata")

days<-unique(universe$date)
days<-days[order(days)]
yearsBacktest<-1
trainend = 1241
trainstart = 736
windowsize = 5
currday = 736
from<-days[currday-windowsize]
to<-days[currday-1]
train<-subset(stock,stock$date>=from&stock$date<=to)[3:ncol(stock)]
train = train[-ncol(train)]
# Identify column names to remove
columns_to_remove <- c("cross.trenddn", "cross.trendup", "cross.lowerBB")
# Remove columns from the dataframe
train <- train[, !(colnames(train) %in% columns_to_remove)]
test<-subset(stock,stock$date==as.Date(days[currday]))

# svm.fit=svm(nextreturn~.-nextopen -nextclose,data=train,
#             kernel="radia", cost=0.001, epsilon = 0.1)
# svm.predicted=predict(svm.fit, test)
# rsq = R2_Score(svm.predicted, test$nextreturn)
# print(paste("RSQ:",rsq))
# preds = test
# preds$rsq<-rep(rsq,nrow(preds))
# preds$prediction<- svm.predicted
# 
# ?gbm
# # GBM
# gbm.model=gbm(nextreturn~.-nextopen -nextclose,
#               data=train,
#               distribution="laplace",
#               n.trees=100,
#               shrinkage=1e-2,
#               interaction.depth=3,
#               cv.fold=5,
#               verbose = FALSE)
# n.trees.cv=gbm.perf(gbm.model, method = "cv")
# # n.trees.cv = 100
# gbm.predicted=predict.gbm(gbm.model,newdata=test,n.trees=n.trees.cv, type = "response")
# rsq = round(R2_Score(gbm.predicted, test$nextreturn), 3)
# print(paste("RSQ:",rsq))
# preds<-test
# preds$rsq<-rep(rsq,nrow(preds))
# preds$prediction<-gbm.predicted

# logistic regression model
# glm.fit=glm(nextreturn~.-nextopen -nextclose, data=train,family=gaussian)
# rsq = with(summary(glm.fit), 1 - deviance/null.deviance)
# glm.predicted=predict(glm.fit,newdata=test,type="response")

#SVM
# ?svm
# svmfit=svm(nextreturn~.-nextopen -nextclose,data=train,
#            kernel="polynomial", cost=0.001, epsilon = 0.1)
# # OptModelsvm=tune(svm,nextreturn~.-nextopen -nextclose,data=train,
# #                   ranges=list(epsilon=seq(0,1,0.2), cost=1:10))
# 
# svm.pred=predict(svmfit, test)
# R2_Score(svm.pred, test$nextreturn)

#  Neural Network
# train_nn = train[-ncol(train)]
# 
nn.fit <- neuralnet(nextreturn~.-nextopen -nextclose,data=train,hidden=c(5, 3),linear.output=TRUE)
nn.predicted <- predict(nn.fit, test)
rsq = R2_Score(nn.predicted, test$nextreturn)

gbm.model=gbm(nextreturn~.-nextopen -nextclose,
              data=train,
              distribution="gaussian",
              n.trees=500,
              shrinkage=1e-2,
              interaction.depth=3, 
              cv.fold=5,
              verbose = FALSE)
n.trees.cv=gbm.perf(gbm.model, method = "cv")
gbm.predicted=predict.gbm(gbm.model,newdata=test,n.trees=n.trees.cv, type = "response")
residuals = test$nextreturn - gbm.predicted

y_test_mean = mean(test$nextreturn)
tss =  sum((test$nextreturn - y_test_mean)^2 )
rss =  sum(residuals^2)
rsq  =  round((1 - (rss/tss)) , 3)

