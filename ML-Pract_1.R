setwd("C:\\F\\NMIMS\\DataScience\\Sem-2\\DS\\28thFebLogistic")

library(caTools)


wholeData<-read.csv("wholeData.csv", header = TRUE, stringsAsFactors = FALSE)
wholeData$default<-as.factor(wholeData$default)
class(wholeData$default)
dim(wholeData)
head(wholeData)

set.seed(100) # set seed to replicate results
split<-sample.split(wholeData$default, SplitRatio=0.8)
train_set<-subset(wholeData, split==TRUE)
test_set<-subset(wholeData, split==FALSE)

cat("\014")
########### Logistics ####################################
## Use scale function if required to do scaling
glm.fit<-glm(default~(age+employ+address+dbtinc+creddebt), data=train_set, family="binomial")
glm.fit
summary(glm.fit)

predictedL<-predict(glm.fit, newdata = test_set, type="response")
confL<-table(predicted=predictedL>0.5, actual=test_set$default)
accuracyL=sum(diag(confL))/sum(confL)
accuracyL


###### ROCR Curve ######
library(ROCR)
ROCRpred <- prediction(predictedL, test_set$default)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
auc.tmp <- performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc


#### Decision Tree ####
## Donot use scaling, directly fit the model on the data
library(rpart)
fit <- rpart(default~(age+employ+address+dbtinc+creddebt), data=train_set ,method="class")
summary(fit)
plot(fit)
text(fit)
predictedD= predict(fit, test_set, type="class")
confD<-table(predicted=predictedD, actual=test_set$default)
accuracyD=sum(diag(confD))/sum(confD)
accuracyD

predicted_d1= predict(fit, test_set, type="prob")

library(pROC)
aucD<-auc(test_set$default, predicted_d1[,2])
plot(roc(test_set$default, predicted_d1[,2]))


#### Naive Bayes ####
## Use scale function if required to do scaling
library(e1071)
naiveFit<-naiveBayes(default~(age+employ+address+dbtinc+creddebt), data=train_set)
naiveFit

predictedN<-predict(naiveFit, test_set, type="raw")
confN<-table(predicted=predictedN[,2]>0.5, actual=test_set$default)
accuracyN=sum(diag(confN))/sum(confN)
accuracyN

###### ROCR Naive
ROCRPred_naive<-prediction(predictedN[,2], test_set$default)
ROCRPref_naive<-performance(ROCRPred_naive, "tpr", "fpr")
plot(ROCRPref_naive, colorize=TRUE)
auc.temp_naive<-performance(ROCRPred_naive, "auc")
auc_naive<-as.numeric(auc.temp_naive@y.values)
auc_naive

auc<-auc(test_set$default, as.numeric(predictedN))
plot(roc(test_set$default, as.numeric(predictedN)))



###### KNN #######
#### Used to determine the value of k
library(class)
library(caret)
set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3)
knnFit1 <- train(default ~ ., data = wholeData, method = "knn", trControl = ctrl, preProcess = c("center","scale"),tuneLength = 20)
knnFit1


kVsAccuracy = matrix(ncol = 2, nrow = 8)
i = 1
for (k in 3:10){
  predictions = knn(train = train_set, test = test_set,cl =  train_set$default, k = k)
  kVsAccuracy[i,] = c(k,mean(predictions==train_set$default))
  i = i+1
}
plot(kVsAccuracy[,1],kVsAccuracy[,2],type = "l",xlab = "Value of k", ylab = "Testing Accuracy",main = "Choosing value of k based on testing accuracy")


cat("\014")
####Use scale function if required to do scaling
predictKnn <- knn(train = train_set, test = test_set, cl = train_set$default, k=9, prob=TRUE)
confK<-table(as.numeric(predictKnn)>0.5, actual=test_set$default)
accuracyk=sum(diag(confK))/sum(confK)
accuracyk

##### ROCR Knn
ROCRPred_knn<-prediction(as.numeric(predictKnn), test_set$default)
ROCRPref_knn<-performance(ROCRPred_knn, "tpr", "fpr")
plot(ROCRPref_knn, colorize=TRUE)
auc.temp_knn<-performance(ROCRPred_naive, "auc")
auc_knn<-as.numeric(auc.temp_knn@y.values)
auc_knn

auck<-auc(test_set$default, as.numeric(predictKnn))
plot(roc(test_set$default, as.numeric(predictKnn)))




######## Lasso Model #######
##If there are lot of data points use it to
##find the significant ones
x=model.matrix(churn~., churnData)[,-1]
y=churnData$churn
train<-sample(1:nrow(x),nrow(x)/2)
test<--train
grid<-10^seq(10, -2, length=100)
##Fit the Lasso model
lasso.fit=glmnet(x[train,], y[train], family="binomial", alpha=1, lambda=grid)
plot(lasso.fit)
##Choose the best value of lambda by cross validationfor predicting
set.seed(1)
cv.out<-cv.glmnet(x[train,], y[train],alpha=1, family="binomial")
plot(cv.out)
bestLambda=cv.out$lambda.min
bestLambda

lasso.pred=predict(lasso.fit, s=bestLambda, newx = x[test,])
#mean((lasso.pred-y[test])^2)

out=glmnet(x,y,alpha=1, lambda=grid, family="binomial")
lasso.coeff=predict(out, type="coefficients", s=bestLambda)[1:36,]
lasso.coeff[lasso.coeff!=0]