setwd("C:\\F\\NMIMS\\DataScience\\Sem-2\\ML-R\\data")

d_data<-read.csv("D1.csv", header = TRUE, stringsAsFactors = FALSE)
dim(d_data)
head(d_data)
summary(d_data)

cat("\014")
#Detect NAs
detectNAs<-function(x){
  return(sum(is.na(x)))
}
lapply(test_set, detectNAs)

#Determine Class
detectCl<-function(x){
  return(class(x))
}
lapply(d_data, detectCl)

d_data$Gender<-as.factor(d_data$Gender)
d_data$No.show<-as.factor(d_data$No.show)
d_data$Scholarship<-as.factor(d_data$Scholarship)
d_data$Hipertension<-as.factor(d_data$Hipertension)
d_data$Diabetes<-as.factor(d_data$Diabetes)
d_data$Alcoholism<-as.factor(d_data$Alcoholism)
d_data$Handcap<-as.factor(d_data$Handcap)
d_data$SMS_received<-as.factor(d_data$SMS_received)

d_data$AppointmentDay<-as.POSIXct(d_data$AppointmentDay, "%Y-%m-%dT%H:%M:%S", tz="UTC")
d_data$ScheduledDay<-as.POSIXct(d_data$ScheduledDay, "%Y-%m-%dT%H:%M:%S", tz="UTC")


## Cleaning Age
d_data$Age_r <- ifelse(d_data$Age < 0, NA, d_data$Age)
d_data$Age_r <- ifelse(d_data$Age_r > 100, NA, d_data$Age_r)
summary(d_data$Age_r)

summary(d_data[is.na(d_data$Age_r),])
d_data$Age_r <- ifelse(is.na(d_data$Age_r), mean(d_data$Age_r, na.rm=T), d_data$Age_r)
summary(d_data$Age_r)




library(caTools)
set.seed(100) # set seed to replicate results
split<-sample.split(d_data$No.show, SplitRatio=0.8)
train_set<-subset(d_data, split==TRUE)
test_set<-subset(d_data, split==FALSE)
########### Logistics ####################################
cat("\014")
glm.fit<-glm(No.show~.-Neighbourhood, data=train_set, family="binomial")
glm.fit
summary(glm.fit)

glm.fit<-glm(No.show~(AppointmentDay+Scholarship+Diabetes+Alcoholism+SMS_received), data=train_set, family="binomial")
summary(glm.fit)


predictedL<-predict(glm.fit, newdata = test_set, type="response")
confL<-table(predicted=predictedL>0.5, actual=test_set$No.show)

predictedL1<-predict(glm.fit, newdata = train_set, type="response")
confL1<-table(predicted=predictedL1>0.5, actual=train_set$No.show)
## Accuracy
accuracyL1=sum(diag(confL1))/sum(confL1)
accuracyL1

## Accuracy
accuracyL=sum(diag(confL))/sum(confL)
accuracyL
###### ROCR Curve ######
library(ROCR)
ROCRpred <- prediction(predictedL, test_set$No.show)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
auc.tmp <- performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc



#### Decision Tree ####
## Donot use scaling, directly fit the model on the data
library(rpart)
fit <- rpart(No.show~(AppointmentDay+Scholarship+Diabetes+Alcoholism+SMS_received), data=train_set, control=rpart.control(minsplit=10) ,method="class")
summary(fit)
plot(fit)
text(fit)
predictedD= predict(fit, test_set, type="class")
confD<-table(predicted=predictedD, actual=testData$default)
accuracyD=sum(diag(confD))/sum(confD)
accuracyD

predicted_d1= predict(fit, test_set, type="prob")

library(pROC)
aucD<-auc(test_set$default, predicted_d1[,2])
plot(roc(test_set$default, predicted_d1[,2]))




#### Naive Bayes ####
library(e1071)
cat("\014")
naiveFit<-naiveBayes(No.show~(AppointmentDay+Scholarship+Diabetes+Alcoholism+SMS_received), data=train_set)
naiveFit

predictedN<-predict(naiveFit, newdata=test_set)
confN<-table(predicted=predictedN, actual=test_set$No.show)
accuracyN=sum(diag(confN))/sum(confN)
accuracyN

auc<-auc(test_set$No.show, as.numeric(predictedN))
plot(roc(test_set$default, as.numeric(predictedN)))



###### KNN #######
#### Used to determine the value of k
library(class)
library(MASS)
library(ISLR)
library(caret)
set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3)
knnFit1 <- train(No.show~(AppointmentDay+Scholarship+Diabetes+Alcoholism+SMS_received), data=train_set, method = "knn", trControl = ctrl, preProcess = c("center","scale"),tuneLength = 20)
knnFit1

cat("\014")
predictKnn <- knn(train = train_set, test = test_set, cl = train_set$No.show, k=332)


confK<-table(predicted=predictKnn, actual=test_set$default)
accuracyk=sum(diag(confK))/sum(confK)
accuracyk

auck<-auc(test_set$default, as.numeric(predictKnn))
plot(roc(test_set$default, as.numeric(predictKnn)))