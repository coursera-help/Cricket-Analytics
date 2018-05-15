setwd("C:\\F\\NMIMS\\DataScience\\Sem-2\\ML-R\\data")

library(caTools)
diabData<-read.csv("diabetes.csv", header = TRUE, stringsAsFactors = FALSE)
head(diabData)
dim(diabData)
View(diabData)


class(diabData$Outcome)
diabData$Outcome<-as.factor(diabData$Outcome)

#Detect NAs
detectNAs<-function(x){
  return(sum(is.na(x)))
}
sapply(diabData, detectNAs)

set.seed(100) # set seed to replicate results
split<-sample.split(diabData$Outcome, SplitRatio=0.8)
train_set<-subset(diabData, split==TRUE)
test_set<-subset(diabData, split==FALSE)

cat("\014")
glm.fit<-glm(Outcome~., data=train_set, family="binomial")
glm.fit
summary(glm.fit)

cat("\014")
glm.fit<-glm(Outcome~.SkinThickness, data=train_set, family="binomial")
summary(glm.fit)

cat("\014")
pred<-predict(glm.fit, newdata = test_set, type="response")
conf<-table(test_set$Outcome,pred>0.5)
accuracy=sum(diag(conf))/sum(conf)
accuracy

############### ROCR Curve ########################
pred<-predict(glm.fit, newdata = train_set, type="response")
ROCRpred <- prediction(pred, train_set$Outcome)
ROCRperf <- performance(ROCRpred, 'tpr','fnr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

################# Using K fold ###############
library(caret)

train_control<- trainControl(method="cv", number=10)

model<- train(default~., data=diabData, trControl=train_control, method="glm", family="binomial")
summary(model)

predTrain = predict(model, newdata=test_set) 
conf <- table(test_set$default, predTrain)
accuracy<-sum(diag(conf))/sum(conf)
accuracy


################## Decision Tree ################
library(rpart)

