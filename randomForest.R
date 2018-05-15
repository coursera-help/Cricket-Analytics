setwd("C:\\F\\NMIMS\\DataScience\\Sem-2\\ML-R\\data")
######### Random Forest ############
library(MASS)
library(ISLR)
library(e1071)
library(caTools)
library(randomForest)


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
train_set$Outcome<-as.factor(train_set$Outcome)

cat("\014")
bestMtry <- tuneRF(train_set, train_set$Outcome, stepFactor=1.2, improve=0.01, trace=TRUE, plot=TRUE)
bestMtry
best.m <- bestMtry[bestMtry[, 2] == min(bestMtry[, 2]), 1]
best.m

########### Random Forest #############
diabForest<-randomForest(Outcome~., data=train_set, mtry=best.m)
diabForest

#Evaluate variable importance
importance(diabForest)
varImpPlot(diabForest)


#### Prediction
predict_diab_rf<-predict(diabForest, newdata = test_set, type="prob")
conf<-table(predict_diab_rf[,2]>0.5, test_set$Outcome)
accuracy=sum(diag(conf))/sum(conf)
accuracy

########### ROCR Random Forest #############
library(ROCR)
ROCRpred.randmForst<-prediction(predict_diab_rf[,2], test_set$Outcome)
ROCRpref.randmForst<-performance(ROCRpred.randmForst, "tpr", "fpr")
plot(ROCRpref.randmForst, colorize=TRUE)
auc.temp_rf<-performance(ROCRpred.randmForst, "auc")
auc_rf<-as.numeric(auc.temp_rf@y.values)
auc_rf




################# Boosting ##################
library (gbm)
library(caret)
set.seed (1)
fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 4)

gbmFit1 <- train(Outcome~(Pregnancies+Glucose+BloodPressure+SkinThickness+Insulin+BMI+DiabetesPedigreeFunction+Age),
                 data=train_set, method = "gbm", trControl = fitControl,verbose = FALSE)

plot(gbmFit1)
diab.boost_pred=predict (gbmFit1 ,newdata=test_set,type="prob")
conf_boost<-table(diab.boost_pred[,2]>0.5, test_set$Outcome)
accuracy_boost=sum(diag(conf_boost))/sum(conf_boost)
accuracy_boost

test_set_outcome<-ifelse(test_set$Outcome==1, "High", "Low")
diab.pred_boost_1<-ifelse(diab.boost_pred[,2]>0.5, "High", "Low")

confusionMatrix(test_set_outcome, diab.pred_boost_1)

########### ROCR Boost #############
ROCRpred.boost<-prediction(diab.boost_pred[,2], test_set$Outcome)
ROCRpref.boost<-performance(ROCRpred.boost, "tpr", "fpr")
plot(ROCRpref.boost, colorize=TRUE)
auc.temp_boost<-performance(ROCRpred.boost, "auc")
auc_boost<-as.numeric(auc.temp_boost@y.values)
auc_boost
