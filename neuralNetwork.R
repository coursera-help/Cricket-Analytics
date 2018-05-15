Boston
Bos <- Boston
Bos
#MAKE histogram on the target variable
hist(Bos$medv)
#here no missing values so go ahead
maxValue <- apply(Bos, 2,max)
minValue <- apply(Bos, 2,min)

#split in test and train dataset

df <- as.data.frame(scale(Bos,center=minValue,scale = maxValue-minValue))
ind <- sample(1:nrow(df), 400)
train_df <- df[ind, ]
testdf <- df[-ind,]
#Settings for ANN
#medv -- target variable
allVars <- colnames(df)
predictorVars <- allVars[!allVars %in%"medv"]
predictorVars <- paste(predictorVars, collapse = "+")
from=as.formula(paste("medv~", predictorVars, collapse = "+"))
#hidden layer with every layer having 2 nodes
library(neuralnet)
neuralModel <- neuralnet(formula=from, hidden=c(4,2), linear.output = T, data = train_df)
plot(neuralModel)
predictions <- compute(neuralModel, testdf[,1:13])
str(predictions)
# For predictions we need to descale the variables to get the output
predictions <- predictions$net.result*(max(testdf$medv)-min(testdf$medv))+min(testdf$medv)
actualValues <- (testdf$medv)*(max(testdf$medv)-(min(testdf$medv)))+min(testdf$medv)
MSE <- sum((predictions-actualValues)^2)/nrow(testdf)
MSE
plot(testdf$medv, predictions, col='blue', main='Real Vs Predicted', pch=1,cex=0.9, type="p", xlab = "actual", ylab = "predicted" )
abline(0, 1, col="black")



################ Classification Problem ##############
library(neuralnet)
library(caTools)
diabData_nn<-read.csv("diabetes.csv", header = TRUE, stringsAsFactors = FALSE)
diabData_nn_1<-as.data.frame(scale(diabData_nn[,1:8]))
diabData_nn<-cbind(diabData_nn_1, Outcome=diabData_nn$Outcome)
head(diabData_nn)
split<-sample.split(diabData_nn$Outcome, SplitRatio=0.8)
train_set_nn<-subset(diabData_nn, split==TRUE)
test_set_nn<-subset(diabData_nn, split==FALSE)

neuralModel_diab <- neuralnet(Outcome~(Pregnancies+Glucose+BloodPressure+SkinThickness+Insulin+BMI+DiabetesPedigreeFunction+Age)
                         , hidden=1, linear.output = T, data = train_set_nn)
plot(neuralModel_diab)
neuralModel_pred<-compute(neuralModel_diab, test_set_nn[,1:8])
diab_nn_result<-neuralModel_pred$net.result

detach(package:neuralnet,unload = T)

conf_nn<-table(neuralModel_pred$net.result>0.5, test_set_nn$Outcome)
accuracy_nn<-sum(diag(conf_nn))/sum(conf_nn)
accuracy_nn



###### ROCR
ROCRpred.nn<-prediction(diab_nn_result, test_set_nn$Outcome)
ROCRpref.nn<-performance(ROCRpred.nn, "tpr", "fpr")
plot(ROCRpref.nn, colorize=TRUE)
auc.temp_nn<-performance(ROCRpred.nn, "auc")
auc_nn<-as.numeric(auc.temp_nn@y.values)
auc_nn
