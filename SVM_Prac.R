##### SVM Practice ############
setwd("C:\\F\\NMIMS\\DataScience\\Sem-2\\ML-R\\data\\")
library(e1071)

iristData<-read.csv("irisData.csv", header = TRUE, stringsAsFactors = FALSE)
dim(iristData)
str(iristData)

iristData$iris[iristData$iris=="Iris-setosa"] = 0
iristData$iris[iristData$iris=="Iris-versicolor"] = 1
iristData$iris[iristData$iris=="Iris-virginica"] = 2

head(iristData)
cat("\014")
x=data.matrix(iristData[,-5])
y=as.factor(iristData[,5])
dat=data.frame(x=x, y=y)
plot(x, col=y)

######## Model Fit ########
svmfit_iris =svm(y~., data=dat , kernel ="linear", cost =1, scale =FALSE)
summary(svmfit_iris)
svmfit_iris$index
table(true=y, pred=predict (svmfit_iris,newdata=dat[,-5]))



tune.out_iris=tune(svm ,y~.,data=dat ,kernel ="linear",
              ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100) ))

summary (tune.out_iris)
bestmod_iris=tune.out_iris$best.model
summary(bestmod_iris)
table(true=y, pred=predict (bestmod_iris, newdata=dat[,-5]))


######## now split into train and test ##############
train=sample(200, 100)
svmfit_1=svm(y~., data=dat [train ,], kernel ="linear",cost =1)
summary(svmfit_1)
table(true=dat[-train, "y"], pred=predict (svmfit_1, newdata=dat[-train, ]))




tune.out_1=tune(svm ,y~.,data=dat[train,] ,kernel ="linear",
              ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100)))

table(true=dat[-train, "y"], pred=predict (tune.out_1$best.model, newdata=dat[-train, ]))



diabData<-read.csv("diabetes.csv", header = TRUE, stringsAsFactors = FALSE)
split<-sample.split(diabData$Outcome, SplitRatio=0.8)
train_set<-subset(diabData, split==TRUE)
test_set<-subset(diabData, split==FALSE)

tune.out_svm_diab=tune(svm ,Outcome~.,data=train_set ,kernel ="linear",
                ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100)))
pred_svm=predict (tune.out_svm_diab$best.model, newdata=test_set)
conf_svm<-table(as.data.frame(pred_svm)>0.5, test_set$Outcome)
conf_svm
accuracy_svm<-sum(diag(conf_svm))/sum(conf_svm)
accuracy_svm


########### ROCR SVM #############
ROCRpred.svm<-prediction(pred_svm, test_set$Outcome)
ROCRpref.svm<-performance(ROCRpred.svm, "tpr", "fpr")
plot(ROCRpref.svm, colorize=TRUE)
auc.temp_svm<-performance(ROCRpred.svm, "auc")
auc_svm<-as.numeric(auc.temp_svm@y.values)
auc_svm
