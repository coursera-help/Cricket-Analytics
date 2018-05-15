####################### Classification Trees ##################
library (ISLR)
library (tree)
library(caTools)

setwd("C:\\F\\NMIMS\\DataScience\\Sem-2\\ML-R\\data\\")

diabData<-read.csv("diabetes.csv", header = TRUE, stringsAsFactors = FALSE)
dim(diabData)
str(diabData)
head(diabData)
diabData$Outcome<-ifelse(diabData$Outcome==1, "High", "Low")
diabData$Outcome<-as.factor(diabData$Outcome)

detectNAs<-function(x){
  return(sum(is.na(x)))
}
lapply(diabData, detectNAs)

########### Fit the tree ###############
set.seed(100) # set seed to replicate results
split<-sample.split(diabData$Outcome, SplitRatio=0.8)
train_set<-subset(diabData, split==TRUE)
test_set<-subset(diabData, split==FALSE)
tree.fit<-tree(Outcome~., train_set)
summary(tree.fit)
plot(tree.fit)
text(tree.fit, pretty=0)
tree.fit

############ Predictions
tree.prediction=predict(tree.fit , test_set ,type ="class")
conf<-table(tree.prediction ,test_set$Outcome)
accuracy=sum(diag(conf))/sum(conf)
accuracy



#### Now applying pruning
#### Through cross validation ddetermine the best node tree
set.seed(1)
cv.diabTree =cv.tree(tree.fit ,FUN=prune.misclass)
cv.diabTree
plot(cv.diabTree$size ,cv.diabTree$dev ,type="b")
plot(cv.diabTree$k ,cv.diabTree$dev ,type="b")
##### 12 or 7 yields the lowest deviance
prune.diabTree=prune.misclass(tree.fit, best=7)
### 7 node tree
plot(prune.diabTree)
text(prune.diabTree ,pretty =0)
tree.prediction_2=predict(prune.diabTree,  test_set, type="class")
conf<-table(tree.prediction_2 ,test_set$Outcome)
accuracy=sum(diag(conf))/sum(conf)
accuracy


prune.diabTree_1=prune.misclass (tree.fit ,best=12)
plot(prune.diabTree_1)
text(prune.diabTree_1 ,pretty =0)
tree.prediction_3=predict(prune.diabTree_1 , test_set, type="class")
conf<-table(tree.prediction_3 ,test_set$Outcome)
accuracy=sum(diag(conf))/sum(conf)
accuracy

library(ROCR)
org_Outcome<-ifelse(test_set$Outcome=="High", 1, 2)
pred_Outcome<-ifelse(tree.prediction_3=="High", 1, 2)
ROCRpred.tree<-prediction(pred_Outcome, org_Outcome)
ROCRpref.tree<-performance(ROCRpred.tree, "tpr", "fpr")
plot(ROCRpref.tree, colorize=TRUE)
auc.temp<-performance(ROCRpred.tree, "auc")
auc<-as.numeric(auc.temp@y.values)
auc

diab_pred<-predict(prune.diabTree_1, test_set, type="vector")
conf_diab3<-table(diab_pred[,2]>0.5, test_set$Outcome)
accuracy_diab3<-sum(diag(conf_diab3))/sum(conf_diab3)
accuracy_diab3

ROCRpred.tree<-prediction(diab_pred[,2], test_set$Outcome)
ROCRpref.tree<-performance(ROCRpred.tree, "tpr", "fpr")
plot(ROCRpref.tree, colorize=TRUE)
auc.temp<-performance(ROCRpred.tree, "auc")
auc<-as.numeric(auc.temp@y.values)
auc
