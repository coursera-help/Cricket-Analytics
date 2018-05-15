library(RCurl)
binData<-getBinaryURL("https://archive.ics.uci.edu/ml/machine-learning-databases/00296/dataset_diabetes.zip",ssl.verifypeer=FALSE)
conObj<-file("dataset_diabetes.zip", open = "wb")
writeBin(binData, conObj)
close(conObj)
files<-unzip("dataset_diabetes.zip")
diabData<-read.csv(files[1], header = TRUE, stringsAsFactors = FALSE)
dim(diabData)


#Detect NAs
detectNAs<-function(x){
  return(sum(is.na(x)))
}
sapply(diabData, detectNAs)

diabData<-subset(diabData,select = -c(encounter_id, patient_nbr))

diabData[diabData=="?"]<-NA
diabData[is.na(diabData)]<-0
diabData$readmitted<-ifelse(diabData$readmitted=='<30', 1, 0)
outcomeName<-"readmitted"
diabData<-subset(diabData, select = -c(diag_1, diag_2, diag_3))

for(colname in colnames(diabData)) {
  print(paste(colname, length(unique(diabData[, colname]))))
  for(newcol in unique(diabData[, colname])) {
    if(!is.na(newcol))
      diabData[, paste0(colname, "_", newcol)]<- ifelse(diabData[, colname]==newcol, 1, 0)
    
  }
  diabData<-diabData[, setdiff(names(diabData), colname)]
}

fit<-lm(readmitted ~., data=traindf)
fit
summary(fit)
preds<-predict(fit, testdf[, predictorNames], se.fit = TRUE)

library(pROC)
print(auc(testdf[, outcomeName], preds$fit))

library(foreach)
library(doParallel)
c1<-makeCluster(8)
registerDoParallel(c1)
length_divisor<-20

predictions<-foreach(m=1:400, .combine = cbind) %dopar% {
  sampleRows<-sample(nrow(traindf), size=floor((nrow(traindf)/length_divisor)))
  fit<-lm(readmitted~., data=traindf[sampleRows,])
  predictions<-data.frame(predict(object = fit, testdf[, predictorNames], se.fit = TRUE)[[1]])
}


stopCluster(c1)

library(pROC)
auc(testdf[,outcomeName], rowMeans(predictions))








#diabData<-diabData[sapply(diabData, function(x) length(levels(factor(x, exclude = NULL)))>1)]

colnames(diabData)<-gsub(x=colnames(diabData), pattern="[[:punct:]]", replacement = "_")
split<-sample(nrow(diabData), floor(0.5*nrow(diabData)))
traindf<-diabData[split,]
testdf<-diabData[-split,]
predictorNames<-setdiff(names(traindf), outcomeName)











View(diabData)