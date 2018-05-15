setwd("C:\\F\\NMIMS\\DataScience\\Sem-2\\ML-R\\data")

#Read the dataset
dataset<- read.csv("Salary_Data.csv", header = TRUE, stringsAsFactors = F)
fix(dataset)

#Splittig the dataset into train and test dataset
library(caTools)
set.seed(123)
split<-sample.split(dataset$Salary, SplitRatio = 2/3)
training_set<- subset(dataset, split==TRUE)
test_set<- subset(dataset, split==FALSE)


#Compute the linear Model
regressor<- lm(formula = Salary~YearsExperience, data=training_set)
summary(regressor)

#Predict based on the Model
y_pred<-predict(regressor, newdata = test_set)


