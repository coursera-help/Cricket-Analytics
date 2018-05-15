setwd("C:\\F\\NMIMS\\DataScience\\Sem-2\\ML-R\\data")

#Read the dataset
dataset<- read.csv("50_Startups.csv", header = TRUE, stringsAsFactors = F)
View(dataset)

#Convert the categorical variables into factors
dataset$State<-factor(dataset$State, levels = c('New York', 'California', 'Florida'), labels = c(1,2,3))

#Splittig the dataset into train and test dataset
library(caTools)
set.seed(123)
split<-sample.split(dataset$Profit, SplitRatio = 0.8)
training_set<- subset(dataset, split==TRUE)
test_set<- subset(dataset, split==FALSE)


#Compute the linear Model
regressor<- lm(formula = Profit~., data=training_set)
summary(regressor)

#Predict based on the Model
y_pred<-predict(regressor, newdata = test_set)


#Backward Elimination
regressor<- lm(formula = Profit~R.D.Spend+Administration+Marketing.Spend+State, data=training_set)
summary(regressor)

#Remove State2 and State3 having sig.Level>0.05
regressor<- lm(formula = Profit~R.D.Spend+Administration+Marketing.Spend, data=training_set)
summary(regressor)

#Remove Administration having SL>0.05
regressor<- lm(formula = Profit~R.D.Spend+Marketing.Spend, data=training_set)
summary(regressor)

#We can either remove MarketingSpend or keep it as its value is closer to SL
regressor<- lm(formula = Profit~R.D.Spend, data=training_set)
summary(regressor)

#Predict
y_pred<- predict(regressor, newdata = test_set)