library(e1071) #package needed for NB
library(class) 
library(gmodels)#For plotting the prediction
train<-read.csv(file.choose()) #importing train data
View(train)
str(train)
train$educationno <- as.factor(train$educationno) #choosing or converting factor education
class(train)
test<-read.csv(file.choose())#importing test data
View(test)
str(test)
test$educationno<-as.data.frame(test$educationno) #converting to dataframe for test data
str(test)
Model <- naiveBayes(train$Salary ~ ., data = train) #naive bayes model 
Model

Model_pred <- predict(Model,test) #testing model with test data
mean(Model_pred==test$Salary) #accuarcy 
