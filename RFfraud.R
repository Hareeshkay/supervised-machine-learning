library(randomForest) #package required for random forest
library(gmodels)#package required for cross tabling
library(caret)
fraudfor<-read.csv(file.choose()) #choosing the dataset
str(fraudfor)
View(fraudfor)

fraudass<-NULL #creating a null object
fraudass<-ifelse(fraudfor$Taxable.Income<=30000,"risky","good") #making a categorical data
fraudfor[,"fraudass"]<-fraudass

fraudfor$Undergrad<-as.factor(fraudfor$Undergrad) #converting it into factors for the required dataobjects 
fraudfor$Marital.Status<-as.factor(fraudfor$Marital.Status)
fraudfor$Urban<-as.factor(fraudfor$Urban)
fraudfor$fraudass<-as.factor(fraudfor$fraudass)

fraudgood<-fraudfor[fraudfor$fraudass=="good",]#making good as a category
View(fraudgood)
fraudrisky<-fraudfor[fraudfor$fraudass=="risky",]#making risky as a category

train<-rbind(fraudgood[1:300,],fraudrisky[1:90,]) #summing up the traindata
View(train)
test<-rbind(fraudgood[301:476,],fraudrisky[91:124,]) #summing up the test data
View(test)
str(train)

model<-randomForest(fraudass~.,data = train,na.action =na.roughfix,importance=TRUE) #building my randomforest model
model$ntree

mean(train$fraudass==predict(model,train)) #predicting the mean
pred<-predict(model,test)
pred
mean(pred==test$fraudass)
cross<-CrossTable(test$fraudass,pred) #cross tabling model with test
