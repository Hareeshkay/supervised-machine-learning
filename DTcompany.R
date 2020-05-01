library(caret) #package neede for crosstabling
library(C50) #package needed for DT
  company<-read.csv(file.choose()) #importing dataset
View(company)
str(company)
summary(company)

Sales_Result <- NULL #creating a null rep
Sales_Result <- ifelse(company$Sales > 10,1,0) #converting the sales data into categorical
company[,"Sales_Result"] <- Sales_Result #adding it in dataset


company$ShelveLoc <- as.factor(company$ShelveLoc) #converting shelveloc,urban,US,salesresult into factor
company$Urban <- as.factor(company$Urban)
company$US <- as.factor(company$US)
company$Sales_Result <- as.factor(company$Sales_Result)


inTraininglocal<-createDataPartition(company$Sales_Result,p=.70,list=F)#creating partitioned data
training1<-company[inTraininglocal,]#training data
testing1<-company[-inTraininglocal,]#testing data
table(testing1$Sales_Result) 
View(training1)

trained_model <- C5.0(training1[,-c(12)], training1$Sales_Result)#DT model for train
plot(trained_model) #plotting my model
summary(trained_model) #summary

pred <- predict.C5.0(trained_model,testing1[,-12])#predicting model with test data
table(pred) #making table with prediction


acc<-c()
for (i in 1:100) #bagging method and fittree
{print(i)
  inTraininglocal<-createDataPartition(company$Sales_Result,p=.85,list=F)
  training1<-company[inTraininglocal,]
  testing1<-company[-inTraininglocal,]
fittree <- C5.0(training1[,-c(12)], training1$Sales_Result)
pred <- predict.C5.0(fittree,testing1[,-12])
a<-table(testing1$Sales_Result,pred)
acc<-c(acc,sum(diag(a))/sum(a))}
acc #output after bagging

summary(acc) #finding summary of bagging
mean(training1$Sales_Result==predict(trained_model,training1)) #finding the mean and accuracy
library(gmodels) #library for crosstable
CrossTable(testing1$Sales_Result,pred)
confusionMatrix(pred,testing1$Sales_Result) #confusion matrix
