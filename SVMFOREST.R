library(kernlab) #package required for svm
library(caret)#package required for mean
forestfire<-read.csv(file.choose()) #importing the data set
View(forestfire)
str(forestfire)
table(forestfire$size_category)
foresttrain<-forestfire[1:400,] #traindata
foresttest<-forestfire[401:517,] #test data

forestrbf<-ksvm(foresttrain$size_category~.,data=foresttrain,kernel="rbfdot") #rbfdot model
pred<-predict(forestrbf,foresttest)                
mean(pred==foresttest$size_category)

forestvanila<-ksvm(foresttrain$size_category~.,data=foresttrain,kernel="vanilladot") #vanilladot model
pred1<-predict(forestvanila,foresttest)
mean(pred1==foresttest$size_category)

forestbessel<-ksvm(foresttrain$size_category~.,data=foresttrain,kernel="besseldot") #besseldot model
pred2<-predict(forestbessel,foresttest)
mean(pred2==foresttest$size_category)

forestpoly<-ksvm(foresttrain$size_category~.,data=foresttrain,kernel="polydot") #polydot model
pred3<-predict(forestpoly,foresttest)
mean(pred3==foresttest$size_category)

