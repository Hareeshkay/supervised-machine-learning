library(kernlab) #package required for svm
library(caret)
salarytrain<-read.csv(file.choose()) #importing the traindata set
View(salarytrain)
salarytest<-read.csv(file.choose()) #importing the testdata set
table(salarytrain$Salary)
str(salarytrain)

modelrdf<-ksvm(salarytrain$Salary~.,data=salarytrain,kernel="rbfdot") #model of svm with kernel rdfdot
pred<-predict(modelrdf,newdata=salarytest)
mean(pred==salarytest$Salary)

# kernal = vanilladot
modelvanila<-ksvm(salarytrain$Salary~.,data=salarytrain,kernel="vanilladot")
pred1<-predict(modelvanila,newdata=salarytest)
mean(pred1==salarytest$Salary)

# kernal = besseldot
model_besseldot<-ksvm(salarytrain$Salary ~.,data = salarytrain,kernel = "besseldot")
pred2<-predict(model_besseldot,newdata=salarytest)
mean(pred2==salarytest$Salary)

# kernel = polydot

model_poly<-ksvm(salarytrain$Salary ~.,data = salarytrain,kernel= "polydot")
pred3<-predict(model_poly,newdata = salarytest)
mean(pred3==salarytest$Salary) # 
