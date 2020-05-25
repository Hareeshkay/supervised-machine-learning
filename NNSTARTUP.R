startup<-read.csv(file.choose()) #importing the data set
View(startup)
str(startup)
library(plyr)#adding plyr package for revalue function
summary(startup)
#state coloumn is in alphabetics, so making them as numeric
startup$State<-as.numeric(revalue(startup$State,c("New York"="0", "California"="1","Florida"="2")))
startup<-as.data.frame(startup) #changing the str to data frame
normalize<-function(x){     #normalising the values
  return ( (x-min(x))/(max(x)-min(x)))
}
Startup_norm<-as.data.frame(lapply(startup,FUN=normalize)) #normalising usling lapply
train<-Startup_norm[1:35,] #framing train data
View(train)
test<-Startup_norm[36:50,] #test
install.packages("neuralnet") #packages required for NN model 
install.packages("nnet")
library(neuralnet)
library(nnet)
nnmodel<-neuralnet(Profit~.,data = train) #building nn model on train
str(nnmodel)
plot(nnmodel) #plotting model

modelstrenght<-compute(nnmodel,test[1:4]) #efficiency on test data
predictmodel<-modelstrenght$net.result #predicting the outcomes
predictmodel
modelstrenght$neurons
cor(predictmodel,test$Profit) #checking for correlation in model and test component
plot(predictmodel,test$Profit)
#Least sse value will be the best model
model2<-neuralnet(Profit~.,data = Startup_norm,hidden = c(5,3)) #new model by adding hidden values 
plot(model2) #plotting model
model2res<-compute(model2,test[1:4])#checking efficiecny with test
predictmodelA<-model2res$net.result
predictmodelA #new model prediction
cor(predictmodelA,test$Profit) #finding the correlation in new model and test component
plot(predictmodelA,test$Profit)
