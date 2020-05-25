library(neuralnet) #package required for NN
library(nnet)
library(plyr)
fire<-read.csv(file.choose())#Importing the data set
View(fire)
str(fire)
summary(fire)
#revaluing the month,day,sizecategory columns as they are not numberic or factors
fire$month<-as.numeric(revalue(fire$month,c("aug"="1","sep"="2","mar"="3","jul"="4","feb"="5","jun"="6")))
fire$day<-as.numeric(revalue(fire$day,c("fri"="1","mon"="2","sat"="3","sun"="4","thu"="5","tue"="6","wed"="7")))                       
fire$size_category<-as.numeric(revalue(fire$size_category,c("large"="1","small"="2")))
#normalising the data
normalise_data<-function(x){return((x-min(x))/(max(x)-min(x)))}
firenorm<-as.data.frame(lapply(fire,FUN = normalise_data))
summary(fire$size_category)
summary(firenorm$size_category)
firetrain<-firenorm[1:400,] #train data 
View(firetrain)
firetest<-firenorm[401:517,]#test data
View(firetest)
firemodel<-neuralnet(size_category~.,data = firetrain)#building the nn model
plot(firemodel)

modeltest<-compute(firemodel,firetest[1:30]) #testing the functionality of the model with test data
predictedsize<-modeltest$net.result
predictedsize
modeltest$neurons
cor(predictedsize,firetest$size_category)#checking the correlation of components 
plot(predictedsize,firetest$size_category)
# New model
model_5<-neuralnet(size_category~.,data= firenorm,hidden = c(5,3))#adding hidden layers to improve the perfomance
plot(model_5)
model_5_res<-compute(model_5,firetest[1:30])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,firetest$size_category)
plot(pred_strn_5,firetest$size_category)
