library(readxl) #inorder to readxl file
bank<-read_excel(file.choose()) #importing the data set
View(bank)
str(bank)
attach(bank)
#building model
bank1<-glm(factor(y)~factor(job)+factor(marital)+factor(education)+factor(default)+balance+factor(housing)+factor(loan)+factor(contact)+day+factor(month)+duration+campaign+pdays+previous+factor(poutcome),family = "binomial",data = bank)
summary(bank1)

#second model by removing pdays and previous
bank2<-glm(factor(y)~factor(job)+factor(marital)+factor(education)+factor(default)+balance+factor(housing)+factor(loan)+factor(contact)+day+factor(month)+duration+campaign+factor(poutcome),family = "binomial",data = bank)
summary(bank2)

#finding probability value

prob <- predict(bank2,type=c("response"),bank)
prob
#forming confusion matrix
confusion<-table(prob>0.5,bank$y)
probo <- prob>0.5
table(probo)
confusion

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
Error <- 1-Accuracy
Error

# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob,bank$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained