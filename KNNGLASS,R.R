library(readr)
glass<-read.csv(file.choose())
View(glass)
structure(glass)
dim(glass)
str(glass)
summary(glass)
normalise_data<-function(x){return((x-min(x))/(max(x)-min(x)))}
glassnorm<-as.data.frame(lapply(glass[1:9],normalise_data))
View(glassnorm)
glass_train<-glassnorm[1:160,]
View(glass_train)
class(glass_train)
glass_test<-glassnorm[161:214,]
class(glass_test)
glas_train_labels<-glass[1:160,10]
View(glas_train_labels)
glass_test_labels<-glass[161:214,10]
library(class)
model_pred<-knn(glass_train,glass_test,cl=glas_train_labels,k=11)

library(caret)
confusion<-(table(model_pred, glass_test_labels))
confusion

accuracy<- sum(diag(confusion))/sum(confusion)
accuracy

#trying KNN for different K values

model_pred<-knn(glass_train,glass_test,cl=glas_train_labels,k=21)
confusion<-(table(model_pred, glass_test_labels))
confusion

accuracy<- sum(diag(confusion))/sum(confusion)
accuracy

model_pred<-knn(glass_train,glass_test,cl=glas_train_labels,k=14)
confusion<-(table(model_pred, glass_test_labels))
confusion

accuracy<- sum(diag(confusion))/sum(confusion)
accuracy

model_pred<-knn(glass_train,glass_test,cl=glas_train_labels,k=9)
confusion<-(table(model_pred, glass_test_labels))
confusion

accuracy<- sum(diag(confusion))/sum(confusion)
accuracy


model_pred<-knn(glass_train,glass_test,cl=glas_train_labels,k=3)
confusion<-(table(model_pred, glass_test_labels))
confusion

accuracy<- sum(diag(confusion))/sum(confusion)
accuracy
