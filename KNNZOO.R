library(readr)
animals<-read.csv(file.choose()) #importing the dataset
View(animals)
structure(animals)#checking the structure of dataset
dim(animals)#checking the dimension of data set
summary(animals[c("hair","feathers","eggs")]) #verifying the summary need to know the format and range of datapoints
normalize_data<-function(x){return((x-min(x))/(max(x)-min(x)))} #we need to normalise the data for better accuracy
animals_norm<-as.data.frame(lapply(animals[2:17],normalize_data)) #making an object with normalised data
View(animals_norm)
animals_train<-animals_norm[1:75,] #building my train data
dim(animals_train)
View(animals_train)
animals_test<-animals_norm[76:101,] #building my test data
dim(animals_test)
View(animals_test)
animals_train_labels<-animals[1:75,18] #making my train labels
View(animals_train_labels)
class(animals_train_labels)
animals_test_labels<-animals[76:101,18] #making test labels
class(animals_test_labels)
library(class) #package needed for knn
animals_pred<-knn(animals_train,animals_test,cl=animals_train_labels,k=7) #knn model 
class(animals_pred)
library(gmodels)
# cross table
CrossTable(x = animals_test_labels, y = animals_pred,
           prop.chisq=FALSE)

library(caret) #confusion matrix and accuracy
confusionMatrix(table(animals_pred,animals_test_labels)) 
plot(animals_pred,main="Classification of animals",xlab="Types of animals")

