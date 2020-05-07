library(readr)
glass <- read.csv(file.choose())
class(glass)
View(glass)
table(glass$Type)
round(prop.table(table(glass$Type))*100,1)
summary(glass)
glass1=sample(2,nrow(glass),replace=T,prob=c(0.8,0.2))

glass1_train=glass[glass1==1,]
glass1_test=glass[glass1==2,]

normalized_data=function(x){
  return((x-min(x))/(max(x)-min(x)))
}
glass_norm_train=as.data.frame(lapply(glass1_train[,-10],normalized_data))
glass_norm_test=as.data.frame(lapply(glass1_test[,-10],normalized_data))


glass1_train_lable=glass1_train[,10]

glass1_test_lable=glass1_test[,10]
library(class)
library(caret)

glass_pred=knn(train=glass_norm_train, test=glass_norm_test,cl=glass1_train_lable,k=1)

library(gmodels)
# Create cross table of predicted and actual
CrossTable( x =  glass1_test_lable, y = glass_pred,prop.chisq=FALSE)
mean(glass_pred==glass1_test_lable)

glass_pred=knn(train=glass_norm_train, test=glass_norm_test,cl=glass1_train_lable,k=3)

# Create cross table of predicted and actual
CrossTable( x =  glass1_test_lable, y = glass_pred,prop.chisq=FALSE)
mean(glass_pred==glass1_test_lable)
confusionMatrix(glass_pred)

glass_pred=knn(train=glass_norm_train, test=glass_norm_test,cl=glass1_train_lable,k=5)

# Create cross table of predicted and actual
CrossTable( x =  glass1_test_lable, y = glass_pred,prop.chisq=FALSE)
mean(glass_pred==glass1_test_lable)

glass_pred=knn(train=glass_norm_train, test=glass_norm_test,cl=glass1_train_lable,k=7)

# Create cross table of predicted and actual
CrossTable( x =  glass1_test_lable, y = glass_pred,prop.chisq=FALSE)
mean(glass_pred==glass1_test_lable)