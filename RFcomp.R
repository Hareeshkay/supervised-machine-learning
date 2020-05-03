library(randomForest) #package required for random forest
company<-read.csv(file.choose()) #selecting the dataset
View(company)
str(company)

result<-NULL #creating a null object
result<-ifelse(company$Sales>9,"good","bad") #using ifelse conditions to categorise the dataset
company[,"result"]<-result 
View(result)

company$ShelveLoc <- as.factor(company$ShelveLoc) #converting shelveloc,urban,US,salesresult into factor
company$Urban <- as.factor(company$Urban)
company$US <- as.factor(company$US)
company$result <- as.factor(company$result)

companygood<-company[company$result=="good",] #making a set of category based on result as good
View(companygood)
companybad<-company[company$result=="bad",] #making a set of category based on result as bad
View(companybad)

train<-rbind(companygood[1:70,],companybad[1:200,]) #building train data
test<-rbind(companygood[71:113,],companybad[201:287,]) #building test data
View(train)
View(test)
str(train)

fit.forest <-randomForest(result~.,data = train,na.action = na.roughfix,importance=TRUE) #building randomforest model
fit.forest$ntree
mean(train$result==predict(fit.forest,train)) #evaluating the mean
pred<-predict(fit.forest,test) #prediction value    
pred     
mean(pred==test$result)

library(gmodels) #package required for cross tabling
cross<-CrossTable(test$result,pred)
