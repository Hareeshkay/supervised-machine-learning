library(readxl) #inorder to read excel file
airforcast<-read_excel(file.choose())#importing the data set
View(airforcast)
x<-data.frame(outer(rep(month.abb,length=96),month.abb,"==" )+0)#creating dummy variables for months
colnames(x)<-month.abb #assigning coloumn names as well
View(x)
forecastdata<-cbind(airforcast,x) #combinig two direcorties 
View(forecastdata)
forecastdata["t"]<-c(1:96)#adding a t coloumn with count
forecastdata["log_pass"]<-log(forecastdata["Passengers"]) #adding log values of passenger data
forecastdata["tsquare"]<-forecastdata["t"]*forecastdata["t"]#finding the square of t values and new cloumn tsquare
train<-forecastdata[1:73,] #train
test<-forecastdata[74:96,] #test data

linearmodel<-lm(Passengers~t,data = train) #linear model
summary(linearmodel)
predictionmodel<-data.frame(predict(linearmodel,interval='predict',newdata = test))
rmsepred<-sqrt(mean((test$Passengers-predictionmodel$fit)^2,na.rm=T))
rmsepred

expomodel<-lm(log_pass~t,data = train)#exponential model
summary(expomodel)
predictionmodel1<-data.frame(predict(expomodel,interval = 'predict',newdata = test))
rmsexpo<-sqrt(mean((test$Passengers-exp(predictionmodel1$fit))^2,na.rm=T))
rmsexpo

quadmodel<-lm(Passengers~tsquare,data = train)  #quadratic model            
summary(quadmodel)              
predictionmodel2<-data.frame(predict(quadmodel,interval = 'predict',newdata = test))
quadpred<-sqrt(mean((test$Passengers-predictionmodel2$fit)^2,na.rm=T))
quadpred

#additive seasonality
addseas<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(addseas)
predictionmodel3<-data.frame(predict(addseas,interval = 'predict',newdata = test))
addpred<-sqrt(mean((test$Passengers-predictionmodel3$fit)^2,na.rm=T))
addpred

#additive seasonality quadratic 
Add_sea_Quad_model<-lm(Passengers~t+tsquare+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad

#multiplicative seasonality
multi_sea_model<-lm(log_pass~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea

#multiplicative additive seasonality
multi_add_sea_model<-lm(log_pass~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model)
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea

#making a table with all the rsme values
table_rmse<-data.frame(c("rmsepred","rmsexpo","quadpred","addpred","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmsepred,rmsexpo,quadpred,addpred,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

#multi_add_sea_model is the least RMSE value and best model