library(forecast)
library(timeSeries)
plastic <- read.csv(file.choose())
View(plastic)
str(plastic)
plot(plastic$Sales,type = "o")
plot(log(plastic$Sales),type = "o")
summary(plastic)

######################## pre processing the data ###############################

#creating dummy variables

month <- data.frame(outer(rep(month.abb,length=60),month.abb,"==")+0)
colnames(month) <- month.abb
View(month)
plastic2<- cbind(plastic,month)
plastic2["log1"] <- log(plastic$Sales)
plastic2["time1"] <- 1:60
plastic2["time1_sq"] <- (plastic2$time1)*(plastic2$time1)
attach(plastic2)

#splitting data to test and train
train1 <- plastic2[1:54,]
test1 <- plastic2[54:60,]

#======================== linear model ===============================#

linear_model <- lm(Sales~time1,data = train1)
summary(linear_model)                           #R^2 = 0.3105
linear_pred <- data.frame(predict(linear_model,newdata = test1,interval = "predict"))
linear_model_rmse <- sqrt(mean((linear_pred$fit-test1$Sales)^2,na.rm = T))
linear_model_rmse                               #252.3652


#======================== Exp model ==================================#

exp_model <- lm(log1~time1,data = train1)
summary(exp_model)                             #R^2 = 0.3065
exp_pred <- data.frame(predict(exp_model,interval = "predict",newdata = test1))
exp_model_rmse <- sqrt(mean((exp_pred$fit-test1$Sales)^2))
exp_model_rmse                                #1431.609

#------------------------ quadratic model -------------------------#
quad_mod <- lm(Sales~time1+time1_sq,data = train1)
summary(quad_mod)                     #0.3109
quad_pred <- data.frame(predict(quad_mod,interval = "predict",newdata = test1))
quad_rmse <- sqrt(mean((test1$Sales-quad_pred$fit)^2,na.rm = T))
quad_rmse                             #250.891

#------------------------ additive seasonality -------------------#
add_seas <- lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train1)
summary(add_seas)                     #0.1492
add_seas_pred <- data.frame(predict(add_seas,interval = "predict",newdata = test1))
add_seas_rmse <- sqrt(mean((test1$Sales-add_seas_pred$fit)^2,na.rm = T))
add_seas_rmse                         #134.3448

#---------------------- additive seasonality with linear -----------#
add_seast <- lm(Sales~time1+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train1)
summary(add_seast)                     #0.9724
add_seast_pred <- data.frame(predict(add_seast,interval = "predict",newdata = test1))
add_seast_rmse <- sqrt(mean((test1$Sales-add_seast_pred$fit)^2,na.rm = T))
add_seast_rmse                         #178.5941

#---------------------- additive seasonality with quadratic -----------#
add_seasq <- lm(Sales~time1+time1_sq+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train1)
summary(add_seasq)                     #0.9816
add_seasq_pred <- data.frame(predict(add_seasq,interval = "predict",newdata = test1))
add_seasq_rmse <- sqrt(mean((test1$Sales-add_seasq_pred$fit)^2,na.rm = T))
add_seasq_rmse                         #247.8046

#---------------------- multiplicative seasonality -------------------#
mul_seas_model <- lm(log1~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train1)
summary(mul_seas_model)                #0.696
mul_seas_pred <- data.frame(predict(mul_seas_model,interval = 'predict',newdata = test1))
mul_seas_rmse <- sqrt(mean((test1$Sales-mul_seas_pred$fit)^2,na.rm = T))
mul_seas_rmse                          #1431.702

#---------------------- multiplicative seasonality with linaer -------------------#
mul_seast_model <- lm(log1~time1+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train1)
summary(mul_seast_model)                #0.9805
mul_seast_pred <- data.frame(predict(mul_seast_model,interval = 'predict',newdata = test1))
mul_seast_rmse <- sqrt(mean((test1$Sales-mul_seast_pred$fit)^2,na.rm = T))
mul_seast_rmse                          #1431.473


table_formate <- data.frame(c("linear_model_rmse","exp_model_rmse","quad_rmse","add_seas_rmse","add_seasq_rmse","add_seast_rmse","mul_seas_rmse","mul_seast_rmse"),c(linear_model_rmse,exp_model_rmse,quad_rmse,add_seas_rmse,add_seasq_rmse,add_seast_rmse,mul_seas_rmse,mul_seast_rmse))
colnames(table_formate) <- c("model","RMSE")
View(table_formate)

#additive seasonality with linear has less RMSE 
#choosing it for forecasting

#--------------------------- final model ----------------------#
finalmodel <- lm(Sales~time1+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = plastic2)
finalmodel
summary(finalmodel)

##################### auto.arima method  ##########################

library(tseries)
plastic_ts <- as.ts(plastic$Sales)
plastic_ts <- ts(plastic_ts,start = c(1949,1),end = c(1953,12),frequency = 12)
class(plastic_ts)
start(plastic_ts)
end(plastic_ts)
sum(is.na(plastic_ts))
summary(plastic_ts)


decompdata1 <- decompose(plastic_ts,"multiplicative")
plot(decompdata1)
cycle(plastic_ts)


#################### model ######################
newmodel1 <- auto.arima(plastic_ts,ic = "aic",trace = T)
plot.ts(newmodel1$residuals)


# verifying p,d,q values using acf and pacf
acf(newmodel1$residuals)                        #q=0
pacf(newmodel1$residuals)                       #p=1


#forecasting the model
forecasting1 <- forecast(newmodel1,level = c(95),h=5*12)
plot(forecasting1)

######################## testing the model ##############
Box.test(newmodel1$residuals,lag = 5,type ="Ljung-Box" )
Box.test(newmodel1$residuals,lag = 2,type = "Ljung-Box")

# p values are smaller