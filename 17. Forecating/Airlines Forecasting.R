library(forecast)
airlines <- readxl::read_xlsx(file.choose())
View(airlines)
plot(airlines$Passengers,type = "o")
plot(log(airlines$Passengers),type = "o")
summary(airlines)
class(airlines)
str(airlines)
airlines$Month <- as.Date(airlines$Month)
str(airlines)

######################## pre processing the data ###############################

#creating dummy variables

month <- data.frame(outer(rep(month.abb,length=96),month.abb,"==")+0)
colnames(month) <- month.abb
View(month)
airlines2 <- cbind(airlines,month)
airlines2["log2"] <- log(airlines2$Passengers)
airlines2["time"] <- 1:96
airlines2["time_sq"] <- (airlines2$time)*(airlines2$time)
attach(airlines2)
#splitting data to test and train
train <- airlines2[1:85,]
test <- airlines2[86:96,]

#======================== linear model ===============================#

linear_model <- lm(Passengers~time,data = train)
summary(linear_model)                           #R^2 = 0.7975
linear_pred <- data.frame(predict(linear_model,newdata = test,interval = "predict"))
linear_model_rmse <- sqrt(mean((linear_pred$fit-test$Passengers)^2,na.rm = T))
linear_model_rmse                               #55.67417


#======================== Exp model ==================================#

exp_model <- lm(log2~time,data = train)
summary(exp_model)                             #R^2 = 0.8273
exp_pred <- data.frame(predict(exp_model,interval = "predict",newdata = test))
exp_model_rmse <- sqrt(mean((exp_pred$fit-test$Passengers)^2))
exp_model_rmse                                #329.6918

#------------------------ quadratic model -------------------------#
quad_mod <- lm(Passengers~time+time_sq,data = train)
summary(quad_mod)                     #0.8009
quad_pred <- data.frame(predict(quad_mod,interval = "predict",newdata = test))
quad_rmse <- sqrt(mean((test$Passengers-quad_pred$fit)^2,na.rm = T))
quad_rmse                             #50.65955

#------------------------ additive seasonality -------------------#
add_seas <- lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(add_seas)                     #0.1492
add_seas_pred <- data.frame(predict(add_seas,interval = "predict",newdata = test))
add_seas_rmse <- sqrt(mean((test$Passengers-add_seas_pred$fit)^2,na.rm = T))
add_seas_rmse                         #134.3448

#---------------------- additive seasonality with linear -----------#
add_seast <- lm(Passengers~time+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(add_seast)                     #0.9559
add_seast_pred <- data.frame(predict(add_seast,interval = "predict",newdata = test))
add_seast_rmse <- sqrt(mean((test$Passengers-add_seast_pred$fit)^2,na.rm = T))
add_seast_rmse                         #36.42285

#---------------------- additive seasonality with quadratic -----------#
add_seasq <- lm(Passengers~time+time_sq+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(add_seasq)                     #0.9609
add_seasq_pred <- data.frame(predict(add_seasq,interval = "predict",newdata = test))
add_seasq_rmse <- sqrt(mean((test$Passengers-add_seasq_pred$fit)^2,na.rm = T))
add_seasq_rmse                         #27.41271

#---------------------- multiplicative seasonality -------------------#
mul_seas_model <- lm(log2~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(mul_seas_model)                #0.1376
mul_seas_pred <- data.frame(predict(mul_seas_model,interval = 'predict',newdata = test))
mul_seas_rmse <- sqrt(mean((test$Passengers-mul_seas_pred$fit)^2,na.rm = T))
mul_seas_rmse                          #330.1927

#---------------------- multiplicative seasonality with linaer -------------------#
mul_seast_model <- lm(log2~time+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(mul_seast_model)                #0.9768
mul_seast_pred <- data.frame(predict(mul_seast_model,interval = 'predict',newdata = test))
mul_seast_rmse <- sqrt(mean((test$Passengers-mul_seast_pred$fit)^2,na.rm = T))
mul_seast_rmse                          #329.6603


table_formate <- data.frame(c("linear_model_rmse","exp_model_rmse","quad_rmse","add_seas_rmse","add_seasq_rmse","add_seast_rmse","mul_seas_rmse","mul_seast_rmse"),c(linear_model_rmse,exp_model_rmse,quad_rmse,add_seas_rmse,add_seasq_rmse,add_seast_rmse,mul_seas_rmse,mul_seast_rmse))
colnames(table_formate) <- c("model","RMSE")
View(table_formate)

#additive seasonality with quadratic has less RMSE and higher R^2 value
#choosing it for forecasting

#--------------------------- final model ----------------------#
finalmodel <- lm(Passengers~time+time_sq+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = airlines2)
finalmodel
summary(finalmodel)

##################### auto.arima method  ##########################

library(tseries)
airlines_ts <- as.ts(airlines$Passengers)
airlines_ts <- ts(airlines_ts,start = c(1995,1),end = c(2002,12),frequency = 12)
class(airlines_ts)
start(airlines_ts)
end(airlines_ts)
sum(is.na(airlines_ts))
summary(airlines_ts)

decompdata <- decompose(airlines_ts,"multiplicative")
plot(decompdata)
cycle(airlines_ts)
boxplot(airlines_ts~cycle(airlines_ts))


#################### model ######################
newmodel <- auto.arima(airlines_ts,ic = "aic",trace = T)
plot.ts(newmodel$residuals)


# verifying p,d,q values using acf and pacf
acf(newmodel$residuals)                        #q=0
pacf(newmodel$residuals)                       #p=1
acf(diff(newmodel$residuals))                  #d=1

#forecasting the model
forecasting <- forecast(newmodel,level = c(95),h=10*12)
plot(forecasting) 

######################## testing the model ##############
Box.test(newmodel$residuals,lag = 5,type ="Ljung-Box" )
Box.test(newmodel$residuals,lag = 10,type = "Ljung-Box")

# p values are smaller