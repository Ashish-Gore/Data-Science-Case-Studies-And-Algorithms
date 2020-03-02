library(forecast)
library(timeSeries)
cococola <- readxl::read_xlsx(file.choose())
cococola$Quarter
plot(cococola$Sales)                       # increased basis of quaterly
plot(log(cococola$Sales))
boxplot(cococola$Sales)                    # No outliers
str(cococola)

#--------------------- pre processing data -----------------------------#
quaterly <- data.frame(outer(rep(c("Q1","Q2","Q3","Q4"),length= nrow(cococola)),c("Q1","Q2","Q3","Q4"),'==')+0)
colnames(quaterly) <- c("Q1","Q2","Q3","Q4")
cococola2 <- cbind(cococola,quaterly)
cococola2["t"] <- 1:42
cococola2["log"] <- log(cococola2$Sales)
cococola2["t_sq"] <- (cococola2$t)*(cococola2$t)
head(cococola2)
attach(cococola2)

#--------------------- splitting of data ----------------------------#
trn <- cococola2[1:30,]
tst <- cococola2[31:42,]


#--------------------------- linear model ---------------------------#
linear_mod <- lm(Sales~t, data = trn)
summary(linear_mod)                       #0.7079
linear_pred <- data.frame(predict(linear_mod,interval = 'predict',newdata=tst))
linear_rmse <- sqrt(mean((tst$Sales-linear_pred$fit)^2,na.rm = T))
linear_rmse                               #714.0144

#-------------------------- exp model ------------------------------#
exp_model <- lm(log~t , data = trn)
summary(exp_model)                      #0.7067
exp_pred <- data.frame(predict(exp_model,interval = 'predict',newdata = tst))
exp_rmse <- sqrt(mean((tst$Sales-exp_pred$fit)^2,na.rm = T))
exp_rmse                               #4252.189

#------------------------ quadratic model -------------------------#
quad_mod <- lm(Sales~t+t_sq,data = trn)
summary(quad_mod)                     #0.8097
quad_pred <- data.frame(predict(quad_mod,interval = "predict",newdata = tst))
quad_rmse <- sqrt(mean((tst$Sales-quad_pred$fit)^2,na.rm = T))
quad_rmse                             #646.2715

#------------------------ additive seasonality -------------------#
add_seas <- lm(Sales~Q1+Q2+Q3+Q4,data = trn)
summary(add_seas)                     #0.1544
add_seas_pred <- data.frame(predict(add_seas,interval = "predict",newdata = tst))
add_seas_rmse <- sqrt(mean((tst$Sales-add_seas_pred$fit)^2,na.rm = T))
add_seas_rmse                         #1778.007

#---------------------- additive seasonality with linear -----------#
add_seast <- lm(Sales~t+Q1+Q2+Q3+Q4,data = trn)
summary(add_seast)                     #0.8457
add_seast_pred <- data.frame(predict(add_seast,interval = "predict",newdata = tst))
add_seast_rmse <- sqrt(mean((tst$Sales-add_seast_pred$fit)^2,na.rm = T))
add_seast_rmse                         #637.9405

#---------------------- additive seasonality with quadratic -----------#
add_seasq <- lm(Sales~t+t_sq+Q1+Q2+Q3+Q4,data = trn)
summary(add_seasq)                     #0.9535
add_seasq_pred <- data.frame(predict(add_seasq,interval = "predict",newdata = tst))
add_seasq_rmse <- sqrt(mean((tst$Sales-add_seasq_pred$fit)^2,na.rm = T))
add_seasq_rmse                         #586.0533

#---------------------- multiplicative seasonality -------------------#
mul_seas_model <- lm(log~Q1+Q2+Q3+Q4,data = trn)
summary(mul_seas_model)                #0.1699
mul_seas_pred <- data.frame(predict(mul_seas_model,interval = 'predict',newdata = tst))
mul_seas_rmse <- sqrt(mean((tst$Sales-mul_seas_pred$fit)^2,na.rm = T))
mul_seas_rmse                          #4252.639

#---------------------- multiplicative seasonality with linaer -------------------#
mul_seast_model <- lm(log~t+Q1+Q2+Q3+Q4,data = trn)
summary(mul_seast_model)                #0.8586
mul_seast_pred <- data.frame(predict(mul_seast_model,interval = 'predict',newdata = tst))
mul_seast_rmse <- sqrt(mean((tst$Sales-mul_seast_pred$fit)^2,na.rm = T))
mul_seast_rmse                          #4252.186
#################################################################################
table_formate <- data.frame(c("linear_rmse","exp_rmse","quad_rmse","add_seas_rmse","add_seasq_rmse","add_seast_rmse","mul_seas_rmse","mul_seast_rmse"),c(linear_rmse,exp_rmse,quad_rmse,add_seas_rmse,add_seasq_rmse,add_seast_rmse,mul_seas_rmse,mul_seast_rmse))
colnames(table_formate) <- c("model","RMSE")
View(table_formate)
#--------------------------- final model ----------------------#
finalmodel <- lm(Sales~t+t_sq+Q1+Q2+Q3+Q4,data = cococola2)
summary(finalmodel)



############################ auto.arima ##########################

cococola3 <- as.ts(cococola$Sales)
cococola_ts <- ts(data = cococola3,start = c(1986,1),end = c(1996,2),frequency = 4)
class(cococola_ts)
cycle(cococola_ts)
summary(cococola_ts)

decomp <- decompose(cococola_ts,"multiplicative")
plot(decomp)
boxplot(cococola_ts~cycle(cococola_ts))

# auto arima model
arimamodel <- auto.arima(cococola_ts,ic="aic",trace = T)
plot.ts(arimamodel$residuals)
#ARIMA(p,d,q) =(0,1,0)

#verifying p,d,q
acf(arimamodel$residuals)#q=0
acf(diff(arimamodel$residuals))#d=1
pacf(arimamodel$residuals)#p=0

#forecasting
forecastmodel <- forecast(arimamodel,level = c(95),h=10*4)
plot(forecastmodel)


#testing 
Box.test(arimamodel$residuals,lag=5,type = "Ljung-Box")
Box.test(arimamodel$residuals,lag=10,type="Ljung-Box")


#pvalues are smaller