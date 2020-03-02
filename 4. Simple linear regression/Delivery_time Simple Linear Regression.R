a=read.csv(file.choose())
View(a)
attach(a)
summary(a)
plot(a$Sorting.Time,a$Delivery.Time)
cor(a$Sorting.Time,a$Delivery.Time)
boxplot(a)
hist(a$Sorting.Time)
hist(a$Delivery.Time)
reg <- lm(Delivery.Time~Sorting.Time)
summary(reg)
predict(reg)
reg$residuals
confint(reg,level=0.95)
predict(reg,interval = "predict")


#log 
plot(log(Sorting.Time),Delivery.Time)
cor(log(Sorting.Time),Delivery.Time)
reg_log <- lm(Delivery.Time~log(Sorting.Time))
summary(reg_log)
predict(reg_log)
reg_log$residuals
confint(reg_log,level=0.95)
predict(reg_log,interval = "predict")


#exp
plot(Sorting.Time,log(Delivery.Time))
cor(Sorting.Time,log(Delivery.Time))
reg_exp <- lm(log(Delivery.Time)~Sorting.Time)
summary(reg_exp)
predict(reg_exp)
reg_exp$residuals
confint(reg_exp,level = 0.95)
predict(reg_exp,interval = "predict")


#poly
plot(Sorting.Time*Sorting.Time,Delivery.Time)
cor(Sorting.Time*Sorting.Time,Delivery.Time)
reg_poly <- lm(Delivery.Time~Sorting.Time+I(Sorting.Time^2))
summary(reg_poly)
predict(reg_poly)
reg_poly$residuals
confint(reg_poly, level = 0.95)
predict(reg_poly, interval = "predict")


#cubic 
plot((Sorting.Time^3), Delivery.Time)
cor(Sorting.Time^3,Delivery.Time)
reg_poly3 <- lm(Delivery.Time~Sorting.Time+I(Sorting.Time^2)+I(Sorting.Time^3))
summary(reg_poly3)
predict(reg_poly3)
reg_poly3$residuals
confint(reg_poly3,level = 0.95)
predict(reg_poly3,interval = "predict")

#exponential model has higher value of r^2 value
plot(reg_exp)