a=read.csv(file.choose())
view(a)
attach(a)
summary(a)
plot(Salary_hike,Churn_out_rate)
boxplot(a)
hist(a$Salary_hike)
hist(a$Churn_out_rate)
reg <- lm(Churn_out_rate~Salary_hike)
summary(reg)
predict(reg)
reg$residuals
confint(reg, level = 0.95)
predict(reg,interval = "predict")


#log
plot(log(Salary_hike),Churn_out_rate)
cor(log(Salary_hike),Churn_out_rate)
reg_log <-lm(Churn_out_rate~log(Salary_hike)) 
summary(reg_log)
predict(reg_log)
reg_log$residuals
confint(reg_log,level = 0.95)
predict(reg_log,interval = "predict")


#exp
plot(Salary_hike,log(Churn_out_rate))
cor(Salary_hike,log(Churn_out_rate))
reg_exp <- lm(log(Churn_out_rate)~Salary_hike)
summary(reg_exp)
predict(reg_exp)
reg_exp$residuals
confint(reg_exp,level = 0.95)
predict(reg_exp,interval = "predict")


#poly
plot(Salary_hike*Salary_hike,Churn_out_rate)
cor(Salary_hike*Salary_hike,Churn_out_rate)
reg_poly <- lm(Churn_out_rate~Salary_hike+I(Salary_hike^2))
summary(reg_poly)
predict(reg_poly)
reg_poly$residuals
confint(reg_poly,level=0.95)
predict(reg_poly, interval = "predict")


#cubic
plot(Salary_hike^3,Churn_out_rate)
cor(Salary_hike^3,Churn_out_rate)
reg_poly3 <-  lm(Churn_out_rate~Salary_hike+I(Salary_hike^2)+I(Salary_hike^3))
summary(reg_poly3)
confint(reg_poly3,level = 0.95)
predict(reg_poly3,interval = "predict")

#cubic module has higher value of r^2 value
plot(reg_poly3)
