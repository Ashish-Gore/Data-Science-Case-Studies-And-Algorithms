a=read.csv(file.choose())
View(a)
attach(a)
summary(a)
plot(a$YearsExperience,a$Salary)
cor(YearsExperience,Salary)
boxplot(a)
reg <- lm(Salary~YearsExperience)
summary(reg)
predict(reg)
reg$residuals
confint(reg,level = 0.95)
predict(reg,interval = "predict")


#log
plot(log(YearsExperience),Salary)
cor(log(YearsExperience),Salary)
reg_log <- lm(Salary~log(YearsExperience))
summary(reg_log)
predict(reg_log)
reg_log$residuals
confint(reg_log,level = 0.95)
predict(reg_log,interval = "predict")


#exp
plot(YearsExperience,log(Salary))
cor(YearsExperience,log(Salary))
reg_exp <- lm(log(Salary)~YearsExperience)
summary(reg_exp)
predict(reg_exp)
reg_exp$residuals
confint(reg_exp,level = 0.95)
predict(reg_exp,interval = "predict")

#poly
plot(YearsExperience*YearsExperience,Salary)
cor(YearsExperience*YearsExperience,Salary)
reg_poly <- lm(Salary~YearsExperience+I(YearsExperience^2))
summary(reg_poly)
predict(reg_poly)
reg_poly$residuals
confint(reg_poly,level = 0.95)
predict(reg_poly,interval = "predict")


#cubic
plot(YearsExperience^3,Salary)
cor(YearsExperience^3,Salary)
reg_poly3 <- lm(Salary~YearsExperience+I(YearsExperience^2)+I(YearsExperience^3))
summary(reg_poly3)
predict(reg_poly3)
reg_poly3$residuals
confint(reg_poly3,level = 0.95)
predict(reg_poly3,interval = "predict")


#cubic model has highest r^2 value 
plot(reg_poly3)