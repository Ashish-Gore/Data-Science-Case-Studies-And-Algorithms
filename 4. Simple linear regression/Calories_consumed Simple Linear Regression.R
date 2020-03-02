a = read.csv(file.choose())
View(a)
attach(a)
summary(a)
plot(a$Calories.Consumed , a$Weight.gained..grams.)
cor(a$Calories.Consumed,a$Weight.gained..grams.)
reg <- lm(Weight.gained..grams.~Calories.Consumed)
summary(reg)
pred= predict(reg)
reg$residuals
sum(reg$residuals)
mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(a))
sqrt(mean(reg$residuals^2))
confint(reg ,level=0.95)
predict(reg , interval= "predict")
library(ggplot2)
ggplot(data = a , aes(x=Calories.Consumed , y =Weight.gained..grams.))+geom_point(colour ="red")+ geom_line(colour="blue", data= a , aes(x=Calories.Consumed , y = pred))

# log
plot(log(Calories.Consumed), Weight.gained..grams.)
cor(log(Calories.Consumed), Weight.gained..grams.)
reg_log <- lm(Weight.gained..grams.~log(Calories.Consumed))
summary(reg_log)
predict(reg_log)
reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(a))
confint(reg_log,level = 0.95)
predict(reg_log, interval="confidence")

#exp
plot(Calories.Consumed, log(Weight.gained..grams.))
cor(Calories.Consumed, log(Weight.gained..grams.))
reg_exp <- lm(log(Weight.gained..grams.)~Calories.Consumed)
summary(reg_exp)
predict(reg_exp)
reg_exp$residuals
sqrt(sum(reg_exp$residuals^2)/nrow(a))
confint(reg_exp, level = 0.95)
predict(reg_exp,interval = "predict")

#poly
plot(Calories.Consumed*Calories.Consumed , Weight.gained..grams.)
cor(Calories.Consumed*Calories.Consumed , Weight.gained..grams.)
reg_poly <- lm(Weight.gained..grams.~ Calories.Consumed+I(Calories.Consumed^2))
summary(reg_poly)
predict(reg_poly)
reg_poly$residuals
confint(reg_poly, level=0.95)
predict(reg_poly , interval = "predict")

#poly cubic
plot(Calories.Consumed^3, Weight.gained..grams.)
cor(Calories.Consumed^3, Weight.gained..grams.)
reg_poly3 <- lm(Weight.gained..grams.~Calories.Consumed+I(Calories.Consumed^2)+I(Calories.Consumed^3))
summary(reg_poly3)
predict(reg_poly3)
reg_poly3$residuals
confint(reg_poly3, level=0.95)
predict(reg_poly3, interval="predict")

#poly cubic model has higher value of adjusted r^2 value
plot(reg_poly3)
