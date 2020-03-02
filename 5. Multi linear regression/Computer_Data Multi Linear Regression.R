a=read.csv(file.choose())
View(a)
b=a[,-c(1)]
View(b)
attach(b)
colnames(b)
str(b)
#creating dummy variables
b$cd=as.integer(factor(b$cd,levels = c('yes','no'),labels = c(1,0)))
b$multi=as.integer(factor(b$multi,levels = c('yes','no'),labels = c(1,0)))
b$premium=as.integer(factor(b$premium,levels = c('yes','no'),labels = c(1,0)))
View(b)
summary(b)#1st moment business decision
library(psych)
describe(b)#2nd ,3rd &4th moment business decision
library(e1071)
colnames(b)
library(GGally)
library(ggplot2)
ggplot(data = b)+geom_histogram(aes(x=price,),bins=40)
ggpairs(data = b)
pairs(b)

#multiple linear regression
model <- lm(price~.,data = b)
summary(model)                                      #R^2 0.7756
rmse <- mean(model$residuals^2)^.5                  #275.129
rmse
pred <- predict(model,newdata = b)
cor(pred,b$price)                                   #0.8807
library(car)
avPlots(model)
vif(model)                           #no colinearity problem
influenceIndexPlot(model,grid = T,id=list(n=10,cex=1.5,col="blue"))
influence.measures(model)
qqPlot(model)                       

#removing the influences over the given data
model2 <- lm(price~.,data = b[-c(1441,1701),])
summary(model2)                               #R^2 0.77
rmse2 <- mean(model2$residuals^2)^.5          #272
rmse2
pred2 <- predict(model2,b)
cor(pred2,b$price)                            #0.88
avPlots(model2)

#transformation technique
#log model
x=log(b[,-1])
b2 <- data.frame(b[,1],x)
colnames(b2)
View(b2)
model3 <- lm(b...1.~speed+hd+ram+screen+cd+multi+premium+ads+trend,data = b2)
summary(model3)                               #r^2 0.7426
rmse3 <- mean(model3$residuals^2)^.5
rmse3                                        #294.65
pred3 <- predict(model3,b2)
cor(pred3,b2$b...1.)                         #86.17%
influenceIndexPlot(model3,grid = T,id=list(n=10,cex=1.5,col="blue"))
influence2 <- as.integer(rownames(influencePlot(model3,id=list(n=5,cex=1.5,col='blue'))))
influence2
qqPlot(model3)
vif(model3)
#log with removing of influence
model4 <- lm(b...1.~speed+hd+ram+screen+cd+multi+premium+ads+trend,data = b2[-c(1441,1701)])
summary(model4)                             #r^2 0.7426
rmse4 <- mean(model4$residuals^2)^.5
pred4 <- predict(model4,b2)
cor(pred4,b2$b...1.)                       #86.17


#model2 has best (R^2,accuracy) value and lesser RMSE 
plot(model2)