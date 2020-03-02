data <- read.csv(file.choose())
library(e1071)
View(data)
corolla <- data[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(corolla)
summary(corolla)# 1st moment business decision
str(corolla)
attach(corolla)
library(psych)
describe(corolla)                          #2nd 3rd &4th moment business decision
plot(Price,Age_08_04)
plot(Price,KM)
plot(Price,HP)
plot(Price,cc)
plot(Price,Doors)
plot(Price,Gears)
plot(Price,Quarterly_Tax)
plot(Price,Weight)

pairs(corolla)
cor(corolla)
library(corpcor)
cor2pcor(cor(corolla))

#multiple linear regression

model1 <- lm(Price~.,data = corolla)
summary(model1)                             #R^2=0.8638
library(car)
vif(model1)                                 #no colinearity problem
rmse <- mean(model1$residuals^2)^.5         #1338.2584
pred <- predict(model1,corolla)
cor(pred,corolla$Price)                     #0.929
influence.measures(model1)
influenceIndexPlot(model1,id=list(col="red",cex=2,n=10))
influencePlot(model1,id=list(col='red',cex=2,n=10))
qqPlot(model1)
residualPlot(model1)
#cc , Doors have more influence

modelcc <- lm(Price~cc,data = corolla)
summary(modelcc)
modeldoors <- lm(Price~Doors,data = corolla)
summary(modeldoors)
modelccdoors <- lm(Price~cc+Doors,data = corolla)
summary(modelccdoors)


#deleting the influenced observations
model2 <- lm(Price~.,data = corolla[-c(81,222),])
summary(model2)                                     #R^2=0.8778 #doors are not influencing factors
rmse2 <- mean(model2$residuals^2)^.5                #1265
pred2 <- predict(model2,corolla)
cor(pred2,corolla$Price)                            #87.3
avPlots(model2)
residualPlot(model2)

#tranformation model
l=log(corolla[,-1])
corolla2 <- data.frame(l,corolla$Price)
#log model
model3 <- lm(corolla.Price~.,data = corolla2)
summary(model3)                                 #0.8353
rmse3 <- mean(model3$residuals^2)^.5            #1471
pred3 <- predict(model3,corolla2)
cor(pred3,corolla2$corolla.Price)              #91.3%
avPlots(model3)
influenceIndexPlot(model3,id=list(col='red',n=10,cex=1.5))
influencePlot(model3,id=list(col='red',n=10,cex=1.5))
qqPlot(model3)
residualPlot(model3)
vif(model3)
#log model with removing influence
model4 <- lm(corolla.Price~.,data = corolla2[-c(185,186)])
summary(model4)                            #0.8353
rmse4 <- mean(model4$residuals^2)^.5       #1471
pred4 <- predict(model4,corolla2)
cor(pred4,corolla2$corolla.Price)          #91.3%


#model2 has highest R^2 ,lesser (RMSE,acc) i.e best model 
plot(model2)