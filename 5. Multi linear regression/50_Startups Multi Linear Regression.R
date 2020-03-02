a=read.csv(file.choose())
View(a)
str(a)
#creating dummy variables for state
a$State <-as.integer(factor(a$State,levels = c('New York','California','Florida'),labels=c(1,2,3)))
View(a)
summary(a)#1st moment business decision
attach(a)
class(a)
library(psych)
describe(a)#2nd ,3rd & 4th moment business decision
plot(Profit,R.D.Spend,col=c("green","red"))
plot(Profit,Marketing.Spend,col=c("green","red"))
plot(Profit,Administration,col=c("green","red"))
boxplot(Profit,R.D.Spend,col=c("green","red"))
boxplot(Marketing.Spend,Administration,R.D.Spend,col=c("green","lightblue","yellow"))
pairs(a,col="red")
cor(a)
library(corpcor)
cor2pcor(cor(a[,-c(4)]))

#multiple linear regression
model <- lm(Profit~Marketing.Spend+R.D.Spend+Administration+State,data=a)
summary(model)                     #R^2=95.07
rmse <- mean(model$residuals^2)^.5 #8854.34
pred <- predict(model,a)
cor(pred,Profit)                   #0.9750
library(car)
vif(model)
avPlots(model,col="red")
influencePlot(model,col="red")
influenceIndexPlot(model)
qqPlot(model)
vif(model)
#modelinfluence plot
model2<- lm(Profit~R.D.Spend+Marketing.Spend+Administration+State,data = a[-c(46,50),])
summary(model2)                                 #r^2=0.9635
rmse2 <- mean(model2$residuals^2)^.5            #7160
pred2 <- predict(model2,a)
cor(pred2,a$Profit)                                     #0.9748
vif(model2)
avPlots(model2)
#removing administration and state
modelrd <- lm(Profit~R.D.Spend+Marketing.Spend,data = a[-c(46,50),])
summary(modelrd)                                        #R^2=0.963
rmserd <- mean(modelrd$residuals^2)^.5                  #7211
predrd <- predict(modelrd,a)
cor(predrd,Profit)                                      #0.9747

#transformation method
#exp model
log=log(a[,5])
a2 <- data.frame(log,a[,-5])
View(a2)
head(a2)
model3 <- lm(log~.,data = a2)
summary(model3)                                #r^2 0.7619
rmse3 <- mean(model3$residuals^2)^.5           #0.2237
pred3 <- predict(model3,a2)
cor(pred3,a2$log)                              #0.87
vif(model3)
qqPlot(model3)
#model 2 have higher R^2 value and accuracy
#model 3 is best model with less rmse
plot(model2)