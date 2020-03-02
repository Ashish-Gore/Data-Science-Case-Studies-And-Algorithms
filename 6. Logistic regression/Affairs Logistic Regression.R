library(AER)
data(Affairs,package = "AER")
View(Affairs)
attach(Affairs)
str(Affairs)
summary(Affairs)
library(psych)
describe(Affairs)
#creating dummy variables

Affairs$ynaffairs[Affairs$affairs>0] <- 1
Affairs$ynaffairs[Affairs$affairs==0] <- 0


boxplot(Affairs$ynaffairs)
plot(gender,Affairs$ynaffairs)
plot(age,Affairs$ynaffairs)
plot(yearsmarried,Affairs$ynaffairs)
plot(children,Affairs$ynaffairs)
plot(religiousness,Affairs$ynaffairs)
plot(occupation,Affairs$ynaffairs)
plot(Affairs$ynaffairs,education)

#creating train and test data
library(caret)
set.seed(101)
split <- createDataPartition(Affairs$ynaffairs,p=0.75,list = F)
split
train <- Affairs[split,]
test <- Affairs[-split,]

#linear regression model without using train and test data

lmmodel <- lm(ynaffairs~factor(gender)+age+yearsmarried+factor(children)+religiousness+education+occupation+rating,data = Affairs)
summary(lmmodel)

#logistic regression -train data
logmodel <- glm(ynaffairs~factor(gender)+age+yearsmarried+factor(children)+religiousness+education+occupation+rating,data=train,family = "binomial")
summary(logmodel)


#prediction based on test data
pred <- predict(logmodel,test,type = "response")
pred
#confusion matrix based on test data
conf <- table(pred>0.5,test$ynaffairs)
conf
#accuracy
accu <- sum(diag(conf)/sum(conf))
accu

#logistic model with removal of influence variables
logmodel1 <- glm(ynaffairs~age+yearsmarried+yearsmarried+religiousness+rating,data = train,family = "binomial")
summary(logmodel1)
predmodel1 <- predict(logmodel1,test,type = "response")
predmodel1
confmodel1 <- table(predmodel1>0.5,test$ynaffairs)
sum(diag(confmodel1)/sum(confmodel1))

#logmodel  without removal of influence factors has more accuracy 

pred_values <- NULL
yes_no <- NULL
pred_values <- ifelse(pred>=0.5,1,0)
yes_no <- ifelse(pred>=0.5,"yes","no")

#creating columns
test[,"prob"] <- pred
test[,"pred_values"] <- pred_values
test[,"yes_no"] <- yes_no
View(test[,c(1,9:11)])
table(test$ynaffairs,test$pred_values)


#rocr plot
library(ROCR)
rocrpred<-prediction(pred,test$ynaffairs)
rocrpref<-performance(rocrpred,'tpr','fpr')
str(rocrpref)

plot(rocrpref,colorize=T)

#rocr cutoff
library(dplyr)
rocr_cutoff <- data.frame(cut_off=rocrpref@alpha.values[[1]],fpr=rocrpref@x.values,tpr=rocrpref@y.values)
View(rocr_cutoff)

rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)
summary(rocr_cutoff$cut_off)