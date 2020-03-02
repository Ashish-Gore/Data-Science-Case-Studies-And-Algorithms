bank <- read.csv(file.choose())
View(bank)
attach(bank)
colnames(bank)

#creating dummy variables for output
bank$termdeposit[bank$y!='no'] <- 1
bank$termdeposit[bank$y=='no'] <- 0
View(bank)

library(psych)
summary(bank)#1st moment bussiness decision
describe(bank)#2nd,3rd&4th moment bussiness decision
attach(bank)
plot(bank$termdeposit,bank$age)
plot(termdeposit,balance)
plot(termdeposit,campaign)
plot(termdeposit,contact)
plot(termdeposit,day)
plot(termdeposit,default)
plot(termdeposit,duration)
plot(termdeposit,housing)
plot(termdeposit,job)
plot(termdeposit,loan)
plot(termdeposit,marital)
plot(termdeposit,poutcome)
plot(termdeposit,previous)
boxplot(bank$termdeposit,bank$age)


#applying linear regression
model1 <- lm(bank$termdeposit~.,data = bank)
summary(model1)
pred <- predict(model1,bank)
pred
plot(age,pred)

#splitting of data into train and test
library(caret)
split <- createDataPartition(termdeposit,p=0.75,list = F)
train <- bank[split,]
test <- bank[-split,]

#applying logistic regression
logit <- glm(train$termdeposit~age+factor(job)+factor(marital)+factor(education)+factor(default)+balance+factor(housing)+factor(loan)+factor(contact)+day+factor(month)+factor(poutcome)+duration+pdays+previous,family = "binomial",data = train)
summary(logit)
#confusion matrix
pred1 <- predict(logit,type = c("response"),test)
pred1
confusion <- table(pred1>0.5,test$termdeposit)
confusion
#model accuracy
accuracy <- sum(diag(confusion)/sum(confusion))
accuracy

#applying logistic regression after removal of influence factors
logit1 <- glm(train$termdeposit~factor(job)+factor(marital)+factor(education)+factor(housing)+factor(loan)+factor(contact)+factor(month)+day+balance+factor(poutcome)+duration+previous,family = "binomial",data = train)
summary(logit1)
predlog <- predict(logit1,type = "response",test)
conflog <- table(predlog>0.5,test$termdeposit)
conflog
accuracylog <- sum(diag(conflog)/sum(conflog))
accuracylog

#logistic model after removal of influence factors have more accuracy

#odds ratio
exp(coef(logit1))



#creating prediction model
pred_values <- NULL
yes_no <- NULL
pred_values <- ifelse(predlog>=0.5,1,0)
yes_no <- ifelse(predlog>=0.5,"yes","no")
test[,"prob"] <- predlog
test[,"pred_values"] <- pred_values
test[,"yes_no"] <- yes_no
colnames(test)
View(test[,c(1,18:21)])

#rocr curve
library(ROCR)
rocrpred <- prediction(predlog,test$termdeposit)
rocrpref <- performance(rocrpred,'tpr','fpr')
plot(rocrpref,colorize= T)

#rocr cutoff
library(dplyr)
str(rocrpref)
rocr_cutoff <- data.frame(cutoff=rocrpref@alpha.values[[1]],fpr=rocrpref@x.values,tpr=rocrpref@y.values)
View(rocr_cutoff)

rocr_cutoff$cutoff <-round(rocr_cutoff$cutoff,6) 
colnames(rocr_cutoff  ) <- c("cutoff","fpr","tpr")
???rocr_cutoff <-arrange( rocr_cutoff,desc(tpr))
View(rocr_cutoff)