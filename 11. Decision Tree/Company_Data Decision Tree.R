library(caret)
library(party)
library(C50)
library(rpart)
library(rattle)
library(gmodels)
data <- read.csv(file.choose())
View(data)
str(data)
summary(data)
pairs(data)

sort(data$Sales)
length(data$Sales)
mean(data$Sales)
sort(data$Sales)[400/3*2] #sales may be high,medium,low
#converting sales to categorical type
sales_cat<- ifelse(data$Sales>8.5,"high","low")
df <- data.frame(sales_cat,data[,-1])
View(df)

#splitting data to train and test data
set.seed(100)
split <- createDataPartition(sales_cat,p=0.75,list = F)
train <- df[split,]
test <- df[-split,]

#building model

#model1 using party package
model1 <- ctree(sales_cat~.,data = train)
pred1 <- predict(model1,test)
table(pred1,test$sales_cat)
mean(pred1==test$sales_cat)                  #73.73% acc
plot(model1)


#model2 using c50 package
model2 <- C5.0(train[,-1],train$sales_cat,trails=100)
pred2 <- predict.C5.0(model2,test)
table(pred2,test$sales_cat)
mean(pred2==test$sales_cat)                #77.78% acc
plot(model2)
C5imp(model2)
CrossTable(test$sales_cat,pred2)
#model3 bagging method

acc <- c()
for(i in 1:100){
  print(i)
  splitting <- createDataPartition(sales_cat,p=0.85,list = F)
  training <- df[splitting,]
  testing <- df[-splitting,]
  
  modelfit <- C5.0(training[,-1],training$sales_cat)
  predictfit <- predict(modelfit,testing)
  a <- table(predictfit,testing$sales_cat)
  acc <- c(acc,sum(diag(a))/sum(a))
}
acc
summary(acc)

#model using rpart package
model4 <- rpart(sales_cat~.,data = train)
plot(model4)
text(model4,pretty = 0)
fancyRpartPlot(model4,cex=0.5,type = 2)
text(model4)
pred4 <- predict(model4,test,type = 'class')
table(pred4,test$sales_cat)
mean(pred4==test$sales_cat)   #75.75 acc