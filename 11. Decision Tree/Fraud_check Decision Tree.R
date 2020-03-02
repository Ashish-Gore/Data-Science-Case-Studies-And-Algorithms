library(C50)
library(rattle)
library(caret)
library(party)
library(rpart)
library(gmodels)
fraud<- read.csv(file.choose())
View(fraud)
attach(fraud)
str(fraud)
summary(fraud)
pairs(fraud)
colSums(is.na(fraud))
hist(Taxable.Income)

#converting taxable.income data to categorical type
tax_cat <- ifelse(fraud$Taxable.Income<=30000,"risky","good")
data1 <- data.frame(tax_cat,fraud[,-3])
View(data1)

#data partition
set.seed(100)
splittax <- createDataPartition(data1$tax_cat,p=0.75,list = F)
traintax <- data1[splittax,]
testtax <- data1[-splittax,]
str(traintax)
str(testtax)
#model building

#model1 using party package
model1 <- ctree(traintax$tax_cat~.,data = traintax)
pred1<-predict(model1,testtax) 
table(pred1,testtax$tax_cat)
mean(pred1==testtax$tax_cat)               #accuracy= 79.33%

#model2 using c50 package 
model2 <- C5.0(tax_cat~.,data = traintax,trails=100)
pred2 <- predict(model2,testtax)
table(pred2,testtax$tax_cat)
mean(pred2==testtax$tax_cat)             #accuracy= 79.33%
CrossTable(pred2,testtax$tax_cat)

#model3 using rpart package
model3 <- rpart(tax_cat~.,data = traintax,method ='class')
plot(model3)
text(model3,pretty = 0)
fancyRpartPlot(model3,cex=0.5,type = 2)
pred3 <- predict(model3,testtax,type = 'class')
table(pred3,testtax$tax_cat)
mean(pred3==testtax$tax_cat)          #accuracy= 78%

#bagging method
acc4 <- c()
for(i in 1:100){
  print(i)
  splits <- createDataPartition(data1$tax_cat,p=0.85,list = F)
  trains <- data1[splits,]
  tests <- data1[-splits,]
  model4 <- rpart(trains[,-6],trains$tax_cat)
  pred4 <- predict(model4,tests,type ='class' )
  a4 <- table(pred4,tests$tax_cat)
  acc4 <- c(acc4,sum(diag(a4))/sum(a4))
}
acc4
summary(acc4)                  
