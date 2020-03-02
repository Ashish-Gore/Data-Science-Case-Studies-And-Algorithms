library(randomForest)
library(caret)
library(psych)
frauddata <- read.csv(file.choose())
View(frauddata)
summary(frauddata)
describe(frauddata)
pairs(frauddata)
str(frauddata)
attach(frauddata)
############### converting taxable to categorical type ###########
tax_cat <- ifelse(Taxable.Income<=30000,"risky","good")
frauddata <- data.frame(tax_cat,frauddata[,-3])
table(frauddata$tax_cat)

############ splitting of data to train and test ########################
set.seed(100)
cut <- createDataPartition(tax_cat,p=0.7,list = F)
train_f <- frauddata[cut,]
test_f <- frauddata[-cut,]

############# model building #############################
#model based on train data
forest <- randomForest(tax_cat~.,data = train_f,importance=TRUE,mtry=2)
forest

# prediction and accuracy based on train data
predict_train <- predict(forest,train_f)
mean(predict_train==train_f$tax_cat)                     # acc = 92.87%
confusionMatrix(predict_train,train_f$tax_cat)


#prediction and accuracy based on test data
predict_test <- predict(forest,test_f)
mean(predict_test==test_f$tax_cat)                      #acc = 78.77%
confusionMatrix(predict_test,test_f$tax_cat)

plot(forest)
legend("topright",col = 2:5,colnames(forest$err.rate),fill = 2:5,cex = 0.5)


importance(forest)
varImpPlot(forest)
#conclusion : most significant variable is city population

# bagging
acc <- c()
i=2
for(i in 2:10){
  set.seed(100)
  d <- createDataPartition(tax_cat,p=0.8,list = F)
  train_d <- frauddata[d,]
  test_d <- frauddata[-d,]
  model_d <- randomForest(tax_cat~.,data = train_d,mtry=i)
  pred_b <- predict(model_d,test_d)
  acc[i-2]=mean(pred_b==test_d$tax_cat)
}
acc
plot(2:9,acc,xlab = "mtry",ylab = "acc")
#higher accuracy is for mtry=2

################ tunning of the random forst model ##########################
tune <- tuneRF(train_f[,-1],train_f[,1],stepFactor = 0.5,ntreeTry = 500,trace = TRUE,improve = 0.05,plot = TRUE)
tune

#choosen mtry as 2 with reasonable  OOB error

finalmodel <- randomForest(tax_cat~.,data = train_f,mtry=2)
finalmodel
mean(predict(finalmodel,test_f)==test_f$tax_cat)