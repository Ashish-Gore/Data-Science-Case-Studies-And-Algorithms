library(randomForest)
library(caret)
library(psych)
company <- read.csv(file.choose())
View(company)
summary(company)
describe(company)
pairs(company)
str(company)
attach(company)

############### creating categorical data on sales variable ####################
length(Sales)
sort(Sales)
mean(Sales)
sort(Sales)[400/3*2]

sales_cat <- ifelse(Sales>8.5,"high","low")
company <- data.frame(sales_cat,company[,-1])
View(company)

############## splitting data to train and test ##########################
set.seed(100)
cutt <- createDataPartition(sales_cat,p=0.7,list=F)
train_comp <- company[cutt,]
test_comp <- company[-cutt,]

############# model building ##############################
companyforest <- randomForest(sales_cat~.,ntree=500,mtry=3,data = train_comp,importnce=T)
companyforest

# prediction and accuracy based on train data
pred_train <- predict(companyforest,train_comp)
mean(pred_train==train_comp$sales_cat)                      # acc = 100%
confusionMatrix(pred_train,train_comp$sales_cat)

# prediction and accuracy based on test data
pred_test <- predict(companyforest,test_comp)
mean(pred_test==test_comp$sales_cat)                       # acc = 79.83%
confusionMatrix(pred_test,test_comp$sales_cat)

# visualisation 
plot(companyforest)
legend("topright",col = 2:11,colnames(companyforest$err.rate),fill = 2:11,cex = 0.5)

# variable importance
importance(companyforest)
varImpPlot(companyforest)
# price is the most significant variable


# bagging 
a <- c()
for(i in 3:10){
  set.seed(100)
  bag <- createDataPartition(sales_cat,p=0.8,list = F)
  train_bag <- company[bag,]
  test_bag <- company[-bag,]
  bag_model <- randomForest(sales_cat~.,data = train_bag,mtry=i,importance=TRUE)
  pred_bag <- predict(bag_model,test_bag,type='class')
  a[i-2] <- mean(pred_bag==test_bag$sales_cat)
}
a
plot(3:10,a,xlab = "mtry",ylab = "acc")
# we get highest accuracy for mtry = 3

############# tune random forest ############################
set.seed(100)
tunerf <- tuneRF(train_comp[,-1],train_comp[,1],improve = 0.5,stepFactor = 0.5)
tunerf

# choosing mtry as 3 with less OOB error


finalmodel <- randomForest(sales_cat~.,data = train_comp,mtry=3,importance=TRUE)
finalmodel
mean(predict(finalmodel,test_comp)==test_comp$sales_cat)