library(randomForest)
library(caret)
library(psych)
data("iris")
View(iris)
summary(iris)
describe(iris)
str(iris)
attach(iris)
#################### splitting of data into train and test ######################
s <- createDataPartition(Species,p=0.7,list = F)
train_iris <-iris[s,] 
test_iris <- iris[-s,]

################### building random forest model ########################

iris_model <- randomForest(Species~.,data = train_iris,importance = T)
iris_model

# prediction and accuracy based on train data
pred_tain_iris <-predict(iris_model,train_iris) 
mean(pred_tain_iris==train_iris$Species)            # acc =100%
confusionMatrix(pred_tain_iris,train_iris$Species)

# prediction and accuracy based on test data
pred_test_iris <- predict(iris_model,test_iris)
mean(pred_test_iris==test_iris$Species)             # acc =91.11%
confusionMatrix(pred_test_iris,test_iris$Species)

# variable importance
importance(iris_model)
varImpPlot(iris_model)
# petal length is th most significant variable
################### selection mtry using tunerf ##########################
tune_iris <- tuneRF(train_iris[,-5],train_iris[,5],trace = T,improve = 0.05,stepFactor = 2)
tune_iris
#mtry = 4, with less OOB error

#bagging
accboost <- c()
for(i in 3:10){
  set.seed(100)
  boost <- createDataPartition(Species,p=0.80,list = F)
  train_boost <- iris[boost,]
  test_boost <- iris[-boost,]
  boost_model <- randomForest(Species~.,data = train_boost,mtry=i)
  pred_boost <- predict(boost_model,test_boost)
  accboost[i-2]=mean(pred_boost==test_boost$Species)
}
accboost
plot(2:9,accboost)

#for different mtry acc is similar

#final model from mtry plot
finalmodel <- randomForest(Species~.,data = train_iris,mtry=4)
finalmodel
mean(predict(finalmodel,test_iris)==test_iris$Species)