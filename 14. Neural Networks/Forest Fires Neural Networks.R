library(neuralnet)
library(nnet)
library(caret)
library(corrplot)
library(NeuralNetTools)
forest <- read.csv(file.choose())
View(forest)
str(forest)
table(forest$size_category)
ggplot(forest)+geom_histogram(mapping = aes(x=log(forest$area)))
colnames(forest)
ggplot(data = forest)+geom_bar(aes(x=forest$size_category))
#creating dummies
forest$month=as.integer(factor(forest$month,levels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"),labels = c(1,2,3,4,5,6,7,8,9,10,11,12)))
forest$day=as.integer(factor(forest$day,levels = c("sun","mon","tue","wed","thu","fri","sat"),labels = c(1,2,3,4,5,6,7)))
forest$size_category=as.integer(factor(forest$size_category,levels = c("large","small"),labels = c(1,0)))
str(forest)
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

norm_foresr <- as.data.frame(lapply(forest,normalize))
View(norm_foresr)
attach(norm_foresr)
#splitting of data to train and test
train <- sample(2,nrow(norm_foresr),prob = c(0.7,0.3),replace = T)
trn <- norm_foresr[train==1,]
tst <- norm_foresr[train==2,]

#model building
set.seed(101)
model <- neuralnet(area~.,data = trn,rep=5)
str(model)
plot(model,"best")

#prediction
set.seed(101)
predict_model <- compute(model,tst)
pred <- predict_model$net.result
cor(pred,tst$area)                                            #ACC is very poor 

#visualisation

plotnet(model,cex=0.8)

#improving performance of the modelby including hidden nodes result from bagging method
set.seed(100)
model2 <- neuralnet(area~.,hidden = 7,data = trn,)                 #hidden choosen from bagging method
str(model2)
set.seed(100)
pred2model <- compute(model2,tst)
pred2 <- pred2model$net.result
cor(pred2,tst$area)                                          #SSE is decreased
plot(model2)
plotnet(model2)


#bagging
acc <- c()
for(i in seq(1,10,1)){
  set.seed(100)
  model_bag <- neuralnet(area~.,hidden = i,data = trn)
  pred_bag <- compute(model_bag,tst)
  acc <- c(acc,cor(pred_bag$net.result,tst$area))
}
acc
plot(seq(1,10,1),acc)