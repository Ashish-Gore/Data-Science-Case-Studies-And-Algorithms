library(psych)
library(caret)
library(class)
library(ggplot2)
zoo_df <- read.csv(file.choose())
zoo1 <- zoo_df[,-1]
View(zoo1)
head(zoo1)
summary(zoo1)
describe(zoo1)
attach(zoo1)
str(zoo1)                                           # data is in int format
zoo1$hair <- as.factor(zoo1$hair)
zoo1$feathers <- as.factor(zoo1$feathers)
zoo1$eggs <- as.factor(zoo1$eggs)
zoo1$milk <- as.factor(zoo1$milk)
zoo1$airborne <- as.factor(zoo1$airborne)
zoo1$aquatic <- as.factor(zoo1$aquatic)
zoo1$predator <- as.factor(zoo1$predator)
zoo1$toothed <- as.factor(zoo1$toothed)
zoo1$backbone <- as.factor(zoo1$backbone)
zoo1$breathes <- as.factor(zoo1$breathes)
zoo1$venomous <- as.factor(zoo1$venomous)
zoo1$fins <- as.factor(zoo1$fins)
zoo1$legs <- as.factor(zoo1$legs)
zoo1$tail <- as.factor(zoo1$tail)
zoo1$domestic <- as.factor(zoo1$domestic)
zoo1$catsize <- as.factor(zoo1$catsize)
zoo1$type <- as.factor(zoo1$type)
str(zoo1)                                       # data is in fact format
table(zoo1$type)
ggplot(zoo1)+geom_bar(mapping = aes(x=type))
# splitting of data to train and test
z <- createDataPartition(type,p=0.8,list = F)
trn_z <- zoo1[z,]
tst_z <- zoo1[-z,]

#model building
set.seed(10)
model <- knn(trn_z,tst_z,k = 7,cl = trn_z$type)
summary(model)
mean(model==tst_z$type)            # acc =84.2%
confusionMatrix(model,tst_z$type)



# improving accuracy #boosting
trn_acc <- c()
tst_acc <- c()
for(i in seq(1,50,2)){
  set.seed(100)
  pred_knn_train <- knn(trn_z,trn_z,k=i,cl=trn_z$type)
  pred_knn_test <- knn(trn_z,tst_z,k=i,cl=trn_z$type)
  trn_acc <- c(trn_acc,mean(pred_knn_train==trn_z$type))
  tst_acc <- c(tst_acc,mean(pred_knn_test==tst_z$type))
}
trn_acc
tst_acc
par(mfrow=c(1,2))
plot(seq(1,50,2),trn_acc,type = 'l',main = "train acc",col='blue')
plot(seq(1,50,2),tst_acc,type = "l",main = "test acc",col='red')

# we have highest test accuracy for k=1

#final model

final_zoo <- knn(trn_z,tst_z,k=1,cl=trn_z$type)
mean(final_zoo==tst_z$type)  #acc = 1
confusionMatrix(final_zoo,tst_z$type)


#visualisation of bagging method
acc_k <- data.frame(list(train_acc=trn_acc,tst_acc=tst_acc,k=seq(1,50,2)))
acc_k
ggplot(acc_k,aes(x=k))+
  geom_line(aes(y=trn_acc,color="trn_acc"))+
  geom_line(aes(y=tst_acc,color="tst_acc"))