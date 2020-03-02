library(party)
library(caret)
library(gmodels)
data()
data("iris")
View(iris)
summary(iris)
colnames(iris)
pairs(iris)
str(iris)
attach(iris)

#splitting of data into test and train
splt <- createDataPartition(iris$Species,p=0.75,list = F)
trn <- iris[splt,]
tst <- iris[-splt,]

#building model using party package (based on problem statement)
ctreemodel <- ctree(Species~.,data = trn)
summary(ctreemodel)
plot(ctreemodel)
predctree <- predict(ctreemodel,tst)
table(predctree,tst$Species)
mean(predctree==tst$Species)                                #acc =94.4%
CrossTable(predctree,tst$Species)

#model using bagging method
ac <- c()
for(i in 1:100){
  print(i)
  spltb <- createDataPartition(Species,p=0.75,list = F)
  trainb <- iris[spltb,]
  testb <- iris[-spltb,]
  
  modelb <- ctree(Species~.,data = trainb)
  predb <- predict(modelb,testb)
  
  ac <-c(ac,mean(predb==testb$Species))
}
ac
summary(ac)