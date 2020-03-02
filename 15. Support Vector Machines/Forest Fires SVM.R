library(caret)
library(ggplot2)
library(psych)
library(kernlab)
forest <- read.csv(file.choose())
View(forest)
str(forest)
table(forest$size_category)
ggplot(data = forest)+geom_histogram(mapping = aes(x=forest$area),binwidth = 50)
ggplot(data = forest)+geom_bar(mapping = aes(x=forest$size_category))
summary(forest)
describe(forest)
anyNA(forest)

#creating dummies
forest$month=as.integer(factor(forest$month,levels =  c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"),labels = c(1,2,3,4,5,6,7,8,9,10,11,12)))
forest$day=as.integer(factor(forest$day,levels = c("sun","mon","tue","wed","thu","fri","sat"),labels = c(1,2,3,4,5,6,7)))
forest$size_category=as.integer(factor(forest$size_category,levels = c("large","small"),labels = c(1,0)))
str(forest)    

corr <- cor(forest)
corrplot::corrplot(corr,method = c("square"),type ="lower" )
#Normalising
norm <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

forest_norm <- as.data.frame(lapply(forest,norm))
head(forest_norm)

#splitting of data to test and train
set.seed(100)
trns <- createDataPartition(forest_norm$area,p=0.8,list = F)
tn <- forest_norm[trns,]
ts <- forest_norm[-trns,]

# Model building

#rbfdot
modelrbfdot <- ksvm(area~.,data=tn,kernel="rbfdot")
predrbfdot <- predict(modelrbfdot,newdata=ts)
cor(predrbfdot,ts$area)                           #acc = 0.3431461
plot(predrbfdot)

#besseldot
modelbessel <- ksvm(area~.,data=tn,kernel="besseldot")
predbessel <- predict(modelbessel,newdata=ts)
cor(predbessel,ts$area)
plot(predbessel)                                 #acc = -0.04111227

#ploydot
modelpoly <- ksvm(area~.,data=tn,kernel="polydot")
predpoly <- predict(modelpoly,newdata=ts)
cor(predpoly,ts$area)                            #acc = 0.3455455
plot(predpoly)

#vanilladot
modelvanilla <- ksvm(area~.,data=tn,kernel="vanilladot")
predvanilla <- predict(modelvanilla,newdata=ts)
cor(predvanilla,ts$area)                         #acc = 0.3464077
plot(predvanilla)

#tanhdot
modeltanh <- ksvm(area~.,data=tn,kernel="tanhdot")
predtanh <- predict(modeltanh,newdata=ts)
cor(predtanh,ts$area)
plot(predtanh)                                  #acc = 0.005924

#anovadot
modelanova <- ksvm(area~.,data=tn,kernel="anovadot")
predanova <- predict(modelanova,newdata=ts)
cor(predanova,ts$area)                          #acc = 0.3368088
plot(predanova)

#using bagging method

kernels <- c("rbfdot","polydot","vanilladot","tanhdot","laplacedot","besseldot","anovadot")
acc_bag <- list()
pred_inf <- list()
table_inf <- list()
for(i in kernels){
  model_bag <- ksvm(area~.,data=tn,kernel=i)
  pred_bag <- predict(model_bag,ts)
  pred_inf[[i]] <- pred_bag
  acc_bag[[i]] <- (cor(pred_bag,ts$area))
  table_inf <- table(pred_bag,ts$area)
}
acc_bag
pred_inf
table_inf
acc_bagg <- data.frame(kernels,acc_bag)
ggplot(data = acc_bagg)+
  geom_col(mapping=aes(x=1:7,y=acc_bag))

# kernel vanilladot  method has highest accuracy