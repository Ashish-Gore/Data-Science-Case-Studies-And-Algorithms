library(neuralnet)
library(NeuralNetTools)
library(nnet)
library(caret)
library(corrplot)
library(GGally)
concrete <- read.csv(file.choose())
head(concrete)
str(concrete)
summary(concrete)
corrr <- cor(concrete)
corrplot(corrr,method = 'number')
ggpairs(concrete)
attach(concrete)
ggplot(data = concrete)+geom_histogram(aes(x=strength),bins = 30)


#normalising function
normm <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
#normalising df
norm_concrete <- as.data.frame(lapply(concrete,normm))
head(norm_concrete)

#splitting of data to test and train
set.seed(100)
trainn <- createDataPartition(norm_concrete$strength,p=0.8,list = F)
trn_c <- norm_concrete[trainn,]
tst_c <- norm_concrete[-trainn,]

#model building
model_con <- neuralnet(strength~.,data = trn_c)
str(model_con)
plot(model_con)
pred_model_con <-compute(model_con,tst_c) 
predict_con <- pred_model_con$net.result
cor(predict_con,tst_c$strength)                      #SSE is more with less acc
plotnet(model_con,cex=0.8)


#improving model performance by includng hidden nodes
set.seed(100)
model2_con <- neuralnet(strength~.,data = trn_c,hidden = 5)
str(model2_con)
plot(model2_con)
predict_model2 <-  compute(model2_con,tst_c) 
predict_con2 <- predict_model2$net.result
cor(predict_con2,tst_c$strength)                  #SSE has decreased
plotnet(model2_con,cex=0.8)