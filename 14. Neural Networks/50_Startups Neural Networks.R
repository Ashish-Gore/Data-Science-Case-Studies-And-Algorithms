library(corrplot)
library(GGally)
library(neuralnet)
library(nnet)
library(caret)
library(NeuralNetTools)
startup <- read.csv(file.choose())
View(startup)
summary(startup)
str(startup)
corr <- cor(startup[,-4])
corrplot(corr,method = "number")
ggpairs(startup[,-4])
ggplot(data = startup)+geom_histogram(mapping=aes(x=startup$Profit),bins=10)
startup$statedummy[startup$State=="New York"] <- 0
startup$statedummy[startup$State=="California"] <-1
startup$statedummy[startup$State=="Florida"] <- 3
startup$statedummy <- as.numeric(startup$statedummy)
str(startup[,-4])

#normalising the dataset
norm <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
denorm <- function(x,min,max){
  return((max-min)*x+min)
  
}
data <- as.data.frame(lapply(startup[,-4],norm))
head(data)
attach(data)
#splitting data to train and test data
set.seed(100)
s <- createDataPartition(data$Profit,p=0.8,list = F)
trn_s <- data[s,]
tst_s <- data[-s,]
trn_s
#model building
set.seed(100)
model <- neuralnet(Profit~Marketing.Spend+Administration+R.D.Spend+statedummy,data = trn_s)
plot(model,rep = "best")
str(model)
#model validation
set.seed(100)
modelresults <- compute(model,tst_s)
predprofit <- modelresults$net.result
cor(predprofit,tst_s$Profit)                      # SSE is higher than 2 model
plot(predprofit,tst_s$Profit)

#visualisation
plotnet(model,x_names = colnames(data[,-4]),y_names = colnames(data[,4]),cex=0.8)

max_s <- max(data$Profit)
min_s <- min(data$Profit) 
actualpred_profit <- denorm(predprofit,min = min_s,max = max_s)
actualpred_profit

#improving model performance 
set.seed(100)
model2 <- neuralnet(Profit~Marketing.Spend+Administration+R.D.Spend+statedummy,data = trn_s,hidden = 5,rep = 5)
plot(model2,"best")
plotnet(model2,cex=0.8)
str(model2)
set.seed(100)
model2result <- compute(model2,tst_s)
pred2profit <- model2result$net.result
cor(pred2profit,tst_s$Profit)                         #SSE is decreased with improved acc
