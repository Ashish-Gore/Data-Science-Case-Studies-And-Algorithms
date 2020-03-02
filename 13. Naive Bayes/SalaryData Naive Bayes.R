library(e1071)
library(corrplot)
library(gmodels)
salary_train <-read.csv(file.choose()) 
View(salary_train)
str(salary_train)
salary_test <- read.csv(file.choose())
View(salary_test)
str(salary_test)

salary <- rbind(salary_train,salary_test)
str(salary)

################################# creating dummies ##################################
level_work <- levels(salary$workclass)
level_edu <- levels(salary$education)
level_mari <- levels(salary$maritalstatus)
level_occ <- levels(salary$occupation)
level_rel <- levels(salary$relationship)
level_race <- levels(salary$race)
level_sex <- levels(salary$sex)
level_native <- levels(salary$native)
level_salary <- levels(salary$Salary)
salary$workclass <- as.integer(factor(salary$workclass,levels =c(level_work),labels=c(1,2,3,4,5,6,7)))
salary$education <- as.integer(factor(salary$education,levels = c(level_edu),labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))) 
salary$maritalstatus <- as.integer(factor(salary$maritalstatus,levels = c(level_mari),labels =c(1,2,3,4,5,6,7) ))
salary$occupation <- as.integer(factor(salary$occupation,levels = c(level_occ),labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)))
salary$relationship <- as.integer(factor(salary$relationship,levels = c(level_rel),labels = c(1,2,3,4,5,6)))
salary$race <- as.integer(factor(salary$race,levels = c(level_race),labels = c(1,2,3,4,5)))
salary$sex <- as.integer(factor(salary$sex,levels = c(level_sex),labels = c(1,2)))
salary$native <- as.integer(factor(salary$native,levels = c(level_native),labels = c(seq(1,40,1))))
salary$Salary <- as.integer(factor(salary$Salary,levels = c(level_salary),labels = c(1,2)))
corrplot(cor(salary),method = c("square"),type = "upper")

# Normalising the data
norm <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
normsalary <- as.data.frame(lapply(salary,norm))
View(normsalary)

# splitting data to test and train
salary_train_norm <- normsalary[1:30161,]
salary_test_norm <- normsalary[30162:45221,]
head(salary_train)
barplot(table(salary_test$Salary),col = c("navyblue","green"),main = " salary in test")
barplot(table(salary_train$Salary),col =c("red","yellow"),main = "salary in train")


# model building
model1 <- naiveBayes(salary_train_norm,y=salary_train$Salary)
summary(model1)
pred1 <- predict(model1,salary_test$Salary)
mean(pred1==salary_test$Salary)                 #acc = 0.7543161
table(salary_test$Salary,pred1)
CrossTable(salary_test$Salary,pred1)

#model bulding with laplase smoothing
model2 <- naiveBayes(salary_train_norm,salary_train$Salary,laplace = 1)
summary(model2)
pred2 <- predict(model2,salary_test_norm)
mean(pred2==salary_test$Salary)                 # acc = 0.9922311
table(salary_test$Salary,pred2)
CrossTable(salary_test$Salary,pred2,prop.c = F,prop.t = F,prop.chisq = F)