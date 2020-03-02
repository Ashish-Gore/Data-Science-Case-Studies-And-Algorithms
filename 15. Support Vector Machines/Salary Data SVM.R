library(corrplot)
library(ggplot2)
library(kernlab)
doParallel::registerDoParallel(cores = 2)
train <- read.csv(file.choose())
View(train)
test <- read.csv(file.choose())
View(test)
str(train)
salary <- rbind(train,test)
View(salary)
table(salary$Salary)
plot(salary$Salary)

#creating dummies
level_work <- levels(salary$workclass)
leveledu <- levels(salary$education)
level_mari <- levels(salary$maritalstatus)
level_occ <- levels(salary$occupation)
level_rel <- levels(salary$relationship)
level_race <- levels(salary$race)
level_sex <- levels(salary$sex)
level_native <- levels(salary$native)
level_salary <- levels(salary$Salary)
salary$workclass <- as.integer(factor(salary$workclass,levels =c(level_work),labels=c(1,2,3,4,5,6,7)))
salary$education <- as.integer(factor(salary$education,levels = c(leveledu),labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))) 
salary$maritalstatus <- as.integer(factor(salary$maritalstatus,levels = c(level_mari),labels =c(1,2,3,4,5,6,7) ))
salary$occupation <- as.integer(factor(salary$occupation,levels = c(level_occ),labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)))
salary$relationship <- as.integer(factor(salary$relationship,levels = c(level_rel),labels = c(1,2,3,4,5,6)))
salary$race <- as.integer(factor(salary$race,levels = c(level_race),labels = c(1,2,3,4,5)))
salary$sex <- as.integer(factor(salary$sex,levels = c(level_sex),labels = c(1,2)))
salary$native <- as.integer(factor(salary$native,levels = c(level_native),labels = c(seq(1,40,1))))
salary$Salary <- as.integer(factor(salary$Salary,levels = c(level_salary),labels = c(1,2)))
corrplot(cor(salary),method = c("square"),type = "upper")

# normalising df
normal <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

norm_salary <- normal(salary)
View(norm_salary)


#splitting of data to test and train
train_norm <- norm_salary[1:30161,]
View(train_norm)
test_norm <- norm_salary[30162:45221,]

#model building

#using bagging method

kernels <- c("rbfdot","vanilladot","besseldot")
acc_bag <- list()
pred_info <- list()
table_info <- list()
for(i in kernels){
  model_bag <- ksvm(Salary~.,data=train_norm,kernel=i)
  pred_bag <- predict(model_bag,test_norm)
  pred_info[[i]] <- (pred_bag)
  acc_bag[[i]] <- cor(pred_bag,test_norm$Salary)
  table_info[[i]] <- table(pred_bag,test_norm$Salary)
}

acc_bag
# acc_rbfdot= 0.6004     
# acc_vanilladot= 0.3166
# acc_besseldot= 0.166
table_info
plot(1:3,acc_bag)
plot(pred_info$rbfdot)
plot(pred_info$vanilladot)
plot(pred_info$besseldot)

table_info$rbfdot
table_info$vanilladot
table_info$besseldot

# kernel= rbfdot  method has highest accuracy with 60.05%