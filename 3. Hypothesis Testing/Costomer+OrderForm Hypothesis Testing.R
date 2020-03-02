a=read.csv(file.choose())
View(a)
library(BSDA)
library(e1071)
library(nortest)
attach(a)
install.packages("tidyr")
library(tidyr)
#table formation and stacking of data
a2 <- table(gather(a,nation,status,1:4))
a2
#H0 <- all the centers have same defective% 
#H1 <- different centers have defective%
chisq.test(a2)
#pvalue>0.05 failed to reject null hypothesis


#all centers have same defective%