#buyers ratio 
#the dependent and independent variables are discrete
#chi-square test
a <- read.csv(file.choose())
View(a)
b <- a[,-1]
b
#H0 ->product sales ratio are same for males and females
#Ha ->product sales ratio are not same for males and females

chisq.test(b)
#p =0.6603 >0.05 -> p high null fly ->failed to reject null hypothesis

##product sales ratio are same for males and females