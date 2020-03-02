cutlets <- read.csv(file.choose())
View(cutlets)
attach(cutlets)

#normality test
library(nortest)
ad.test(cutlets$Unit.A)#pvalue =0.2866
ad.test(cutlets$Unit.B)#pvalue =0.6869

#variance test
#H0 <- both the var are same
#Ha <- both have diff var
var.test(cutlets$Unit.A,cutlets$Unit.B)
#variances=0.70536 , pvalue =0.3136 
#failed to reject null hypothesis

#2-sample t-test for equal variance
#H0 <- cutlets have same diameter of 2 units
#Ha <- cutlets have diff diameter of 2 units

t.test(Unit.A,Unit.B,alternative = 'two.sided',conf.level = 0.95)

#failed to reject null hypothesis 
#cutlets have same diameters