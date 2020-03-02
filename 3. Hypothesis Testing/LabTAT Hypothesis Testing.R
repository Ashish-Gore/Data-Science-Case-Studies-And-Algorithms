lab <- read.csv(file.choose())
View(lab)
attach(lab)

#normality test
library(nortest)
shapiro.test(Laboratory.1)#pvalue=0.5508
shapiro.test(Laboratory.2)#pvalue=0.8637
shapiro.test(Laboratory.3)#pvalue=0.4205
shapiro.test(Laboratory.4)#pvalue=0.6619
##all the variables are normal

#variance test
stacked <- stack(lab)
stacked
library(car)
leveneTest(stacked$values~stacked$ind,data=stacked)#pvalue=0.05161
#variance are equal

#one way ANOVA test
anova_result <- aov(values~ind,data=stacked)
summary(anova_result)#pvalue=2e-16
#reject null hypothesis

#there is difference in average TAT between different labouratory