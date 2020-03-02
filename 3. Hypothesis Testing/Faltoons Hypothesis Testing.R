fantaloons <- read.csv(file.choose())
View(fantaloons)
attach(fantaloons)
#table formation
table <- table(Weekdays,Weekend)
table
#2-propotion test
prop.test(x=c(66,47),n=c(422,588),conf.level = 0.95,alternative = "two.sided")#pvalue=0.0002147
#H0 <- % of males visiting store equal to % of females
#Ha <- % of males visiting store not equal to % of females


#2-propotion test for alternative is less 
prop.test(x=c(66,47),n=c(422,588),conf.level = 0.95,alternative = "less")#pvalue=0.9999
#H0 <- %of males visiting store is less than % of females
#Ha <- % of males visiting store is equal to % of females

#per of males visiting the store is different from per of females during week days