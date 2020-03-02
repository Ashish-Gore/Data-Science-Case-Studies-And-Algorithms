library(arules)
library(arulesViz)
library(GGally)
library(corrplot)
library(psych)

book <- read.csv(file.choose())
View(book)
summary(book)
describe(book)
ggpairs(book)
corrplot(cor(book),method = "square",type = "lower")
sum(is.na(book))

# applying apriorimalgorithm
# apriori algorithm with support =0.02 , confi =0.5 & minlen = 3
rules_book <- apriori(as.matrix(book),parameter = list(support =0.02,confidence=0.5,minlen=3))
rules_book
inspect(head(sort(rules_book,by="lift")))
inspect(head(sort(rules_book,by="confidence")))
inspect(head(sort(rules_book,by="support")))
inspect(head(sort(rules_book,by=c("count","lift"))))  #maximum count = 299

head(quality(rules_book))

# visualisation
colfunction <- colorRampPalette(c("red","blue","yellow"))
plot(rules_book,method = "scatterplot",jitter=0,col=colfunction(5))
plot(rules_book,method = "paracoord")
plot(head(sort(rules_book,by="lift"),n=10),method = "graph")
plot(rules_book,method = "grouped matrix",col=colfunction(5),n)
plot(rules_book,method = "matrix")
plot(rules_book,method = "matrix3D")

# using differnt support and confidence

rules_book2 <- apriori(as.matrix(book),parameter = list(support = 0.01,confidence =0.7, minlen = 4))
rules_book2
inspect(head(sort(rules_book2,by="lift")))
inspect(head(sort(rules_book2,by="confidence")))
inspect(head(sort(rules_book2,by="support")))      # max count =178
inspect(head(sort(rules_book2,by=c("count","lift"))))

#plot methods matrix , mosaic , doubledecker , graph , paracoord , scatterplot , groupedmatrix , two-key plot , matrix3D ,iplots

plot(rules_book2,method = "two-key plot",jitter=0)
plot(rules_book2,method = "matrix3D")
plot(rules_book2,method = "matrix")
plot(head(sort(rules_book2,by="lift"),n=10),method = "graph")
plot(rules_book2,method = "paracoord")
plot(rules_book2,method = "grouped matrix",col=colfunction(5))
plot(rules_book2,method = "matrix",engine = "3d")
