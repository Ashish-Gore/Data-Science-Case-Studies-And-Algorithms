movies <- read.csv(file.choose())
View(movies)
head(movies)
summary(movies)
describe(movies)
corrplot(cor(movies[,6:15]),method = "square",type = "upper")
ggpairs(movies[,6:15])

#applying apriori algorithm
# confidence =0.05 , support =0.005 ,minlen =3

movies_rules <- apriori(as.matrix(movies[,6:15]),parameter = list(support = 0.005,confidence= 0.05,minlen=3))
movies_rules
inspect(head(sort(movies_rules,by="lift")))
inspect(head(sort(movies_rules,by="confidence")))
inspect(head(sort(movies_rules,by="support")))
inspect(head(sort(movies_rules,by=c("count","lift"))))# max count = 4

head(quality(movies_rules))

#plot methods matrix , mosaic , doubledecker , graph , paracoord , scatterplot , groupedmatrix , two-key plot , matrix3D ,iplots

#visualisation
plot(movies_rules,method = "scatterplot",jitter=0,col=colfunction(5))
plot(movies_rules,method = "grouped matrix",col=colfunction(5))
plot(head(sort(movies_rules,by="lift"),n=10),method = "graph")
plot(movies_rules,method = "paracoord")
plot(movies_rules,method = "matrix")


# using different support and confidence

movies_rules2 <- apriori(as.matrix(movies[,6:15]),parameter = list(support =0.005, confidence =0.5,minlen=2))
movies_rules2
inspect(head(sort(movies_rules2,by="lift")))
inspect(head(sort(movies_rules2,by="confidence")))
inspect(head(sort(movies_rules2,by="support")))
inspect(head(sort(movies_rules2,by=c("count","lift"))))# max count = 6

#visualisation
plot(movies_rules2,method = "scatterplot",jitter=0,col=colfunction(5))
plot(movies_rules2,method = "grouped matrix",col=colfunction(5))
plot(head(sort(movies_rules2,by="lift"),n=20),method = "graph")
plot(movies_rules2,method = "paracoord")
plot(movies_rules,method = "matrix")
plot(movies_rules2,method = "two-key plot", jitter=0)
plot(movies_rules2,method = "matrix",engine = "3d")