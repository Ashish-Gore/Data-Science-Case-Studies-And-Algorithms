groceries <- read.transactions(file.choose(),sep = ",",format = "basket")
inspect(groceries[1:5])
groceries@itemInfo
itemFrequencyPlot(x=groceries,topN=10)

# applying Apriori algorithm 
# with support = 0.005 , confidence = 0.2 & minlen = 3

grocerie_rules <- apriori(groceries,parameter = list(support =0.005,confidence=0.2,minlen = 3))
grocerie_rules

inspect(head(sort(grocerie_rules,by="lift")))
inspect(head(sort(grocerie_rules,by="confidence")))
inspect(head(sort(grocerie_rules,by="support")))
inspect(head(sort(grocerie_rules,by=c("count","lift")))) #max count = 228

head(quality(grocerie_rules))

#plot methods matrix , mosaic , doubledecker , graph , paracoord , scatterplot , groupedmatrix , two-key plot , matrix3D ,iplots
#visualisation
plot(grocerie_rules,method = "scatterplot",jitter=0,col=colfunction(5))
plot(grocerie_rules,method = "grouped matrix",col=colfunction(5))
plot(head(sort(grocerie_rules,by="support"),n=20),method="graph")
plot(grocerie_rules,method = "paracoord")
plot(grocerie_rules,method = "matrix")

# using different support and confidence
grocerie_rules2 <- apriori(groceries,parameter = list(support=0.005,confidence = 0.5,minlen=2))
grocerie_rules2
inspect(head(sort(grocerie_rules2,by="lift")))
inspect(head(sort(grocerie_rules2,by="confidence")))
inspect(head(sort(grocerie_rules2,by="support")))
inspect(head(sort(grocerie_rules2,by=c("count","lift")))) #max count = 219

#visualisation
plot(grocerie_rules2,method = "scatterplot",jitter=0,col=colfunction(5))
plot(grocerie_rules2,method = "grouped matrix",col=colfunction(5))
plot(head(sort(grocerie_rules2,by="support"),n=20),method="graph")
plot(grocerie_rules2,method = "paracoord")
plot(grocerie_rules2,method = "matrix")