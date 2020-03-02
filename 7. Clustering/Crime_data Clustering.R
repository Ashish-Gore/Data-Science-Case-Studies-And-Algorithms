crime <- read.csv(file.choose())
View(crime)
norm_crime <- scale(crime[,-1])  
View(norm_crime)
library(animation)
library(cluster)

#finding number of clusters
wss=(nrow(norm_crime)-1)*sum(apply(norm_crime,2,var))
for(i in 2:8) wss[i]=sum(kmeans(norm_crime,centers=i)$withinss)
plot(1:8,wss,type = "b",xlab = "clusters",ylab = "with in sum of squares",main = "K-means clustering")
############### 2 is the number of clusters ####################

#to find optimal no of clusters
#install.packages("factoextra")
library(factoextra)
fviz_nbclust(norm_crime,method = 'wss',FUNcluster = kmeans)
fviz_nbclust(norm_crime,method = 'silhouette',FUNcluster = kmeans)
fviz_nbclust(norm_crime,method = 'gap_stat',FUNcluster = kmeans)

# from all the elbow plots it is clear that optimal number of clusters is 2

final <- kmeans(norm_crime,2)
finalanim <- kmeans.ani(norm_crime,2)
fviz_cluster(final,data = crime[-1])
finaldata <- data.frame(final$cluster,crime)
View(finaldata)
aggregate(crime[,-1],by=list(final$cluster),FUN = mean)