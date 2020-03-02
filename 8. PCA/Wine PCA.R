library(corrplot)
library(psych)
wine1 <- read.csv(file.choose())
wine <- wine1[,-1]
View(wine)
summary(wine)
describe(wine)
str(wine)
attach(wine)
a <- cor(wine)
a
corrplot(a,method = "number")
pcawine <- princomp(wine,cor = TRUE,scores = TRUE,covmat = NULL) #PCA
summary(pcawine)
plot(pcawine)
biplot(pcawine)
pcawine$scores
wine_score <- cbind(wine,pcawine$scores[,1:3]) #[,1:3]considering 66.5% pca components scores as per problem statement
View(wine_score)


#cluster analysis
############### hierarchial clustering for principle components scores ###############
library(factoextra)
library(NbClust)
library(dendextend)

clust_data <-wine_score[,14:16] 
norm_clust <- scale(clust_data)
clust <- eclust(norm_clust,"hclust",k=3,graph = FALSE)
fviz_dend(clust,rect = TRUE)
groups <- cutree(clust,k=3)
finalhclust <- data.frame(groups,wine)


aggregate(finalhclust,by=list(clust$cluster),FUN=mean)

################ kmeans clustering for principle components scores ##################

wss <- (nrow(norm_clust)-1)*sum(apply(norm_clust,2,var))
for(i in 1:10){
  wss[i]=sum(kmeans(norm_clust,centers = i)$withinss)
}
plot(1:10,wss, type = "o") #from scree plot no of clusters is 3
#alternative method
noofclust <- NbClust(clust_data,distance = "euclidean",method = "kmeans",min.nc = 2,max.nc = 10,index = "all")
fviz_nbclust(noofclust)                                  #optimal number of clusters
km <- kmeans(norm_clust,3)
fviz_cluster(km,data = wine[-1])
final <- data.frame(km$cluster,wine)                    


aggregate(final,by=list(km$cluster),FUN = mean)

# no of clusters of principle components scores is similar to the wine data (i.e. problem statement)