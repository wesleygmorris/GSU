library(factoextra)
library(cluster)
setwd("~/Dropbox/GSU/Feedback Comp/PERSUADEnlpToolData")
###LOAD
index_data <- read.csv('index_data.csv')
set.seed(42)

head(index_data)
df <- as.data.frame(scale(index_data[,5:55]))
head(df)
#define linkage methods
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

#function to compute agglomerative coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}

#calculate agglomerative coefficient for each clustering linkage method
sapply(m, ac)

#perform hierarchical clustering using Ward's minimum variance
clust <- agnes(df, method = "ward")

#produce dendrogram
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram") 

#calculate gap statistic for each number of clusters (up to 10 clusters)
gap_stat <- clusGap(df, FUN = hcut, nstart = 25, K.max = 10, B = 50)

#produce plot of clusters vs. gap statistic
fviz_gap_stat(gap_stat)

#compute distance matrix
d <- dist(df, method = "euclidean")

#perform hierarchical clustering using Ward's method
final_clust <- hclust(d, method = "ward.D2" )

#cut the dendrogram into 4 clusters
groups <- cutree(final_clust, k=4)

#find number of observations in each cluster
table(groups)

#append cluster labels to original data
final_data <- cbind(index_data, cluster = groups)

#display first six rows of final data
head(final_data)

###KMEANS CLUSTERING
wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i, iter.max = 20)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group")}

wssplot(df, nc = 20)

#Define kmeans_clusters
library(cluster)
set.seed(123)
kmeans_clusters <- kmeans(df, centers = 5, iter.max = 40)
kmeans_clusters

#Silhouette chart
kmeans_clusters <- kmeans(df, centers = 5, iter.max=20)
sil <- silhouette(kmeans_clusters$cluster, dist(df))
fviz_silhouette(sil)

#Visualize Plots
library(fpc)
plotcluster(df, kmeans_clusters$cluster)
clusplot(df, kmeans_clusters$cluster, color=TRUE, shade=TRUE)
