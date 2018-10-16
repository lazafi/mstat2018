#ruspini
#kmeans
library(cluster)
data(ruspini)

x <- scale (ruspini)

#Hartigan Index: log(Bk/Wk) ie (heterogenity/homogenity)
res <- kmeans(x, 4)
str(res)
log(res$betweenss/res$tot.withinss)


?silhouette

plot(silhouette(res$cluster, dist(x)))

?clusGap

x <- scale(ruspini)
resgap <- clusGap(x, FUN= kmeans, K.max = 10)
plot(resgap)
maxSE(resgap$Tab[,3],resgap$Tab[,4])

#hierarchical
d <- dist(scale(ruspini), method = "euclidean")
res <- hclust(d, method="complete")
str(res)
plot(res)

cutree(res, k=4)

#model clustering

library(mclust)
res <- Mclust(ruspini)

plot(res)