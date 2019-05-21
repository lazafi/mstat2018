#0026842
#ex2

#install.packages('classifly')
library(classifly)
data(olives)
str(olives)
plot(olives, col=olives$Region)

# select the numeric columns, exclude eicosenoic TODO: why?
allcols <- c("palmitic", "palmitoleic", "stearic", "oleic", "linoleic", "linolenic", "arachidic", "eicosenoic" )
cols <- c("palmitic", "palmitoleic", "stearic", "oleic", "linoleic", "linolenic", "arachidic" )
selcols <- olives[,cols]

# all clustering methods rely on distance messures being comperable, thus scale all variables
#s <- scale(selcols, center=FALSE)
s <- scale(selcols)

# scatterplot
plot(data.frame(s), col=olives$Region)
str(s)

##1

# kmeans with 3 clusters
res1 <- kmeans(s, 3)

# missclassified samples
# NOTE: due to random labeling of the resulting clusters we cannot rely on this method of comparing cluster-labels to Regions
sum(res1$cluster != olives$Region)

# to stabilize the results we calculate 3 cluster centers according to Region
centers <- sapply(1:3, function (x) { colMeans(s[olives$Region==x,]) })

# use the centers to initialize kmeans
# -> this intriduces apriori structure into the data! ie: 3 clusters
res2 <- kmeans(s, t(centers))

# missclassified samples
sum(res2$cluster != olives$Region)

##2

###Calinski-Harabasz
chs <- lapply(2:15, function(k) {
  r <- kmeans(s,k, nstart=10)
  (r$betweenss/(k-1))/(r$tot.withinss/(nrow(s)-k))
})
plot(2:15,chs, xlab="k")

###hartigan
hartigans <- lapply(2:15, function (k) {
  r <- kmeans(s,k)
  #ret <- c(x=k, y=log(r$betweenss/r$tot.withinss))
  ret <- log(r$betweenss/r$tot.withinss)
  #names(ret) <- c("x", "y")
  ret
})
plot(2:15, hartigans, xlab="k")


###silhouette
library(cluster)
# silhoutte not working on original data
#plot(silhouette(olives$Region, dist(s)))

#subsample olives with 100 random samples
#only few samples have s > 0.4, most of samples around 0 or below
sub_olives <- olives[sample(1:nrow(olives), 100, replace=FALSE),]
subs <- scale(sub_olives[,cols])
dist <- dist(subs) 
sil <- silhouette(sub_olives$Region, dist)
# shoud have used kmeans with different ks. then compare the avg.silhuette and pick the maximum
plot(sil)


###gap
?clusGap
resgap <- clusGap(s, FUN= kmeans, K.max = 10)
plot(resgap)
maxSE(resgap$Tab[,3],resgap$Tab[,4])

###comparison

#kmeans with k=3 and predefined centers classifies most of the samples correctly
#finding the optimal value for k is not reliable with Calinski-Harabasz or Hartigan index. values between 4-6
#silhouette reliably finds 3 clusters
#gap gives values from 3 to 6

##3

d <- dist(s, method = "euclidean")
res <- hclust(d, method="complete")
str(res)
plot(res)
c <- cutree(res, k=3)

res_complete <- hclust(d, method="complete")
plot(res_complete)
table(cutree(res_complete, k=3), olives$Region)
sum(cutree(res_complete, k=3) != olives$Region)

res_single <- hclust(d, method="single")
plot(res_single)
sum(cutree(res_single, k=3) != olives$Region)

res_average <- hclust(d, method="average")
plot(res_average)
sum(cutree(res_average, k=3) != olives$Region)

###comparison
# resulting dendrogram very different, "complete-linkage" looks most promising

##4

library(mclust)
mclust_res <- Mclust(s, G = 1:10)
sum(mclust_res$classification != olives$Region)
plot(mclust_res)
