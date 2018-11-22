#0026842

red.data <- read.csv(file="winequality-red.csv", header=TRUE, sep=";")
str(red.data)

white.data <- read.csv(file="winequality-white.csv", header=TRUE, sep=";")
str(white.data)

set.seed(123)

matplot(sample(red.data,5), type="l")

#1

library(rrcov)
red.pca <- PcaHubert(red.data, scale = TRUE)
summary(red.pca)

white.pca <- PcaHubert(white.data, scale = TRUE)
summary(white.pca)

biplot(red.pca, cex=0.8)
# variables associated quality: alcohol, total.sulfur.dioxide, volatile.acidity, sulphates, free.sulfur.dioxide, chlorides, residual.sugar

biplot(white.pca, cex=0.8)
# variables associated quality: alcohol, density, residual.sugar, clorides

#2

plot(red.pca)
plot(white.pca)
# outliers with large SD also visible on the biplots


#3
#proj1 <- scale(red.data,center=white.pca@center,scale=white.pca@scale) %*% white.pca@loadings

# rescale red-wine data with the same center/scale then white
red.rescaled <- scale(red.data, center=white.pca@center,scale=white.pca@scale)
#calculate scores
red.proj <- red.rescaled %*% white.pca@loadings
#plot biplot and projections
biplot(white.pca, cex=0.8)
points(red.proj[,1:2], col="grey")

#-> plotting inside the biplot not usefull, instad use a simple plot 
plot(white.pca@scores[,1], white.pca@scores[,2], col="red")
points(red.proj[,1], red.proj[,2])

#4

#filter high quality samples, ommit quality variable
red.high <- red.data[which(red.data$quality==7 | red.data$quality==8),1:11]
#calculate pca
red.high.pca <- PcaHubert(red.high, scale=TRUE, k=2)
summary(red.high.pca)

#filter low quality samples, ommit quality variable
red.low <- red.data[which(red.data$quality==3 | red.data$quality==4),1:11]
# calculate scores
red.low.proj <- scale(red.low, center=red.high.pca@center, scale=red.high.pca@scale) %*% red.high.pca@loadings 
biplot(red.high.pca)
points(red.low.proj[,1:2], col="green", pch=19)

# ->
plot(red.high.pca@scores[,1], red.high.pca@scores[,2], col="red")
points(red.low.proj[,1], red.low.proj[,2])

# low quality wines have all variables in the middle range with higher volatile.acidity and density

#5
plot(red.high.pca)

# use k=3 becouse 3 components cover over 80% variance
# -> k=3 means od = 0 becouse there is no orthogonal distance
k = 2
loadings <- red.high.pca@loadings[,1:k]
red.low.scaled <- scale(red.low, center=red.high.pca@center, scale=red.high.pca@scale)
scores <- red.low.scaled %*% loadings
vars <- apply(scores, 2, var)
#-> use eigenvalues:
vars <- red.high.pca@eigenvalues
sds <- sqrt(apply(scores^2 / vars, 1, sum))
library(fields) # for rdist
ods <- diag(rdist(red.low.scaled, t(loadings %*% t(scores))))
#diag(rdist(red.low.scaled[1,], t(loadings %*% t(scores))[1,]))

#-<
a <- red.high.pca@eigenvalues
sd.low <- apply(t(t(scores^2)/a), 1, sum)^(1/2)  #??
red.low.scaled <- scale(red.low, red.high.pca@center, red.high.pca@scale)
G <- red.high.pca@loadings
od.low <- apply(red.low.scaled - scores %*% t(G), 1, vecnorm)

plot(red.high.pca)
points(sd.low, od.low, col=2)

plot(red.high.pca)
points(sds, ods, col="red", pch=19)


