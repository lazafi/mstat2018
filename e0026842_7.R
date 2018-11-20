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
#data with big SD also visible on the biplots

#3
#proj1 <- scale(red.data,center=white.pca@center,scale=white.pca@scale) %*% white.pca@loadings

red.rescaled <- scale(red.data, center=white.pca@center,scale=white.pca@scale)
red.proj <- red.rescaled %*% white.pca@loadings
biplot(white.pca, cex=0.8)
points(red.proj[,1:2], col="grey")

#4

red.high <- red.data[which(red.data$quality==7 | red.data$quality==8),1:11]
red.high.pca <- PcaHubert(red.high, scale=TRUE)
summary(red.high.pca)
red.low <- red.data[which(red.data$quality==2 | red.data$quality==3),1:11]

red.low.proj <- scale(red.low, center=red.high.pca@center, scale=red.high.pca@scale) %*% red.high.pca@loadings 
biplot(red.high.pca)
points(red.low.proj[,1:2], col="green", pch=19)

# low quality wines have all variables in the middle range with higher volatile.acidity and density

#5
plot(red.high.pca)

k = 3
loadings <- red.high.pca@loadings[,1:k]
red.low.scaled <- scale(red.low, center=red.high.pca@center, scale=red.high.pca@scale)
scores <- red.low.scaled %*% loadings
vars <- apply(scores, 2, var)
sds <- sqrt(apply(scores^2 / vars, 1, sum))
#apply(red.low.scaled - t(loadings %*% t(scores)),2,dist)


points(sds, ods, col="red", pch=19)


