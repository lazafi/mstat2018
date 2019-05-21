#############################################################
#PCA
# R code should be correct, but not documented ...

r <- read.csv("winequality-red.csv",dec=".",sep=";")
w <- read.csv("winequality-white.csv",dec=".",sep=";")

r1 <- princomp(r,cor=TRUE)
biplot(r1)
w1 <- princomp(w,cor=TRUE)
biplot(w1)

r.sc <- scale(r,w1$center,w1$scale)
plot(w1$sco[,1:2])
r.scores <- r.sc%*%w1$loa
points(r.scores[,1:2],col=2)

library(rrcov)
r2 <- PcaHubert(r,scale=TRUE)
biplot(r2)
w2 <- PcaHubert(w,scale=TRUE)
biplot(w2)

plot(r2)
plot(w2)

r.sc <- scale(r,w2@center,w2@scale)
plot(w2@scores[,1:2])
r.scores <- r.sc%*%w2@loadings
points(r.scores[,1:2],col=2)


####################################
rlow <- r[r$quality==3 | r$quality==4,-12]
rhigh <- r[r$quality==7 | r$quality==8,-12]

rhigh.pca <- PcaHubert(rhigh,scale=TRUE)
biplot(rhigh.pca)
rlow.sc <- scale(rlow,rhigh.pca@center,rhigh.pca@scale)
plot(rhigh.pca@scores[,1:2])
rlow.scores <- rlow.sc%*%rhigh.pca@loadings
points(rlow.scores[,1:2],col=2)

rlow.md <- sqrt(mahalanobis(rlow.scores[,1:2],center=c(0,0),diag(rhigh.pca@eigenvalues[1:2])))

# distances:
summary(rhigh.pca) # 3 components explain >80%
k <- 2
sdev.rhigh <- sqrt(rhigh.pca@eigenvalues)
SD.rhigh <- sqrt(apply(t(t(rhigh.pca@scores[,1:k]^2)/sdev.rhigh[1:k]^2),1, sum))
# plot(SD.rhigh,rhigh.pca@sd) # same only for k=5 !
rhigh.md <- sqrt(mahalanobis(rhigh.pca@scores[,1:2],center=c(0,0),diag(rhigh.pca@eigenvalues[1:2])))
# plot(SD.rhigh,rhigh.md) # stimmt
rhigh.s <- scale(rhigh, center = rhigh.pca@center, scale = rhigh.pca@scale)
OD.rhigh <- sqrt(apply((rhigh.s - rhigh.pca@scores[, 1:k] %*% t(rhigh.pca@loadings[, 1:k]))^2, 1, sum))
plot(SD.rhigh,OD.rhigh)
critSD <- sqrt(qchisq(0.975, k))
critOD <- (median(OD.rhigh^(2/3)) + mad(OD.rhigh^(2/3)) * qnorm(0.975))^(3/2)
abline(h=critOD)
abline(v=critSD)

# project other data:
rlow.s <- scale(rlow,rhigh.pca@center,rhigh.pca@scale)
rlow.scores <- rlow.s%*%rhigh.pca@loadings
SD.r <- sqrt(apply(t(t(rlow.scores[,1:k]^2)/sdev.rhigh[1:k]^2),1, sum))
#plot(SD.r,rlow.md) # stimmt
OD.r <- sqrt(apply((rlow.s - rlow.scores[, 1:k] %*% t(rhigh.pca@loadings[, 1:k]))^2, 1, sum))
points(SD.r,OD.r,col=2)

# function for plotting diagnostic plots
#distanceDiag <- function(data, scores, loadings) {
data <- rhigh.s
scores <- rhigh.pca@scores[,1:2]
loadings <- rhigh.pca@loadings[, 1:2]
 #eigen <- eigen(cormat, only.values = TRUE)
  vars <- apply(scores, 2, var)
  #vars <- c(0.481, 0.176)
  #sds <- apply(t(t(scores^2)/vars[1:2]), 1, sum)^(1/2)  
  sds <- sqrt(apply(t(t(scores^2)/vars^2),1, sum))
  sd.cv <-  sqrt(qchisq(0.975, 2))
  #ods <- apply(data - (scores %*% t(loadings)), 1, vecnorm)
  ods <- sqrt(apply((data - scores %*% t(loadings))^2, 1, sum))
  od.cv <- (median(ods^(2/3)) + mad(ods^(2/3)) * qnorm(0.975))^(3/2)
  
  #  plot(sds, ods, xlim=c(min(sds)-0.5,max(sds)+0.5))
  plot(sds, ods)
  abline(v=sd.cv)
  abline(h=sd.cv)
#}

  

