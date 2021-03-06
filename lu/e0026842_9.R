# 0026842

library(StatDA)
library(rrcov)

car.data.raw <- read.csv(file="cardata.csv", header=TRUE, sep=",")
car.data.proj <- car.data.raw[,-c(1:9,15,16,18)]
car.data <- na.omit(car.data.proj)
str(car.data)


# function for plotting diagnostic plots
distanceDiag <- function(data, scores, loadings) {
  #eigen <- eigen(cormat, only.values = TRUE)
  #vars <- apply(scores, 2, var)
  #->
  vars <- diag(loadings %*% t(loadings))
  #sds <- sqrt(apply(t(t(scores^2)/vars^2),1, sum))
  #SD <- sqrt(apply(scores^2 / ))
  # -> sds are mahalanobis distances
  sds <- mahalanobis(scores,rep(0,ncol(scores)),cov(scores))
  sd.cv <-  sqrt(qchisq(0.975, 2))
  #ods <- apply(data - (scores %*% t(loadings)), 1, vecnorm)
  ods <- sqrt(apply((data - scores %*% t(loadings))^2, 1, sum))
  od.cv <- (median(ods^(2/3)) + mad(ods^(2/3)) * qnorm(0.975))^(3/2)

  plot(sds, ods)
  abline(v=sd.cv)
  abline(h=od.cv)
}



#1a

car.fa <- pfa(scale(car.data),factors=2,scores="regression")
summary(car.fa)
biplot(car.fa$scores[,1:2], car.fa$loadings[,1:2])
#-> uniquenesses: big u indicates there is not much info in the factors

#1b
distanceDiag(scale(car.data), car.fa$scores, car.fa$loadings)
#DiagPlot(scale(car.data), car.fa$scores, car.fa$loadings)
#2
cov <- covMcd(car.data)
#<- scale with robust
car.fa2 <- pfa(scale(car.data,cov$center,sqrt(diag(cov$cov))), factors=2, scores="regression", covmat=cov)
biplot(car.fa2$scores[,1:2], car.fa2$loadings[,1:2])
distanceDiag(scale(car.data), car.fa2$scores, car.fa2$loadings)

#3
# only non diesel
car.data.nondiesel <- na.omit(car.data[car.data.raw$fuel.type!="diesel",])
car.data.sc <- scale(car.data.nondiesel)

car.fa3 <- pfa(car.data.sc, factors=2, scores="regression")
biplot(car.fa3$scores[,1:2], car.fa3$loadings[,1:2])
distanceDiag(scale(car.data.nondiesel), car.fa3$scores, car.fa3$loadings)


# only diesel
car.data.diesel <- na.omit(car.data[car.data.raw$fuel.type=="diesel",])
#compute scores
center <- attr(car.data.sc, "scaled:center")
scale <- attr(car.data.sc, "scaled:scale")
car.data.diesel.sc <- scale(car.data.diesel, center=center, scale=scale)
scores <- car.data.diesel.sc %*% car.fa3$loadings
vars <- apply(scores, 2, var)
sds_d <- apply(t(t(scores^2)/vars[1:2]), 1, sum)^(1/2)  
ods_d <- apply(car.data.diesel.sc - (scores %*% t(car.fa3$loadings)), 1, vecnorm)
points(sds_d, ods_d, col=2)


#4

car.fa4 <- pfa(scale(car.data),factors=2,scores="regression",rotation = "none")
library(GPArotation)
car.fa4.v <- pfa(scale(car.data),factors=2,scores="regression",rotation = "varimax")
biplot(car.fa4.v$scores[,1:2], car.fa4.v$loadings[,1:2])
#car.fa4.o <- pfa(scale(car.data),factors=2,scores="regression",rotation = "oblimin")
#biplot(car.fa4.o$scores[,1:2], car.fa4.o$loadings[,1:2])

#c ->
# see solution


