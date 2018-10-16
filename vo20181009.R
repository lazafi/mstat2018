library(ISLR)
data(Auto)
X <- Auto[,c(1,3,4,5,6)]
X.md2 <- mahalanobis(X,center=apply(X,2,mean), cov=cov(X))


#chiq verteilung
#plot(qchisq())

library(mvoutlier)
chisq.plot(X)

# estimate lambda
library(car)
l <- powerTransform(Auto$horsepower)

#boxcox
# har to interpret
x.bc <- bcPower(Auto$horsepower, lambda=l.labda)


#distances
?dist

#k means
data(ruspini)
res <- kmeans(ruspini,4)

