#vo20181220

library(cluster)
data(ruspini)
plot(ruspini)

# grupper per kmeans
grp <- kmeans(ruspini,4)
plot(ruspini,col=grp$cluster)

# lda
library(MASS)
res <- lda(ruspini,grp$cluster)
x <- c(55,80)
predict(res,x)

#qda
library(MASS)
res <- qda(ruspini,grp$cluster)
x <- c(55,80)
predict(res,x)