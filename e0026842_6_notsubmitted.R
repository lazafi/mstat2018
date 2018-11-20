#0026842

#1

library(pls)
data(yarn)
X <- yarn$NIR
str(X)
matplot(X, type="l")
res <- prcomp(X)
str(res$rotation)
matplot(res$rotation, type="l")

str(res$rotation%*%res$x)