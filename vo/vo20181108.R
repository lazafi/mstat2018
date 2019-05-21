#vo20181108

library(bootstrap)
data(scor)

X <- scor
res <- princomp(X, cor = TRUE)

res$sdev^2
sum(res$sdev^2)
# summ of all sdev = 5 (becouse we have 5 variables). X is standartized ie sdev = 1

screeplot(res,type="l")

#which variables are represented in the first PK ?
res$loadings

#computationly:

#we compute the squared correlation between x and pk (Bsp 5.5.4)

#biplot
biplot(res)

#SVD

svd(X)
str(svd(X))

# v = princomp$loadings
# for svd X must be mean-centered
V <- svd(scale(scor,F,T))$v

#biplot
# variable can be reconstructed by projecting it on the arraw
# length of arrow corresponds to the variance represented by 2 pcs
# cos(angle(v1, v2)) represents the cor btw variables