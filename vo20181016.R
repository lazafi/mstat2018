
#zur ex2
# howto compare cluster with region
library(e1071)
table(olives$Region,res$cluster)
matchedClasses(table(olives$Region,res$cluster))

#vo 20181016

library(cluster)
data(ruspini)
res <- fanny(ruspini,k=5)
plot(res)
res$membership
apply(res$membership, 1, which.max)

res <- fanny(ruspini,k=4)
plot(ruspini, col=gray(1-res$membership[,1]))


