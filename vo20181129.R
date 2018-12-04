#vo 20181129

library(bootstrap)
data(scor)
Y<- scale(scor)

library(StatDA)
res1<- pfa(Y,factors = 1, rotation="none")
res2<- pfa(Y,factors = 2,rotation = "none")
# no rotation

library(GPArotation)
res3<-pfa(Y,factors = 2,rotation = "quartimax")
?quartimax

quartimax(res2$loadings)

varimax(res2$loadings)

res3 <- quartimin(res2$loadings)
# invertible retmat
res3$Th

res4<- pfa(Y,factors = 2,rotation = "varimax", scores="regression")
res5<- pfa(Y,factors = 2,rotation = "varimax", scores="Bartlett")

plot(res4$scores)
points(res5$scores, col=2)

biplot(res4$scores[,1:2], res4$loadings[,1:2])
