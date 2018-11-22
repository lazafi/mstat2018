# 20181122

#Factor analysis

library(bootstrap)
data(scor)

Y<- scale(scor)

library(StatDA)
# principal factor analysis
res1 <- pfa(Y,factors=1, rotation="none")
res1 <- pfa(Y,factors=2, rotation="none") 
str(res1)
#uniquenessed : variances of the error term
res1$uniquenesses
#loadings : lamda
res1$loadings



# max likelyhood method
res <- factanal(Y,3)

# estimates of the communalities by hand
R <- cor(Y)
Rinv <- solve(R)
1-1/diag(Rinv)

