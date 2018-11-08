#vo 20181107

library(bootstrap)
data(scor)

X <- scale(scor)

# if we scale we decomposit the correlation
# so every variable has the same scale = important
eigen(cov(scor))
eigen(cov(X))
#=
eigen(cor(scor))

G <- eigen(cor(scor))$vectors
Z <- X%*%G

#should be 0
apply(Z,2,mean)
cov(Z)

?princomp
res <- princomp(scor, cor=TRUE)

res$sdev^2

cor(X,Z)
# first comp correlated with all vars, second less ... 5th comp still correlated to alg (ie: without comp 5 we might loose information about alg)

G

plot(Z[,1:2])
plot(Z[,1:2],type="n")
text(Z[,1],Z[,2],1:88)

# G represents the contributions of the variables to comp (column=comp)
# x axixs high = average score in all subjects
#y axix high = good results in first 2 subjects and bad results in last 2

# choosing relevant components using test statistics
?pchisq
pchisq(26.11,9)
1-pchisq(26.11,9)
1-pchisq(7,5)
