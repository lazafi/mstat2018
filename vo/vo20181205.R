# vo20181205

library(bootstrap)
data(scor)

X <- as.matrix(scor)

x <- X[,1]
y <- X[,-1]
Ryy <- cor(y)
Ryx <- cor(y,x)
n<-nrow(y)
p<-ncol(y)
r2<-t(Ryx)%*%solve(Ryy)%*%Ryx
r2
#summary(lm(mec~.,data=scor))
summary(lm(mec~.,data=scor))

a <- solve(Ryy)%*%Ryx
a

solve(cov(y))%*%cov(y,x)
#same es the liniar model. use variance instead of correlation
# for correlation analysis use correlation

# test for uncorrelateness
Fstat <- (n-1-p)*r2/(p*(1-r2))
Fstat
1-pf(Fstat,p,n-1-p)
#reject -> should be small

# canoncical

?cancor
X <- as.matrix(scor[,1:2])
Y <- as.matrix(scor[,3:5])

res <- cancor(X,Y)
str(res)

plot(X%*%res$xcoef[,1],Y%*%res$ycoef[,1])
cor(X%*%res$xcoef[,1],Y%*%res$ycoef[,1])
# = first value

X.sc <- scale(scor[,1:2])
Y.sc <- scale(scor[,3:5])
res.sc <- cancor(X.sc,Y.sc)
plot(X.sc%*%res.sc$xcoef[,1],Y.sc%*%res.sc$ycoef[,1])
res.sc$xcoef[,1]
res.sc$ycoef[,1]
# = contributions 


library(ccaPP)

res1 <- CCAgrid(X,Y,k=2,method="pearson",standardize = TRUE)
res1
res2 <- CCAgrid(X,Y,k=2,method="spearman",standardize = TRUE)

set.seed(123)
resp <- permTest(X,Y,R=1000,nCores=4,method="pearson")
resp