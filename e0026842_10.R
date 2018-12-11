#0026842

library(ggplot2movies)

data(movies)
str(movies)
?movies


#delete 
data <- na.omit(movies)


x <- data[,"rating"]
y <- as.matrix(data[,c("year", "length", "budget", "votes")])


#1a

Ryy <- cor(y)
Ryx <- cor(y,x)
n<-nrow(y)
p<-ncol(y)
corcoef<-t(Ryx)%*%solve(Ryy)%*%Ryx
corcoef

#coefficients
coef <- solve(Ryy)%*%Ryx
coef
# compute linear model for comparison
m.lm <- lm(rating~year+length+budget+votes,data=data)
summary(m.lm)
# r2 == corcoef

#1b

#F-test
#?
Fstat <- (n-1-p)*corcoef/(p*(1-corcoef))
Fstat
1-pf(Fstat,p,n-1-p)

y <- as.matrix(data[,c("year", "length", "budget")])

Ryy <- cor(y)
Ryx <- cor(y,x)
n<-nrow(y)
p<-ncol(y)
corcoef<-t(Ryx)%*%solve(Ryy)%*%Ryx
corcoef

Fstat <- (n-1-p)*corcoef/(p*(1-corcoef))
Fstat
1-pf(Fstat,p,n-1-p)
# slightly more correlation

#1c

library(ccaPP)
CCAgrid(x, y, method="pearson")
CCAgrid(x, y, method="spearman")
# coef much bigger, different correlation


set.seed(123)
permTest(X,Y,R=1000,nCores=4,method="pearson")
permTest(X,Y,R=1000,nCores=4,method="spearman")
# yes same result -> correlation is significant

#2a

?cancor
X <- as.matrix(data[,c("year", "length", "budget", "rating", "votes")])
Y <- as.matrix(data[,c("Action", "Animation", "Comedy", "Drama", "Documentary", "Romance", "Short")])

res <- cancor(scale(X),scale(Y), xcenter=FALSE,ycenter=FALSE)
res
res$cor

plot(res$cor)

