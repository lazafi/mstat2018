#0026842

#Robust covariance estimation and regression diagnostics

##1

library(UsingR)
attach(fat)

formula <- body.fat ~ . - case - body.fat.siri - density

res_ls <- lm(formula, data=fat)

###1a

hat <- lm.influence(res_ls)$hat

#calculated:
#X <- as.matrix(subset(fat, select(c(-body.fat,~case))))
X <- cbind(rep(1,n),X)
H <- X %*% solve(t(X) %*% X) %*% t(X)


plot(hat)

q <- mean(hat) * 252 -1
# calculated:
hii <- diag(H)
q <- sum(hii) -1
lev <- which(hii > 2 * (q*1)/n)



n <- nrow(fat)
upper_bound = 2 * (q+1)/n 
abline(a=upper_bound, b=0)

###1b

library(robustbase)

data <- fat[,c(5:19)]

# compute robust md
rs <- covMcd(data)
rmd <- mahalanobis(data, rs$center, rs$cov)
# or
rmd <- sqrt(rs$mah)

sqrt(qchisq(0.975,q))

plot(rmd, log="y")
abline(a=sqrt(qchisq(0.975,q)), b=0 ) #?

# exclude row 42
plot(rmd[c(1:41,43:252)])

##2
# training data
data_s <- as.data.frame(fat[101:252,])
str(data_s)

###2a
res.lm <- lm(formula, data_s)
summary(res.lm)
# very significant: weight height BMI ffweight 
# also: abdomen, thigh
plot(res.lm)
#1 clear structure in the resuduals


library(robustbase)
res.lmrob <- lmrob(formula, data_s)
summary(res.lmrob)
#most significant: weight, ffweight, knee
plot(res.lmrob)
#1 many outliers masked by standarized residuals


###2b
cd <- cooks.distance(res.lm)

#computed
plot(cd, res.lmrob$rweights)
abline(v=(4/(252-100)))

#plot(cd)
plot(cd[c(1:41,43:252)], ylim=c(0,1))
abline(4/nrow(fat),0)
points(1- res.lmrob$rweights, col="red")
# robust model downweights far more observations


###2c
data_new <- as.data.frame(fat[1:100,])

#lm
pre.lm <- predict(res.lm, data_new)
plot(data_new$body.fat, pre.lm)
abline(1,1)

# rquared
1 - sum((data_new$body.fat - pre.lm)^2) / sum((data_new$body.fat - mean(data_new$body.fat))^2)
#or
mean((data_new$body.fat - pre.lm)^2)

#rubust
pre.lmrob <- predict(res.lmrob, data_new)
plot(data_new$body.fat, pre.lmrob)
abline(1,1)

# rquared
1 - sum((data_new$body.fat - pre.lmrob)^2) / sum((data_new$body.fat - mean(data_new$body.fat))^2)
# better use robust method (ie with trimmed average)

## PCA
library(ISLR)
data(Auto)

str(Auto)
data2 <- Auto[c("mpg", "displacement", "horsepower", "weight", "acceleration")]

pc <-princomp(data2, cor=TRUE)
summary(pc)
# first 2 components represent 90% of the cum. variance
plot(pc)
pc$loadings

pc$scores

plot(pc$scores[,1], pc$scores[,2], col=Auto$origin)
# large 1 comp = smaler cars

pc.rob <-princomp(data2, cor=TRUE, covmat = covMcd(data2))
summary(pc.rob)
plot(pc.rob)
plot(pc.rob$scores[,1], pc.rob$scores[,2], col=Auto$origin)


