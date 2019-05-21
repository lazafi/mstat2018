#vo20181024

library(robustbase)
?lmrob

data("starsCYG")
?starsCYG
plot(starsCYG)
mod.lm <- lm(log.light~log.Te, data=starsCYG)
abline(mod.lm)

mod.rob <- lmrob(log.light~log.Te, data=starsCYG)
abline(mod.rob,col=3)
summary(mod.rob)

hist(mod.rob$residuals)
# see outliers

#residual scale estimator is important.
#look at standard-deviation
sd(mod.rob$residuals)
# vs robust estimator:
mod.rob$scale
#vs mad
mad(mod.rob$residuals)

plot(mod.rob)
#1 cut at 2.5 and -2.5: range of most normal distributet scaled residuals 
#2 outliers not satisfy normal distribution
#3 outliers not predicted well

w <- mod.rob$rweights
s <- mod.rob$scale
r <- mod.rob$residuals
plot(r/s,w)
# w function

?ltsReg
mod.ltsreg <- ltsReg(log.light~log.Te, alpha=0.9, data=starsCYG)
mod.ltsreg$lts.wt
sum(mod.ltsreg$lts.wt)

# weights 0 or 1


plot(starsCYG)
abline(mod.ltsreg, col=4)

#20181025

library(robustbase)
data(hbk)
?hbk
plot(hbk)
plot(lm(Y~.,data=hbk))
#4 cook distance: leverage points

#lp: outlier in x
# good leverage points: outlier in x but on the regression line

library(bootstrap)
data(scor)
dim(scor)
X <- scor
colMeans(X)

1/88*t(X)%*%rep(1,88)

# cmd estimator
covMcd(X)

#distance distance plot
plot(covMcd(X))
# upper left corner: outliers masked in classical mahalanobis


plot(lmrob(Y~.,data=hbk))
#??
install.packages("ISLR")
library(ISLR)
data(Hitters)

ltsReg(Salary~.,data=X)
# does not work becouse of caterogical vars
# thats the downside of rubust estimatiors




