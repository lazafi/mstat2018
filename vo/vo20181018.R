#20181018

3.2 multiple regression
BLUE:
Best
linear
Unbiased
Estimator
         
Voraussetzung: Y: normalverteilt (Satz 3.3.4)

#install.packages("bootstrap")
library(bootstrap)
data(scor)
res <- lm(mec~.,data=scor)
summary(res)
# last column: probability that beta = 0

plot(scor$mec, predict(res))
abline(c(0,1))

#validity of assumptions (y=normalverteilt)
plot(res)
# 1. plot: checking for N in residuals
# 2.plot: plot shoud not have any structure: ie: all residuals schoud have the same distribution
# 


#3.4 multivariate linear regression

names(scor)
res <- lm(cbind(mec,vec)~.,data=scor)
summary(res)
plot(res)
class(res)

#use manova for multivariate reg
res <- manova(cbind(mec,vec)~.,data=scor)
summary(res, test="Wilks")
# last column gives test of significance for both Y
?summary.manova

# kapitel 4
# robust statistik
# FRB - robust multivariate regression

library(quantreg)
library(robustbase)
...

# Regression M-estimator
  
