#0026842

#1

library(robustbase)
data(milk)
str(milk)

#1a 
res_ls <- lm(X4~X5, data=milk)
#plot(res_ls)
plot(milk$X5, milk$X4)
abline(res_ls, col="red")


#1b
res_lts <- ltsReg(X4~X5, data=milk)
abline(res_lts, col="green")
summary(res_lts)
#plot(milk$X5, milk$X4, col=res_lts$lts.wt)
#points(milk$X5, res_lts$fitted.values, col=res_lts$lts.wt+2)

#1c
res_rob <- lmrob(X4~X5, data=milk)
abline(res_rob, col="blue")
points(milk$X5, milk$X4, pch=20, cex=res_rob$rweights*3, col="blue")

#1d
plot(res_ls)
#1 trend in the residuals (no normal dist in residuals)
#2
#3 stucture in the residuals
#4 70 potential leverage
plot(res_lts)
#1 
#2
#3 70 bad leverage point, some good lp and vertical outliers
#4 "
plot(res_rob)

# one x outlier
#?
plot(res_lts$lts.wt, res_rob$rweights)

#2

#2a
resm_ls <- lm(X4~., data=milk)
plot(milk$X4,resm_ls$fitted.values)
abline(c(0,1))

resm_lts <- ltsReg(X4~., data=milk)
plot(milk$X4, resm_lts$fitted.values, col=resm_lts$lts.wt+2)
abline(c(0,1))

resm_rob <- lmrob(X4~., data=milk)
plot(milk$X4,resm_rob$fitted.values)
abline(c(0,1))

#robust regression model results in much better predictions, except for the outliners

#2b

#ls
# r2 equals r2 output from summary
summary(resm_ls)
1 - sum((milk$X4 - resm_ls$fitted.values)^2) / sum((milk$X4 - mean(milk$X4))^2)

#lts
# variables
w <- as.vector(resm_lts$lts.wt)
y <- resm_lts$Y
yhat <- resm_lts$fitted.values

# lts r2
lts_mean <- sum(y * w) / sum(w)
rss <- sum(w * (y - yhat)^2)
tss <- sum(w * (y - lts_mean)^2)
1 - rss/tss
# summary.lts calculates the mean differently, using fitted values instead of the original y
# also it uses a different formula
lts_mean <- sum(yhat * w) / sum(w)
rss <- sum(w * (y - yhat)^2)
tss <- sum(w * (yhat - lts_mean)^2)
tss/(tss+rss)

# in the robust case r2 ignores outliers, so its also not very informative

#rob r2
w <- as.vector(resm_rob$rweights)
y <- milk$X4
yhat <- resm_rob$fitted.values

rob_mean <- sum(y * w) / sum(w)
rss <- sum(w * (y - yhat)^2)
tss <- sum(w * (y - lts_mean)^2)
1 - rss/tss


#2c

summary(resm_ls)
# X3 constributes most significantly to the model
# r^2 of 0.86 not very good
summary(resm_lts)
# X3 and X6 contribute strongly, X7 slightly to the model
# r^2 of 0.9707 better
summary(resm_rob)
# X3 most significant but X5,X6 and X7 contribute

#2d
plot(resm_ls)
#1 several outliers in x space, these also have big residuals
#2 residuals not normal distributed
#3 1,2,41 coud be outliers but still in the range +-2.5 becouse no robust scale is used for standartising residuals
#4 

plot(resm_lts)
#1 residuals normal distributed except clear outliers 1,2,41,70
#2 standartised residuals in the range of -2.5 to 2.5, clear outliers
#3 "
#4 most stand.residuals in range +-2.5. lot of samples are good leverage points
#  samples 1,2,41 big residuals but distance still below chi^2=2.75. sample 70 bad leverage point 

plot(resm_rob)
#1 most residuals in range +-2.5 but lot of them with RD above 2.75 of chi² = good leverage points
#  70 bad leverage point
#2 most residuals normal distributed
#3 good fit except for outliers
#4 residuals in ?range?
#5 residuals big for bix Y
