#e0026842

#1 fuzzy clustering

library(classifly)
data(olives)
cols <- c("palmitic", "palmitoleic", "stearic", "oleic", "linoleic", "linolenic", "arachidic" )

sed.seed(123)

library(cluster)
res_fanny <- fanny(olives[,cols], k = 3)
summary(res_fanny)
library(StatDA)
ternary(res_fanny$membership, grid=TRUE, col=olives$Region)
?ternary

library(e1071)
?cmeans
res_cmeans <- cmeans(olives[,cols], 3)
summary(res_cmeans)
ternary(res_cmeans$membership, grid=TRUE, col=olives$Region)

#conclusions
# cmeans classifies samples more clearly to one class

#2 multiple regression

school = read.csv("schooldata.csv", header = TRUE)

# linear model dependent variable: reading
m1 <- lm(reading~education+occupation+visit+counseling+teacher,data=school)
summary(m1)
# occupation most significant coefficient

#plot(m1)
#plot(school$reading, predict(m1))
#abline(c(0,1))


# linear model dependent variable: mathematics
m2 <- lm(mathematics~education+occupation+visit+counseling+teacher,data=school)
summary(m2)
# occupation most significant coefficient, visit slightly and others not significant
# plot occupation vs mathematics
plot(school$occupation, school$mathematics)
# visualize model (using only occupation coefficient)
abline(m1$coefficients[1], m1$coefficients[3])
#plot(m2)

# linear model dependent variable: selfesteem
m3 <- lm(selfesteem~education+occupation+visit+counseling+teacher,data=school)
summary(m3)
# occupation most significant coefficient but education and visit also significant
#plot(m3)

#3

# linear model dependent variable is combination of reading,mathematics,selfesteem
m4 <- lm(cbind(reading,mathematics,selfesteem)~education+occupation+visit+counseling+teacher,data=school)
summary(m4)
#plot(m4)
# model just a combination o m1 m2 m3, not usefull
# occupation most significant, also education and visit

#4

m5 <- manova(cbind(reading,mathematics,selfesteem)~education+occupation+visit+counseling+teacher,data=school)
summary(m5, test="Wilks")
#education, occupation and visit significant

#5 stepwise variable selection

m1s <- step(m1)
summary(m1s)

# dropped variables
m1s$anova
# AIC is minimized by stepwise dropping of variables

m2s <- step(m2)
summary(m2s)

m3s <- step(m3)
summary(m3s)

# the resulting models are simular to previous models but use only the 3 most significant variables
# adjusted r-squared slightly better in simpler models

#6 stepwise variable selection for multivariate model

step(m5)

# AIC not appropriate
# use other messure to drop variables from model

#7 crossValidation

#install.packages("cvTools")
library(cvTools)

?cvFit
m5_cv <- cvFit(m5,data=school,y=cbind(school$reading,school$mathematics,school$selfesteem),R=100)
summary(m5_cv)
plot(m5_cv)
# cvFit evaluates the model by crossValidating. Splits the data in blocks and uses one block to fit the model and another for predictions. return estimated errors

# do CV with all possible combinations of variables
# variables
vars <- c("education","occupation","visit","counseling","teacher");
# construct formulas with all possible combinations
n <- length(vars)
id1<-unlist(lapply(1:n,function(x)combn(1:n,x,simplify=F)),recursive=F) 
f1<-lapply(id1,function(x)paste("cbind(reading,mathematics,selfesteem)~",paste(vars[x],collapse="+"))) 
# build models with all formulas
res1 <- lapply(f1,function(x) manova(as.formula(x),data=school)) 
# crossvalidate all models
cvs <- lapply(res1, function(x) cvFit(x,data=school,y=cbind(school$reading,school$mathematics,school$selfesteem),R=5)$cv)
# choose model with min error
min_idx <- which.min(unlist(cvs))
winner <- res1[[min_idx]]
# print model details
summary(winner)
cvs[min_idx]

#8 plot predictions

plot(school$reading, predict(winner)[,"reading"], xlab="reading", ylab="y-hat")
abline(c(0,1))
cor(school$reading, predict(winner)[,"reading"])
plot(school$mathematics, predict(winner)[,"mathematics"], xlab="mathematics", ylab="y-hat")
abline(c(0,1))
cor(school$mathematics, predict(winner)[,"mathematics"])
plot(school$selfesteem, predict(winner)[,"selfesteem"], xlab="selfesteem", ylab="y-hat")
abline(c(0,1))
cor(school$selfesteem, predict(winner)[,"selfesteem"])

# correlation = 1 means perfectly predicted values -> same as r_square = not appropriate