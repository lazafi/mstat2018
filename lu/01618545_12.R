###################################################################
#                           Exercise 11                           #
###################################################################

library(MASS)
library(rrcov)

redwine <- read.csv("winequality-red.csv", sep = ";")
redwine$quality <- factor(redwine$quality, levels = 3:8, labels = c(rep("Low", times = 3), 
                                                                    rep("High", times = 3)))
set.seed(123)
train <- sample(nrow(redwine), round(nrow(redwine) * 2/3))
test <- (1:nrow(redwine))[-train]


### Task 1 ###

wine1 <- redwine[,c(1,11,12)]
plot(wine1[train,], col = ifelse(wine1[train,3] == "Low", 2, 3))

## a ##
res.lda <- lda(wine1[train,-3], wine1[train,3])
pred.lda <- predict(res.lda, wine1[test,-3])
tab.lda <- table(wine1[test,3], pred.lda$class)
mis.lda <- 1-sum(diag(tab.lda))/sum(tab.lda) # misclassification rate

## b ##
np <- length(test)
nd.x <- seq(from = min(wine1[,1]), to = max(wine1[,1]), length.out = np)
nd.y <- seq(from = min(wine1[,2]), to = max(wine1[,2]), length.out = np)
nd <- expand.grid(x = nd.x, y = nd.y)

prd <- as.numeric(predict(res.lda, newdata = nd)$class)

plot(wine1[test,1:2], col = ifelse(wine1[test,3] == "Low", 2, 3))
points(res.lda$means, pch = "+", cex = 3, col = 2:3)
contour(x = nd.x, y = nd.y, z = matrix(prd, nrow = np, ncol = np), 
        levels = c(1, 2), add = TRUE, drawlabels = FALSE, lwd = 2)

## c ##
res.qda <- qda(wine1[train,-3], wine1[train,3])
pred.qda <- predict(res.qda, wine1[test,-3])
tab.qda <- table(wine1[test,3], pred.qda$class)
mis.qda <- 1-sum(diag(tab.qda))/sum(tab.qda) # misclassification rate

prd <- as.numeric(predict(res.qda, newdata = nd)$class)

plot(wine1[test,1:2], col = ifelse(wine1[test,3] == "Low", 2, 3))
points(res.qda$means, pch = "+", cex = 3, col = 2:3)
contour(x = nd.x, y = nd.y, z = matrix(prd, nrow = np, ncol = np), 
        levels = c(1, 2), add = TRUE, drawlabels = FALSE, lwd = 2)

## d ##
# LdaClassic
res.lda.classic <- LdaClassic(wine1[train,-3], wine1[train,3])
pred.lda.classic <- predict(res.lda.classic, wine1[test,-3])
tab.lda.classic <- table(wine1[test,3], pred.lda.classic@classification)
mis.lda.classic <- 1-sum(diag(tab.lda.classic))/sum(tab.lda.classic) # misclassification rate

prd <- as.numeric(predict(res.lda.classic, newdata = nd)@classification)

plot(wine1[test,1:2], col = ifelse(wine1[test,3] == "Low", 2, 3))
points(res.lda.classic@center, pch = "+", cex = 3, col = 2:3)
contour(x = nd.x, y = nd.y, z = matrix(prd, nrow = np, ncol = np), 
        levels = c(1, 2), add = TRUE, drawlabels = FALSE, lwd = 2)

# QdaClassic
res.qda.classic <- QdaClassic(wine1[train,-3], wine1[train,3])
pred.qda.classic <- predict(res.qda.classic, wine1[test,-3])
tab.qda.classic <- table(wine1[test,3], pred.qda.classic@classification)
mis.qda.classic <- 1-sum(diag(tab.qda.classic))/sum(tab.qda.classic) # misclassification rate

prd <- as.numeric(predict(res.qda.classic, newdata = nd)@classification)

plot(wine1[test,1:2], col = ifelse(wine1[test,3] == "Low", 2, 3))
points(res.qda.classic@center, pch = "+", cex = 3, col = 2:3)
contour(x = nd.x, y = nd.y, z = matrix(prd, nrow = np, ncol = np), 
        levels = c(1, 2), add = TRUE, drawlabels = FALSE, lwd = 2)

# Linda
res.linda <- Linda(wine1[train,-3], wine1[train,3])
pred.linda <- predict(res.linda, wine1[test,-3])
tab.linda <- table(wine1[test,3], pred.linda@classification)
mis.linda <- 1-sum(diag(tab.linda))/sum(tab.linda) # misclassification rate

prd <- as.numeric(predict(res.linda, newdata = nd)@classification)

plot(wine1[test,1:2], col = ifelse(wine1[test,3] == "Low", 2, 3))
points(res.linda@center, pch = "+", cex = 3, col = 2:3)
contour(x = nd.x, y = nd.y, z = matrix(prd, nrow = np, ncol = np), 
        levels = c(1, 2), add = TRUE, drawlabels = FALSE, lwd = 2)

# QdaCov
res.qda.cov <- QdaCov(wine1[train,-3], wine1[train,3])
pred.qda.cov <- predict(res.qda.cov, wine1[test,-3])
tab.qda.cov <- table(wine1[test,3], pred.qda.cov@classification)
mis.qda.cov <- 1-sum(diag(tab.qda.cov))/sum(tab.qda.cov) # misclassification rate

prd <- as.numeric(predict(res.qda.cov, newdata = nd)@classification)

plot(wine1[test,1:2], col = ifelse(wine1[test,3] == "Low", 2, 3))
points(res.qda.cov@center, pch = "+", cex = 3, col = 2:3)
contour(x = nd.x, y = nd.y, z = matrix(prd, nrow = np, ncol = np), 
        levels = c(1, 2), add = TRUE, drawlabels = FALSE, lwd = 2)

## e ##
mis <- c(mis.lda, mis.qda, mis.lda.classic, mis.qda.classic, mis.linda, mis.qda.cov)
names(mis) <- c("lda", "qda", "LdaClassic", "QdaClassic", "Linda", "QdaCov")
sort(mis)
# As we can see the robust implementations did not obtain better misclassification rates,
# and the quadratic models did worse than the linear ones.


### Task 2 ###

## a ##
res.all.lda <- lda(quality ~., data = redwine[train,])
pred.all.lda <- predict(res.all.lda, redwine[test,-12])
tab.all.lda <- table(redwine[test,12], pred.all.lda$class)
1-sum(diag(tab.all.lda))/sum(tab.all.lda) # misclassification rate

res.all.class <- LdaClassic(quality ~., data = redwine[train,])
pred.all.class <- predict(res.all.class, redwine[test,-12])
tab.all.class <- table(redwine[test,12], pred.all.class@classification)
1-sum(diag(tab.all.class))/sum(tab.all.class) # misclassification rate

kappa(res.all.class@cov) # Wenn die Zahl groß ist, ist die Matrix nahezu singulär
 # und die Kovarianz Matrix ist nahezu instabil.
eigen(res.all.class@cov)$val

## b ##
res.all.class <- LdaClassic(quality ~., data = redwine[train,])
pred.all.class <- predict(res.all.class, redwine[test,-12])
tab.all.class <- table(redwine[test,12], pred.all.class@classification)
1-sum(diag(tab.all.class))/sum(tab.all.class) # misclassification rate

# The lda() function scales the data, while the LdaClassic() function works with
# the 'raw' data. If I scale the data and then apply LdaClassic(), the same
# misclassification rate as for lda() is obtained.

x <- as.data.frame(scale(redwine[,-12]))
x$quality <- redwine[,12]
res.sc <- LdaClassic(quality~., data = x[train,])
pred.sc <- predict(res.sc, x[test,-12])
tab.sc <- table(x[test,12], pred.sc@classification)
1-sum(diag(tab.sc))/sum(tab.sc)

# dimension reduction using PcaHubert()
wine.pca <- PcaHubert(redwine[train,-12], scale = TRUE, k = 10)
summary(wine.pca)
k <- 10

scores.train <- wine.pca@scores[,1:k] # We take k components
scores.test <- scale(redwine[test,-12], wine.pca@center, wine.pca@scale) %*% 
  wine.pca@loadings[,1:k]

# apply LdaClassic on reduced data
class.train.pca <- LdaClassic(scores.train, redwine[train,12])
pred.class.pca <- predict(class.train.pca, scores.test)
tab.class.pca <- table(redwine[test,12], pred.class.pca@classification)
1-sum(diag(tab.class.pca))/sum(tab.class.pca) # misclassification rate
# The misclassification rate is still slightly higher than for lda(), but it got way
# better than before.