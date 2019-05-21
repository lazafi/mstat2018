#vo20181219

library(gclus)
data(wine)
table(wine$Class)

set.seed(123)

train <- sample(nrow(wine), round(nrow(wine)*2/3))
test <- (1:nrow(wine))[-train]
library(MASS)
res <- lda(wine[train,-1],wine[train,1])
test.pred <- predict(res,wine[test,-1])

table(wine[test,1],test.pred$class)

# "leave one out" - cv
res <- qda(wine[train,-1],wine[train,1], CV=TRUE)
table(wine[train,1], res$class)
