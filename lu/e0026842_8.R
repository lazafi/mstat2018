#0026842

library(rrcov)

car.data.raw <- read.csv(file="cardata.csv", header=TRUE, sep=",")
car.data <- car.data.raw[,-c(1:9,15,16,18)]
car.data <- na.omit(car.data)
str(car.data)

#1

#car.pca <- PcaHubert(car.data, scale = TRUE, k = 10)
car.pca <- prcomp(car.data, scale = TRUE)
biplot(car.pca)
car.data.raw$fuel.type
# iterpretation:
# PC1 holds info about size-related variables like width, length, engine-size, horsepower
# PC2 holds info about engine quality (gas vs diesel)
summary(car.pca)
# PC1 + PC2 explain about 70% of the variance
plot(car.pca, type="l")
# 3 PCs would explain about 79% of the variance. with 4 we would gain just 6% more

#2
car.fa <- factanal(scale(car.data),factors=3,scores="regression")
#car.fa <- factanal(scale(car.data),factors=6,scores="regression")
summary(car.fa)
#a
# maximum 
# 6 ?
#-> formula for max k in the notes page74 !

#b
# loadings
# PC1/Factor1 different scale but roughly same
# PC2/Factor2 factor2-loadings all positive except peak.rpm

car.fa$uniquenesses
# -> uniquenesses show how much of the variance is pushed to the error term (stroke)


# uniquenesses describe how much of the variance is remaining in the error term not described by the k factors
#c
biplot(car.fa$scores[,1:2], car.fa$loadings[,1:2])


#d
# Factor1: overal size of the car
# Factor2: how sportive is the car.

#e
print(car.fa)

#3
library(StatDA)
car.pfa <- pfa(scale(car.data), factors=3, scores="regression")
biplot(car.pfa$scores[,1:2],car.pfa$loadings[,1:2])

# results very simular
# maybe different parameter estimation algorithm ?
# -> principal factor analysis

