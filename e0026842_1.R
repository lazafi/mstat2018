#ue1
data(Auto,package="ISLR")

#data inspection
str(Auto)
names(Auto)
#change origin to factor
Auto$origin <- as.factor(Auto$origin)

# scatterplot
col1 = "green"
col2 = "red"
par(mfrow = c(1,1))
plot(Auto, pch=as.numeric(levels(Auto$origin)), col=Auto$origin)
#legend(1, 1, c("Male", "Female", "3"), as.numeric(levels(Auto$origin)), col=Auto$origin)
#concusions:
# * origin is correlated with most of the other variables

#extract the continous numeric attributes, we ommit cylinders,year,origin and name
Auto_num <- Auto[,c(1,3,4,5,6)] 
names(Auto_num)
library(StatDA)
par(mfrow = c(2,3))
for ( i in names(Auto_num)) {
  edaplot(Auto[,i], P.main=i, P.xlab=NULL)
}
# conclusions:
# mpg: data norm-distributed
# displacement: data not continous
# horsepower: left-leaning norm-distribution with a lot of outliers beyond 200
# weight: left-leaning norm-distribution
# norm dist with few outliers on both ends

cor_pearson <- cor(Auto_num)
cor_spear <- cor(Auto_num, method="spearman")
cor_kendall <- cor(Auto_num, method="kendall")
?CorCompare
par(mfrow = c(2,2))
CorCompare(cor_pearson, cor_spear, "pearson", "spearman", "pearson", "spearman", ndigits = 2)
# Spearman correlation between two variables is equal to the Pearson correlation of the rank values

CorCompare(cor_pearson, cor_kendall, "pearson", "kendall", "pearson", "kendall", ndigits = 2)
CorCompare(cor_spear, cor_kendall, "spearman", "kendall", "spearman", "kendall", ndigits = 2)

Auto_splits <- split(Auto,Auto$origin)
names(Auto_splits)
cor_matrix <- vector("list")
for ( i in names(Auto_splits)) {
  Auto_num_origin <-  Auto_splits[[i]][,c(1,3,4,5,6)]
  cor_matrix[[i]] <- cor(Auto_num_origin)
}

par(mfrow = c(2,2))
#compare correlation matrix of samples with origin 1 with origin 2
CorCompare(cor_matrix[[1]], cor_matrix[[2]], "1", "2", "1", "2", ndigits = 2)
#compare correlation matrix of samples with origin 1 with origin 3
CorCompare(cor_matrix[[1]], cor_matrix[[3]], "1", "3", "1", "3", ndigits = 2)
#compare correlation matrix of samples with origin 2 with origin 3
CorCompare(cor_matrix[[2]], cor_matrix[[3]], "2", "3", "2", "3", ndigits = 2)

#conclusion: 
# * dispacement/mpg is stronger negative correlated in origin=1 as the others
# * displacement/acceleration and weight/acceleration are positively correlated in origin=2

#eigen decomposition of the covariance matrix with original data
eigen(cov(Auto_num))

#eigen decomposition of the covariance matrix with scaled data
eigen(cov(scale(Auto_num)))
#eigen decomposition of the peason correlation matrix
eigen(cor(Auto_num))
#since pearson correlation coefficients are scaled, we get the same results then before.
