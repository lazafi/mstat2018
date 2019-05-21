#vo 20181031

#PCA

# goal: dimension reduction

#stars data
library(bootstrap)
data(scor)
S <- cov(scor)
eigen(S)
