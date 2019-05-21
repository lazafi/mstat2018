#vo 20181121

library(bootstrap)
data(scor)
library(rrcov)
res <- PcaHubert(scor)

plot(res)

plot(PcaClassic(scor, k=2))

#faktorenanalyse

