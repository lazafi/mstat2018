#0026842

library(bootstrap)

#a

# M^(-1/2) 
invsqrt <- function (m) {
  ei <- eigen(m)
  U <- ei$vectors
  e <- ei$values
  Dsqrtinv <- diag(x=1/sqrt(e))
  res <- U%*%Dsqrtinv%*%solve(U)
  res
}

cancorR <- function (df, i, j, cov = "cov", cor = "cor") {
  
  if (is.matrix(cor)) {
    corM <- cor
  } else {
    corM <- cor(df)
  }

  if (is.matrix(cov)) {
    covM <- cov
  } else {
    covM <- cov(df)
  }

  r11 <- corM[i,i]
  s11 <- covM[i,i]
  r22 <- corM[j,j]
  s22 <- covM[j,j]
  r12 <- corM[i,j]
  r21 <- corM[j,i]
  
  f <- invsqrt(r11) %*% r12 %*% solve(r22) %*% r21 %*% invsqrt(r11)
  g <- invsqrt(r22) %*% r21 %*% solve(r11) %*% r12 %*% invsqrt(r22)
  
  cancor <- sqrt(eigen(f)$values)
  
  Astar <- invsqrt(r11) %*% eigen(f)$vectors
  Bstar <- invsqrt(r22) %*% eigen(g)$vectors

  U <- invsqrt(diag(diag(s11)))
  V <- invsqrt(diag(diag(s22)))

  A <- U %*% Astar
  B <- V %*% Bstar
  
  phi <- as.matrix(df[,i]) %*% A
  eta <- as.matrix(df[,j]) %*% B

  res <- list("cancor" = cancor, "eta" = eta, "phi" = phi, "A" = A, "B" = B, "data" = df, "Astar" = Astar, "Bstar" = Bstar)
  res
}

#b

fitness.data.raw <- read.csv(file="fitness.csv", header=TRUE, sep="")

i <- c("Weight", "Waist",  "Pulse")
j <- c("Chins",  "Situps", "Jumps")
cca <- cancorR(fitness.data.raw, i, j)

covcor <- covMcd(fitness.data.raw, cor = TRUE)
ccaR <- cancorR(fitness.data.raw, i, j, cov=covcor$cov, cor=covcor$cor)

#c

plot(cca$phi[,1], cca$eta[,1], pch = ".", ylim=c(-4,0), xlim=c(-16, -8))
text(cca$phi[,1], cca$eta[,1], labels = rownames(cca$data))
text(ccaR$phi[,1], ccaR$eta[,1], labels = rownames(ccaR$data), col=covcor$raw.weights+2)


#d

library(ccaPP)
cca <- CCAgrid(fitness.data.raw[,i], fitness.data.raw[,j], k = 2, method="M")
#ccaR <- cancorR(fitness.data.raw, i, j, cov=covcor$cov, cor=covcor$cor)
#cca <- cancor(fitness.data.raw[,i], fitness.data.raw[,j])
#phi <- as.matrix(fitness.data.raw[,i]) %*% cca$xcoef
#eta <- as.matrix(fitness.data.raw[,i]) %*% cca$ycoef
phi <- as.matrix(fitness.data.raw[,i]) %*% cca$A
eta <- as.matrix(fitness.data.raw[,j]) %*% cca$B

plot(phi[,1], eta[,1], pch = ".")
text(phi[,1], eta[,1], labels = rownames(fitness.data.raw))
text(phi[,1], eta[,1], labels = rownames(fitness.data.raw), col="blue")

