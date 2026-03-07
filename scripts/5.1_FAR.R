setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/FunctionalDataObject")
data_off <- read.table("data_off.txt", header = T)
data_bid <- read.table("data_bid.txt", header = T)
namedata <- colnames(data_off)

# Creating functional data object ----------------------------------------------

threshold <- c(20000,50000)
n.sample <- 600
x.off <- seq(threshold[1], threshold[2], length.out = n.sample)
x.bid <- seq(threshold[1], threshold[2], length.out = n.sample)

data_off <- as.matrix(data_off)
data_bid <- as.matrix(data_bid)

rem <- colnames(data_off)[which(data_off[598, ]>750)]
data_off <- data_off[, setdiff(colnames(data_off), rem)]
rem <- colnames(data_off)[which(data_off[600, ]>750)]
data_off <- data_off[, setdiff(colnames(data_off), rem)]


library(fda)

noff <- n.sample
offRng <- range(x.off)
nbid <- n.sample
bidRng <- range(x.bid)

# spline order
m <- 1

# number of bases
nbasis_off <- 120
nbasis_bid <- 120

# create the basis with B-Splin
basis_off <- create.bspline.basis(offRng, nbasis = nbasis_off, norder = m)
basis_bid <- create.bspline.basis(bidRng, nbasis = nbasis_bid, norder = m)

data_off_obj <- smooth.basis(y = data_off, argvals = x.off, fdParobj = basis_off)
data_bid_obj <- smooth.basis(y = data_bid, argvals = x.bid, fdParobj = basis_bid)

data_off.fd <- data_off_obj$fd
data_bid.fd <- data_bid_obj$fd

palette <- colorRampPalette(c("yellow", "red"))(dim(data_off)[2])
par(mfrow = c(1,2))
plot.fd(data_off.fd, col = palette, lwd = 1, xlab = "Quantity [MWh]", ylab = "Price [Euro]")
plot.fd(data_bid.fd, col = palette, lwd = 1, xlab = "Quantity [MWh]", ylab = "Price [Euro]", ylim = c(0,520))
par(mfrow = c(1,1))

# FPCA of OFF -----------------------------------------------------------------

data_off.fd$basis$params <- data_off.fd$basis$params[-length(data_off.fd$basis$params)]
pca_off <- pca.fd(data_off.fd, nharm = 119, centerfns=TRUE)

# scree plot
plot(pca_off$values, xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_off$values)/sum(pca_off$values), xlab='j', ylab='CPV',ylim=c(0.8,1))

# plot of the FPCs as perturbation of the mean
media_off <- mean.fd(data_off.fd)

# FAR(1) for  OFF --------------------------------------------------------------

# number of principal component choosen
npc <- 5

# Scores and data
scores.off <- pca_off$scores[, 1:npc]
lambdas <- pca_off$values[1:npc]
harmonics <- pca_off$harmonics
load <- harmonics$coefs[, 1:npc]
  
# preparin matrices for FAR(1)
n <- nrow(scores.off)
p <- ncol(scores.off)
  
  # current data and delayed data
X <- scores.off[1:(n-1), ]
Y <- scores.off[2:n, ]


# Step 3: estimation of Hilbert-Schmidt operator
Psi_hat <- 1/(n-1) * t(X)%*%Y
for(i in 1:p){
  for(j in 1:p){
    Psi_hat[i,j] <- Psi_hat[i,j]/lambdas[j]
  }
}
Psi_hat

Psi_vec <- rep(0,p)
X_hat <- rep(0,nbasis_off)
for(k in 1:p){
  for(l in 1:p){
    Psi_vec[k] <- Psi_hat[k,l]*scores.off[n, l] + Psi_vec[k]
  }
  X_hat <- X_hat + Psi_vec[k]*load[,k] 
}
X_hat

X_hat_fd <- fd(coef = X_hat, basisobj = basis_off)
X_pred_o <- media_off+X_hat_fd
plot(X_pred_o, col = 'red', lwd = 1.5)
  

## FPCA of BID -----------------------------------------------------------

data_bid.fd$basis$params <- data_off.fd$basis$params[-length(data_off.fd$basis$params)]
pca_bid <- pca.fd(data_bid.fd, nharm = 119,centerfns=TRUE)

# scree plot
plot(pca_bid$values, xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_bid$values)/sum(pca_bid$values), xlab='j', ylab='CPV',ylim=c(0.8,1))

# plot of the FPCs as perturbation of the mean
media_bid <- mean.fd(data_bid.fd)

# FAR(1) for  OFF --------------------------------------------------------------

npc <- 5

scores.bid <- pca_bid$scores[, 1:npc]
lambdas <- pca_bid$values[1:npc]
harmonics <- pca_bid$harmonics
load <- harmonics$coefs[, 1:npc]

n <- nrow(scores.bid)
p <- ncol(scores.bid)

X <- scores.bid[1:(n-1), ]
Y <- scores.bid[2:n, ]

Psi_hat <- 1/(n-1) * t(X)%*%Y
for(i in 1:p){
  for(j in 1:p){
    Psi_hat[i,j] <- Psi_hat[i,j]/lambdas[j]
  }
}
Psi_hat

Psi_vec <- rep(0,p)
X_hat <- rep(0,nbasis_bid)
for(k in 1:p){
  for(l in 1:p){
    Psi_vec[k] <- Psi_hat[k,l]*scores.bid[n, l] + Psi_vec[k]
  }
  X_hat <- X_hat + Psi_vec[k]*load[,k] 
}
X_hat

X_hat_fd <- fd(coef = X_hat, basisobj = basis_bid)
X_pred_b <- media_bid+X_hat_fd
plot(X_pred_b, led = 1.5, col = 'blue')


