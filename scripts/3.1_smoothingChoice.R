# SMOOTHING --------------------------------------------------------------------


# PROCESSING THE DATA ----------------------------------------------------------

library(xml2)
library(XML)

#Processing the data
# set the working directory with the dataset
setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/Dataset")

#trasform daily data from xml
df <- xmlToDataFrame("20230809MGPDomandaOfferta.xml")[-1,-1]

#Choose hour of day
df_1 <- df[df$Ora == 10,]
df_1 <- df_1[df_1$ZonaMercato == "CALA;CNOR;CSUD;NORD;SARD;SICI;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;MALT;COUP;MONT;",]

#Change data types
df_1$Quantita <- as.numeric(df_1$Quantita)
df_1$Prezzo <- as.numeric(df_1$Prezzo)

#Build the cumulative observations
#Sorted offer array
df_1_off <- df_1[df_1$Tipo == "OFF",]
df_1_off <- df_1_off[order(df_1_off$Prezzo),]

#sorted bid array
df_1_bid <- df_1[df_1$Tipo == "BID",]
df_1_bid <- df_1_bid[order(df_1_bid$Prezzo, decreasing=TRUE),]

#Saving x and y
off_quant_cum <- cumsum(df_1_off$Quantita)
bid_quant_cum <- cumsum(df_1_bid$Quantita)

off_prices <- df_1_off$Prezzo
bid_prices <- df_1_bid$Prezzo

#Step Functions
step_off <- stepfun(off_quant_cum, c(off_prices, tail(off_prices, n=1) + 100))
step_bid <- stepfun(bid_quant_cum, c(bid_prices, tail(bid_prices, n=1) - 100))

# Graphical representation
plot(off_quant_cum, off_prices, col = "darkred", xlab = "Quantity", ylab = "Price")
points(bid_quant_cum, bid_prices, col = 'darkblue')
lines(step_off, col = "red")
lines(step_bid, col = "blue")

# FDA: SMOOTHING --------------------------------------------------------------

library(fda)

# saving some useful data 
noff <- length(off_quant_cum)
offRng <- range(off_quant_cum)
nbid <- length(bid_quant_cum)
bidRng <- range(bid_quant_cum)

## Regression B-Spline ---------------------------------------------------------
# In our case, since we are just looking at the zero-derivative, the spline
# order equal to 4 should be enough, or 5 in order to have a regularity on the
# first-derivative

# spline order
m <- 4

# number of bases
nbasis_off <- 15
nbasis_bid <- 8

# create the basis with B-Splin
basis_off <- create.bspline.basis(offRng, nbasis = nbasis_off, norder = m)
basis_bid <- create.bspline.basis(bidRng, nbasis = nbasis_bid, norder = m)
# for better result we should insert breaks

# built the curves estimated coefficient with respect to the basis
basismat_off <- eval.basis(evalarg = off_quant_cum, basisobj = basis_off)
est_coef_off <- lsfit(basismat_off, off_prices, intercept=FALSE)$coef
off_coef <- basismat_off %*% est_coef_off

basismat_bid <- eval.basis(evalarg = bid_quant_cum, basisobj = basis_bid)
est_coef_bid <- lsfit(basismat_bid, bid_prices, intercept=FALSE)$coef
bid_coef <- basismat_bid %*% est_coef_bid

# plots
plot(step_off, col = 'salmon', xlab="Quantity", ylab="Prices")
lines(step_bid, col = 'lightblue')
points(off_quant_cum, off_coef ,type = "l", lty = "dashed", col="darkred",lwd = 1.5)
points(bid_quant_cum, bid_coef ,type = "l", lty = "dashed", col="darkblue",lwd = 1.5)

plot(step_off, col = 'salmon', xlab="Quantity", ylab="Prices", xlim = c(20000,40000), ylim = c(50,300))
lines(step_bid, col = 'lightblue')
points(off_quant_cum, off_coef ,type = "l", lty = "dashed", col="darkred",lwd = 1.5)
points(bid_quant_cum, bid_coef ,type = "l", lty = "dashed", col="darkblue",lwd = 1.5)


# COMMENT: Increasing the number of basis the smoothing is good, in particular 
# in the middle. On the other hand there are to problems:
# - the smoothed function becomes irregular (not monotone)
# - the behavior of the function in the border is totally wrong

## Monotone Constrain (with penalization) --------------------------------------

# We set up an order 5 spline basis as before
norder <- 5
nbasisoff <- 8
nbasisbid <- 8
off_basis <- create.bspline.basis(offRng, nbasis = nbasisoff, norder = norder)
bid_basis <- create.bspline.basis(bidRng, nbasis = nbasisbid, norder = norder)

# We construct the functional parameter with penalization of the second-derivative
Lfdobj <- 1          
lambda <- 10^(-4)  

# numerical method: initialization
cvecf_off <- matrix(0, nbasisoff, 1)  # in this casi matrix (nbasis x 1)
cvecf_bid <- matrix(0, nbasisbid, 1)  # in this casi matrix (nbasis x 1)

# for the numerical techniques
Wfd0_off <- fd(coef = cvecf_off, basisobj = off_basis)
off_fdPar <- fdPar(fdobj = Wfd0_off, Lfdobj = Lfdobj, lambda = lambda)

Wfd0_bid <- fd(coef = cvecf_bid, basisobj = bid_basis)
bid_fdPar <- fdPar(fdobj = Wfd0_bid, Lfdobj = Lfdobj, lambda = lambda)

# monotone smoothing
offMon <- smooth.monotone(argvals = off_quant_cum, y = off_prices, WfdParobj = off_fdPar)
Wfd_off <- offMon$Wfd
betaf_off <- offMon$beta
off_fhatfd <- offMon$yhatfd

bidMon <- smooth.monotone(argvals = bid_quant_cum, y = bid_prices, WfdParobj = bid_fdPar)
Wfd_bid <- bidMon$Wfd
betaf_bid <- bidMon$beta
bid_fhatfd <- bidMon$yhatfd

# plot
plot(step_off, col = 'salmon', xlab="Quantity", ylab="Prices")
lines(step_bid, col = 'lightblue')
lines( off_fhatfd, type = "l", lty = "dashed", col = "darkred", lwd = 1.5)
lines( bid_fhatfd, type = "l", lty = "dashed", col = "darkblue", lwd = 1.5)

plot(step_off, col = 'salmon', xlab="Quantity", ylab="Prices", xlim=c(30000,40000), ylim = c(50,300))
lines(step_bid, col = 'lightblue')
lines( off_fhatfd, type = "l", lty = "dashed", col = "darkred", lwd = 1.5)
lines( bid_fhatfd, type = "l", lty = "dashed", col = "darkblue", lwd = 1.5)

# COMMENT: Increasing the number of basis the smoothing is better, but we cannot
# be convinced by this smoothing, even if it monotone because:
# - in the middle the approximation is not good at all
# - we cannot reach the border properly


## Kernel Smoothing ------------------------------------------------------------

library(KernSmooth)

#order of the polynomial
m <- 1
degree <- m-1

#bandwidth
bw <- 5000
offRng1 <- c(0,7000)
Xsm0 <- locpoly(off_quant_cum, off_prices, degree=degree,
                bandwidth=bw, gridsize=noff, 
                range.x=offRng1)
Xsm1 <- locpoly(bid_quant_cum, bid_prices, degree=degree,
                bandwidth=bw, gridsize=nbid, 
                range.x=bidRng)
Xsm0 <- Xsm0$y
Xsm1 <- Xsm1$y

plot(step_off, col='salmon', xlab="t",ylab="observed data")
lines(step_bid, col='lightblue')
points(off_quant_cum, Xsm0 ,type="l",col="darkred")
points(bid_quant_cum, Xsm1 ,type="l",col="darkblue")



## USEFUL TOOL -----------------------------------------------------------------

# generalized cross-validation
nbasis <- 6:24
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.bspline.basis( c(off_quant_cum[1], tail(off_quant_cum, n=1)), nbasis[i], m)
  gcv[i] <- smooth.basis(off_quant_cum, df_1_off$Prezzo, basis)$gcv
}
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
abline(v = nbasis[which.min(gcv)], col = 2)