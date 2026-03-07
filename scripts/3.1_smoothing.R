# SMOOTHING --------------------------------------------------------------------


# PROCESSING THE DATA ----------------------------------------------------------

library(xml2)
library(XML)

#set working directory with the .xml files 
setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/Dataset")
#trasform daily data from xml
df <- xmlToDataFrame("20230109MGPDomandaOfferta.xml")[-1,-1]

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

# Creating the step functions in order to visualize them 
step_off <- stepfun(off_quant_cum, c(off_prices, tail(off_prices, 1)))
step_bid <- stepfun(bid_quant_cum, c(bid_prices, tail(bid_prices, 1)))
plot(step_off, do.points = FALSE, col = "red", main = "Step Function", xlab = "Quantità cumulativa", ylab = "Prezzo")
plot(step_bid, do.points = FALSE, col = "blue", main = "Step Function", xlab = "Quantità cumulativa", ylab = "Prezzo")

# select a threshold and create the sub data frame
df_off <- data.frame(cbind(off_quant_cum, off_prices))
df_bid <- data.frame(cbind(bid_quant_cum, bid_prices))

threshold <- c(20000,50000)

df_off <- df_off[df_off$off_quant_cum >= threshold, ]
df_off <- df_off[df_off$off_quant_cum <= threshold, ]
df_bid <- df_bid[df_bid$bid_quant_cum >= threshold, ]
df_bid <- df_bid[df_bid$bid_quant_cum <= threshold, ]

# create the new fuction to sampling observations
step_off <- stepfun(df_off$off_quant_cum, c(df_off$off_prices, tail(df_off$off_prices, 1)))
step_bid <- stepfun(df_bid$bid_quant_cum, c(df_bid$bid_prices, tail(df_bid$bid_prices, 1)))

plot(step_off, do.points = FALSE, col = "red", main = "Step Function", xlab = "Quantità cumulativa", ylab = "Prezzo")
lines(step_bid, do.points = FALSE, col = "blue")

x.off <- seq((df_off$off_quant_cum)[1], tail(df_off$off_quant_cum, 1), length.out = 350)
y.off <- step_off(x.off)

x.bid <- seq((df_bid$bid_quant_cum)[1], tail(df_bid$bid_quant_cum, 1), length.out = 350)
y.bid <- step_bid(x.bid)

# SMOOTHING --------------------------------------------------------------------

library(fda)

noff <- length(off_quant_cum)
offRng <- range(off_quant_cum)
nbid <- length(bid_quant_cum)
bidRng <- range(bid_quant_cum)

# spline order
m <- 4

# number of bases
nbasis_off <- 15
nbasis_bid <- 8

# create the basis with B-Splin
basis_off <- create.bspline.basis(offRng, nbasis = nbasis_off, norder = m)
basis_bid <- create.bspline.basis(bidRng, nbasis = nbasis_bid, norder = m)

# with the sampling we use as x equispaced observations, so we can use
# so we can use equispaced breaks

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


# -----------------------------------------------------------------------------------------------------------------

noff <- 350
offRng <- range(x.off)
nbid <- 350
bidRng <- range(x.bid)

# spline order
m <- 1

# number of bases
nbasis_off <- 75
nbasis_bid <- 75

# create the basis with B-Splin
basis_off <- create.bspline.basis(offRng, nbasis = nbasis_off, norder = m)
basis_bid <- create.bspline.basis(bidRng, nbasis = nbasis_bid, norder = m)

# with the sampling we use as x equispaced observations, so we can use
# so we can use equispaced breaks

# built the curves estimated coefficient with respect to the basis
basismat_off <- eval.basis(evalarg = x.off , basisobj = basis_off)
est_coef_off <- lsfit(basismat_off, y.off, intercept=FALSE)$coef
off_coef <- basismat_off %*% est_coef_off

basismat_bid <- eval.basis(evalarg = x.bid, basisobj = basis_bid)
est_coef_bid <- lsfit(basismat_bid, y.bid, intercept=FALSE)$coef
bid_coef <- basismat_bid %*% est_coef_bid


# complete the functions in the range
off_coef <- c(off_coef[1], off_coef)
off_coef <- c(off_coef, tail(off_coef, 1))
x.off <- c(threshold[1], x.off)
x.off <- c(x.off, threshold[2])

bid_coef <- c(bid_coef[1], bid_coef)
bid_coef <- c(bid_coef, tail(bid_coef, 1))
x.bid <- c(threshold[1], x.bid)
x.bid <- c(x.bid, threshold[2])

# plots
plot(step_off, col = 'salmon', xlab="Quantity", ylab="Prices")
lines(step_bid, col = 'lightblue')
points(x.off, off_coef ,type = "l", lty = "dashed", col="darkred",lwd = 1.5)
points(x.bid, bid_coef ,type = "l", lty = "dashed", col="darkblue",lwd = 1.5)

plot(step_off, col = 'salmon', xlab="Quantity", ylab="Prices", xlim = c(20000,50000), ylim = c(0,3000))
lines(step_bid, col = 'lightblue')
points(x.off, off_coef ,type = "l", lty = "dashed", col="darkred",lwd = 1.5)
points(x.bid, bid_coef ,type = "l", lty = "dashed", col="darkblue",lwd = 1.5)


# generalized cross-validation for OFF
nbasis <- 30:200
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.bspline.basis( offRng, nbasis[i], m)
  gcv[i] <- smooth.basis(x.off, y.off, basis)$gcv
}
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
abline(v = nbasis[which.min(gcv)], col = 2)

# generalized cross-validation for BID
nbasis <- 30:200
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.bspline.basis( bidRng, nbasis[i], m)
  gcv[i] <- smooth.basis(x.bid, y.bid, basis)$gcv
}
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
abline(v = nbasis[which.min(gcv)], col = 2)

# WE CONCLUDE AN OPTIMA NUMBER OF BASIS SHOULD BE
# OFF: 75
# BID: 75


# ALTERNATIVE APPROACH -------------------------------------------------------------

## Process the data ----------------------------------------------------------

library(xml2)
library(XML)

#set working directory with the .xml files 
setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/Dataset")
# transform daily data from xml
df <- xmlToDataFrame("20230109MGPDomandaOfferta.xml")[-1,-1]

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

# Creating the step functions in order to visualize them 
step_off <- stepfun(off_quant_cum, c(off_prices, tail(off_prices, 1)))
step_bid <- stepfun(bid_quant_cum, c(bid_prices, tail(bid_prices, 1)))
plot(step_off, do.points = FALSE, col = "red", main = "Step Function", xlab = "Quantità cumulativa", ylab = "Prezzo",
     ylim = c(-500, 5000))
lines(step_bid, do.points = FALSE, col = "blue")

threshold <- c(20000,50000)

n.sample <- 600
x.off <- seq(threshold[1], threshold[2], length.out = n.sample)
y.off <- step_off(x.off)
x.bid <- seq(threshold[1], threshold[2], length.out = n.sample)
y.bid <- step_bid(x.bid)

## SMOOTHING -----------------------------------------------------------------

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

# with the sampling we use as x equispaced observations, so we can use
# so we can use equispaced breaks

# built the curves estimated coefficient with respect to the basis
basismat_off <- eval.basis(evalarg = x.off , basisobj = basis_off)
est_coef_off <- lsfit(basismat_off, y.off, intercept=FALSE)$coef
off_coef <- basismat_off %*% est_coef_off

basismat_bid <- eval.basis(evalarg = x.bid, basisobj = basis_bid)
est_coef_bid <- lsfit(basismat_bid, y.bid, intercept=FALSE)$coef
bid_coef <- basismat_bid %*% est_coef_bid

# plots
plot(step_off, col = 'salmon', xlab="Quantity", ylab="Prices")
lines(step_bid, col = 'lightblue')
points(x.off, off_coef ,type = "l", lty = "dashed", col="darkred",lwd = 1.5)
points(x.bid, bid_coef ,type = "l", lty = "dashed", col="darkblue",lwd = 1.5)

plot(step_off, col = 'salmon', xlab="Quantity", ylab="Prices", xlim = c(20000,50000), ylim = c(0,500))
lines(step_bid, col = 'lightblue')
points(x.off, off_coef ,type = "l", lty = "dashed", col="darkred",lwd = 1.5)
points(x.bid, bid_coef ,type = "l", lty = "dashed", col="darkblue",lwd = 1.5)


## GCV ---------------------------------------------------------------
# generalized cross-validation for OFF
nbasis <- 30:400
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.bspline.basis( offRng, nbasis[i], m)
  gcv[i] <- smooth.basis(x.off, y.off, basis)$gcv
}
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
abline(v = nbasis[which.min(gcv)], col = 2)

# generalized cross-validation for BID
nbasis <- 30:400
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.bspline.basis( bidRng, nbasis[i], m)
  gcv[i] <- smooth.basis(x.bid, y.bid, basis)$gcv
}
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
abline(v = nbasis[which.min(gcv)], col = 2)

# n basis opt
# OFF: 300
# BID: 300

# but looking the elbow more o less 120

# BASIS CORRECTION --------------------------------------------
# in this section we repeat the things as above but using smooth.basis
# which allow us to have directly the functional data 
library(xml2)
library(XML)

#set working directory with the .xml files 
setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/Dataset")
# transform daily data from xml
df <- xmlToDataFrame("20231009MGPDomandaOfferta.xml")[-1,-1]

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

# Creating the step functions in order to visualize them 
step_off <- stepfun(off_quant_cum, c(off_prices, tail(off_prices, 1)))
step_bid <- stepfun(bid_quant_cum, c(bid_prices, tail(bid_prices, 1)))
plot(step_off, do.points = FALSE, col = "red", main = "Step Function", xlab = "Quantità cumulativa", ylab = "Prezzo",
     ylim = c(-500, 5000))
lines(step_bid, do.points = FALSE, col = "blue")

threshold <- c(20000,50000)

n.sample <- 600
x.off <- seq(threshold[1], threshold[2], length.out = n.sample)
y.off <- step_off(x.off)
x.bid <- seq(threshold[1], threshold[2], length.out = n.sample)
y.bid <- step_bid(x.bid)

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

# with the sampling we use as x equispaced observations, so we can use
# so we can use equispaced breaks

Xps_off <- smooth.basis(argvals = x.off, y = y.off, fdParobj = basis_off)
Xps_bid <- smooth.basis(argvals = x.bid, y = y.bid, fdParobj = basis_bid)

# plots
plot(step_off, col = 'salmon', xlab="Quantity", ylab="Prices")
lines(step_bid, col = 'lightblue')
lines(Xps_off$fd , col="darkred",lwd = 1.5)
lines(Xps_bid$fd , col="darkblue",lwd = 1.5)

plot(step_off, col = 'salmon', xlab="Quantity", ylab="Prices", xlim = c(20000,50000), ylim = c(0,500))
lines(step_bid, col = 'lightblue')
lines(Xps_off$fd , col="darkred",lwd = 1.5)
lines(Xps_bid$fd, col="darkblue",lwd = 1.5)

