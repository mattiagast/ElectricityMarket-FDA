# PROCESSING AND COLLECTING THE DATA ----------------------------------------------------------

library(xml2)
library(XML)

setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/Dataset")
#creates the list of all the xml files in the directory
listxml <- dir(pattern = "*.xml")

data_off <- NULL
data_bid <- NULL
namedata <- NULL

for (i in 1097:1461){   # number of elements: CHOOSE YOURS
  #set working directory with the .xml files 
  setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/Dataset")
  #trasform daily data from xml
  df <- xmlToDataFrame(listxml[i])[-1,-1]
  
  #Choose hour of day
  df_1 <- df[df$Ora == 10,]
  df_1 <- df_1[df_1$ZonaMercato == "CALA;CNOR;CSUD;NORD;SARD;SICI;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;MALT;COUP;MONT;",]
  
  #Change data types
  df_1$Quantita <- as.numeric(df_1$Quantita)
  df_1$Prezzo <- as.numeric(df_1$Prezzo)
  
  ind <- substring(listxml[i], 5, 8)
  
  if(dim(df_1)[1] != 0){
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
    
    threshold <- c(20000,50000)
    
    n.sample <- 600
    x.off <- seq(threshold[1], threshold[2], length.out = n.sample)
    y.off <- step_off(x.off)
    x.bid <- seq(threshold[1], threshold[2], length.out = n.sample)
    y.bid <- step_bid(x.bid)
    
    data_off <- cbind(data_off, y.off)
    data_bid <- cbind(data_bid, y.bid)
    
    namedata <- c(namedata, ind)
  }
  
}

dimnames(data_off) <- list(NULL, namedata)
dimnames(data_bid) <- list(NULL, namedata)

# setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket")
# write.table(data_off, file = "data_off.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
# write.table(data_bid, file = "data_bid.txt", sep = "\t", row.names = FALSE, col.names = TRUE)

# START HERE -------------------------------------------------------------------

setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/FunctionalDataObject")
data_off <- read.table("data_off.txt", header = T)
data_bid <- read.table("data_bid.txt", header = T)
namedata <- colnames(data_off)

threshold <- c(20000,50000)
n.sample <- 600
x.off <- seq(threshold[1], threshold[2], length.out = n.sample)
x.bid <- seq(threshold[1], threshold[2], length.out = n.sample)

data_off <- as.matrix(data_off)
data_bid <- as.matrix(data_bid)

# SMOOTHING & PROPERTIES ---------------------------------------------------------------------

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
palette1 <- colorRampPalette(c("black", "red"))(dim(data_off)[2])
palette2 <- colorRampPalette(c("darkblue", "blue"))(dim(data_off)[2])
plot(data_off.fd, col = 'blue', lwd = 1)
plot.fd(data_bid.fd, col = palette, lwd = 1, xlab = 'Quantity [MWh]', ylab = 'Price [Euro]' )

# estimate of the mean and of the covariance kernel
library(fields)

x11(width=10)
par(mfrow=c(2,2))

#mean
plot.fd(data_off.fd, col = palette, lwd = 1)
lines(mean.fd(data_off.fd), lwd = 1.5)

plot.fd(data_bid.fd, col = palette, lwd = 1)
lines(mean.fd(data_bid.fd), lwd = 1.5)

# covariance
eval.off <- eval.fd(x.off, data_off.fd)
image.plot(x.off, x.off, (cov(t(eval.off))[1:length(x.off), ])) 

eval.bid <- eval.fd(x.bid, data_bid.fd)
image.plot(x.bid, x.bid, (cov(t(eval.bid))[1:length(x.bid), ]))

graphics.off()

# FUNCTIONAL PCA ---------------------------------------------------------------

data_off.fd$basis$params <- data_off.fd$basis$params[-length(data_off.fd$basis$params)]
data_bid.fd$basis$params <- data_off.fd$basis$params[-length(data_off.fd$basis$params)]

## FPCA OFF ---------------------------------------------------------------------

plot.fd(data_off.fd, col = palette, lwd = 1)
pca_off <- pca.fd(data_off.fd, nharm = 119, centerfns=TRUE)

# scree plot
plot(pca_off$values, xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_off$values)/sum(pca_off$values), xlab='j', ylab='CPV',ylim=c(0.8,1))

# first two FPCs
# x11()
# layout(cbind(1,2))
# plot(pca_off$harmonics[1,], col=1, ylab='FPC1', ylim=c(-0.1,0.08))
# abline(h=0,lty=2)
# plot(pca_off$harmonics[2,], col=2, ylab='FPC2', ylim=c(-0.1,0.08))

# plot of the FPCs as perturbation of the mean
media_off <- mean.fd(data_off.fd)

plot(media_off,ylim=c(0,300),ylab='', xlab = '',main='PC1 Supply')
lines(media_off+pca_off$harmonics[1,]*sqrt(pca_off$values[1]), col='salmon', lwd = 2)
lines(media_off-pca_off$harmonics[1,]*sqrt(pca_off$values[1]), col='lightblue', lwd = 2)

plot(media_off,ylim=c(0,300),ylab='', xlab='',main='PC2 Supply')
lines(media_off+pca_off$harmonics[2,]*sqrt(pca_off$values[2]), col='salmon', lwd = 2)
lines(media_off-pca_off$harmonics[2,]*sqrt(pca_off$values[2]), col='lightblue', lwd = 2)

graphics.off()

# scatter plot of the scores
par(mfrow=c(1,2))
plot(pca_off$scores[,1],pca_off$scores[,2],xlab="Scores FPC1",ylab="Scores FPC2",lwd=2)

plot(pca_off$scores[,1],pca_off$scores[,2],type="n",xlab="Scores FPC1",
     ylab="Scores FPC2")
text(pca_off$scores[,1],pca_off$scores[,2],dimnames(data_off)[[2]], cex=1)
par(mfrow=c(1,1))

## FPCA BID -----------------------------------------------------------

plot.fd(data_bid.fd, col = palette, lwd = 1)
pca_bid <- pca.fd(data_bid.fd, nharm = 119,centerfns=TRUE)

# scree plot
plot(pca_bid$values, xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_bid$values)/sum(pca_bid$values), xlab='j', ylab='CPV',ylim=c(0.8,1))

# first two FPCs
# x11()
# layout(cbind(1,2))
# plot(pca_bid$harmonics[1,], col=1, ylab='FPC1', ylim=c(-0.1,0.08))
# abline(h=0,lty=2)
# plot(pca_bid$harmonics[2,], col=2, ylab='FPC2', ylim=c(-0.1,0.08))

# plot of the FPCs as perturbation of the mean
media_bid <- mean.fd(data_bid.fd)

plot(media_bid,lwd=2,ylim=c(-500,4000),ylab='price',main='PC1')
lines(media_bid+pca_bid$harmonics[1,]*sqrt(pca_bid$values[1]), col=2)
lines(media_bid-pca_bid$harmonics[1,]*sqrt(pca_bid$values[1]), col=3)

plot(media_bid,lwd=2,ylim=c(-500,4000),ylab='price',main='PC2')
lines(media_bid+pca_bid$harmonics[2,]*sqrt(pca_bid$values[2]), col=2)
lines(media_bid-pca_bid$harmonics[2,]*sqrt(pca_bid$values[2]), col=3)

graphics.off()

# scatter plot of the scores
par(mfrow=c(1,2))
plot(pca_bid$scores[,1],pca_bid$scores[,2],xlab="Scores FPC1",ylab="Scores FPC2",lwd=2)

plot(pca_bid$scores[,1],pca_bid$scores[,2],type="n",xlab="Scores FPC1",
     ylab="Scores FPC2")
text(pca_bid$scores[,1],pca_bid$scores[,2],dimnames(data_bid)[[2]], cex=1)
par(mfrow=c(1,1))


## Outliers OFF ----------------------------------------------------------------

rem <- colnames(data_off)[which(data_off[598, ]>750)]
data_off <- data_off[, setdiff(colnames(data_off), rem)]

rem <- colnames(data_off)[which(data_off[600, ]>750)]
data_off <- data_off[, setdiff(colnames(data_off), rem)]


noff <- n.sample
offRng <- range(x.off)

# spline order
m <- 1

# number of bases
nbasis_off <- 120

# create the basis with B-Splin
basis_off <- create.bspline.basis(offRng, nbasis = nbasis_off, norder = m)

data_off_obj <- smooth.basis(y = data_off, argvals = x.off, fdParobj = basis_off)

data_off.fd <- data_off_obj$fd

palette <- colorRampPalette(c("yellow", "red"))(dim(data_off)[2])
plot.fd(data_off.fd, col = palette, lwd = 1, ylab = 'Price [Euro]', xlab = 'Quantity [MWh]')



x11(width=10)
par(mfrow=c(1,2))

#mean
plot.fd(data_off.fd, col = palette, lwd = 1)
lines(mean.fd(data_off.fd), lwd = 1.5)

# covariance
eval.off <- eval.fd(x.off, data_off.fd)
image.plot(x.off, x.off, (cov(t(eval.off))[1:length(x.off), ])) 

graphics.off()



plot.fd(data_off.fd, col = palette, lwd = 1)
data_off.fd$basis$params <- data_off.fd$basis$params[-length(data_off.fd$basis$params)]


plot.fd(data_off.fd, col = palette, lwd = 1)
pca_off <- pca.fd(data_off.fd, nharm = 119, centerfns=TRUE)

# scree plot
plot(pca_off$values, xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_off$values)[1:20]/sum(pca_off$values), xlab='j', ylab='CPV',ylim=c(0.7,1))

# first two FPCs
# x11()
# layout(cbind(1,2))
# plot(pca_off$harmonics[1,], col=1, ylab='FPC1', ylim=c(-0.1,0.08))
# abline(h=0,lty=2)
# plot(pca_off$harmonics[2,], col=2, ylab='FPC2', ylim=c(-0.1,0.08))

# plot of the FPCs as perturbation of the mean
media_off <- mean.fd(data_off.fd)

plot(media_off,lwd=2,ylim=c(0,300),ylab='price',main='PC1')
lines(media_off+pca_off$harmonics[1,]*sqrt(pca_off$values[1]), col=2)
lines(media_off-pca_off$harmonics[1,]*sqrt(pca_off$values[1]), col=3)

plot(media_off,lwd=2,ylim=c(0,300),ylab='price',main='PC2')
lines(media_off+pca_off$harmonics[2,]*sqrt(pca_off$values[2]), col=2)
lines(media_off-pca_off$harmonics[2,]*sqrt(pca_off$values[2]), col=3)

graphics.off()

# scatter plot of the scores
par(mfrow=c(1,2))
plot(pca_off$scores[,1],pca_off$scores[,2],xlab="Scores FPC1",ylab="Scores FPC2",lwd=2)

plot(pca_off$scores[,1],pca_off$scores[,2],type="n",xlab="Scores FPC1",
     ylab="Scores FPC2")
text(pca_off$scores[,1],pca_off$scores[,2],dimnames(data_off)[[2]], cex=1)
par(mfrow=c(1,1))

## Cluster BID -----------------------------------------------------------------

plot(pca_bid$scores[,1],pca_bid$scores[,2],xlab="Scores FPC1",ylab="Scores FPC2",lwd=2)


dafr <- data.frame(PC1 = pca_bid$scores[,1], PC2 = pca_bid$scores[,2])

dafr.e <- dist(dafr, method='euclidean')
dafr.es <- hclust(dafr.e, method='single')
plot(dafr.es)

cluster <- cutree(dafr.es, k=2)
cluster <- factor(cluster, levels = c("1","2"))

plot(dafr$PC1, dafr$PC2, pch = 16)
points(dafr$PC1[which(cluster == "1")], dafr$PC2[which(cluster == "1")], pch = 16, col = 'blue')
points(dafr$PC1[which(cluster == "2")], dafr$PC2[which(cluster == "2")], pch = 16, col = 'red')

i1 <- which(cluster == "1")
i2 <- which(cluster == "2")

col1 <- namedata[i1]
col2 <- namedata[i2]


data_cl1 <- data_bid[, setdiff(colnames(data_bid), col2)]
data_cl2 <- data_bid[, setdiff(colnames(data_bid), col1)]

nbid <- n.sample
bidRng <- range(x.bid)

# spline order
m <- 1

# number of bases
nbasis_bid <- 120

# create the basis with B-Splin
basis_bid <- create.bspline.basis(bidRng, nbasis = nbasis_bid, norder = m)

data_bid1_obj <- smooth.basis(y = data_cl1, argvals = x.bid, fdParobj = basis_bid)
data_bid2_obj <- smooth.basis(y = data_cl2, argvals = x.bid, fdParobj = basis_bid)

fd1 <- data_bid1_obj$fd
fd2 <- data_bid2_obj$fd

par(mfrow=c(1,1))
# Traccia il primo functional data object
plot(fd1, xlim=c(20000, 50000), ylim=c(-500, 4000), col="lightblue", xlab = '', ylab = '' )

# Aggiungi il secondo functional data object al plot
lines(fd2, col="cyan")

col1

par(mfrow=c(2,3))
layout_matrix <- matrix(c(1, 2, 2, 
                          3, 4, 4), 
                        nrow = 2, byrow = TRUE)

# Imposta il layout
layout(layout_matrix)

plot(cumsum(pca_off$values)[1:10]/sum(pca_off$values), xlab='', ylab='CPV',ylim=c(0.7,1))
plot(pca_off$scores[,1],pca_off$scores[,2],xlab="Scores FPC1",ylab="Scores FPC2", col = 'salmon')
plot(cumsum(pca_bid$values)[1:10]/sum(pca_bid$values), xlab='', ylab='CPV',ylim=c(0.7,1))
plot(dafr$PC1, dafr$PC2, xlab="Scores FPC1",ylab="Scores FPC2")
points(dafr$PC1[which(cluster == "1")], dafr$PC2[which(cluster == "1")], col = 'lightblue')
points(dafr$PC1[which(cluster == "2")], dafr$PC2[which(cluster == "2")], col = 'cyan')



par(mfrow = c(2,2))
plot(media_off,ylim=c(0,300),ylab='', xlab = '',main='PC1 Supply')
lines(media_off+pca_off$harmonics[1,]*sqrt(pca_off$values[1]), col='salmon', lwd = 2)
lines(media_off-pca_off$harmonics[1,]*sqrt(pca_off$values[1]), col='lightblue', lwd = 2)

plot(media_off,ylim=c(0,300),ylab='', xlab='',main='PC2 Supply')
lines(media_off+pca_off$harmonics[2,]*sqrt(pca_off$values[2]), col='salmon', lwd = 2)
lines(media_off-pca_off$harmonics[2,]*sqrt(pca_off$values[2]), col='lightblue', lwd = 2)



plot(media_bid,ylim=c(-500,4000),ylab='price',main='PC1 Demand')
lines(media_bid+pca_bid$harmonics[1,]*sqrt(pca_bid$values[1]), col='salmon', lwd = 2)
lines(media_bid-pca_bid$harmonics[1,]*sqrt(pca_bid$values[1]), col='lightblue', lwd = 2)

plot(media_bid,ylim=c(-500,4000),ylab='price',main='PC2 Demand')
lines(media_bid+pca_bid$harmonics[2,]*sqrt(pca_bid$values[2]), col='salmon', lwd = 2)
lines(media_bid-pca_bid$harmonics[2,]*sqrt(pca_bid$values[2]), col='lightblue', lwd = 2)
