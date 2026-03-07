#DEMAND - OFFER CURVES

library(xml2)
library(XML)


#Processing the data
# set the working directory with the dataset
setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/Dataset")

#trasform daily data from xml
df <- xmlToDataFrame("20230109MGPDomandaOfferta.xml")[-1,-1]

#Choose hour of day
df_1 <- df[df$Ora == 10,]

df_1 <- df_1[df_1$ZonaMercato == "CALA;CNOR;CSUD;NORD;SARD;SICI;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;MALT;COUP;MONT;",]
    

#Change data types
df_1$Quantita <- as.numeric(df_1$Quantita)
df_1$Prezzo <- as.numeric(df_1$Prezzo)

#Filter high quantities
#df_1 <- df_1[df_1$Quantita < 1000,]

#Build the cumulative observations
#Sorted offer array
df_1_off <- df_1[df_1$Tipo == "OFF",]
df_1_off <- df_1_off[order(df_1_off$Prezzo),]

#sorted bid array
df_1_bid <- df_1[df_1$Tipo == "BID",]
df_1_bid <- df_1_bid[order(df_1_bid$Prezzo, decreasing=TRUE),]

#Plot supply/demand curves
plot(cumsum(df_1_off$Quantita), df_1_off$Prezzo, xlim=c(0,50000), ylim=c(0,300))
points(cumsum(df_1_bid$Quantita), df_1_bid$Prezzo)


#Calculate differences
off_quant_cum <- cumsum(df_1_off$Quantita)
bid_quant_cum <- cumsum(df_1_bid$Quantita)

bid_quant_difference <- bid_quant_cum[-1] - bid_quant_cum[-length(bid_quant_cum)]
bid_price_difference <- df_1_bid$Prezzo[-1] - df_1_bid$Prezzo[-length(df_1_bid$Prezzo)]
bid_differential = bid_price_difference / bid_quant_difference
plot(cumsum(df_1_bid$Quantita)[-1], bid_differential, ylim=c(-4,0))

off_quant_difference <- off_quant_cum[-1] - off_quant_cum[-length(off_quant_cum)]
off_price_difference <- df_1_off$Prezzo[-1] - df_1_off$Prezzo[-length(df_1_off$Prezzo)]
off_differential = off_price_difference / off_quant_difference
plot(cumsum(df_1_off$Quantita)[-1], off_differential)

#Build the cumulative curves
#Plot supply/demand curves
step_off <- stepfun(cumsum(df_1_off$Quantita), c(df_1_off$Prezzo, tail(df_1_off$Prezzo, n=1) + 100))
step_bid <- stepfun(cumsum(df_1_bid$Quantita), c(df_1_bid$Prezzo, tail(df_1_bid$Prezzo, n=1) - 100))

plot(step_off, main = "CURVES", xlim=c(0,50000), 
     ylim=c(0,300), xlab="Quantity [MWh]", ylab = "Price [EUR/MWh]",col='red')
lines(step_bid, col='blue')
grid()
legend("topleft", title = "Legend", legend = c("offer", "bid"), col = c('red', 'blue'), lty = 1, lwd = 2)

