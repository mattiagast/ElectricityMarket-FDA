# OBTAIN DATA

library(xml2)
library(XML)

#set working directory with the .xml files 
setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/Dataset")

#creates the list of all the xml files in the directory
listxml <- dir(pattern = "*.xml")
it = 0

for (i in 1431:1461){   # number of elements: CHOOSE YOURS
  
  setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/Dataset")
  #trasform daily data from xml
  df <- xmlToDataFrame(listxml[i])[-1,-1] #listcsv[i] 
  
  for(k in 1:24){
    #Choose hour of day & ZonaMercato
    df_1 <- df[df$Ora == k,]
    df_1 <- df_1[df_1$ZonaMercato == "CALA;CNOR;CSUD;NORD;SARD;SICI;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;MALT;COUP;MONT;",]
    
    #Sorted offer array
    df_1_off <- df_1[df_1$Tipo == "OFF",]
    df_1_off <- df_1_off[order(df_1_off$Prezzo),]
    
    #sorted bid array
    df_1_bid <- df_1[df_1$Tipo == "BID",]
    df_1_bid <- df_1_bid[order(df_1_bid$Prezzo, decreasing=TRUE),]
    
    # save the specific PrezzoZonale
    if(dim(df_1)[1] == 0){
      eqO <- NA
      eqB <- NA
    } else {
      eqO <- tail(cumsum(df_1_off$Quantita), n=1)
      eqB <- tail(cumsum(df_1_bid$Quantita), n=1)
    }
    
    # Create the daily PrezzoZonale vector
    if(k == 1) {
      eqO.day <- eqO
      eqB.day <- eqB
    } else {
      eqO.day <- c(eqO.day, eqO)
      eqB.day <- c(eqB.day, eqB)
    }
    
  }
  
  # create the Matrix of daily PrezzoZonale vectors of the period
  if(it == 0){
    eqO.period = eqO.day
    eqB.period = eqB.day
  } else {
    eqO.period <-rbind(eqO.period, eqO.day)
    eqB.period <-rbind(eqB.period, eqB.day)
  }
  
  it <- it + 1 
}

dfO_period <- as.data.frame(eqO.period)
dfB_period <- as.data.frame(eqB.period)

# Claim the starting day and the associate days dimensions
date1 <- as.Date("01-12-2023", format = "%d-%m-%Y")
cdate <- date1

for(k in 2:31){
  date1 <- date1 + 1;
  cdate <- c(cdate, date1)
}

# Add to the matrix the column specifying the days of the observations
dfO_period <- cbind(dfO_period, cdate)
dfB_period <- cbind(dfB_period, cdate)

# Specify where you are going to save the file, and the name
setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket")
file_name <- "202312.txt"

# Save the matrix in a .txt file
setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/2023_OfferQuantity")
write.table(dfO_period, file_name, sep = "\t", row.names = FALSE, col.names = FALSE)

# Save the matrix in a .txt file
setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/2023_DemandQuantity")
write.table(dfB_period, file_name, sep = "\t", row.names = FALSE, col.names = FALSE)

# Chech in the file has been correctly created
if (file.exists(percorso_file)) {
  print("MATRIX HAS BEEN SAVED SUCCESFULLY IN A .txt FILE")
} else {
  print("ERROR")
}