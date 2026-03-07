# OBTAIN DATA

library(xml2)
library(XML)

#set working directory with the .xml files 
setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/Dataset")

#creates the list of all the xml files in the directory
listxml <- dir(pattern = "*.xml")
it = 0

for (i in 1431:1432){   # number of elements: CHOOSE YOURS
  
  setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/Dataset")
  #trasform daily data from xml
  df <- xmlToDataFrame(listxml[i])[-1,-1] #listcsv[i] 
  
  for(k in 1:24){
    #Choose hour of day & ZonaMercato
    df_1 <- df[df$Ora == k,]
    df_1 <- df_1[df_1$ZonaMercato == "CALA;CNOR;CSUD;NORD;SARD;SICI;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;MALT;COUP;MONT;",]
    
    # save the specific PrezzoZonale
    if(dim(df_1)[1] == 0){
      eq <- NA
    } else {
      eq <- df_1$PrezzoZonale[1]
    }
    
    # Create the daily PrezzoZonale vector
    if(k == 1) {
      eq.day <- eq
    } else {
      eq.day <-c(eq.day, eq)
    }
    
  }
  
  # create the Matrix of daily PrezzoZonale vectors of the period
  if(it == 0){
    eq.period = eq.day
  } else {
    eq.period <-rbind(eq.period, eq.day)
  }
  
  it <- it + 1 
}

df_period <- as.data.frame(eq.period)

# Claim the starting day and the associate days dimensions
date1 <- as.Date("01-12-2023", format = "%d-%m-%Y")
cdate <- date1


for(k in 2:31){
  date1 <- date1 + 1;
  cdate <- c(cdate, date1)
}

# Add to the matrix the column specifying the days of the observations
df_period <- cbind(df_period, cdate)

# Specify where you are going to save the file, and the name
setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket")
file_name <- "test.txt"

# Save the matrix in a .txt file
write.table(df_period, file_name, sep = "\t", row.names = FALSE, col.names = FALSE)

# Chech in the file has been correctly created
if (file.exists(percorso_file)) {
  print("MATRIX HAS BEEN SAVED SUCCESFULLY IN A .txt FILE")
} else {
  print("ERROR")
}