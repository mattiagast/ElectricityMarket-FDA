# GRAPHICAL EXPLORATION OF THE DATASET

## 2022 ---------------------------------------------------------------


setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/2022_PrezzoZonale")


it <- 0
listtxt <- dir(pattern = "*.txt")

for(k in 1:12){
  dati <- read.table(listtxt[k], header = FALSE)
  
  colnames(dati) <- paste0("h", 1:25)
  
  # Estrai la colonna 'data' (25esima colonna)
  data_colonna <- dati[, 25]
  
  # Rimuovi la colonna 'data' dal dataframe
  dati <- dati[, -25]
  
  # Inserisci la colonna 'data' come prima colonna del dataframe
  dati <- cbind(data_colonna, dati)
  
  # Assegna il nome 'data' alla prima colonna
  colnames(dati)[1] <- "data"
  
  
  if(it == 0){
    df_prezzoZonale <- dati
  } else {
    df_prezzoZonale <- rbind(df_prezzoZonale, dati)
  }
  
  it <- it + 1
}

# Seleziona le colonne dalla 2 alla 25
colonne_da_plot = df_prezzoZonale[, 2:25]

# Crea il boxplot delle colonne selezionate
boxplot(colonne_da_plot, main = "2022", xlab = "Hours", ylab = "Price [EUR/MWh]", col = 'lightblue')

## COMPARISON ------------------------------


indici_righe <- 1:365

# Trova le righe complete (senza NA) nel vettore di osservazioni
osservazioni <- df_prezzoZonale[,3]
df_complete <- osservazioni[complete.cases(osservazioni)]

# Trova gli indici di rischi corrispondenti alle righe complete
indici_righe_complete <- indici_righe[complete.cases(osservazioni)]


# Plotta il vettore media_righe
plot(indici_righe_complete, df_complete, type = "l", col = "lightblue", xlab = "Day", ylab = "Medium Price", main = "Price behaviour")



## WEEKLY ----------------

days <- "SAT"
day_of_the_week_22 <- c("SAT", "SUN", "MON", "TUE", "WED", "THU", "FRI")

for(i in 2:365){
  if(i %% 7 == 1){
    days <-c(days, day_of_the_week_22[1])
  }
  if(i %% 7 == 2){
    days <-c(days, day_of_the_week_22[2])
  }
  if(i %% 7 == 3){
    days <-c(days, day_of_the_week_22[3])
  }
  if(i %% 7 == 4){
    days <-c(days, day_of_the_week_22[4])
  }
  if(i %% 7 == 5){
    days <-c(days, day_of_the_week_22[5])
  }
  if(i %% 7 == 6){
    days <-c(days, day_of_the_week_22[6])
  }
  if(i %% 7 == 0){
    days <-c(days, day_of_the_week_22[7])
  }
  
}

df_prezzoZonale_data <- as.data.frame(cbind(df_prezzoZonale[,3], days))
df_prezzoZonale_data$days <- factor(df_prezzoZonale_data$days)

df_prezzoZonale_data <- na.omit(df_prezzoZonale_data)
df_prezzoZonale_data$V1 <- as.numeric(df_prezzoZonale_data$V1)

# Creazione di un boxplot dei dati numerici in base alla variabile categorica
boxplot(V1 ~ days, data = df_prezzoZonale_data, col = 'lightblue', 
        xlab = "Days of the week", ylab = "Price", 
        main = "Weekly BoxPlot 2022")
