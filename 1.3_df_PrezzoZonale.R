# TAKE THE DATA FRAMES

# 2022 ------------------------------------------------------------------------

# in this paragraph we take the 3 data frames from the .txt files

## df_prezzoZonale ------------------------------------------------------------

# set the wd with the 2022 PrezzoZonale files
setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/2022_PrezzoZonale")

# variable useful in order to save the data frame correctly
it <- 0

# list with all the file.txt in the working directory
listtxt <- dir(pattern = "*.txt")   

for(k in 1:12){
  # pick the data from the .txt
  data <- read.table(listtxt[k], header = FALSE)  # pick the data from the .txt
  
  # create the column names vector
  colnames(data) <- paste0("h", 1:25)  
  
  # pick the 25th column (with the information about the YYYY/MM/DD)
  data_column <- data[, 25]
  
  # Remove this column from the dataframe
  data <- data[, -25]
  
  # Insert the column with the date as first
  data <- cbind(data_column, data)
  
  # Give the name 'date' at the first column
  colnames(data)[1] <- "date"
  
  # compose the dataframe
  if(it == 0){
    df_prezzoZonale22 <- data
  } else {
    df_prezzoZonale22 <- rbind(df_prezzoZonale22, data)
  }
  
  it <- it + 1
}

## df_offQuantity -----------------------------------------------------------

# set the wd with the 2022 Offer Quantity files
setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/2022_OfferQuantity")

# variable useful in order to save the data frame correctly
it <- 0

# list with all the file.txt in the working directory
listtxt <- dir(pattern = "*.txt")   

for(k in 1:12){
  # pick the data from the .txt
  data <- read.table(listtxt[k], header = FALSE)  # pick the data from the .txt
  
  # create the column names vector
  colnames(data) <- paste0("h", 1:25)  
  
  # pick the 25th column (with the information about the YYYY/MM/DD)
  data_column <- data[, 25]
  
  # Remove this column from the dataframe
  data <- data[, -25]
  
  # Insert the column with the date as first
  data <- cbind(data_column, data)
  
  # Give the name 'date' at the first column
  colnames(data)[1] <- "date"
  
  # compose the dataframe
  if(it == 0){
    df_offQ22 <- data
  } else {
    df_offQ22 <- rbind(df_offQ22, data)
  }
  
  it <- it + 1
}

## df_bidQuantity -------------------------------------------------------------

# set the wd with the 2022 Offer Quantity files
setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/2022_DemandQuantity")

# variable useful in order to save the data frame correctly
it <- 0

# list with all the file.txt in the working directory
listtxt <- dir(pattern = "*.txt")   

for(k in 1:12){
  # pick the data from the .txt
  data <- read.table(listtxt[k], header = FALSE)  # pick the data from the .txt
  
  # create the column names vector
  colnames(data) <- paste0("h", 1:25)  
  
  # pick the 25th column (with the information about the YYYY/MM/DD)
  data_column <- data[, 25]
  
  # Remove this column from the dataframe
  data <- data[, -25]
  
  # Insert the column with the date as first
  data <- cbind(data_column, data)
  
  # Give the name 'date' at the first column
  colnames(data)[1] <- "date"
  
  # compose the dataframe
  if(it == 0){
    df_bidQ22 <- data
  } else {
    df_bidQ22 <- rbind(df_bidQ22, data)
  }
  
  it <- it + 1
}




# 2023 ------------------------------------------------------------------------

# in this paragraph we take the 3 data frames from the .txt files

## df_prezzoZonale ------------------------------------------------------------

# set the wd with the 2023 prezzo zonale files
setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/2023_PrezzoZonale")

# variable useful in order to save the dataframe correctly
it <- 0

# list with all the file.txt in the wd
listtxt <- dir(pattern = "*.txt")   

for(k in 1:12){
  # pick the data from the .txt
  data <- read.table(listtxt[k], header = FALSE)  # pick the data from the .txt
  
  # create the column names vector
  colnames(data) <- paste0("h", 1:25)  
  
  # pick the 25th column (with the information about the YYYY/MM/DD)
  data_column <- data[, 25]
  
  # Remove this column from the dataframe
  data <- data[, -25]
  
  # Insert the column with the date as first
  data <- cbind(data_column, data)
  
  # Give the name 'date' at the first column
  colnames(data)[1] <- "date"
  
  # compose the dataframe
  if(it == 0){
    df_prezzoZonale23 <- data
  } else {
    df_prezzoZonale23 <- rbind(df_prezzoZonale23, data)
  }
  
  it <- it + 1
}


## df_offQuantity -----------------------------------------------------------

# set the wd with the 2022 Offer Quantity files
setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/2023_OfferQuantity")

# variable useful in order to save the data frame correctly
it <- 0

# list with all the file.txt in the working directory
listtxt <- dir(pattern = "*.txt")   

for(k in 1:12){
  # pick the data from the .txt
  data <- read.table(listtxt[k], header = FALSE)  # pick the data from the .txt
  
  # create the column names vector
  colnames(data) <- paste0("h", 1:25)  
  
  # pick the 25th column (with the information about the YYYY/MM/DD)
  data_column <- data[, 25]
  
  # Remove this column from the dataframe
  data <- data[, -25]
  
  # Insert the column with the date as first
  data <- cbind(data_column, data)
  
  # Give the name 'date' at the first column
  colnames(data)[1] <- "date"
  
  # compose the dataframe
  if(it == 0){
    df_offQ23 <- data
  } else {
    df_offQ23 <- rbind(df_offQ23, data)
  }
  
  it <- it + 1
}

## df_bidQuantity -------------------------------------------------------------

# set the wd with the 2022 Offer Quantity files
setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/2023_DemandQuantity")

# variable useful in order to save the data frame correctly
it <- 0

# list with all the file.txt in the working directory
listtxt <- dir(pattern = "*.txt")   

for(k in 1:12){
  # pick the data from the .txt
  data <- read.table(listtxt[k], header = FALSE)  # pick the data from the .txt
  
  # create the column names vector
  colnames(data) <- paste0("h", 1:25)  
  
  # pick the 25th column (with the information about the YYYY/MM/DD)
  data_column <- data[, 25]
  
  # Remove this column from the dataframe
  data <- data[, -25]
  
  # Insert the column with the date as first
  data <- cbind(data_column, data)
  
  # Give the name 'date' at the first column
  colnames(data)[1] <- "date"
  
  # compose the dataframe
  if(it == 0){
    df_bidQ23 <- data
  } else {
    df_bidQ23 <- rbind(df_bidQ23, data)
  }
  
  it <- it + 1
}

# END OF THE PROCESS OF THE DATA FRAMES --------------------------------------

## savings the dataframes ----------------------------------------------------

setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/Dataframes")
write.csv(df_prezzoZonale22, "22PrezzoZonale.csv", row.names = FALSE)
write.csv(df_bidQ22, "22BID.csv", row.names = FALSE)
write.csv(df_offQ22, "22OFF.csv", row.names = FALSE)
write.csv(df_prezzoZonale23, "23PrezzoZonale.csv", row.names = FALSE)
write.csv(df_bidQ23, "23BID.csv", row.names = FALSE)
write.csv(df_offQ23, "23OFF.csv", row.names = FALSE)

