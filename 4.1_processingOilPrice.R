setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/Oil")

listcsv <- dir(pattern = "*.csv")
i = 1
price <- NULL
for(i in 1:365){
  data <- read.csv(listcsv[i], skip = 1, sep = ";", stringsAsFactors = FALSE)
  data$descCarburante <- as.factor(data$descCarburante)
  data$isSelf <- as.factor(data$isSelf)
  
  index <- which(data$descCarburante=="Benzina" & data$isSelf=="1")
  
  data <- data[index, 3]
  p <- mean(data)
  price <- c(price, p)
}

price

write.table(price, file = "oilPrice.txt", sep = "\t", row.names = FALSE, col.names = FALSE)


setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/Gas")

library("readxl")
data1 <- read_excel("Annotermico_2022-2023_09.xlsx", sheet = "MGP-GAS - Negoziazione continua", skip = 1)  
data2 <- read_excel("Annotermico_2023-2024_05.xlsx", sheet = "MGP-GAS - Negoziazione continua", skip = 1)  

data <- rbind(data1[93:365,2], data2[1:92,2])
write.table(data, file = "gasPrice.txt", sep = "\t", row.names = FALSE, col.names = FALSE)




setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/RE")
data <- read.csv("Actual Generation per Production Type_202301010000-202401010000.csv")
names(data)

index <- 11
elem <- 11
for(i in 2:365){
  index <- index + 24
  elem <- c(elem, index)
}
length(elem)


data <- data[elem, ]

geoterm <- data[ ,11]
idro <- data[,12]
solar <- data[,20]
wind <- data[,22] + data[,23]

write.table(geoterm, file = "geoterm.txt", sep = "\t", row.names = FALSE, col.names = FALSE)
write.table(idro, file = "idro.txt", sep = "\t", row.names = FALSE, col.names = FALSE)
write.table(solar, file = "solar.txt", sep = "\t", row.names = FALSE, col.names = FALSE)
write.table(wind, file = "wind.txt", sep = "\t", row.names = FALSE, col.names = FALSE)
