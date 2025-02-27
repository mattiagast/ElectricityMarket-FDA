# EXAMPLE CODE

data_off <- read.table("data_off.txt", header = T)
data_bid <- read.table("data_bid.txt", header = T)
namedata <- colnames(data_off)

threshold <- c(20000,50000)
n.sample <- 600
x.off <- seq(threshold[1], threshold[2], length.out = n.sample)
x.bid <- seq(threshold[1], threshold[2], length.out = n.sample)

data_off <- as.matrix(data_off)
data_bid <- as.matrix(data_bid)


bid_kmeans2 <- fdakmeans(x.bid, t(data_bid), n_clusters = 2L, 
                         cluster_on_phase = FALSE)

bid_kmeans2_phase <- fdakmeans(x.bid, t(data_bid), n_clusters = 2L, 
                               cluster_on_phase = TRUE)

bid_kmean3_phase<- fdakmeans(x.bid, t(data_bid), n_clusters = 3L, 
                             cluster_on_phase = TRUE)