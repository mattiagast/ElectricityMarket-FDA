# Possible Extension: FAR(k) ---------------------------------------------------------
setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/FunctionalDataObject")

data_off <- read.table("data_off.txt", header = T)
data_bid <- read.table("data_bid.txt", header = T)

threshold <- c(20000,50000)
n.sample <- 600
x.off <- seq(threshold[1], threshold[2], length.out = n.sample)
x.bid <- seq(threshold[1], threshold[2], length.out = n.sample)

data_off <- as.matrix(data_off)
data_bid <- as.matrix(data_bid)

rem <- colnames(data_off)[which(data_off[598, ]>750)]
data_off <- data_off[, setdiff(colnames(data_off), rem)]
rem <- colnames(data_off)[which(data_off[600, ]>750)]
data_off <- data_off[, setdiff(colnames(data_off), rem)]

no <- dim(data_off)[2]
nb <- dim(data_bid)[2]

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


# order for OFF ----------------------------------------------------------------
## k = 1 -----------------------------------------------------------------------
data_off1_obj <- smooth.basis(y = data_off[,1:(no-1)], argvals = x.off, fdParobj = basis_off)
data_off1.fd <- data_off1_obj$fd

data_off2_obj <- smooth.basis(y = data_off[,2:no], argvals = x.off, fdParobj = basis_off)
data_off2.fd <- data_off2_obj$fd

cor_off1 <- cor.fd(x.off, data_off1.fd, x.off, data_off2.fd)

library(gplots)
x11()
heatmap.2(cor_off1,
          trace = "none",           # Non mostrare le trace lines
          col = heat.colors(12),    # Schema di colori per il heatmap
          scale = "none",           # Non applicare la scala ai dati
          key = TRUE,               # Mostra la scala di colori
          keysize = 1.5,            # Dimensione della scala di colori
          density.info = "none",    # Non mostrare le informazioni sulla densità
          margins = c(12, 12),      # Impostazioni dei margini per una migliore visualizzazione
          Rowv = NA,                # Disabilita il clustering delle righe
          Colv = NA,                # Disabilita il clustering delle colonne
          dendrogram = "none",       # Non mostrare i dendrogrammi
          labCol = round(x.off),
          labRow = round(x.off))

## k = 5 -----------------------------------------------------------------------

data_off1_obj <- smooth.basis(y = data_off[,1:(no-5)], argvals = x.off, fdParobj = basis_off)
data_off1.fd <- data_off1_obj$fd

data_off2_obj <- smooth.basis(y = data_off[,6:no], argvals = x.off, fdParobj = basis_off)
data_off2.fd <- data_off2_obj$fd

cor_off2 <- cor.fd(x.off, data_off1.fd, x.off, data_off2.fd)

library(gplots)
x11()
heatmap.2(cor_off2,
          trace = "none",           # Non mostrare le trace lines
          col = heat.colors(12),    # Schema di colori per il heatmap
          scale = "none",           # Non applicare la scala ai dati
          key = TRUE,               # Mostra la scala di colori
          keysize = 1.5,            # Dimensione della scala di colori
          density.info = "none",    # Non mostrare le informazioni sulla densità
          margins = c(12, 12),      # Impostazioni dei margini per una migliore visualizzazione
          Rowv = NA,                # Disabilita il clustering delle righe
          Colv = NA,                # Disabilita il clustering delle colonne
          dendrogram = "none",       # Non mostrare i dendrogrammi
          labCol = round(x.off),
          labRow = round(x.off))

## k = 10 -----------------------------------------------------------------------

data_off1_obj <- smooth.basis(y = data_off[,1:(no-10)], argvals = x.off, fdParobj = basis_off)
data_off1.fd <- data_off1_obj$fd

data_off2_obj <- smooth.basis(y = data_off[,11:no], argvals = x.off, fdParobj = basis_off)
data_off2.fd <- data_off2_obj$fd

cor_off3 <- cor.fd(x.off, data_off1.fd, x.off, data_off2.fd)

library(gplots)
x11()
heatmap.2(cor_off3,
          trace = "none",           # Non mostrare le trace lines
          col = heat.colors(12),    # Schema di colori per il heatmap
          scale = "none",           # Non applicare la scala ai dati
          key = TRUE,               # Mostra la scala di colori
          keysize = 1.5,            # Dimensione della scala di colori
          density.info = "none",    # Non mostrare le informazioni sulla densità
          margins = c(12, 12),      # Impostazioni dei margini per una migliore visualizzazione
          Rowv = NA,                # Disabilita il clustering delle righe
          Colv = NA,                # Disabilita il clustering delle colonne
          dendrogram = "none",       # Non mostrare i dendrogrammi
          labCol = round(x.off),
          labRow = round(x.off))

# order for BID ----------------------------------------------------------------

## k = 1 -----------------------------------------------------------------------
data_bid1_obj <- smooth.basis(y = data_bid[,1:(no-1)], argvals = x.bid, fdParobj = basis_bid)
data_bid1.fd <- data_bid1_obj$fd

data_bid2_obj <- smooth.basis(y = data_bid[,2:no], argvals = x.bid, fdParobj = basis_bid)
data_bid2.fd <- data_bid2_obj$fd

cor_bid1 <- cor.fd(x.bid, data_bid1.fd, x.bid, data_bid2.fd)

library(gplots)
x11()
heatmap.2(cor_bid1,
          trace = "none",           # Non mostrare le trace lines
          col = heat.colors(12),    # Schema di colori per il heatmap
          scale = "none",           # Non applicare la scala ai dati
          key = TRUE,               # Mostra la scala di colori
          keysize = 1.5,            # Dimensione della scala di colori
          density.info = "none",    # Non mostrare le informazioni sulla densità
          margins = c(12, 12),      # Impostazioni dei margini per una migliore visualizzazione
          Rowv = NA,                # Disabilita il clustering delle righe
          Colv = NA,                # Disabilita il clustering delle colonne
          dendrogram = "none",       # Non mostrare i dendrogrammi
          labCol = round(x.bid),
          labRow = round(x.bid))

## k = 5 -----------------------------------------------------------------------
data_bid1_obj <- smooth.basis(y = data_bid[,1:(no-5)], argvals = x.bid, fdParobj = basis_bid)
data_bid1.fd <- data_bid1_obj$fd

data_bid2_obj <- smooth.basis(y = data_bid[,6:no], argvals = x.bid, fdParobj = basis_bid)
data_bid2.fd <- data_bid2_obj$fd

cor_bid2 <- cor.fd(x.bid, data_bid1.fd, x.bid, data_bid2.fd)

library(gplots)
x11()
heatmap.2(cor_bid2,
          trace = "none",           # Non mostrare le trace lines
          col = heat.colors(12),    # Schema di colori per il heatmap
          scale = "none",           # Non applicare la scala ai dati
          key = TRUE,               # Mostra la scala di colori
          keysize = 1.5,            # Dimensione della scala di colori
          density.info = "none",    # Non mostrare le informazioni sulla densità
          margins = c(12, 12),      # Impostazioni dei margini per una migliore visualizzazione
          Rowv = NA,                # Disabilita il clustering delle righe
          Colv = NA,                # Disabilita il clustering delle colonne
          dendrogram = "none",       # Non mostrare i dendrogrammi
          labCol = round(x.bid),
          labRow = round(x.bid))

## k = 7 -----------------------------------------------------------------------
data_bid1_obj <- smooth.basis(y = data_bid[,1:(no-7)], argvals = x.bid, fdParobj = basis_bid)
data_bid1.fd <- data_bid1_obj$fd

data_bid2_obj <- smooth.basis(y = data_bid[,8:no], argvals = x.bid, fdParobj = basis_bid)
data_bid2.fd <- data_bid2_obj$fd

cor_bid3 <- cor.fd(x.bid, data_bid1.fd, x.bid, data_bid2.fd)

library(gplots)
x11()
heatmap.2(cor_bid3,
          trace = "none",           # Non mostrare le trace lines
          col = heat.colors(12),    # Schema di colori per il heatmap
          scale = "none",           # Non applicare la scala ai dati
          key = TRUE,               # Mostra la scala di colori
          keysize = 1.5,            # Dimensione della scala di colori
          density.info = "none",    # Non mostrare le informazioni sulla densità
          margins = c(12, 12),      # Impostazioni dei margini per una migliore visualizzazione
          Rowv = NA,                # Disabilita il clustering delle righe
          Colv = NA,                # Disabilita il clustering delle colonne
          dendrogram = "none",       # Non mostrare i dendrogrammi
          labCol = round(x.bid),
          labRow = round(x.bid))

cor_off1
cor_off2
cor_off3
cor_bid1
cor_bid2
cor_bid3



library(ggplot2)
library(reshape2)
library(gridExtra)

# Lista delle matrici
matrices <- list(cor_off1, cor_off2, cor_off3, cor_bid1, cor_bid2, cor_bid3)

# Nomi delle matrici
matrix_names <- c("OFF k=1", "OFF k=5", "OFF k=10", "BID k=1", "BID k=5", "BID k=7")

# Funzione per convertire una matrice in un data frame
matrix_to_df <- function(mat, name) {
  df <- melt(mat)
  colnames(df) <- c("Var1", "Var2", "value")
  df$matrix <- name
  return(df)
}

# Converte tutte le matrici in data frame e le combina
dfs <- lapply(1:length(matrices), function(i) matrix_to_df(matrices[[i]], matrix_names[i]))
df_combined <- do.call(rbind, dfs)

# Crea i plot per ogni matrice
plots <- lapply(unique(df_combined$matrix), function(matrix_name) {
  df <- subset(df_combined, matrix == matrix_name)
  ggplot(df, aes(x=Var1, y=Var2, fill=value)) +
    geom_tile() +
    scale_fill_gradient2(low = "yellow", mid = "orange", high = "red", midpoint = 0) +
    ggtitle(matrix_name) +
    theme_minimal() +
    theme(axis.text.x=element_blank(), 
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks=element_blank(), 
          legend.position="none") +
    labs(x = " ")
})

# Funzione per estrarre la legenda da un plot
g_legend <- function(a.gplot){
  tmp <- ggplotGrob(a.gplot)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Crea un plot separato per estrarre la legenda
legend_plot <- ggplot(df_combined, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low = "yellow", mid = "orange", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(legend.position="right", legend.title=element_blank())

# Estrai la legenda
legend <- g_legend(legend_plot)

# Organizza i plot in una griglia 2x3 e aggiungi la legenda
grid.arrange(grobs = c(plots, list(legend)), ncol = 3, nrow = 3, layout_matrix = rbind(c(1, 2, 3), c(4, 5, 6), c(NA, NA, 7)))

