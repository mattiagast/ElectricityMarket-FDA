setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/FunctionalDataObject")
data_off <- read.table("data_off.txt", header = T)
data_bid <- read.table("data_bid.txt", header = T)
namedata <- colnames(data_off)






# Creating functional data object ----------------------------------------------

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
par(mfrow = c(1,2))
plot.fd(data_off.fd, col = palette, lwd = 1, xlab = "Quantity [MWh]", ylab = "Price [Euro]")
plot.fd(data_bid.fd, col = palette, lwd = 1, xlab = "Quantity [MWh]", ylab = "Price [Euro]", ylim = c(0,520))
par(mfrow = c(1,1))

## Transform the observation in corresponding days --------------------------

dates <- colnames(data_off)
months <- substr(dates, 2, 3) 
days <- substr(dates, 4, 5)
months <- as.integer(months)
days <- as.integer(days)

days.year <- numeric(length(dates))

for (i in seq_along(dates)) {
  month <- months[i]
  day <- days[i]
  
  days.month <- cumsum(c(0, c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30)))
  
  days.year[i] <- days.month[month] + day
}

days.off <- days.year

dates <- colnames(data_bid)
months <- substr(dates, 2, 3) 
days <- substr(dates, 4, 5)
months <- as.integer(months)
days <- as.integer(days)

days.year <- numeric(length(dates))

for (i in seq_along(dates)) {
  month <- months[i]
  day <- days[i]
  
  days.month <- cumsum(c(0, c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30)))
  
  days.year[i] <- days.month[month] + day
}

days.bid <- days.year


# GAS --------------------------------------------------------------------------

setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/Gas")
gas <- read.table("gasPrice.txt", header = F)
gas <- gas$V1

go <- NULL
for(i in 1:n.sample){
  go <- rbind(go, gas[days.off])
}

data_gaso_obj <- smooth.basis(y = go, argvals = x.off, fdParobj = basis_off)
data_gaso.fd <- data_gaso_obj$fd
plot.fd(data_gaso.fd, col = palette, lwd = 1, xlab = "Quantity [MWh]", ylab = "Price [Euro]")

gb <- NULL
for(i in 1:n.sample){
  gb <- rbind(gb, gas[days.bid])
}

data_gasb_obj <- smooth.basis(y = gb, argvals = x.bid, fdParobj = basis_bid)
data_gasb.fd <- data_gasb_obj$fd
plot.fd(data_gasb.fd, col = palette, lwd = 1, xlab = "Quantity [MWh]", ylab = "Price [Euro]")


cor_gas_off <- cor.fd(x.off, data_off.fd, x.off, data_gaso.fd)
cor_gas_bid <- cor.fd(x.bid, data_bid.fd, x.bid, data_gasb.fd)

library(gplots)

# Utilizza heatmap.2 per visualizzare il heatmap con scala di colori
x11()
heatmap.2(t(cor_gas_off[,1:50]),
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
          labRow = NA,
          asp = 1)

x11()
heatmap.2(t(cor_gas_bid[,1:100]),
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
          labRow = NA)
          

# OIL --------------------------------------------------------------------------




# GEOTERM ----------------------------------------------------------------------
setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/RE")
geo <- read.table("geoterm.txt", header = F)
geo <- geo$V1

geo.o <- NULL
for(i in 1:n.sample){
  geo.o <- rbind(geo.o, geo[days.off])
}

data_geoo_obj <- smooth.basis(y = geo.o, argvals = x.off, fdParobj = basis_off)
data_geoo.fd <- data_geoo_obj$fd
plot.fd(data_geoo.fd, col = palette, lwd = 1, xlab = "Quantity [MWh]", ylab = "Price [Euro]")

geo.b <- NULL
for(i in 1:n.sample){
  geo.b <- rbind(geo.b, geo[days.bid])
}

data_geob_obj <- smooth.basis(y = geo.b, argvals = x.bid, fdParobj = basis_bid)
data_geob.fd <- data_geob_obj$fd
plot.fd(data_geob.fd, col = palette, lwd = 1, xlab = "Quantity [MWh]", ylab = "Price [Euro]")


cor_geo_off <- cor.fd(x.off, data_off.fd, x.off, data_geoo.fd)
cor_geo_bid <- cor.fd(x.bid, data_bid.fd, x.bid, data_geob.fd)

library(gplots)

# Utilizza heatmap.2 per visualizzare il heatmap con scala di colori
x11()
heatmap.2(t(cor_geo_off[,1:100]),
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
          labRow = NA)

x11()
heatmap.2(t(cor_geo_bid[,1:100]),
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
          labRow = NA)


# IDRO ----------------------------------------------------------------------
setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/RE")
idr <- read.table("idro.txt", header = F)
idr <- idr$V1

idr.o <- NULL
for(i in 1:n.sample){
  idr.o <- rbind(idr.o, idr[days.off])
}

idr.o <- replace(idr.o, is.na(idr.o), 0)

data_idro_obj <- smooth.basis(y = idr.o, argvals = x.off, fdParobj = basis_off)
data_idro.fd <- data_idro_obj$fd
plot.fd(data_idro.fd, col = palette, lwd = 1, xlab = "Quantity [MWh]", ylab = "Price [Euro]")

idr.b <- NULL
for(i in 1:n.sample){
  idr.b <- rbind(idr.b, idr[days.bid])
}

idr.b <- replace(idr.b, is.na(idr.b), 0)

data_idrb_obj <- smooth.basis(y = idr.b, argvals = x.bid, fdParobj = basis_bid)
data_idrb.fd <- data_idrb_obj$fd
plot.fd(data_idrb.fd, col = palette, lwd = 1, xlab = "Quantity [MWh]", ylab = "Price [Euro]")


cor_idr_off <- cor.fd(x.off, data_off.fd, x.off, data_idro.fd)
cor_idr_bid <- cor.fd(x.bid, data_bid.fd, x.bid, data_idrb.fd)

library(gplots)

# Utilizza heatmap.2 per visualizzare il heatmap con scala di colori
x11()
heatmap.2(t(cor_idr_off[,1:100]),
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
          labRow = NA)

x11()
heatmap.2(t(cor_idr_bid[,1:100]),
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
          labRow = NA)



# SOLAR ----------------------------------------------------------------------
setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/RE")
sol <- read.table("solar.txt", header = F)
sol <- sol$V1

sol.o <- NULL
for(i in 1:n.sample){
  sol.o <- rbind(sol.o, sol[days.off])
}

data_solo_obj <- smooth.basis(y = sol.o, argvals = x.off, fdParobj = basis_off)
data_solo.fd <- data_solo_obj$fd
plot.fd(data_solo.fd, col = palette, lwd = 1, xlab = "Quantity [MWh]", ylab = "Price [Euro]")

sol.b <- NULL
for(i in 1:n.sample){
  sol.b <- rbind(sol.b, sol[days.bid])
}

data_solb_obj <- smooth.basis(y = sol.b, argvals = x.bid, fdParobj = basis_bid)
data_solb.fd <- data_solb_obj$fd
plot.fd(data_solb.fd, col = palette, lwd = 1, xlab = "Quantity [MWh]", ylab = "Price [Euro]")


cor_sol_off <- cor.fd(x.off, data_off.fd, x.off, data_solo.fd)
cor_sol_bid <- cor.fd(x.bid, data_bid.fd, x.bid, data_solb.fd)

library(gplots)

# Utilizza heatmap.2 per visualizzare il heatmap con scala di colori
x11()
heatmap.2(t(cor_sol_off[,1:100]),
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
          labRow = NA)

x11()
par(mfrow=c(2,5))
heatmap.2(t(cor_sol_bid[,1:100]),
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
          labRow = NA)

# WIND ----------------------------------------------------------------------
setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/RE")
win <- read.table("wind.txt", header = F)
win <- win$V1

win.o <- NULL
for(i in 1:n.sample){
  win.o <- rbind(win.o, win[days.off])
}

data_wino_obj <- smooth.basis(y = win.o, argvals = x.off, fdParobj = basis_off)
data_wino.fd <- data_wino_obj$fd
plot.fd(data_wino.fd, col = palette, lwd = 1, xlab = "Quantity [MWh]", ylab = "Price [Euro]")

win.b <- NULL
for(i in 1:n.sample){
  win.b <- rbind(win.b, win[days.bid])
}

data_winb_obj <- smooth.basis(y = win.b, argvals = x.bid, fdParobj = basis_bid)
data_winb.fd <- data_winb_obj$fd
plot.fd(data_winb.fd, col = palette, lwd = 1, xlab = "Quantity [MWh]", ylab = "Price [Euro]")


cor_win_off <- cor.fd(x.off, data_off.fd, x.off, data_wino.fd)
cor_win_bid <- cor.fd(x.bid, data_bid.fd, x.bid, data_winb.fd)

library(gplots)

# Utilizza heatmap.2 per visualizzare il heatmap con scala di colori
x11()
heatmap.2(t(cor_win_off[,1:100]),
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
          labRow = NA)

x11()
heatmap.2(t(cor_win_bid[,1:100]),
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
          labRow = NA)















library(gplots)


x11()
par(mfrow = c(2,5))

heatmap.2(t(cor_gas_off[,1:100]),
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
          labRow = NA)

heatmap.2(t(cor_gas_bid[,1:100]),
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
          labRow = NA)

cor_gas_off <- cor_gas_off[,1:50]
cor_geo_off <- cor_geo_off[,1:50]
cor_idr_off <- cor_idr_off[,1:50]
cor_sol_off <- cor_sol_off[,1:50]
cor_win_off <- cor_win_off[,1:50]

cor_gas_bid <- cor_gas_bid[,1:50]
cor_geo_bid <- cor_geo_bid[,1:50]
cor_idr_bid <- cor_idr_bid[,1:50]
cor_sol_bid <- cor_sol_bid[,1:50]
cor_win_bid <- cor_win_bid[,1:50]


library(ggplot2)
library(reshape2)
library(gridExtra)

# Genera delle matrici di esempio di dimensioni 600x50

# Lista delle matrici
matrices <- list(cor_gas_off, cor_geo_off, cor_idr_off, cor_sol_off, cor_win_off,
                 cor_gas_bid, cor_geo_bid, cor_idr_bid, cor_sol_bid, cor_win_bid)

# Nomi delle matrici
matrix_names <- c("OFF : GAS", "OFF : GEOTHERMAL", "OFF : IDROELECTRIC", "OFF : SOLAR", "OFF : WIND",
                  "BID : GAS", "BID : GEOTHERMAL", "BID : IDROELECTRIC", "BID : SOLAR", "BID : WIND")

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
    scale_fill_gradient2(low = "blue", mid = "white", high = "magenta", midpoint = 0) +
    ggtitle(matrix_name) +
    theme_minimal() +
    theme(axis.text.x=element_blank(), 
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks=element_blank(), 
          legend.position="none") +
    labs(x = "[20000 MWh - 50000 MWh]")
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
  scale_fill_gradient2(low = "blue", mid = "white", high = "magenta", midpoint = 0) +
  theme_minimal() +
  theme(legend.position="right", legend.title=element_blank())

# Estrai la legenda
legend <- g_legend(legend_plot)

# Organizza i plot in una griglia 2x5 e aggiungi la legenda
grid.arrange(grobs = c(plots, list(legend)), ncol = 5, nrow = 3, layout_matrix = rbind(c(1, 2, 3, 4, 5), c(6, 7, 8, 9, 10), c(NA, NA, NA, NA, 11)))
