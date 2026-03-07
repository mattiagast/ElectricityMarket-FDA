# GRAPHICAL EXPLORATION OF THE DATA

# open the data
setwd("C:/Users/Mattia/Desktop/APPLIED STATISTICS/Project/ElectricityMarket/Dataframes")
df_prezzoZonale22 <- read.csv("22PrezzoZonale.csv")
df_prezzoZonale23 <- read.csv("23PrezzoZonale.csv")
df_offQuantity22 <- read.csv("22OFF.csv")
df_offQuantity23 <- read.csv("23OFF.csv")
df_bidQuantity22 <- read.csv("22BID.csv")
df_bidQuantity23 <- read.csv("23BID.csv")

# RAPPRESENTATION OF THE DATA --------------------------------------------------

# visualization of the NA in the period
# Prezzo Zonale
mtr22 <- as.matrix(df_prezzoZonale22[-1])
mtr23 <- as.matrix(df_prezzoZonale23[-1])

par(mfrow = c(2,2))
layout(matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE))
image(x = 1:365, y = 1:24, z = mtr22, xlab = "days", ylab = "hours", 
      main = "2022 Price")
image(x = 1:365, y = 1:24, z = mtr23, xlab = "days", ylab = "hours", main = "2023 Price")
image(x = 1:730, y = 1:24, z = rbind(mtr22,mtr23), xlab = "days", ylab = "hours", 
      main = "2022-2023 Price")
par(mfrow = c(1,1))

# Offer
mtr22 <- as.matrix(df_offQuantity22[-1])
mtr23 <- as.matrix(df_offQuantity23[-1])

par(mfrow = c(2,2))
layout(matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE))
image(x = 1:365, y = 1:24, z = mtr22, xlab = "days", ylab = "hours", 
      main = "2022 Offer")
image(x = 1:365, y = 1:24, z = mtr23, xlab = "days", ylab = "hours", 
      main = "2023 Offer")
image(x = 1:730, y = 1:24, z = rbind(mtr22,mtr23), xlab = "days", ylab = "hours", 
      main = "2022-2023 Offer")
par(mfrow = c(1,1))

# Demand
mtr22 <- as.matrix(df_bidQuantity22[-1])
mtr23 <- as.matrix(df_bidQuantity23[-1])

par(mfrow = c(2,2))
layout(matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE))
image(x = 1:365, y = 1:24, z = mtr22, xlab = "days", ylab = "hours", 
      main = "2022 Demand")
image(x = 1:365, y = 1:24, z = mtr23, xlab = "days", ylab = "hours", main = "2023 Demand")
image(x = 1:730, y = 1:24, z = rbind(mtr22,mtr23), xlab = "days", ylab = "hours", 
      main = "2022-2023 Demand")
par(mfrow = c(1,1))

#library("rgl")
#plot3d(x = 1:730, y = 1:24, z = rbind(mtr22,mtr23), xlab = "days", ylab = "hours", 
#       main = "2022 Price")


# BOXPLOT PER HOUR
# Select the column for the plot
cols22 <- df_prezzoZonale22[, 2:25]
cols23 <- df_prezzoZonale23[, 2:25]

# Create the boxplots 
par(mfrow = c(1,2))
boxplot(cols22, main = "2022", xlab = "Hours", ylab = "Price [EUR/MWh]", col = 'lightblue')
means <- sapply(df_prezzoZonale22[2:25], mean, na.rm = TRUE)
for (i in 1:24) {
  lines(c(i - 0.5, i + 0.5), c(means[i], means[i]), col = "blue", lwd = 2)
}

boxplot(cols23, main = "2023", xlab = "Hours", ylab = "Price [EUR/MWh]", col = 'lightblue')
means <- sapply(df_prezzoZonale23[2:25], mean, na.rm = TRUE)
for (i in 1:24) {
  lines(c(i - 0.5, i + 0.5), c(means[i], means[i]), col = "blue", lwd = 2)
}

par(mfrow = c(1,1))

# in my opinion is  event that there are difference in the prezzoZonale
# grouping by hour. We have the same trend in both the year

# ANOVA on the days of the week ------------------------------------------------

# Groping by days
daysOfTheWeek <- c("MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN")
days <- c("SAT", "SUN")
for(i in length(days):53){
  days <- c(days, daysOfTheWeek)
}
days <- days[1:365]
df_prezzoZonale22 <- cbind(df_prezzoZonale22, days)
df_prezzoZonale22$days <- factor(df_prezzoZonale22$days, levels = daysOfTheWeek)

days <- "SUN"
for(i in length(days):53){
  days <- c(days, daysOfTheWeek)
}
days <- days[1:365]
df_prezzoZonale23 <- cbind(df_prezzoZonale23, days)
df_prezzoZonale23$days <- factor(df_prezzoZonale23$days, levels = daysOfTheWeek)

# plot
par(mfrow = c(1,2))
boxplot(df_prezzoZonale22$h7 ~ df_prezzoZonale22$days, main = "2022", 
        xlab = "Hours", ylab = "Price [EUR/MWh]", col = 'lightblue')
means <- tapply(df_prezzoZonale22$h7, df_prezzoZonale22$days, mean, na.rm = TRUE)
for (i in 1:7) {
  lines(c(i - 0.5, i + 0.5), c(means[i], means[i]), col = "blue", lwd = 2)
}

boxplot(df_prezzoZonale23$h7 ~ df_prezzoZonale23$days, main = "2023", 
        xlab = "Hours", ylab = "Price [EUR/MWh]", col = 'lightblue')
means <- tapply(df_prezzoZonale23$h7, df_prezzoZonale23$days, mean, na.rm = TRUE)
for (i in 1:7) {
  lines(c(i - 0.5, i + 0.5), c(means[i], means[i]), col = "blue", lwd = 2)
}

par(mfrow = c(1,1))


# verify the assumptions ofANOVA:
# 1) normality (univariate) in each group (7 tests)
Ps <- c(shapiro.test(df_prezzoZonale23$h7[days==daysOfTheWeek[1]])$p,
        shapiro.test(df_prezzoZonale23$h7[days==daysOfTheWeek[2]])$p,
        shapiro.test(df_prezzoZonale23$h7[days==daysOfTheWeek[3]])$p,
        shapiro.test(df_prezzoZonale23$h7[days==daysOfTheWeek[4]])$p,
        shapiro.test(df_prezzoZonale23$h7[days==daysOfTheWeek[5]])$p,
        shapiro.test(df_prezzoZonale23$h7[days==daysOfTheWeek[6]])$p,
        shapiro.test(df_prezzoZonale23$h7[days==daysOfTheWeek[7]])$p) 
Ps

# Not all the groups are normal with an high p-value.
# We cannot use the Bartlett test but instead a Levene test
library(car)
leveneTest(df_prezzoZonale23$h7, df_prezzoZonale23$days)

# We have significance that the variance WITHIN GROUPS are the same
# Diagnostic: Hp's for ONE-WAY ANOVA are fulfilled beaucse 
# ANOVA is robust wrt normality assumption. We can proceed with ANOVA

fit_days <- aov(df_prezzoZonale23$h7 ~ df_prezzoZonale23$days)
summary(fit_days)

# ANOVA is not so high, for this reason at level 0.95 we can accept differeces
# between groups, but not at level 0.99

# let's understand which groups are significantly different
tukey_days <- TukeyHSD(fit_days)
print(tukey_days)

# This test contains TAKES TO ACCOUNT the Bonferroni Correction.
# No difference between the mean of SUN and SAT, and significant difference with
# some of the other days of the week (as we expected). 

# Let's us group them together

df_prezzoZonale23$days <- as.character(df_prezzoZonale23$days)
week.days <- NULL
for(i in 1:365){
  if(df_prezzoZonale23$days[i] == "SAT" || df_prezzoZonale23$days[i] == "SUN"){
    week.days[i] = "END"
  } else {
    week.days[i] = "WEEK"
  }
}

df_prezzoZonale23 <- cbind(df_prezzoZonale23, week.days)
weekend <- c("WEEK", "END")
df_prezzoZonale23$week.days <- factor(df_prezzoZonale23$week.days, levels = weekend)

# plot
boxplot(df_prezzoZonale23$h7 ~ df_prezzoZonale23$week.days, main = "2023", 
        xlab = "Hours", ylab = "Price [EUR/MWh]", col = 'lightblue')
means <- tapply(df_prezzoZonale23$h7, df_prezzoZonale23$week.days, mean, na.rm = TRUE)
for (i in 1:2) {
  lines(c(i - 0.5, i + 0.5), c(means[i], means[i]), col = "blue", lwd = 2)
}

# verify the assumptions:
# i) normality
Ps <- c(shapiro.test(df_prezzoZonale23$h7[week.days==weekend[1]])$p,
        shapiro.test(df_prezzoZonale23$h7[week.days==weekend[2]])$p)
Ps

# ii) same covariance structure
leveneTest(df_prezzoZonale23$h7, df_prezzoZonale23$week.days)

# We have significance that the variance WITHIN GROUPS are the same
# Diagnostic: Hp's for ONE-WAY ANOVA are fulfilled.
# ANOVA is robust wrt normality assumption. We can proceed with ANOVA

fit_week <- aov(df_prezzoZonale23$h7 ~ df_prezzoZonale23$week.days)
summary(fit_week)

# Way more significant difference between week days and a weekend days


# CLUSTERING ------------------------------------------------------------------

# create a sub data frame concerning all the in-week observation, at 7AM
# in terms of total quantity of offer, bid, and prezzoZonale

df_offQuantity23 <- cbind(df_offQuantity23, week.days)
df_bidQuantity23 <- cbind(df_bidQuantity23, week.days)

subdf <- data.frame(cbind(df_offQuantity23$h7[week.days == "WEEK"],
                          df_bidQuantity23$h7[week.days == "WEEK"],
                          df_prezzoZonale23$h7[week.days == "WEEK"]))

subdf <- na.omit(subdf)
names(subdf) <- c("totOff", "totBid", "prezzoZonale")
# subdf <- subdf[-c(1,3,5),]

plot(subdf$totOff, subdf$totBid, xlab = "Total Quantity offered [MWh]", 
     ylab = "Total Quantity demanded [MWh]")

library(ggplot2)

# Creation with ggplot2
ggplot(data = subdf, aes(x = totOff, y = totBid, color = prezzoZonale)) +
  geom_point() +
  labs(x = "Total Quantity offered [MWh]", y = "Total Quantity demanded [MWh]",
       color = "Prezzo Zonale")

## CART ------------------------------------------------------------------------

# For decision tree model
library(rpart)
# For data visualization
library(rpart.plot)


# Built the classification tree
fit.tree = rpart(prezzoZonale ~ ., data=subdf, method = "anova", cp=0.008)
summary(fit.tree)

# Visualizing the unpruned tree
rpart.plot(fit.tree)

# Prune the tree
printcp(fit.tree)

# bestcp <- fit.tree$cptable[which.min(fit.tree$cptable[,"xerror"]),"CP"]
# bestcp
# Prune the tree with the best cp  value (the lowest cross-validation error - xerror)

# Prune tge tree with the "elbow" point in order to not overfit the data
pruned.tree <- prune(fit.tree, cp = 0.021361)

# Visualizing the pruned tree
rpart.plot(pruned.tree)

# visualization of the split
ggplot(data = subdf, aes(x = totOff, y = totBid, color = prezzoZonale)) +
  geom_point() +
  labs(x = "Total Quantity offered [MWh]", y = "Total Quantity demanded [MWh]",
       color = "Prezzo Zonale") +
  ggtitle("Exploration of Prezzo Zonale") +
  # Personalizza lo stile del titolo
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black")) +
  geom_vline(xintercept = 85000) +
  geom_vline(xintercept = 49000) +
  geom_segment(aes(x = 85000, xend = Inf, y = 32000, yend = 32000), col = 'black') +
  geom_segment(aes(x = 117000, xend = 117000, y = -Inf, yend = 32000), col = 'black') +
  geom_segment(aes(x = -Inf, xend = 49000, y = 30000, yend = 30000), col = 'black') +
  geom_segment(aes(x = 85000, xend = 117000, y = 27000, yend = 27000), col = 'black') +
  geom_label(aes(x = 70000, y = 25000, label = "114" ), color = "Black", fill = "white", size = 3) + 
  geom_label(aes(x = 36000, y = 25000, label = "118" ), color = "Black", fill = "white", size = 3) +
  geom_label(aes(x = 36000, y = 35000, label = "142" ), color = "Black", fill = "white", size = 3) +
  geom_label(aes(x = 100000, y = 22000, label = "166" ), color = "Black", fill = "white", size = 3) +
  geom_label(aes(x = 100000, y = 29000, label = "138" ), color = "Black", fill = "white", size = 3) +
  geom_label(aes(x = 100000, y = 35000, label = "167" ), color = "Black", fill = "white", size = 3) +
  geom_label(aes(x = 127000, y = 25000, label = "112" ), color = "Black", fill = "white", size = 3)





# Aggiungi il segmento orizzontale al plot

# Prediction on a test set
set.test <- 
pred.prune = predict(pruned.tree, set.test)

# confusion matrix
table(pred.prune, set.test$prezzoZonale)

# Calcualte the MSE for the pruned tree
mse <- mean((pred.prune - test.test$prezzoZonale)^2)
mse
