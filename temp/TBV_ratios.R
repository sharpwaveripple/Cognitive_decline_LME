library(mice)
library(lavaan)
library(semTools)
library(psych)

datafile <- "data/RUNDMC_datasheet_long.csv"
df <- read.csv(datafile, header=T)

# hippocampus
df$hvprop06 <- (df$hv06 / df$tbv06)*100
df$hvprop11 <- (df$hv11 / df$tbv11)*100
df$hvprop15 <- (df$hv15 / df$tbv15)*100

df$hvratio06 <- (df$hv06 / df$tbv06)*1000
df$hvratio11 <- (df$hv11 / df$tbv11)*1000
df$hvratio15 <- (df$hv15 / df$tbv15)*1000

hist(df$hvratio06)
hist(df$hvratio11)
hist(df$hvratio15)

# wmh
df$wmhprop06 <- (df$wmh06 / df$tbv06)*100
df$wmhprop11 <- (df$wmh11 / df$tbv11)*100
df$wmhprop15 <- (df$wmh15 / df$tbv15)*100

df$wmhratio06 <- log((df$wmh06 / df$tbv06)*100000)
df$wmhratio11 <- log((df$wmh11 / df$tbv11)*100000)
df$wmhratio15 <- log((df$wmh15 / df$tbv15)*100000)

hist(df$wmhratio06)
hist(df$wmhratio11)
hist(df$wmhratio15)
