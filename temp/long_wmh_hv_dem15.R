library(mice)
library(lavaan)
library(semTools)
library(psych)
library(semPlot)
library(ggraph)
library(ggplot2)

datafile <- "data/RUNDMC_datasheet_long.csv"
df <- read.csv(datafile, header=T)

df$hvratio06 <- (df$hv06 / df$tbv06)*1000
df$hvratio11 <- (df$hv11 / df$tbv11)*1000
df$hvratio15 <- (df$hv15 / df$tbv15)*1000

df$wmhratio06 <- log((df$wmh06 / df$tbv06)*100000)
df$wmhratio11 <- log((df$wmh11 / df$tbv11)*100000)
df$wmhratio15 <- log((df$wmh15 / df$tbv15)*100000)

variables <- c("wmhratio06", "hvratio06", "mem06", "psexf06",
               "wmhratio11", "hvratio11", "mem11", "psexf11",
               "wmhratio15", "hvratio15", "mem15", "psexf15")
df.var <- df[variables]
df.var.incl <- df.var[complete.cases(df.var), ]


# dem
variables.dem15 <- c("wmhratio06", "hvratio06",
               "wmhratio11", "hvratio11",
               "wmhratio15", "hvratio15", "dementiai15")
df.var.dem15 <- df[variables.dem15]
df.var.dem15.incl <- df.var.dem15[complete.cases(df.var.dem15), ]

modelfile.dem15 <- paste("temp/long_wmh_hv_dem15.lav", sep="/")
model.dem15 <- readLines(modelfile.dem15)

fit.dem15 <- growth(model.dem15, data=df.var.dem15.incl)
fitMeasures(fit.dem15)
summary(fit.dem15)
semPaths(fit.dem15)




