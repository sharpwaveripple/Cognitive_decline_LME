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

df$psexf15 <- df$pp1sat15 + df$stroop1sat15 + df$stroop2sat15 + df$ldstcorrect15 +
  df$fluencysupermarket15 + df$fluencyjobs15 + df$stroopinterference15 +	df$vsattotalsat15

variables.psexf <- c("wmhratio06", "wmhratio11", "wmhratio15",
                     "hvratio06", "hvratio11", "hvratio15",
                     "psexf15")
df.subset.psexf <- df[variables.psexf]
df.subset.psexf <- df.subset.psexf[complete.cases(df.subset.psexf), ]


modelfile.int.psexf <- paste("temp/int_wmhprop_hvprop_psexf15.lav", sep="/")
model.int.psexf <- readLines(modelfile.int.psexf)

fit.int.psexf <- growth(model.int.psexf, data=df.subset.psexf)
summary(fit.int.psexf)
fitMeasures(fit.int.psexf)

semPaths(fit.int.psexf)

variables15 <- c("wmh15", "hv15", "ps15", "mem15", "exf15")
df.var.incl15 <- df.var.incl[variables15]
corr15.mat <- corr.test(df.var.incl15)
