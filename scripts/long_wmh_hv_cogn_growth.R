library(mice)
library(lavaan)
library(semTools)
library(psych)
library(semPlot)
library(ggraph)
library(ggplot2)

df = read.csv("G:/Documenten_Esther/OiO/R/RUNDMC_data_wide.csv", sep=";", dec=",")

df$hvratio06 <- (df$hv06 / df$tbv06)*1000
df$hvratio11 <- (df$hv11 / df$tbv11)*1000
df$hvratio15 <- (df$hv15 / df$tbv15)*1000

df$wmhratio06 <- log((df$wmh06 / df$tbv06)*100000)
df$wmhratio11 <- log((df$wmh11 / df$tbv11)*100000)
df$wmhratio15 <- log((df$wmh15 / df$tbv15)*100000)


# cognind
variables.cognind <- c("wmhratio06", "hvratio06", "cognitiveindex06",
               "wmhratio11", "hvratio11", "cognitiveindex11",
               "wmhratio15", "hvratio15", "cognitiveindex15")
df.var.cognind <- df[variables.cognind]
df.var.cognind.incl <- df.var.cognind[complete.cases(df.var.cognind), ]

modelfile.cognind <- paste("G:/Documenten_Esther/OiO/Cambridge/RUNDMC_LGM/RUNDMC_LGM/temp/long_wmh_hv_cognind.lav", sep="/")
model.cognind <- readLines(modelfile.cognind)

fit.cognind <- growth(model.cognind, data=df.var.cognind.incl)
fitMeasures(fit.cognind)
summary(fit.cognind)
semPaths(fit.cognind, what="path", nCharNodes = 0, residuals=FALSE, intercepts=FALSE)


