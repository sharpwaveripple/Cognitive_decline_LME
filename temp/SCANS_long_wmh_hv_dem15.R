library(mice)
library(lavaan)
library(semTools)
library(psych)
library(semPlot)
library(ggraph)
library(ggplot2)

datafile <- "data/scans_data_clean.csv"
df <- read.csv(datafile, header=T)

df$hvratio1 <- (df$hv1 / df$TBV1)*1000
df$hvratio2 <- (df$hv2 / df$TBV2)*1000
df$hvratio3 <- (df$hv3 / df$TBV3)*1000
#df$hvratio4 <- (df$hv4 / df$TBV4)*1000

hist(df$WMH_vol1)
hist(df$wmhratio3)

df$wmhratio1 <- log((df$WMH_vol1 / df$TBV1)*1000)
df$wmhratio2 <- log((df$WMH_vol2 / df$TBV2)*1000)
df$wmhratio3 <- log((df$WMH_vol3 / df$TBV3)*1000)
#df$wmhratio4 <- log((df$WMH_vol4 / df$TBV4)*1000)

# dem
variables.dem <- c("wmhratio1", "hvratio1",
               "wmhratio2", "hvratio2",
               "wmhratio3", "hvratio3",
               "dementia3")
df.var.dem <- df[variables.dem]
df.var.dem.incl <- df.var.dem[complete.cases(df.var.dem), ]
df.var.dem.incl$dementia3 <- as.ordered(df.var.dem.incl$dementia3)

modelfile.dem <- paste("temp/SCANS_long_wmh_hv_dem.lav", sep="/")
model.dem <- readLines(modelfile.dem)

fit.dem <- growth(model.dem, data=df.var.dem.incl)
fitMeasures(fit.dem)
summary(fit.dem)
semPaths(fit.dem)




