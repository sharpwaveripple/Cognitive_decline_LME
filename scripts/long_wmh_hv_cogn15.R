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


df$mem06 <- df$wvlt123correctmean06 + df$wvltdelayrecall06 + 
  df$reyimmrecalltotalscore06 +	df$reydelayrecalltotalscore06 +
  df$pp2sat06 + df$pp3sat06
df$mem11 <- df$wvlt123correctmean11 + df$wvltdelayrecall11 + 
  df$reyimmrecalltotalscore11 +	df$reydelayrecalltotalscore11 +
  df$pp2sat11 + df$pp3sat11
df$mem15 <- df$wvlt123correctmean15 + df$wvltdelayrecall15 + 
  df$reyimmrecalltotalscore15 +	df$reydelayrecalltotalscore15 +
  df$pp2sat15 + df$pp3sat15

df$psexf06 <- df$pp1sat06 + df$stroop1sat06 + df$stroop2sat06 + df$ldstcorrect06 +
  df$fluencyanimals06 + df$fluencyjobs06 + df$stroopinterference06 +	df$vsattotalsat06
df$psexf11 <- df$pp1sat11 + df$stroop1sat11 + df$stroop2sat11 + df$ldstcorrect11 +
  df$fluencyanimals11 + df$fluencyjobs11 + df$stroopinterference11 +	df$vsattotalsat11
df$psexf15 <- df$pp1sat15 + df$stroop1sat15 + df$stroop2sat15 + df$ldstcorrect15 +
  df$fluencysupermarket15 + df$fluencyjobs15 + df$stroopinterference15 +	df$vsattotalsat15

df$ps06 <- df$pp1sat06 + df$stroop1sat06 + df$stroop2sat06 + df$ldstcorrect06
df$ps11 <- df$pp1sat11 + df$stroop1sat11 + df$stroop2sat11 + df$ldstcorrect11
df$ps15 <- df$pp1sat15 + df$stroop1sat15 + df$stroop2sat15 + df$ldstcorrect15

df$exf06 <- df$fluencyanimals06 + df$fluencyjobs06 + df$stroopinterference06 +	df$vsattotalsat06
df$exf11 <- df$fluencyanimals11 + df$fluencyjobs11 + df$stroopinterference11 +	df$vsattotalsat11
df$exf15 <- df$fluencysupermarket15 + df$fluencyjobs15 + df$stroopinterference15 +	df$vsattotalsat15                         


variables <- c("wmhratio06", "hvratio06", "mem06", "psexf06",
               "wmhratio11", "hvratio11", "mem11", "psexf11",
               "wmhratio15", "hvratio15", "mem15", "psexf15")
df.var <- df[variables]
df.var.incl <- df.var[complete.cases(df.var), ]

# psexf
variables.psexf15 <- c("wmhratio06", "hvratio06",
               "wmhratio11", "hvratio11",
               "wmhratio15", "hvratio15", "psexf15")
df.var.psexf15 <- df[variables.psexf15]
df.var.psexf15.incl <- df.var.psexf15[complete.cases(df.var.psexf15), ]

modelfile.psexf15 <- paste("temp/long_wmh_hv_psexf15.lav", sep="/")
model.psexf15 <- readLines(modelfile.psexf15)

fit.psexf15 <- growth(model.psexf15, data=df.var.psexf15.incl)
fitMeasures(fit.psexf15)
summary(fit.psexf15)
semPaths(fit.psexf15)



# mem
variables.mem15 <- c("wmhratio06", "hvratio06",
               "wmhratio11", "hvratio11",
               "wmhratio15", "hvratio15", "mem15")
df.var.mem15 <- df[variables.mem15]
df.var.mem15.incl <- df.var.mem15[complete.cases(df.var.mem15), ]

modelfile.mem15 <- paste("temp/long_wmh_hv_mem15.lav", sep="/")
model.mem15 <- readLines(modelfile.mem15)

fit.mem15 <- growth(model.mem15, data=df.var.mem15.incl)
fitMeasures(fit.mem15)
summary(fit.mem15)
semPaths(fit.mem15)



# ps
variables.ps15 <- c("wmhratio06", "hvratio06",
                     "wmhratio11", "hvratio11",
                     "wmhratio15", "hvratio15", "ps15")
df.var.ps15 <- df[variables.ps15]
df.var.ps15.incl <- df.var.ps15[complete.cases(df.var.ps15), ]

modelfile.ps15 <- paste("temp/long_wmh_hv_ps15.lav", sep="/")
model.ps15 <- readLines(modelfile.ps15)

fit.ps15 <- growth(model.ps15, data=df.var.ps15.incl)
fitMeasures(fit.ps15)
summary(fit.ps15)
semPaths(fit.ps15)


# exf
variables.exf15 <- c("wmhratio06", "hvratio06",
                     "wmhratio11", "hvratio11",
                     "wmhratio15", "hvratio15", "exf15")
df.var.exf15 <- df[variables.exf15]
df.var.exf15.incl <- df.var.exf15[complete.cases(df.var.exf15), ]

modelfile.exf15 <- paste("temp/long_wmh_hv_exf15.lav", sep="/")
model.exf15 <- readLines(modelfile.exf15)

fit.exf15 <- growth(model.exf15, data=df.var.exf15.incl)
fitMeasures(fit.exf15)
summary(fit.exf15)
semPaths(fit.exf15)
