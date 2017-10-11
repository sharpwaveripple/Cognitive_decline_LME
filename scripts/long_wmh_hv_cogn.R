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
variables.psexf <- c("wmhratio06", "hvratio06", "psexf06",
               "wmhratio11", "hvratio11", "psexf11",
               "wmhratio15", "hvratio15", "psexf15")
df.var.psexf <- df[variables.psexf]
df.var.psexf.incl <- df.var.psexf[complete.cases(df.var.psexf), ]

modelfile.psexf <- paste("temp/long_wmh_hv_psexf.lav", sep="/")
model.psexf <- readLines(modelfile.psexf)

fit.psexf <- growth(model.psexf, data=df.var.psexf.incl)
fitMeasures(fit.psexf)
summary(fit.psexf)
semPaths(fit.psexf)



# mem
variables.mem <- c("wmhratio06", "hvratio06", "mem06",
               "wmhratio11", "hvratio11", "mem11",
               "wmhratio15", "hvratio15", "mem15")
df.var.mem <- df[variables.mem]
df.var.mem.incl <- df.var.mem[complete.cases(df.var.mem), ]

modelfile.mem <- paste("temp/long_wmh_hv_mem.lav", sep="/")
model.mem <- readLines(modelfile.mem)

fit.mem <- growth(model.mem, data=df.var.mem.incl)
fitMeasures(fit.mem)
summary(fit.mem)
semPaths(fit.mem)



# ps
variables.ps <- c("wmhratio06", "hvratio06", "ps06",
                     "wmhratio11", "hvratio11", "ps11",
                     "wmhratio15", "hvratio15", "ps15")
df.var.ps <- df[variables.ps]
df.var.ps.incl <- df.var.ps[complete.cases(df.var.ps), ]

modelfile.ps <- paste("temp/long_wmh_hv_ps.lav", sep="/")
model.ps <- readLines(modelfile.ps)

fit.ps <- growth(model.ps, data=df.var.ps.incl)
fitMeasures(fit.ps)
summary(fit.ps)
semPaths(fit.ps)


# psexf
variables.exf <- c("wmhratio06", "hvratio06", "exf06",
                     "wmhratio11", "hvratio11", "exf11",
                     "wmhratio15", "hvratio15", "exf15")
df.var.exf <- df[variables.exf]
df.var.exf.incl <- df.var.exf[complete.cases(df.var.exf), ]

modelfile.exf <- paste("temp/long_wmh_hv_exf.lav", sep="/")
model.exf <- readLines(modelfile.exf)

fit.exf <- growth(model.exf, data=df.var.exf.incl)
fitMeasures(fit.exf)
summary(fit.exf)
semPaths(fit.exf)
