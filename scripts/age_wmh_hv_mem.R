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


variables <- c("wmhratio06", "hvratio06", "age06", "mem06", "psexf06",
               "wmhratio11", "hvratio11", "age11", "mem11", "psexf11",
               "wmhratio15", "hvratio15", "age15", "mem15", "psexf15")
df.var <- df[variables]
df.var.incl <- df.var[complete.cases(df.var), ]

# Model - Controlling for age - Continuous
variables.mem15 <- c("wmhratio15", "hvratio15", "age15", "mem15")
df.var.mem15 <- df[variables.mem15]
df.var.mem15.incl <- df.var.mem15[complete.cases(df.var.mem15), ]

model <- readLines("temp/age_wmh_hv_mem.lav")
fit <- sem(model, data=df.var.mem15.incl)
summary(fit, standardized=T, rsquare=T)
fitMeasures(fit)

semPaths(fit, "stand", sizeMan = 14, label.cex = 2.3, shapeMan = "square", nCharNodes = 0, edge.label.cex = 3,
         residuals=F, edge.color = "black", edge.width = 0.5)



# Model - Controlling for age - Ordinal
age06_60min <- subset(df, age06 < 60)
age06_6070 <- subset(df, age06 >= 60 & age06 <70)
age06_70plus <- subset(df, age06 >= 70)
   
model.strat <- readLines("temp/age_stratified_wmh_hv_mem.lav")

variables.age.mem15 <- c("age06", "wmhratio15", "hvratio15", "mem15")
df.var.age.mem15 <- df[variables.age.mem15]
df.var.age.mem15.incl <- df.var.age.mem15[complete.cases(df.var.age.mem15), ]

fit.all <- sem(model.strat, data=df.var.age.mem15.incl)
summary(fit.all, standardized=T, rsquare=T)
fitMeasures(fit.all)
semPaths(fit.all, "stand", sizeMan = 14, label.cex = 2.3, shapeMan = "square", nCharNodes = 0, edge.label.cex = 3,
         residuals=F, edge.color = "black", edge.width = 0.5)

mem15.age06_60min <- subset(df.var.age.mem15, age06 < 60)
mem15.age06_60min.incl <- mem15.age06_60min[complete.cases(mem15.age06_60min), ]
fit.60m <- sem(model.strat, data=mem15.age06_60min.incl)
summary(fit.60m, standardized=T, rsquare=T)
fitMeasures(fit.60m)
semPaths(fit.60m, "stand", sizeMan = 14, label.cex = 2.3, shapeMan = "square", nCharNodes = 0, edge.label.cex = 3,
         residuals=F, edge.color = "black", edge.width = 0.5)

mem15.age06_6070 <- subset(df.var.age.mem15, age06 >= 60 & age06 <70)
mem15.age06_6070.incl <- mem15.age06_6070[complete.cases(mem15.age06_6070), ]
fit.6070 <- sem(model.strat, data=mem15.age06_6070.incl)
summary(fit.6070, standardized=T, rsquare=T)
fitMeasures(fit.6070)
semPaths(fit.6070, "stand", sizeMan = 14, label.cex = 2.3, shapeMan = "square", nCharNodes = 0, edge.label.cex = 3,
         residuals=F, edge.color = "black", edge.width = 0.5)

mem15.age06_70plus <- subset(df.var.age.mem15, age06 >= 70)
mem15.age06_70plus.incl <- mem15.age06_70plus[complete.cases(mem15.age06_70plus), ]
fit.70p <- sem(model.strat, data=mem15.age06_70plus.incl)
summary(fit.70p, standardized=T, rsquare=T)
fitMeasures(fit.70p)
semPaths(fit.70p, "stand", sizeMan = 14, label.cex = 2.3, shapeMan = "square", nCharNodes = 0, edge.label.cex = 3,
         residuals=F, edge.color = "black", edge.width = 0.5)

anova(fit.60m, fit.6070, fit.70p)

# median split for age
model.strat <- readLines("temp/age_stratified_wmh_hv_mem.lav")

variables.age.mem15 <- c("age06", "wmhratio15", "hvratio15", "mem15")
df.var.age.mem15 <- df[variables.age.mem15]
df.var.age.mem15.incl <- df.var.age.mem15[complete.cases(df.var.age.mem15), ]

fit.all <- sem(model.strat, data=df.var.age.mem15.incl)
summary(fit.all, standardized=T, rsquare=T)
fitMeasures(fit.all)

median.age06 <- median(df.var.age.mem15$age06) #64.9

mem15.age06_65min <- subset(df.var.age.mem15, age06 < 65)
mem15.age06_65min.incl <- mem15.age06_65min[complete.cases(mem15.age06_65min), ]
fit.65m <- sem(model.strat, data=mem15.age06_65min.incl)
summary(fit.65m, standardized=T, rsquare=T)
fitMeasures(fit.65m)
semPaths(fit.65m, "stand", sizeMan = 14, label.cex = 2.3, shapeMan = "square", nCharNodes = 0, edge.label.cex = 3,
         residuals=F, edge.color = "black", edge.width = 0.5)

mem15.age06_65plus <- subset(df.var.age.mem15, age06 >= 65)
mem15.age06_65plus.incl <- mem15.age06_65plus[complete.cases(mem15.age06_65plus), ]
fit.65p <- sem(model.strat, data=mem15.age06_65plus.incl)
summary(fit.65p, standardized=T, rsquare=T)
fitMeasures(fit.65p)
semPaths(fit.65p, "stand", sizeMan = 14, label.cex = 2.3, shapeMan = "square", nCharNodes = 0, edge.label.cex = 3,
         residuals=F, edge.color = "black", edge.width = 0.5)
