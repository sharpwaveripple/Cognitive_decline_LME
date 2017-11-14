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

# mediation
variables.mem15 <- c("wmhratio15", "hvratio15", "mem15")
df.mem15 <- df[variables.mem15]
df.subset.mem15 <- df.mem15[complete.cases(df.mem15), ]

y <- c("mem15")
x <- c("wmhratio15")
m <- c("hvratio15")

mediate(y, x, m, data=df.subset.mem15, mod = NULL, n.obs = NULL, use = "pairwise", n.iter = 5000, 
        alpha = 0.05, std = TRUE,plot=TRUE)

# mediation & moderation
variables.mod.mem15 <- c("wmhratio15", "hvratio15", "mem15", "age15")
df.mod.mem15 <- df[variables.mod.mem15]
df.subset.mod.mem15 <- df.mod.mem15[complete.cases(df.mod.mem15), ]
y <- c("mem15")
x <- c("wmhratio15")
m <- c("hvratio15")
age <- c("age15")
mediate(y, x, m, data=df.subset.mod.mem15, mod = age, n.obs = NULL, use = "pairwise", n.iter = 5000, 
        alpha = 0.05, std = TRUE,plot=TRUE)



#mediate(y, x, m, data, mod = NULL, n.obs = NULL, use = "pairwise", n.iter = 5000, 
#        alpha = 0.05, std = FALSE,plot=TRUE)
#mediate.diagram(medi,digits=2,ylim=c(3,7),xlim=c(-1,10),show.c=TRUE,
#                main="Mediation model",...)
#moderate.diagram(medi,digits=2,ylim=c(2,8),main="Moderation model",...)

