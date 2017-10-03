library(mice)
library(lavaan)
library(semTools)
library(psych)

datafile <- "D:/Esther/Documenten/Werk/Neurologie/LGM_Cambridge/RUNDMC_datasheet_long.csv"
df <- read.csv(datafile, sep=";", dec=",")
df$ps06 <- df$pp1sat06 + df$stroop1sat06 + df$stroop2sat06 + df$ldstcorrect06
df$ps11 <- df$pp1sat11 + df$stroop1sat11 + df$stroop2sat11 + df$ldstcorrect11
df$ps15 <- df$pp1sat15 + df$stroop1sat15 + df$stroop2sat15 + df$ldstcorrect15

df.incl = df[df$Inclusion==1,]

# wmh
variables.ps <- c("wmh06", "wmh11", "wmh15",
                  "hv06", "hv11", "hv15",
                  "ps06", "ps11", "ps15")
df.ps.incl <- df.incl[variables.ps] 

modelfile.ps <- paste(wd, "model_ps.lav", sep="/")
model.ps <- readLines(modelfile.ps)

fit.ps <- growth(model.ps, data=df.ps.incl)
summary(fit.ps)
fitMeasures(fit.ps)


variables06 <- c("wmh06", "hv06", "ps06")
df06 <- df.incl[variables06]
corr06.mat <- corr.test(df06)
summary(corr06.mat)