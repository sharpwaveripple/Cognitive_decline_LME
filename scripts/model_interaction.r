library(mice)
library(lavaan)
library(semTools)
library(psych)

impute.data <- function(data, nImps, imp.method, nIter, nseed) {

imp_data = mice(data, m = nImps, method = imp.method, seed = nseed, maxit = nIter, printFlag = T)
  imp_data_means = 0
  for(i in 1:nImps) {
    imp_data_means = imp_data_means + complete(imp_data,i)
  }

  imp_data_means = imp_data_means/nImps
  return(imp_data_means)
}

datafile <- "D:/Esther/Documenten/Werk/Neurologie/LGM_Cambridge/RUNDMC_datasheet_long.csv"
df <- read.csv(datafile, sep=";", dec=",")
df.imp <- impute.data(df, 50, "pmm", 50, 1105)
df$ps06 <- df$pp1sat06 + df$stroop1sat06 + df$stroop2sat06 + df$ldstcorrect06
df$ps11 <- df$pp1sat11 + df$stroop1sat11 + df$stroop2sat11 + df$ldstcorrect11
df$ps15 <- df$pp1sat15 + df$stroop1sat15 + df$stroop2sat15 + df$ldstcorrect15

df$mem06 <- df$wvlt123correctmean06 + df$wvltdelayrecall06 +
  df$reyimmrecalltotalscore06 + df$reydelayrecalltotalscore06
df$mem11 <- df$wvlt123correctmean11 + df$wvltdelayrecall11 +
  df$reyimmrecalltotalscore11 + df$reydelayrecalltotalscore11
df$mem15 <- df$wvlt123correctmean15 + df$wvltdelayrecall15 +
  df$reyimmrecalltotalscore15 + df$reydelayrecalltotalscore15

df$exf06 <- df$fluencya06 + df$fluencyb06 + df$stroopinterference06 +   df$vsattotalsat06
df$exf11 <- df$fluencya11 + df$fluencyb11 + df$stroopinterference11 +   df$vsattotalsat11
df$exf15 <- df$fluencya15 + df$fluencyb15 + df$stroopinterference15 +   df$vsattotalsat15

df.incl = df[df$Inclusion==1,]


variables.ps <- c("wmh06", "wmh11", "wmh15",
                  "hv06", "hv11", "hv15",
                  "ps06", "ps11", "ps15")
df.ps <- df[variables.ps]

wd <- getwd()
modelfile.ps.int <- paste(wd, "model_interaction.lav", sep="/")
model.ps.int <- readLines(modelfile.ps.int)

fit.ps.int <- growth(model.ps.int, data=df.ps, missing="ML")
summary(fit.ps.int)
fitMeasures(fit.ps.int)
