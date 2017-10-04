library(mice)
library(lavaan)
library(semTools)

datafile <- "D:/Esther/Documenten/Werk/Neurologie/LGM_Cambridge/RUNDMC_datasheet_long.csv"
df <- read.csv(datafile, sep=";", dec=",")
df.incl = df[df$Inclusion==1,]

# Psychomotor speed
df$ps06 <- df$pp1sat06 + df$stroop1sat06 + df$stroop2sat06 + df$ldstcorrect06
df$ps11 <- df$pp1sat11 + df$stroop1sat11 + df$stroop2sat11 + df$ldstcorrect11
df$ps15 <- df$pp1sat15 + df$stroop1sat15 + df$stroop2sat15 + df$ldstcorrect15

# Memory
mem06 <- df$wvlt123correctmean06 + df$wvltdelayrecall06 + 
            df$reyimmrecalltotalscore06 +	df$reydelayrecalltotalscore06
mem11 <- df$wvlt123correctmean11 + df$wvltdelayrecall11 + 
            df$reyimmrecalltotalscore11 +	df$reydelayrecalltotalscore11
mem15 <- df$wvlt123correctmean15 + df$wvltdelayrecall15 + 
            df$reyimmrecalltotalscore15 +	df$reydelayrecalltotalscore15

# Executive function
exf06 <- df$fluencya06 + df$fluencyb06 + df$stroopinterference06 +	df$vsattotalsat06
exf11 <- df$fluencya11 + df$fluencyb11 + df$stroopinterference11 +	df$vsattotalsat11
exf15 <- df$fluencya15 + df$fluencyb15 + df$stroopinterference15 +	df$vsattotalsat15                         

