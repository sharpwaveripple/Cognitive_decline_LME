library(mice)
library(lavaan)
library(semTools)
library(psych)

datafile <- "../../data/RUNDMC_datasheet_long.csv"
df <- read.csv(datafile, header=T)

# Psychomotor speed
df$ps06 <- df$pp1sat06 + df$stroop1sat06 + df$stroop2sat06 + df$ldstcorrect06
df$ps11 <- df$pp1sat11 + df$stroop1sat11 + df$stroop2sat11 + df$ldstcorrect11
df$ps15 <- df$pp1sat15 + df$stroop1sat15 + df$stroop2sat15 + df$ldstcorrect15

# Memory
df$mem06 <- df$wvlt123correctmean06 + df$wvltdelayrecall06 + 
            df$reyimmrecalltotalscore06 +	df$reydelayrecalltotalscore06 +
            df$pp2sat06 + df$pp3sat06
df$mem11 <- df$wvlt123correctmean11 + df$wvltdelayrecall11 + 
            df$reyimmrecalltotalscore11 +	df$reydelayrecalltotalscore11 +
            df$pp2sat11 + df$pp3sat11
df$mem15 <- df$wvlt123correctmean15 + df$wvltdelayrecall15 + 
            df$reyimmrecalltotalscore15 +	df$reydelayrecalltotalscore15 +
            df$pp2sat15 + df$pp3sat15

# Executive function
df$exf06 <- df$fluencyanimals06 + df$fluencyjobs06 + df$stroopinterference06 +	df$vsattotalsat06
df$exf11 <- df$fluencyanimals11 + df$fluencyjobs11 + df$stroopinterference11 +	df$vsattotalsat11
df$exf15 <- df$fluencysupermarket15 + df$fluencyjobs15 + df$stroopinterference15 +	df$vsattotalsat15                         



# Correlations
variables <- c("wmh06", "hv06", "ps06", "mem06", "exf06",
                 "wmh11", "hv11", "ps11", "mem11", "exf11",
                 "wmh15", "hv15", "ps15", "mem15", "exf15")
df.var <- df[variables]

df.var.incl <- df.var[complete.cases(df.var), ]

variables06 <- c("wmh06", "hv06", "ps06", "mem06", "exf06")
df.var.incl06 <- df.var.incl[variables06]
variables11 <- c("wmh11", "hv11", "ps11", "mem11", "exf11")
df.var.incl11 <- df.var.incl[variables11]
variables15 <- c("wmh15", "hv15", "ps15", "mem15", "exf15")
df.var.incl15 <- df.var.incl[variables15]

corr06.mat <- corr.test(df.var.incl06)
corr11.mat <- corr.test(df.var.incl11)
corr15.mat <- corr.test(df.var.incl15)

# Table
df.cross <- as.data.frame(matrix(0, ncol=12, nrow=5))

df.cross[1:5,1] <- round(corr06.mat$r[1:5,1], digits=3)
df.cross[1:5,2] <- round(corr06.mat$p[1:5,1], digits=3)
df.cross[1:5,3] <- round(corr11.mat$r[1:5,1], digits=3)
df.cross[1:5,4] <- round(corr11.mat$p[1:5,1], digits=3)
df.cross[1:5,5] <- round(corr15.mat$r[1:5,1], digits=3)
df.cross[1:5,6] <- round(corr15.mat$p[1:5,1], digits=3)
df.cross[1:5,7] <- round(corr06.mat$r[1:5,2], digits=3)
df.cross[1:5,8] <- round(corr06.mat$p[1:5,2], digits=3)
df.cross[1:5,9] <- round(corr11.mat$r[1:5,2], digits=3)
df.cross[1:5,10] <- round(corr11.mat$p[1:5,2], digits=3)
df.cross[1:5,11] <- round(corr15.mat$r[1:5,2], digits=3)
df.cross[1:5,12] <- round(corr15.mat$p[1:5,2], digits=3)

rownames(df.cross) <- c("wmh", "hv", "psychomotor speed", "memory", "executive function")
colnames(df.cross) <- c("wmh06", "p-val", "wmh11", "p-val", "wmh15", "p-val",
                        "hv06", "p-val", "hv11", "p-val", "hv15", "p-val")
