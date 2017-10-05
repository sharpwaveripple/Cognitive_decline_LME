library(lavaan)

volVars <- "logwmh"
timepoints <- c("06", "11", "15")
datafile <- "../../data/RUNDMC_datasheet_long.csv"
varNames <- c(as.vector(outer(volVars, timepoints, paste, sep="")))

latSlope <- paste(volVars, "slope =~", sep="_")
latInt <- paste(volVars, "int", sep="_")
latIntForm <- paste(latInt, "=~",
                    paste("1*", varNames, sep="", collapse=" + "))

times <- length(timepoints)
lin <- (0:(times-1))
quad <- (0:(times-1))^2

timeStarLin <- paste(lin, "*", varNames, sep="", collapse=" + ")
latSlopeLin <- paste(latSlope, timeStarLin)
timeStarQuad <- paste(quad, "*", varNames, sep="", collapse=" + ")
latSlopeQuad <- paste(latSlope, timeStarQuad)

model1 <- paste(latSlopeLin, latIntForm, sep="\n")
model2 <- paste(latSlopeQuad, latIntForm, sep="\n")
df <- read.csv(datafile, header=T)
df.subset <- df[varNames]
df.subset <- df.subset[complete.cases(df.subset), ]
fit1 <- growth(model1, data=df.subset)
fit2 <- growth(model2, data=df.subset)
#summary(fit1)
#summary(fit2)
#print(fitMeasures(fit1))
#print(fitMeasures(fit2))
round(cbind(m1=inspect(fit1, 'fit.measures'), m2=inspect(fit2, 'fit.measures')), 3)
anova(fit1, fit2)
