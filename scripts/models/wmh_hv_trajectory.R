library(lavaan)
library(semPlot)


MakeLatentFormulas <- function(latVar, latVarNames) {
  latSlope <- paste(latVar, "slope", sep="_")
  latInt <- paste(latVar, "int", sep="_")
  timeVec <- character()

  for (times in 1:length(latVarNames)-1) {
    timeStar <- paste(times, "*", sep="")
    timeVec <- c(timeVec, timeStar)
  }

  latSlopeTerms <- paste(timeVec, latVarNames, sep="")
  latSlopeForm <- paste(latSlope, "=~",
                        paste(latSlopeTerms, collapse=" + "))
  latIntForm <- paste(latInt, "=~",
                        paste("1*", latVarNames, sep="", collapse=" + "))
  #latCovar <- paste(latSlope, latInt, sep=" ~~ ")

  return(c(latSlopeForm, latIntForm))#, latCovar))

}


TwoFactorLGM <- function(var1, var1names, var2, var2names) {
  lat1form <- MakeLatentFormulas(var1, var1names)
  lat2form <- MakeLatentFormulas(var2, var2names)
  slopeCovar <- paste(c(var1, var2), "_slope", sep="", collapse=" ~~ ")
  intCovar <- paste(c(var1, var2), "_int", sep="", collapse=" ~~ ")
  return(c(lat1form, lat2form,
           slopeCovar, intCovar))
       
}

#MTL vols?
volVars <- c("logwmh")
hpcVars <- c("hv")
timepoints <- c("06", "11", "15")
datafile <- "../../data/RUNDMC_datasheet_long.csv"
df <- read.csv(datafile, header=T)

for (i in hpcVars) {
  for (j in volVars) {
    variables <- c(i, j)
    varNames <- c(as.vector(outer(variables, timepoints, paste, sep="")))
    vecLen <- length(varNames)
    hpcNames <- varNames[seq(1, vecLen, 2)]
    volNames <- varNames[seq(2, vecLen, 2)]
    df.subset <- df[varNames]
    df.subset <- df.subset[complete.cases(df.subset), ]
    df.subset[volNames] <- log(df.subset[volNames] + 1)
    modelSyntax <- TwoFactorLGM(i, hpcNames, j, volNames)
    modelFileName <- paste("../../temp/", i, "_", j, "_growth.lav", sep="")
    modelFile <- file(modelFileName)
    writeLines(modelSyntax, modelFile)
    close(modelFile) 
    fit1 <- growth(readLines(modelFileName),
                  data=df.subset)
    summary(fit, standardized=T)
    print(fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "rmsea")))
  
  }

}
#df.subset[volNames] <- log(df.subset[volNames] + 1)
#f.subset[hpcNames] <- log(df.subset[hpcNames]) 
#df.subset[volNames] <- df.subset[volNames] + 2 

semPaths(fit2, "stand", sizeMan = 8, label.cex = 2, shapeMan = "square", layout = "tree2",
         nCharNodes = 0, edge.label.cex = 1,
         residuals=F, intercepts=F, edge.color = "black", edge.width = 0.5)
fit <- growth(readLines('C:/Users/jonat/Desktop/esther/temp/hv_wmh_growth.lav'),
              data = df.subset)
df.subset[wmh.names] <- log(df.subset[wmh.names])
#
#model <- readLines("wmh_hv_trajectory.lav")
#fit <- sem(model,
#           data=df.subset)
#summary(fit, standardized=T)
