library(lavaan)
library(semPlot)
library(ggraph)
library(ggplot2)

MakeLatentStrings <- function(latVar, latVarNames) {

  latSlope <- paste(latVar, "slope", sep="_")
  latInt <- paste(latVar, "int", sep="_")
  timeVec <- character()

  for (times in range(0, length(latVarNames)-1)) {
    timeVec <- c(timeVec, times)
  }

  latSlopeForm <- paste(latSlope, "=~",
                        paste("1*", latVarNames, sep="", collapse=" + "))
  latIntForm <- paste(latInt, "=~",
                        paste("1*", latVarNames, sep="", collapse=" + "))

}


ThreeFactorSEM <- function(var1, var1names, var2, var2names, var3, var3names) {
  
  sem1form <- paste(var3names, "~", paste(var1names, var2names, sep=" + "))
  sem2form <- paste(var1names, var2names, sep=" ~~ ")
    
  test <- file("../../temp/cross_wmh_hv_exf_2.lav")
  writeLines(c("# regressions",
               sem1form,
               "
               ",
               "# covariances",
               sem2form),
             test)
  close(test) 
}

#MTL vols?
volVars <- c("logwmh")
hpcVars <- c("hv")
cognVars <- c("exf")
timepoints <- c("06")
datafile <- "../../data/RUNDMC_datasheet_long.csv"
df <- read.csv(datafile, header=T)

exf <- c("fluencyanimals", "fluencyjobs", "stroopinterference", "vsattotalsat")
exfVars <- paste(exf, "06", sep="")
df$exf06 <- rowSums(df[exfVars])


for (i in hpcVars) {
  for (j in volVars) {
    for (k in cognVars) {
      variables <- c(i, j, k)
      varNames <- c(as.vector(outer(variables, timepoints, paste, sep="")))
      vecLen <- length(varNames)
      hpcNames <- varNames[seq(1, vecLen, 3)]
      volNames <- varNames[seq(2, vecLen, 3)]
      cognNames <- varNames[seq(3, vecLen, 3)]
      df.subset <- df[varNames]
      df.subset <- df.subset[complete.cases(df.subset), ]
      ThreeFactorSEM(i, hpcNames, j, volNames, k, cognNames)
    }
    
  }

}


model <- readLines("../../temp/cross_wmh_hv_exf_2.lav")
fit <- sem(model,
           data=df.subset)
summary(fit, standardized=T, rsquare=T)
fitMeasures(fit)

semPaths(fit, "stand", sizeMan = 14, label.cex = 2.3, shapeMan = "square", nCharNodes = 0, edge.label.cex = 3,
         residuals=F, edge.color = "black", edge.width = 0.5)


