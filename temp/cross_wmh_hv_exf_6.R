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


ThreeFactorSEM <- function(var1, var1names, var2, var2names, var3, var3names, var4, var4names) {
  
  sem1form <- paste(var3names, "~", paste(var1names, var2names, sep=" + "))
  sem2form <- paste(var3names, var4names, sep=" ~ ")
  
  sem3form <- paste(var1names, var2names, sep=" ~~ ")
  sem4form <- paste(var1names, var4names, sep=" ~~ ")
  sem5form <- paste(var2names, var4names, sep=" ~~ ")
  
  test <- file("../../temp/cross_wmh_hv_exf_6.lav")
  writeLines(c("# regressions",
               sem1form,
               sem2form,
               "
               ",
               "# covariances",
               sem3form,
               sem4form,
               sem5form),
             test)
  close(test) 
}

#MTL vols?
volVars <- c("logwmh")
hpcVars <- c("hv")
cognVars <- c("exf")
intVars <- c("int")
timepoints <- c("06")
datafile <- "../../data/RUNDMC_datasheet_long.csv"
df <- read.csv(datafile, header=T)

exf <- c("fluencyanimals", "fluencyjobs", "stroopinterference", "vsattotalsat")
exfVars <- paste(exf, "06", sep="")
df$exf06 <- rowSums(df[exfVars])

df$int06 <- df$logwmh06*df$wmh06

for (i in hpcVars) {
  for (j in volVars) {
    for (k in cognVars) {
      for (l in intVars) {
        variables <- c(i, j, k, l)
        varNames <- c(as.vector(outer(variables, timepoints, paste, sep="")))
        vecLen <- length(varNames)
        hpcNames <- varNames[seq(1, vecLen, 4)]
        volNames <- varNames[seq(2, vecLen, 4)]
        cognNames <- varNames[seq(3, vecLen, 4)]
        intNames <- varNames[seq(4, vecLen, 4)]
        df.subset <- df[varNames]
        df.subset <- df.subset[complete.cases(df.subset), ]
        ThreeFactorSEM(i, hpcNames, j, volNames, k, cognNames, l, intNames)
      }
      
    }
    
  }

}


model <- readLines("../../temp/cross_wmh_hv_exf_6.lav")
fit <- sem(model,
           data=df.subset)
summary(fit, standardized=T, rsquare=T)
fitMeasures(fit)

semPaths(fit, "stand", sizeMan = 14, label.cex = 2.3, shapeMan = "square", nCharNodes = 0, edge.label.cex = 3,
         residuals=F, edge.color = "black", edge.width = 0.5)


